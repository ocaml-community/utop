(*
 * uTop_gtk.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open Lwt
open Lwt_react
open UTop_styles

(* Copy stderr for errors. *)
let stderr_fd = Unix.dup Unix.stderr
let stderr = Unix.out_channel_of_descr stderr_fd

(* Logs to the original stderr: *)
let () =
  Lwt_log.default := Lwt_log.channel ~close_mode:`Close ~channel:(Lwt_io.of_fd ~mode:Lwt_io.output (Lwt_unix.of_unix_file_descr ~blocking:true ~set_flags:false stderr_fd)) ()

(* Just to prevent ocaml from doing stuppid things with the
   terminal. *)
let () = Unix.putenv "TERM" "dumb"

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

let colors_16 = [|
  (0x00, 0x00, 0x00);
  (0xcd, 0x00, 0x00);
  (0x00, 0xcd, 0x00);
  (0xcd, 0xcd, 0x00);
  (0x00, 0x00, 0xee);
  (0xcd, 0x00, 0xcd);
  (0x00, 0xcd, 0xcd);
  (0xe5, 0xe5, 0xe5);
  (0x7f, 0x7f, 0x7f);
  (0xff, 0x00, 0x00);
  (0x00, 0xff, 0x00);
  (0xff, 0xff, 0x00);
  (0x5c, 0x5c, 0xff);
  (0xff, 0x00, 0xff);
  (0x00, 0xff, 0xff);
  (0xff, 0xff, 0xff);
|]

let color_of_term_color default = function
  | LTerm_style.Default ->
      default ()
  | LTerm_style.Index n ->
      if n >= 0 && n <= 15 then
        let r, g, b = colors_16.(n) in
        `RGB (r * 65535 / 255, g * 65535 / 255, b * 65535 / 255)
      else
        default ()
  | LTerm_style.RGB (r, g, b) ->
      `RGB (r * 65535 / 255, g * 65535 / 255, b * 65535 / 255)

let default_foreground () =
  match S.value UTop.profile with
    | UTop.Dark -> `WHITE
    | UTop.Light -> `BLACK

let default_background () =
  match S.value UTop.profile with
    | UTop.Dark -> `BLACK
    | UTop.Light -> `WHITE

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

let history = ref []

let init_history () =
  let hist_name = Filename.concat LTerm_resources.home ".utop-history" in
  (* Save history on exit. *)
  Lwt_main.at_exit (fun () -> LTerm_read_line.save_history hist_name !history);
  (* Load history. *)
  lwt h = LTerm_read_line.load_history hist_name in
  history := h;
  return ()

(* +-----------------------------------------------------------------+
   | GTK ui                                                          |
   +-----------------------------------------------------------------+ *)

(* Initializes GTK. *)
let _ = GMain.init ~setlocale:false ()

(* Create the main window. *)
let window = GWindow.window ~title:"utop" ~width:800 ~height:600 ~allow_shrink:true ()

(* The scrolled window which contains the edition widget. *)
let scrolled_window = GBin.scrolled_window ~packing:window#add ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()

(* Create the edition widget which will contains ocaml output. *)
let edit = GText.view ~packing:scrolled_window#add ~editable:false ()

(* The edition buffer. *)
let edit_buffer = edit#buffer

(* Uneditable text tag. *)
let frozen = edit#buffer#create_tag [`EDITABLE false]

(* Start of prompt. *)
let prompt_start = ref 0

(* End of prompt. *)
let prompt_stop = ref 0

(* Mutex used to protect access to [edit#buffer], [prompt_start] and
   [prompt_stop]. *)
let edit_mutex = Mutex.create ()

(* [true] iff the current insertion is done by the computer and not by
   the user. *)
let computer_insertion = ref false

(* Exit when the window is closed. *)
let _ =
  window#connect#destroy (fun () ->
                            (* Destroy the main window immedlatly,
                               because the saving of history may take
                               a while. *)
                            window#destroy ();
                            exit 0)

(* Condition which is signaled when the user press Return. *)
let accept_cond = Lwt_condition.create ()

(* Notification used to notify the main thread that input is
   available. *)
let notification = Lwt_unix.make_notification (Lwt_condition.signal accept_cond)

(* Accept current input when the user press Return. *)
let _ =
  edit#event#connect#key_press
    (fun ev ->
       if GdkEvent.Key.keyval ev = GdkKeysyms._Return then
         Lwt_unix.send_notification notification;
       false)

(* +-----------------------------------------------------------------+
   | Styling                                                         |
   +-----------------------------------------------------------------+ *)

(* Is there pending data ? *)
let pending = ref false

(* Input sent to ocaml but not yet finished. *)
let pending_string = ref ""

(* Length of pending input, in unicode characters. *)
let pending_length = ref 0

let gdk_color spec = Gdk.Color.alloc ~colormap:edit#misc#colormap spec

let tag_of_term_style style =
  let props = [] in
  let props = if LTerm_style.bold style = Some true then `WEIGHT `BOLD :: props else props in
  let props = if LTerm_style.underline style = Some true then `UNDERLINE `SINGLE :: props else props in
  let props =
    if LTerm_style.reverse style = Some true then
      let props =
        match LTerm_style.foreground style with
          | Some color -> `BACKGROUND_GDK (gdk_color (color_of_term_color default_foreground color)) :: props
          | None -> `BACKGROUND_GDK (gdk_color (default_foreground ())) :: props
      in
      let props =
        match LTerm_style.background style with
          | Some color -> `FOREGROUND_GDK (gdk_color (color_of_term_color default_background color)) :: props
          | None -> `FOREGROUND_GDK (gdk_color (default_background ())) :: props
      in
      props
    else
      let props =
        match LTerm_style.foreground style with
          | Some color -> `FOREGROUND_GDK (gdk_color (color_of_term_color default_foreground color)) :: props
          | None -> props
      in
      let props =
        match LTerm_style.background style with
          | Some color -> `BACKGROUND_GDK (gdk_color (color_of_term_color default_background color)) :: props
          | None -> props
      in
      props
  in
  edit_buffer#create_tag props

(* Handle buffer modifications. *)
let changed argv =
  if not !computer_insertion then begin
    Mutex.lock edit_mutex;
    let start = edit_buffer#get_iter (`OFFSET !prompt_stop) and stop = edit_buffer#end_iter in
    (* First remove all tags from the input. *)
    edit_buffer#remove_all_tags ~start ~stop;
    (* Syntax highlighting. *)
    let stylise start stop style =
      let start = !prompt_stop + max 0 (start - !pending_length) and stop = !prompt_stop + stop - !pending_length in
      if start < stop then begin
        let start = edit_buffer#get_iter (`OFFSET start) and stop = edit_buffer#get_iter (`OFFSET stop) in
        edit_buffer#apply_tag ~start ~stop (tag_of_term_style style)
      end
    in
    UTop_styles.stylise stylise (UTop_lexer.lex_string (!pending_string ^ edit_buffer#get_text ~start ~stop ()));
    Mutex.unlock edit_mutex
  end

let _ =
  GtkSignal.connect_by_name
    edit_buffer#as_buffer
    ~name:"changed"
    ~callback:(Gobject.Closure.create changed)
    ~after:false

(* +-----------------------------------------------------------------+
   | Standard outputs redirections                                   |
   +-----------------------------------------------------------------+ *)

let copy ic =
  while true do
    let line = input_line ic in
    Mutex.lock edit_mutex;
    (* Insert the line before the prompt. *)
    let iter = edit_buffer#get_iter (`OFFSET !prompt_start) in
    computer_insertion := true;
    edit_buffer#insert ~iter ~tags:[frozen] line;
    edit_buffer#insert ~iter ~tags:[frozen] "\n";
    computer_insertion := false;
    (* Advance the prompt. *)
    let delta = iter#offset - !prompt_start in
    prompt_start := !prompt_start + delta;
    prompt_stop := !prompt_stop + delta;
    Mutex.unlock edit_mutex
  done

let redirect fd =
  let fdr, fdw = Unix.pipe () in
  Unix.dup2 fdw fd;
  Unix.close fdw;
  Thread.create copy (Unix.in_channel_of_descr fdr)

let _ = redirect Unix.stdout
let _ = redirect Unix.stderr

(* +-----------------------------------------------------------------+
   | OCaml integration                                               |
   +-----------------------------------------------------------------+ *)

(* The text typed by the user. *)
let input = ref ""

(* The position of the text already sent to ocaml in {!input}. *)
let pos = ref 0

let rec read_input prompt buffer length =
  if !pos = String.length !input then begin
    (match prompt with
       | "# " ->
           (* New phrase. *)

           (* Reset completion. *)
           UTop_complete.reset ();

           (* Increment the command counter. *)
           UTop_private.set_count (React.S.value UTop_private.count + 1);

           (* Call hooks. *)
           Lwt_sequence.iter_l (fun f -> f ()) UTop.new_command_hooks;
           Lwt_sequence.iter_l (fun f -> f ()) UTop.new_prompt_hooks;

           Mutex.lock edit_mutex;

           (* Add the previous line to the history. *)
           if !pending then begin
             history := LTerm_read_line.add_entry !pending_string !history;
             pending := false;
             pending_string := "";
             pending_length := 0
           end;

           (* Insert the prompt. *)
           prompt_start := edit#buffer#end_iter#offset;
           computer_insertion := true;
           edit_buffer#insert ~iter:edit_buffer#end_iter ~tags:[frozen] prompt;
           computer_insertion := false;
           prompt_stop := edit_buffer#end_iter#offset;

           Mutex.unlock edit_mutex;

       | "* " | "  " ->
           (* Continuation of the current phrase. *)

           (* Call hooks. *)
           Lwt_sequence.iter_l (fun f -> f ()) UTop.new_prompt_hooks;

           (* Insert the prompt. *)
           Mutex.lock edit_mutex;
           prompt_start := edit#buffer#end_iter#offset;
           computer_insertion := true;
           edit_buffer#insert ~iter:edit_buffer#end_iter ~tags:[frozen] prompt;
           computer_insertion := false;
           prompt_stop := edit_buffer#end_iter#offset;
           Mutex.unlock edit_mutex

       | _ ->
           (* Unknown prompt: error. *)

           let dialog = GWindow.dialog ~title:"error" () in
           ignore (GMisc.label ~text:(Printf.sprintf "unrecognized prompt %S!" prompt) ~packing:dialog#vbox#add ());
           dialog#add_button_stock `OK `OK;
           ignore (dialog#run ());
           exit 1);

    (* Make the buffer editable. *)
    edit#set_editable true;

    (* Wait for the user to press Return. *)
    let () = Lwt_main.run (Lwt_condition.wait accept_cond) in

    Mutex.lock edit_mutex;
    (* Get the user input. *)
    let start = edit_buffer#get_iter (`OFFSET !prompt_stop) and stop = edit_buffer#end_iter in
    let text = edit_buffer#get_text ~start ~stop () in
    (* Froze the input. *)
    edit_buffer#apply_tag ~start ~stop frozen;
    (* Advance the prompt. *)
    let offset = stop#offset in
    prompt_start := offset;
    prompt_stop := offset;
    Mutex.unlock edit_mutex;

    (* Make the buffer uneditable while ocaml is executing things. *)
    edit#set_editable false;

    input := text;
    pos := 0;

    (* Add current input to pending input. *)
    if !pending then begin
      pending_string := !pending_string ^ "\n" ^ text;
      pending_length := !pending_length + 1 + Zed_utf8.length text
    end else begin
      pending := true;
      pending_string := text;
      pending_length := Zed_utf8.length text
    end;

    read_input prompt buffer length
  end else begin
    (* There is still some pending input. *)
    let i = ref 0 in
    while !i < length && !pos < String.length !input do
      buffer.[!i] <- (!input).[!pos];
      incr i;
      incr pos
    done;
    (!i, false)
  end

let () = Toploop.read_interactive_input := read_input

(* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ *)

lwt () = join [
  init_history ();
  UTop_styles.load ();
]

(* Set the font of the edition buffer. *)
let () =
  match styles.style_font with
    | Some font -> edit#misc#modify_font_by_name font
    | None -> ()

(* Set foreground color. *)
let () =
  match styles.style_foreground with
    | Some color ->
        edit#misc#modify_text [(`NORMAL, color_of_term_color default_foreground color)]
    | None ->
        edit#misc#modify_text [(`NORMAL, default_foreground ())]

(* Set background color. *)
let () =
  match styles.style_background with
    | Some color ->
        edit#misc#modify_base [(`NORMAL, color_of_term_color default_background color)]
    | None ->
        edit#misc#modify_base [(`NORMAL, default_background ())]

(* The glib main loop. *)
let main () =
  (* For some reason, this must happen in the dispatcher thread on
     windows. *)
  window#show ();

  while true do
    Lwt_glib.iter ()
  done

(* Start the glib main loop in another thread. *)
let _ = Thread.create main ()
