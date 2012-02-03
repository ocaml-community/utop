;; utop.el
;; -------
;; Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
;; Licence   : BSD3
;;
;; This file is a part of utop.

;; +-----------------------------------------------------------------+
;; | Customizable variables                                          |
;; +-----------------------------------------------------------------+

(defgroup utop nil
  "A toplevel for the ocaml programming language which interact
with Emacs to provide an enhanced environment."
  :tag "The Caml Emacs-Lisp Toplevel"
  :version "1.0"
  :group 'applications)

(defcustom utop-command "utop-emacs"
  "The command to execute for utop."
  :type 'string
  :group 'utop)

(defcustom utop-prompt 'utop-default-prompt
  "The function which create the prompt for utop."
  :type 'function
  :group 'utop)

(defcustom utop-mode-hook nil
  "A hook that gets run when `utop-mode' is entered."
  :type 'hook
  :group 'utop)

(defcustom utop-exit-hook nil
  "A hook that is run whenever `utop' is exited.
This hook is only run if exiting actually kills the buffer."
  :type 'hook
  :group 'utop)

(defface utop-prompt
  '((t (:foreground "Cyan1")))
  "The face used to highlight the prompt."
  :group 'utop)

(defface utop-stdout
  nil
  "The face used to highlight messages comming from stdout."
  :group 'utop)

(defface utop-stderr
  nil
  "The face used to highlight messages commong from stderr."
  :group 'utop)

(defface utop-frozen
  '((t (:bold t)))
  "The face used to highlight text that has been sent to utop.")

;; +-----------------------------------------------------------------+
;; | Constants                                                       |
;; +-----------------------------------------------------------------+

(defconst utop-buffer-name "*utop*"
  "The name of the buffer utop is running on.")

(defconst utop-non-editable-properties '(read-only t rear-nonsticky (read-only face))
  "List of text properties for the non-editable part of the buffer")

;; +-----------------------------------------------------------------+
;; | Variables                                                       |
;; +-----------------------------------------------------------------+

(defvar utop-process nil
  "The Lisp-object for the utop sub-process")

(defvar utop-mode-map nil
  "The utop local keymap.")

(defvar utop-prompt-min 0
  "The point at the beginning of the current prompt.")

(defvar utop-prompt-max 0
  "The point at the end of the current prompt.")

(defvar utop-last-prompt 0
  "The contents of the last displayed prompt.")

(defvar utop-output ""
  "The output of the utop sub-process not yet processed.")

(defvar utop-command-number 0
  "The number of the current command.")

(defvar utop-history nil
  "The history of typed command.")

(defvar utop-history-prev nil
  "The history before the cursor.")

(defvar utop-history-next nil
  "The history after the cursor.")

(defvar utop-pending nil
  "The text not yet added to the history.")

(defvar utop-completion nil
  "Current completion.")

(defvar utop-inhibit-check nil
  "When set to a non-nil value, always insert text, even if it is
before the end of prompt.")

(defvar utop-state nil
  "State of utop. It is one of:

- edit: the user is typing a command
- comp: waiting for completion
- wait: ocaml is evaluating a phrase
- done: ocaml has died.")

(defvar utop-initial-command nil
  "Initial phrase to evaluate.")

;; +-----------------------------------------------------------------+
;; | Utils                                                           |
;; +-----------------------------------------------------------------+

(defmacro utop-perform (&rest actions)
  "Execute the given actions while checks are inhibited."
  (list 'let (list (list 'utop-inhibit-check t) (list 'inhibit-read-only t)) (cons 'progn actions)))

(defun utop-insert (&rest args)
  "Insert text with checks inhibited."
  (utop-perform (apply 'insert args)))

(defun utop-goto-point-max-all-windows ()
  "Move the point to the end of buffer in all utop windows."
  (let ((buffer (get-buffer utop-buffer-name)))
    (walk-windows
     (lambda (window)
       (when (eq (window-buffer window) buffer)
         (select-window window)
         (goto-char (point-max)))))))

;; +-----------------------------------------------------------------+
;; | Edition control                                                 |
;; +-----------------------------------------------------------------+

(defun utop-cannot-edit ()
  (cond
   ((eq utop-state 'wait)
    (signal 'text-read-only '("You cannot edit the buffer while ocaml is evaluating a phrase")))
   ((eq utop-state 'done)
    (signal 'text-read-only '("You cannot edit the buffer when ocaml is not running")))
   ((eq utop-state 'comp)
    (signal 'text-read-only '("You cannot edit the buffer while waiting for completion")))))

(defun utop-before-change (start stop)
  (unless utop-inhibit-check
    (cond
     ((not (eq utop-state 'edit))
      (add-hook 'post-command-hook 'utop-add-change nil t)
      (utop-cannot-edit))
     ((< stop utop-prompt-max)
      (add-hook 'post-command-hook 'utop-add-change nil t)
      (signal 'text-read-only '("You cannot edit this part of the buffer"))))))

(defun utop-add-change ()
  (remove-hook 'post-command-hook 'utop-add-change t)
  (add-hook 'before-change-functions 'utop-before-change nil t))

;; +-----------------------------------------------------------------+
;; | Prompt                                                          |
;; +-----------------------------------------------------------------+

(defun utop-default-prompt ()
  "The default prompt function."
  (let ((prompt (format "utop[%d]> " utop-command-number)))
    (add-text-properties 0 (length prompt) '(face utop-prompt) prompt)
    prompt))

;; +-----------------------------------------------------------------+
;; | History                                                         |
;; +-----------------------------------------------------------------+

(defun utop-history-goto-prev ()
  "Go to the previous entry of the history."
  (interactive)
  (with-current-buffer utop-buffer-name
    (when (and (eq utop-state 'edit) utop-history-prev)
      ;; Push current input after the history cursor
      (push (delete-and-extract-region utop-prompt-max (point-max)) utop-history-next)
      ;; Go to after the prompt to insert the previous input
      (goto-char utop-prompt-max)
      ;; Pop one element from history before the cursor and insert it
      (insert (pop utop-history-prev)))))

(defun utop-history-goto-next ()
  "Go to the next entry of the history."
  (interactive)
  (with-current-buffer utop-buffer-name
    (when (and (eq utop-state 'edit) utop-history-next)
      ;; Push current input before the history cursor
      (push (delete-and-extract-region utop-prompt-max (point-max)) utop-history-prev)
      ;; Go to after the prompt to insert the next input
      (goto-char utop-prompt-max)
      ;; Pop one element from history after the cursor and insert it
      (insert (pop utop-history-next)))))

;; +-----------------------------------------------------------------+
;; | Receiving input from the utop sub-process                     |
;; +-----------------------------------------------------------------+

(defun utop-insert-output (output &optional face)
  "Insert the given output before the prompt."
  (save-excursion
    (let ((line (concat output "\n")))
      ;; Apply the given face if provided
      (when face (add-text-properties 0 (length line) (list 'face face) line))
      ;; Goto before the prompt
      (goto-char utop-prompt-min)
      ;; Insert the output
      (insert line)
      ;; Advance the prompt
      (setq utop-prompt-min (+ utop-prompt-min (length line)))
      (setq utop-prompt-max (+ utop-prompt-max (length line)))
      ;; Make everything before the end prompt read-only
      (add-text-properties (point-min) utop-prompt-max utop-non-editable-properties))))

(defun utop-insert-prompt (prompt)
  "Insert the given prompt."
  ;; Goto the end of the buffer
  (goto-char (point-max))
  ;; Make it the start of the prompt
  (setq utop-prompt-min (point))
  ;; Insert the prompt
  (insert prompt)
  ;; Set the end of prompt
  (setq utop-prompt-max (point))
  ;; Make everything before the end prompt read-only
  (add-text-properties (point-min) utop-prompt-max utop-non-editable-properties)
  ;; We are now editing
  (setq utop-state 'edit)
  ;; Move the point to the end of buffer in all utop windows
  (utop-goto-point-max-all-windows))

(defun utop-process-line (line)
  "Process one line from the utop sub-process."
  ;; Extract the command and its argument
  (string-match "\\`\\([a-z-]*\\):\\(.*\\)\\'" line)
  (let ((command (match-string 1 line)) (argument (match-string 2 line)))
    (cond
     ;; Output on stdout
     ((string= command "stdout")
      (utop-insert-output argument 'utop-stdout))
     ;; Output on stderr
     ((string= command "stderr")
      (utop-insert-output argument 'utop-stderr))
     ;; A new prompt
     ((string= command "prompt")
      (let ((prompt (apply utop-prompt ())))
        ;; Push pending input to the history if it is different from
        ;; the top of the history
        (when (and utop-pending (or (null utop-history) (not (string= utop-pending (car utop-history)))))
          (push utop-pending utop-history))
        ;; Clear pending input
        (setq utop-pending nil)
        ;; Reset history
        (setq utop-history-prev utop-history)
        (setq utop-history-next nil)
        ;; Save current prompt
        (setq utop-last-prompt prompt)
        ;; Insert the new prompt
        (utop-insert-prompt prompt)
        ;; Increment the command number
        (setq utop-command-number (+ utop-command-number 1))
        ;; Send the initial command if any
        (when utop-initial-command
          (goto-char (point-max))
          (insert utop-initial-command ";;")
          (setq utop-initial-command nil)
          (utop-send-input))))
     ;; Continuation of previous input
     ((string= command "continue")
      ;; Reset history
      (setq utop-history-prev utop-history)
      (setq utop-history-next nil)
      ;; Insert the last prompt
      (utop-insert-prompt utop-last-prompt))
     ;; Complete with a word
     ((string= command "completion-word")
      (setq utop-state 'edit)
      (insert argument)
      ;; Hide completion
      (minibuffer-hide-completions))
     ;; Start of completion
     ((string= command "completion-start")
      (setq utop-completion nil))
     ;; A new possible completion
     ((string= command "completion")
      (push argument utop-completion))
     ;; End of completion
     ((string= command "completion-stop")
      (setq utop-state 'edit)
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list (nreverse utop-completion)))
      (setq utop-completion nil)))))

(defun utop-process-output (process output)
  "Process the output of utop"
  (with-current-buffer utop-buffer-name
    (utop-perform
     ;; Concatenate the output with the output not yet processed
     (setq utop-output (concat utop-output output))
     ;; Split lines. Each line contains exactly one command
     (let ((lines (split-string utop-output "\n")))
       (while (>= (length lines) 2)
         ;; Process the first line
         (utop-process-line (car lines))
         ;; Remove it and continue
         (setq lines (cdr lines)))
       ;; When the list contains only one element, then this is either
       ;; the end of commands, either an unterminated one, so we save
       ;; it for later
       (setq utop-output (car lines))))))

;; +-----------------------------------------------------------------+
;; | Sending data to the utop sub-process                            |
;; +-----------------------------------------------------------------+

(defun utop-send-input ()
  "Send the text typed at current prompt to the utop
sub-process."
  (interactive)
  (with-current-buffer utop-buffer-name
    (when (eq utop-state 'edit)
      (utop-perform
       ;; We are now waiting for ocaml
       (setq utop-state 'wait)
       ;; Push input to pending input
       (let ((input (buffer-substring-no-properties utop-prompt-max (point-max))))
         (if utop-pending
             (setq utop-pending (concat utop-pending "\n" input))
           (setq utop-pending input))
         ;; Goto the end of the buffer
         (goto-char (point-max))
         ;; Terminate input by a newline
         (insert "\n")
         ;; Move the point to the end of buffer of all utop windows
         (utop-goto-point-max-all-windows)
         ;; Make everything read-only
         (add-text-properties (point-min) (point-max) utop-non-editable-properties)
         (let ((start utop-prompt-max) (stop (point-max)))
           ;; Set the frozen face for the text we just sent.
           (add-text-properties start stop '(face utop-frozen))
           ;; Move the prompt to the end of the buffer
           (setq utop-prompt-min stop)
           (setq utop-prompt-max stop)
           ;; Send all lines to utop
           (let ((lines (split-string input "\n")))
             (process-send-string utop-process "input:\n")
             (while lines
               ;; Send the line
               (process-send-string utop-process (concat "data:" (car lines) "\n"))
               ;; Remove it and continue
               (setq lines (cdr lines)))
             (process-send-string utop-process "end:\n"))))))))

;; +-----------------------------------------------------------------+
;; | Completion                                                      |
;; +-----------------------------------------------------------------+

(defun utop-complete ()
  "Complete current input."
  (interactive)
  ;; Complete only if the cursor is after the prompt
  (when (and (eq utop-state 'edit) (>= (point) utop-prompt-max))
    ;; Extract the input before the cursor
    (let ((input (buffer-substring-no-properties utop-prompt-max (point))))
      ;; Split it
      (let ((lines (split-string input "\n")))
        ;; We are now waiting for completion
        (setq utop-state 'comp)
        ;; Send all lines to utop
        (process-send-string utop-process "complete:\n")
        (while lines
          ;; Send the line
          (process-send-string utop-process (concat "data:" (car lines) "\n"))
          ;; Remove it and continue
          (setq lines (cdr lines)))
        (process-send-string utop-process "end:\n")))))

;; +-----------------------------------------------------------------+
;; | Tuareg integration                                              |
;; +-----------------------------------------------------------------+

(defun utop-prepare-for-eval ()
  "Prepare utop for evaluation."
  (save-excursion
    ;; Create the utop buffer if it does not exists, otherwise just
    ;; retreive it
    (let ((buf (get-buffer-create utop-buffer-name)))
      ;; Make it appear
      (display-buffer buf)
      (with-current-buffer buf
        (cond
         ((not (eq major-mode 'utop-mode))
          ;; The buffer has just been created, start utop
          (utop-mode))
         ((eq utop-state 'done)
          ;; UTop exited, restart it
          (utop-restart))
         ((not (eq utop-state 'edit))
          ;; Edition cannot be performed right now
          (utop-cannot-edit)))))))

(defun utop-eval (start end)
  "Eval the given region in utop."
  ;; From tuareg
  (setq tuareg-interactive-last-phrase-pos-in-source start)
  ;; Select the text of the region
  (let ((text
         (save-excursion
           ;; Search the start and end of the current paragraph
           (goto-char start)
           (tuareg-skip-blank-and-comments)
           (setq start (point))
           (goto-char end)
           (tuareg-skip-to-end-of-phrase)
           (setq end (point))
           (buffer-substring-no-properties start end))))
    (with-current-buffer utop-buffer-name
      (cond
       ((eq utop-state 'edit)
        ;; Insert it at the end of the utop buffer
        (goto-char (point-max))
        (insert text ";;")
        ;; Send input to utop now
        (utop-send-input))
       ((eq utop-state 'wait)
        ;; utop is starting, save the initial command to send
        (setq utop-initial-command text))))))

(defun utop-eval-region (start end)
  "Eval the current region in utop."
  (interactive "r")
  (utop-prepare-for-eval)
  (utop-eval start end))

(defun utop-eval-phrase ()
  "Eval the surrounding Caml phrase (or block) in utop."
  (interactive)
  (utop-prepare-for-eval)
  (let ((end))
    (save-excursion
      (let ((pair (tuareg-discover-phrase)))
	(setq end (nth 2 pair))
	(utop-eval (nth 0 pair) (nth 1 pair))))
    (if tuareg-skip-after-eval-phrase
	(goto-char end))))

(defun utop-eval-buffer ()
  "Send the buffer to utop."
  (interactive)
  (utop-prepare-for-eval)
  (utop-eval (point-min) (point-max)))

(defun utop-tuareg-setup ()
  "Override tuareg interactive functions by utop ones.

You can call this function after loading the tuareg mode to let
it use utop instead of its builtin support for interactive
toplevel.

To automatically do that just add these lines to your .emacs:

  (autoload 'utop-tuareg-setup \"utop\" \"Toplevel for OCaml\" t)
  (add-hook 'tuareg-mode-hook 'utop-tuareg-setup)"
  (interactive)
  (defun tuareg-eval-phrase () (interactive) (utop-eval-phrase))
  (defun tuareg-eval-region (start end) (interactive "r") (utop-eval-region start end))
  (defun tuareg-eval-buffer () (interactive) (utop-eval-buffer))
  (defun tuareg-interrupt-caml () (interactive) (utop-interrupt))
  (defun tuareg-kill-caml () (interactive) (utop-kill))
  (defun tuareg-run-caml () (interactive) (utop))
  nil)

;; +-----------------------------------------------------------------+
;; | Edition functions                                               |
;; +-----------------------------------------------------------------+

(defun utop-bol ()
  "Go to the beginning of line or to the end of the prompt."
  (interactive)
  (with-current-buffer utop-buffer-name
    (if (= (point-at-bol) utop-prompt-min)
        (goto-char utop-prompt-max)
      (move-beginning-of-line 1))))

;; +-----------------------------------------------------------------+
;; | Process control                                                 |
;; +-----------------------------------------------------------------+

(defun utop-interrupt ()
  "Interrupt utop."
  (interactive)
  (with-current-buffer utop-buffer-name
    (interrupt-process utop-process)))

(defun utop-kill ()
  "Kill utop."
  (interactive)
  (with-current-buffer utop-buffer-name
    (kill-process utop-process)))

(defun utop-sentinel (process msg)
  "Callback for process' state change."
  (let ((buffer (get-buffer utop-buffer-name)))
    ;; Do nothing if the buffer does not exist anymore
    (when buffer
      (with-current-buffer utop-buffer-name
        (let ((status (process-status utop-process)))
          (when (or (eq status 'exit) (eq status 'signal))
            ;; The process is terminated
            (setq utop-state 'done)
            (let ((exit-code (process-exit-status utop-process)))
              (utop-perform
               ;; Insert a message at the end
               (goto-char (point-max))
               (cond
                ((eq status 'exit)
                 (insert "\n\nProcess utop exited with code " (number-to-string exit-code) "\n"))
                ((eq status 'signal)
                 (insert "\n\nProcess utop has been killed by signal " (number-to-string exit-code) "\n")))
               ;; Go to the end of the buffer
               (goto-char (point-max))
               ;; Make the whole buffer read-only
               (add-text-properties (point-min) (point-max) utop-non-editable-properties)))))))))

;; +-----------------------------------------------------------------+
;; | The mode                                                        |
;; +-----------------------------------------------------------------+

(defun utop-start ()
  "Start utop."
  ;; Set the initial state: we are waiting for ocaml to send the
  ;; initial prompt
  (setq utop-state 'wait)

  ;; Reset variables
  (setq utop-prompt-min (point-max))
  (setq utop-prompt-max (point-max))
  (setq utop-output "")
  (setq utop-command-number 0)
  (setq utop-pending nil)
  (setq utop-completion nil)

  ;; Create the sub-process
  (setq utop-process (start-process "utop" (current-buffer) utop-command))

  ;; Filter the output of the sub-process with our filter function
  (set-process-filter utop-process 'utop-process-output)

  ;; Set the process sentinel
  (set-process-sentinel utop-process 'utop-sentinel))

(defun utop-restart ()
  "Restart utop."
  (goto-char (point-max))
  (utop-insert "\nRestarting...\n\n")
  (utop-start))

(defun utop-mode ()
  "Caml Emacs-Lisp Toplevel.

\\{utop-mode-map}"

  ;; Local variables
  (make-local-variable 'utop-mode-map)
  (make-local-variable 'utop-process)
  (make-local-variable 'utop-prompt-min)
  (make-local-variable 'utop-prompt-max)
  (make-local-variable 'utop-last-prompt)
  (make-local-variable 'utop-output)
  (make-local-variable 'utop-command-number)
  (make-local-variable 'utop-history)
  (make-local-variable 'utop-history-prev)
  (make-local-variable 'utop-history-next)
  (make-local-variable 'utop-pending)
  (make-local-variable 'utop-inhibit-check)
  (make-local-variable 'utop-state)
  (make-local-variable 'utop-initial-command)

  ;; Set the major mode
  (setq major-mode 'utop-mode)
  (setq mode-name "utop")

  ;; Create and use the local keymap utop-mode-map
  (setq utop-mode-map (make-sparse-keymap))
  (use-local-map utop-mode-map)

  ;; Set the hook to call before changing the buffer
  (add-hook 'before-change-functions 'utop-before-change nil t)

  ;; Define keys
  (define-key utop-mode-map [return] 'utop-send-input)
  (define-key utop-mode-map [(control ?m)] 'utop-send-input)
  (define-key utop-mode-map [(control ?j)] 'utop-send-input)
  (define-key utop-mode-map [home] 'utop-bol)
  (define-key utop-mode-map [(control ?a)] 'utop-bol)
  (define-key utop-mode-map [(meta ?p)] 'utop-history-goto-prev)
  (define-key utop-mode-map [(meta ?n)] 'utop-history-goto-next)
  (define-key utop-mode-map [tab] 'utop-complete)
  (define-key utop-mode-map [(control ?c) (control ?c)] 'utop-interrupt)
  (define-key utop-mode-map [(control ?c) (control ?i)] 'utop-interrupt)
  (define-key utop-mode-map [(control ?c) (control ?k)] 'utop-kill)

  ;; Register the exit hook
  (add-hook 'kill-buffer-hook (lambda () (run-hooks 'utop-exit-hook)) t t)

  ;; Start utop
  (utop-start)

  ;; Call hooks
  (run-mode-hooks 'utop-mode-hook))

;; +-----------------------------------------------------------------+
;; | Starting utop                                                   |
;; +-----------------------------------------------------------------+

;;;###autoload
(defun utop ()
  "Start utop."
  (interactive)
  ;; Create the utop buffer if it does not exists, otherwise just
  ;; retreive it
  (let ((buf (get-buffer-create utop-buffer-name)))
    ;; Jump to that buffer
    (pop-to-buffer buf)
    (cond
     ((not (eq major-mode 'utop-mode))
      ;; The buffer has just been created, set the utop mode
      (utop-mode))
     ((eq utop-state 'done)
      ;; utop has exited, restart it
      (utop-restart)))
    ;; Finally return it
    buf))

(provide 'utop)
