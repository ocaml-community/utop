;; utop.el
;; -------
;; Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
;; Licence   : BSD3
;;
;; This file is a part of utop.

(require 'easymenu)

;; tabulated-list is a part of Emacs 24
(require 'tabulated-list nil t)

;; +-----------------------------------------------------------------+
;; | License                                                         |
;; +-----------------------------------------------------------------+

(defconst utop-license "BSD3"
"Copyright (c) 2011, Jeremie Dimino <jeremie@dimino.org>
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Jeremie Dimino nor the names of his
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR AND CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
\(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.")

;; +-----------------------------------------------------------------+
;; | Customizable variables                                          |
;; +-----------------------------------------------------------------+

(defgroup utop nil
  "A toplevel for the ocaml programming language which interact
with Emacs to provide an enhanced environment."
  :tag "The Caml Emacs-Lisp Toplevel"
  :version "1.0"
  :group 'applications)

(defcustom utop-command "utop -emacs"
  "The command to execute for utop."
  :type 'string
  :group 'utop)

(defcustom utop-edit-command t
  "Whether to read the command from the minibuffer before running utop.

If nil, `utop-command' will be used without modification."
  :type 'boolean
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
  '((((background dark)) (:foreground "Cyan1"))
    (((background light)) (:foreground "blue")))
  "The face used to highlight the prompt."
  :group 'utop)

(defface utop-stdout
  nil
  "The face used to highlight messages comming from stdout."
  :group 'utop)

(defface utop-stderr
  nil
  "The face used to highlight messages comming from stderr."
  :group 'utop)

(defface utop-frozen
  '((t (:bold t)))
  "The face used to highlight text that has been sent to utop.")

(defface utop-error
  '((t (:foreground "#ff4040" :bold t :underline t)))
  "The face used to highlight errors in phrases."
  :group 'utop)

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

(defvar utop-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'utop-eval-input-or-newline)
    (define-key map [(control ?m)] 'utop-eval-input-or-newline)
    (define-key map [(control ?j)] 'utop-eval-input-or-newline)
    (define-key map [home] 'utop-bol)
    (define-key map [(control ?a)] 'utop-bol)
    (define-key map [(meta ?p)] 'utop-history-goto-prev)
    (define-key map [(meta ?n)] 'utop-history-goto-next)
    (define-key map [tab] 'utop-complete)
    (define-key map [(control ?c) (control ?c)] 'utop-interrupt)
    (define-key map [(control ?c) (control ?i)] 'utop-interrupt)
    (define-key map [(control ?c) (control ?k)] 'utop-kill)
    (define-key map [(control ?c) (control ?g)] 'utop-exit)
    (define-key map [(control ?c) (control ?s)] 'utop)
    (define-key map [(control ?c) ?m] 'utop-copy-old-input)
    map)
  "The utop local keymap.")

(defvar utop-prompt-min 0
  "The point at the beginning of the current prompt.")

(defvar utop-prompt-max 0
  "The point at the end of the current prompt.")

(defvar utop-output ""
  "The output of the utop sub-process not yet processed.")

(defvar utop-command-number 0
  "The number of the current command.")

(defvar utop-completion nil
  "Current completion.")

(defvar utop-inhibit-check nil
  "When set to a non-nil value, always insert text, even if it is
before the end of prompt.")

(defvar utop-state nil
  "State of utop. It is one of:

- edit: the user is typing a command
- comp: waiting for completion
- hist: waiting for history
- wait: ocaml is evaluating a phrase
- done: ocaml has died.")

(defvar utop-complete-buffer nil
  "The buffer that requested completion.")

(defvar utop-initial-command nil
  "Initial phrase to evaluate.")

(defvar utop-phrase-terminator ";;"
  "The OCaml phrase terminator.")

(defvar utop-pending-entry nil
  "History entry")

(defvar utop-pending-position nil
  "The position of the cursor in the phrase sent to OCaml (where
to add the newline character if it is not accepted).")

(defvar utop-package-list nil
  "List of packages to load when visiting OCaml buffer.
Useful as file variable.")

;; +-----------------------------------------------------------------+
;; | Compability                                                     |
;; +-----------------------------------------------------------------+

(unless (featurep 'tabulated-list)
  ;; tabulated-list.el is part of Emacs 24
  ;; This is a thin layer building compability with previous versions
  (defvar tabulated-list-format nil)
  (defvar tabulated-list-sort-key nil)
  (defvar tabulated-list-printer nil)
  (defvar tabulated-list-revert-hook nil)
  (defvar tabulated-list-entries nil)
  (define-derived-mode tabulated-list-mode special-mode "Mini-tabulated list mode"
    "Tabulated list"
    (make-local-variable 'tabulated-list-format)
    (make-local-variable 'tabulated-list-sort-key)
    (make-local-variable 'tabulated-list-printer)
    (set (make-local-variable 'revert-buffer-function) 'tabulated-list-revert)

  (defun tabulated-list-init-header ()
    (save-excursion
      (let ((inhibit-read-only t))
            (mapc
             (lambda (entry)
               (let* ((name (nth 0 entry))
                      (size (length name))
                      (padding (- (nth 1 entry) size)))
                 (insert name)
                 (insert-char ?\s padding)
                 )) tabulated-list-format)
            (insert "\n"))))

  (defun tabulated-list-print (dummy)
    (save-excursion
      (let ((inhibit-read-only t))
        (mapc (lambda (entry)
                (goto-char (point-max))
                (apply tabulated-list-printer entry))
              tabulated-list-entries))
      t))

  (defun tabulated-list-revert (ignore-auto noconfirm)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (tabulated-list-init-header)
      (tabulated-list-print t))))
)

;; +-----------------------------------------------------------------+
;; | Utils                                                           |
;; +-----------------------------------------------------------------+

(defmacro utop-perform (&rest actions)
  "Execute the given actions while checks are inhibited."
  `(let ((utop-inhibit-check t)
         (inhibit-read-only t))
     (progn ,@actions)))

(defun utop-send-string (str)
  "Send a string to the utop process. This function can only be
called in the utop buffer and while the state is not 'done."
  (process-send-string utop-process str))

(defun utop-send-command (str)
  "Send a command to utop. If utop is not running or has exited,
it is started."
  (let ((buf (get-buffer utop-buffer-name)))
    (unless buf
      (setq buf (save-excursion (utop))))
    (with-current-buffer buf
      (when (eq utop-state 'done) (utop-restart))
      (process-send-string utop-process str))))

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

(defun utop-set-state (state)
  "Change the utop state and mode-line-process."
  (setq utop-state state)
  (setq mode-line-process
        (cond
         ((eq state 'edit)
          ": idle")
         ((eq state 'comp)
          ": completion")
         ((eq state 'hist)
          ": history")
         ((eq state 'wait)
          ": running")
         ((eq state 'copy)
          ": copying")
         ((eq state 'done)
          (let ((status (process-status utop-process)) (code (process-exit-status utop-process)))
            (cond
             ((and (eq status 'exit) (= code 0))
              ": exited[0]")
             ((eq status 'exit)
              (let ((msg (concat ": exited[" (int-to-string code) "]")))
                (add-text-properties 0 (length msg) '(face bold) msg)
                msg))
             ((eq status 'signal)
              (let ((msg (concat ": killed[" (int-to-string code) "]")))
                (add-text-properties 0 (length msg) '(face bold) msg)
                msg))
             (t
              ": unknown"))))
         (t
          ": unknown"))))

(defun utop-send-data (cmd)
  "Send current input to utop"
  (let ((lines (split-string (buffer-substring-no-properties utop-prompt-max (point-max)) "\n")))
    ;; Send all lines to utop
    (utop-send-string cmd)
    (while lines
      ;; Send the line
      (utop-send-string (concat "data:" (car lines) "\n"))
      ;; Remove it and continue
      (setq lines (cdr lines)))
    (utop-send-string "end:\n")))

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
    (signal 'text-read-only '("You cannot edit the buffer while waiting for completion")))
   ((eq utop-state 'copy)
    (signal 'text-read-only '("You cannot edit the buffer while waiting for copy of last input")))
   ((eq utop-state 'hist)
    (signal 'text-read-only '("You cannot edit the buffer while waiting for history")))))

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

(defun utop-copy-old-input ()
  (interactive)
  (with-current-buffer utop-buffer-name
    (when (eq utop-state 'edit)
      (utop-set-state 'copy)
      (setq utop-pending-entry nil)
      (utop-send-data "history-prev:\n"))))

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
    (when (eq utop-state 'edit)
      (utop-set-state 'hist)
      (setq utop-pending-entry nil)
      (utop-send-data "history-prev:\n"))))

(defun utop-history-goto-next ()
  "Go to the next entry of the history."
  (interactive)
  (with-current-buffer utop-buffer-name
    (when (eq utop-state 'edit)
      (utop-set-state 'hist)
      (setq utop-pending-entry nil)
      (utop-send-data "history-next:\n"))))

(defun utop-save-history ()
  "Save history to the history file."
  (interactive)
  (with-current-buffer utop-buffer-name
    (unless (eq utop-state 'done)
      (utop-send-string "save-history:\n"))))

;; +-----------------------------------------------------------------+
;; | Receiving input from the utop sub-process                       |
;; +-----------------------------------------------------------------+

(defun utop-insert-output (output &optional face)
  "Insert the given output before the prompt."
  (let ((current-max (point-max)))
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
        (add-text-properties (point-min) utop-prompt-max utop-non-editable-properties)))
    ;; If OCaml is executing a phrase, follow its output
    (when (eq utop-state 'wait)
      (let ((buffer (get-buffer utop-buffer-name)))
        (walk-windows
         (lambda (window)
           (when (eq (window-buffer window) buffer)
             (select-window window)
             ;; Only move the point if we were at the end of the
             ;; buffer
             (when (eq (point) current-max)
               (goto-char (point-max))))))))))

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
  (utop-set-state 'edit)
  ;; Move the point to the end of buffer in all utop windows
  (utop-goto-point-max-all-windows))

(defun utop-insert-phrase-terminator ()
  "Insert the phrase terminator at the end of buffer."
  ;; Search the longest suffix of the input which is a prefix of the
  ;; phrase terminator
  (let* ((end (point-max)) (pos (max utop-prompt-max (- end (length utop-phrase-terminator)))))
    (while (not (string-prefix-p (buffer-substring-no-properties pos end) utop-phrase-terminator))
      (setq pos (1+ pos)))
    ;; Insert only the missing part
    (insert (substring utop-phrase-terminator (- end pos)))))

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
     ;; Synchronisation of the phrase terminator
     ((string= command "phrase-terminator")
      (setq utop-phrase-terminator argument))
     ;; A new prompt
     ((string= command "prompt")
      (let ((prompt (apply utop-prompt ())))
        ;; Insert the new prompt
        (utop-insert-prompt prompt)
        ;; Increment the command number
        (setq utop-command-number (+ utop-command-number 1))
        ;; Send the initial command if any
        (when utop-initial-command
          (goto-char (point-max))
          (insert utop-initial-command)
          (setq utop-initial-command nil)
          (utop-eval-input nil t nil))))
     ;; Input has been accepted
     ((string= command "accept")
      ;; Add a newline character at the end of the buffer
      (goto-char (point-max))
      (insert "\n")
      ;; Make input frozen
      (add-text-properties utop-prompt-max (point-max) '(face utop-frozen))
      ;; Highlight errors
      (let ((offsets (split-string argument "," t)))
        (while offsets
          (let ((a (string-to-number (car offsets)))
                (b (string-to-number (car (cdr offsets)))))
            (add-text-properties (+ utop-prompt-max a) (+ utop-prompt-max b) '(face utop-error))
            (setq offsets (cdr (cdr offsets))))))
      ;; Make everything read-only
      (add-text-properties (point-min) (point-max) utop-non-editable-properties)
      ;; Advance the prompt
      (setq utop-prompt-min (point-max))
      (setq utop-prompt-max (point-max)))
     ;; Continue editiong
     ((string= command "continue")
      ;; Add a newline character at the position where the user
      ;; pressed enter
      (when utop-pending-position
        (goto-char (+ utop-prompt-max utop-pending-position))
        (insert "\n"))
      ;; Reset the state
      (utop-set-state 'edit))
     ;; Part of a history entry
     ((string= command "history-data")
      (cond
       (utop-pending-entry
        (setq utop-pending-entry (concat utop-pending-entry "\n" argument)))
       (t
        (setq utop-pending-entry argument))))
     ;; End of history data
     ((string= command "history-end")
      (progn
        (cond
         ((eq utop-state 'copy)
           (kill-new utop-pending-entry))
          (t
           (goto-char utop-prompt-max)
           ;; Delete current input
           (delete-region utop-prompt-max (point-max))
           ;; Insert entry
           (insert utop-pending-entry)))
           ;; Resume edition
        (utop-set-state 'edit)))
      ;; We are at a bound of history
     ((string= command "history-bound")
      ;; Just resume edition
      (utop-set-state 'edit))
     ;; Complete with a word
     ((string= command "completion-word")
      (utop-set-state 'edit)
      (with-current-buffer utop-complete-buffer (insert argument))
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
      (utop-set-state 'edit)
      (with-current-buffer utop-complete-buffer
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list (nreverse utop-completion))))
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

(defun utop-eval-input (&optional allow-incomplete auto-end add-to-history)
  "Send the current input to the utop process and let ocaml
evaluate it.

If ALLOW-INCOMPLETE is non-nil and the phrase is not terminated,
then a newline character will be inserted and edition will
continue.

If AUTO-END is non-nill then ALLOW-INCOMPLETE is ignored and a
phrase terminator (;; or ; if using revised syntax) will be
automatically inserted by utop.

If ADD-TO-HISTORY is t then the input will be added to history."
  (interactive)
  (with-current-buffer utop-buffer-name
    (when (eq utop-state 'edit)
      ;; Clear saved pending position
      (setq utop-pending-position nil)
      ;; Insert the phrase terminator if requested
      (cond
       (auto-end
        (utop-insert-phrase-terminator))
       (allow-incomplete
        ;; Save cursor position
        (setq utop-pending-position (- (point) utop-prompt-max))
        ;; If the point is before the prompt, insert the newline
        ;; character at the end
        (when (< utop-pending-position 0)
          (setq utop-pending-position (- (point-max) utop-prompt-max)))))
      ;; We are now waiting for ocaml
      (utop-set-state 'wait)
      (utop-send-data
       (cond
        ((and allow-incomplete (not auto-end) add-to-history)
         "input:allow-incomplete,add-to-history\n")
        (add-to-history
         "input:add-to-history\n")
        (t
         "input:\n"))))))

(defun utop-eval-input-or-newline ()
  "Same as (`utop-eval-input' t nil t)."
  (interactive)
  (utop-eval-input t nil t))

(defun utop-eval-input-auto-end ()
  "Same as (`utop-eval-input' nil t t)."
  (interactive)
  (utop-eval-input nil t t))

;; +-----------------------------------------------------------------+
;; | Completion                                                      |
;; +-----------------------------------------------------------------+

(defun utop-complete-input (input)
  "Send input to complete to utop."
  ;; Split it
  (let ((lines (split-string input "\n")))
    ;; We are now waiting for completion
    (utop-set-state 'comp)
    ;; Send all lines to utop
    (utop-send-string "complete:\n")
    (while lines
      ;; Send the line
      (utop-send-string (concat "data:" (car lines) "\n"))
      ;; Remove it and continue
      (setq lines (cdr lines)))
    (utop-send-string "end:\n")))

(defun utop-complete ()
  "Complete current input."
  (interactive)
  ;; Complete only if the cursor is after the prompt
  (when (and (eq utop-state 'edit) (>= (point) utop-prompt-max))
    ;; Use this buffer
    (setq utop-complete-buffer (current-buffer))
    ;; Send the input before the cursor
    (utop-complete-input
     (buffer-substring-no-properties utop-prompt-max (point)))))

;; +-----------------------------------------------------------------+
;; | Caml/Tuareg/Typerex integration                                 |
;; +-----------------------------------------------------------------+

(defun utop-choose (symbol)
  "Be best at resolving caml, tuareg or typerex dependencies even
when byte-compiling."
  (cond
   ((eq major-mode 'tuareg-mode)
    (intern (concat "tuareg-" symbol)))
   ((eq major-mode 'typerex-mode)
    (intern (concat "typerex-" symbol)))
   ((eq major-mode 'caml-mode)
    (intern (concat "caml-" symbol)))
   ((require 'tuareg nil t)
    (intern (concat "tuareg-" symbol)))
   ((require 'typerex nil t)
    (intern (concat "typerex-" symbol)))
   ((require 'caml nil t)
    (intern (concat "caml-" symbol)))
   (error (concat "unsupported mode: " (symbol-name major-mode) ", utop support only caml, tuareg and typerex modes"))))

(defmacro utop-choose-symbol (symbol)
  (utop-choose symbol))

(defmacro utop-choose-call (symbol &rest args)
  `(,(utop-choose symbol) ,@args))

(defmacro utop-choose-defun (symbol &rest args)
  `(defun ,(utop-choose symbol) ,@args))

(defun utop-prepare-for-eval ()
  "Prepare utop for evaluation."
  (save-excursion
    (let ((buf (get-buffer utop-buffer-name)))
      (cond
       (buf
        ;; Make the buffer appear
        (display-buffer buf)
        (with-current-buffer buf
          (cond
           ((eq utop-state 'done)
            ;; UTop exited, restart it
            (utop-restart))
           ((not (eq utop-state 'edit))
            ;; Edition cannot be performed right now
            (utop-cannot-edit)))))
       (t
        ;; The buffer does not exist, read arguments before creating
        ;; it so the user can cancel starting utop
        (utop-query-arguments)
        ;; Create the buffer
        (setq buf (get-buffer-create utop-buffer-name))
        ;; Make it appear
        (display-buffer buf)
        ;; Put it in utop mode
        (with-current-buffer buf (utop-mode)))))))

(defun utop-eval (start end)
  "Eval the given region in utop."
  ;; From tuareg
  (unless (eq major-mode 'caml-mode)
    (set (utop-choose "interactive-last-phrase-pos-in-source") start))
  ;; Select the text of the region
  (let ((text
         (save-excursion
           ;; Search the start and end of the current paragraph
           (goto-char start)
           (utop-choose-call "skip-blank-and-comments")
           (setq start (point))
           (goto-char end)
           (utop-choose-call "skip-to-end-of-phrase")
           (setq end (point))
           (buffer-substring-no-properties start end))))
    (with-current-buffer utop-buffer-name
      (cond
       ((eq utop-state 'edit)
        ;; Insert it at the end of the utop buffer
        (goto-char (point-max))
        (insert text)
        ;; Send input to utop now, telling it to automatically add the
        ;; phrase terminator
        (utop-eval-input nil t nil))
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
      (let ((pair (utop-choose-call "discover-phrase")))
	(setq end (nth 2 pair))
	(utop-eval (nth 0 pair) (nth 1 pair))))
    (if (utop-choose-symbol "skip-after-eval-phrase")
	(goto-char end))))

(defun utop-eval-buffer ()
  "Send the buffer to utop."
  (interactive)
  (utop-prepare-for-eval)
  (utop-eval (point-min) (point-max)))

(defun utop-edit-complete ()
  "Completion in a caml/tuareg/typerex."
  (interactive)
  ;; Find the start of the current phrase
  (save-excursion
    (let* ((end (point))
           (start (nth 0 (utop-choose-call "discover-phrase")))
           (input (buffer-substring-no-properties start end))
           (edit-buffer (current-buffer)))
      ;; Start utop if needed
      (let ((utop-buffer (get-buffer utop-buffer-name)))
        (unless utop-buffer
          ;; The buffer does not exist, read arguments before creating
          ;; it so the user can cancel starting utop
          (utop-query-arguments)
          ;; Create the buffer
          (setq utop-buffer (get-buffer-create utop-buffer-name))
          ;; Put it in utop mode
          (with-current-buffer utop-buffer (utop-mode)))
        (with-current-buffer utop-buffer
          ;; Complete only if we are in edition mode
          (when (eq utop-state 'edit)
            ;; Use this buffer for completion
            (setq utop-complete-buffer edit-buffer)
            ;; Send the phrase to complete
            (utop-complete-input input)))))))

(defun utop-setup-ocaml-buffer ()
  "Override caml/tuareg/typerex interactive functions by utop ones.

You can call this function after loading the caml/tuareg/typerex
mode to let it use utop instead of its builtin support for
interactive toplevel.

To automatically do that just add these lines to your .emacs:

  (autoload 'utop-setup-ocaml-buffer \"utop\" \"Toplevel for OCaml\" t)
  (add-hook 'caml-mode-hook 'utop-setup-ocaml-buffer)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
  (add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)"
  (interactive)
  ;; Redefine caml/tuareg/typerex functions
  (utop-choose-defun "eval-phrase" () (interactive) (utop-eval-phrase))
  (utop-choose-defun "eval-region" (start end) (interactive "r") (utop-eval-region start end))
  (utop-choose-defun "eval-buffer" () (interactive) (utop-eval-buffer))
  (utop-choose-defun "interrupt-caml" () (interactive) (utop-interrupt))
  (utop-choose-defun "kill-caml" () (interactive) (utop-kill))
  (utop-choose-defun "run-caml" () (interactive) (utop))
  ;; Redefine this variable so menu will work
  (set (utop-choose "interactive-buffer-name") utop-buffer-name)
  (make-local-variable 'utop-package-list)
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

(defun utop-exit (&optional exit-code)
  "Try to gracefully exit utop.

EXIT-CODE is the exit code that shoud be returned by utop. It
defaults to 0."
  (interactive)
  (with-current-buffer utop-buffer-name
    (unless (eq utop-state 'done)
      (utop-send-string (format "exit:%d\n" (or exit-code 0))))))

(defun utop-sentinel (process msg)
  "Callback for process' state change."
  (let ((buffer (get-buffer utop-buffer-name)))
    ;; Do nothing if the buffer does not exist anymore
    (when buffer
      (with-current-buffer utop-buffer-name
        (let ((status (process-status utop-process)))
          (when (or (eq status 'exit) (eq status 'signal))
            ;; The process is terminated
            (utop-set-state 'done)
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
;; | ocamlfind package loading                                       |
;; +-----------------------------------------------------------------+

(defun utop-ocamlfind-list-packages ()
  "Return the list of all findlib packages with their version."
  (let ((lines (split-string (shell-command-to-string "ocamlfind list") "[ \t]*\r?\n")))
    (let ((packages))
      ;; Split lines and extract package names and versions
      (mapc
       (lambda (line)
         (when (string-match "\\([^ \t(]*\\)[ \t]*(version:[ \t]*\\([^)]*\\))" line)
           (push (cons (match-string 1 line) (match-string 2 line)) packages)))
       lines)
      (nreverse packages))))

(define-derived-mode utop-list-packages-mode tabulated-list-mode "OCaml package list"
  "Major mode for listing the findlib OCaml packages."
  (setq tabulated-list-format [("Name" 32 t)
			       ("Version" 32 t)])
  (setq tabulated-list-sort-key (cons "Name" nil))
  (setq tabulated-list-printer 'utop-package-printer)
  (add-hook 'tabulated-list-revert-hook 'utop-list-packages--refresh nil t)
  (tabulated-list-init-header))

(defun utop-list-packages--refresh ()
  "Refresh the list of findlib packages."
  (interactive)
  ;; Clear up list of entries
  (setq tabulated-list-entries nil)
  ;; Get the list of packages
  (let* ((packages (utop-ocamlfind-list-packages))
         (max-name-length 0))
        ;; Find the longest package name
    (mapc
     (lambda (package)
       (setq max-name-length
             (max max-name-length (length (car package)))))
     packages)
    ;; Minimal size of the name entry shall be 16 characters
    (setq max-name-length (1+ (max max-name-length 16)))
    ;; Set the header column size to the maximal length
    (setcdr (elt tabulated-list-format 0) (list max-name-length t))
    ;; Build a list, accumulating in tabulated-list-entries
    (while packages
      (let* ((package (car packages))
             (name (car package))
             (version (cdr package)))
        (push (list package (vector name version))
              tabulated-list-entries))
      (setq packages (cdr packages))))
  (setq tabulated-list-entries (nreverse tabulated-list-entries)))

(defun utop-package-printer (id cols)
  "Print one findlib package entry."
  (let ((width (cadr (elt tabulated-list-format 0))))
    (insert-text-button (elt cols 0)
                        'follow-link t
                        'action 'utop-require-package-button-action)
    (insert-char ?\s (- width (length (elt cols 0))))
    (insert (elt cols 1) "\n")))

(defun utop-load-package (package)
  (when (y-or-n-p (format "Load package `%s'? " package))
    ;; Load it
    (utop-send-command (format "require:%s\n" package))))

(defun utop-require-package-button-action (button)
  (utop-load-package (button-label button)))

(defun utop-list-ocaml-packages (&optional buffer)
  "Display a list of all ocaml findlib packages"
  (interactive)
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Findlib packages*")))
  (with-current-buffer buffer
    (utop-list-packages-mode)
    (utop-list-packages--refresh)
    (tabulated-list-print t)
  (display-buffer buffer)))

(defun utop-query-load-package-list ()
  "Load packages defined in utop-package-list buffer local variable."
  (when (and utop-package-list
             (y-or-n-p
              "You've defined utop-package-list variable, but uTop toplevel is not running, would you like me to start the toplevel?"))
    (with-current-buffer (utop))
    (mapc 'utop-load-package utop-package-list)))

;; +-----------------------------------------------------------------+
;; | Menu                                                            |
;; +-----------------------------------------------------------------+

(defun utop-is-running ()
  (let ((buf (get-buffer utop-buffer-name)))
    (when buf
      (with-current-buffer buf
        (and utop-process (eq (process-status utop-process) 'run))))))

(defun utop-about ()
  (interactive)
  (describe-variable 'utop-license))

(defun utop-help ()
  (interactive)
  (describe-function 'utop))

(easy-menu-define
  utop-menu utop-mode-map
  "utop menu."
  '("utop"
    ["Start OCaml" utop t]
    ["Interrupt OCaml" utop-interrupt :active (utop-is-running)]
    ["Kill OCaml" utop-kill :active (utop-is-running)]
    ["Exit utop gracefully" utop-exit :active (utop-is-running)]
    ["Evaluate Phrase" utop-eval-input-auto-end :active (and (utop-is-running) (eq utop-state 'edit))]
    "---"
    ["Customize utop" (customize-group 'utop) t]
    "---"
    ["About" utop-about t]
    ["Help" utop-help t]))

;; +-----------------------------------------------------------------+
;; | The mode                                                        |
;; +-----------------------------------------------------------------+

(defun utop-arguments ()
  "Get argument list from the given command line of utop"
  ;; Split the command line
  (let ((arguments (split-string-and-unquote utop-command)))
    ;; Ensure it contains at least one argument
    (when (not arguments) (error "The utop command line is empty"))
    arguments))

(defun utop-query-arguments ()
  "Returns the arguments of the utop command to run."
  ;; Read the command to run
  (when utop-edit-command
    (setq utop-command (read-shell-command "utop command line: " utop-command))
    (utop-arguments)))

(defun utop-start (arguments)
  "Start utop."
  ;; Reset variables
  (setq utop-prompt-min (point-max))
  (setq utop-prompt-max (point-max))
  (setq utop-output "")
  (setq utop-command-number 0)
  (setq utop-completion nil)

  ;; Set the state to done to allow utop to be restarted if
  ;; start-process fails
  (setq utop-state 'done)

  ;; Create the sub-process
  (setq utop-process (apply 'start-process "utop" (current-buffer) (car arguments) (cdr arguments)))

  ;; Set the initial state: we are waiting for ocaml to send the
  ;; initial prompt
  (utop-set-state 'wait)

  ;; Filter the output of the sub-process with our filter function
  (set-process-filter utop-process 'utop-process-output)

  ;; Set the process sentinel
  (set-process-sentinel utop-process 'utop-sentinel))

(defun utop-restart ()
  "Restart utop."
  (let ((arguments (utop-query-arguments)))
    (goto-char (point-max))
    (utop-insert "\nRestarting...\n\n")
    (utop-start arguments)))

(define-derived-mode utop-mode fundamental-mode "utop"
  "Set the buffer mode to utop."

  ;; Local variables
  (make-local-variable 'utop-process)
  (make-local-variable 'utop-prompt-min)
  (make-local-variable 'utop-prompt-max)
  (make-local-variable 'utop-last-prompt)
  (make-local-variable 'utop-output)
  (make-local-variable 'utop-command-number)
  (make-local-variable 'utop-inhibit-check)
  (make-local-variable 'utop-state)
  (make-local-variable 'utop-complete-buffer)
  (make-local-variable 'utop-initial-command)
  (make-local-variable 'utop-phrase-terminator)
  (make-local-variable 'utop-pending-position)
  (make-local-variable 'utop-pending-entry)

  ;; Set the hook to call before changing the buffer
  (add-hook 'before-change-functions 'utop-before-change nil t)

  ;; Register the exit hook
  (add-hook 'kill-buffer-hook (lambda () (run-hooks 'utop-exit-hook)) t t)

  ;; Save history before killing the buffer
  (add-hook 'kill-buffer-query-functions (lambda () (utop-save-history) t) nil t)

  ;; Start utop
  (utop-start (utop-arguments)))
;; +-----------------------------------------------------------------+
;; | Starting utop                                                   |
;; +-----------------------------------------------------------------+

;;;###autoload
(defun utop ()
  "A universal toplevel for OCaml.

url: https://forge.ocamlcore.org/projects/utop/

utop is a enhanced toplevel for OCaml with many features,
including context sensitive completion.

This is the emacs frontend for utop. You can use the utop buffer
as a standard OCaml toplevel.

To complete an identifier, simply press TAB.

Special keys for utop:
\\{utop-mode-map}"
  (interactive)
  (let ((buf (get-buffer utop-buffer-name)))
    (cond
     (buf
      ;; Jump to the buffer
      (pop-to-buffer buf)
      ;; Restart utop if it exited
      (when (eq utop-state 'done) (utop-restart)))
     (t
      ;; The buffer does not exist, read the command line before
      ;; creating it so if the user quit it won't be created
      (utop-query-arguments)
      ;; Create the buffer
      (setq buf (get-buffer-create utop-buffer-name))
      ;; Jump to the buffer
      (pop-to-buffer buf)
      ;; Put it in utop mode
      (with-current-buffer buf (utop-mode))))
  buf))

(provide 'utop)
