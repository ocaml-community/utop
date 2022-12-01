;;; utop.el --- Universal toplevel for OCaml -*- lexical-binding: t -*-

;; Copyright: (c) 2011, Jeremie Dimino <jeremie@dimino.org>
;; Author: Jeremie Dimino <jeremie@dimino.org>
;; URL: https://github.com/ocaml-community/utop
;; Licence: BSD3
;; Version: 1.11
;; Package-Requires: ((emacs "26") (tuareg "2.2.0"))
;; Keywords: ocaml languages

;; This file is a part of utop.

;;; Commentary:

;; This package provides interaction with an OCaml utop toplevel (REPL).
;;
;; utop.el has two components - a nice OCaml REPL with auto-completion and a
;; minor mode (`utop-minor-mode'), which extends OCaml major modes
;; (e.g. `caml-mode' and `tuareg-mode') with commands to evaluate forms directly
;; in the REPL.
;;
;; See the "Integration with Emacs" section of the README for more info.

;;; Code:

(require 'easymenu)
(require 'pcase)
(require 'seq)
(require 'tabulated-list)
(require 'tuareg)

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
  "A toplevel for the ocaml programming language which interacts
with Emacs to provide an enhanced environment."
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/ocaml-community/utop")
  :link '(emacs-commentary-link :tag "Commentary" "utop"))

(defcustom utop-command "utop -emacs"
  "The command to execute for utop."
  :type 'string
  :safe 'stringp)

(defcustom utop-edit-command t
  "Whether to read the command from the minibuffer before running utop.

If nil, `utop-command' will be used without modification."
  :type 'boolean
  :safe 'booleanp)

(defcustom utop-prompt #'utop-default-prompt
  "The function which create the prompt for utop."
  :type 'function
  :safe 'functionp)

(defcustom utop-mode-hook nil
  "A hook that gets run when `utop-mode' is entered."
  :type 'hook)

(defcustom utop-exit-hook nil
  "A hook that is run whenever `utop' is exited.
This hook is only run if exiting actually kills the buffer."
  :type 'hook)

(defcustom utop-load-packages-without-asking nil
  "Load packages from file local variables without asking"
  :type 'boolean
  :safe 'booleanp)

(defcustom utop-capf-wait-interval 0.01
  "Length of time to wait when polling for completion candidates."
  :type 'float
  :safe 'floatp)

(defcustom utop-capf-max-wait-time 0.1
  "Maximum time to wait before giving up on completion."
  :type 'float
  :safe 'floatp)

(defface utop-prompt
  '((((background dark)) (:foreground "Cyan1"))
    (((background light)) (:foreground "blue")))
  "The face used to highlight the prompt.")

(defface utop-stdout
  nil
  "The face used to highlight messages coming from stdout.")

(defface utop-stderr
  nil
  "The face used to highlight messages coming from stderr.")

(defface utop-frozen
  '((t (:bold t)))
  "The face used to highlight text that has been sent to utop.")

(defface utop-error
  '((t (:foreground "#ff4040" :bold t :underline t)))
  "The face used to highlight errors in phrases.")

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

(defvar-local utop-process nil
  "The Lisp-object for the utop sub-process")

(defvar-local utop-prompt-min 0
  "The point at the beginning of the current prompt.")

(defvar-local utop-prompt-max 0
  "The point at the end of the current prompt.")

(defvar-local utop-input-prompt-max 0
  "The point at the end of the last input prompt.")

(defvar-local utop-output ""
  "The output of the utop sub-process not yet processed.")

(defvar-local utop-command-number 0
  "The number of the current command.")

(defvar-local utop-completion nil
  "Current completion.")

(defvar-local utop-capf-completion-candidates nil
  "Current completion when using capf.")

(defvar-local utop-completion-prefixes nil
  "Prefixes for current completion.")

(defvar-local utop-inhibit-check nil
  "When set to a non-nil value, always insert text, even if it is
before the end of prompt.")

(defvar-local utop-state nil
  "State of utop. It is one of:

- edit: the user is typing a command
- comp: waiting for completion
- hist: waiting for history
- wait: ocaml is evaluating a phrase
- done: ocaml has died.")

(defvar-local utop-complete-buffer nil
  "The buffer that requested completion.")

(defvar-local utop-initial-command nil
  "Initial phrase to evaluate.")

(defvar-local utop-initial-mode nil
  "Mode to evaluate utop-initial-command in (nil or :multi).")

(defvar-local utop-phrase-terminator ";;"
  "The OCaml phrase terminator.")

(defvar-local utop-pending-entry nil
  "History entry")

(defvar-local utop-pending-position nil
  "The position of the cursor in the phrase sent to OCaml (where
to add the newline character if it is not accepted).")

(defvar-local utop-package-list nil
  "List of packages to load when visiting OCaml buffer.
Useful as file variable.")

(defvar-local utop-next-phrase-beginning 'utop-compat-next-phrase-beginning
  "The function used to find the beginning of the next phrase.")

(defvar-local utop-discover-phrase 'utop-compat-discover-phrase
  "The function used to discover a phrase.
It should return a triple (begin-pos, end-pos,
end-pos-with-comments)." )

(defvar-local utop-skip-after-eval-phrase t
  "Whether to skip to next phrase after evaluation.

Non-nil means skip to the end of the phrase after evaluation in the
Caml toplevel")

(defvar-local utop--complete-k (lambda (_candidates) '())
  "continuation function to populate the candidates for the company
backend")

(defvar-local utop-protocol-version "0"
  "detected version of utop protocol. 0 for unknown or version pre")

(defvar-local utop--read-version nil
  "whether we've tried to detect the utop version")

(defvar-local utop--company-loaded nil)

(defun utop--supports-company ()
  (and
   ;; version< only works on version numbers
   (condition-case nil
       (version<= "1" utop-protocol-version)
     (error t))
   (featurep 'company)))

;; +-----------------------------------------------------------------+
;; | Compatibility with different ocaml major modes                  |
;; +-----------------------------------------------------------------+

(defun utop-compat-resolve (choices)
  "Resolve a symbol based on the current major mode. CHOICES is a
list of 3 function symbols: (tuareg-symbol typerex-symbol caml-symbol)."
  (nth
   (pcase major-mode
     ('tuareg-mode 0)
     ('typerex-mode 1)
     ('caml-mode 2)
     ('reason-mode 3)
     (major-mode (error (format "utop doesn't support the major mode \"%s\". It
supports caml, tuareg, typerex and reason modes by default. For other
modes you need to set these variables:

- `utop-next-phrase-beginning'
- `utop-discover-phrase'
" major-mode))))
   choices))

(defun utop-tuareg-next-phrase ()
  "Move to the next phrase after point."
  (let* ((pos (save-excursion
                (when (looking-at-p "[;[:blank:]]*$")
                  (skip-chars-backward ";[:blank:]")
                  (when (> (point) 1)
                    (- (point) 1)))))
         (pos (if pos pos (point)))
         (phrase (tuareg-discover-phrase pos)))
    (when phrase
      (goto-char (caddr phrase))
      (tuareg-skip-blank-and-comments)
      (when (looking-at ";;[ \t\n]*")
        (goto-char (match-end 0)))
      (tuareg-skip-blank-and-comments))))

(defun utop-compat-next-phrase-beginning ()
  (funcall
   (utop-compat-resolve '(utop-tuareg-next-phrase
                          typerex-skip-to-end-of-phrase
                          caml-skip-to-end-of-phrase
                          reason-next-phrase))))

(defun utop-compat-discover-phrase ()
  (funcall
   (utop-compat-resolve '(tuareg-discover-phrase
                          typerex-discover-phrase
                          caml-find-phrase
                          reason-discover-phrase))))

;; +-----------------------------------------------------------------+
;; | Utils                                                           |
;; +-----------------------------------------------------------------+

(defmacro utop-perform (&rest actions)
  "Execute the given actions while checks are inhibited."
  `(let ((utop-inhibit-check t)
         (inhibit-read-only t))
     (progn ,@actions)))

(defun utop-buffer ()
  "Return the utop buffer."
  (get-buffer utop-buffer-name))

(defun utop-send-string (str)
  "Send a string to the utop process.
This function can only be called in the utop buffer and while the
state is not `done'."
  (process-send-string utop-process str))

(defun utop-send-command (str)
  "Send a command to utop. If utop is not running or has exited,
it is started."
  (let ((buf (utop-buffer)))
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
  (let ((buffer (utop-buffer)))
    (walk-windows
     (lambda (window)
       (when (eq (window-buffer window) buffer)
         (select-window window)
         (goto-char (point-max)))))))

(defun utop-set-state (state)
  "Change the utop state and mode-line-process."
  (setq utop-state state)
  (setq mode-line-process
        (pcase state
          ('edit ": idle")
          ('comp ": completion")
          ('hist ": history")
          ('wait ": running")
          ('copy ": copying")
          ('done
           (let ((status (process-status utop-process))
                 (code (process-exit-status utop-process)))
             (cond
              ((and (eq status 'exit) (= code 0))
               ": exited[0]")
              ((or (eq status 'exit) (eq status 'signal))
               (let* ((status-name (pcase status ('exit "exited") ('signal "killed")))
                      (msg (concat ": " status-name "[" (int-to-string code) "]")))
                 (add-text-properties 0 (length msg) '(face bold) msg)
                 msg))
              (t ": unknown"))))
          (_ ": unknown"))))

(defun utop-send-data (cmd)
  "Send current input to utop"
  (let ((lines (split-string (buffer-substring-no-properties utop-prompt-max (point-max)) "\n")))
    (setq utop-input-prompt-max utop-prompt-max)
    ;; Send all lines to utop
    (utop-send-string cmd)
    (dolist (line lines)
      ;; Send the line
      (utop-send-string (concat "data:" line "\n")))
    (utop-send-string "end:\n")))

;; +-----------------------------------------------------------------+
;; | Edition control                                                 |
;; +-----------------------------------------------------------------+

(defun utop-cannot-edit ()
  (signal
   'text-read-only
   (pcase utop-state
     ('wait "You cannot edit the buffer while ocaml is evaluating a phrase")
     ('done "You cannot edit the buffer when ocaml is not running")
     ('comp "You cannot edit the buffer while waiting for completion")
     ('copy "You cannot edit the buffer while waiting for copy of last input")
     ('hist "You cannot edit the buffer while waiting for history"))))

(defun utop-before-change (_start stop)
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
      (let ((buffer (utop-buffer)))
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
  (re-search-forward ";*[ \t\n\r]*\\'")
  (goto-char (match-beginning 0))
  (unless (looking-at-p ";;")
    (insert ";;")))

(defun utop-process-line (line)
  "Process one line from the utop sub-process."
  ;; Extract the command and its argument
  (string-match "\\`\\([a-z-]*\\):\\(.*\\)\\'" line)
  (let ((command (match-string 1 line))
        (argument (match-string 2 line)))
    (pcase command
      ;; Output on stdout
      ("stdout"
       (utop-insert-output argument 'utop-stdout))
      ;; Output on stderr
      ("stderr"
       (utop-insert-output argument 'utop-stderr))
      ;; Synchronisation of the phrase terminator
      ("phrase-terminator"
       (setq utop-phrase-terminator argument))
      ;; A new prompt
      ("prompt"
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
           (utop-eval-input nil t nil utop-initial-mode)
           (setq utop-initial-mode nil))))
      ;; Input has been accepted
      ("accept"
       ;; Add a newline character at the end of the buffer
       (goto-char (point-max))
       (insert "\n")
       ;; Make input frozen
       (add-text-properties utop-prompt-max (point-max) '(face utop-frozen))
       ;; Highlight errors
       (let ((offsets (split-string argument "," t)))
         (while offsets
           (let ((a (string-to-number (car offsets)))
                 (b (string-to-number (cadr offsets))))
             (add-text-properties (min (point-max) (+ utop-input-prompt-max a))
                                  (min (point-max) (+ utop-input-prompt-max b))
                                  '(face utop-error))
             (setq offsets (cdr (cdr offsets))))))
       ;; Make everything read-only
       (add-text-properties (point-min) (point-max) utop-non-editable-properties)
       ;; Advance the prompt
       (setq utop-prompt-min (point-max))
       (setq utop-prompt-max (point-max)))
      ;; Continue editiong
      ("continue"
       ;; Add a newline character at the position where the user
       ;; pressed enter
       (when utop-pending-position
         (goto-char (+ utop-prompt-max utop-pending-position))
         (insert "\n"))
       ;; Reset the state
       (utop-set-state 'edit))
      ;; Part of a history entry
      ("history-data"
       (cond
        (utop-pending-entry
         (setq utop-pending-entry (concat utop-pending-entry "\n" argument)))
        (t
         (setq utop-pending-entry argument))))
      ;; End of history data
      ("history-end"
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
      ("history-bound"
       ;; Just resume edition
       (utop-set-state 'edit))
      ;; Complete with a word
      ("completion-word"
       (utop-set-state 'edit)
       (with-current-buffer utop-complete-buffer (insert argument))
       ;; Hide completion
       (minibuffer-hide-completions))
      ;; Start of completion
      ("completion-start"
       (setq utop-completion nil))
      ;; A new possible completion
      ("completion"
       (catch 'done
         (dolist (prefix utop-completion-prefixes)
           ;; We need to handle specially prefixes like "List.m" as
           ;; the responses from utop don't include the module prefix.
           (let ((prefix (if (string-match-p "\\." prefix)
                             (cadr (split-string prefix "\\."))
                           prefix)))
             (when (string-prefix-p prefix argument)
               (push argument utop-completion)
               (throw 'done t))))))
      ;; End of completion
      ("completion-stop"
       (utop-set-state 'edit)
       (if (utop--supports-company)
           (funcall utop--complete-k (nreverse utop-completion))
         (setq utop-capf-completion-candidates (nreverse utop-completion)))
       (setq utop-completion nil)))))

(defun utop-process-output (_process output)
  "Process the output of utop"
  (with-current-buffer utop-buffer-name
    (utop-perform
     ;; Concatenate the output with the output not yet processed
     (setq utop-output (concat utop-output output))
     ;; Split lines. Each line contains exactly one command
     (let ((lines (split-string utop-output "\n")))
       (while (>= (length lines) 2)
         ;; process first line. if we haven't tried reading the version, we'll
         ;; trying to do it now.
         (let ((line (car lines)))
           (if utop--read-version
               (utop-process-line line)
             (progn
               (save-match-data
                 (if (string-match "protocol-version:\\([0-9]+\\)" line)
                     (setq utop-protocol-version (match-string 1 line))
                   (utop-process-line line)))
               (setq utop--read-version t)))
           ;; Remove it and continue
           (setq lines (cdr lines))))
       ;; When the list contains only one element, then this is either
       ;; the end of commands, either an unterminated one, so we save
       ;; it for later
       (setq utop-output (car lines))))))

;; +-----------------------------------------------------------------+
;; | Sending data to the utop sub-process                            |
;; +-----------------------------------------------------------------+

(defun utop-eval-input (&optional allow-incomplete auto-end add-to-history input-multi)
  "Send the current input to the utop process and let ocaml
evaluate it.

If ALLOW-INCOMPLETE is non-nil and the phrase is not terminated,
then a newline character will be inserted and edition will
continue.

If AUTO-END is non-nil then ALLOW-INCOMPLETE is ignored and the
phrase terminator (;;) will be
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
        ((eq input-multi :multi)
         "input-multi:\n")
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
    (utop-send-string "complete-company:\n")
    ;; Keep track of the prefixes, so we can avoid returning
    ;; completion which don't have a match.
    (setq utop-completion-prefixes lines)
    (dolist (line lines)
      ;; Send the line
      (utop-send-string (concat "data:" line "\n")))
    (utop-send-string "end:\n")))

(defun utop-complete-start ()
  "Conditionally begins to request completion candidates from utop."
  ;; Complete only if the cursor is after the prompt
  (when (and (eq utop-state 'edit) (>= (point) utop-prompt-max))
    ;; Use this buffer
    (setq utop-complete-buffer (current-buffer))
    ;; Send the input before the cursor
    (utop-complete-input
     (buffer-substring-no-properties utop-prompt-max (point)))))

(defun utop-completion-at-point ()
  "Complete thing at point."
  (setq utop-capf-completion-candidates nil)
  (utop-complete-start)

  (let ((elapsed-time 0))
    (while (and (eq utop-state 'comp)
                (> utop-capf-max-wait-time elapsed-time))
      (sleep-for utop-capf-wait-interval)
      (setq elapsed-time (+ elapsed-time utop-capf-wait-interval))))

  (when (>= (length utop-capf-completion-candidates) 1)
    (list
     utop-prompt-max
     (point)
     utop-capf-completion-candidates)))

(defun utop-complete ()
  "Complete current input."
  (interactive)
  (if (utop--supports-company)
      (utop-complete-start)
    (completion-at-point)))

;; +-----------------------------------------------------------------+
;; | Eval                                                            |
;; +-----------------------------------------------------------------+

(defun utop-prepare-for-eval ()
  "Prepare utop for evaluation."
  (save-excursion
    (let ((buf (utop-buffer)))
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

(defun utop-eval-string (string &optional mode)
  (with-current-buffer utop-buffer-name
    (cond
     ((eq utop-state 'edit)
      ;; Insert it at the end of the utop buffer
      (goto-char (point-max))
      (insert string)
      ;; Send input to utop now, telling it to automatically add the
      ;; phrase terminator
      (utop-eval-input nil t nil mode))
     ((eq utop-state 'wait)
      ;; utop is starting, save the initial command to send
      (setq utop-initial-command string)
      (setq utop-initial-mode mode)))))

(defun utop-eval (start end &optional mode)
  "Eval the given region in utop."
  ;; Select the text of the region
  (utop-eval-string (buffer-substring-no-properties start end) mode))

(defun utop-eval-region (start end)
  "Eval the current region in utop."
  (interactive "r")
  (utop-prepare-for-eval)
  (utop-eval start end :multi))

(defun utop-eval-phrase ()
  "Eval the surrounding Caml phrase (or block) in utop."
  (interactive)
  (utop-prepare-for-eval)
  (let ((end))
    (save-excursion
      (let ((triple (funcall utop-discover-phrase)))
        (setq end (nth 2 triple))
        (utop-eval (nth 0 triple) (nth 1 triple))))
    (when utop-skip-after-eval-phrase
      (goto-char end)
      (funcall utop-next-phrase-beginning))))

(defun utop-eval-buffer ()
  "Send the buffer to utop."
  (interactive)
  (utop-prepare-for-eval)
  (utop-eval (point-min) (point-max) :multi))

(defun utop-edit-complete ()
  "Completion in a caml/tuareg/typerex."
  (interactive)
  ;; Find the start of the current phrase
  (save-excursion
    (let* ((end (point))
           (start (nth 0 (funcall utop-discover-phrase)))
           (input (buffer-substring-no-properties start end))
           (edit-buffer (current-buffer)))
      ;; Start utop if needed
      (let ((utop-buffer (utop-buffer)))
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

;; +-----------------------------------------------------------------+
;; | Edition functions                                               |
;; +-----------------------------------------------------------------+

(defun utop-bol ()
  "Go to the beginning of line or to the end of the prompt."
  (interactive)
  (with-current-buffer utop-buffer-name
    (if (= (line-beginning-position) utop-prompt-min)
        (goto-char utop-prompt-max)
      (move-beginning-of-line 1))))

;; +-----------------------------------------------------------------+
;; | Switch to REPL                                                  |
;; +-----------------------------------------------------------------+

(defun utop-switch-to-repl (eob-p)
  "Switch to the inferior utop process buffer.
With prefix argument EOB-P, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process utop-buffer-name)
      (pop-to-buffer utop-buffer-name)
    (call-interactively #'utop))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun utop-switch-to-recent-buffer ()
  "Switch to the most recently used `utop-minor-mode' buffer."
  (interactive)
  (let ((recent-utop-minor-mode-buffer (seq-find (lambda (buf)
                                                          (with-current-buffer buf (bound-and-true-p utop-minor-mode)))
                                                        (buffer-list))))
    (if recent-utop-minor-mode-buffer
        (pop-to-buffer recent-utop-minor-mode-buffer)
      (message "utop: No recent OCaml buffer found."))))


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

EXIT-CODE is the exit code that should be returned by utop. It
defaults to 0."
  (interactive)
  (with-current-buffer utop-buffer-name
    (unless (eq utop-state 'done)
      (utop-send-string (format "exit:%d\n" (or exit-code 0))))))

(defun utop-sentinel (_process _msg)
  "Callback for process's state change."
  (let ((buffer (utop-buffer)))
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
    (dolist (package packages)
      (let* ((package package)
             (name (car package))
             (version (cdr package)))
        (push (list package (vector name version))
              tabulated-list-entries))))
  (setq tabulated-list-entries (nreverse tabulated-list-entries)))

(defun utop-package-printer (_id cols)
  "Print one findlib package entry."
  (let ((width (cadr (elt tabulated-list-format 0))))
    (insert-text-button (elt cols 0)
                        'follow-link t
                        'action 'utop-require-package-button-action)
    (insert-char ?\s (- width (length (elt cols 0))))
    (insert (elt cols 1) "\n")))

(defun utop-load-package (package)
  (when (or utop-load-packages-without-asking
            (y-or-n-p (format "Load package `%s'? " package)))
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
    (mapc 'utop-load-package utop-package-list)
    (message "uTop: OCaml packages loaded by file local variables")))

(defun utop-hack-local-variables ()
  "Perform actions defined by local variables"
  (utop-query-load-package-list))


;; +-----------------------------------------------------------------+
;; | The minor mode                                                  |
;; +-----------------------------------------------------------------+

(defun utop-arguments ()
  "Get argument list from the given command line of utop."
  ;; Split the command line
  (let ((arguments (split-string-and-unquote utop-command)))
    ;; Ensure it contains at least one argument
    (when (not arguments) (error "The utop command line is empty"))
    arguments))

(defun utop-query-arguments ()
  "Return the arguments of the utop command to run."
  ;; Read the command to run
  (when utop-edit-command
    (setq utop-command (read-shell-command "utop command line: " utop-command)))
  (utop-arguments))

(defun utop-start (arguments)
  "Start utop given ARGUMENTS."
  ;; Reset variables
  (setq utop-prompt-min (point-max))
  (setq utop-prompt-max (point-max))
  (setq utop-input-prompt-max (point-max))
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

(defun utop--state ()
  "Retrieve the state of the utop buffer."
  ;; we can either be in the utop buffer itself
  (if (derived-mode-p 'utop-mode)
      utop-state
    ;; or in another buffer
    (let ((buf (utop-buffer)))
      (when buf
        (with-current-buffer buf
         utop-state)))))

;;;###autoload
(define-minor-mode utop-minor-mode
  "Minor mode for utop."
  :lighter " utop"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-s") #'utop)
            (define-key map (kbd "C-x C-e") #'utop-eval-phrase)
            (define-key map (kbd "C-c C-e") #'utop-eval-phrase)
            (define-key map (kbd "C-x C-r") #'utop-eval-region)
            (define-key map (kbd "C-c C-b") #'utop-eval-buffer)
            (define-key map (kbd "C-c C-k") #'utop-kill)
            (define-key map (kbd "C-c C-z") #'utop-switch-to-repl)
            (easy-menu-define
              utop-minor-mode-menu map
              "utop menu."
              '("utop"
                ["Start utop" utop t]
                ["Switch to utop" utop-switch-to-repl t]
                ["Interrupt utop" utop-interrupt :active (utop-running-p)]
                ["Kill utop" utop-kill :active (utop-running-p)]
                ["Exit utop gracefully" utop-exit :active (utop-running-p)]
                "---"
                ["Evaluate phrase" utop-eval-phrase :active (and (utop-running-p) (eq (utop--state) 'edit))]
                ["Evaluate region" utop-eval-region :active (and (utop-running-p) (eq (utop--state) 'edit))]
                ["Evaluate buffer" utop-eval-buffer :active (and (utop-running-p) (eq (utop--state) 'edit))]
                "---"
                ["Customize utop" (customize-group 'utop) t]
                "---"
                ["About" utop-about t]
                ["Help" utop-help t]))
            map)
  ;; Load local file variables
  (add-hook 'hack-local-variables-hook 'utop-hack-local-variables))

(defvar company-backends)

;; +-----------------------------------------------------------------+
;; | The major mode                                                  |
;; +-----------------------------------------------------------------+

(defun utop-running-p ()
  (let ((buf (utop-buffer)))
    (when buf
      (with-current-buffer buf
        (and utop-process (eq (process-status utop-process) 'run))))))

(define-obsolete-function-alias 'utop-is-running 'utop-running-p "2.0")

(defun utop-about ()
  (interactive)
  (describe-variable 'utop-license))

(defun utop-help ()
  (interactive)
  (describe-function 'utop))

(defvar utop-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     #'utop-eval-input-or-newline)
    (define-key map (kbd "C-m")     #'utop-eval-input-or-newline)
    (define-key map (kbd "C-j")     #'utop-eval-input-auto-end)
    (define-key map (kbd "<home>")  #'utop-bol)
    (define-key map (kbd "C-a")     #'utop-bol)
    (define-key map (kbd "M-p")     #'utop-history-goto-prev)
    (define-key map (kbd "M-n")     #'utop-history-goto-next)
    (define-key map (kbd "TAB")     #'utop-complete)
    (define-key map (kbd "C-c C-c") #'utop-interrupt)
    (define-key map (kbd "C-c C-i") #'utop-interrupt)
    (define-key map (kbd "C-c C-k") #'utop-kill)
    (define-key map (kbd "C-c C-g") #'utop-exit)
    (define-key map (kbd "C-c C-s") #'utop)
    (define-key map (kbd "C-c C-z") #'utop-switch-to-recent-buffer)
    (define-key map (kbd "C-c m")   #'utop-copy-old-input)
    (easy-menu-define
      utop-menu map
      "utop menu."
      '("utop"
        ["Start utop" utop t]
        ["Interrupt utop" utop-interrupt :active (utop-running-p)]
        ["Kill utop" utop-kill :active (utop-running-p)]
        ["Exit utop gracefully" utop-exit :active (utop-running-p)]
        "---"
        ["Evaluate Phrase" utop-eval-input-auto-end :active (and (utop-running-p) (eq utop-state 'edit))]
        ["Switch to OCaml source buffer" utop-switch-to-recent-buffer t]
        "---"
        ["Customize utop" (customize-group 'utop) t]
        "---"
        ["About" utop-about t]
        ["Help" utop-help t]))
    map)
  "The utop local keymap.")

;;;###autoload
(define-derived-mode utop-mode fundamental-mode "utop"
  "Set the buffer mode to utop."

  ;; Set the hook to call before changing the buffer
  (add-hook 'before-change-functions 'utop-before-change nil t)

  ;; Register the exit hook
  (add-hook 'kill-buffer-hook (lambda () (run-hooks 'utop-exit-hook)) t t)

  ;; Save history before killing the buffer
  (add-hook 'kill-buffer-query-functions (lambda () (utop-save-history) t) nil t)

  ;; add company completion hook:
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'utop-company-backend))

  (add-hook 'completion-at-point-functions #'utop-completion-at-point nil 'local)

  ;; Start utop
  (utop-start (utop-arguments)))

;; +-----------------------------------------------------------------+
;; | Starting utop                                                   |
;; +-----------------------------------------------------------------+

;;;###autoload
(defun utop ()
  "A universal toplevel for OCaml.

url: https://github.com/ocaml-community/utop

utop is a enhanced toplevel for OCaml with many features,
including context sensitive completion.

This is the emacs frontend for utop. You can use the utop buffer
as a standard OCaml toplevel.

To complete an identifier, simply press TAB.

Special keys for utop:
\\{utop-mode-map}"
  (interactive)
  (let ((buf (utop-buffer)))
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
      ;; Jump to the buffer - If utop-command is used as a
      ;; buffer-local variable we pass the value along to the utop
      ;; buffer.
      (let ((cmd utop-command))
        (pop-to-buffer buf)
        (setq utop-command cmd)
        ;; Put it in utop mode
        (with-current-buffer buf (utop-mode)))))
    buf))

(declare-function company-begin-backend "ext:company")
(declare-function company-grab-symbol-cons "ext:company")

(defun utop-company-backend (command &optional _arg &rest _ignored)
  "Perform an action determined by COMMAND."
  (interactive (list 'interactive))
  (pcase command
    ('interactive (company-begin-backend 'utop-company-backend))
    ('sorted t)
    ('prefix (and (derived-mode-p 'utop-mode)
                  (or (company-grab-symbol-cons "\\." 1) 'stop)))
    ('candidates
     (progn
       `(:async
         . ,(lambda (k)
              (setq utop--complete-k k)
              (call-interactively #'utop-complete)))))))

(provide 'utop-minor-mode)
(provide 'utop)

;;; utop.el ends here
