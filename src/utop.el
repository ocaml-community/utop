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
  "The text not yet added to the history")

;; +-----------------------------------------------------------------+
;; | Utils                                                           |
;; +-----------------------------------------------------------------+

(defun utop-add-text-properties-rear-nonsticky (start end properties nonsticky-properties &optional object)
  "Same as ``add-text-properties'' but put the last character in
non-sticky mode."
  (when (< start end)
    ;; Put everything between start and end-1 in sticky read-only mode
    (add-text-properties start (- end 1) properties object)
    ;; Put the last character in non-sticky mode
    (add-text-properties (- end 1) end
                         (append
                          properties
                          (list 'rear-nonsticky nonsticky-properties))
                         object)))

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
  (unless (null utop-history-prev)
    (with-current-buffer utop-buffer-name
      ;; Push current input after the history cursor
      (push (delete-and-extract-region utop-prompt-max (point-max)) utop-history-next)
      ;; Go to after the prompt to insert the previous input
      (goto-char utop-prompt-max)
      ;; Pop one element from history before the cursor and insert it
      (insert (pop utop-history-prev)))))

(defun utop-history-goto-next ()
  "Go to the next entry of the history."
  (interactive)
  (unless (null utop-history-next)
    (with-current-buffer utop-buffer-name
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
  (with-current-buffer utop-buffer-name
    (save-excursion
      (let ((line (concat output "\n")))
        ;; Make the line read-only
        (add-text-properties 0 (length line) '(read-only t) line)
        ;; Apply the given face if provided
        (when face (add-text-properties 0 (length line) (list 'face face) line))
        ;; Goto before the prompt
        (goto-char utop-prompt-min)
        ;; Insert the output
        (let ((inhibit-read-only t)) (insert line))
        ;; Advance the prompt
        (setq utop-prompt-min (+ utop-prompt-min (length line)))
        (setq utop-prompt-max (+ utop-prompt-max (length line)))))))

(defun utop-insert-prompt (prompt)
  "Insert the given prompt."
  (with-current-buffer utop-buffer-name
    ;; Make the old prompt sticky so we cannot edit after it
    (let ((inhibit-read-only t))
      (remove-text-properties utop-prompt-min utop-prompt-max '(rear-nonsticky nil)))
    ;; Make the prompt read-only. Make the read-only property
    ;; non-sticky so the buffer can be edited after the prompt
    (utop-add-text-properties-rear-nonsticky 0 (length prompt) '(read-only t) '(face read-only) prompt)
    ;; Goto the end of the buffer
    (goto-char (point-max))
    ;; Make it the start of the prompt
    (setq utop-prompt-min (point))
    ;; Insert the prompt
    (let ((inhibit-read-only t)) (insert prompt))
    ;; Set the end of prompt
    (setq utop-prompt-max (point))))

(defun utop-process-line (line)
  "Process one line from the utop sub-process."
  ;; Extract the command and its argument
  (string-match "\\`\\([a-z]*\\):\\(.*\\)\\'" line)
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
        ;; Check whether there is something to push to the history
        (if (stringp utop-pending)
            ;; Push pending input to the history if it is different
            ;; from the top of the history
            (unless (and (consp utop-history) (string= utop-pending (car utop-history)))
              (push utop-pending utop-history)))
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
        (setq utop-command-number (+ utop-command-number 1))))
     ;; Continuation of previous input
     ((string= command "continue")
      ;; Reset history
      (setq utop-history-prev utop-history)
      (setq utop-history-next nil)
      ;; Insert the last prompt
      (utop-insert-prompt utop-last-prompt)))))

(defun utop-process-output (process output)
  "Process the output of utop"
  (with-current-buffer utop-buffer-name
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
      (setq utop-output (car lines)))))

;; +-----------------------------------------------------------------+
;; | Sending data to the utop sub-process                            |
;; +-----------------------------------------------------------------+

(defun utop-send-input ()
  "Send the text typed at current prompt to the utop
sub-process."
  (interactive)
  (with-current-buffer utop-buffer-name
    ;; Push input to pending input
    (let ((input (buffer-substring-no-properties utop-prompt-max (point-max))))
      (if (stringp utop-pending)
          (setq utop-pending (concat utop-pending "\n" input))
        (setq utop-pending input)))
    ;; Goto the end of the buffer
    (goto-char (point-max))
    ;; Terminate input by a newline
    (insert "\n")
    (let ((start utop-prompt-max) (stop (point-max)))
      ;; Make the text read-only
      (add-text-properties start stop '(read-only t))
      ;; Make the old prompt sticky so we cannot edit after it
      (let ((inhibit-read-only t))
        (remove-text-properties utop-prompt-min utop-prompt-max '(rear-nonsticky nil)))
      ;; Makes the text sent read only and add it the frozen face.
      (let ((inhibit-read-only t))
        (utop-add-text-properties-rear-nonsticky start stop
                                                 '(read-only t face utop-frozen)
                                                 '(face read-only)))
      ;; Move the prompt to the end of the buffer
      (setq utop-prompt-min stop)
      (setq utop-prompt-max stop)
      ;; Send everything after the prompt to utop
      (process-send-region utop-process start stop))))

;; +-----------------------------------------------------------------+
;; | Completion                                                      |
;; +-----------------------------------------------------------------+


;; +-----------------------------------------------------------------+
;; | Tuareg integration                                              |
;; +-----------------------------------------------------------------+

(defun utop-start ()
  "Start utop if not already started."
  ;; Create the utop buffer if it does not exists, otherwise just
  ;; retreive it
  (let ((buf (get-buffer-create utop-buffer-name)))
    ;; Make it appear
    (display-buffer buf)
    ;; Set the utop mode in that buffer if not already done
    (with-current-buffer buf (unless (eq major-mode 'utop-mode) (utop-mode)))))

(defun utop-eval-region (start end)
  "Eval the current region in utop."
  (interactive "r")
  ;; Start utop if needed
  (save-excursion (utop-start))
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
      ;; Insert it at the end of the utop buffer
      (goto-char (point-max))
      (insert text ";;")
      ;; Send input to utop now
      (utop-send-input))))

(defun utop-eval-phrase ()
  "Eval the surrounding Caml phrase (or block) in utop."
  (interactive)
  (let ((end))
    (save-excursion
      (let ((pair (tuareg-discover-phrase)))
	(setq end (nth 2 pair))
	(utop-eval-region (nth 0 pair) (nth 1 pair))))
    (if tuareg-skip-after-eval-phrase
	(goto-char end))))

(defun utop-eval-buffer ()
  "Send the buffer to utop."
  (interactive)
  (utop-eval-region (point-min) (point-max)))

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
;; | The mode                                                        |
;; +-----------------------------------------------------------------+

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

  ;; Set the major mode
  (setq major-mode 'utop-mode)
  (setq mode-name "utop")

  ;; Create and use the local keymap utop-mode-map
  (setq utop-mode-map (make-sparse-keymap))
  (use-local-map utop-mode-map)

  ;; Create the sub-process
  (setq utop-process (start-process "utop" (current-buffer) utop-command))

  ;; Filter the output of the sub-process with our filter function
  (set-process-filter utop-process 'utop-process-output)

  ;; Define keys
  (define-key utop-mode-map [return] 'utop-send-input)
  (define-key utop-mode-map [(control ?m)] 'utop-send-input)
  (define-key utop-mode-map [(control ?j)] 'utop-send-input)
  (define-key utop-mode-map [home] 'utop-bol)
  (define-key utop-mode-map [(control ?a)] 'utop-bol)
  (define-key utop-mode-map [(meta ?p)] 'utop-history-goto-prev)
  (define-key utop-mode-map [(meta ?n)] 'utop-history-goto-next)

  ;; Register the exit hook
  (add-hook 'kill-buffer-hook (lambda () (run-hooks 'utop-exit-hook)) t t)

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
    ;; Set the utop mode in that buffer if not already done
    (unless (eq major-mode 'utop-mode) (utop-mode))
    ;; Finally return it
    buf))

(provide 'utop)
