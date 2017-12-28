;;; -*- lexical-binding: t; -*-

(defgroup typer nil
  "Practise your keyboard typing speed."
  :prefix "typer-"
  :group 'games)

(defcustom typer-max-lines 20
  "If more than this amount of lines are added to the typer buffer, the game ends."
  :group 'typer
  :type 'number)

(defcustom typer-buffer-name "*Typer*"
  "Name used for Typer buffer."
  :group 'typer
  :type 'string)

(defcustom typer-min-words-per-line 1
  "The minimum number of words per line. Each line contains a number of words
  between this and typer-max-words-per-line."
  :group 'typer
  :type 'integer)

(defcustom typer-max-words-per-line 3
  "The minimum number of words per line. Each line contains a number of words
  between this and typer-min-words-per-line."
  :group 'typer
  :type 'integer)

(defmacro typer-do (&rest body)
  "Disable `read-only-mode', evaluate BODY, then enable it again"
  `(progn (read-only-mode 0) ,@body (read-only-mode 1)))

(defun typer-handle-match ()
  (typer-do
   (delete-char 1)
   (if (string= (string (following-char)) "\n")
	   (progn
		 (move-beginning-of-line nil)
		 (kill-line))
	 (insert " "))
   (setq typer-point (point))))

(defun typer-handle-miss ()
  (typer-add-line))

(defun typer-animate-line-insertion ()
  (if typer-line-queue
	  (let ((token (pop typer-line-queue)))
		(goto-char (point-max))
		(when (not (string= token "\n"))
		  (goto-char (point-at-bol)))
		(typer-do (insert token))
		(goto-char typer-point))
	(cancel-timer typer-animation-timer)
	(setq typer-animation-timer nil)))

(defun typer-add-line ()
  (let ((line (typer-random-sentences 1)))
	(setq typer-line-queue (append typer-line-queue '("\n") (reverse (split-string line "" t)))))
  (when (not (timerp typer-animation-timer))
	(setq typer-animation-timer (run-at-time nil 0.01 'typer-animate-line-insertion))))

(defun typer-game-over ()
  (typer-do
   (setq typer-state :typer-game-over)
   (cancel-timer typer-animation-timer)
   (erase-buffer)
   (insert "Game over!")
   (setq cursor-type nil)))

(defun typer-check-state ()
  (let ((line-count (count-lines (point-min) (point-max))))
	(when (>= line-count typer-max-lines)
		(typer-game-over))))

(defun typer-handle-char (arg)
  (if (string= (string (following-char)) arg)
	  (typer-handle-match)
	(typer-handle-miss)))

(defun typer-insert-command ()
  (interactive)
  (when (equal (symbol-value typer-state) :typer-playing)
	(typer-handle-char (this-command-keys))
	(typer-check-state)))

(defvar typer-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map [remap self-insert-command] 'typer-insert-command)
	map))

(defun typer-post-command-hook ()
  (goto-char typer-point))

(defun typer-kill-buffer-hook ()
  (when (timerp typer-animation-timer)
	(cancel-timer typer-animation-timer)))

(define-derived-mode typer-mode nil "Typer"
  "A game for practising typing speed"
  (read-only-mode 1)
  (buffer-disable-undo)
  (defvar-local typer-point (point-min))
  (defvar-local typer-state :typer-playing)
  (defvar-local typer-line-queue '())
  (defvar-local typer-animation-timer nil)
  (add-hook 'post-command-hook 'typer-post-command-hook nil :local)
  (add-hook 'kill-buffer-hook 'typer-kill-buffer-hook nil :local)
  (typer-do
   (erase-buffer)
   (insert (typer-random-sentences 10)))
  (goto-char (point-min)))

(defun typer-random-between (min max)
  (if (<= max min)
	  min
	(+ min (random (1+ (- max min))))))

(defun typer-random-words-from-current-buffer (n)
  (if (< n 1)
	  nil
	(goto-char (random (point-max)))
	(backward-word)
	(mark-word)
	(let ((word (downcase (buffer-substring-no-properties (mark) (point)))))
	  (cons word (typer-random-words-from-current-buffer (1- n))))))

(defun typer-random-sentences-from-current-buffer (n)
  (if (> n 0)
	  (let* ((word-count (typer-random-between typer-min-words-per-line typer-max-words-per-line))
			 (words (typer-random-words-from-current-buffer word-count))
			 (sentence (mapconcat 'identity words " ")))
		(cons sentence (typer-random-sentences-from-current-buffer (1- n))))
	nil))

(defun typer-random-sentences (n)
  "Opens the elisp manual in a temporary buffer and generates sentences from there."
  (with-temp-buffer
	(info "(elisp)" (buffer-name))
	(mapconcat 'identity (typer-random-sentences-from-current-buffer n) "\n")))

(defun typer ()
  (interactive)
  (select-window (or (get-buffer-window typer-buffer-name)
					 (selected-window)))
  (switch-to-buffer typer-buffer-name)
  (typer-mode))
