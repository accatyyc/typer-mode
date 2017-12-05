;;; -*- lexical-binding: t; -*-

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
  (typer-do
   (goto-char (point-max))
   (insert (typer-random-sentences 2))
   (goto-char typer-point)))

(defun typer-handle-char (arg)
  (if (string= (string (following-char)) arg)
	  (typer-handle-match)
	(typer-handle-miss)))

(defun typer-insert-command ()
  (interactive)
  (typer-handle-char (this-command-keys)))

(defvar typer-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map [remap self-insert-command] 'typer-insert-command)
	map))

(defun typer-post-command-hook ()
  (goto-char typer-point))

(define-derived-mode typer-mode nil "Typer"
  "A game for practising typing speed"
  (read-only-mode 1)
  (set (make-local-variable 'typer-point) (point-min))
  (add-hook 'post-command-hook 'typer-post-command-hook nil :local))

(defun typer-random-words (n)
  (goto-char (random (point-max)))
  (backward-word)
  (mark-word)
  (let ((word (downcase (buffer-substring-no-properties (mark) (point)))))
	(if (eq 1 n)
		word
	  (concat word " " (typer-random-words (- n 1))))))

(defun typer-random-sentences (n)
  (with-temp-buffer
	(info "(efaq)" (buffer-name))
	(let ((string ""))
	  (dotimes (i n)
		(setq string (concat string (typer-random-words (+ 1 (random 3))) "\n")))
	  string)))

(defun typer ()
  (interactive)
  (let ((buffer-name "*Typer*"))
	(select-window (or (get-buffer-window buffer-name)
					   (selected-window)))
	(switch-to-buffer buffer-name))
  (typer-mode)
  (typer-do
   (erase-buffer)
   (insert (typer-random-sentences 10)))
  (goto-char (point-min)))
