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

(defun typer-handle-char (arg)
  (if (string= (string (following-char)) arg)
	  (typer-handle-match)))

(setq typer-mode-map
  (let ((map (make-sparse-keymap)))
	(dolist (key (split-string "abcdefghijklmnopqrstuvwxyz" ""))
	  (define-key map (kbd key) (lambda () (interactive)(typer-handle-char key))))
	(define-key map (kbd "SPC") (lambda () (interactive)(typer-handle-char " ")))
	map))

(defun typer-post-command-hook ()
  (goto-char typer-point))

(define-derived-mode typer-mode nil "Typer"
  "A game for practising typing speed"
  (read-only-mode 1)
  (set (make-local-variable 'typer-point) (point-min))
  (add-hook 'post-command-hook 'typer-post-command-hook nil :local))

(defun typer ()
  (interactive)
  (let ((buffer-name "*Typer*"))
	(select-window (or (get-buffer-window buffer-name)
					   (selected-window)))
	(switch-to-buffer buffer-name))
  (typer-mode)
  (typer-do (erase-buffer))
  (typer-do (insert "hello\nsome\nwords\nin a list\n atc"))
  (goto-char (point-min)))
