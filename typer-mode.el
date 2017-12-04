;;; -*- lexical-binding: t; -*-

(defun typer-insert (arg)
  (interactive)
  (read-only-mode 0)
  (insert arg)
  (read-only-mode 1))

(defun typer-delete (n)
  (read-only-mode 0)
  (delete-char n)
  (read-only-mode 1))

(defun typer-handle-match ()
  (read-only-mode 0)
  (delete-char 1)
  (if (string= (string (following-char)) "\n")
	  (progn
		(move-beginning-of-line nil)
		(kill-line))
	(insert " "))
  (setq typer-point (point))
  (read-only-mode 1))

(defun typer-handle-char (arg)
  (interactive)
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

(defun insert-words ()
  (typer-insert "hello\nsome\nwords\nin a list\n atc"))

(defun typer ()
  (interactive)
  (let ((buffer-name "*Typer*"))
	(select-window (or (get-buffer-window buffer-name)
					   (selected-window)))
	(switch-to-buffer buffer-name))
  (typer-mode)
  (insert-words)
  (goto-char (point-min)))
