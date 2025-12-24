(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
		 'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
	 'utf-8))
(prefer-coding-system 'utf-8)

(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
	(when at-work
		;; (setenv "PATH" (concat "C:/Users/k.c.juntunen/bin/PortableGit/usr/bin" (getenv "PATH")))
		(setq find-program "C:/Users/k.c.juntunen/bin/PortableGit/usr/bin/find.exe"
					grep-program "C:/Users/k.c.juntunen/opt/ripgrep-13.0.0-x86_64-pc-windows-gnu/rg.exe"))
	(when (not at-work)
		;; (setenv "PATH" (concat "C:/Users/k.c.juntunen/bin/PortableGit/usr/bin" (getenv "PATH")))
		(setq find-program "c:/\"Program Files\"/Git/usr/bin/find.exe"
					grep-program "c:/ProgramData/chocolatey/bin/rg.exe")))

(add-to-list 'display-buffer-alist
             '("CAPTUR.*"
               (display-buffer-in-direction)
               (direction . below)))

(add-to-list 'display-buffer-alist
             '("\\*Capture\\*"
               (display-buffer-in-direction)
               (direction . below)))

(add-to-list 'display-buffer-alist
             '("\\*Org Select\\*"
               (display-buffer-in-direction)
               (direction . below)))

(add-to-list 'display-buffer-alist
             '("\\.org$"
               (display-buffer-in-direction)
               (direction . below)))

(setq diff-command "C:/Users/k.c.juntunen/bin/PortableGit/bin/git.exe diff --no-index")

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))

(defvar-local my-minibuffer-font-remap-cookie nil
  "The current face remap of `my-minibuffer-set-font'.")

(defface my-minibuffer-default
  '((t :height 0.8))
  "Face for the minibuffer and the Completions.")

(defun my-minibuffer-set-font ()
  (setq-local my-minibuffer-font-remap-cookie
              (face-remap-add-relative 'default 'my-minibuffer-default)))

(defun kc/kill-buffers-commonly-in-my-way ()
	"Sometimes I have a bunch of buffers open which I don't need anymore but are
getting in the way of selecting the buffers I do want."
	(interactive)
	(let ((cnt 0))
		(dolist (b (buffer-list))
			(if (string-match-p "[Ll]og\\|repo" (buffer-name b))
					(progn (kill-buffer b)
								 (setq cnt (+ 1 cnt)))))
		(message "Killed %s buffer(s)" cnt)))

(add-hook 'minibuffer-mode-hook #'my-minibuffer-set-font)

(provide 'win-config)
