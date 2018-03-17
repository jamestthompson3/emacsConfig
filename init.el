;;; Config  ------ Summary -----------------
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup t)
(package-initialize)
(package-install-selected-packages)
;; ------------- Install Required Packages --------------
;; (defvar my-packages '(multiple-cursors js2-mode rjsx-mode flow-minor-mode projectile prettier-js emmet-mode web-mode dumb-jump neotree flycheck wakatime afternoon-theme auto-complete ac-js2 company) ; POPULATE YOUR TO-BE-INSTALLED PACKAGE LIST HERE

;; ;; Auto install the required packages
;; ;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
;; ;; http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
;; (defvar modi/missing-packages '()
;;   "List populated at each startup that contains the list of packages that need
;; to be installed.")

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (add-to-list 'modi/missing-packages p)))

;; (when modi/missing-packages
;;   (message "Emacs is now refreshing its package database...")
;;   (package-refresh-contents)
;;   ;; Install the missing packages
;;   (dolist (p modi/missing-packages)
;;     (message "Installing `%s' .." p)
;;     (package-install p))
;;   (setq modi/missing-packages '()))

;; -------------------- End Installing Required Packages ----------------------


;;---------- Multi cursors -------------

(when (require 'multiple-cursors nil 'noerror)

  (package-install 'multiple-cursors)
  )
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-D") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-f") 'mc/mark-all-like-this)
;; ------------- End Multi cursors -------------

;; --------   Emmet ----------------------------
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'js2-jsx-mode-hook 'emmet-mode) ;; Auto-start on jsx
(setq emmet-move-cursor-between-quotes t) ;; default nil

;; ----------- End Emmet --------------------------------

;; ------------ JS2 mode -------------------------
(setq js2-strict-missing-semi-warning nil)
(when (require 'rjsx nil 'noerror)
  (package-install 'rjsx-mode)
  )
;; ---------- End JS2 Mode -----------------

;; ----------- Projectile Mode -------------
(when (require 'projectile nil 'noerror)
  (package-install 'projectile)
  )
(projectile-global-mode)

;; ----------- End Projectile Mode -------------

;; ------- Delete selected text -----------------
(delete-selection-mode 1)

;; ----------- RJSX Mode ----------------
(add-to-list 'auto-mode-alist '("src\\/.*\\.js\\'" . rjsx-mode))

;; --------- Auto refresh buffers ----------------
(global-auto-revert-mode t)

;; ------------  Tree on sidebar -------------
(when (require 'neotree  nil 'noerror)
  (package-install 'neotree)
  )
  (global-set-key [f8] 'neotree-toggle)

;; ---------- Tab Width Settings ------------
(setq tab-width 2) ; or any other preferred value
    (defvaralias 'c-basic-offset 'tab-width)
    (defvaralias 'cperl-indent-level 'tab-width)

;; ----------- No dinging --------------
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; ------- Flycheck Mode --------------------------------------

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(when (require 'flycheck  nil 'noerror)
  (package-install 'flycheck)
  )
(global-flycheck-mode)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkersg
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; -------------- End Flycheck ----------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy nil)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(backup-directory-alist (quote (("." . "~/.local/share/emacs/backups"))))
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path (quote (nil "src")))
 '(custom-enabled-themes (quote (misterioso)))
 '(electric-indent-mode nil)
 '(electric-pair-mode t)
 '(fci-rule-color "#14151E")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-move-visual t)
 '(line-number-mode t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   (quote
    (react-snippets ## yasnippet company-tern company tern tern-auto-complete ac-js2 auto-complete afternoon-theme flow-minor-mode diminish prettier-js flycheck multiple-cursors wakatime-mode dumb-jump projectile rjsx-mode)))
 '(prettier-js-args
   (quote
    ("--trailing-comma" "none" "--parser" "flow" "--semi" "false" "single-quote" "true" "--write")))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules" "lib")))
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil)
 '(visible-bell t)
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ------------- Wakatime Mode -----------------
(when (require 'wakatime nil 'noerror)
  (package-install 'wakatime-mode)
  )

(setq wakatime-api-key "09ce4cf1-376a-4562-bcfa-0d6c9f89ea96")
;; windows check
;;(setq wakatime-cli-path "/opt/conda/bin/wakatime")
   (setq wakatime-cli-path "/usr/local/bin/wakatime")
(global-wakatime-mode)

;; --------- End Wakatime Mode ---------------------

;; ------------------- Flow ----------------------------
(when (require 'flow-minor-mode nil 'noerror)
  (package-install 'flow-minor-mode)
  )

;; ---------------- End Flow -------------------------

;; -------- Prettier-JS ----------------------------
(when (require 'prettier-js  nil 'noerror)
  (package-install 'prettier-js)
  )
;;; add-node-modules-path.el --- Add node_modules to your exec-path

;; Copyright (C) 2016 Neri Marschik
;; This package uses the MIT License.
;; See the LICENSE file.

;; Author: Neri Marschik <marschik_neri@cyberagent.co.jp>
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: javascript, node, node_modules, eslint
;; URL: https://github.com/codesuki/add-node-modules-path

;;; Commentary:
;;
;; This file provides `add-node-modules-path', which searches
;; the current files parent directories for the `node_modules/.bin/' directory
;; and adds it to the buffer local `exec-path'.
;; This allows Emacs to find project based installs of e.g. eslint.
;;
;; Usage:
;;     M-x add-node-modules-path
;;
;;     To automatically run it when opening a new buffer:
;;     (Choose depending on your favorite mode.)
;;
;;     (eval-after-load 'js-mode
;;       '(add-hook 'js-mode-hook #'add-node-modules-path))
;;
;;     (eval-after-load 'js2-mode
;;       '(add-hook 'js2-mode-hook #'add-node-modules-path))

;;; Code:

;;;###autoload
(defvar add-node-modules-path-debug nil
  "Enable verbose output when non nil.")

;;;###autoload
(defun add-node-modules-path ()
  "Search the current buffer's parent directories for `node_modules/.bin`.
If it's found, then add it to the `exec-path'."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name "node_modules/.bin/" root))))
    (if root
        (progn
          (make-local-variable 'exec-path)
          (add-to-list 'exec-path path)
          (when add-node-modules-path-debug
            (message (concat "added " path  " to exec-path"))))
      (when add-node-modules-path-debug
        (message (concat "node_modules not found in " root))))))

(provide 'add-node-modules-path)

;;; add-node-modules-path.el ends here


;;; -------------- JS MODE CONFIG --------------
(require 'ac-js2)
;; (require 'tern)
;; (require 'tern-auto-complete)
;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))
(setq ac-js2-evaluate-calls t)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'js2-jsx-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
  "--trailing-comma" "none"
  "--parser" "flow"
  "--semi" "false"
  "single-quote" "true"
  ))
(require 'company)
(require 'company-tern)
(require 'yasnippet)
(require 'react-snippets)
(yas-global-mode 1)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-tern)
;; Disable completion keybindings, as we use xref-js2 instead
;; (define-key tern-mode-keymap (kbd "M-.") nil)
;; (define-key tern-mode-keymap (kbd "M-,") nil)

(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook 'flow-minor-mode)
       (add-hook 'web-mode-hook 'ac-js2-mode)
       ;; (add-hook 'web-mode-hook (lambda ()
       ;;                     (tern-mode)
       ;;                     (company-mode)))
       (add-hook 'web-mode-hook #'prettier-js-mode)))

(eval-after-load 'rjsx-mode
    '(progn
       (add-hook 'rjsx-mode-hook #'add-node-modules-path)
       (add-hook 'rjsx-mode-hook 'flow-minor-mode)
       (add-hook 'rjsx-mode-hook 'ac-js2-mode)
       ;; (add-hook 'rjsx-mode-hook (lambda ()
       ;;                     (tern-mode)
       ;;                     (company-mode)))
       (add-hook 'rjsx-mode-hook #'prettier-js-mode)))

(eval-after-load 'js2-mode
    '(progn
       (add-hook 'js2-mode-hook #'add-node-modules-path)
       (add-hook 'js2-mode-hook 'flow-minor-mode)
       ;; (add-hook 'rjsx-mode-hook (lambda ()
       ;;                     (tern-mode)
       ;;                     (company-mode)))
       (add-hook 'js2-mode-hook 'ac-js2-mode)
       (add-hook 'js2-mode-hook #'prettier-js-mode)))

(eval-after-load "flow-minor-mode"
     '(define-key flow-minor-mode-map (kbd "C-S-f") 'flow-minor-status))
;;  --------------- End PrettierJS --------------------

;; --------  Dumb jump same as Vscode -------------------
(global-set-key [f12] 'dumb-jump-go)

;; ----------  Web Mode Wrap same as VSCode ------------
(global-set-key (kbd "C-S-g") 'web-mode-element-wrap)

;--------------{Set Font}--------------;
;; (setq casey-font "PragmataPro")
;; ;; Font cosmetic edits
;; (add-to-list 'default-frame-alist '(font . "PragmataPro"))
;; (set-face-attribute 'default t :font "PragmataPro")
(require 'afternoon-theme)
(load-theme 'afternoon t)
;; WIP CHANGE FROM VSCODE
;; (set-background-color "#1b1f23")
;; ;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
;;  (set-face-attribute 'font-lock-comment-face nil :foreground "#637577")
;;  (set-face-attribute 'font-lock-constant-face nil :foreground "#01afff")
;;  (set-face-attribute 'font-lock-doc-face nil :foreground "#64a3aa")
;; (set-face-attribute 'font-lock-function-name-face nil :foreground "#ac6bdb")
;; ;;(set-face-attribute 'font-lock-parameter-face nil :foreground "#dbdb95")
;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#f4892b")
;; (set-face-attribute 'font-lock-string-face nil :foreground "#23d7d7")
;; ;; (set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
;;  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#bfbfbf")


;------------{NO Scrollbar}------------;

(scroll-bar-mode -1)
;------------{NO toolbar}------------;
(tool-bar-mode 0)

;;------------- Line Numbers ----------------


;; Show all line numbering by default (you can turn this off if you would like)
(line-number-mode t)
(linum-mode t)

;; Adjust Spacing
(setq linum-format "%d  ")

;; -------- End Line Numbers ------------

;; ------------- Random Modes ------------------
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )

(defun xah-quote-lines ()
  "Change current text block's lines to quoted lines with comma or other separator char.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

URL `http://ergoemacs.org/emacs/emacs_quote_lines.html'
Version 2017-01-08"
  (interactive)
  (let* (
         $p1
         $p2
         ($quoteToUse
          (read-string
           "Quote to use:" "\"" nil
           '(
             ""
             "\""
             "'"
             "("
             "{"
             "["
             )))
         ($separator
          (read-string
           "line separator:" "," nil
           '(
             ""
             ","
             ";"
             )))
         ($beginQuote $quoteToUse)
         ($endQuote
          ;; if begin quote is a bracket, set end quote to the matching one. else, same as begin quote
          (let (($syntableValue (aref (syntax-table) (string-to-char $beginQuote))))
            (if (eq (car $syntableValue ) 4) ; ; syntax table, code 4 is open paren
                (char-to-string (cdr $syntableValue))
              $quoteToUse
              ))))
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "NOERROR")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (skip-chars-forward "\t ")
        (insert $beginQuote)
        (goto-char (point-max))
        (insert $endQuote)
        (goto-char (point-min))
        (while (re-search-forward "\n\\([\t ]*\\)" nil "NOERROR" )
          (replace-match
           (concat $endQuote $separator (concat "\n" (match-string 1)) $beginQuote) "FIXEDCASE" "LITERAL"))
        ;;
        ))))

;: ------------- Auto-complete -------------------
(require 'auto-complete)
;;(global-auto-complete-mode)
;;(ac-set-trigger-key "TAB")
;; Basic .emacs with a good set of defaults, to be used as template for usage
;; with OCaml and OPAM
;;
;; Author: Louis Gesbert <louis.gesbert@ocamlpro.com>
;; Released under CC0

;; Generic, recommended configuration options



;; ANSI color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Some key bindings

(global-set-key [f3] 'next-match)
(defun prev-match () (interactive nil) (next-match -1))
(global-set-key [(shift f3)] 'prev-match)
(global-set-key [backtab] 'auto-complete)

;; OCaml configuration
;;  - better error and backtrace matching

(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
    2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))

(add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
(add-hook 'caml-mode-hook 'set-ocaml-error-regexp)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;;; .emacs ends here
