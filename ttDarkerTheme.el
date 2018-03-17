;==============================={Cosmetic Edits}===============================;


;-----------{Bright red TODO}----------;

; Bright-red TODOs
 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode))
 (make-face 'font-lock-fixme-face)
 (make-face 'font-lock-note-face)
 (mapc (lambda (mode)
     (font-lock-add-keywords
      mode
      '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
    fixme-modes)
 (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
 (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)


;----------{Line Highlighting}---------;



;------------{NO Scrollbar}------------;

(scroll-bar-mode -1)


;------{Dont use shift to select}------;

(setq shift-select-mode nil)

;--------------{Set Font}--------------;

(setq casey-font "PragmataPro")
;; Font cosmetic edits
(add-to-list 'default-frame-alist '(font . "PragmataPro"))
(set-face-attribute 'default t :font "PragmataPro")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")


;-------------{NO toolbar}-------------;

; Turn off the toolbar
(tool-bar-mode 0)


;---------{Post Loading Stuff}---------;

(defun post-load-stuff ()
  (interactive)
  (menu-bar-mode -1)
  (maximize-frame)
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")
)
(add-hook 'window-setup-hook 'post-load-stuff t)

;---------------{Unsure}---------------;

(load-library "view")


;------------{Use a beacon}------------;

;; from http://endlessparentheses.com/beacon-never-lose-your-cursor-again.html
(use-package beacon-mode)
(package-install 'beacon)
(try-require 'beacon-mode)
(beacon-mode 1)
(setq beacon-push-mark 35)
(setq beacon-color "#666600")


;------------{Region state}------------;

;; https://github.com/xuchunyang/region-state.el
(package-install 'region-state)
(try-require 'region-state)
(add-hook 'rectangle-mark-mode 'region-state-mode)

;--------------{Spacemacs}-------------;


;;  http://xenodium.com/#installing-emacs-spaceline
(use-package spaceline :ensure t
  :config
  (use-package spaceline-config
    :config
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-buffer-encoding-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (setq powerline-default-separator 'arrow-fade)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-define-segment line-column
      "The current line and column numbers."
      "l:%l c:%2c")
    (spaceline-define-segment time
      "The current time."
      (format-time-string "%H:%M"))
    (spaceline-define-segment date
      "The current date."
      (format-time-string "%h %d"))
    (spaceline-toggle-time-on)
    (spaceline-emacs-theme 'date 'time)
	(spaceline-helm-mode)))


;--------{Powerline alternative}-------;

  
;; Alternatively custumise into a powerline https://github.com/milkypostman/powerline

;(use-package spaceline-config
;  :ensure spaceline
;  :config (progn
;            (defun malb/load-mode-line-theme ()
;              (setq spaceline-workspace-numbers-unicode nil)
;
;              ;; https://github.com/TheBB/spaceline/issues/54
;              (spaceline-define-segment line-column
;                "The current line and column numbers."
;                (if (eq major-mode 'pdf-view-mode)
;                    (concat (number-to-string (pdf-view-current-page))
;                            "/"
;                            (number-to-string (pdf-cache-number-of-pages)))
;                  mode-line-position
;                  "%l:%2c"))
;
;              (spaceline-spacemacs-theme)
;              (spaceline-helm-mode)
;
;              (spaceline-toggle-hud-off)
;              (spaceline-toggle-buffer-encoding-abbrev-off)

;              (set-face-attribute 'powerline-active1 nil :background "grey22" :foreground "white smoke")
;              (set-face-attribute 'powerline-active2 nil :background "grey40" :foreground "gainsboro")
;              (set-face-attribute 'powerline-inactive1 nil :background "grey55" :foreground "white smoke")
;              (set-face-attribute 'powerline-inactive2 nil :background "grey65" :foreground "gainsboro")
;              (set-face-attribute 'mode-line-buffer-id nil :foreground "white smoke")
;              (powerline-reset))))  
			  


;==============================={Color Blending}===============================;

;; http://oremacs.com/page10/
(defun colir-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun colir-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'colir-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))		

;; http://oremacs.com/page17/ - test emacs init file
(defun ora-test-emacs ()
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
          "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))		  



;------------{Zenburn theme}-----------;

;; Consistent things
(defvar me/font-family            "PragmataPro"  "The font to use.")

(when casey-win32
  (defvar me/font-size-default      120       "The font size to use for default text.")
  (defvar me/font-size-header       140       "The font size to use for headers.")
  (defvar me/font-size-mode-line    120       "The font size to use for the mode line."))


(when casey-linux
  (defvar me/font-size-default      420       "The font size to use for default text.")
  (defvar me/font-size-header       440       "The font size to use for headers.")
  (defvar me/font-size-mode-line    420       "The font size to use for the mode line."))

(when casey-linux
   (set-face-attribute 'default nil :height 200))

; 
; 
; ;; zenburn theme https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
; 
; (package-install 'zenburn-theme)
; (try-require 'zenburn-theme)
; 
; (defconst zenburn/bg+3      "#6F6F6F"  "Zenburn palette: #6F6F6F.")
; (defconst zenburn/bg+2      "#5F5F5F"  "Zenburn palette: #5F5F5F.")
; (defconst zenburn/bg+1      "#4F4F4F"  "Zenburn palette: #4F4F4F.")
; (defconst zenburn/bg+0      "#494949"  "Zenburn palette: #494949.")
; (defconst zenburn/bg        "#3F3F3F"  "Zenburn palette: #3F3F3F.")
; (defconst zenburn/bg-0      "#383838"  "Zenburn palette: #383838.")
; (defconst zenburn/bg-1      "#2B2B2B"  "Zenburn palette: #2B2B2B.")
; (defconst zenburn/bg-2      "#000000"  "Zenburn palette: #000000.")
; (defconst zenburn/blue+1    "#94BFF3"  "Zenburn palette: #94BFF3.")
; (defconst zenburn/blue      "#8CD0D3"  "Zenburn palette: #8CD0D3.")
; (defconst zenburn/blue-1    "#7CB8BB"  "Zenburn palette: #7CB8BB.")
; (defconst zenburn/blue-2    "#6CA0A3"  "Zenburn palette: #6CA0A3.")
; (defconst zenburn/blue-3    "#5C888B"  "Zenburn palette: #5C888B.")
; (defconst zenburn/blue-4    "#4C7073"  "Zenburn palette: #4C7073.")
; (defconst zenburn/blue-5    "#366060"  "Zenburn palette: #366060.")
; (defconst zenburn/cyan      "#93E0E3"  "Zenburn palette: #93E0E3.")
; (defconst zenburn/fg+1      "#FFFFEF"  "Zenburn palette: #FFFFEF.")
; (defconst zenburn/fg        "#DCDCCC"  "Zenburn palette: #DCDCCC.")
; (defconst zenburn/fg-1      "#656555"  "Zenburn palette: #656555.")
; (defconst zenburn/green+4   "#BFEBBF"  "Zenburn palette: #BFEBBF.")
; (defconst zenburn/green+3   "#AFD8AF"  "Zenburn palette: #AFD8AF.")
; (defconst zenburn/green+2   "#9FC59F"  "Zenburn palette: #9FC59F.")
; (defconst zenburn/green+1   "#8FB28F"  "Zenburn palette: #8FB28F.")
; (defconst zenburn/green     "#7F9F7F"  "Zenburn palette: #7F9F7F.")
; (defconst zenburn/green-1   "#5F7F5F"  "Zenburn palette: #5F7F5F.")
; (defconst zenburn/magenta   "#DC8CC3"  "Zenburn palette: #DC8CC3.")
; (defconst zenburn/orange    "#DFAF8F"  "Zenburn palette: #DFAF8F.")
; (defconst zenburn/red+1     "#DCA3A3"  "Zenburn palette: #DCA3A3.")
; (defconst zenburn/red       "#CC9393"  "Zenburn palette: #CC9393.")
; (defconst zenburn/red-1     "#BC8383"  "Zenburn palette: #BC8383.")
; (defconst zenburn/red-2     "#AC7373"  "Zenburn palette: #AC7373.")
; (defconst zenburn/red-3     "#9C6363"  "Zenburn palette: #9C6363.")
; (defconst zenburn/red-4     "#8C5353"  "Zenburn palette: #8C5353.")
; (defconst zenburn/yellow    "#F0DFAF"  "Zenburn palette: #F0DFAF.")
; (defconst zenburn/yellow-1  "#E0CF9F"  "Zenburn palette: #E0CF9F.")
; (defconst zenburn/yellow-2  "#D0BF8F"  "Zenburn palette: #D0BF8F.")
; 

;-------------{Malb theme}-------------;

(package-install 'solarized-theme)					  
;; malb theme
(defun malb/load-main-theme ()
                (load-theme 'solarized-light t)
                (when (boundp 'hl-sentence-mode)
                  (set-face-attribute 'hl-sentence-face nil :background (solarized-with-color-variables
                                                                          'light
                                                                          (solarized-color-blend base02 base03 0.6))))
                (when (boundp 'which-func-mode)
                  (set-face-attribute 'which-func nil :foreground "#DEB542"))



                (set-face-attribute 'highlight-indent-guides-even-face nil :background (solarized-with-color-variables 'light base02))
                (set-face-attribute 'highlight-indent-guides-odd-face  nil :background (solarized-with-color-variables 'light base02)))



;-----{Cycle between color themes}-----;

;; Cycle between themes using f12 http://blog.modelworks.ch/switching-between-emacs-color-themes/
(package-install 'color-theme)
(try-require 'color-theme)
;(color-theme-initialize)
 
;(setq my-color-themes (list 'color-theme-shaman
;                            'malb/load-main-theme
;                            'malb/load-mode-line-theme
;                              ))


;(defun my-theme-set-default () ; Set the first row
;      (interactive)
;      (setq theme-current my-color-themes)
;      (funcall (car theme-current)))
; 
;    (defun my-describe-theme () ; Show the current theme
;      (interactive)
;      (message "%s" (car theme-current)))
; 
;   ; Set the next theme (fixed by Chris Webber - thanks)
;    (defun my-theme-cycle ()
;      (interactive)
;      (setq theme-current (cdr theme-current))
;      (if (null theme-current)
;      (setq theme-current my-color-themes))
;      (funcall (car theme-current))
;      (message "%S" (car theme-current)))
; 
;    (setq theme-current my-color-themes)
;    (setq color-theme-is-global nil) ; Initialization
;    (my-theme-set-default)
;    (global-set-key [f12] 'my-theme-cycle)
	
;; hook for modes
(add-hook 'markdown-mode-hook
  (lambda ()
    (set-frame-parameter (window-frame) 'background-mode 'dark)
    (enable-theme 'solarized)))


;--------{Highlight selections}--------;

;; Highlight line number ;; some bugs
;(package-install 'hlinum)
;(require 'hlinum)
;(hlinum-activate)


;; Rainbow mode for colours str representations
(package-install 'rainbow-mode)
(require 'rainbow-mode)
(use-package rainbow-mode
  :ensure t
  :config (progn
            (add-hook 'emacs-lisp-mode-hook #'rainbow-mode))
  :diminish rainbow-mode)


;;https://github.com/antonj/Highlight-Indentation-for-Emacs
(package-install 'highlight-indentation)
(require 'highlight-indentation)
(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :config (progn (setq highlight-indent-guides-method 'column)
                 (add-hook 'python-mode-hook #'highlight-indent-guides-mode)))

;; https://github.com/k-talo/volatile-highlights.el
;; in ~/lisp
(package-install 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; https://github.com/fgeller/highlight-thing.el
(package-install 'highlight-thing)
(require 'highlight-thing)
(setq highlight-thing-delay-seconds 0.15) ;; default at 0.5
;(global-highlight-thing-mode)

;;https://github.com/nschum/highlight-symbol.el
(package-install 'highlight-symbol)
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)


;--------{Enforce a line limit}--------;

;;https://github.com/jordonbiondo/column-enforce-mode
(package-install 'column-enforce-mode)
(require 'column-enforce-mode)
(setq column-enforce-column 80)
;(add-hook 'python-mode-hook 'column-enforce-hook)
(setq column-enforce-comments nil)


;------{Show the number of chars}------;

;; https://github.com/xuchunyang/region-state.el
(package-install 'region-state)
(require 'region-state)
(add-hook 'rectangle-mark-mode 'region-state-mode)
(add-hook 'text-mode 'region-state-mode)


;--------{Fill selected column}--------;

;; http://www.lunaryorn.com/ : unsure
;(package-install 'visual-fill-column)
;(use-package visual-fill-column
;  :ensure t
;  :defer t
;  :bind (("C-c t v" . visual-fill-column-mode))
;  :init
;  (dolist (hook '(visual-line-mode-hook
;                  prog-mode-hook
;                  text-mode-hook))
;    (add-hook hook #'visual-fill-column-mode))
;  :config (setq-default visual-fill-column-center-text t
;                        visual-fill-column-fringes-outside-margins nil))


;---------{Avoid line clutter}---------;

;; Diminish other modes to avoid line clutter https://github.com/emacsmirror/diminish
(package-install 'diminish)
(require 'diminish)
(diminish 'highlight-thing-mode)
(diminish 'volatile-highlights-mode)
(diminish 'highlight-parentheses-mode)
(use-package diminish
  :ensure t) ;; to use as :diminish in use packages

;--------------{Use icons}-------------;

;;https://github.com/domtronn/all-the-icons.el all the icons 

(package-install 'all-the-icons)
(require 'all-the-icons)
(use-package all-the-icons
  :ensure t)

;----------{Use lambda symbol}---------;
; 		   
; ; real lisp hackers use the lambda character
; ; courtesy of stefan monnier on c.l.l
; (defun sm-lambda-mode-hook ()
;   (font-lock-add-keywords
;    nil `(("\\<lambda\\>"
;    (0 (progn (compose-region (match-beginning 0) (match-end 0)
;         ,(make-char 'greek-iso8859-7 107))
;       nil))))))
; (add-hook 'emacs-lisp-mode-hook 'sm-lambda-mode-hook)
; (add-hook 'lisp-interactive-mode-hook 'sm-lamba-mode-hook)
; (add-hook 'scheme-mode-hook 'sm-lambda-mode-hook)
; (add-hook 'python-mode-hook 'sm-lamba-mode-hook)
;
;; https://github.com/emacsmirror/pretty-mode for unicode representations very slow
;(package-install 'pretty-mode)
;(require 'pretty-mode)
; if you want to set it globally
;(global-pretty-mode t)


;-----------{If you miss vim}----------;
		
;; Vim empty line indicator : Remove due to clash with hl-diff mode 

;(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
;(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
;(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
;(setq-default indicate-empty-lines t)


;=========================={Dynamically switch fonts}==========================;

; (defvar Input-font '(:family "Input" :size 10))
; (defvar PragmataPro-font '(:family "Essential PragmataPro" :size 12))
; (defvar Go-font '(:family "Go Mono" :size 12))
; (defvar Terminus-font '(:family "Terminus (TTF)" :size 12))
; (defvar DOS-font '(:family "Perfect DOS VGA 437" :size 13))
; 
; (set-frame-font (apply 'font-spec PragmataPro-font) nil t)
; 
; (when *is-a-mac*
;   (set-fontset-font
;      t 'symbol
;      (font-spec :family "Apple Color Emoji") nil 'prepend))
; 
; (defun my-switch-font (font)
;   (interactive "sSwitch font (1. PragmataPro 2. Go Mono 3. Terminus 4. DOS): ")
;   (cond ((string= font "1") (set-frame-font (apply 'font-spec PragmataPro-font) nil t))
;         ((string= font "2") (set-frame-font (apply 'font-spec Go-font) nil t))
;         ((string= font "3") (set-frame-font (apply 'font-spec Terminus-font) nil t))
;         ((string= font "4") (set-frame-font (apply 'font-spec DOS-font) nil t))
;         (t (message "Invalid option. Please choose 1 or 2."))))


(provide 'sdev-cosmetic)
