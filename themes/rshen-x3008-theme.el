(deftheme rshen-x3008
  "Created 2019-12-21.")

(custom-theme-set-faces
 'rshen-x3008
 '(default ((t (:family "Fira Code" :foundry "nil" :width normal :height 180 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "#ffffff" :background "#002451" :stipple nil :inherit nil))))
 '(cursor ((t (:background "#ff9da4"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "#bbdaff"))))
 '(highlight ((t (:inverse-video nil :background "#00346e"))))
 '(region ((t (:inverse-video nil :background "#003f8e"))))
 '(shadow ((t (:foreground "#7285b7"))))
 '(secondary-selection ((t (:background "#00346e"))))
 '(trailing-whitespace ((t (:inherit (whitespace-trailing)))))
 '(font-lock-builtin-face ((t (:foreground "#ebbbff"))))
 '(font-lock-comment-delimiter-face ((t (:slant italic :foreground "#7285b7"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#7285b7"))))
 '(font-lock-constant-face ((t (:foreground "#bbdaff"))))
 '(font-lock-doc-face ((t (:foreground "#ebbbff"))))
 '(font-lock-function-name-face ((t (:foreground "#ffc58f"))))
 '(font-lock-keyword-face ((t (:foreground "#d1f1a9"))))
 '(font-lock-negation-char-face ((t (:foreground "#bbdaff"))))
 '(font-lock-preprocessor-face ((t (:foreground "#ebbbff"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#ffeead"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#ebbbff"))))
 '(font-lock-string-face ((t (:foreground "#99ffff"))))
 '(font-lock-type-face ((t (:foreground "#bbdaff"))))
 '(font-lock-variable-name-face ((t (:foreground "#ffeead"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#ff9da4"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:foreground "#7285b7" :background "#00002c2c5fdf"))))
 '(header-line ((t (:foreground "#99ffff" :inherit (mode-line-inactive)))))
 '(tooltip ((t (:inverse-video t :foreground "#ffeead" :background "#002451"))))
 '(mode-line ((t (:weight normal :box (:line-width 1 :color "#003f8e" :style nil) :inverse-video nil :foreground "gray60" :background "black"))))
 '(mode-line-buffer-id ((t (:inherit (sml/filename)))))
 '(mode-line-emphasis ((t (:slant italic :foreground "#ffffff"))))
 '(mode-line-highlight ((t (:weight bold :box nil :foreground "#ebbbff"))))
 '(mode-line-inactive ((t (:weight normal :inverse-video nil :foreground "gray60" :background "#404045" :inherit (mode-line)))))
 '(isearch ((t (:inverse-video t :foreground "#ffeead" :background "#002451"))))
 '(isearch-fail ((t (:inverse-video t :background "#002451" :inherit (font-lock-warning-face)))))
 '(lazy-highlight ((t (:inverse-video t :foreground "#99ffff" :background "#002451"))))
 '(match ((t (:inverse-video t :foreground "#bbdaff" :background "#002451"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'rshen-x3008)
