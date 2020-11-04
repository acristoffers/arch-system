;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq! user-full-name "Álan Crístoffer e Sousa"
       user-mail-address "acristoffers@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! julia-formatter)
(use-package! zig-mode)

(setq! lsp-clients-clangd-executable "/Users/Alan/.config/coc/extensions/coc-clangd-data/install/10.0.0/clangd_10.0.0/bin/clangd")
(setq! lsp-clients-kotlin-server-executable "/Users/Alan/.config/coc/extensions/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'TeX-mode-hook #'prettify-symbols-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'python-mode-hook (lambda () (format-all-mode -1)))
(add-hook 'julia-mode-hook #'julia-formatter-server-start)

(map! :desc "Run all Jupyter notebook cells"
      :map ein:notebook-mode-map
      "C-c a" #'ein:worksheet-execute-all-cells)

(map! :desc "Stop ein notebook"
      :map ein:notebooklist-mode-map
      "C-c C-s" #'ein:stop)

(map! :leader
      :after evil-org
      :map evil-org-mode-map
      :n "m X" #'org-babel-execute-src-block)

(map! :leader
      :desc "Format code"
      "g =" #'lsp-format-buffer)

(map! :leader
      :desc "Spell action menu"
      "z =" #'flyspell-correct-word-before-point)

(map! :leader
      :desc "Clears search highlight"
      "s c" #'evil-ex-nohighlight)

(set-frame-position (selected-frame) 583 0)
(set-frame-size (selected-frame) 119 62)

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode
            python-mode))

;; (setenv "DICTIONARY" "en")
;; (setenv "LANG" "en")
;; (after! ispell
;;   (let ((dictionaries (string-join (list "en" "en_GB" "en_CA" "pt" "pt_PT" "de"
;;                                          "fr" "it" "ru")
;;                                    ",")))
;;     (setq ispell-dictionary dictionaries)
;;     (ispell-set-spellchecker-params)
;;     (ispell-hunspell-add-multi-dic dictionaries)
;;     (setq ispell-personal-dictionary "~/.hunspell_personal")
;;     (setq flyspell-lazy-idle-seconds 10)
;;     (unless (file-exists-p ispell-personal-dictionary)
;;       (write-region "" nil ispell-personal-dictionary nil 0))))

(after! doom-modeline (display-time))

(defun matlab-to-python ()
  "Turns [0 1; 1 0] into np.array([[0, 1], [1, 0]])"
  (interactive)
  (backward-up-list)
  (mark-sexp)
  (if (use-region-p)
      (let* ((numpy (->> (buffer-substring (region-beginning) (region-end))
                         (s-replace "[" "")
                         (s-replace "]" ";")
                         (s-replace-regexp "[ ;]*;[ ;]*" ";")
                         (s-replace-regexp ";$" "")
                         (s-replace " " ",")
                         (s-replace-regexp ",+" ",")
                         (s-replace ";" "],[")
                         (s-replace-regexp "^," "")
                         (s-replace "," ", ")
                         )))
        (replace-region-contents
         (region-beginning)
         (region-end)
         (lambda () (concat "np.array([[" numpy "]])"))))))

(map! :leader
      :desc "Turns MATLAB array into NumPy array"
      "m p"
      #'matlab-to-python)

(after! projectile
  (projectile-register-project-type 'latex '("latexmkrc")
                                    :compilation-dir "."
                                    :project-file "latexmkrc"
                                    :compile "latexmk"))

(after! lsp (setq lsp-enable-symbol-highlighting nil))

(setq lsp-julia-default-environment "~/.julia/environments/v1.5")
(setq lsp-julia-package-dir nil)

(setq lsp-enable-folding t)
(setq lsp-folding-range-limit 100)

;; In Julia, run
;; ] add https://github.com/julia-vscode/LanguageServer.jl

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

;; Inline images in EIN
(setq ein:output-area-inlined-images t)
