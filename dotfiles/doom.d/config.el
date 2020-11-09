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
;; (setq! doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq! doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory (expand-file-name "~/.org/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq! display-line-numbers-type 'visual)

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
(use-package! appt)
(use-package! alert
  :custom (alert-default-style 'osx-notifier))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq! flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'TeX-mode-hook #'prettify-symbols-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'python-mode-hook (lambda () (format-all-mode -1)))
(add-hook 'julia-mode-hook #'julia-formatter-server-start)
(add-hook 'TeX-mode-hook (lambda () (setq! TeX-electric-math (cons "\\(" ""))))

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

(setq! +format-on-save-enabled-modes
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
;;     (setq! ispell-dictionary dictionaries)
;;     (ispell-set-spellchecker-params)
;;     (ispell-hunspell-add-multi-dic dictionaries)
;;     (setq! ispell-personal-dictionary "~/.hunspell_personal")
;;     (setq! flyspell-lazy-idle-seconds 10)
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

(after! lsp (setq! lsp-enable-symbol-highlighting nil))
(after! org (setq! org-tags-column -80))

(defvar-local coc-extensions (expand-file-name "~/.config/coc/extensions"))
(defvar-local coc-clangd-bin "/coc-clangd-data/install/11.0.0/clangd_11.0.0/bin/clangd")
(defvar-local coc-kotlin-bin "/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")

(setq! lsp-clients-clangd-executable (format "%s%s" coc-extensions coc-clangd-bin)
       lsp-clients-kotlin-server-executable (format "%s%s" coc-extensions coc-kotlin-bin)

       ;; aligns annotation to the right hand side
       company-tooltip-align-annotations t

       ;; tells pyright what is the right python executable
       lsp-pyright-python-executable-cmd (executable-find "python3")

       ;; Julia LSP config
       ;; In Julia, run
       ;; ] add https://github.com/julia-vscode/LanguageServer.jl
       lsp-julia-default-environment "~/.julia/environments/v1.5"
       lsp-julia-package-dir nil
       lsp-enable-folding t
       lsp-folding-range-limit 100

       ;; org-journal
       org-journal-dir (expand-file-name "~/.org/journal")
       org-journal-date-prefix "#+TITLE: "
       org-journal-time-prefix "* "
       org-journal-date-format "%a, %Y-%m-%d"
       org-journal-file-format "%Y-%m-%d.org"

       ;; EIN (Jupyter Notebook) config
       ein:output-area-inlined-images t)

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(when IS-MAC
  (setq! mac-option-modifier 'meta)
  (setq! mac-right-option-modifier 'meta))

(setq! appt-time-msg-list nil       ;; clear existing appt list
       appt-display-interval 15     ;; warn every 15 minutes
       appt-message-warning-time 60 ;; send first warning 60 minutes before appointment
       appt-display-mode-line nil   ;; don't show in the modeline
       appt-display-format 'window) ;; pass warnings to the designated window function

(defun agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(add-hook 'after-save-hook
          '(lambda ()
             (if (-contains? (mapcar 'expand-file-name org-agenda-files) (buffer-file-name))
                 (agenda-to-appt))))

(appt-activate t)                                    ;; activate appointment notification
(agenda-to-appt)                                     ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'agenda-to-appt) ;; update appt list on agenda view

;; set up the call to alert
(defun send-notification (title msg)
  (alert msg :title title))

;; designate the window function for my-appt-send-notification
(defun appt-display (min-to-app _ msg)
  (send-notification
   (format "Task in %s minutes" min-to-app)
   msg))

(setq appt-disp-window-function (function appt-display))
