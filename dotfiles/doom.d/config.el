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
(setq! doom-font (font-spec
                  :family "Inconsolata Nerd Font Mono"
                  :size 14
                  :weight 'medium)
       doom-variable-pitch-font (font-spec
                                 :family "Inconsolata Nerd Font"
                                 :size 14
                                 :weight 'medium))

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
  :custom (alert-default-style (if IS-MAC 'osx-notifier 'notifications)))

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
(add-hook 'TeX-mode-hook (lambda ()
                           (setq! TeX-electric-math (cons "\\(" ""))
                           (setq TeX-quote-after-quote t)
                           (sp-with-modes '(
                                            tex-mode
                                            plain-tex-mode
                                            latex-mode
                                            LaTeX-mode
                                            )
                             (sp-local-pair "``" nil
                                            :actions :rem))
                           ))

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

(map! :leader
      :desc "Go to previous empty line"
      "{" #'go-prev-empty-line)

(map! :leader
      :desc "Go to next empty line"
      "}" #'go-next-empty-line)

(map! :after latex
      :textobj "C" #'evil-tex-inner-command #'evil-tex-a-command)

(dotimes (n 10) (global-set-key (kbd (format "C-s-%d" n)) 'centaur-tabs-select-visible-tab))

(defun go-next-empty-line ()
  (interactive)
  (evil-next-visual-line)
  (re-search-forward "^[[:blank:]]*$"))

(defun go-prev-empty-line ()
  (interactive)
  (evil-previous-visual-line)
  (re-search-backward "^[[:blank:]]*$"))

(defun fv ()
  (set-frame-position (selected-frame) 583 0)
  (set-frame-size (selected-frame) 120 63))

(when IS-MAC (fv))

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

;; lsp-mode IS the name of the package, we're NOT referencing the mode.
;; (after! lsp-mode (setq! lsp-enable-symbol-highlighting nil))
(after! org (setq! org-tags-column -80))

(defvar-local coc-extensions (expand-file-name "~/.config/coc/extensions"))
(defvar-local coc-clangd-bin "/coc-clangd-data/install/11.0.0/clangd_11.0.0/bin/clangd")
(defvar-local coc-kotlin-bin "/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (executable-find "zls"))
    :major-modes '(zig-mode)
    :server-id 'zls)))

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
  (alert (xah-asciify-string msg) :title (xah-asciify-string title)))

;; designate the window function for my-appt-send-notification
(defun appt-display (min-to-app _ msg)
  (send-notification
   (format "Task in %s minutes" (substring-no-properties min-to-app))
   (substring-no-properties msg)))

(setq appt-disp-window-function (function appt-display))

(defun xah-asciify-text (&optional @begin @end)
  "Remove accents in some letters and some
   Change European language characters into equivalent ASCII ones, e.g. “café” ⇒ “cafe”.
   When called interactively, work on current line or text selection.

   URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
   Version 2018-11-12"  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
          [" " " "]       ; thin space etc
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
          ])
        $begin $end
        )
    (if (null @begin)
        (if (use-region-p)
            (setq $begin (region-beginning) $end (region-end))
          (setq $begin (line-beginning-position) $end (line-end-position)))
      (setq $begin @begin $end @end))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $begin $end)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

(defun xah-asciify-string (@string)
  "Returns a new string. European language chars are changed ot ASCII ones e.g. “café” ⇒ “cafe”.
   See `xah-asciify-text'
   Version 2015-06-08"
  (with-temp-buffer
    (insert @string)
    (xah-asciify-text (point-min) (point-max))
    (buffer-string)))
