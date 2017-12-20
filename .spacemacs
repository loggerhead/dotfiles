;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     erc
     better-defaults
     emacs-lisp
     osx
     git
     html
     gtags
     github
     csv
     yaml
     markdown
     org
     fasd
     python
     docker
     dockerfile
     rust
     ycmd
     spell-checking
     syntax-checking
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-return-key-behavior 'complete
                      auto-completion-complete-with-key-sequence-delay 0.01
                      auto-completion-enable-sort-by-usage t)
     (version-control :variables
                      vc-follow-symlinks t
                      version-control-global-margin t
                      version-control-diff-tool 'diff-hl)
     (shell :variables
            shell-default-shell 'eshell
            shell-protect-eshell-prompt t)
     (colors :variables
             colors-colorize-identifiers nil
             colors-enable-nyan-cat-progress-bar t)
     )
   dotspacemacs-additional-packages
   '(
     helm-swoop
     youdao-dictionary
     highlight-thing
     hideshow
     color-theme-solarized
     xterm-color
     eshell-prompt-extras
     windsize
     hackernews
     )
   dotspacemacs-excluded-packages
   '(
     rainbow-delimiters
     )
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Monaco"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key "'"
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil

   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom

   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.3
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t

   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup t

   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90

   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  (fset 'xterm-color-unfontify-region 'font-lock-default-unfontify-region)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "https://elpa.zilongshanren.com/melpa/")
          ("org-cn"   . "https://elpa.zilongshanren.com/org/")
          ("gnu-cn"   . "https://elpa.zilongshanren.com/gnu/")))

  (setq-default rust-enable-racer t)
  (setq company-tooltip-align-annotations t)
  (custom-set-variables
   '(spacemacs-theme-comment-bg nil)
   '(spacemacs-theme-org-height nil))
  )

(defun dotspacemacs/user-config ()
  (defun my-fold-1 ()
    (interactive)
    (hs-hide-level 1))
  (defun my-fold-2 ()
    (interactive)
    (hs-hide-level 2))
  (defun my-fold-3 ()
    (interactive)
    (hs-hide-level 3))
  (defun my-fold-4 ()
    (interactive)
    (hs-hide-level 4))
  (defun my-scroll-down-line ()
    (interactive)
    (evil-scroll-line-up 3))
  (defun my-scroll-up-line ()
    (interactive)
    (evil-scroll-line-down 3))

  ;; powerline
  (setq powerline-default-separator 'utf-8)
  ;; load theme
  (when (or (eq (car dotspacemacs-themes) 'solarized-dark)
            (eq (car dotspacemacs-themes) 'solarized-light))
    (load-theme 'solarized t)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (let ((mode (if (display-graphic-p frame) 'light 'dark)))
                  (set-frame-parameter frame 'background-mode mode)
                  (set-terminal-parameter frame 'background-mode mode))
                (enable-theme 'solarized))))

  ;; global mode
  (global-hungry-delete-mode t)
  (global-company-mode t)
  (global-diff-hl-mode t)
  (global-highlight-thing-mode t)
  (diff-hl-flydiff-mode t)
  (delete-selection-mode t)

  ;; fuzzy-match setting
  (setq helm-swoop-use-fuzzy-match nil)

  ;; TODO: need test fuzzy search
  ;; ycmd completion
  ;; (setq ycmd-server-command (list "python" (file-truename "~/some/path")))
  ;; (setq ycmd-force-semantic-completion t)
  ;; (add-hook 'prog-mode-hook 'ycmd-mode)
  ;; (add-hook 'c-mode-hook 'ycmd-mode)
  ;; (add-hook 'c++-mode-hook 'ycmd-mode)
  ;; (add-hook 'rust-mode-hook 'ycmd-mode)
  ;; (add-hook 'python-mode-hook 'ycmd-mode)

  ;; eshell
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-dakrone "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-dakrone))

  (setq-default spacemacs-show-trailing-whitespace nil)

  ;; complete
  (setq hippie-expand-try-function-list '(try-expand-debbrev
                                          try-expand-debbrev-all-buffers
                                          try-expand-debbrev-from-kill
                                          try-complete-file-name-partially
                                          try-complete-file-name
                                          try-expand-all-abbrevs))

  ;; org
  (setq org-agenda-files '("~/org"))

  ;; highlight-thing
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-delay-seconds 0.1)
  (setq highlight-thing-limit-to-defun t)
  (setq highlight-thing-case-sensitive-p t)

  ;; TODO: replace to more powerful plugin
  ;; hideshow
  (setq hs-hide-comments nil)
  (setq hs-isearch-open 't)
  (add-hook 'prog-mode-hook 'hs-minor-mode)

  ;; nyan cat
  (setq nyan-wavy-trail nil)
  (setq nyan-animate-nyancat nil)

  ;; bookmarks
  (defadvice bookmark-jump (after bookmark-jump activate)
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (delq latest bookmark-alist))
      (add-to-list 'bookmark-alist latest)))

  ;; popwin
  (push "*Warnings*" popwin:special-display-config)

  ;; youdao-dict
  (setq url-automatic-caching t)
  (setq youdao-dictionary-search-history-file "~/.spacemacs.d/youdao_history")
  (push "*Youdao Dictionary*" popwin:special-display-config)
  (spacemacs/declare-prefix "d" "dictionary")
  (spacemacs/set-leader-keys
    "d s" 'youdao-dictionary-search-at-point+
    "d i" 'youdao-dictionary-search-from-input
    "d v" 'youdao-dictionary-play-voice-at-point)

  ;; key-binding
  (global-set-key (kbd "s-x") 'spacemacs/backward-kill-word-or-region)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "s-S-z") 'undo-tree-redo)
  (global-set-key (kbd "C-;") 'evilnc-comment-operator)
  (global-set-key (kbd "C-\\") 'find-file-at-point)
  ;; (global-set-key (kbd "M-;") 'yas-expand)

  (global-set-key (kbd "C-S-h f") 'describe-function)
  (global-set-key (kbd "C-S-h v") 'describe-variable)
  (global-set-key (kbd "C-S-h k") 'describe-key)
  (global-set-key (kbd "C-S-h m") 'describe-mode)

  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
  (evil-define-key 'normal evil-normal-state-map "," 'evil-repeat-find-char-reverse)

  (define-key evil-normal-state-map (kbd "C-b") 'backward-char)
  (define-key evil-normal-state-map (kbd "C-f") 'forward-char)
  (define-key evil-normal-state-map (kbd "C-n") 'next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'previous-line)
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-normal-state-map (kbd "C-k") 'my-scroll-down-line)
  (define-key evil-normal-state-map (kbd "C-j") 'my-scroll-up-line)
  (define-key evil-normal-state-map (kbd "M-S-d") 'kill-sexp)
  (define-key evil-hybrid-state-map (kbd "C-h") 'backward-delete-char-untabify)
  (define-key evil-normal-state-map (kbd "C-;") 'evilnc-comment-operator)
  (define-key evil-hybrid-state-map (kbd "C-;") 'evilnc-comment-operator)

  ;; jump to functions
  (spacemacs/set-leader-keys
    "s r" 'helm-imenu)

  (spacemacs/set-leader-keys
    "g <tab>" 'magit-checkout
    "g B" 'magit-branch-popup
    "g a" 'magit-stage-file
    "g A" 'spacemacs//vcs-magit-stage-file-w
    "g d" 'magit-diff-unstaged
    "g D" 'magit-diff-popup
    "g p" 'magit-push-current-to-upstream
    "g P" 'magit-push-popup
    "g l" 'magit-log-all
    "g L" 'magit-log-popup
    "g c" 'magit-commit
    "g C" 'magit-commit-popup
    "g T" 'magit-tag-popup
    "g M" 'magit-merge-popup
    "g r" 'magit-remote-popup
    "g R" 'magit-reset-popup)

  (spacemacs/declare-prefix "B" "bookmark")
  (spacemacs/set-leader-keys
    "B B" 'helm-bookmarks
    "B s" 'bookmark-save
    "B d" 'bookmark-delete
    "B j" 'bookmark-jump
    "B l" 'bookmark-bmenu-list
    "B s" 'bookmark-set)

  (spacemacs/declare-prefix "j" "jump")
  (spacemacs/set-leader-keys
    "j j" 'evil-avy-goto-word-1
    "j d" (kbd "' g g")
    "j c" 'evil-avy-goto-char
    "j l" 'evil-avy-goto-line
    "j w" 'evil-avy-goto-word-1)

  (spacemacs/declare-prefix "F" "fold")
  (spacemacs/set-leader-keys
    "F 1" 'my-fold-1
    "F 2" 'my-fold-2
    "F 3" 'my-fold-3
    "F 4" 'my-fold-4
    "F 0" 'hs-show-all
    "F f" 'hs-hide-block
    "F s" 'hs-show-block)

  (spacemacs/set-leader-keys
    "b n" 'spacemacs/new-empty-buffer
    "b p" 'my-switch-to-recent-buffer)

  (spacemacs/set-leader-keys
    "w -" 'split-window-below-and-focus
    "w /" 'split-window-right-and-focus
    "w ," 'windsize-up
    "w ." 'windsize-down
    "w <" 'windsize-left
    "w >" 'windsize-right)

  (global-set-key (kbd "C-S-j") 'move-text-down)
  (global-set-key (kbd "C-S-k") 'move-text-up)

  ;; TODO: multiple cursors
  ;; (global-set-key (kbd "C-S-<down>") 'add-cursor-down)
  ;; (global-set-key (kbd "C-S-<up>") 'add-cursor-up)
  )



;; Do NOT write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(package-selected-packages
   (quote
    (web-mode tagedit slim-mode scss-mode sass-mode less-css-mode jade-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data dockerfile-mode bind-key undo-tree async package-build yaml-mode flycheck-ycmd company-ycmd ycmd request-deferred company-flx eclim auto-dim-other-buffers rainbow-mode rainbow-identifiers youdao-dictionary helm-flyspell auto-dictionary helm-gtags ggtags gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md magit-popup company-statistics company-quickhelp pos-tip company yasnippet ac-ispell auto-complete multiple-cursors git-commit with-editor hackernews hlinum color-theme-solarized color-theme xterm-color shell-pop multi-term eshell-prompt-extras esh-help windsize windresize origami yafolding highlight-thing vimish-fold org-pomodoro alert log4e toc-org org-repo-todo org-present gntp org-plus-contrib org-bullets htmlize gnuplot toml-mode racer rust-mode pyvenv pytest pyenv-mode py-yapf pip-requirements magit-gh-pulls hy-mode helm-pydoc github-clone github-browse-file git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht flycheck-rust flycheck-pos-tip flycheck fasd grizzl erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks diff-hl cython-mode company-racer deferred company-anaconda anaconda-mode pythonic f smeargle reveal-in-osx-finder pbcopy osx-trash orgit mmm-mode markdown-toc markdown-mode magit-gitflow launchctl helm-gitignore request helm-company helm-c-yasnippet evil-magit magit auto-yasnippet ws-butler window-numbering volatile-highlights vi-tilde-fringe spaceline s powerline smooth-scrolling restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox hydra spinner page-break-lines open-junk-file neotree move-text macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu eval-sexp-fu highlight elisp-slime-nav define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm which-key use-package spacemacs-theme quelpa popup helm-core evil bind-map avy)))
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-org-height nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
