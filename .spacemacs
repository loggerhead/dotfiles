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
     gtags
     github
     markdown
     org
     ;; spell-checking
     syntax-checking
     fasd
     python
     rust
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-complete-with-key-sequence-delay 0.05
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     (version-control :variables
                      vc-follow-symlinks t
                      version-control-global-margin t
                      version-control-diff-tool 'diff-hl)
     (shell :variables
            shell-default-shell 'eshell
            shell-protect-eshell-prompt t)
     )
   dotspacemacs-additional-packages
   '(
     color-theme-solarized
     xterm-color
     eshell-prompt-extras
     helm-swoop
     highlight-thing
     windsize
     hideshow
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
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         monokai
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Monaco"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
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
   dotspacemacs-loading-progress-bar nil

   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup t

   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90

   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "https://elpa.zilongshanren.com/melpa/")
          ("org-cn"   . "https://elpa.zilongshanren.com/org/")
          ("gnu-cn"   . "https://elpa.zilongshanren.com/gnu/")))

  (setq-default rust-enable-racer t)
  (setq company-tooltip-align-annotations t)
  )

(defun dotspacemacs/user-config ()
  (defun my-switch-to-recent-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer)))

  (defun my-fold ()
    (interactive)
    (let ((lv (- (read-char) ?0)))
      (hs-hide-level lv)))

  (defun my-comment-line ()
    (interactive)
    (comment-line))

  ;; powerline
  (setq powerline-default-separator 'bar)
  (spaceline-compile)
  ;; load theme
  (load-theme 'solarized t)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (let ((mode (if (display-graphic-p frame) 'light 'dark)))
                (set-frame-parameter frame 'background-mode mode)
                (set-terminal-parameter frame 'background-mode mode))
              (enable-theme 'solarized)))

  ;; global mode
  (global-company-mode t)
  (global-auto-revert-mode t)
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t)

  ;; eshell
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-dakrone "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-dakrone))

  ;; settings
  (setq-default spacemacs-show-trailing-whitespace nil)
  (setq highlight-thing-what-thing 'sexp)
  ;; TODO: replace
  ;; hideshow
  (setq hs-hide-comments nil)
  (setq hs-isearch-open 't)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  ;; (add-hook 'rust-mode-hook 'hs-minor-mode)

  ;; keymap
  (global-set-key (kbd "C-=") 'er/expand-region)

  (evil-leader/set-key "j c" 'evil-avy-goto-char)
  (evil-leader/set-key "j l" 'evil-avy-goto-line)
  (evil-leader/set-key "j w" 'evil-avy-goto-word-1)

  (global-set-key (kbd "s-S-z") 'undo-tree-redo)

  (global-set-key (kbd "C-;") 'evilnc-comment-operator)

  (evil-leader/set-key "h c" 'apropos)
  (evil-leader/set-key "s r" 'helm-imenu)
  (global-set-key (kbd "C-\\") 'find-file-at-point)
  (global-set-key (kbd "C-x b") 'my-switch-to-recent-buffer)

  (evil-leader/set-key "F" 'my-fold)

  (evil-leader/set-key "b n" 'spacemacs/new-empty-buffer)
  (evil-leader/set-key "b p" 'my-switch-to-recent-buffer)

  (evil-leader/set-key "w -" 'split-window-below-and-focus)
  (evil-leader/set-key "w /" 'split-window-right-and-focus)

  (evil-leader/set-key "w ," 'windsize-up)
  (evil-leader/set-key "w ." 'windsize-down)
  (evil-leader/set-key "w <" 'windsize-left)
  (evil-leader/set-key "w >" 'windsize-right)

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
    (helm-gtags ggtags gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md magit-popup company-statistics company-quickhelp pos-tip company yasnippet ac-ispell auto-complete multiple-cursors git-commit with-editor hackernews hlinum color-theme-solarized color-theme xterm-color shell-pop multi-term eshell-prompt-extras esh-help windsize windresize origami yafolding highlight-thing vimish-fold org-pomodoro alert log4e toc-org org-repo-todo org-present gntp org-plus-contrib org-bullets htmlize gnuplot toml-mode racer rust-mode pyvenv pytest pyenv-mode py-yapf pip-requirements magit-gh-pulls hy-mode helm-pydoc github-clone github-browse-file git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht flycheck-rust flycheck-pos-tip flycheck fasd grizzl erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks diff-hl cython-mode company-racer deferred company-anaconda anaconda-mode pythonic f smeargle reveal-in-osx-finder pbcopy osx-trash orgit mmm-mode markdown-toc markdown-mode magit-gitflow launchctl helm-gitignore request helm-company helm-c-yasnippet evil-magit magit auto-yasnippet ws-butler window-numbering volatile-highlights vi-tilde-fringe spaceline s powerline smooth-scrolling restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox hydra spinner page-break-lines open-junk-file neotree move-text macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu eval-sexp-fu highlight elisp-slime-nav define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm which-key use-package spacemacs-theme quelpa popup helm-core evil bind-map avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
