;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuratio'relativen 'relativelayers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
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
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(
     rainbow-delimiters
     )
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "called at the very startup of Spacemacs initialization before layers configuration."
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update nil
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(projects recents)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "called immediately after `dotspacemacs/init', before layer configuration executes.
 This function is mostly useful for variables that need to be set before packages are loaded."
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
  "called at the very end of Spacemacs initialization after layers configuration."
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

  ;; TODO: replace
  ;; hideshow
  (setq hs-hide-comments nil)
  (setq hs-isearch-open 't)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  ;; (add-hook 'rust-mode-hook 'hs-minor-mode)

  ;; highlight
  (setq highlight-thing-what-thing 'sexp)

  ;; keymap
  (global-set-key (kbd "C-=") 'er/expand-region)

  (evil-leader/set-key "h k" 'describe-key)
  (evil-leader/set-key "h v" 'describe-variable)
  (evil-leader/set-key "h f" 'describe-function)

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
