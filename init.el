
;; global setting

;; 言語設定		     
(set-language-environment 'Japanese)
;; UTF-8		     
(prefer-coding-system 'utf-8)
;; D&D時の挙動		     
(define-key global-map [ns-drag-file] 'ns-find-file)
;; 起動時の画面をスキップ    
(setq inhibit-startup-message t)
;; 起動時の画面のメッセージ部分も消す
(setq initial-scratch-message nil)
;; 行番号を表示		    
(global-linum-mode t)	     
;; 矩形モードオン	     
(cua-mode t)
(setq cua-enable-cua-keys nil) ;デフォルトのキーバインドを無効化
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;; バックアップファイル作成を停止
(setq make-backup-files nil)
;; オートセーブファイルの作成を停止 tishadowで不要なファイルが出来た瞬間におちるから
(setq auto-save-default nil)
;; 保存時にオートセーブファイル（#filename#）の削除
(setq delete-auto-save-files t)
;; dired で選択したファイルを開くウィンドウを別にする
;;(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; カーソルの形を変更 box,hollow,hbar
(setq-default cursor-type 'bar)
;; window switch
(define-key global-map (kbd "C-`") 'other-window)
;; delete other window
(define-key global-map (kbd "C-1") 'delete-other-windows)
(define-key global-map (kbd "C-c C-l") 'goto-line)
;; dired
(define-key global-map (kbd "M-d") 'dired)
;; init.el再読み込み
;;(define-key global-map (kbd "<f5>") 'eval-buffer)

;; カーソル行ハイライト
(global-hl-line-mode t)

(put 'dired-find-alternate-file 'disabled nil)
;; dired バッファを増やさないようにする
(defun dired-up-alternate-directory ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

;;(define-key dired-mode-map (kbd "RET") 'dired-up-alternate-directory)
;;(define-key dired-mode-map (kbd "a") 'dired-find-alternate-file)
;; ディレクトリを先に表示(下記2行ともmacで動作せず)
;;(setq ls-lisp-dirs-first t)
;;(setq dired-listing-switches "-AFl --group-directories-first")

;; 検索
(setq grep-find-command '("find . -name '*.log' -prune -o -type f -exec grep -nH -e  {} +"))

;; フォルダ検索
(define-key global-map (kbd "C-c C-f") 'grep-find)

;; タブをスペースで扱う
(setq-default tab-width 2 indent-tabs-mode nil)

;; magit-status
(define-key global-map (kbd "C-x C-g") 'magit-status)

;; 行間設定
(setq-default line-spacing 0.1)

;; 大文字小文字無視
(setq completion-ignore-case t)
;; buffer自動再読み込み
(global-auto-revert-mode 1)

;; cask && pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(global-undo-tree-mode t)
(global-set-key (kbd "C-?") 'undo-tree-redo)
;; default directory setting
(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

;;;;;; window-systemでの設定 ;;;;;;
(let ((ws window-system))
(cond ((eq ws 'ns)
;; メニューバーを隠す
(menu-bar-mode -1)
;; toolbarを消す
(tool-bar-mode -1)
;; タイトルバーにbufferを表示させる
(setq frame-title-format (format "%%b - Emacs@%s" (system-name)))
;; 画面サイズ
(set-frame-size (selected-frame) 220 54)
  (set-face-attribute 'default nil :family "Ricty" :height 120)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
  (set-fontset-font nil 'katakana-jisx0201 (font-spec :family "Ricty"))
;; フレーム透過 透過させると負荷が高くなるきがするのでとりあえずやめる
;;    (progn
;;      (set-frame-parameter nil 'alpha 90))

;; MacのPATHをemacsに引き継がせる
(exec-path-from-shell-initialize)

;; scroll speed 調整
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;com.apple.inputmethod.Kotoeri.Japanese
;;(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
(defvar is_inline-patch (eq (boundp 'mac-input-method-parameters) t))
(when is_inline-patch
  (setq default-input-method "MacOSX")
  (mac-set-input-method-parameter "com.apple.inputmethod.Kotoeri.Japanese" `title "え")
  (mac-set-input-method-parameter "com.apple.inputmethod.Kotoeri.Japanese" `cursor-type 'hbar)
  (mac-set-input-method-parameter "com.apple.inputmethod.Kotoeri.Japanese" `cursor-color "red")
  (mac-set-input-method-parameter "com.apple.inputmethod.Kotoeri.Roman" `title "A")
  (mac-set-input-method-parameter "com.apple.inputmethod.Kotoeri.Roman" `cursor-type 'bar)
  (mac-set-input-method-parameter "com.apple.inputmethod.Kotoeri.Roman" `cursor-color "white")
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
  )
;; 通常のミニバッファ
;;(add-hook 'minibuffer-setup-hook '(ime-force-off))
;; /window-system
)))

;; テーマ
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'blackboard t)
;; auto-install setting

;; helm
(require 'helm-mode)
;; helm-mode on
(helm-mode 1)
;; helm-projectile
;; helm C-x C-f tab補完
;;(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
;;(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; helm find-fileを使わないようにする
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;; helm-buffers-list key bind
(define-key global-map (kbd "<C-tab>") 'helm-buffers-list)

;; helm-ls-git
(define-key global-map (kbd "C-x C-q") 'helm-ls-git-ls)
;; helm-ls-git起動時にフルパス表示をデフォルトにする
(setq-default helm-ls-git-show-abs-or-relative 'absolute)
(setq-default helm-ff-transformer-show-only-basename nil)

;; 終了コマンドを無効化
(global-set-key (kbd "C-x C-c") 'helm-M-x)
;; 終了コマンドにエイリアスをつける
(defalias 'exit 'save-buffers-kill-emacs)

(tabbar-mode 1)
(tabbar-mwheel-mode -1)
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

(setq tabbar-buffer-groups-function nil)
(setq tabbar-use-images nil)
(global-set-key (kbd "<M-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<M-S-tab>") 'tabbar-backward-tab)

;; tabbarに*から始まる物を表示しない
(defvar my-tabbar-displayed-buffers
  '("*scratch*")
  "*Regexps matches buffer names always included tabs.")

(defun my-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `my-tabbar-displayed-buffers'
are always included."
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt my-tabbar-displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (or (string-match re name)
                                           (not (memq (aref name 0) hides)))
                                   buf)))
                             (buffer-list)))))
    ;; Always include the current buffer.
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)


;; git-gutter-fringe
(require 'git-gutter-fringe)
(global-git-gutter-mode)

;;;; scratch
(defun save-scratch-data ()
  (let ((str (progn
               (set-buffer (get-buffer "*scratch*"))
               (buffer-substring-no-properties
                (point-min) (point-max))))
        (file "~/.emacs.d/backups/.scratch"))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert str)
    (save-buffer)
    (kill-buffer buf)))
;;; 
(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))
;;; 
(defun read-scratch-data ()
  (let ((file "~/.emacs.d/backups/.scratch"))
    (when (file-exists-p file)
      (set-buffer (get-buffer "*scratch*"))
      (erase-buffer)
      (insert-file-contents file))
    ))

(read-scratch-data)
;;;; end scratch

(require 'auto-complete-config)
(global-auto-complete-mode t)

;; autocompleteの画面が表示されているときだけ有効
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(setq ac-dictionary-directories t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(custom-set-variables
 '(js3-auto-indent-p t)
 '(js3-curly-indent-offset 0)
 '(js3-enter-indents-newline t)
 '(js3-expr-indent-offset 2))

;;(add-hook 'js-mode-hook 'js2-minor-mode)
;;(add-hook 'js3-mode-hook 'ac-js2-mode)
(setq ac-modes (cons 'js3-mode ac-modes))
(add-hook 'js3-mode-hook
          '(lambda ()
             (add-to-list 'ac-dictionary-files "~/.emacs.d/ac-dict/titanium")
))

(require 'direx)
;;; C-x C-jをdirex:dired-jumpと入れ替える
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

;; diredの
(require 'popwin)
(popwin-mode 1)
;; * hoge *形式のbufferは全てpopwinで開く
(push '("^\\*.*\\*$" :regexp t) popwin:special-display-config)
;; direxをpopwinで開く
(push '("^.*<2>$" :regexp t :position left :width 40 :dedicated t) popwin:special-display-config)
(push '(direx:direx-mode :position left :width 40 :dedicated t)
      popwin:special-display-config)

(require 'flycheck)
(global-flycheck-mode t)

;; ruby mode
(add-hook 'ruby-mode-hook
          '(lambda ()
             (robe-mode)
             (robe-ac-setup)))

;; rainbow delimiters
(require 'rainbow-delimiters)
;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;(global-rainbow-delimiters-mode)

;; anzu
(global-anzu-mode t)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000))
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; dash
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

;; cfm
(add-to-list 'auto-mode-alist '("\\.cfm$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cfc$" . html-mode))

;; marked
(defun markdown-preview-file ()  
  "run Marked on the current file and revert the buffer"  
  (interactive)  
  (shell-command  
   (format "open -a /Applications/Marked\\ 2.app %s"  
       (shell-quote-argument (buffer-file-name))))  
)  
(global-set-key "\C-cm" 'markdown-preview-file) 
