;;;; global setting

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
;; 終了時にオートセーブファイル（#filename#）の削除
(setq delete-auto-save-files t)
;; dired で選択したファイルを開くウィンドウを別にする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; カーソルの形を変更 box,hollow,hbar
(setq-default cursor-type 'bar)
;; window switch
(define-key global-map (kbd "C-`") 'other-window)
;; delete other window
(define-key global-map (kbd "C-1") 'delete-other-windows)

;; dired
(define-key global-map (kbd "M-d") 'dired)
;; init.el再読み込み
;;(define-key global-map (kbd "<f5>") 'eval-buffer)

;; 検索
(setq grep-find-command '("find . -name '*.log' -prune -o -type f -exec grep -nH -e  {} +"))

;; フォルダ検索
(define-key global-map (kbd "C-c C-f") 'grep-find)

;; タブをスペースで扱う
(setq-default tab-width 4 indent-tabs-mode nil)

;; magit-status
(define-key global-map (kbd "C-x C-g") 'magit-status)

;; 行間設定
(setq-default line-spacing 0.1)

;; cask && pallet
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

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
(set-frame-size (selected-frame) 210 54)
  (set-face-attribute 'default nil :family "Ricty" :height 120)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
  (set-fontset-font nil 'katakana-jisx0201 (font-spec :family "Ricty"))
;; フレーム透過
    (progn
      (set-frame-parameter nil 'alpha 90))

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

(require 'popwin)
(popwin-mode 1)
;; * hoge *形式のbufferは全てpopwinで開く
(push '("^\\*.*\\*$" :regexp t) popwin:special-display-config)

(require 'flycheck)
(global-flycheck-mode t)

;; ruby mode
(add-hook 'ruby-mode-hook
          '(lambda ()
             (robe-mode)
             (robe-ac-setup)))

;; rainbow delimiters
(global-rainbow-delimiters-mode)

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
