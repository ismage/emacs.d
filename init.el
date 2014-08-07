;;; global setting

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

;; フォルダ検索
(define-key global-map (kbd "C-c C-f") 'grep-find)

;; タブをスペースで扱う
(setq-default tab-width 4 indent-tabs-mode nil)

;; helm-ls-git
(define-key global-map (kbd "C-x C-q") 'helm-ls-git-ls)
;; helm-ls-git起動時にフルパス表示をデフォルトにする
(setq-default helm-ls-git-show-abs-or-relative 'absolute)

;; magit-status
(define-key global-map (kbd "C-x C-g") 'magit-status)

;; 行間設定
(setq-default line-spacing 0.1)

(require 'cask "~/.cask/cask.el")
(cask-initialize)


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
;; タイトルバーにファイルパスを表示させる
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))
;; 画面サイズ
(set-frame-size (selected-frame) 144 54)
  (set-face-attribute 'default nil :family "Ricty" :height 120)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
  (set-fontset-font nil 'katakana-jisx0201 (font-spec :family "Ricty"))
;; フレーム透過
    (progn
      (set-frame-parameter nil 'alpha 90))
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


;; git-gutter-fringe
(require 'git-gutter-fringe)
(global-git-gutter-mode)

;;;;;; scratch ;;;;;;
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

(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))

(defun read-scratch-data ()
  (let ((file "~/.emacs.d/backups/.scratch"))
    (when (file-exists-p file)
      (set-buffer (get-buffer "*scratch*"))
      (erase-buffer)
      (insert-file-contents file))
    ))

(read-scratch-data)

;;;;;; end scratch ;;;;;;
(require 'auto-complete-config)
(global-auto-complete-mode t)

(require 'popwin)
(popwin-mode 1)
(push "*grep*" popwin:special-display-config)
(push '("\\*magit: .*\\*" :regexp t) popwin:special-display-config)

(require 'flycheck)
(global-flycheck-mode t)

;; ruby mode
(add-hook 'ruby-mode-hook
          '(lambda ()
             (robe-mode)
             (robe-ac-setup)))


