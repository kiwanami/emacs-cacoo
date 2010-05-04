;;; cacoo.el --- Minor mode for Cacoo

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai@kiwanami.net>
;; Version: 1.0
;; Keywords: convenience, diagram

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Cacooで書いた画像をインラインで表示したり、編集画面をさっと開くことのできる
;; マイナーモードです。画像はローカルにキャッシュしていますので、オフラインでも
;; 画像を見ることができます。インライン表示時には自動的に指定のサイズに縮小します。
;; 実際にはCacooだけではなく、URLで参照できてEmacsとImageMagickが解釈できる
;; 図であれば大抵インライン表示できます。

;;; Installation:

;; ロードパスに cacoo.el を置き、以下のように呼び出し用のコードを
;; .emacs などに追加してください。以下の例では、Altキーを押しながら「-」
;; を押すとOn/Offが切り替わるようになります。

;; ;設定例
;; (require 'cacoo)
;; (global-set-key (kbd "M--") 'toggle-cacoo-minor-mode)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize

(defvar cacoo:base-url "https://cacoo.com/diagrams/")
(defvar cacoo:new-url (concat cacoo:base-url "new"))
(defvar cacoo:edit-url (concat cacoo:base-url "%KEY%/edit"))
(defvar cacoo:view-url (concat cacoo:base-url "%KEY%"))
(defvar cacoo:list-url cacoo:base-url)

(defvar cacoo:img-regexp "\\[img:\\([^]]*\\)\\]") ; 本文から画像に置き換える文字列を取ってくる正規表現
(defvar cacoo:key-regexp "diagrams\\/\\([a-zA-Z0-9]*\\)") ; URLからCacooの画像のKeyを取ってくる正規表現

(defvar cacoo:img-dir ".cimg") ; 画像キャッシュフォルダ
(defvar cacoo:img-dir-ok nil) ; 勝手にディレクトリを作って良ければ t

(defvar cacoo:max-size 450) ; 縮小サイズ

(setq cacoo:external-viewer "display") ; 画像

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fundamental Functions

(defmacro cacoo:aif (test-form then-form &rest else-forms)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'ewm:aif 'lisp-indent-function 2)

(defun cacoo:define-keymap (keymap-list)
  (let ((map (make-sparse-keymap)))
    (mapc 
     (lambda (i)
       (define-key map
         (if (stringp (car i))
             (read-kbd-macro (car i)) (car i))
         (cdr i)))
     keymap-list)
    map))

(defun cacoo:get-cache-dir() 
  (let* ((base-dir (file-name-directory 
                    (or buffer-file-name
                        default-directory))))
    (expand-file-name cacoo:img-dir base-dir)))

(defun cacoo:fix-directory ()
  ;;バッファのカレントディレクトリに保存先ディレクトリを作る
  ;;一応作って良いかどうか聞く
  (let* ((img-dir (cacoo:get-cache-dir)))
    (unless (file-directory-p img-dir)
      (when (or cacoo:img-dir-ok 
                (y-or-n-p 
                 (format "Image directory [%s] not found. Create it ?" 
                         cacoo:img-dir)))
        (make-directory img-dir))
      (unless (file-directory-p img-dir)
        (error "Could not create a image directory.")))
    img-dir))

(defun cacoo:get-filename-from-url (url)
  (if (string-match "[^/]*$" url) ; URLがパラメーターの時は怪しい
      (match-string 0 url)
    (error "Wrong URL pattern [%s]" url)))

(defun cacoo:file-exists-p (file)
  (and (file-exists-p file)
       (< 0 (nth 7 (file-attributes file)))))

(defun cacoo:clear-cache-file (file)
  (when (file-exists-p file)
    (delete-file file))
  (cacoo:aif (cacoo:get-resize-path file)
             (when (file-exists-p it)
               (delete-file it))))

(defun cacoo:load-diagram-remote(url pos-start pos-end &optional force-reload)
  (lexical-let* ((pos-start pos-start) (pos-end pos-end)
                 (save-dir (cacoo:fix-directory))
                 (org-path (expand-file-name (cacoo:get-filename-from-url url) save-dir))
                 proc tmpbuf)
    (when force-reload (cacoo:clear-cache-file org-path))
    (cond
     ((cacoo:file-exists-p org-path)
      (cacoo:resize-diagram org-path pos-start pos-end)
      t)
     (t
      (setq tmpbuf (get-buffer-create 
                    (format " *Cacoo:Download-temp:%s*" url)))
      (buffer-disable-undo tmpbuf)
      (setq proc (start-process
                  "Cacoo:wget" tmpbuf "wget"
                  "-q" "-O" org-path url))
      (set-process-sentinel
       proc (lambda(proc event)
              (cond 
               ((string-match "exited abnormally" event)
                (kill-buffer tmpbuf)
                (cacoo:display-diagram-by-text pos-start pos-end
                                               "No network connection."))
               ((equal event "finished\n")
                (kill-buffer tmpbuf)
                (if (cacoo:file-exists-p org-path)
                    (cacoo:resize-diagram org-path pos-start pos-end)
                  (cacoo:display-diagram-by-text 
                   pos-start pos-end "No network connection.")))))) 
      t))))

(defun cacoo:get-local-path-from-url(url)
  (cond
   ((string-match "^file://\\(.*\\)$" url) ; フルパスを仮定
    (match-string 1 url))
   ((cacoo:file-exists-p (expand-file-name url default-directory))
    (expand-file-name url default-directory))
   (t
    (error "Wrong URL pattern [%s]" url))))

(defun cacoo:load-diagram-local(url pos-start pos-end &optional force-reload)
  (let* ((save-dir (cacoo:fix-directory))
         (from-path (cacoo:get-local-path-from-url url))
         (org-path (expand-file-name
                    (cacoo:get-filename-from-url url) 
                    save-dir)))
    (when force-reload (cacoo:clear-cache-file org-path))
    (unless (cacoo:file-exists-p org-path)
      (ignore-errors
        (copy-file from-path org-path t t)))
    (if (cacoo:file-exists-p org-path)
        (cacoo:resize-diagram org-path pos-start pos-end)
      (cacoo:display-diagram-by-text
       pos-start pos-end "Could not copy a file."))))

(defun cacoo:get-resize-path (path)
  (expand-file-name
   (format "resize_%s" (file-name-nondirectory path))
   (file-name-directory path)))

(defun cacoo:get-image-size (file)
  (let ((ret (get-buffer-create " *Cacoo:identify*")) line)
    (buffer-disable-undo ret)
    (call-process "identify" nil ret nil "-format"
                  "%[fx:w] %[fx:h]" file)
    (prog1
        (with-current-buffer ret
          (let* ((line (buffer-string))
                 (cols (split-string line " "))
                 (width (string-to-int (car cols)))
                 (height (string-to-int (cadr cols))))
            (max width height)))
      (kill-buffer ret))))

(defun cacoo:resize-diagram (org-path pos-start pos-end &optional force-reload)
  (lexical-let* ((org-size (cacoo:get-image-size org-path))
                 (resize-path (cacoo:get-resize-path org-path)))
    (cond
     ((< org-size cacoo:max-size)
      (cacoo:display-diagram org-path pos-start pos-end)
      t)
     ((and (not force-reload) (cacoo:file-exists-p resize-path))
      (cacoo:display-diagram resize-path pos-start pos-end)
      t)
     (t
      (lexical-let* 
          ((pos-start pos-start) (pos-end pos-end)
           (tmpbuf (get-buffer-create (format " *Cacoo:resize-temp:%s*" org-path)))
           (proc
            (start-process 
             "Cacoo:convert" tmpbuf "convert" 
             "-resize" 
             (format "%ix%i" cacoo:max-size cacoo:max-size)
             "-transparent-color" "'#ffffff'"
             org-path 
             (concat (file-name-extension resize-path)
                     ":" resize-path))))
        (set-process-sentinel
         proc (lambda (proc event)
                (cond
                 ((string-match "exited abnormally" event)
                  (kill-buffer tmpbuf)
                  (cacoo:display-diagram-by-text 
                   pos-start pos-end "Could not convert."))
                 ((equal event "finished\n")
                  (kill-buffer tmpbuf)
                  (if (and (file-exists-p resize-path)
                           (< 0 (nth 7 (file-attributes resize-path))))
                      (cacoo:display-diagram resize-path pos-start pos-end)
                    (cacoo:display-diagram-by-text 
                     pos-start pos-end "Could not convert."))))
                )))
      t))))

(defun cacoo:get-image-type (filename)
  (let ((type (intern (file-name-extension filename))))
    (if (eq type 'jpg) 'jpeg type)))

(defun cacoo:display-diagram (resize-path pos-start pos-end)
  (clear-image-cache)
  (let ((img (create-image 
              resize-path (cacoo:get-image-type resize-path) nil
              :relief 1)))
    (put-text-property pos-start pos-end 'display img)
    (cacoo:add-click-action pos-start pos-end)))

(defun cacoo:display-diagram-by-text (pos-start pos-end &optional text)
  (put-text-property 
   pos-start pos-end 'help-echo (format "Cacoo: Error  %s" text)))

(defun cacoo:do-next-diagram (action)
  (cond
   ((re-search-forward cacoo:img-regexp nil t)
    (let* ((line (match-string 0))
           (url (match-string 1))
           (pos-start (match-beginning 0))
           (pos-end (match-end 0)))
      (funcall action url pos-start pos-end line)
      (goto-char pos-end)))
   (t
    nil)))

(defun cacoo:load-next-diagram (&optional force-reload)
  (lexical-let ((force-reload force-reload))
    (cacoo:do-next-diagram
     (lambda (url pos-start pos-end line)
       (cond
        ((string-match "^file:\\/\\/\\/" url)
         (cacoo:load-diagram-local url pos-start pos-end force-reload)
         pos-end)
        ((string-match "^https?:\\/\\/" url)
         (cacoo:load-diagram-remote url pos-start pos-end force-reload)
         pos-end)
        (t ; 相対パスを仮定
         (cacoo:load-diagram-local
          url pos-start pos-end force-reload)
         pos-end))))))

(defun cacoo:revert-next-diagram ()
  (cacoo:do-next-diagram
   (lambda (url pos-start pos-end line)
     (remove-text-properties 
      pos-start pos-end
      '(display nil mouse-face nil help-echo nil keymap nil))
     pos-end)))

(defun cacoo:add-click-action (pos-start pos-end)
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'cacoo:do-click-link)
    (define-key map (kbd "\n") 'cacoo:do-click-link)
    (add-text-properties pos-start pos-end
                         (list 'keymap map 'mouse-face 'highlight))))

(defun cacoo:do-click-link ()
  (interactive)
  (beginning-of-line)
  (cacoo:view-next-diagram-command))

(defun cacoo:clear-all-cache-files ()
  ;;現在のキャッシュディレクトリの中身を全部削除する
  (let ((imd-dir (cacoo:get-cache-dir)))
    (loop for i in (directory-files imd-dir)
          if (file-regular-p i)
          do (delete-file i))))

(defun cacoo:view-original-cached-image (cached-file)
  ;;原寸大の画像を参照する
  (cond
   (cacoo:external-viewer
    ;;外部ビューアーコマンドが指定されていればそれを起動
    (call-process cacoo:external-viewer
                  nil 0 nil cached-file))
   (t
    ;;外部ビューアーがなければimage-modeで開く
    (find-file cached-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands and Minor Mode Definitions

(defun cacoo:reload-next-diagram-command ()
  (interactive)
  ;;カーソール直後の図をリロードする
  (save-excursion
    (cacoo:load-next-diagram t)))

(defun cacoo:reload-all-diagrams-command ()
  (interactive)
  ;;バッファ内のすべての図を更新する
  (save-excursion
    (goto-char (point-min))
    (while (cacoo:load-next-diagram t))))

(defun cacoo:revert-next-diagram-command ()
  (interactive)
  ;;カーソール直後の図をテキストに戻す
  (save-excursion
    (cacoo:revert-next-diagram)))

(defun cacoo:revert-all-diagrams-command ()
  (interactive)
  ;;バッファ内のすべての図をテキストに戻す
  (save-excursion
    (goto-char (point-min))
    (while (cacoo:revert-next-diagram))))

(defun cacoo:display-all-diagrams-command () 
  (interactive)
  ;;バッファ内のすべての図を表示する
  ;;キャッシュがあればそれを使う
  (save-excursion
    (goto-char (point-min))
    (while (cacoo:load-next-diagram))))

(defun cacoo:display-next-diagram-command () 
  (interactive)
  ;;カーソール直後の図を表示する
  ;;キャッシュがあればそれを使う
  (save-excursion
    (cacoo:load-next-diagram)))

(defun cacoo:create-new-diagram-command ()
  (interactive)
  ;;ブラウザで新規図を開く
  (browse-url-default-browser cacoo:new-url))

(defun cacoo:open-diagram-list-command ()
  (interactive)
  ;;ブラウザで図の一覧を開く
  (browse-url-default-browser cacoo:list-url))

(defun cacoo:get-key-from-url (url)
  (if (string-match cacoo:key-regexp url)
      (match-string 1 url)
    nil))

(defun cacoo:make-url (tmpl-url key)
  (if key 
      (replace-regexp-in-string "%KEY%" key tmpl-url t) nil))

(defun cacoo:edit-next-diagram-command ()
  (interactive)
  ;;カーソール直後の図をブラウザで編集する
  (cacoo:open-diagram-gen cacoo:edit-url))

(defun cacoo:open-diagram-gen (tmpl-url)
  ;;カーソール直後の図をブラウザで編集する
  (let (error-message)
    (save-excursion
      (cond
       ((re-search-forward cacoo:img-regexp)
        (let* ((line (match-string 0))
               (url (match-string 1))
               (key (cacoo:get-key-from-url url))
               (open-url (cacoo:make-url tmpl-url key)))
          (if open-url 
              (browse-url-default-browser open-url)
            (browse-url-default-browser url))))
       (t
        (setq "URL is not found."))))
    (when error-message 
      (message error-message))))

(defun cacoo:view-next-diagram-command ()
  (interactive)
  ;;カーソール直後の図の詳細画面をブラウザで開く
  (cacoo:open-diagram-gen cacoo:view-url))

(defun cacoo:navi-next-diagram-command ()
  (interactive)
  (end-of-line)
  (cacoo:do-next-diagram 
   (lambda (url pos-start pos-end line) t))
  (beginning-of-line))

(defun cacoo:navi-prev-diagram-command ()
  (interactive)
  (if (re-search-backward cacoo:img-regexp nil t)
      (beginning-of-line)))

(setq cacoo-minor-mode-keymap
      (cacoo:define-keymap
       '(("C-c C-q" . cacoo:minor-mode-off-command)
         ("C-c n"   . cacoo:navi-next-diagram-command)
         ("C-c p"   . cacoo:navi-prev-diagram-command)

         ("C-c r"   . cacoo:reload-next-diagram-command)
         ("C-c R"   . cacoo:reload-all-diagrams-command)

         ("C-c t"   . cacoo:revert-next-diagram-command)
         ("C-c T"   . cacoo:revert-all-diagrams-command)

         ("C-c d"   . cacoo:display-next-diagram-command)
         ("C-c D"   . cacoo:display-all-diagrams-command)

         ("C-c N"   . cacoo:create-new-diagram-command)
         ("C-c e"   . cacoo:edit-next-diagram-command)
         ("C-c v"   . cacoo:view-next-diagram-command)

         ("C-c l"   . cacoo:open-diagram-list-command)
         )))

(defun cacoo:minor-mode-off-command ()
  (interactive)
  (cacoo-minor-mode -1))

(defvar cacoo-minor-mode-hook nil)

(defvar cacoo-minor-mode nil) ; dummy

(define-minor-mode cacoo-minor-mode
  "Cacoo mode"
  :init-value nil
  :lighter " Cacoo"
  :keymap cacoo-minor-mode-keymap
  :group 'cacoo-mode
  (if cacoo-minor-mode
      (progn
        (cacoo:minor-mode-setup)
        (run-hooks 'cacoo-minor-mode-hook))
    (cacoo:minor-mode-abort)))

(defun cacoo:minor-mode-setup ()
  (cacoo:display-all-diagrams-command))

(defun cacoo:minor-mode-abort ()
  (cacoo:revert-all-diagrams-command))

(defun toggle-cacoo-minor-mode ()
  (interactive)
  (cacoo-minor-mode 
   (if cacoo-minor-mode -1 1)))

(provide 'cacoo)
;;; cacoo.el ends here
