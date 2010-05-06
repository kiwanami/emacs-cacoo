;;; cacoo.el --- Minor mode for Cacoo

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
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

;; 必要なもの、環境
;; wget, ImageMagick(convert, identify, display)

;; ロードパスに cacoo.el を置き、以下のように呼び出し用のコードを
;; .emacs などに追加してください。以下の例では、Altキーを押しながら「-」
;; を押すとOn/Offが切り替わるようになります。

;; ;設定例
;; (require 'cacoo)
;; (global-set-key (kbd "M--") 'toggle-cacoo-minor-mode)

;;; Usage:

;; テキスト中に以下のように記述するとCacooの絵が入ります。
;; [img:https://cacoo.com/diagrams/6m4ATG1ddlUiHPqd-0FAF7.png]
;; 
;; cacoo-minor-modeがOnの時に以下のキーバインドが使えます。
;; （cacoo-minor-mode-keymapをカスタマイズしてみてください）

;; * バッファ全体に対して
;; C-c T : バッファのすべての図をテキストに戻す
;; C-c D : バッファのすべての図を表示する（キャッシュがあればネットワークに接続しない）
;; C-c R : バッファのすべての図を取得し直す

;; * カーソール直後の図に対して
;; C-c t : テキストに戻す
;; C-c d : 図を表示する（キャッシュがあればネットワークに接続しない）
;; C-c r : 図を取得し直して表示する
;; C-c e : 図の編集画面を表示する（https://cacoo.com/diagrams/xxxxx/edit）
;; C-c v : 図の生涯画面を表示する（https://cacoo.com/diagrams/xxxxx）
;; C-c V : ローカルの図を外部ビューアーで開く

;; * Cacooの機能に対して
;; C-c N : 新規図の作成 （https://cacoo.com/diagrams/new）
;; C-c l : 図の一覧 （https://cacoo.com/diagrams/）

;; * ナビゲーション、編集
;; C-c n : 次の図に移動
;; C-c p : 前の図に移動
;; C-c i : 図のマークアップを挿入
;; C-c y : クリップボードのテキストを使って図のマークアップを挿入

;; * その他
;; C-c C : キャッシュディレクトリを空にする

;; Cacoo以外でも以下のような図を表示することができます。
;; （もちろん編集はできません）
;; Web上の画像
;; [img:http://example.com/zzz.png]
;; ローカルの画像（絶対パス）
;; [img:file:///xxx/yyy/zzz.png]
;; ローカルの画像（相対パス）
;; [img:zzz.png]

;;; カスタマイズなど

;; ブラウザーはEmacsで設定してあるデフォルトブラウザーを使います。
;; browse-url-browser-function 周辺を設定してみてください。
;; 例：Macでsafariを使う場合
;; (setq browse-url-browser-function 'browse-url-generic)
;; (setq browse-url-generic-program "open")

;; cacoo:img-regexp, cacoo:img-pattern を変更することで、Wiki記法などに
;; あわせることができます。

;; キャッシュ用のディレクトリ名は cacoo:img-dir で指定します。いちいち
;; ディレクトリ作成で確認が必要なければ、 cacoo:img-dir-ok を t に設定
;; してください。

;; 画像の縮小サイズは cacoo:max-size で指定します。
;; 以下のように書くことで画像ごとにも指定できます。
;; [img:file:///xxx/yyy/zzz.png 600]

;; 外部画像ビューアーのプログラムは cacoo:external-viewer で指定します。
;; nilに設定するとEmacsで開こうとします。

;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize

(defvar cacoo:base-url "https://cacoo.com/diagrams/")
(defvar cacoo:new-url (concat cacoo:base-url "new"))
(defvar cacoo:edit-url (concat cacoo:base-url "%KEY%/edit"))
(defvar cacoo:view-url (concat cacoo:base-url "%KEY%"))
(defvar cacoo:list-url cacoo:base-url)

(defvar cacoo:img-regexp "\\[img:\\([^]]*\\)\\]") ; 本文から画像に置き換える文字列を取ってくる正規表現
(defvar cacoo:img-pattern "[img:%s]") ; 画像として挿入する文字列
(defvar cacoo:key-regexp "diagrams\\/\\([a-zA-Z0-9]*\\)") ; URLからCacooの画像のKeyを取ってくる正規表現

(defvar cacoo:img-dir ".cimg") ; 画像キャッシュフォルダ
(defvar cacoo:img-dir-ok nil) ; 勝手にディレクトリを作って良ければ t

(defvar cacoo:max-size 450) ; デフォルトの縮小サイズ

(defvar cacoo:external-viewer "display") ; ローカルの画像ビューアー。nilだとEmacsで開く。

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

;; cacoo:$img 画像データの構造体
;; url          : 画像を取ってくるURL
;; line         : 画像表示指示テキスト全体
;; cached-file  : コピーしたオリジナルの画像
;; resized-file : サイズ調整した画像
;; start        : テキスト置き換えの開始位置
;; end          : テキスト置き換えの終了位置
;; size         : この画像の表示サイズ
;; error        : 何か問題が発生したときの表示内容
(defstruct cacoo:$img url line cached-file resized-file start end size error)

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

(defun cacoo:get-cache-path (filename)
  (expand-file-name 
   filename (cacoo:get-cache-dir)))

(defun cacoo:get-resize-path (filename)
  (expand-file-name
   (format "resize_%s" filename) (cacoo:get-cache-dir)))

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

(defun cacoo:load-diagram-remote(data &optional force-reload)
  (lexical-let* ((data data) (force-reload force-reload)
                 (org-path (cacoo:$img-cached-file data))
                 (url (cacoo:$img-url data))
                 proc tmpbuf)
    (when force-reload (cacoo:clear-cache-file org-path))
    (cond
     ((cacoo:file-exists-p org-path)
      (cacoo:resize-diagram data force-reload)
      t)
     (t
      (setq tmpbuf (get-buffer-create 
                    (format " *Cacoo:Download-temp:%s*" url)))
      (buffer-disable-undo tmpbuf)
      (setq proc (start-process
                  "Cacoo:wget" tmpbuf "wget"
                  "-q" "--no-check-certificate" ; 古い証明書しかないとcacooのサイトが検証できないため
                  "-O" org-path url))
      (set-process-sentinel
       proc (lambda(proc event)
              (cond 
               ((string-match "exited abnormally" event)
                (setf (cacoo:$img-error data)
                      (cacoo:buffer-string "No network connection. %s" tmpbuf))
                (kill-buffer tmpbuf)
                (cacoo:display-diagram-by-text data))
               ((equal event "finished\n")
                (kill-buffer tmpbuf)
                (if (cacoo:file-exists-p org-path)
                    (cacoo:resize-diagram data force-reload)
                  (setf (cacoo:$img-error data)
                        (cacoo:buffer-string "No network connection. %s" tmpbuf))
                  (cacoo:display-diagram-by-text data))))))
      t))))

(defun cacoo:get-local-path-from-url(url)
  (cond
   ((string-match "^file://\\(.*\\)$" url) ; フルパスを仮定
    (match-string 1 url))
   ((cacoo:file-exists-p (expand-file-name url default-directory))
    (expand-file-name url default-directory))
   (t
    (error "Wrong URL pattern [%s]" url))))

(defun cacoo:load-diagram-local(data &optional force-reload)
  (let* ((url (cacoo:$img-url data))
         (from-path (cacoo:get-local-path-from-url url))
         (org-path (cacoo:$img-cached-file data)))
    (when force-reload (cacoo:clear-cache-file org-path))
    (unless (cacoo:file-exists-p org-path)
      (ignore-errors
        (copy-file from-path org-path t t)))
    (if (cacoo:file-exists-p org-path)
        (cacoo:resize-diagram data force-reload)
      (setf (cacoo:$img-error data) "Could not copy a file.")
      (cacoo:display-diagram-by-text data))))

(defun cacoo:get-image-size (data)
  (let* ((file (cacoo:$img-cached-file data))
         (ret (get-buffer-create (format " *Cacoo:identify:%s*" file))) 
         line)
    (buffer-disable-undo ret)
    (call-process "identify" nil ret nil "-format"
                  "%[fx:w] %[fx:h]" file)
    (prog1
        (with-current-buffer ret
          (let* ((line (buffer-string))
                 (cols (split-string line " "))
                 (width (string-to-number (car cols)))
                 (height (string-to-number (cadr cols))))
            (max width height)))
      (kill-buffer ret))))

(defun cacoo:resize-diagram (data &optional force-reload)
  (lexical-let* 
      ((org-size (cacoo:get-image-size data))
       (data data))
    (cond
     ((< org-size (cacoo:$img-size data))
      (setf (cacoo:$img-resized-file data) 
            (cacoo:$img-cached-file data))
      (cacoo:display-diagram data)
      t)
     ((and (not force-reload) 
           (cacoo:file-exists-p (cacoo:$img-resized-file data)))
      (cacoo:display-diagram data)
      t)
     (t
      (lexical-let* 
          ((org-path (cacoo:$img-cached-file data))
           (resize-path (cacoo:$img-resized-file data))
           (size (cacoo:$img-size data))
           (tmpbuf (get-buffer-create 
                    (format " *Cacoo:resize-temp:%s*" org-path)))
           (proc
            (start-process 
             "Cacoo:convert" tmpbuf "convert" 
             "-resize" 
             (format "%ix%i" size size)
             "-transparent-color" "'#ffffff'"
             org-path 
             (concat (file-name-extension resize-path)
                     ":" resize-path))))
        (set-process-sentinel
         proc (lambda (proc event)
                (cond
                 ((string-match "exited abnormally" event)
                  (setf (cacoo:$img-error data)
                        (cacoo:buffer-string "Could not convert. \n%s" tmpbuf))
                  (kill-buffer tmpbuf) 
                  (cacoo:display-diagram-by-text data))
                 ((equal event "finished\n")
                  (if (and (file-exists-p resize-path)
                           (< 0 (nth 7 (file-attributes resize-path))))
                      (cacoo:display-diagram data)
                    (setf (cacoo:$img-error data)
                          (cacoo:buffer-string "Could not convert. \n%s" tmpbuf))
                    (cacoo:display-diagram-by-text data))
                  (kill-buffer tmpbuf))))))
      t))))

(defun cacoo:buffer-string (format buf)
  (format format
          (with-current-buffer buf (buffer-string))))

(defun cacoo:get-image-type (data)
  (let ((type (intern (file-name-extension 
                       (cacoo:$img-resized-file data)))))
    (if (eq type 'jpg) 'jpeg type)))

(defun cacoo:display-diagram (data)
  (clear-image-cache)
  (let ((img (create-image 
              (cacoo:$img-resized-file data)
              (cacoo:get-image-type data) nil
              :relief 1))
        (map (make-sparse-keymap)))
    (define-key map [mouse-1] 'cacoo:do-click-link)
    (define-key map (kbd "\n") 'cacoo:do-click-link)
    (add-text-properties 
     (cacoo:$img-start data)
     (cacoo:$img-end data)
     (list 'display img 'keymap map 'mouse-face 'highlight))))


(defun cacoo:display-diagram-by-text (data)
  (put-text-property 
   (cacoo:$img-start data) (cacoo:$img-end data)
   'help-echo (format "Cacoo: Error  %s" 
                      (cacoo:$img-error data))))

(defun cacoo:do-next-diagram (action)
  (cond
   ((re-search-forward cacoo:img-regexp nil t)
    (let* ((line (match-string 0))
           (start (match-beginning 0))
           (end (match-end 0))
           (cols (split-string (match-string 1) "[ \t]+"))
           (url (car cols))
           (filename (cacoo:get-filename-from-url url))
           (data 
            (make-cacoo:$img
             :line line :url url :start start :end end
             :cached-file (cacoo:get-cache-path filename)
             :resized-file (cacoo:get-resize-path filename)
             :size (cacoo:aif (cadr cols) (string-to-number it) cacoo:max-size))))
      (funcall action data)
      (goto-char end)))
   (t
    nil)))

(defun cacoo:load-next-diagram (&optional force-reload)
  (lexical-let ((force-reload force-reload))
    (cacoo:do-next-diagram
     (lambda (data)
       (let ((url (cacoo:$img-url data))
             (pos-end (cacoo:$img-end data)))
         (cacoo:fix-directory)
         (cond
          ((string-match "^file:\\/\\/\\/" url)
           (cacoo:load-diagram-local data force-reload)
           pos-end)
          ((string-match "^https?:\\/\\/" url)
           (cacoo:load-diagram-remote data force-reload)
           pos-end)
          (t ; 相対パスを仮定
           (cacoo:load-diagram-local data force-reload)
           pos-end)))))))

(defun cacoo:revert-next-diagram ()
  (cacoo:do-next-diagram
   (lambda (data)
     (remove-text-properties 
      (cacoo:$img-start data) (cacoo:$img-end data) 
      '(display nil mouse-face nil help-echo nil keymap nil))
     (cacoo:$img-end data))))

(defun cacoo:do-click-link ()
  (interactive)
  (beginning-of-line)
  (cacoo:edit-next-diagram-command))

(defun cacoo:clear-all-cache-files ()
  ;;現在のキャッシュディレクトリの中身を全部削除する
  (let ((imd-dir (cacoo:get-cache-dir)))
    (loop for i in (directory-files imd-dir)
          for f = (expand-file-name i imd-dir)
          if (file-regular-p f)
          do (delete-file f))))

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

(defun cacoo:insert-pattern-command ()
  (interactive)
  (insert (format cacoo:img-pattern "")))

(defun cacoo:insert-yank-command ()
  (interactive)
  (insert (format cacoo:img-pattern 
                  (current-kill 0))))

(defun cacoo:view-local-cache-next-diagram-command () 
  (interactive)
  ;;原寸大の画像をローカルの環境で参照する
  (cacoo:do-next-diagram
   (lambda (data)
     (cacoo:view-original-cached-image 
      (cacoo:$img-cached-file data)))))

(defun cacoo:clear-all-cache-files-command ()
  (interactive)
  ;;キャッシュフォルダの中身を空にする
  (when (yes-or-no-p "Delete all local cache files?")
    (cacoo:clear-all-cache-files)
    (message "Delete all local cache files.")))

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
  (browse-url cacoo:new-url))

(defun cacoo:open-diagram-list-command ()
  (interactive)
  ;;ブラウザで図の一覧を開く
  (browse-url cacoo:list-url))

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
              (browse-url open-url)
            (browse-url url))))
       (t
        (setq error-message "URL is not found."))))
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
   (lambda (data) t))
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

         ("C-c i"   . cacoo:insert-pattern-command)
         ("C-c y"   . cacoo:insert-yank-command)

         ("C-c N"   . cacoo:create-new-diagram-command)
         ("C-c e"   . cacoo:edit-next-diagram-command)
         ("C-c v"   . cacoo:view-next-diagram-command)
         ("C-c V"   . cacoo:view-local-cache-next-diagram-command)

         ("C-c C"   . cacoo:clear-all-cache-files-command)

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
