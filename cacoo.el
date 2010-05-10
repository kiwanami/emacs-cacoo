;;; cacoo.el --- Minor mode for Cacoo

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
;; Version: 1.2
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
;; [img:https://cacoo.com/diagrams/6m4ATG1ddlUiHPqd-0FAF7.png 200]
;; 
;; ※注意点として、Cacoo側でURLによる公開設定をしておく必要があります。
;; 公開設定にしても、URLが他人に知られない限り図を見ることができません。

;; cacoo-minor-modeがOnの時に以下のキーバインドが使えます。
;; （使いやすくcacoo-minor-mode-keymapをカスタマイズしてみてください）

;; * バッファ全体に対して
;; C-c , T : バッファのすべての図をテキストに戻す
;; C-c , D : バッファのすべての図を表示する（キャッシュがあればネットワークに接続しない）
;; C-c , R : バッファのすべての図を取得し直す

;; * カーソール直後の図に対して
;; C-c , t : テキストに戻す
;; C-c , d : 図を表示する（キャッシュがあればネットワークに接続しない）
;; C-c , r : 図を取得し直して表示する
;; C-c , e : 図の編集画面を表示する（https://cacoo.com/diagrams/xxxxx/edit）
;; C-c , v : 図の生涯画面を表示する（https://cacoo.com/diagrams/xxxxx）
;; C-c , V : ローカルの図を外部ビューアーで開く

;; * Cacooの機能に対して
;; C-c , N : 新規図の作成 （https://cacoo.com/diagrams/new）
;; C-c , l : 図の一覧 （https://cacoo.com/diagrams/）

;; * ナビゲーション、編集
;; C-c , n : 次の図に移動
;; C-c , p : 前の図に移動
;; C-c , i : 図のマークアップを挿入
;; C-c , y : クリップボードのテキストを使って図のマークアップを挿入

;; * その他
;; C-c , C : キャッシュディレクトリを空にする

;; Cacoo以外でも以下のような図を表示することができます。
;; （もちろん編集はできません）
;; Web上の画像
;; [img:http://example.com/zzz.png]
;; ローカルの画像（絶対パス）
;; [img:file:///xxx/yyy/zzz.png]
;; ローカルの画像（相対パス）
;; [img:zzz.png]

;; 画像の取得や変換中にエラーが起きた場合、該当箇所の色が変わります。ま
;; た、マウスオーバーで短くエラーメッセージがポップアップで表示されます。
;; 大抵の問題は、wgetが見つからない、convertが見つからない、URL先の図が
;; 見つからないなどだと思います。

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

;; Emacs22などではPNGの透過部分が黒くなることがあります。
;; (setq cacoo:png-background "white") を設定に追加して画像の
;; リロードをしてみてください。

;;; 制限事項

;; 同一バッファ内で、同一画像を複数回表示することができません。

;;; 更新履歴

;; Revision 1.2  2010/05/08  sakurai
;; バイトコンパイル時のバグ修正（by kitokitokiさん）
;; 本文コメントに注意点などを追加
;; 画像クリック時の動作を改善
;; 
;; Revision 1.1  2010/05/08  sakurai
;; エラー箇所をオーバーレイで強調
;; 非同期処理経過の表示
;; 透過PNGの背景色がおかしい場合や、ImageMagickのバージョンが古い場合に対応。
;;
;; Revision 1.0  2010/05/07  sakurai
;; Initial revision

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
(defvar cacoo:png-background nil) ; 透過PNGの背景色がおかしい場合にセット

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

(eval-and-compile
  (defvar cacoo:debug nil "Debug output switch.")) ; debug
(defvar cacoo:debug-count 0 "[internal] Debug output counter.") ; debug

(defmacro cacoo:log (&rest args)
  (when cacoo:debug
    `(progn 
       (with-current-buffer (get-buffer-create "*cacoo:debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" cacoo:debug-count (format ,@args)))))
       (incf cacoo:debug-count))))

(defun cacoo:message-mark ()
  (interactive)
  (cacoo:log "==================== mark ==== %s" 
             (format-time-string "%H:%M:%S" (current-time))))

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
      (match-string 0 url)))

(defun cacoo:file-exists-p (file)
  (and (file-exists-p file)
       (< 0 (nth 7 (file-attributes file)))))

(defun cacoo:clear-cache-file (file)
  (when (file-exists-p file)
    (delete-file file))
  (cacoo:aif (cacoo:get-resize-path file)
             (when (file-exists-p it)
               (delete-file it))))

(defvar cacoo:uid-count 0) ; 非同期プロセス作成用

(defun cacoo:uid (name)
  (incf cacoo:uid-count)
  (format " *Cacoo:%s:%i*" name cacoo:uid-count))

(defvar cacoo:async-counter-start  0) ; 開始非同期プロセスの数
(defvar cacoo:async-counter-finish 0) ; 終了非同期プロセスの数
(defvar cacoo:async-counter-error  0) ; エラー終了非同期プロセスの数
(make-variable-buffer-local 'cacoo:async-counter-start)
(make-variable-buffer-local 'cacoo:async-counter-finish)
(make-variable-buffer-local 'cacoo:async-counter-error)

(defun cacoo:async-reset-counter ()
  (setq cacoo:async-counter-start  0
        cacoo:async-counter-finish 0
        cacoo:async-counter-error  0)
  (cacoo:log "ASYNC COUNTER RESET"))

(defun cacoo:async-start ()
  (incf cacoo:async-counter-start)
  (cacoo:async-display-message))

(defun cacoo:async-finish ()
  (incf cacoo:async-counter-finish)
  (cacoo:async-display-message))

(defun cacoo:async-finish-error ()
  (incf cacoo:async-counter-finish)
  (incf cacoo:async-counter-error)
  (cacoo:async-display-message))

(defun cacoo:async-display-message ()
  (let ((error-message
         (if (< 0 cacoo:async-counter-error)
             (format "  %i errors" cacoo:async-counter-error) "")))
    (message 
     (if (eql cacoo:async-counter-finish cacoo:async-counter-start)
         "Cacoo tasks are done. (%i/%i%s)"
       "Cacoo processing.. (%i/%i%s)" )
     cacoo:async-counter-finish cacoo:async-counter-start
     error-message)))

(defun cacoo:async (&rest procs)
  (cacoo:async-gen 'cacoo:async-start (current-buffer) procs))

(defun cacoo:async-gen (args buf procs)
  ;;procsを実行して回る
  ;;エラーが出たら中断
  (when procs
    (when (eq args 'cacoo:async-start) 
      (cacoo:async-start))
    (lexical-let*
        ((proc (car procs)) (next-procs (cdr procs))
         (name (car proc)) (cmds (cadr proc))
         (ok (nth 2 proc)) (ng (nth 3 proc))
         (tag (car cmds)) (buf buf)
         (tmpbuf (get-buffer-create 
                  (format " *Cacoo:temp:%s*" name)))
         (rcmds (if (and tag (symbolp tag) (eq tag 'lambda))
                    (funcall cmds args) cmds))
         (proc
          (apply 'start-process (cacoo:uid name) tmpbuf rcmds))
         (dcount cacoo:uid-count))
      (cacoo:log "ASYNC-S %i : %s %s" dcount tag rcmds)
      (set-process-sentinel
       proc
       (lambda (proc event)
         (with-current-buffer buf
           (cacoo:log "ASYNC-E %i : %s %s" dcount tag event)
           (cond
            ((string-match "exited abnormally" event)
             (let ((msg (if (buffer-live-p tmpbuf)
                            (cacoo:buffer-string "%s" tmpbuf)
                          (concat "NA:" name))))
               (cacoo:log "ASYNC-E %i [%s]" dcount msg)
               (kill-buffer tmpbuf)
               (funcall ng msg)
               (cacoo:async-finish-error)))
            ((equal event "finished\n")
             (let* ((msg (prog1 
                             (if (buffer-live-p tmpbuf)
                                 (cacoo:buffer-string "%s" tmpbuf)
                               (concat "NA:" name))
                           (kill-buffer tmpbuf)))
                    (ret (funcall ok msg)))
               (cacoo:log "ASYNC-E %i [%s]" dcount msg)
               (if next-procs
                   (cacoo:async-gen ret buf next-procs)
                 (cacoo:async-finish)))))))))))

(defun cacoo:proc (name commands ok ng)
  ;; name プロセス、バッファを特定する名前
  ;; commands コマンドラインで実行するものの文字列リスト
  ;; →lambdaの場合は前のプロセスのokの実行結果を引数にしてfuncallする
  ;; ok プロセス正常終了時に呼ばれる。引数：出力文字列
  ;; ng プロセス異常終了時に呼ばれる。引数：出力文字列
  (list name commands ok ng))

(defun cacoo:load-diagram-remote(data &optional force-reload)
  (lexical-let* ((data data) (force-reload force-reload)
                 (org-path (cacoo:$img-cached-file data))
                 (url (cacoo:$img-url data)))
    (when force-reload (cacoo:clear-cache-file org-path))
    (cond
     ((cacoo:file-exists-p org-path)
      (cacoo:resize-diagram data force-reload)
      t)
     (t
      (cacoo:async
       (cacoo:proc
        org-path
        (list "wget" "-q" 
              "--no-check-certificate" ; 古い証明書しかないとcacooのサイトが検証できないため
              "-O" org-path url)
        (lambda (msg)
          (if (cacoo:file-exists-p org-path)
              (cacoo:resize-diagram data force-reload)
            (setf (cacoo:$img-error data) msg)
            (cacoo:display-diagram-by-text data)))
        (lambda (msg)
          (setf (cacoo:$img-error data) msg)
          (cacoo:display-diagram-by-text data))))
      t))))

(defun cacoo:get-local-path-from-url(url)
  (cond
   ((string-match "^file://\\(.*\\)$" url) ; フルパスを仮定
    (match-string 1 url))
   ((cacoo:file-exists-p (expand-file-name url default-directory))
    (expand-file-name url default-directory))
   (t
    nil)))

(defun cacoo:load-diagram-local(data &optional force-reload)
  (let* ((url (cacoo:$img-url data))
         (from-path (cacoo:get-local-path-from-url url))
         (org-path (cacoo:$img-cached-file data)))
    (when from-path
      (when force-reload (cacoo:clear-cache-file org-path))
      (unless (cacoo:file-exists-p org-path)
        (ignore-errors
          (copy-file from-path org-path t t)))
      (if (cacoo:file-exists-p org-path)
          (cacoo:resize-diagram data force-reload)
        (setf (cacoo:$img-error data) "Could not copy a file.")
        (cacoo:display-diagram-by-text data)))))

(defun cacoo:get-image-size (data)
  (let* ((file (cacoo:$img-cached-file data))
         (ret (get-buffer-create (format " *Cacoo:identify:%s*" file))) 
         line)
    (buffer-disable-undo ret)
    (call-process "identify" nil ret nil "-format" "%w %h" file)
    (prog1
        (with-current-buffer ret
          (let* ((line (buffer-string))
                 (cols (split-string line " "))
                 (width (string-to-number (car cols)))
                 (height (string-to-number (cadr cols))))
            (cacoo:log "SIZE %s <= %s" line file)
            (max width height)))
      (kill-buffer ret))))

(defun cacoo:resize-diagram (data &optional force-reload)
  (let* 
      ((org-size (cacoo:get-image-size data))
       (size (cacoo:$img-size data))
       (not-resizep (< org-size size))
       (resize-path (cacoo:$img-resized-file data))
       (data data))
    (cond
     ((and (not force-reload) 
           (cacoo:file-exists-p resize-path))
      (cacoo:display-diagram data)
      t)
     (cacoo:png-background
      (cacoo:resize-diagram-for-fillbg data not-resizep)
      t)
     (t
      (cacoo:resize-diagram-for-transparent data not-resizep)
      t))))

(defun cacoo:get-background-img-path (path)
  (expand-file-name
   (format "back_%s" (file-name-nondirectory path))
   (file-name-directory path)))

(defun cacoo:resize-diagram-for-transparent (data not-resizep)
  (lexical-let* 
      ((org-size (cacoo:get-image-size data))
       (size (cacoo:$img-size data))
       (org-path (cacoo:$img-cached-file data))
       (resize-path (cacoo:$img-resized-file data))
       (data data))
    (cond 
     (not-resizep
      (ignore-errors
        (copy-file org-path resize-path t t))
      (cacoo:display-diagram data))
     (t
      (cacoo:async
       (cacoo:proc
        org-path
        (list "convert" "-resize" (format "%ix%i" size size)
              "-transparent-color" "#ffffff"
              org-path (concat (file-name-extension resize-path)
                               ":" resize-path))
        (lambda (msg)
          (if (cacoo:file-exists-p resize-path)
              (cacoo:display-diagram data)
            (setf (cacoo:$img-error data) msg)
            (cacoo:display-diagram-by-text data)))
        (lambda (msg)
          (setf (cacoo:$img-error data) msg)
          (cacoo:display-diagram-by-text data))))))
    t))

(defun cacoo:resize-diagram-for-fillbg (data not-resizep)
  (lexical-let* 
      ((org-size (cacoo:get-image-size data))
       (data data)
       (org-path (cacoo:$img-cached-file data))
       (resize-path (cacoo:$img-resized-file data))
       (size (cacoo:$img-size data))
       (resize-size nil)
       (tmpfile (cacoo:get-background-img-path org-path))
       (err (lambda (errmsg) 
              `(lambda (msg) 
                 (setf (cacoo:$img-error ,data)
                       (format ,(format "%s. \n%%s" errmsg) msg))
                 (cacoo:display-diagram-by-text ,data)))) procs)
    (setq procs
          (list
           (cacoo:proc ; 縮小した画像のサイズを取得
            org-path
            (list "identify" "-format" "%wx%h" resize-path)
            'identity ; <- 次の実行で使う
            (funcall err "Could not identify"))
           (cacoo:proc ; 縮小した画像と同じサイズの背景画像を準備
            org-path
            (lambda (args) (list "convert" "-size" args
                                 (concat "xc:" cacoo:png-background) 
                                 tmpfile))
            'identity
            (funcall err "Could not make bgimage"))
           (cacoo:proc ; 背景に重ねる
            org-path
            (list "convert" tmpfile resize-path "-flatten" resize-path)
            (lambda (msg)
              (if (cacoo:file-exists-p resize-path)
                  (cacoo:display-diagram data)
                (setf (cacoo:$img-error data)
                      (format "Could not compose. \n%s" msg))
                (cacoo:display-diagram-by-text data))
              (when (file-exists-p tmpfile)
                (delete-file tmpfile)))
            (funcall err "Could not compose"))))
    (if not-resizep
        (ignore-errors ; 十分画像のサイズが小さいときはコピー
          (copy-file org-path resize-path t t))
        (setq procs  ; 大きい画像は縮小する（タスクの先頭に追加）
              (cons 
               (cacoo:proc
                org-path
                (list "convert" org-path "-resize" (format "%ix%i" size size)
                      (concat (file-name-extension resize-path) ":" resize-path))
                'identity
                (funcall err "Could not convert")) procs)))
    (cacoo:async-gen 'cacoo:async-start (current-buffer) procs)))

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
        (map (make-sparse-keymap))
        (mod (buffer-modified-p)))
    (define-key map [mouse-1] 'cacoo:do-click-link)
    (define-key map (kbd "\n") 'cacoo:do-click-link)
    (add-text-properties 
     (cacoo:$img-start data)
     (cacoo:$img-end data)
     (list 'display img 'keymap map 'mouse-face 'highlight))
    (cacoo:display-diagram-overlay-remove 
     (cacoo:$img-start data)
     (cacoo:$img-end data))
    (set-buffer-modified-p mod)))

(defvar cacoo:display-diagram-overlays nil) ; エラー表示用のオーバーレイ
(make-variable-buffer-local 'cacoo:display-diagram-overlays)

(defun cacoo:display-diagram-overlay-add (start end)
  (when
      (loop for i in cacoo:display-diagram-overlays
            if (and ; 既存のOLとかぶってないかチェック
                (< start (overlay-end i))
                (> end (overlay-start i)))
            return nil
            finally return t)
    (let ((ol (make-overlay start end)))
      (overlay-put ol 'face 'next-error)
      (push ol cacoo:display-diagram-overlays))))

(defun cacoo:display-diagram-overlay-remove (start end)
   (setq cacoo:display-diagram-overlays 
         (loop for i in cacoo:display-diagram-overlays
               if (and
                   (overlay-buffer i)
                   (or
                    (> start (overlay-end i))
                    (< end (overlay-start i))))
               collect i
               else
               do (delete-overlay i))))

(defun cacoo:display-diagram-overlay-clear ()
  (loop for i in cacoo:display-diagram-overlays
        if (overlay-buffer i)
        do (delete-overlay i))
  (setq cacoo:display-diagram-overlays nil))

(defun cacoo:display-diagram-by-text (data)
  (let ((mod (buffer-modified-p)))
    (put-text-property 
     (cacoo:$img-start data) (cacoo:$img-end data)
     'help-echo (format "Cacoo: Error  %s" 
                        (cacoo:$img-error data)))
    (cacoo:display-diagram-overlay-add
     (cacoo:$img-start data) (cacoo:$img-end data))
    (set-buffer-modified-p mod)))

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
      (if filename
          (funcall action data))
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
     (let ((mod (buffer-modified-p)))
       (remove-text-properties 
        (cacoo:$img-start data) (cacoo:$img-end data) 
        '(display nil mouse-face nil help-echo nil keymap nil))
       (cacoo:display-diagram-overlay-remove
        (cacoo:$img-start data) (cacoo:$img-end data))
       (set-buffer-modified-p mod))
     (cacoo:$img-end data))))

(defun cacoo:do-click-link ()
  (interactive)
  (beginning-of-line)
  (cacoo:do-next-diagram
   (lambda (data)
     (let ((url (cacoo:$img-url data))
           (pos-end (cacoo:$img-start data))
           (pos-start (cacoo:$img-start data)))
       (goto-char pos-start)
       (beginning-of-line)
       (cond
        ((string-match "^file:\\/\\/\\/" url)
         (cacoo:view-original-cached-image
          (cacoo:$img-cached-file data))
         pos-end)
        ((string-match "^https?:\\/\\/" url)
         (cacoo:edit-next-diagram-command)
         pos-end)
        (t ; 相対パスを仮定
         (cacoo:view-original-cached-image
          (cacoo:$img-cached-file data))
         pos-end))))))

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
  (cacoo:async-reset-counter)
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
  (cacoo:async-reset-counter)
  (save-excursion
    (cacoo:load-next-diagram t)))

(defun cacoo:reload-all-diagrams-command ()
  (interactive)
  ;;バッファ内のすべての図を更新する
  (cacoo:async-reset-counter)
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
  (cacoo:async-reset-counter)
  (save-excursion
    (goto-char (point-min))
    (while (cacoo:load-next-diagram))))

(defun cacoo:display-next-diagram-command () 
  (interactive)
  ;;カーソール直後の図を表示する
  ;;キャッシュがあればそれを使う
  (cacoo:async-reset-counter)
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
               (url (car (split-string (match-string 1) "[ \t]")))
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

(defvar cacoo-minor-mode-keymap
      (cacoo:define-keymap
       '(("C-c , C-q" . cacoo:minor-mode-off-command)
         ("C-c , n"   . cacoo:navi-next-diagram-command)
         ("C-c , p"   . cacoo:navi-prev-diagram-command)

         ("C-c , r"   . cacoo:reload-next-diagram-command)
         ("C-c , R"   . cacoo:reload-all-diagrams-command)

         ("C-c , t"   . cacoo:revert-next-diagram-command)
         ("C-c , T"   . cacoo:revert-all-diagrams-command)

         ("C-c , d"   . cacoo:display-next-diagram-command)
         ("C-c , D"   . cacoo:display-all-diagrams-command)

         ("C-c , i"   . cacoo:insert-pattern-command)
         ("C-c , y"   . cacoo:insert-yank-command)

         ("C-c , N"   . cacoo:create-new-diagram-command)
         ("C-c , e"   . cacoo:edit-next-diagram-command)
         ("C-c , v"   . cacoo:view-next-diagram-command)
         ("C-c , V"   . cacoo:view-local-cache-next-diagram-command)

         ("C-c , C"   . cacoo:clear-all-cache-files-command)

         ("C-c , l"   . cacoo:open-diagram-list-command)
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
  (cacoo:display-diagram-overlay-clear)
  (cacoo:revert-all-diagrams-command))

(defun toggle-cacoo-minor-mode ()
  (interactive)
  (cacoo-minor-mode 
   (if cacoo-minor-mode -1 1)))

(require 'easymenu)

(defvar cacoo:minor-mode-menu-spec
'("Cacoo"
    ["Display all diagrams" cacoo:display-all-diagrams-command :active t :visible t]
    ["Revert all diagrams" cacoo:revert-all-diagrams-command  :active t :visible t]
    ["Reload all diagrams" cacoo:reload-all-diagrams-command  :active t :visible t]
    "----"
    ["Display a diagram" cacoo:display-next-diagram-command t]
    ["Revert a diagram" cacoo:revert-next-diagram-command t]
    ["Reload a diagram" cacoo:reload-next-diagram-command t]
    ["Edit a diagram" cacoo:edit-next-diagram-command t]
    ["View diagram details" cacoo:view-next-diagram-command t]
    ["View a local cache" cacoo:view-local-cache-next-diagram-command t]
    "----"
    ["Create new diagram" cacoo:create-new-diagram-command t]
    ["List diagrams" cacoo:open-diagram-list-command t]
    "----"
    ["Clear all cache files" cacoo:clear-all-cache-files-command t]))


(easy-menu-define cacoo-menu-map
  cacoo-minor-mode-keymap "Cacoo menu map" 
  cacoo:minor-mode-menu-spec)
(easy-menu-add cacoo-menu-map cacoo-minor-mode-keymap)

;; for test

;; [img:https://cacoo.com/diagrams/70HJMrqSy7WmIiVi-FC3DC.png 200]
;; [ img:https://cacoo.com/diagrams/70HJMrqSy7WmIiVi-FC3DC.png 800]
;; [img:http://www.google.co.jp/intl/ja/images/about_logo.gif 100]
;; [ img:http://www.google.co.jp/intl/ja/images/about_logo.gif 800]
;; [img:http://kiwanami.net/images/2006-05.jpg 200]
;; [ img:http://kiwanami.net/images/2006-05.jpg 800]
;; [img:mimi.jpg 200]
;; [ img:mimi.jpg 2000]

;; (setq cacoo:png-background nil)
;; (setq cacoo:png-background "white")
;; (setq cacoo:debug t)
;; (setq cacoo:debug nil)

(provide 'cacoo)
;;; cacoo.el ends here
