;;; cacoo-plugins.el --- plugins for cacoo.el

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
;; Keywords: 

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

;; This file defines some plugins for cacoo.el.

;; * How to define a plugin

;; 引数に以下のものが渡る。
;; - 画像マーカーの開始位置：start
;; - 終了位置：end
;; - 画像マーカーの中身：content
;; 返値として (cacoo:$img 構造体 . キャッシュ作成deferred) 
;; を返すとプラグインで処理するものと見なす

;;; Code:

;; ** 長文埋め込みのプラグイン
;; [img:* (url) (filename) (size:省略可)]
;; (長文)
;; <<<
;; urlの中の <<< がヒアドキュメントのようにURLエンコーディングして埋め込まれる
(defvar cacoo:plugin-long-url-regexp
      "^\\* \\([^ \n\r\t]+\\) \\([^ \n\r\t]+\\)\\( [0-9]+\\)?")
(defvar cacoo:plugin-long-url-terminator "<<<")

(defun cacoo:plugin-long-url (start end content)
  (when (string-match cacoo:plugin-long-url-regexp content)
    (let ((url (match-string 1 content))
          (filename (match-string 2 content))
          (size (match-string 3 content)))
      (cacoo:plugin-long-url-gen 
       start end content url filename size))))

(defun cacoo:plugin-long-url-gen (start end content url filename size)
  (save-excursion
    (goto-char end)
    (and (re-search-forward cacoo:plugin-long-url-terminator)
         (let*
             ((t-start (match-beginning 0))
              (t-end (match-end 0))
              (text (buffer-substring (1+ end) (1- t-start)))
              (rurl 
               (replace-regexp-in-string 
                cacoo:plugin-long-url-terminator
                (cacoo:plugin-url-encode-string text) url)))
           (cons
            (make-cacoo:$img
             :url (cacoo:plugin-url-create 'long-url filename)
             :start start :end t-end
             :size (cacoo:aif size (string-to-number it) 
                     cacoo:max-size)))
           (lexical-let ((rurl rurl))
             (lambda ()
               (deferred:$
                 (cacoo:http-get-d
                  nil rurl (cacoo:get-cache-path filename))
                 (deferred:nextc it
                   (lambda (err)
                     (cacoo:log ">>   http response : %s" err))))))))))

(defun cacoo:plugin-url-encode-string (str)
  (let ((array (string-to-vector str)))
    (mapconcat 'identity
     (loop for i from 0 below (length array)
           for ch = (aref array i)
           collect
           (cond
            ((eq ch ?\n) "%0D%0A")
            ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch))
             (char-to-string ch))
            (t
             (format "%%%02X" ch)))) nil)))

;; ** コマンドライン起動埋め込みのプラグイン
;; [img:CMD "(command)" (filename) (size:省略可)]
;; ファイルとして書き出すもの
;; <<<
;; commandや書き出し文字列の中の %IN% がテンポラリファイル名、
;; %OUT%が出力ファイル名に入れ替わる。

(defvar cacoo:plugin-cmd-regexp
      "^CMD '\\([^']+\\)' \\([^ \t\r\n]+\\)\\( [0-9]+\\)?")
(defvar cacoo:plugin-cmd-terminator "<<<")

(defun cacoo:plugin-cmd (start end content)
  (when (string-match cacoo:plugin-cmd-regexp content)
    (let ((cmd (match-string 1 content))
          (filename (match-string 2 content))
          (size (match-string 3 content)))
      (cacoo:plugin-cmd-gen 
       start end content cmd filename size))))

(push 'cacoo:plugin-cmd cacoo:plugins)

(defun cacoo:plugin-cmd-gen (start end content cmd filename size)
  (save-excursion
    (goto-char end)
    (and (re-search-forward cacoo:plugin-cmd-terminator)
         (lexical-let* 
             ((t-start (match-beginning 0))
              (t-end (match-end 0)) (cmd cmd)
              (text (buffer-substring (1+ end) (1- t-start)))
              (output-path (cacoo:get-cache-path filename)))
           (cons
            (make-cacoo:$img
             :url (cacoo:plugin-url-create 'command filename)
             :start start :end t-end
             :size (cacoo:aif size (string-to-number it)
                     cacoo:max-size))
            (lambda () 
              (cacoo:plugin-cmd-exec-d cmd output-path text)))))))

(defun cacoo:plugin-cmd-replace-io (text in out)
  (replace-regexp-in-string 
   "%IN%" in (replace-regexp-in-string
              "%OUT%" out text t) t))

(defun cacoo:plugin-cmd-exec-d (cmd output-path text)
  (lexical-let*
      ((tmpfile (format "tmp_%s.txt" (file-name-nondirectory output-path)))
       (rcmd  (cacoo:plugin-cmd-replace-io cmd  tmpfile output-path))
       (rtext (cacoo:plugin-cmd-replace-io text tmpfile output-path))
       (output-path output-path))
    (deferred:$
      (deferred:next
        (lambda (x) (write-region rtext nil tmpfile)))
      (deferred:process-shellc it rcmd)
      (deferred:nextc it 
        (lambda (x) 
          (cacoo:log "CMD EXE[%s] return value -> [%s]" rcmd x)
          (ignore-errors (delete-file tmpfile))
          (unless (cacoo:file-exists-p output-path)
            (error "Can not create the output file : %s" output-path)))))))

;; ** Graphviz図埋め込みのプラグイン
;; [img:DOT (filename) (size:省略可)]
;; dot記述 (http://www.graphviz.org/)
;; <<<

(defvar cacoo:plugin-dot-regexp "DOT \\([^ \n\r\t]+\\)\\( [0-9]+\\)?")

(defun cacoo:plugin-dot-diagram (start end content)
  (when (string-match cacoo:plugin-dot-regexp content)
    (let ((filename (match-string 1 content))
          (size (match-string 2 content)))
      (cacoo:plugin-cmd-gen 
       start end content "dot -Tpng %IN% -o %OUT%" filename size))))

(push 'cacoo:plugin-dot-diagram cacoo:plugins)

;; ** SVG
;; [img:SVG (filename) (size:省略可)]
;; SVG記述
;; <<<

(defvar cacoo:plugin-svg-regexp "SVG \\([^ \n\r\t]+\\)\\( [0-9]+\\)?")

(defun cacoo:plugin-svg (start end content)
  (when (string-match cacoo:plugin-svg-regexp content)
    (let ((filename (match-string 1 content))
          (size (match-string 2 content)))
      (save-excursion
        (goto-char end)
        (when (re-search-forward cacoo:plugin-cmd-terminator)
          (let* ((t-start (match-beginning 0))
                 (t-end (match-end 0))
                 (text (buffer-substring (1+ end) (1- t-start)))
                 (output-path (cacoo:get-cache-path filename)))
            (cons 
             (make-cacoo:$img
              :url (cacoo:plugin-url-create 'long-url filename)
              :start start :end t-end
              :size (cacoo:aif size (string-to-number it)
                      cacoo:max-size))
             (lexical-let ((text text) (output-path output-path))
               (lambda () 
                 (write-region text nil output-path)
                 (deferred:succeed))))))))))

(push 'cacoo:plugin-svg cacoo:plugins)

(provide 'cacoo-plugins)
;;; cacoo-plugins.el ends here
