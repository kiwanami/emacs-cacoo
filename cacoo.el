;;; cacoo.el --- Minor mode for Cacoo (http://cacoo.com)

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
;; Version: 1.7
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

;; A minor mode for editing a document with Cacoo diagrams.  Diagrams
;; are saved for local cache so that you can use the diagrams in the
;; offline environment. Diagrams are re-sized by ImageMagick
;; automatically and displayed in-line.

;; Integrating Emacs with Cacoo, the diagramming tool on the Web, 
;; I'm sure that Emacs becomes the most powerful documentation tool.

;; Not only Cacoo diagrams, but also any images those are indicated by
;; the URL can be displayed.

;;; Installation:

;; This program is dependent on followings:
;; - deferred.el (http://github.com/kiwanami/emacs-deferred/raw/master/deferred.el)
;; - concurrent.el (http://github.com/kiwanami/emacs-deferred/raw/master/concurrent.el)
;; - wget, ImageMagick(convert, identify, display)

;; Put cacoo.el in your load-path, and add following code.

;; (require 'cacoo)
;; (global-set-key (kbd "M--") 'toggle-cacoo-minor-mode) ; key bind example

;;; Usage:

;; This program replaces following markups into diagram images.
;; [img:https://cacoo.com/diagrams/6m4ATG1ddlUiHPqd-0FAF7.png]

;; `cacoo-minor-mode' defines some key binding.
;; (You can customize key bindings through `cacoo-minor-mode-keymap'.

;; * For all diagram markups
;; C-c , T : Revert all diagrams to text in the buffer.
;; C-c , D : Display all diagrams in the buffer. (Using local cache files)
;; C-c , R : Clear all cache files and retrieve diagram images again.

;; * For the just following markup
;; C-c , t : Revert to text
;; C-c , d : Display the diagram
;; C-c , r : Clear the cache file and retrieve the diagram image again
;; C-c , e : Open Cacoo editor with the browser (https://cacoo.com/diagrams/xxxxx/edit)
;; C-c , v : Open Cacoo detail page with the browser (https://cacoo.com/diagrams/xxxxx)
;; C-c , V : Open the diagram image with the external viewer.

;; * In the editing markup
;; C-c , g : Toggle text and image

;; * For Cacoo integration
;; C-c , N : Create a new diagram with the browser (https://cacoo.com/diagrams/new)
;; C-c , l : Open the diagram list with the browser (https://cacoo.com/diagrams/)

;; * Navigation and editing
;; C-c , n : Move to the next diagram
;; C-c , p : Move to the previous diagram
;; C-c , i : Insert a blank diagram markup
;; C-c , y : Insert a diagram markup with the clipboard(or kill-ring) text

;; * Etc
;; C-c , C : Remove all files in the cache directory

;; You can display following URLs:
;; (Of course, you can not edit the images with Cacoo!)

;; - Images on the Web
;;     [img:http://example.com/zzz.png]
;; - Local images (absolute path)
;;     [img:file:///xxx/yyy/zzz.png]
;; - Local images (abstract path from the visiting buffer)
;;     [img:zzz.png]

;; If an error is occurred, this program highlight the markup text.
;; You can check the error messages with the mouse over pop-up.  In
;; most cases, the troubles are 'program (wget, convert, identify) not
;; found' and 'the indicated image not found'.

;;; Customize:

;; This program uses `cacoo:browser-function' as default browser.
;; The default value is `browse-url-browser-function'.

;; Ex: for safari on Mac
;;   (setq cacoo:browser-function 'browse-url-generic)
;;   (setq browse-url-generic-program "open")

;; Changing `cacoo:img-regexp' and `cacoo:img-pattern', you can adopt
;; other markup format. The variable `cacoo:img-regexp' accepts a list
;; of regexp strings.

;; The directory name for the local cache files is set by
;; `cacoo:img-dir'.  If you don't need confirmation of creating
;; directories, set `cacoo:img-dir-ok' non-nil.

;; Large images are reduced by `cacoo:max-size'.
;; You can specify the image size individually, like this.
;; [img:file:///xxx/yyy/zzz.png 600]

;; The external image viewer is set by `cacoo:external-viewer'.
;; If this variable is nil, open the image with Emacs.

;; In Emacs 22, the transparent color of PNG images may display as black.
;; Then, following code maybe solve the problem.
;;   (setq cacoo:png-background "white") 

;; This program can be extended by the plugin mechanism. See the comments
;; for the details (cacoo:plugin-***).

;;; History:

;; Revision 1.7  2010/10/27  sakurai
;; English translation.
;; Rewrite on concurrent.el and deferred.el.
;; 
;; Revision 1.6  2010/07/19  sakurai
;; Added SVG plugin.
;; 
;; Revision 1.5  2010/07/05  sakurai
;; Removing parameter strings from the URL.
;; Added a variable `cacoo:browser-function'.
;; 
;; Revision 1.4  2010/06/17  sakurai
;; Improved `cacoo:img-regexp' that accepts a list of regexp strings.
;; Added a plugin of 'hatena fotolife'
;; 
;; Revision 1.3  2010/05/10  sakurai
;; Added the plugin mechanism.
;; Added a variable `cacoo:translation-exts'.
;; 
;; Revision 1.2  2010/05/08  sakurai
;; Bug fixed: byte-compiling (thx id:kitokitoki)
;; Added: some documents.
;; Improved: mouse clicking.
;; 
;; Revision 1.1  2010/05/08  sakurai
;; Improved: highlighting error markups by overlay.
;; Improved: asynchronous tasks.
;; Improved: adopting transparent PNG and older ImageMagick program.
;;
;; Revision 1.0  2010/05/07  sakurai
;; Initial revision


;; * memo
;; clear-cache
;; anything-integration
;; get image by api
;; plugins

;;; Code:

(eval-when-compile (require 'cl))
(require 'url-file)
(require 'concurrent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize variables

(defvar cacoo:api-key "y6KlL0PdnxBc5QNOln7N" "Set your API key!")


(defvar cacoo:img-regexp "\\[img:\\(.*\\)\\][^]\n\r]*$" "Markup regexp for searching diagrams.")
(defvar cacoo:img-pattern "[img:%s]" "Pattern for inserting a markup")
(defvar cacoo:key-regexp "diagrams\\/\\([a-zA-Z0-9]*\\)" "Regexp for extracting a diagram key in Cacoo.")

(defvar cacoo:img-dir ".cimg" "Directory name for cache files.")
(defvar cacoo:img-dir-ok nil "If non-nil, this program does not confirm creating a cache directory.")

(defvar cacoo:process-num 1 "Maximum external process number")
(defvar cacoo:cmd-copy "cp" "Copy command")
(defvar cacoo:copy-by-command t "If non-nil, this program copies files by the external command asynchronously. If nil, this program uses Emacs copy function `copy-file' synchronously.")

(defvar cacoo:max-size 450 "Default maximum image size.")

(defvar cacoo:external-viewer "display" "External viewer command. If nil, this program opens the image file in Emacs.")
(defvar cacoo:png-background nil "If the transparent color of PNG images seems not to be good, set it non-nil.")

(defvar cacoo:translation-exts '("eps" "ps") "A list of the extensions those need to translate to display in Emacs.")
(defvar cacoo:browser-function browse-url-browser-function "The browser to open the Cacoo editor.")

;;; Internal variables

(defvar cacoo:base-url "https://cacoo.com/diagrams/" "[internal] The base URL in Cacoo")
(defvar cacoo:new-url (concat cacoo:base-url "new") "[internal] URL for creating a diagram in Cacoo")
(defvar cacoo:edit-url (concat cacoo:base-url "%KEY%/edit") "[internal] URL for editing a diagram in Cacoo")
(defvar cacoo:view-url (concat cacoo:base-url "%KEY%") "[internal] URL for the diagram details in Cacoo")
(defvar cacoo:list-url cacoo:base-url "[internal] URL for the diagram list in Cacoo")

(defvar cacoo:plugins nil "[internal] A list of plugin symbols.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fundamental Functions

(defmacro cacoo:aif (test-form then-form &rest else-forms)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'cacoo:aif 'lisp-indent-function 2)

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



;;; File Utilities

(defun cacoo:get-cache-dir() 
  (let* ((base-dir (file-name-directory 
                    (or buffer-file-name
                        default-directory))))
    (expand-file-name cacoo:img-dir base-dir)))

(defun cacoo:fix-directory ()
  "Make a directory for cache files in the current directory which has visiting file."
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

(defun cacoo:get-resize-path (filename size)
  (let ((ext (file-name-extension filename)))
    (expand-file-name
     (format "resize_%s_%s"
             size
             (if (member ext cacoo:translation-exts)
                 (replace-regexp-in-string
                  (concat "\\." ext "$") ".png" filename)
               filename))
             (cacoo:get-cache-dir))))

(defun cacoo:get-filename-from-url (url)
  (cond
   ((string-match "^http" url)
    (url-file-nondirectory url))
   (t
    (if (string-match "[^/]*$" url)
        (match-string 0 url)))))

(defun cacoo:get-cache-path-from-url (url)
  (cacoo:get-cache-path (cacoo:get-filename-from-url url)))

(defun cacoo:get-resize-path-from-url (url size)
  (cacoo:get-resize-path (cacoo:get-filename-from-url url) size))


(defun cacoo:get-local-path-from-url(url)
  (cond
   ((string-match "^file://\\(.*\\)$" url) ; assuming full path
    (match-string 1 url))
   ((cacoo:file-exists-p (expand-file-name url default-directory))
    (expand-file-name url default-directory))
   (t
    nil)))


(defun cacoo:file-exists-p (file)
  (and (file-exists-p file)
       (< 0 (nth 7 (file-attributes file)))))

(defun cacoo:get-image-type (file)
  (let ((type (intern (file-name-extension file))))
    (cond 
     ((eq type 'jpg) 'jpeg)
     (t type))))

(defun cacoo:get-key-from-url (url)
  (if (string-match cacoo:key-regexp url)
      (match-string 1 url)
    nil))

(defun cacoo:make-url (tmpl-url key)
  (if key (replace-regexp-in-string "%KEY%" key tmpl-url t) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Struct

;; cacoo:$img   : Structure for the image markups
;; url          : URL for an image or relative path. It is also a key for the dataflow variable.
;; start        : Start point for a markup
;; end          : End point for a markup
;; size         : Maximum image size along the long axis.

(defstruct cacoo:$img url start end size)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image Workplace

(defvar cacoo:image-wp-process-semaphore nil)

(defun cacoo:image-wp-acquire-semaphore-d (&optional d)
  (cond
   (d (deferred:nextc d 
        (lambda (x) 
          (lexical-let ((x x))
            (deferred:nextc 
              (cc:semaphore-acquire cacoo:image-wp-process-semaphore)
              (lambda (y) x))))))
   (t (cc:semaphore-acquire cacoo:image-wp-process-semaphore))))

(defun cacoo:image-wp-release-semaphore-d (d)
  (deferred:nextc d
    (lambda (x) 
      (cc:semaphore-release cacoo:image-wp-process-semaphore)
      x)))

(defun cacoo:image-wp-resized-equal (a b)
  (and (string-equal (car a) (car b))
       (equal (cdr a) (cdr b))))

(defvar cacoo:image-wp-original nil)
(defvar cacoo:image-wp-resized  nil)

(defun cacoo:image-wp-init ()
  (setq cacoo:image-wp-process-semaphore (cc:semaphore-create cacoo:process-num)
        cacoo:image-wp-original (cc:dataflow-environment nil 'equal)
        cacoo:image-wp-resized  (cc:dataflow-environment nil 'cacoo:image-wp-resized-equal))
  (cc:dataflow-connect cacoo:image-wp-original 'get-first 'cacoo:image-wp-create-original)
  (cc:dataflow-connect cacoo:image-wp-resized  'get-first 'cacoo:image-wp-create-resized)
  (when cacoo:debug
    (cc:dataflow-connect 
     cacoo:image-wp-original t (lambda (args) (cacoo:log "DF ORG / %S" args)))
    (cc:dataflow-connect 
     cacoo:image-wp-resized  t (lambda (args) (cacoo:log "DF RSZ / %S" args)))))

(defun cacoo:image-wp-get-resized-d (url size)
  ;;ここを呼ぶ
  ;;deferredの引数には画像ファイル名が渡ってくる
  (cc:dataflow-get cacoo:image-wp-resized (cons url size)))

(defun cacoo:image-wp-get-original-d (url)
  ;;ここを呼ぶ
  ;;deferredの引数には画像ファイル名が渡ってくる
  (cc:dataflow-get cacoo:image-wp-original url))

(defun cacoo:image-wp-set-resized (url size file)
  (cc:dataflow-set cacoo:image-wp-resized (cons url size) file))
  
(defun cacoo:image-wp-set-original (url file)
  (cc:dataflow-set cacoo:image-wp-original url file))

(defun cacoo:image-wp-create-resized (args)
  ;;画像をリサイズしてWPに追加
  (destructuring-bind (event ((url . size))) args
    (cacoo:resize-diagram url size)))

(defun cacoo:image-wp-create-original (args)
  ;; 画像を取ってきてWPに追加
  (destructuring-bind (event (url)) args
    (cacoo:fix-directory)
    (cond
     ((string-match "^file:\\/\\/\\/" url) ; local
      (cacoo:load-diagram-local url))
     ((string-match "^https?:\\/\\/" url) ; web
      (cacoo:load-diagram-remote url))
     (t ; 相対パスを仮定
      (cacoo:load-diagram-local url)))))

;;; Create Original Image Cache

(defun cacoo:load-diagram-remote(url)
  (cacoo:log ">> cacoo:load-diagram-remote : %s" url)
  (lexical-let* ((org-path (cacoo:get-cache-path-from-url url))
                 (url url))
    (cond
     ((cacoo:file-exists-p org-path)
      (cacoo:image-wp-set-original url org-path))
     (t
      (deferred:$
        (cacoo:image-wp-acquire-semaphore-d)
        (deferred:processc it
          "wget" "-q" 
          "--no-check-certificate" ; 古い証明書しかないとcacooのサイトが検証できないため
          "-O" org-path url)
        (deferred:nextc it
          (lambda (msg)
            (cacoo:image-wp-set-original 
             url (if (cacoo:file-exists-p org-path) org-path
                   (cons 'error msg)))))
        (deferred:error it
          (lambda (msg)
            (cacoo:image-wp-set-original url (cons 'error msg))))
        (cacoo:image-wp-release-semaphore-d it))))))

(defun cacoo:load-diagram-local(url)
  (cacoo:log ">> cacoo:load-diagram-local : %s" url)
  (let* ((url url)
         (from-path (cacoo:get-local-path-from-url url))
         (cache-path (cacoo:get-cache-path-from-url filename)))
    (cond
     ((cacoo:file-exists-p cache-path)
      (cacoo:image-wp-set-original url cache-path))
     (t
      (deferred:$
        (cacoo:image-wp-acquire-semaphore-d)
        (cacoo:copy-file-d it from-path cache-path)
        (deferred:nextc it
          (lambda (x) 
            (cacoo:image-wp-set-original url cache-path)))
        (deferred:error it
          (lambda (msg) 
            (cacoo:image-wp-set-original url
             (cons 'error (format "Can not copy file %s -> %s" 
                                  from-path cache-path)))))
        (cacoo:image-wp-release-semaphore-d it))))))

(defun cacoo:copy-file-d (d from-path to-path)
  (unless d (setq d (deferred:next 'identity)))
  (lexical-let ((from-path from-path) (to-path to-path))
    (deferred:$
      (if cacoo:copy-by-command
          (deferred:processc d cacoo:cmd-copy from-path to-path)
        (deferred:nextc d
          (lambda (x) (ignore-errors (copy-file from-path to-path t t)))))
      (deferred:nextc it
        (lambda (x) 
          (unless (cacoo:file-exists-p to-path)
            (error "Can not copy the file : %s -> %s" from-path to-path)))))))

;;; Create Resized Image Cache

(defun cacoo:identify-diagram-d (d)
  (deferred:nextc d
    (lambda (file)
      (deferred:$
        (deferred:process "identify" "-format" "%w %h" file)
        (deferred:nextc it
          (lambda (line)
            (let* ((cols (split-string line " "))
                   (width (string-to-number (car cols)))
                   (height (string-to-number (cadr cols))))
              (cacoo:log "SIZE %S > %s" line (max width height))
              (max width height))))))))

(defun cacoo:resize-diagram (url max-size)
  (cacoo:log ">> cacoo:resize-diagram : %s / %s" url max-size)
  (lexical-let ((url url) (max-size max-size))
    (deferred:$
      (cacoo:image-wp-get-original-d url)
      (cacoo:image-wp-acquire-semaphore-d it)
      (cacoo:identify-diagram-d it)
      (deferred:nextc it
        (lambda (org-size)
          (let ((resize-path (cacoo:get-resize-path-from-url url max-size))
                (not-resizep (< org-size max-size)))
            (cond
             ((cacoo:file-exists-p resize-path)
              (cacoo:image-wp-set-resized url max-size resize-path))
             ((= 0 org-size)
              (cacoo:image-wp-set-resized 
               url max-size (cons 'error (format "Can not copy file %s" url))))
             (cacoo:png-background
              (cacoo:resize-diagram-for-fillbg url max-size not-resizep))
             (t
              (cacoo:resize-diagram-for-transparent url max-size not-resizep)))
            nil)))
      (cacoo:image-wp-release-semaphore-d it))))

(defun cacoo:resize-diagram-for-transparent (url max-size not-resizep)
  (cacoo:log ">> cacoo:resize-diagram-for-transparent : %s / %s / resize: %s" url max-size not-resizep)
  (lexical-let
      ((url url) (max-size max-size)
       (org-path (cacoo:get-cache-path-from-url url))
       (resize-path (cacoo:get-resize-path-from-url url max-size)))
    (deferred:$
      (cond 
       ((and not-resizep
             (equal (file-name-extension org-path)
                    (file-name-extension resize-path)))
        (cacoo:copy-file-d nil org-path resize-path))
       (t
        (deferred:$
          (cacoo:image-wp-acquire-semaphore-d)
          (deferred:processc it
            "convert" "-resize" (format "%ix%i" max-size max-size)
            "-transparent-color" "#ffffff"
            org-path (concat (file-name-extension resize-path)
                             ":" resize-path))
          (cacoo:image-wp-release-semaphore-d it))))
      (deferred:nextc it
        (lambda (msg)
          (if (cacoo:file-exists-p resize-path)
              (cacoo:image-wp-set-resized url max-size resize-path))))
      (deferred:error it
        (lambda (msg) 
          (cacoo:image-wp-set-resized url max-size
           (cons 'error (format "Can not resize image %s -> %s" 
                                org-path resize-path))))))))

(defun cacoo:get-background-img-path (path)
  (expand-file-name
   (format "back_%s" (file-name-nondirectory path))
   (file-name-directory path)))

(defun cacoo:resize-diagram-for-fillbg (url max-size not-resizep)
  (cacoo:log ">> cacoo:resize-diagram-for-fillbg : %s / %s / resize: %s" url max-size not-resizep)
  (lexical-let*
      ((url url) (max-size max-size)
       (org-path (cacoo:get-cache-path-from-url url))
       (resize-path (cacoo:get-resize-path-from-url url max-size))
       resize-size 
       (tmpfile (cacoo:get-background-img-path org-path)))
    (deferred:$
      (cond
       ((and not-resizep ; 小さい場合はコピー
             (equal (file-name-extension org-path)
                    (file-name-extension resize-path)))
        (cacoo:copy-file-d nil org-path resize-path))
       (t ; 通常はリサイズする
        (deferred:process
          "convert" org-path "-resize" (format "%ix%i" max-size max-size)
          (concat (file-name-extension resize-path) ":" resize-path))))
      (deferred:processc it ; 縮小した画像のサイズを取得
        "identify" "-format" "%wx%h" resize-path)
      (deferred:nextc it ; 縮小した画像と同じサイズの背景画像を準備
        (lambda (dim) (deferred:process "convert" "-size" dim
                      (concat "xc:" cacoo:png-background) tmpfile)))
      (deferred:processc it "convert" tmpfile resize-path "-flatten" resize-path)
      (deferred:nextc it
        (lambda (x)
          (if (cacoo:file-exists-p resize-path)
              (cacoo:image-wp-set-resized url max-size resize-path))))
      (deferred:error it
        (lambda (msg) 
          (cacoo:image-wp-set-resized url max-size
           (cons 'error (format "Can not resize image %s -> %s" 
                                org-path resize-path))))))))



;;; Display

(defun cacoo:display-diagram-revert (data)
  (let ((mod (buffer-modified-p))
        (start (cacoo:$img-start data))
        (end (cacoo:$img-end data)))
    (remove-text-properties start end
     '(display nil mouse-face nil help-echo nil keymap nil))
    (cacoo:display-diagram-overlay-remove start end)
    (set-buffer-modified-p mod)))

(defun cacoo:display-diagram (data)
  (lexical-let ((data data)
                (start (cacoo:$img-start data))
                (end (cacoo:$img-end data)))
    (cacoo:log ">> cacoo:display-diagram : %s / %s" (cacoo:$img-url data) (cacoo:$img-size data))
    (deferred:$
      (cacoo:image-wp-get-resized-d (cacoo:$img-url data) (cacoo:$img-size data))
      (deferred:nextc it
        (lambda (x) 
          (if (and (consp x) (eq 'error (car x))) (error (cdr x))
            (cacoo:display-diagram-by-image x data))))
      (deferred:error it
        (lambda (e) (cacoo:display-diagram-by-text start end e))))))

(defun cacoo:display-diagram-by-image (image-file data)
  (cacoo:log ">> cacoo:display-diagram-by-image : %s <- %s / %s"
                 image-file (cacoo:$img-url data) (cacoo:$img-size data))
  (clear-image-cache)
  (let ((img (ignore-errors 
               (create-image image-file
                             (cacoo:get-image-type image-file) nil
                             :relief 1)))
        (start (cacoo:$img-start data))
        (end (cacoo:$img-end data))
        (map (make-sparse-keymap))
        (mod (buffer-modified-p)))
    (cond
     ((null img)
      (cacoo:display-diagram-by-text
       start end (format "The Emacs could not display this image type.")))
     (t
      (define-key map [mouse-1] 'cacoo:do-click-link)
      (define-key map (kbd "\n") 'cacoo:do-click-link)
      (add-text-properties 
       start end (list 'display img 'keymap map 'mouse-face 'highlight))
      (cacoo:display-diagram-overlay-remove start end)
      (set-buffer-modified-p mod)))))

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

(defun cacoo:display-diagram-by-text (start end msg)
  (cacoo:log ">> cacoo:display-diagram-by-text : %s" msg)
  (let ((mod (buffer-modified-p)))
    (put-text-property start end
     'help-echo (format "Cacoo: Error  %s" msg))
    (cacoo:display-diagram-overlay-add start end)
    (set-buffer-modified-p mod)))

;;; Error overlays

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



;;; Navigation

(defun cacoo:image-search (&optional backward)
  ;;バッファ上のポイント位置以降の画像のテキストパターンを
  ;;re-search-forward（もしくはbackwardがnil以外の場合re-search-backward）で検索する。
  ;;cacoo:img-regexpが文字列の場合は普通に re-search-forward する。
  ;;cacoo:img-regexpがリストの場合、複数のパターンが入っているものと見
  ;;なして、一番近所のポイント位置を返す。
  (let ((f (if backward 're-search-backward 're-search-forward))
        (cmp (if backward '> '<)))
    (cond
     ((stringp cacoo:img-regexp)
      (funcall f cacoo:img-regexp nil t))
     ((listp cacoo:img-regexp)
      (loop for re in cacoo:img-regexp
            with val = nil ; (pos . match-data)
            do
            (save-excursion
              (let ((pos (funcall f re nil t)))
                (when pos
                  (when (or (null val) (funcall cmp pos (car val)))
                    (setq val (cons pos (match-data)))))))
            finally return
            (progn 
              (if val 
                  (progn
                    (set-match-data (cdr val))
                    (goto-char (car val))
                    (car val))))))
     (t
      (error "cacoo:img-regexp is not regexp pattern. [%s]" cacoo:img-regexp)))))

(defun cacoo:try-plugins (start end content)
  ;;プラグインを試してみる
  (loop for i in cacoo:plugins
        for data = (funcall i start end content)
        if data 
        return data
        finally return nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cacoo API

(defun cacoo:image-search-forward ()
  (cacoo:image-search))

(defun cacoo:image-search-backward ()
  (cacoo:image-search t))

(defun cacoo:do-next-diagram (action)
  (cond
   ((cacoo:image-search-forward)
      (let* 
          ((start (match-beginning 0)) (end (match-end 0))
           (content (match-string 1)) data)
        (setq data 
              (cacoo:aif 
                  (cacoo:try-plugins start end content) it 
                (let* ((cols (split-string content "[ \t]+"))
                       (url (car cols))
                       (size (cacoo:aif (cadr cols)
                                 (string-to-number it) cacoo:max-size)))
                  (make-cacoo:$img :url url :start start :end end :size size))))
        (funcall action data)
        (goto-char end)))
   (t nil)))

(defun cacoo:load-next-diagram ()
  (cacoo:do-next-diagram 'cacoo:display-diagram))

(defun cacoo:revert-next-diagram ()
  (cacoo:do-next-diagram 'cacoo:display-diagram-revert))

(defun cacoo:clear-cache-next-diagram ()
  (save-excursion
    (cacoo:do-next-diagram 
     (lambda (data) 
       
       ))))

(defun cacoo:clear-all-cache-files ()
  "Clear all files in the current cache directory."
  (let ((imd-dir (cacoo:get-cache-dir)))
    (loop for i in (directory-files imd-dir)
          for f = (expand-file-name i imd-dir)
          if (file-regular-p f)
          do (delete-file f))))

(defun cacoo:view-original-cached-image (url)
  (deferred:$
    (cacoo:image-wp-get-original-d url)
    (deferred:nextc it
      (lambda (cached-file) 
        (cond
         (cacoo:external-viewer
          (deferred:process cacoo:external-viewer cached-file))
         (t
          (find-file cached-file)))))))

(defun cacoo:insert-pattern-url (url)
  (insert (format cacoo:img-pattern url)))

(defun cacoo:open-browser (url)
  (let ((browse-url-browser-function cacoo:browser-function))
    (browse-url url)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands

(defun cacoo:insert-pattern-command ()
  (interactive)
  (cacoo:insert-pattern-url ""))

(defun cacoo:insert-yank-command ()
  (interactive)
  (cacoo:insert-pattern-url (current-kill 0)))

(defun cacoo:view-local-cache-next-diagram-command () 
  (interactive)
  ;;原寸大の画像をローカルの環境で参照する
  (cacoo:do-next-diagram
   (lambda (data)
     (cacoo:view-original-cached-image (cacoo:$img-url data)))))

(defun cacoo:clear-all-cache-files-command ()
  (interactive)
  ;;キャッシュフォルダの中身を空にする
  (when (yes-or-no-p "Delete all local cache files?")
    (cacoo:clear-all-cache-files)
    (message "Delete all local cache files.")))

(defun cacoo:reload-next-diagram-command ()
  (interactive)
  ;;カーソール直後の図をリロードする
  (cacoo:clear-cache-next-diagram)
  (save-excursion
    (cacoo:load-next-diagram)))

(defun cacoo:reload-or-revert-current-diagram-command ()
  (interactive)
  ;;カーソールのある図をリロードする
  ;;(画像マーカーの先頭に移動してリロード)
  (save-excursion
    (cond
     ((get-text-property (point) 'display)
      (end-of-line)
      (cacoo:navi-prev-diagram-command)
      (cacoo:revert-next-diagram))
     (t
      (end-of-line)
      (cacoo:navi-prev-diagram-command)
      (cacoo:clear-cache-next-diagram)
      (cacoo:load-next-diagram)))))

(defun cacoo:reload-all-diagrams-command ()
  (interactive)
  ;;バッファ内のすべての図を更新する
  (save-excursion
    (goto-char (point-min))
    (while (cacoo:image-search-forward)
      (cacoo:clear-cache-next-diagram))
    (goto-char (point-min))
    (while (cacoo:load-next-diagram))))

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
  (cacoo:open-browser cacoo:new-url))

(defun cacoo:open-diagram-list-command ()
  (interactive)
  ;;ブラウザで図の一覧を開く
  (cacoo:open-browser cacoo:list-url))

(defun cacoo:edit-next-diagram-command ()
  (interactive)
  ;;カーソール直後の図をブラウザで編集する
  (cacoo:open-diagram-gen cacoo:edit-url))

(defun cacoo:open-diagram-gen (tmpl-url)
  ;;カーソール直後の図をブラウザで編集する
  (save-excursion
    (cacoo:do-next-diagram
     (lambda (data) 
       (let* ((url (cacoo:$img-url data))
              (key (cacoo:get-key-from-url url))
              (open-url (cacoo:make-url tmpl-url key)))
         (cacoo:open-browser (or open-url url)))))))

(defun cacoo:view-next-diagram-command ()
  (interactive)
  ;;カーソール直後の図の詳細画面をブラウザで開く
  (cacoo:open-diagram-gen cacoo:view-url))

(defun cacoo:navi-next-diagram-command ()
  (interactive)
  (end-of-line)
  (if (cacoo:image-search-forward)
      (beginning-of-line)))

(defun cacoo:navi-prev-diagram-command ()
  (interactive)
  (if (cacoo:image-search-backward)
      (beginning-of-line)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor mode, Key bindings and Menu

(defvar cacoo-minor-mode-keymap
      (cacoo:define-keymap
       '(("C-c , C-q" . cacoo:minor-mode-off-command)
         ("C-c , n"   . cacoo:navi-next-diagram-command)
         ("C-c , p"   . cacoo:navi-prev-diagram-command)

         ("C-c , r"   . cacoo:reload-next-diagram-command)
         ("C-c , R"   . cacoo:reload-all-diagrams-command)

         ("C-c , g"   . cacoo:reload-or-revert-current-diagram-command)

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
  (cacoo:image-wp-init)
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
    ["Display all diagrams" cacoo:display-all-diagrams-command t]
    ["Revert all diagrams" cacoo:revert-all-diagrams-command t]
    ["Reload all diagrams" cacoo:reload-all-diagrams-command t]
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

;; (setq cacoo:png-background nil)
;; (setq cacoo:png-background "white")
;; (setq cacoo:debug t)
;; (setq cacoo:debug nil)
;; (setq cacoo:plugins nil)

(provide 'cacoo)
;;; cacoo.el ends here
