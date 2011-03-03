;;; cacoo.el --- Minor mode for Cacoo (http://cacoo.com)

;; Copyright (C) 2010, 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai atmark kiwanami.net>
;; Version: 2.0
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
;; - anything.el (http://www.emacswiki.org/emacs/Anything)
;; - deferred.el (http://github.com/kiwanami/emacs-deferred/raw/master/deferred.el)
;; - concurrent.el (http://github.com/kiwanami/emacs-deferred/raw/master/concurrent.el)
;; - wget, ImageMagick(convert, identify, display)

;; Put cacoo.el and cacoo-plugins.el in your load-path, and add following code.

;; (require 'cacoo)
;; (require 'cacoo-plugins)      ; option
;; (setq cacoo:api-key "APIKEY") ; option
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

;; Revision 2.0  2011/03/03  sakurai
;; English translation for docstrings.
;; Re-build on the architecture of concurrent.el and deferred.el.
;; Added the anything interface for Cacoo diagrams.
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

;;; Code:

(eval-when-compile (require 'cl))
(require 'url-file)
(require 'concurrent)
(require 'json)

(defvar cacoo:version nil "version number")
(setq cacoo:version "2.0")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize variables

(defvar cacoo:api-key nil "Set your API key!")


(defvar cacoo:img-regexp "\\[img:\\(.*\\)\\][^]\n\r]*$" "Markup regexp for searching diagrams.")
(defvar cacoo:img-pattern "[img:%s]" "Pattern for inserting a markup")

(defvar cacoo:img-dir ".cimg" "Directory name for cache files.")
(defvar cacoo:img-dir-ok nil "If non-nil, this program does not confirm creating a cache directory.")

(defvar cacoo:process-num 4 "Maximum external process number")
(defvar cacoo:cmd-copy "cp" "Copy command")
(defvar cacoo:copy-by-command t "If non-nil, this program copies files by the external command asynchronously. If nil, this program uses Emacs copy function `copy-file' synchronously.")

(defvar cacoo:max-size 450 "Default maximum image size.")

(defvar cacoo:external-viewer "display" "External viewer command. If nil, this program opens the image file in Emacs.")
(defvar cacoo:png-background nil "If the transparent color of PNG images seems not to be good, set it non-nil.")

(defvar cacoo:translation-exts '("eps" "ps" "svg") "A list of the extensions those need to translate to display in Emacs.")
(defvar cacoo:browser-function browse-url-browser-function "The browser to open the Cacoo editor.")

(defvar cacoo:http-get-file-cmd '("wget" "-q" "-S" "--no-check-certificate" "-O" output-file url))
(defvar cacoo:http-get-stdout-cmd '("wget" "-q" "-O" "-" url))

(defvar cacoo:api-diagrams-cache-dir "~/.emacs.d/cacoo/" "Cache directory for Cacoo data.")
(defvar cacoo:api-diagrams-cache-file "diagrams.dat" "Cache file for Cacoo diagrams.")

;;; Internal variables

(defvar cacoo:key-regexp "diagrams\\/\\([a-zA-Z0-9]*\\)" "Regexp for extracting a diagram key in Cacoo.")
(defvar cacoo:api-url-base "https://cacoo.com/api/v1/" "[internal] Cacoo API base URL.")

(defvar cacoo:base-url "https://cacoo.com/diagrams/" "[internal] The base URL in Cacoo")
(defvar cacoo:new-url (concat cacoo:base-url "new") "[internal] URL for creating a diagram in Cacoo")
(defvar cacoo:edit-url (concat cacoo:base-url "%KEY%/edit") "[internal] URL for editing a diagram in Cacoo")
(defvar cacoo:view-url (concat cacoo:base-url "%KEY%") "[internal] URL for the diagram details in Cacoo")
(defvar cacoo:list-url cacoo:base-url "[internal] URL for the diagram list in Cacoo")

(defvar cacoo:plugins nil "[internal] A list of plugin symbols.")

(defvar cacoo:process-semaphore nil "[internal] Semaphore object for external processes. The number of processes is controlled by `cacoo:process-num'.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fundamental Functions

(defmacro cacoo:aif (test-form then-form &rest else-forms)
  (declare (debug (form form &rest form)))
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

(defun cacoo:k (key alist)
  (or (cdr (assq key alist)) ""))

;;; for debug

(eval-and-compile
  (defvar cacoo:debug nil "Debug output switch.")) ; debug
(defvar cacoo:debug-count 0 "[internal] Debug output counter.") ; debug

(defmacro cacoo:log (&rest args)
  "[internal] Debug macro."
  (when cacoo:debug
    `(progn 
       (with-current-buffer (get-buffer-create "*cacoo:debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" cacoo:debug-count (format ,@args)))))
       (incf cacoo:debug-count))))

(defun cacoo:message-mark ()
  "[internal] Debug marker."
  (interactive)
  (cacoo:log "==================== mark ==== %s" 
             (format-time-string "%H:%M:%S" (current-time))))

(defun cacoo:debug-report-semaphore ()
  "[internal] Show current status of semaphores."
  (interactive)
  (message
   "Semaphore: process permits: %s / waiting: %s  preview permit: %s / waiting: %s"
   (cc:semaphore-permits cacoo:process-semaphore)
   (length (cc:semaphore-waiting-deferreds cacoo:process-semaphore))
   (cc:semaphore-permits cacoo:preview-semaphore)
   (length (cc:semaphore-waiting-deferreds cacoo:preview-semaphore))))

(defmacro cacoo:api-debug-deferred (d msg &rest args)
  "[internal] Debug message output of deferred tasks."
  `(deferred:nextc ,d
     (lambda (x) (funcall 'message ,msg ,@args) x)))

(defun cacoo:api-debug-dbuffer (d)
  "[internal] Debug pp output of deferred tasks."
  (deferred:nextc d
    (lambda (x)
      (pop-to-buffer
       (with-current-buffer (get-buffer-create "*cacoo:api*")
         (erase-buffer)
         (insert (pp-to-string x))
         (current-buffer))))))



;;; File Utilities

(defun cacoo:get-cache-dir()
  "[internal] Return the cache directory for current buffer. If
current buffer does not have visiting file, return
`default-directory'."
  (let* ((base-dir (file-name-directory 
                    (or buffer-file-name
                        default-directory))))
    (expand-file-name cacoo:img-dir base-dir)))

(defun cacoo:fix-directory ()
  "[internal] Make a directory for cache files in the current
directory which has visiting file."
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
  "[internal] Return an absolute path for the cache file for
FILENAME. FILENAME does not include directory path. If the cache
directory does not exist, this function creates it."
  (expand-file-name 
   filename (cacoo:get-cache-dir)))

(defun cacoo:get-resize-path (filename size)
  "[internal] Return an absolute  path for the resized cache file for
FILENAME. FILENAME does not include directory path. If the cache
directory does not exist, this function creates it."
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
  "[internal] Return a filename that does not include directory
path from URL. URL can also be a local relative path."
  (cond
   ((string-match "^http" url)
    (url-file-nondirectory url))
   (t
    (if (string-match "[^/]*$" url)
        (match-string 0 url)))))

(defun cacoo:get-cache-path-from-url (url)
  "[internal] Return an absolute path for the cache of
URL. If the cache directory does not exist, this function creates
it."
  (cacoo:get-cache-path (cacoo:get-filename-from-url url)))

(defun cacoo:get-resize-path-from-url (url size)
  "[internal] Return an absolute path for the resized cache of
URL. If the cache directory does not exist, this function creates
it."
  (cacoo:get-resize-path (cacoo:get-filename-from-url url) size))

(defun cacoo:get-local-path-from-url(url)
  "[internal] Return an absolute path for the cache of URL for
local filesystem."
  (cond
   ((string-match "^file://\\(.*\\)$" url) ; assuming full path
    (match-string 1 url))
   ((cacoo:file-exists-p (expand-file-name url default-directory))
    (expand-file-name url default-directory))
   (t
    nil)))

(defun cacoo:file-exists-p (file)
  "[internal] Return non-nil, if FILE exists and the file size is
larger than zero byte."
  (cond
   ((file-exists-p file)
    (cond
     ((< 0 (nth 7 (file-attributes file))) t)
     (t (delete-file file) nil)))
   (t nil)))

(defun cacoo:get-image-type (file)
  "[internal] Return the image type as symbol. This function
guesses from the filename extension."
  (let ((type (intern (file-name-extension file))))
    (cond 
     ((eq type 'jpg) 'jpeg)
     (t type))))

(defun cacoo:get-key-from-url (url)
  "[internal] Return the key string of the cacoo diagram of
URL. If URL has no key, return nil."
  (if (string-match cacoo:key-regexp url)
      (match-string 1 url)
    nil))

(defun cacoo:make-url (tmpl-url key)
  "[internal] Return an URL which is compiled from TMPL-URL and KEY."
  (if key (replace-regexp-in-string "%KEY%" key tmpl-url t) nil))

(defun cacoo:list-template (template-list data-alist)
  "[internal] Return an expanded list which is compiled
TEMPLATE-LIST and DATA-ALIST."
  (loop for i in template-list
        collect
        (cond 
         ((symbolp i)
          (cacoo:k i data-alist))
         (t i))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cacoo API Functions

(defun cacoo:api-param-serialize (params)
  "[internal] Return an serialized string for URL in which PARAMS are encoded."
  (cond
   (params
    (mapconcat
     'identity
     (loop for p in params
           collect (format "%s=%s" (car p) (cdr p)))
     "&"))
   (t "")))

(defvar cacoo:api-cancel-flag nil
  "[internal] Global working state. The deferred tasks watch this variable.")

(defun cacoo:api-get-d (method &optional params)
  "[internal] Return a deferred object which retrieves a JSON
data via cacoo API. The next deferred receives an alist that is
parsed as JSON. If something is wrong, `nil' is passed."
  (lexical-let
      ((url (concat cacoo:api-url-base method ".json"
                    "?" (cacoo:api-param-serialize
                         (cons (cons 'apiKey cacoo:api-key)
                               params)))))
    (cc:semaphore-with cacoo:process-semaphore
      (lambda (x) 
        (unless cacoo:api-cancel-flag
          (deferred:$
            (apply 'deferred:process 
                   (cacoo:list-template 
                    cacoo:http-get-stdout-cmd `((url . ,url))))
            (deferred:nextc it
              (lambda (x)
                (let ((json-array-type 'list))
                  (json-read-from-string x))))))))))

(defun cacoo:http-get-apikey (url)
  "[internal] Return a cacoo diagram key which is extracted from
URL. If no key is found, return nil."
  (if (and cacoo:api-key
           (string-match (regexp-quote cacoo:api-url-base) url))
      (concat url "?" (cacoo:api-param-serialize
                       (list (cons 'apiKey cacoo:api-key))))
    url))

(defun cacoo:http-get-d (url output-path)
  "[internal] Return a deferred object which retrieves a file via
HTTP GET. The received file is saved as OUTPUT-PATH.  The next
deferred object receives `nil' if receiving finishes normally.
If something is wrong, the http-response text is passed."
  (lexical-let ((url (cacoo:http-get-apikey url))
                (output-path output-path))
    (cacoo:log "  >> URL %s" url)
    (deferred:try
      (deferred:$
        (apply 'deferred:process
               (cacoo:list-template 
                cacoo:http-get-file-cmd 
                `((output-file . ,output-path) (url . ,url))))
        (deferred:nextc it
          (lambda (response-text)
            (cacoo:log "  >> RESPONSE : %s" response-text)
            (let* ((headers (split-string response-text "[\r\n]+"))
                   (response (car headers)))
              (if (string-match "200" response)
                  nil 
                (ignore-errors (delete-file output-path))
                (cacoo:log "  >> RESPONSE : %s" response-text)
                response)))))
      :catch
      (lambda (err)
        (cacoo:log "  >> HTTP GET Error : %s" err)
        (error (format "Can not access / HTTP GET : %s" url))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Struct

;; cacoo:$img   : Structure for the image markups
;; url          : URL for an image or relative path. It is also a key for the dataflow variable.
;; start        : Start point for a markup
;; end          : End point for a markup
;; size         : Maximum pixel size along the long axis.

(defstruct cacoo:$img url start end size)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image Workplace


(defun cacoo:image-wp-resized-equal (a b)
  "[internal] If the URL and image size of A is identical to ones of B, return non-nil."
  (and (string-equal (car a) (car b))
       (equal (cdr a) (cdr b))))

(defvar cacoo:image-wp-original nil "[internal] Dataflow environment object for cache files.")
(defvar cacoo:image-wp-resized  nil "[internal] Dataflow environment object for resized cache files.")
(make-variable-buffer-local 'cacoo:image-wp-original)
(make-variable-buffer-local 'cacoo:image-wp-resized)

(defun cacoo:image-wp-init ()
  "[internal] Initialize image workplaces those are implemented by dataflow objects."
  (setq cacoo:process-semaphore (cc:semaphore-create cacoo:process-num)
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
  "[internal] Return a deferred object to get the resized cache
file from the image workplace. The next deferred object receives
the image path."
  (cc:dataflow-get cacoo:image-wp-resized (cons url size)))

(defun cacoo:image-wp-get-original-d (url)
  "[internal] Return a deferred object to get the cache file from
the image workplace. The next deferred object receives the image
path."
  (cc:dataflow-get cacoo:image-wp-original url))

(defun cacoo:image-wp-set-resized (url size file)
  "[internal] Bind the resized cache file to the cons pair (url . size) 
in the image workplace. The waiting tasks will receive the notification of binding."
  (cc:dataflow-set cacoo:image-wp-resized
                   (cons (substring-no-properties url) size) file))
  
(defun cacoo:image-wp-set-original (url file)
  "[internal] Bind the cache file to the URL in the image
workplace. The waiting tasks will receive the notification of
binding."
  (cc:dataflow-set cacoo:image-wp-original 
                   (substring-no-properties url) file))

(defun cacoo:image-wp-clear-cache (url &optional size)
  "[internal] Clear the information related to URL from the image
workplaces and delete the corresponding cache files."
  ;; remove the cache file for the original image.
  (cacoo:aif (cc:dataflow-get-sync cacoo:image-wp-original url)
      (progn
        (if (and (stringp it) (file-exists-p it))
            (ignore-errors (delete-file it)))
        (cc:dataflow-clear cacoo:image-wp-original url))
    (let ((file (cacoo:get-cache-path-from-url url)))
      (when (cacoo:file-exists-p file)
        (ignore-errors (delete-file file)))))
  ;; remove the cache file for the re-sized image.
  (let ((remove-keys 
         (loop for i in (cc:dataflow-get-avalable-pairs 
                         cacoo:image-wp-resized)
               for key = (car i)
               for key-url = (car key)
               if (equal url key-url)
               collect key)))
    (loop for i in remove-keys
          do
          (cacoo:aif (cc:dataflow-get-sync cacoo:image-wp-resized i)
              (progn
                (if (and (stringp it) (file-exists-p it))
                    (ignore-errors (delete-file it)))
                (cc:dataflow-clear cacoo:image-wp-resized i)))))
  (when size
    (let ((file (cacoo:get-resize-path-from-url url size)))
      (when (cacoo:file-exists-p file)
        (ignore-errors (delete-file file))))))

;;; Event handling

(defun cacoo:image-wp-create-resized (args)
  "[internal] Event handler for `get-first' of
`cacoo:image-wp-resized'. This handler resizes the image and
registers it to the image workplace `cacoo:image-wp-resized'."
  (destructuring-bind (event ((url . size))) args
    (cacoo:resize-diagram url size)))

(defun cacoo:image-wp-create-original (args)
  "[internal] Event handler for `get-first' of
`cacoo:image-wp-original'. This handler gets the image and
registers it to the image workplace `cacoo:image-wp-original'."
  (destructuring-bind (event (url)) args
    (cacoo:fix-directory)
    (cond
     ((cacoo:plugin-url-p url) ; plugins
      (cacoo:load-diagram-plugin url))
     ((string-match "^file:\\/\\/\\/" url) ; local
      (cacoo:load-diagram-local url))
     ((string-match "^https?:\\/\\/" url) ; web
      (cacoo:load-diagram-remote url))
     (t ; relative path
      (cacoo:load-diagram-local url)))))

;;; Create Original Image Cache

(defun cacoo:load-diagram-plugin(url)
  "[internal] Make a resource by the plugin for URL and register it to the image workplace.
If something is wrong, the error message is registered."
  (lexical-let ((url url) (cache-path (cacoo:get-cache-path-from-url url)))
    (cond
     ((cacoo:file-exists-p cache-path)
      (cacoo:log ">>   found cache file : %s" cache-path)
      (cacoo:image-wp-set-original url cache-path))
     ((cacoo:plugin-creator-get url)
      (deferred:try
        (deferred:$
          (funcall (cacoo:plugin-creator-get url))
          (deferred:nextc it
            (lambda (x)
              (if (cacoo:file-exists-p cache-path)
                  (cacoo:image-wp-set-original url cache-path)
                (error "Can not get the image file.")))))
        :catch
        (lambda (e) (cacoo:image-wp-set-original url (cons 'error e)))))
     (t
      (cacoo:image-wp-set-original 
       url (cons 'error (format "Can not found plugin creator. (BUG) %S" url)))))))

(defun cacoo:load-diagram-remote(url)
  "[internal] Retrieve a resource of URL and register it to the image workplace.
If something is wrong, the error message is registered."
  (cacoo:log ">> cacoo:load-diagram-remote : %s" url)
  (lexical-let* ((cache-path (cacoo:get-cache-path-from-url url))
                 (url url))
    (cond
     ((cacoo:file-exists-p cache-path)
      (cacoo:log ">>   found cache file : %s" cache-path)
      (cacoo:image-wp-set-original url cache-path))
     (t
      (cacoo:log ">>   http request : %s" url)
      (cc:semaphore-with cacoo:process-semaphore
        (lambda (x) 
          (deferred:$
            (cacoo:http-get-d url cache-path)
            (deferred:nextc it
              (lambda (x)
                (cacoo:image-wp-set-original 
                 url (if (cacoo:file-exists-p cache-path) cache-path
                       (cons 'error "File not found")))))))
        (lambda (e) 
          (cacoo:image-wp-set-original url (cons 'error e))))))))

(defun cacoo:load-diagram-local(url)
  "[internal] Retrieve a resource of URL and register it to the image workplace.
If something is wrong, the error message is registered."
  (cacoo:log ">> cacoo:load-diagram-local : %s" url)
  (lexical-let
      ((url url)
       (from-path (cacoo:get-local-path-from-url url))
       (cache-path (cacoo:get-cache-path-from-url url)))
    (cond
     ((cacoo:file-exists-p cache-path)
      (cacoo:log ">>   found cache file : %s" cache-path)
      (cacoo:image-wp-set-original url cache-path))
     (t
      (cc:semaphore-with cacoo:process-semaphore
        (lambda (x) 
          (deferred:$
            (cacoo:copy-file-d from-path cache-path)
            (deferred:nextc it
              (lambda (x) (cacoo:image-wp-set-original url cache-path)))))
        (lambda (e) 
          (cacoo:image-wp-set-original 
           url (cons 'error (format "Can not copy file %s -> %s" from-path cache-path)))))))))

(defun cacoo:copy-file-d (from-path to-path)
  "[internal] Return a deferred object to copy a file
asynchronously. If something is wrong, the copy task throws an
error that should be caught by some errorback."
  (cacoo:log ">>   local copy : %s" from-path)
  (lexical-let ((from-path from-path) (to-path to-path))
    (deferred:$
      (if cacoo:copy-by-command
          (deferred:process cacoo:cmd-copy from-path to-path)
        (deferred:next
          (lambda (x) (ignore-errors (copy-file from-path to-path t t)))))
      (deferred:nextc it
        (lambda (x) 
          (unless (cacoo:file-exists-p to-path)
            (error "Can not copy the file : %s -> %s" from-path to-path)))))))

;;; Create Resized Image Cache

(defun cacoo:identify-diagram-d (filename)
  "[internal] Return a deferred object to get image size. The next deferred object receives pixel size along the long axis."
  (lexical-let ((filename filename))
    (deferred:$
      (deferred:process "identify" "-format" "%w %h" filename)
      (deferred:nextc it
        (lambda (line)
          (cacoo:log ">>   identify : %s" line)
          (let* ((cols (split-string line " "))
                 (width (string-to-number (car cols)))
                 (height (string-to-number (cadr cols))))
            (cacoo:log "SIZE %S > %s" line (max width height))
            (max width height)))))))

(defun cacoo:resize-diagram (url max-size)
  "[internal] Resize the cache file for URL with MAX-SIZE and
register the resized file to the image workplace. This function
gets the original cache file from the image workplace
`cacoo:image-wp-original'. The actual resizing task is executed by
the function `cacoo:resize-diagram-convert'."
  (cacoo:log ">> cacoo:resize-diagram : %s / %s" url max-size)
  (lexical-let ((url url) (max-size max-size))
    (deferred:$
      (cacoo:image-wp-get-original-d url)
      (deferred:nextc it
        (lambda (x) 
          (cacoo:log ">>   get : %S" x)
          (cond
           ((and x (consp x) (eq 'error (car x)))
            (cacoo:image-wp-set-resized url max-size (cons 'error (cdr x))))
           (t
            (cacoo:resize-diagram-convert url max-size x))))))))

(defun cacoo:resize-diagram-convert (url max-size filename)
  "[internal] Resize the cache file for URL with MAX-SIZE and
register the resized file to the image workplace. This function
is called by the function `cacoo:resize-diagram'."
  (cacoo:log ">> cacoo:resize-diagram-convert : %s / %s" url max-size)
  (lexical-let ((url url) (max-size max-size) (filename filename)
                (resize-path (cacoo:get-resize-path-from-url url max-size)))
    (cond
     ((cacoo:file-exists-p resize-path)
      (cacoo:log ">>   found cache file : %s" resize-path)
      (cacoo:image-wp-set-resized url max-size resize-path))
     (t
      (cc:semaphore-with cacoo:process-semaphore
        (lambda (x) 
          (deferred:$
            (cacoo:identify-diagram-d filename)
            (deferred:nextc it
              (lambda (org-size)
                (let ((not-resizep (< org-size max-size)))
                  (cond
                   ((= 0 org-size)
                    (cacoo:image-wp-set-resized 
                     url max-size (cons 'error (format "Can not copy file %s" url))))
                   (cacoo:png-background
                    (cacoo:resize-diagram-for-fillbg url max-size not-resizep))
                   (t
                    (cacoo:resize-diagram-for-transparent url max-size not-resizep)))
                  nil)))))
        (lambda (e) 
          (cacoo:image-wp-set-resized url max-size (cons 'error e))))))))

(defun cacoo:resize-diagram-for-transparent (url max-size not-resizep)
  "[internal] Resize the cache file for URL by the later version of command `convert'."
  (cacoo:log ">> cacoo:resize-diagram-for-transparent : %s / %s / not-resize: %s" 
             url max-size not-resizep)
  (lexical-let
      ((url url) (max-size max-size)
       (cache-path (cacoo:get-cache-path-from-url url))
       (resize-path (cacoo:get-resize-path-from-url url max-size)))
    (deferred:try
      (deferred:$
        (if (and not-resizep
                 (equal (file-name-extension cache-path)
                        (file-name-extension resize-path)))
            (cacoo:copy-file-d cache-path resize-path)
          (deferred:process
            "convert" "-resize" (format "%ix%i" max-size max-size)
            "-transparent-color" "#ffffff"
            cache-path (concat (file-name-extension resize-path)
                               ":" resize-path)))
        (deferred:nextc it
          (lambda (msg)
            (if (cacoo:file-exists-p resize-path)
                (cacoo:image-wp-set-resized url max-size resize-path)))))
      :catch
      (lambda (msg) 
        (cacoo:image-wp-set-resized 
         url max-size (cons 'error (format "Can not resize image %s -> %s" 
                                           cache-path resize-path)))))))

(defun cacoo:get-background-img-path (path)
  (expand-file-name
   (format "back_%s" (file-name-nondirectory path))
   (file-name-directory path)))

(defun cacoo:resize-diagram-for-fillbg (url max-size not-resizep)
  (cacoo:log ">> cacoo:resize-diagram-for-fillbg : %s / %s / resize: %s" url max-size not-resizep)
  (lexical-let*
      ((url url) (max-size max-size)
       (cache-path (cacoo:get-cache-path-from-url url))
       (resize-path (cacoo:get-resize-path-from-url url max-size))
       resize-size 
       (tmpfile (cacoo:get-background-img-path cache-path)))
    (deferred:$
      (if (and not-resizep ; small
             (equal (file-name-extension cache-path)
                    (file-name-extension resize-path)))
          (cacoo:copy-file-d cache-path resize-path)
        (deferred:process
          "convert" cache-path "-resize" (format "%ix%i" max-size max-size)
          (concat (file-name-extension resize-path) ":" resize-path)))
      (deferred:processc it ; get the resized image size
        "identify" "-format" "%wx%h" resize-path)
      (deferred:nextc it ; make a background image
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
                                cache-path resize-path))))))))



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
    (cacoo:counter-add)
    (deferred:$
      (cacoo:image-wp-get-resized-d (cacoo:$img-url data) (cacoo:$img-size data))
      (deferred:nextc it
        (lambda (x) 
          (cacoo:log ">>   get(display) : %S" x)
          (if (and x (consp x) (eq 'error (car x))) (error (cdr x))
            (cacoo:display-diagram-by-image x data))))
      (deferred:error it
        (lambda (e) (cacoo:display-diagram-by-text start end e)))
      (deferred:watch it
        (lambda (x) (cacoo:counter-burn))))))

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
          (cacoo:$img-url data))
         pos-end)
        ((string-match "^https?:\\/\\/" url)
         (cacoo:edit-next-diagram-command)
         pos-end)
        (t ; assuming a relative path.
         (cacoo:view-original-cached-image
          (cacoo:$img-url data))
         pos-end))))))

(defun cacoo:display-diagram-by-text (start end msg)
  (cacoo:log ">> cacoo:display-diagram-by-text : %s" msg)
  (let ((mod (buffer-modified-p)))
    (put-text-property start end
     'help-echo (format "Cacoo: Error  %s" msg))
    (cacoo:display-diagram-overlay-add start end)
    (set-buffer-modified-p mod)))

;;; Error overlays

(defvar cacoo:display-diagram-overlays nil "[internal] a list of the overlay objects.")
(make-variable-buffer-local 'cacoo:display-diagram-overlays)

(defun cacoo:display-diagram-overlay-add (start end)
  (when
      (loop for i in cacoo:display-diagram-overlays
            if (and ; checking current overlays
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
  "[internal] Return the point of beginning of the image
markup. If BACKWARD is non-nil, this function searches backward.
If the value `cacoo:img-regexp' is a string, this function just
uses it as a regexp.  If the value is a list of strings, this
function tries all patterns and chooses the nearest point."
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
  "[internal] If a plugin handles the region, return an image
object of structure `cacoo:$img'. Otherwise return nil."
  (loop for i in cacoo:plugins
        for (data . creator) = (funcall i start end content)
        if data
        return (progn
                 (cacoo:plugin-creator-add (cacoo:$img-url data) creator)
                 data)
        finally return nil))

(defvar cacoo:counter (cons 0 0)
  "[internal] The progress counter variable. (unsolved . solved)")

(defun cacoo:counter-reset ()
  "Rest the progress counter `cacoo:counter'."
  (setq cacoo:counter (cons 0 0))
  (cacoo:counter-report))

(defun cacoo:counter-add ()
  "Increase the unsolved number of progress counter."
  (incf (car cacoo:counter))
  (cacoo:counter-report))

(defun cacoo:counter-burn ()
  "Increase the solved number of progress counter."
  (incf (cdr cacoo:counter))
  (cacoo:counter-report))

(defun cacoo:counter-report ()
  "Display the progress counter."
  (message "Cacoo: Progress  %s/%s  %s"
           (car cacoo:counter) (cdr cacoo:counter)
           (if (and
                (< 0 (car cacoo:counter))
                (equal (car cacoo:counter) (cdr cacoo:counter)))
               "[complete]" "...")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High level API 

(defun cacoo:image-search-forward ()
  (cacoo:image-search))

(defun cacoo:image-search-backward ()
  (cacoo:image-search t))

(defun cacoo:do-next-diagram (action)
  (cond
   ((cacoo:image-search-forward)
      (let* 
          ((start (match-beginning 0)) (end (match-end 0))
           (content (substring-no-properties (match-string 1))) data)
        (setq data 
              (cacoo:aif (cacoo:try-plugins start end content) it 
                (let* ((cols (split-string content "[ \t]+"))
                       (url (car cols))
                       (size (cacoo:aif (cadr cols)
                                 (string-to-number it) 
                               cacoo:max-size)))
                  (make-cacoo:$img :url url :start start :end end :size size))))
        (funcall action data)
        (goto-char end)))
   (t nil)))

(defun cacoo:load-next-diagram ()
  (cacoo:do-next-diagram 'cacoo:display-diagram))

(defun cacoo:revert-next-diagram ()
  (cacoo:do-next-diagram 'cacoo:display-diagram-revert))

(defun cacoo:reload-next-diagram ()
  (cacoo:do-next-diagram 
   (lambda (data) 
     (cacoo:image-wp-clear-cache (cacoo:$img-url data) (cacoo:$img-size data))
     (cacoo:display-diagram data))))

(defun cacoo:clear-cache-next-diagram ()
  (save-excursion
    (cacoo:do-next-diagram 
     (lambda (data) 
       (cacoo:image-wp-clear-cache (cacoo:$img-url data) (cacoo:$img-size data))))))

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
  "View the original image with the local application."
  (interactive)
  (cacoo:do-next-diagram
   (lambda (data)
     (cacoo:view-original-cached-image (cacoo:$img-url data)))))

(defun cacoo:clear-all-cache-files-command ()
  "Remove all files in the cache directory."
  (interactive)
  (when (yes-or-no-p "Delete all local cache files?")
    (cacoo:clear-all-cache-files)
    (cacoo:plugin-creator-clear)
    (message "Delete all local cache files.")))

(defun cacoo:reload-next-diagram-command ()
  "Reload the next diagram."
  (interactive)
  (cacoo:counter-reset)
  (save-excursion
    (cacoo:reload-next-diagram)))

(defun cacoo:reload-or-revert-current-diagram-command ()
  "Toggle the display style between image and text on the current position."
  (interactive)
  (save-excursion
    (cond
     ((get-text-property (point) 'display)
      (end-of-line)
      (cacoo:navi-prev-diagram-command)
      (cacoo:revert-next-diagram))
     (t
      (end-of-line)
      (cacoo:navi-prev-diagram-command)
      (cacoo:counter-reset)
      (cacoo:reload-next-diagram)))))

(defun cacoo:reload-all-diagrams-command ()
  "Reload all images in the current buffer."
  (interactive)
  (cacoo:plugin-creator-clear)
  (cacoo:counter-reset)
  (save-excursion
    (goto-char (point-min))
    (while (cacoo:image-search-forward)
      (cacoo:clear-cache-next-diagram))
    (goto-char (point-min))
    (cacoo:counter-reset)
    (while (cacoo:load-next-diagram))))

(defun cacoo:revert-next-diagram-command ()
  "Hide the image and display the original markup text."
  (interactive)
  (save-excursion
    (cacoo:revert-next-diagram)))

(defun cacoo:revert-all-diagrams-command ()
  "Hiding all images."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (cacoo:revert-next-diagram))))

(defun cacoo:display-all-diagrams-command ()
  "Display all images."
  (interactive)
  (cacoo:counter-reset)
  (cacoo:plugin-creator-clear) 
  (save-excursion
    (goto-char (point-min))
    (while (cacoo:load-next-diagram))))

(defun cacoo:display-next-diagram-command ()
  "Display the next image."
  (interactive)
  (cacoo:counter-reset)
  (save-excursion
    (cacoo:load-next-diagram)))

(defun cacoo:create-new-diagram-command ()
  "Open the cacoo editor for a new diagram with the web browser."
  (interactive)
  (cacoo:open-browser cacoo:new-url))

(defun cacoo:open-diagram-list-command ()
  "Open the list of cacoo diagrams with the web browser."
  (interactive)
  (cacoo:open-browser cacoo:list-url))

(defun cacoo:edit-next-diagram-command ()
  "Open the cacoo editor for the next diagram with the web browser."
  (interactive)
  (cacoo:open-diagram-gen cacoo:edit-url))

(defun cacoo:open-diagram-gen (tmpl-url)
  "Open the url with the web browser. TMPL-URL is an URL template."
  (save-excursion
    (cacoo:do-next-diagram
     (lambda (data) 
       (let* ((url (cacoo:$img-url data))
              (key (cacoo:get-key-from-url url))
              (open-url (cacoo:make-url tmpl-url key)))
         (cacoo:open-browser (or open-url url)))))))

(defun cacoo:view-next-diagram-command ()
  "Open the detail page for the next diagram with the web browser."
  (interactive)
  (cacoo:open-diagram-gen cacoo:view-url))

(defun cacoo:navi-next-diagram-command ()
  "Move to the next diagram."
  (interactive)
  (end-of-line)
  (if (cacoo:image-search-forward)
      (beginning-of-line)))

(defun cacoo:navi-prev-diagram-command ()
  "Move to the previous diagram."
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

         ("C-c , I"   . cacoo:anything-command)
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
  (cacoo:plugin-creator-clear)
  (cacoo:display-all-diagrams-command)
  (cacoo:api-diagrams-init-cache))

(defun cacoo:minor-mode-abort ()
  (cacoo:display-diagram-overlay-clear)
  (cacoo:plugin-creator-clear)
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
    ["Select by Anything" cacoo:anything-command t]
    "----"
    ["Create new diagram" cacoo:create-new-diagram-command t]
    ["List diagrams" cacoo:open-diagram-list-command t]
    "----"
    ["Clear all cache files" cacoo:clear-all-cache-files-command t]))

(easy-menu-define cacoo-menu-map
  cacoo-minor-mode-keymap "Cacoo menu map" 
  cacoo:minor-mode-menu-spec)
(easy-menu-add cacoo-menu-map cacoo-minor-mode-keymap)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; plugin actions in the image workplace

(defvar cacoo:plugin-creator-alist nil
  "[internal] Plugin creator alist. (url -> creator object)")
(make-variable-buffer-local 'cacoo:plugin-creator-alist)

(defun cacoo:plugin-creator-add (url creator-d)
  "[internal] Add an entry of plugin to the creator alist
`cacoo:plugin-creator-alist'. If an entry has already existed,
this function replaces the creator object."
  (cond
   ((assoc url cacoo:plugin-creator-alist)
    (setcdr (assoc url cacoo:plugin-creator-alist) creator-d))
   (t
    (push (cons url creator-d) cacoo:plugin-creator-alist)))
  nil)

(defun cacoo:plugin-creator-clear ()
  "[internal] Clear the plugin creator alist `cacoo:plugin-creator-alist'."
  (setq cacoo:plugin-creator-alist nil))

(defun cacoo:plugin-creator-get (url)
  "[internal] Return a creator object corresponding to the
URL. This function is used in the image workplace."
  (cdr (assoc url cacoo:plugin-creator-alist)))

(defun cacoo:plugin-url-p (url)
  "[internal] Return non-nil if URL should be handled by plugin creators.
This function is used in the image workplace."
  (string-match "^plugin:\\/\\/" url))

(defun cacoo:plugin-url-create (plugin-name image-filename)
  "[internal] Return a plugin url. This function is used in the
image workplace."
  (format "plugin://%s/%s/%s" plugin-name 
          (file-name-nondirectory (buffer-file-name)) 
          image-filename))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diagram Cache Controller

(defun cacoo:api-diagrams-local-cache-load ()
  "[internal] Read the diagram data from the local diagram cache file and return it.
If the cache file not found, return nil."
  (let ((file
         (expand-file-name
          cacoo:api-diagrams-cache-file
          cacoo:api-diagrams-cache-dir)))
    (when (file-exists-p file)
      (let ((buf (find-file-noselect file)) ret)
        (unwind-protect
            (setq ret (read buf))
          (kill-buffer buf))
        ret))))

(defun cacoo:api-diagrams-local-cache-save (diagrams)
  "[internal] Save the diagram data into the local diagram cache file."
  (unless (file-exists-p (expand-file-name cacoo:api-diagrams-cache-dir))
    (make-directory (expand-file-name cacoo:api-diagrams-cache-dir) t))
  (let ((buf (find-file-noselect
              (expand-file-name
               cacoo:api-diagrams-cache-file
               cacoo:api-diagrams-cache-dir))))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert (prin1-to-string diagrams))
          (save-buffer))
      (kill-buffer buf))))

;; (cacoo:api-diagrams-local-cache-save cacoo:api-diagrams-cache)
;; (equal cacoo:api-diagrams-cache (cacoo:api-diagrams-local-cache-load))

(defun cacoo:api-diagrams-local-cache-delete ()
  "[internal] Delete local diagram cache file."
  (let ((file (expand-file-name
               cacoo:api-diagrams-cache-file
               cacoo:api-diagrams-cache-dir)))
    (when (file-exists-p file)
      (delete-file file))))

(defvar cacoo:api-diagrams-cache-wp nil "[internal] Data flow variable for the diagram data retrieved by the Cacoo API.")

(defun cacoo:api-diagrams-init-cache ()
  "[internal] Reset the data flow variable `cacoo:api-diagrams-cache-wp' and start retrieving diagram data by Cacoo API."
  (setq cacoo:api-diagrams-cache-wp (cc:dataflow-environment))
  (cacoo:api-retrieve-diagrams))

(defun cacoo:api-default-url (diagram)
  "[internal] Return the public URL of the diagram if the diagram is open. 
Otherwise return the URL for API."
  (if (equal "url" (cacoo:k 'security diagram))
      (cacoo:k 'imageUrl diagram)
    (cacoo:k 'imageUrlForApi diagram)))

(defun cacoo:api-retrieve-sheets-d (diagram-json)
  "[internal] Return a deferred object that retrieves json data
of sheets from Cacoo API and that appends the data to the given
diagram data structure. This function is called by `cacoo:api-check-diagram-cache-d'."
  (lexical-let ((diagram-json diagram-json))
    (deferred:$
      (cacoo:api-get-d (format "diagrams/%s" (cacoo:k 'diagramId diagram-json)))
      (deferred:nextc it
        (lambda (whole-json)
          (let ((sheets-json (cdr (assq 'sheets whole-json))))
            (loop for i in sheets-json
                  do (cacoo:preview-image-cache-expire (cacoo:api-default-url i)))
            (cons (cons 'sheets sheets-json) diagram-json)))))))

(defun cacoo:api-diagrams-get-by-id (id diagrams-json)
  "[internal] Return the detail data of the diagram indicated by ID."
  (loop for i in diagrams-json
        if (equal (cacoo:k 'diagramId i) id)
        return i))

(defun cacoo:api-check-diagram-list-cache-d (new-json cached-json each-func)
  "[internal] Return a deferred object that prepares retrieving sheet data for all diagrams in parallel.
This function is called by `cacoo:api-retrieve-diagrams'."
  (lexical-let ((each-func each-func))
    (deferred:parallel
      (loop for i in new-json
            for cache = (and cached-json 
                             (cacoo:api-diagrams-get-by-id 
                              (cacoo:k 'diagramId i) cached-json))
            collect
            (deferred:nextc
              (cacoo:api-check-diagram-cache-d i cache)
              (lambda (x) (funcall each-func x) x))))))

(defun cacoo:api-check-diagram-cache-d (new-json cached-json)
  "[internal] Return a deferred object that retrieves json data of sheets from Cacoo API.
If cached data is valid comparing with time stamps, this task
does not access Cacoo API. This function is called by
`cacoo:api-check-diagram-list-cache-d'."
  (let ((new-time    (date-to-time (cacoo:k 'updated new-json)))
        (cached-time (and cached-json 
                          (date-to-time 
                           (cacoo:k 'updated cached-json)))))
    (if (or (null cached-time) (time-less-p cached-time new-time))
        (cacoo:api-retrieve-sheets-d new-json)
      (deferred:succeed cached-json))))

(defun cacoo:api-retrieve-diagrams ()
  "[internal] Retrieve the data of sheets and diagrams from Cacoo
API and store the data to the local cache. If `cacoo:api-key' is
nil, this function does nothing.  If it is offline state, this
function uses the local cache that is retrieved previously.  See
the local cache API functions, `cacoo:api-diagrams-local-cache-load' and
`cacoo:api-diagrams-local-cache-save'."
  (lexical-let ((sheet-counter 0) (diagrams-counter 1)
                (last-diagrams-json (cacoo:api-diagrams-local-cache-load))
                diagrams-number)
    (cacoo:api-prepare-cancel)
    (cond
     ((null cacoo:api-key)
      (cc:dataflow-set cacoo:api-diagrams-cache-wp 'diagrams nil))
     (t
      (deferred:$
        (cacoo:api-get-d "diagrams")
        (deferred:nextc it
          (lambda (whole-json)
            (let ((diagrams-json (cacoo:k 'result whole-json)))
              (setq diagrams-number (length diagrams-json))
              (cacoo:api-check-diagram-list-cache-d
               diagrams-json
               last-diagrams-json
               (lambda (x)
                 (incf sheet-counter (cacoo:k 'sheetCount x))
                 (message "Cacoo: Getting diagram and sheet information... %s/%s" 
                          diagrams-counter diagrams-number)
                 (incf diagrams-counter))))))
        (deferred:nextc it
          (lambda (x)
            (cond
             (cacoo:api-cancel-flag
              (cc:dataflow-set cacoo:api-diagrams-cache-wp 'diagrams 
                               last-diagrams-json)
              (message "Cacoo: Canceled."))
             (t
              (cc:dataflow-set cacoo:api-diagrams-cache-wp 'diagrams x)
              (cacoo:api-diagrams-local-cache-save x)
              (message "Cacoo: all sheets [%s] are collected." sheet-counter)))))
        (deferred:error it
          (lambda (err)
            (message "Cacoo: Can not retrieve diagram data by API. -> %s" err)
            (cc:dataflow-set cacoo:api-diagrams-cache-wp 'diagrams 
                             last-diagrams-json))))))))

;;; Canceling asynchronous tasks

(defun cacoo:api-prepare-cancel ()
  (interactive)
  (setq cacoo:api-cancel-flag nil)
  (defadvice keyboard-quit (before cacoo:api-cancel)
    (cacoo:api-cancel)
    (cacoo:api-clear-cancel))
  (ad-activate-regexp "cacoo:api-cancel"))

(defun cacoo:api-clear-cancel ()
  (interactive)
  (cacoo:log "AT: clear-cancel")
  (ignore-errors
    (ad-deactivate-regexp "cacoo:api-cancel")
    (ad-remove-advice 'keyboard-quit 'after 'cacoo:api-cancel)))

(defun cacoo:api-cancel ()
  (interactive)
  (setq cacoo:api-cancel-flag t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image Preview

;;; Cache Control

(defvar cacoo:preview-image-cache nil "[internal] The alist of image cache for preview.")
(defvar cacoo:preview-image-cache-num 10 "[internal] The maximum number of the image cache.")

(defun cacoo:preview-image-cache-get-mru (url)
  (let ((cached-pair (assoc url cacoo:preview-image-cache)))
    (when cached-pair
      (setq cacoo:preview-image-cache
            (cons cached-pair
                  (loop for i in cacoo:preview-image-cache
                        for iurl = (car i)
                        with count = 1
                        unless (or (equal url iurl) 
                                   (<= cacoo:preview-image-cache-num count))
                        collect (progn (incf count) i)))))
    (cdr cached-pair)))

(defun cacoo:preview-image-cache-add (url image)
  (push (cons url image) cacoo:preview-image-cache) image)

(defun cacoo:preview-image-cache-expire (url)
  (setq cacoo:preview-image-cache
        (loop for i in cacoo:preview-image-cache
              for iurl = (car i)
              unless (equal url iurl) 
              collect i)))

;;; Preview Buffer

(defvar cacoo:anything-channel nil "[internal] The asynchronous communication channel for the anything buffer and the preview one.")
(defconst cacoo:preview-buffer " *cacoo:preview*")

(defun cacoo:preview-buffer-init (title)
  (let ((buf (get-buffer cacoo:preview-buffer)))
    (unless buf
      (setq buf (get-buffer-create cacoo:preview-buffer))
      (with-current-buffer buf
        (buffer-disable-undo buf)
        (set (make-local-variable 'preview-title) "")
        (set (make-local-variable 'preview-progress) "")
        (set (make-local-variable 'preview-count) 0))
      (cc:signal-disconnect-all cacoo:anything-channel)
      (loop for i in '((show-image . cacoo:preview-buffer-on-show-image)
                       (progress . cacoo:preview-buffer-on-show-progress)
                       (animation . cacoo:preview-buffer-on-show-animation)
                       (image-load-start . cacoo:preview-buffer-start-animation)
                       (image-load-finish . cacoo:preview-buffer-stop-animation))
            for ev = (car i)
            for f = (cdr i)
            do (cc:signal-connect cacoo:anything-channel ev f)))

    (cc:signal-send cacoo:anything-channel 'show-image title nil nil)
    buf))

(defun cacoo:preview-buffer-on-show-image (args)
  (with-current-buffer (get-buffer cacoo:preview-buffer)
    (destructuring-bind (event (title url img)) args
      (setq preview-title title
            preview-count 0
            preview-progress "")
      (cacoo:preview-buffer-update-mode-line)
      (erase-buffer)
      (cond 
       (url
        (insert (propertize " " 'display `(space :align-to (+ center (-0.5 . ,img)))))
        (insert-image img)
        (let ((win (get-buffer-window cacoo:preview-buffer)))
          (when win (set-window-point win (1+ (point-min))))))
       (t
        (insert "No image..."))))))

(defconst cacoo:preview-mode-line-format "%s %5s %s") ; animation, progress, title

(defun cacoo:preview-buffer-update-mode-line ()
  (let ((anm "-/|\\"))
    (setq mode-line-format 
          (format cacoo:preview-mode-line-format
                  (char-to-string 
                   (aref anm (% preview-count (length anm))))
                  preview-progress preview-title)))
    (force-mode-line-update))

(defun cacoo:preview-buffer-on-show-progress (args)
  (with-current-buffer (get-buffer cacoo:preview-buffer)
    (destructuring-bind (event (progress)) args
      (setq preview-progress progress)
      (cacoo:preview-buffer-update-mode-line))))

(defun cacoo:preview-buffer-on-show-animation (buf)
  (with-current-buffer (get-buffer cacoo:preview-buffer)
    (incf preview-count)
    (cacoo:preview-buffer-update-mode-line)))


(defvar cacoo:preview-buffer-thread nil "[internal] Thread object for busy animation on the mode-line.")

(defun cacoo:preview-buffer-stop-animation ()
  (setq cacoo:preview-buffer-thread nil))

(defun cacoo:preview-buffer-start-animation ()
  (unless cacoo:preview-buffer-thread
    (setq cacoo:preview-buffer-thread t)
    (cc:thread 
     60 
     (while cacoo:preview-buffer-thread
       (cc:signal-send cacoo:anything-channel 'animation)))))


(defun cacoo:preview-progress (d current total)
  (lexical-let
      ((progress (apply 'concat
             (loop for i from 1 to total
                   collect (if (<= i current) "O" ".")))))
    (deferred:watch (or d (deferred:succeed))
      (lambda (x)
        (cc:signal-send cacoo:anything-channel 'progress progress)))))

(defun cacoo:preview-image-get-d (url)
  (cacoo:log ">> cacoo:preview-image-get-d : %s" url)
  (let ((image (cacoo:preview-image-cache-get-mru url)))
    (cond
     (image (deferred:succeed image))
     (t
      (lexical-let
          ((url url) 
           (org-file (expand-file-name "_preview_org.png" temporary-file-directory))
           (resized-file (expand-file-name "_preview_resized.png" temporary-file-directory))
           (win (cacoo:preview-get-preview-window)))
        (deferred:try
          (deferred:$
            (cc:semaphore-interrupt-all cacoo:preview-semaphore)
            (cacoo:preview-progress it 1 4)
            (deferred:nextc it
              (lambda (x)
                (cc:signal-send cacoo:anything-channel 'image-load-start)
                (cacoo:log ">>   http request : %s" url)
                (cacoo:http-get-d url org-file)))
            (cacoo:preview-progress it 3 4)
            (deferred:nextc it
              (lambda (x)
                (if (cacoo:file-exists-p org-file)
                    (deferred:process "identify" "-format" "%w %h" org-file)
                  (error "File not found"))))
            (deferred:nextc it
              (lambda (sizestr)
                (cacoo:log ">>   identify : %s" sizestr)
                (let* ((ww (* (window-width win) (frame-char-width)))
                       (wh (* (- (window-height win) 2) (frame-char-height)))
                       (isize (mapcar 'string-to-number (split-string sizestr))))
                  (if (or (< ww (car isize)) (< wh (cadr isize)))
                      (progn 
                        (cacoo:preview-progress nil 4 4)
                        (deferred:$
                          (deferred:process
                            "convert" "-resize" (format "%ix%i" ww wh)
                            org-file (concat (file-name-extension resized-file) ":" resized-file))
                          (deferred:nextc it (lambda (x) resized-file))))
                    org-file))))
            (deferred:nextc it
              (lambda (ifile)
                (clear-image-cache)
                (let ((img (create-image (cacoo:preview-load-image-data ifile) 'png t)))
                  (cacoo:preview-image-cache-add url img)
                  img))))
          :catch
          (lambda (e) (cacoo:log "Preview Error : %s" e) nil)
          :finally
          (lambda (x)
            (cc:semaphore-release cacoo:preview-semaphore)
            (cc:signal-send cacoo:anything-channel 'image-load-finish)
            (when (file-exists-p org-file)
              (ignore-errors (delete-file org-file)))
            (when (file-exists-p resized-file)
              (ignore-errors (delete-file resized-file))))))))))

(defun cacoo:preview-load-image-data (file)
  (let ((buf (find-file-noselect file t t)))
    (prog1 (with-current-buffer buf (buffer-string))
      (kill-buffer buf))))

(defvar cacoo:preview-window nil "[internal] Preview window object.")

(defun cacoo:preview-init-preview-window ()
  (let ((win (anything-window)))
    (unless (and cacoo:preview-window
                 (window-live-p cacoo:preview-window))
      (setq cacoo:preview-window 
            (cond
             ((< (window-width win) (* 2 (window-height win)))
              (split-window win))
             (t
              (split-window win (/ (window-width win) 2) t))))
      (set-window-buffer 
       cacoo:preview-window 
       (cacoo:preview-buffer-init "No Image...")))
    cacoo:preview-window))

(defun cacoo:preview-get-preview-window ()
  (and (window-live-p cacoo:preview-window)
       cacoo:preview-window))

(defvar cacoo:preview-semaphore (cc:semaphore-create 1) "[internal] Semaphore object for preview tasks.")

(defun cacoo:preview (title url)
  (cacoo:log "AT preview %s" url)
  (lexical-let ((url url) (title title))
    (deferred:$
      (cacoo:preview-image-get-d url)
      (deferred:nextc it
        (lambda (img)
          (when img
            (cc:signal-send cacoo:anything-channel 'show-image title url img))))
      (deferred:error it
        (lambda (e) (message "Preview Error : %s" e))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cacoo-Anything Application Functions

(defun cacoo:anything-format (diagram sheet)
  (cons 
   (format "[%s] %s : %s   %s"
           (if (equal "url" (cacoo:k 'security diagram)) "+" "-")
           (cacoo:k 'title diagram)
           (cacoo:k 'name sheet)
           (cacoo:k 'ownerNickname diagram))
   (cons diagram sheet)))

(defun cacoo:anything-collect-diagrams ()
  (let (lines)
    (loop for d in (cc:dataflow-get-sync cacoo:api-diagrams-cache-wp 'diagrams)
          do
          (loop for s in (cdr (assq 'sheets d))
                do
                (push (cacoo:anything-format d s) lines)))
    (nreverse lines)))

(defvar cacoo:preview-action-last-data nil)
(defun cacoo:preview-action (data)
  (unless cacoo:preview-window
    (cacoo:preview-init-preview-window))
  (unless (eq cacoo:preview-action-last-data data)
    (setq cacoo:preview-action-last-data data)
    (let ((diagram (car data)) (sheet (cdr data)))
      (cacoo:preview 
       (car (cacoo:anything-format diagram sheet))
       (cacoo:k 'imageUrlForApi sheet)))))

(defun cacoo:anything-insert-and-display (url)
  (save-excursion
    (cacoo:insert-pattern-url url))
  (cacoo:display-next-diagram-command))

(defvar anything-c-source-cacoo 
  '((name . "Image source")
    (candidates . cacoo:anything-collect-diagrams)
    (action 
     ("Insert URL" 
      . (lambda (x) 
          (cacoo:anything-insert-and-display 
           (cacoo:api-default-url (cdr x)))))
     ("Insert API URL" 
      . (lambda (x) (cacoo:anything-insert-and-display (cacoo:k 'imageUrlForApi (cdr x)))))
     ("Add URL to kill-ring" 
      . (lambda (x) (kill-new (cacoo:k 'imageUrl (cdr x)))))
     ("Show Detail (Browser)" 
      . (lambda (x) (cacoo:open-browser (cacoo:k 'url (cdr x)))))
     ("Edit Diagram (Browser)" 
      . (lambda (x) (cacoo:open-browser 
                     (cacoo:make-url 
                      cacoo:edit-url 
                      (cacoo:k 'diagramId (car x)))))))
    (candidate-number-limit . 200)
    (migemo)
    (persistent-action . cacoo:preview-action)))

(defadvice anything-move-selection-common (after cacoo:anything)
  (when (eq (anything-buffer-get) anything-buffer)
    (anything-execute-persistent-action)))
(ad-deactivate-regexp "cacoo:anything")

;;; Startup and Cleanup

(defvar cacoo:anything-lock nil "[internal] This variable is
employed for the exclusive start-up of the Anything command.  If
the Anything start-up is required, this variable is `t'. See the
functions, `cacoo:anything-command', `cacoo:anything-startup' and
`cacoo:anything-cleanup'.")

(defun cacoo:anything-startup ()
  (cacoo:log "AT: startup")
  (setq cacoo:anything-lock t
        cacoo:preview-window nil
        cacoo:anything-channel (cc:signal-channel 'cacoo:anything)))

(defun cacoo:anything-cleanup ()
  (let ((buf (get-buffer cacoo:preview-buffer)))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf)))
  (setq cacoo:anything-lock nil)
  (cc:signal-disconnect-all cacoo:anything-channel)
  (setq cacoo:anything-channel nil)
  (cacoo:log "AT: cleanup"))

(defun cacoo:anything-cache-clear ()
  "Clear all cache data around the Cacoo API. After clearing, the
asynchronous task is started to retrieving the latest data of
sheets and diagrams."
  (interactive)
  (cc:dataflow-clear cacoo:api-diagrams-cache-wp 'diagrams)
  (cacoo:api-diagrams-local-cache-delete)
  (cacoo:api-diagrams-init-cache)
  (setq cacoo:preview-image-cache nil))

;;; Anything command

(defun cacoo:anything-command ()
  "Start up the anything command for diagrams of Cacoo."
  (interactive)
  (cond
   ((null cacoo:api-key)
    (message "Get your Cacoo API key and set it `cacoo:api-key'."))
   (cacoo:anything-lock
    (cacoo:log "cacoo:anything locked."))
   (t
    (unless (cc:dataflow-get-sync cacoo:api-diagrams-cache-wp 'diagrams)
      (message "Now retrieving diagram data. Wait a moment..."))
    (cacoo:anything-startup)
    (deferred:$
      (cc:dataflow-get cacoo:api-diagrams-cache-wp 'diagrams)
      (deferred:nextc it
        (lambda (x) 
          (cond
           ((eq 'error x)
            (error "Can not retrieve diagram data. Check your network status and settings."))
           (t x))))
      (deferred:nextc it
        (lambda (x) 
          (ad-activate-regexp "cacoo:anything")
          (cacoo:api-clear-cancel)
          (unless cacoo:api-cancel-flag
            (anything anything-c-source-cacoo))))
      (deferred:error it
        (lambda (e) (message "Error : %s" e)))
      (deferred:watch it
        (lambda (x)
          (ad-deactivate-regexp "cacoo:anything")
          (cacoo:anything-cleanup)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for test

;; (setq cacoo:process-num 1)
;; (setq cacoo:process-num 4)

;; (setq cacoo:debug t)
;; (setq cacoo:debug nil)

;; (progn (toggle-debug-on-error 1) (setq deferred:debug-on-signal t))
;; (progn (toggle-debug-on-error -1) (setq deferred:debug-on-signal nil))

;; (setq cacoo:plugins nil)
;; (eval-current-buffer)

;; (cacoo:debug-report-semaphore)
;; (cacoo:anything-command)
;; (cacoo:anything-cache-clear)

(provide 'cacoo)
;;; cacoo.el ends here
