;;; anything-cacoo.el

(require 'cacoo)
(require 'json)

(defvar cacoo:api-url-base "https://cacoo.com/api/v1/" "[internal] Cacoo API base URL.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility Functions

(defun cacoo:k (key alist)
  (or (cdr (assq key alist)) ""))

(defun cacoo:api-report-semaphore ()
  (interactive)
  (message "Semaphore: get permits : %s / waiting : %s   preview permit : %s / waiting : %s"
           (cc:semaphore-permits cacoo:get-semaphore)
           (length (cc:semaphore-waiting-deferreds 
                    cacoo:get-semaphore))
           (cc:semaphore-permits cacoo:preview-semaphore)
           (length (cc:semaphore-waiting-deferreds 
                    cacoo:preview-semaphore))))

(defmacro cacoo:api-debug-deferred (d msg &rest args)
  `(deferred:nextc ,d
     (lambda (x) (funcall 'message ,msg ,@args) x)))

(defun cacoo:api-debug-dbuffer (d)
  (deferred:nextc d
    (lambda (x)
      (pop-to-buffer
       (with-current-buffer (get-buffer-create "*json*")
         (erase-buffer)
         (insert (pp-to-string x))
         (current-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cacoo API Functions

(defun cacoo:api-param-serialize (params)
  (cond
   (params
    (mapconcat
     'identity
     (loop for p in params
           collect (format "%s=%s" (car p) (cdr p)))
     "&"))
   (t "")))

(defvar cacoo:api-cancel-flag nil "[internal]")
(defvar cacoo:get-semaphore (cc:semaphore-create cacoo:process-num) "[internal]")

(defun cacoo:api-get (method &optional params)
  (lexical-let
      ((url (concat cacoo:api-url-base method ".json"
                    "?" (cacoo:api-param-serialize
                         (cons (cons 'apiKey cacoo:api-key)
                               params)))))
    (deferred:$
      (cc:semaphore-aquire cacoo:get-semaphore)
      (deferred:nextc it
        (lambda (x)
          (unless cacoo:api-cancel-flag
            (deferred:$
              (deferred:process "wget" "-q" "-O" "-" url)
              (deferred:nextc it
                (lambda (x) 
                  (let ((json-array-type 'list))
                    (json-read-from-string x))))))))
      (deferred:error it
        (lambda (e) (message "API Error: %s" e) nil))
      (deferred:nextc it
        (lambda (x) (cc:semaphore-release cacoo:get-semaphore) x)))))

;(cacoo:api-debug-dbuffer (cacoo:api-get "diagrams"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diagram Cache Controller

(defun cacoo:api-retrieve-sheets (diagram-json)
  (lexical-let ((diagram-json diagram-json))
    (deferred:$
      (cacoo:api-get (format "diagrams/%s" (cacoo:k 'diagramId diagram-json)))
      (deferred:nextc it
        (lambda (whole-json)
          (let ((sheets-json (cdr (assq 'sheets whole-json))))
            (cons (cons 'sheets sheets-json) diagram-json)))))))

(defun cacoo:api-diagrams-get-by-id (id diagrams-json)
  (loop for i in diagrams-json
        if (equal (cacoo:k 'diagramId i) id)
        return i))

(defun cacoo:api-check-diagram-list-cache (new-json cached-json each-func)
  (lexical-let ((each-func each-func))
    (deferred:parallel
      (loop for i in new-json
            for cache = (and cached-json 
                             (cacoo:api-diagrams-get-by-id 
                              (cacoo:k 'diagramId i) cached-json))
            collect
            (deferred:nextc
              (cacoo:api-check-diagram-cache i cache)
              (lambda (x) (funcall each-func x) x))))))

(defun cacoo:api-check-diagram-cache (new-json cached-json)
  (let ((new-time    (date-to-time (cacoo:k 'updated new-json)))
        (cached-time (and cached-json 
                          (date-to-time 
                           (cacoo:k 'updated cached-json)))))
    (if (or (null cached-time) (time-less-p cached-time new-time))
        (cacoo:api-retrieve-sheets new-json)
      (deferred:succeed cached-json))))

(defvar cacoo:api-diagrams-cache nil "[internal]")

(defun cacoo:api-retrieve-diagrams ()
  (lexical-let ((sheet-counter 0) 
                (diagrams-counter 1) 
                diagrams-number)
    (deferred:$
      (cacoo:api-get "diagrams")
      (deferred:nextc it
        (lambda (whole-json)
          (let ((diagrams-json (cacoo:k 'result whole-json)))
            (setq diagrams-number (length diagrams-json))
            (cacoo:api-check-diagram-list-cache
             diagrams-json
             cacoo:api-diagrams-cache
             (lambda (x) 
               (incf sheet-counter (cacoo:k 'sheetCount x))
               (message "Cacoo: Getting diagram and sheet informations... %s/%s" 
                        diagrams-counter diagrams-number)
               (incf diagrams-counter))))))
      (deferred:nextc it
        (lambda (x)
          (cond
           (cacoo:api-cancel-flag
            (message "Cacoo: Cancelled." sheet-counter))
           (t
            (setq cacoo:api-diagrams-cache x)
            (message "Cacoo: all sheets [%s] are collected." sheet-counter)))
          x)))))

; (cacoo:api-debug-dbuffer (cacoo:api-retrieve-diagrams))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image Preview

;;; Cache Control

(defvar cacoo:api-image-cache nil "[internal]")
(defvar cacoo:api-image-cache-num 10 "[internal]")

(defun cacoo:api-image-cache-get-mru (url)
  (let ((cached-pair (assoc url cacoo:api-image-cache)))
    (when cached-pair
      (setq cacoo:api-image-cache
            (cons cached-pair
                  (loop for i in cacoo:api-image-cache
                        for iurl = (car i)
                        with count = 1
                        unless (or (equal url iurl) 
                                   (<= cacoo:api-image-cache-num count))
                        collect (progn (incf count) i)))))
    (cdr cached-pair)))

(defun cacoo:api-image-cache-add (url image)
  (push (cons url image) cacoo:api-image-cache) image)

;;; Preview Buffer

(defvar cacoo:anything-channel nil "[internal]")
(defconst cacoo:anything-preview-buffer " *cacoo:preview*")

(defun cacoo:anything-preview-buffer-init (title)
  (let ((buf (get-buffer cacoo:anything-preview-buffer)))
    (unless buf
      (setq buf (get-buffer-create cacoo:anything-preview-buffer))
      (with-current-buffer buf
        (buffer-disable-undo buf)
        (set (make-local-variable 'preview-title) "")
        (set (make-local-variable 'preview-progress) "")
        (set (make-local-variable 'preview-count) 0))
      (cc:signal-disconnect-all cacoo:anything-channel)
      (loop for i in '((show-image . cacoo:anything-preview-buffer-on-show-image)
                       (progress . cacoo:anything-preview-buffer-on-show-progress)
                       (animation . cacoo:anything-preview-buffer-on-show-animation)
                       (image-load-start . cacoo:anything-preview-buffer-start-animation)
                       (image-load-finish . cacoo:anything-preview-buffer-stop-animation))
            for ev = (car i)
            for f = (cdr i)
            do (cc:signal-connect cacoo:anything-channel ev f)))

    (cc:signal-send cacoo:anything-channel 'show-image title nil nil)
    buf))

(defun cacoo:anything-preview-buffer-on-show-image (args)
  (with-current-buffer (get-buffer cacoo:anything-preview-buffer)
    (destructuring-bind (event (title url img)) args
      (setq preview-title title
            preview-count 0
            preview-progress "")
      (cacoo:anything-preview-buffer-update-mode-line)
      (erase-buffer)
      (cond 
       (url
        (insert " ")
        (add-text-properties (point-min) (point-max) (list 'display img))
        (goto-char (point-min)))
       (t
        (insert "No image..."))))))

(defconst cacoo:anything-preview-mode-line-format "%s %5s %s") ; animation, progress, title

(defun cacoo:anything-preview-buffer-update-mode-line ()
  (let ((anm "-/|\\"))
    (setq mode-line-format 
          (format cacoo:anything-preview-mode-line-format
                  (char-to-string 
                   (aref anm (% preview-count (length anm))))
                  preview-progress preview-title)))
    (force-mode-line-update))

(defun cacoo:anything-preview-buffer-on-show-progress (args)
  (with-current-buffer (get-buffer cacoo:anything-preview-buffer)
    (destructuring-bind (event (progress)) args
      (setq preview-progress progress)
      (cacoo:anything-preview-buffer-update-mode-line))))

(defun cacoo:anything-preview-buffer-on-show-animation (buf)
  (with-current-buffer (get-buffer cacoo:anything-preview-buffer)
    (incf preview-count)
    (cacoo:anything-preview-buffer-update-mode-line)))


(defvar cacoo:anything-preview-buffer-thread nil "[internal]")

(defun cacoo:anything-preview-buffer-stop-animation ()
  (setq cacoo:anything-preview-buffer-thread nil))

(defun cacoo:anything-preview-buffer-start-animation ()
  (unless cacoo:anything-preview-buffer-thread
    (setq cacoo:anything-preview-buffer-thread t)
    (cc:thread 
     60 
     (while cacoo:anything-preview-buffer-thread
       (cc:signal-send cacoo:anything-channel 'animation)))))


(defun cacoo:anything-preview-progress (d current total)
  (lexical-let
      ((progress (apply 'concat
             (loop for i from 1 to total
                   collect (if (<= i current) "O" ".")))))
    (deferred:nextc (or d (deferred:succeed))
      (lambda (x)
        (cc:signal-send cacoo:anything-channel 'progress progress)
        x))))


(defun cacoo:api-image-get (url win)
  (let ((image (cacoo:api-image-cache-get-mru url)))
    (cond
     (image
      (deferred:succeed image))
     (t
      (lexical-let
          ((url url) (file "/tmp/a.png") (tmpfile "/tmp/_ip.png")
           (win (cacoo:anything-prepare-window)))
        (deferred:$
          (cc:semaphore-interrupt-all cacoo:preview-semaphore)
          (cacoo:anything-preview-progress it 1 4)
          (deferred:nextc it
            (lambda (x)
              (cc:signal-send cacoo:anything-channel 'image-load-start)
             (deferred:process "wget" "-q" "-O" file (concat url "?apiKey=" cacoo:api-key))))
          (cacoo:anything-preview-progress it 3 4)
          (deferred:nextc it
            (lambda (x)
              (deferred:process "identify" "-format" "%w %h" file)))
          (deferred:nextc it
            (lambda (sizestr)
              (let* ((ww (* (window-width win) (frame-char-width)))
                     (wh (* (- (window-height win) 2) (frame-char-height)))
                     (isize (mapcar 'string-to-int (split-string sizestr))))
                (if (or (< ww (car isize)) (< wh (cadr isize)))
                    (progn 
                      (cacoo:anything-preview-progress nil 4 4)
                      (deferred:$
                        (deferred:process
                          "convert" "-resize" (format "%ix%i" ww wh)
                          file (concat (file-name-extension tmpfile) ":" tmpfile))
                        (deferred:nextc it (lambda (x) tmpfile))))
                  file))))
          (deferred:nextc it
            (lambda (ifile)
              (clear-image-cache)
              (let ((img (create-image (cacoo:load-image-data ifile) 'png t)))
                (cacoo:api-image-cache-add url img)
                img)))
          (deferred:error it
            (lambda (e) (message "Preview Error : %s" e)))
          (deferred:nextc it
            (lambda (x)
              (cc:semaphore-release cacoo:preview-semaphore)
              (cc:signal-send cacoo:anything-channel 'image-load-finish)
              (when (file-exists-p tmpfile)
                (delete-file tmpfile))
              x))))))))

(defun cacoo:load-image-data (file)
  (let ((buf (find-file-noselect file t t)))
    (prog1 (with-current-buffer buf (buffer-string))
      (kill-buffer buf))))

(defvar cacoo:anything-preview-window nil "[internal]")

(defun cacoo:anything-prepare-window ()
  (let ((win (anything-window)))
    (unless cacoo:anything-preview-window
      (setq cacoo:anything-preview-window 
            (cond
             ((< (window-width win) (* 2 (window-height win)))
              (split-window win))
             (t
              (split-window win (/ (window-width win) 2) t))))
      (set-window-buffer 
       cacoo:anything-preview-window 
       (cacoo:anything-preview-buffer-init "No Image...")))
    cacoo:anything-preview-window))

(defvar cacoo:preview-semaphore (cc:semaphore-create 1) "[internal]")

(defun cacoo:anything-preview (title url)
  (lexical-let ((url url) (title title)
                (win (cacoo:anything-prepare-window)))
    (deferred:$
      (cacoo:api-image-get url win)
      (deferred:nextc it
        (lambda (img)
          (cc:signal-send cacoo:anything-channel 'show-image title url img)))
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
    (loop for d in cacoo:api-diagrams-cache
          do
          (loop for s in (cdr (assq 'sheets d))
                do
                (push (cacoo:anything-format d s) lines)))
    (nreverse lines)))

(defun cacoo:anything-preview-action (data)
  (let ((diagram (car data)) (sheet (cdr data)))
    (cacoo:anything-preview 
     (car (cacoo:anything-format diagram sheet))
     (cacoo:k 'imageUrlForApi sheet))))

(defvar anything-c-source-cacoo 
  '((name . "Image source")
    (candidates . cacoo:anything-collect-diagrams)
    (action 
     ("Insert URL" 
      . (lambda (x) (insert (cacoo:k 'imageUrl (cdr x)))))
     ("Show Detail (Browser)" 
      . (lambda (x) (cacoo:open-browser (cacoo:k 'url (cdr x)))))
     ("Edit Diagram (Browser)" 
      . (lambda (x) (cacoo:open-browser 
                     (cacoo:make-url 
                      cacoo:edit-url 
                      (cacoo:k 'diagramId (car x)))))))
    (candidate-number-limit . 200)
    (migemo)
    (persistent-action
     . cacoo:anything-preview-action)))

(defadvice anything-move-selection-common (after cacoo:anything)
  (when (eq (anything-buffer-get) anything-buffer)
    (anything-execute-persistent-action)))

;;; Canceling asynchronous tasks

(defun cacoo:anything-prepare-cancel ()
  (interactive)
  (defadvice keyboard-quit (before cacoo:anything-cancel)
    (cacoo:anything-cancel)
    (cacoo:anything-clear-cancel))
  (ad-activate-regexp "cacoo:anything-cancel"))

(defun cacoo:anything-clear-cancel ()
  (interactive)
  (ignore-errors
    (ad-deactivate-regexp "cacoo:anything-cancel")
    (ad-remove-advice 'keyboard-quit 'after 'cacoo:anything-cancel)))

(defun cacoo:anything-cancel ()
  (interactive)
  (setq cacoo:api-cancel-flag t))

;;; Startup and Cleanup

(defun cacoo:anything-startup ()
  (setq cacoo:api-cancel-flag nil)
  (setq cacoo:anything-preview-window nil)
  (setq cacoo:anything-channel (cc:signal-channel 'cacoo:anything))
  (cacoo:anything-prepare-cancel))

(defun cacoo:anything-cleanup ()
  (let ((buf (get-buffer cacoo:anything-preview-buffer)))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf)))
  (cc:signal-disconnect-all cacoo:anything-channel)
  (setq cacoo:anything-channel nil))

(defun cacoo:anything-cache-clear ()
  (interactive)
  (setq cacoo:api-diagrams-cache nil
        cacoo:api-image-cache nil))

;;; Anything command

(defun cacoo:anything-command ()
  (interactive)
  (cacoo:anything-startup)
  (deferred:$
    (cacoo:api-retrieve-diagrams)
    (deferred:nextc it
      (lambda (x) 
        (ad-activate-regexp "cacoo:anything")
        (cacoo:anything-clear-cancel)
        (unless cacoo:api-cancel-flag
          (anything anything-c-source-cacoo))))
    (deferred:error it
      (lambda (e) (message "Error : %s" e)))
    (deferred:nextc it
      (lambda (x)
        (setq cacoo:api-cancel-flag t)
        (ad-deactivate-regexp "cacoo:anything")
        (cacoo:anything-cleanup)))))

; (cacoo:api-report-semaphore)
; (cacoo:anything-command)
; (cacoo:anything-cache-clear)
