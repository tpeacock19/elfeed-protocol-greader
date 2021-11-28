;;; elfeed-protocol-greader.el --- Tiny Tiny RSS protocol for elfeed -*- lexical-binding: t; -*-

;;; Commentary:
;; Tiny Tiny RSS protocol for elfeed.

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'subr-x)
(require 'elfeed)
(require 'elfeed-protocol-common)

;;; Code:

(defcustom elfeed-protocol-greader-sid nil
  "Tiny Tiny RSS session id.
Will generate one if is empty or invalid."
  :group 'elfeed-protocol
  :type 'string)

(defcustom elfeed-protocol-greader-api-token nil
  "Tiny Tiny RSS session id.
Will generate one if is empty or invalid."
  :group 'elfeed-protocol
  :type 'string)

(defcustom elfeed-protocol-greader-maxsize 500
  "Maximize entries size for each request.
As the document said, before API level 6 maximum amount of returned headlines is
capped at 60, API 6 and above sets it to 200. So set bigger than 200 just
invalid."
  :group 'elfeed-protocol
  :type 'integer)

(defcustom elfeed-protocol-greader-star-tag 'starred
  "Default star tag for Tiny Tiny RSS entry.
If one entry set or remove the tag,
then the starred state in Tiny Tiny RSS will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defcustom elfeed-protocol-greader-publish-tag 'publish
  "Default publish tag for Tiny Tiny RSS entry.
If one entry set or remove the tag,
then the published state in Tiny Tiny RSS will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defcustom elfeed-protocol-greader-fetch-category-as-tag t
  "If true, tag the Tiny Tiny RSS feed category to feed item."
  :group 'elfeed-protocol
  :type 'boolean)

(defvar elfeed-protocol-greader-feeds (make-hash-table :test 'equal)
  "Feed list from Tiny Tiny RSS, will be filled before updating operation.")

(defvar elfeed-protocol-greader-ids (make-hash-table :test 'equal)
  "Feed list from Tiny Tiny RSS, will be filled before updating operation.")

(defvar elfeed-protocol-greader-categories (make-hash-table :test 'equal)
  "Category list from Tiny Tiny RSS, will be used to tag entries with their GREADER category.")

(defconst elfeed-protocol-greader-api-path "/reader/api/0")
(defconst elfeed-protocol-greader-login-path "/accounts/ClientLogin?")
(defconst elfeed-protocol-greader-api-token-path "/token")
(defconst elfeed-protocol-greader-edit-tag-path "/edit-tag")

(defconst elfeed-protocol-greader-api-user-info "/user-info?output=json")
(defconst elfeed-protocol-greader-api-max-limit 20)
(defconst elfeed-protocol-greader-api-status-ok 0)
(defconst elfeed-protocol-greader-api-status-err 1)

;; list of categories/folders and tags/labels (including unread)
(defconst elfeed-protocol-greader-api-tag-list "/tag/list")
;; full list of subs/feeds & their category/folder
(defconst elfeed-protocol-greader-api-subscription-list "/subscription/list")

(defconst elfeed-protocol-greader-api-reading-list "/stream/contents/user/-/state/com.google/reading-list")
(defconst elfeed-protocol-greader-api-reading-list-ids "/stream/items/ids")
(defconst elfeed-protocol-greader-api-reading-list-contents "/stream/items/contents")
(defconst elfeed-protocol-greader-api-starred "/stream/contents/user/-/state/com.google/starred")
(defconst elfeed-protocol-greader-api-starred-ids "/subscription/list")

(defun elfeed-protocol-greader-api-client-params (&optional num continue)
  (let* ((time (car (time-convert nil 1000))))
    (concat (format "?output=json&client=elfeed&ck=%s&n=%s" time (or num elfeed-protocol-greader-maxsize))
            (when continue (format "&c=%s" continue)))))

(defun elfeed-protocol-greader-api-time-filter (&optional up-time)
  "Defaults to 1 week."
  (let* ((time (car (time-convert nil 1000)))
         (update-time (or up-time
                          (substring (number-to-string (- time (* 7 3600))) nil -3))))
    (format "&ot=%s" update-time)))


(defconst elfeed-protocol-greader-api-feed-id-starred -1)
(defconst elfeed-protocol-greader-api-feed-id-published -2)
(defconst elfeed-protocol-greader-api-feed-id-fresh -3)
(defconst elfeed-protocol-greader-api-feed-id-all-articles -4)
(defconst elfeed-protocol-greader-api-feed-id-archived 0)
(defconst elfeed-protocol-greader-api-view-mode-all-articles "all_articles")
(defconst elfeed-protocol-greader-api-view-mode-unread "unread")
(defconst elfeed-protocol-greader-api-view-mode-adaptive "adaptive")
(defconst elfeed-protocol-greader-api-view-mode-marked "marked")
(defconst elfeed-protocol-greader-api-view-mode-updated "updated")
(defconst elfeed-protocol-greader-api-update-article-field-starred "starred")
(defconst elfeed-protocol-greader-api-update-article-field-unread "unread")
(defconst elfeed-protocol-greader-api-update-article-field-article-note 3)

(defun elfeed-protocol-greader-id (url)
  "Get greader protocol id with URL."
  (elfeed-protocol-id "greader" url))

(defun elfeed-protocol-greader--init-headers (host-url)
  "Get http request headers for greader."
  (if-let ((user (elfeed-protocol-meta-user (elfeed-protocol-greader-id host-url)))
           (headers `(("Agent" . ,elfeed-user-agent)
                      ("Connection" . "Keep-Alive")
                      ("Accept-Encoding" . "gzip"))))
      (progn
        (when elfeed-protocol-greader-sid
          (push (cons "Authorization"
                      (format "GoogleLogin auth=%s/%s" user elfeed-protocol-greader-sid))
                headers))
        headers)
    (elfeed-log 'error "elfeed-protocol-greader: missing username")))

(defun elfeed-protocol-greader--get-api-url (host-url)
  "Get fever server API url.
HOST-URL is the host name of Fever server."
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         (url (elfeed-protocol-meta-data proto-id :api-url)))
    url))


(defun elfeed-protocol-greader-get-update-mark (proto-id update-action)
  "Get last update mark for special UPDATE-ACTION.
PROTO-ID is the target protocol feed id.  UPDATE-ACTION could be update,
update-older or update-star.  If not initialized, just return -1."
  (interactive (list (completing-read "Protocol Feed: " (elfeed-protocol-feed-list))
                     (intern (completing-read "Update action: " '(update update-older update-star) nil t))))
  (let* ((key (cond
               ((eq update-action 'update) :last-entry-id)
               ((eq update-action 'update-older) :first-entry-id)
               ((eq update-action 'update-star) :star-entry-skip)))
         (mark (elfeed-protocol-get-feed-meta-data proto-id key)))
    (if mark mark -1)))

(defun elfeed-protocol-greader-set-update-mark (proto-id update-action mark)
  "Set last update mark to elfeed db.
PROTO-ID is the target protocol feed id.  UPDATE-ACTION could be update,
update-older or update-star.  MARK the target value."
  (interactive (list (completing-read "Protocol Feed: " (elfeed-protocol-feed-list))
                     (intern (completing-read "Update action: " '(update update-older update-star)))
                     (read-number "Mark number: ")))
  (let* ((key (cond
               ((eq update-action 'update) :last-entry-id)
               ((eq update-action 'update-older) :first-entry-id)
               ((eq update-action 'update-star) :star-entry-skip))))
    (elfeed-protocol-set-feed-meta-data proto-id key mark)))

(defmacro elfeed-protocol-greader--parse-json (&rest body)
  "Parse greader api result JSON buffer.
Will eval rest BODY expressions at end."
  (declare (indent defun))
  `(let* ((json-array-type 'list)
          (content (json-read))
          (err "Unauthorized!")
          (continue (map-elt content 'continuation)))
     (if (eq content err)
         (elfeed-log 'error "elfeed-protocol-greader: %s" err)
       ;; (elfeed-log 'error "elfeed-protocol-greader: %s" content)
       (setq err nil)
       ,@body)))

(defmacro elfeed-protocol-greader--parse-plain (&rest body)
  "Parse greader api result JSON buffer.
Will eval rest BODY expressions at end."
  (declare (indent defun))
  `(let* ((str (string-chop-newline (buffer-string))))
     ,@body))

(defmacro elfeed-protocol-greader--parse-login (&rest body)
  "Parse greader api result JSON buffer.
Will eval rest BODY expressions at end."
  (declare (indent defun))
  `(let* ((str (buffer-string))
          (match (progn (string-match
                         (rx "Auth" ?= (group (one-or-more alnum)) ?/ (group (one-or-more alnum)))
                         str)
                        (match-data)))
          (user (match-string 1 str))
          (sid (match-string 2 str)))
     ,@body))

(defmacro elfeed-protocol-greader--login (host-url method data &rest body)
  "Just like `elfeed-with-fetch' but special for greader HTTP request.
HOST-URL is the host name of Tiny Tiny RSS server, METHOD could be
\"GET\" or \"POST\", DATA is in JSON string format.  Optional argument
BODY is the rest Lisp code after operation finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl)
          (headers (elfeed-protocol-greader--init-headers host-url))
          (no-auth-url (elfeed-protocol-no-auth-url url))
          (cb (lambda (status)
                (if (elfeed-is-status-error status use-curl)
                    (let ((print-escape-newlines t))
                      (elfeed-handle-http-error
                       no-auth-url
                       (if use-curl elfeed-curl-error-message status)))
                  (progn
                    (unless use-curl
                      (elfeed-move-to-first-empty-line)
                      (set-buffer-multibyte t))
                    (when elfeed-protocol-log-trace
                      (elfeed-log 'debug "Login elfeed-protocol-greader: %s" (buffer-string)))
                    (elfeed-protocol-greader--parse-login ,@body)
                    (unless use-curl
                      (kill-buffer)))))))
     (if use-curl
         (elfeed-curl-enqueue no-auth-url cb :headers headers
                              :method ,method :data ,data)
       (let ((url-request-extra-headers headers)
             (url-request-method ,method)
             (url-request-data ,data))
         (url-retrieve no-auth-url cb () t t)))))

(defmacro elfeed-protocol-greader--api-token (host-url path method &rest body)
  "Just like `elfeed-with-fetch' but special for greader HTTP request.
HOST-URL is the host name of Tiny Tiny RSS server, METHOD could be
\"GET\" or \"POST\", DATA is in JSON string format.  Optional argument
BODY is the rest Lisp code after operation finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl)  ; capture current value in closure
          (url (concat (elfeed-protocol-greader--get-api-url host-url) ,elfeed-protocol-greader-api-path ,path))
          (headers (elfeed-protocol-greader--init-headers ,host-url))
          (no-auth-url (elfeed-protocol-no-auth-url url))
          (cb (lambda (status)
                (if (elfeed-is-status-error status use-curl)
                    (let ((print-escape-newlines t))
                      (elfeed-handle-http-error
                       no-auth-url
                       (if use-curl elfeed-curl-error-message status)))
                  (unless use-curl
                    (elfeed-move-to-first-empty-line)
                    (set-buffer-multibyte t))
                  (elfeed-protocol-greader--parse-plain ,@body)
                  (unless use-curl
                    (kill-buffer))))))
     (if use-curl
         (elfeed-curl-enqueue no-auth-url cb :headers headers)
       (let ((url-request-extra-headers headers))
         (url-retrieve no-auth-url cb () t t)))))

(defun elfeed-protocol-greader-get-api-token (host-url)
  "Login remote Tiny Tiny RSS server.
The success session id will saved to
`elfeed-protocol-greader-sid'.  HOST-URL is the target Tiny Tiny RSS
server url, and will call CALLBACK after login."
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         (c-time (round (time-to-seconds))))
    (unless (and elfeed-protocol-greader-api-token
                 (< (- c-time (cdr elfeed-protocol-greader-api-token)) (* .5 3600)))
      (when elfeed-protocol-log-trace
        (elfeed-log 'debug "elfeed-protocol-greader: getting api token"))
      (elfeed-protocol-greader--api-token
        host-url elfeed-protocol-greader-api-token-path "GET"
        (setq elfeed-protocol-greader-api-token (cons str c-time))))))

(defun elfeed-protocol-greader-login (host-url &optional callback)
  "Login remote Tiny Tiny RSS server.
The success session id will saved to
`elfeed-protocol-greader-sid'.  HOST-URL is the target Tiny Tiny RSS
server url, and will call CALLBACK after login."
  (elfeed-log 'debug "elfeed-protocol-greader: login")
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         (user (elfeed-protocol-meta-user proto-id))
         (password (elfeed-protocol-meta-password proto-id))
         (url (concat (elfeed-protocol-greader--get-api-url host-url) elfeed-protocol-greader-login-path )))
    (elfeed-protocol-greader--login
      url "POST" (format "&Email=%s&Passwd=%s" user password)
      (setq elfeed-protocol-greader-sid sid)
      (elfeed-protocol-greader-get-api-token host-url)
      (when callback (funcall callback)))))

(defmacro elfeed-protocol-greader-with-fetch (host-url path method data &rest body)
  "Just like `elfeed-with-fetch' but special for greader HTTP request.
HOST-URL is the host name of Tiny Tiny RSS server, METHOD could be
\"GET\" or \"POST\", DATA is in JSON string format.  Optional argument
BODY is the rest Lisp code after operation finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl)  ; capture current value in closure
          (url (concat (elfeed-protocol-greader--get-api-url host-url) ,elfeed-protocol-greader-api-path ,path))
          (headers (elfeed-protocol-greader--init-headers host-url))
          (no-auth-url (elfeed-protocol-no-auth-url url))
          (cb (lambda (status)
                (if (elfeed-is-status-error status use-curl)
                    (let ((print-escape-newlines t))
                      (elfeed-handle-http-error
                       no-auth-url
                       (if use-curl elfeed-curl-error-message status)))
                  (unless use-curl
                    (elfeed-move-to-first-empty-line)
                    (set-buffer-multibyte t))
                  ;; (when elfeed-protocol-log-trace
                  ;;   (elfeed-log 'debug "elfeed-protocol-greader: %s" (buffer-string)))
                  (elfeed-protocol-greader--parse-json ,@body)
                  (unless use-curl
                    (kill-buffer))))))
     (if use-curl
         (if ,data
             (elfeed-curl-enqueue no-auth-url cb :headers headers
                                  :method "POST" :data ,data)
           (elfeed-curl-enqueue no-auth-url cb :headers headers))
       (if ,data
           (let ((url-request-extra-headers headers)
                 (url-request-method "POST")
                 (url-request-data ,data))
             (url-retrieve no-auth-url cb () t t))
         (let ((url-request-extra-headers headers))
           (url-retrieve no-auth-url cb () t t))))))

(defmacro elfeed-protocol-greader--parse-ids (host-url content &rest body)
  "Parse the feeds JSON buffer and fill results to db.
HOST-URL is the host name of Tiny Tiny RSS server.  CONTENT is the
result JSON content by http request.  Return
`elfeed-protocol-greader-feeds'."
  (declare (indent defun))
  `(let* ((sync-resp (map-elt content 'itemRefs))
          (sync-ids (mapcar (lambda (id)
                              (string-to-number (map-elt id 'id)))
                            sync-resp))
          (entries (cl-loop for k being the hash-keys of elfeed-db-entries
                            using (hash-values v)
                            if (member (elfeed-meta v :id) sync-ids)
                            collect v into tagged
                            else
                            collect v into untagged
                            finally return (cons tagged untagged))))
     ;; (elfeed-log 'debug "elfeed-protocol-greader: parsing inc: %s" entries)
     ,@body))

(defun elfeed-protocol-greader-sync (host-url tag)
  "Sync the TAG status of feeds for HOST-URL greader server."
  ;; ensure categories are updated
  (let* ((tag-name (elfeed-protocol-greader--get-category-id
                    host-url
                    (if (eq tag 'unread) 'read tag))))
    (elfeed-protocol-greader-with-fetch host-url
      (concat elfeed-protocol-greader-api-reading-list-ids
              (elfeed-protocol-greader-api-client-params 10000)
              (cond
               ((eq tag 'unread)
                "&s=user/-/state/com.google/reading-list&xt=user/-/state/com.google/read")
               ((eq tag elfeed-protocol-greader-star-tag)
                "&s=user/-/state/com.google/starred")))
      "GET" nil
      (elfeed-protocol-greader--parse-ids host-url content
        (when tag-name
          (elfeed-tag (car entries) tag)
          (elfeed-untag (cdr entries) tag))))))

(defun elfeed-protocol-greader-sync-states (host-url)
  (interactive)
  (let ((host-url (or (elfeed-protocol-url host-url)
                      host-url)))
    (when elfeed-protocol-greader-api-token
      (elfeed-protocol-greader--update-categories-list host-url)
      (elfeed-protocol-greader-sync host-url elfeed-protocol-greader-star-tag)
      (elfeed-protocol-greader-sync host-url 'unread))))

(add-hook 'elfeed-update-hooks #'elfeed-protocol-greader-sync-states)
;; * Categories

(defun elfeed-protocol-greader--parse-categories (host-url content)
  "Parse the feeds JSON buffer and cache the result.
HOST-URL is the host name of Tiny Tiny RSS server.  CONTENT is the
result JSON content by http request.  Return cached
`elfeed-protocol-greader-categories'."
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         (categories (map-elt content 'tags))
         (named-1 (cl-loop for category in categories
                           do (setq cat (map-elt category 'id))
                           (setq name (progn (string-match
                                              (rx (one-or-more anything) ?/
                                                  (group (one-or-more (or alnum " "))))
                                              cat)
                                             (match-string 1 cat)))
                           collect (push (cons 'name name) category)))
         (named (push (list (cons 'name "read") (cons 'id "user/-/state/com.google/read")) named-1)))
    (puthash proto-id named elfeed-protocol-greader-categories)
    elfeed-protocol-greader-categories))

(defun elfeed-protocol-greader--update-categories-list (host-url &optional callback)
  "Update Tiny Tiny RSS server categories list.
HOST-URL is the host name of Tiny Tiny RSS server.  Will call CALLBACK
at end."
  (elfeed-log 'debug "elfeed-protocol-greader: update category list")
  (elfeed-protocol-greader-with-fetch
    host-url (concat elfeed-protocol-greader-api-tag-list
                     (elfeed-protocol-greader-api-client-params))
    "GET" nil
    (elfeed-protocol-greader--parse-categories host-url content)
    (when callback (funcall callback))))

(defun elfeed-protocol-greader--get-category-name (host-url category-id)
  "Return category name from HOST-URL (symbol) for CATEGORY-ID."
  ;; (elfeed-log 'debug "elfeed-protocol-greader: Fetching category name for id %s" category-id)
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         (categories (gethash proto-id elfeed-protocol-greader-categories))
         (category (cl-some (lambda (category)
                              (let ((id (map-elt category 'id ))
                                    (category-name (map-elt category 'name)))
                                (if (string= id category-id) (intern category-name))))
                            categories)))
    category))

(defun elfeed-protocol-greader--get-category-id (host-url category-name)
  "Return category id from HOST-URL (symbol) for CATEGORY-NAME."
  (elfeed-log 'debug "elfeed-protocol-greader: Fetching category name for id %s" category-name)
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         (categories (gethash proto-id elfeed-protocol-greader-categories))
         (category (cl-some (lambda (category)
                              (let ((id (map-elt category 'id ))
                                    (name (map-elt category 'name)))
                                (when (string= name category-name) id)))
                            categories)))
    category))

;; * Feeds

(defun elfeed-protocol-greader--parse-feeds (host-url content)
  "Parse the feeds JSON buffer and fill results to db.
HOST-URL is the host name of Tiny Tiny RSS server.  CONTENT is the
result JSON content by http request.  Return
`elfeed-protocol-greader-feeds'."
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         (feeds (map-elt content 'subscriptions)))
    (puthash proto-id feeds elfeed-protocol-greader-feeds)
    (cl-loop for feed in feeds do
             (let* ((feed-url (map-elt feed 'url))
                    (feed-id (elfeed-protocol-format-subfeed-id
                              proto-id feed-url))
                    (feed-title (map-elt feed 'title))
                    (feed-db (elfeed-db-get-feed feed-id)))
               (setf (elfeed-feed-url feed-db) feed-id
                     (elfeed-feed-title feed-db) feed-title)))
    (elfeed-log 'debug "elfeed-protocol-greader: found %s feeds" (length feeds))
    elfeed-protocol-greader-feeds))

(defun elfeed-protocol-greader--update-feed-list (host-url &optional callback)
  "Update Tiny Tiny RSS server feeds list.
HOST-URL is the host name of Tiny Tiny RSS server.  Will call CALLBACK
at end."
  (elfeed-log 'debug "elfeed-protocol-greader: update feed list")
  (let* ((parse-feeds (lambda ()
                        (elfeed-protocol-greader-with-fetch
                          host-url (concat elfeed-protocol-greader-api-subscription-list
                                           (elfeed-protocol-greader-api-client-params))
                          "GET" nil
                          (elfeed-protocol-greader--parse-feeds host-url content)
                          (when callback (funcall callback))))))
    (if (and elfeed-protocol-greader-fetch-category-as-tag
             elfeed-protocol-greader-api-token)
        (progn (elfeed-protocol-greader--update-categories-list host-url)
               (funcall parse-feeds))
      (elfeed-protocol-greader-get-api-token host-url)
      (elfeed-protocol-greader--update-categories-list host-url)
      (funcall parse-feeds))))

(defmacro elfeed-protocol-greader-fetch-prepare (host-url &rest body)
  "Ensure logged in and feed list updated before expressions.
HOST-URL is the host name of Fever server.  And will eval rest
BODY expressions at end."
  (declare (indent defun))
  `(if (and elfeed-protocol-greader-sid
            elfeed-protocol-greader-api-token)
       (elfeed-protocol-greader--update-feed-list
        ,host-url (lambda () ,@body))
     (elfeed-protocol-greader-login
      host-url
      (lambda()
        (elfeed-protocol-greader-get-api-token host-url)
        (elfeed-protocol-greader--update-feed-list
         ,host-url (lambda () ,@body))))))

;; * Subfeeds

(defun elfeed-protocol-greader--get-subfeed-url (host-url feed-id)
  "Get sub feed url for the greader protocol feed HOST-URL and FEED-ID."
  (let* ((url (catch 'found
                (let* ((proto-id (elfeed-protocol-greader-id host-url))
                       (feeds (gethash proto-id elfeed-protocol-greader-feeds))
                       (length (length feeds)))
                  (dotimes (i length)
                    (let* ((feed (elt feeds i))
                           (id (map-elt feed 'id))
                           (url (map-elt feed 'url)))
                      (when (string= id feed-id)
                        (throw 'found url))))))))
    (unless url
      (setq url elfeed-protocol-unknown-feed-url)
      (elfeed-log 'warn "elfeed-protocol-greader: no subfeed for feed id %s, fallback to unknown feed" feed-id))
    url))

(defun elfeed-protocol-greader--get-subfeed-id (host-url feed-url)
  "Get sub feed id the greader protocol feed HOST-URL and FEED-URL."
  (let* ((id (catch 'found
               (let* ((proto-id (elfeed-protocol-greader-id host-url))
                      (feeds (gethash proto-id elfeed-protocol-greader-feeds))
                      (length (length feeds)))
                 (dotimes (i length)
                   (let* ((feed (elt feeds i))
                          (id (map-elt feed 'id))
                          (url (map-elt feed 'url)))
                     (when (string= url feed-url)
                       (throw 'found id))))))))
    (unless id
      (elfeed-log 'error "elfeed-protocol-greader: no subfeed for feed url %s" feed-url))
    id))

(defun elfeed-protocol-greader--get-subfeed-category-id (host-url feed-id)
  "Get sub feed category id the greader protocol feed HOST-URL and FEED-ID."
  (let* ((category-id
          (catch 'found
            (let* ((proto-id (elfeed-protocol-greader-id host-url))
                   (feeds (gethash proto-id elfeed-protocol-greader-feeds))
                   (length (length feeds)))
              (dotimes (i length)
                (let* ((feed (elt feeds i))
                       (id (map-elt feed 'id))
                       (category-id (map-elt (car (map-elt feed 'categories)) 'id)))
                  (when (string= id feed-id)
                    (throw 'found category-id))))))))
    category-id))

(defun elfeed-protocol-greader--get-subfeed-id-by-title (host-url feed-title)
  "Get sub feed id the greader protocol feed HOST-URL and FEED-TITLE."
  (let* ((id (catch 'found
               (let* ((proto-id (elfeed-protocol-greader-id host-url))
                      (feeds (gethash proto-id elfeed-protocol-greader-feeds))
                      (length (length feeds)))
                 (dotimes (i length)
                   (let* ((feed (elt feeds i))
                          (id (map-elt feed 'id))
                          (title (map-elt feed 'title)))
                     (when (string= title feed-title)
                       (throw 'found id))))))))
    (unless id
      (elfeed-log 'error "elfeed-protocol-greader: no subfeed for feed title %s" feed-title))
    id))

(defun elfeed-protocol-greader-entry-p (entry)
  "Check if specific ENTRY is fetched from Tiny Tiny RSS."
  (let* ((proto-id (elfeed-protocol-entry-protocol-id entry))
         (proto-type (when proto-id (elfeed-protocol-type proto-id))))
    (string= proto-type "greader")))

;; * Parsing Entries

(defun elfeed-protocol-greader--parse-entries (host-url &optional update-action callback content)
  "Parse the entries JSON buffer and fill results to elfeed db.
Ensure the point in the right place that `json-read' could execute.
HOST-URL is the host name of greader server.  If UPDATE-ACTION is nil,
then just not update the :last-modifed, :first-entry-id
and :last-entry-id values.  If CALLBACK is not nil, will call it with
the result entries as argument.  Return parsed entries.

User could download items.json from greader manually, for example
http://myhost.com/items?type=3&batchSize=-1, and import the entries by calling
`elfeed-protocol-greader--parse-entries' in the buffer."
  (if (> (hash-table-count elfeed-protocol-greader-feeds) 0)
      (let* ((proto-id (elfeed-protocol-greader-id host-url))
             (unread-num 0)
             (starred-num 0)
             (begin-time (time-to-seconds))
             (begin-time-id (car (time-convert nil 1)))
             (entry-mark (elfeed-protocol-greader-get-update-mark proto-id update-action))
             (first-entry-id (elfeed-protocol-get-first-entry-id proto-id))
             (last-star-entry-id (elfeed-protocol-greader-get-update-mark 'update-star proto-id))
             (last-entry-id (elfeed-protocol-get-last-entry-id proto-id))
             (json-array-type 'list)
             items entries
             (entries-list '()))
        (elfeed-log 'debug "elfeed-protocol-greader: parsing entries, first-entry-id: %s last-entry-id: %s last-star-entry-id: %s"
                    first-entry-id last-entry-id last-star-entry-id)
        (goto-char (point-min))
        (setq items (map-elt (or content (json-read)) 'items))
        (setq entries
              (cl-loop for item in items collect
                       (pcase-let* (((map ('timestampUsec id) ('id guid) ('origin origin) ('canonical entry-url) title
                                          ('published pub-date) summary categories ('enclosure enclosures)
                                          author ('timestampUsec last-modified))
                                     item)
                                    (id (if (stringp id) (string-to-number id) id))
                                    (entry-id (car (time-convert (cons id 1000000) 1)))
                                    (entry-url (map-elt (elt entry-url 0) 'href))
                                    (feed-title (map-elt origin 'title))
                                    (feed-id (map-elt origin 'streamId))
                                    (catnames (delete 'nil (mapcar (lambda (c)
                                                                     (elfeed-protocol-greader--get-category-name host-url c))
                                                                   categories)))
                                    (feed-url (elfeed-protocol-greader--get-subfeed-url host-url feed-id))
                                    (starred (member 'starred catnames))
                                    (categories (if starred
                                                    (cl-substitute elfeed-protocol-greader-star-tag 'starred catnames)
                                                  catnames))
                                    (unread (not (member 'read categories)))
                                    (namespace (elfeed-url-to-namespace feed-url))
                                    (guid-hash (progn (string-match (rx (one-or-more anything) ?/ (group (one-or-more (or alnum " "))))
                                                                    guid)
                                                      (match-string 1 guid)))
                                    (full-id (cons namespace (elfeed-cleanup guid-hash)))
                                    (original (elfeed-db-get-entry full-id))
                                    (original-date (and original
                                                        (elfeed-entry-date original)))
                                    (autotags (elfeed-protocol-feed-autotags proto-id feed-url))
                                    (fixtags (elfeed-normalize-tags
                                              autotags elfeed-initial-tags categories))
                                    (tags (progn
                                            (unless unread
                                              (progn (setq fixtags (delete 'unread fixtags))
                                                     (setq fixtags (delete 'read fixtags))))
                                            fixtags))
                                    (body (map-elt summary 'content))
                                    (last-modified (if (stringp last-modified) (string-to-number last-modified) last-modified))
                                    (enclosure-link (map-elt (elt enclosures 0) 'href))
                                    (enclosure-mime (map-elt (elt enclosures 0) 'type))
                                    (enclosures (when enclosure-link
                                                  (list (list enclosure-link
                                                              enclosure-mime 0))))
                                    (db-entry (elfeed-entry--create
                                               :title (elfeed-cleanup title)
                                               :id full-id
                                               :feed-id (elfeed-protocol-format-subfeed-id
                                                         proto-id feed-url)
                                               :link (elfeed-cleanup entry-url)
                                               :tags tags
                                               :date (elfeed-new-date-for-entry
                                                      original-date pub-date)
                                               :enclosures enclosures
                                               :content body
                                               :content-type 'html
                                               :meta `(,@(when author
                                                           (list :author author))
                                                       ,@(list :protocol-id proto-id
                                                               :id id
                                                               :guid-hash guid-hash
                                                               :feed-id feed-id)))))
                         (when unread (setq unread-num (1+ unread-num)))
                         (when starred (setq starred-num (1+ starred-num)))

                         ;; add table for timestampUsec->full-id
                         ;; relationship
                         (puthash id full-id elfeed-protocol-greader-ids)
                         ;; force override unread and star tags without repeat sync operation
                         (when original
                           (if unread (elfeed-tag-1 original 'unread)
                             (elfeed-untag-1 original 'unread))
                           (if starred (elfeed-tag-1 original elfeed-protocol-greader-star-tag)
                             (elfeed-untag-1 original elfeed-protocol-greader-star-tag)))

                         ;; calculate the last modified time and first last entry id
                         (setq entries-list (cl-pushnew entry-id entries-list))

                         (let ((last-entry (+ (apply #'max entries-list) 1))
                               (first-entry (- (apply #'min entries-list) 1)))
                           (when (eq update-action 'update-star)
                             (when (and (> last-entry last-star-entry-id)
                                        (> begin-time-id last-entry))
                               (setq last-star-entry-id last-entry)))

                           (when (eq update-action 'update)
                             (when (and (> last-entry last-entry-id)
                                        (> begin-time-id last-entry-id))
                               (setq last-entry-id last-entry)))

                           (when (or (< first-entry-id 0) (< entry-id first-entry-id))
                             (setq first-entry-id first-entry)))

                         (dolist (hook elfeed-new-entry-parse-hook)
                           (run-hook-with-args hook :greader item db-entry))
                         db-entry)))
        (elfeed-db-add entries)
        (when callback (funcall callback entries))

        (if (>= entry-mark 0)
            ;; update entry mark
            (cond
             ((eq update-action 'update-star)
              ;; only update star when we have results returned
              (when (> (length entries) 0)
                (elfeed-protocol-greader-set-update-mark
                 proto-id update-action last-star-entry-id)))
             ((eq update-action 'update)
              (elfeed-protocol-greader-set-update-mark
               proto-id update-action last-entry-id))
             ((eq update-action 'update-older)
              (elfeed-protocol-greader-set-update-mark
               proto-id update-action first-entry-id)))
          ;; init entry mark
          (unless (= (length entries) 0)
            (setq first-entry-id (max 1 first-entry-id)))
          (cond
           ((eq update-action 'init)
            (elfeed-protocol-greader-set-update-mark proto-id 'update first-entry-id)
            (elfeed-protocol-greader-set-update-mark proto-id 'update-star first-entry-id)
            (elfeed-protocol-greader-set-update-mark proto-id 'update-older first-entry-id))
           ((eq update-action 'update)
            (elfeed-protocol-greader-set-update-mark proto-id update-action last-entry-id)
            ;; set :first-entry-id same with :last-entry-id
            (elfeed-protocol-greader-set-update-mark proto-id 'update-older first-entry-id))
           ;; ((eq update-action 'update-older)
           ;;  (elfeed-protocol-greader-set-update-mark proto-id update-action first-entry-id))
           ;; init star skip
           ((eq update-action 'update-star)
            (elfeed-protocol-greader-set-update-mark proto-id update-action last-star-entry-id))))

        (elfeed-log 'debug "elfeed-protocol-greader: %s, parsed %d entries (%d unread, %d starred, first-entry-id: %s last-entry-id: %s last-star-entry-id: %s) with %fs, entry-mark: %d"
                    update-action (length entries) unread-num starred-num first-entry-id last-entry-id last-star-entry-id
                    (- (time-to-seconds) begin-time)
                    (elfeed-protocol-greader-get-update-mark proto-id update-action))
        entries)
    (progn
      (elfeed-log 'error "elfeed-protocol-greader:: elfeed-protocol-greader-feeds is nil, please call elfeed-protocol-greader--update-feed-list first")
      nil)))

(defun elfeed-protocol-greader--do-update (host-url action &optional arg callback continue limit)
  "Real greader updating operations.
HOST-URL is the host name of greader server, and user field
authentication info is always required so could find the related
protocol feed id correctly, for example
\"https://user:pass@myhost.com\". ACTION could be init,
update-since-time, update-subfeed and update-since-id. For init, will
fetch unread and starred entries. For update-subfeed, will fetch
unread entries for special sub feed, the ARG is the feed id. For
update-since-id, will fetch all entries after the provide entry
id. And for a update-since-time means only update entries since the
special modified time, the ARG is the time-stamp.  If CALLBACK is not
nil, will call it with the result entries as argument."
  (elfeed-log 'debug "elfeed-protocol-greader: update entries with action %s, arg %s" action arg)
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         ;; prevent the api max from being too high and causing php
         ;; memory limit issues
         (limit (or limit
                    elfeed-protocol-greader-maxsize))
         (entry-mark (elfeed-protocol-greader-get-update-mark proto-id action))
         (path-init-unread (concat elfeed-protocol-greader-api-reading-list
                                   (elfeed-protocol-greader-api-client-params limit continue)
                                   "&xt=user/-/state/com.google/read"))
         (path-all (concat elfeed-protocol-greader-api-reading-list
                           (elfeed-protocol-greader-api-client-params limit continue)))
         (path-nostar (concat elfeed-protocol-greader-api-reading-list
                              (elfeed-protocol-greader-api-client-params limit continue)
                              "&xt=user/-/state/com.google/starred"))
         (path-starred (concat elfeed-protocol-greader-api-starred
                               (elfeed-protocol-greader-api-client-params limit continue)))
         (mark-state t)
         path-opt data)
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (cond
     ;; initial sync, fetch unread entries
     ((eq action 'init)
      (elfeed-protocol-greader-set-update-mark proto-id 'update -1)
      (elfeed-protocol-greader-set-update-mark proto-id 'update-star -1)
      (elfeed-protocol-greader-set-update-mark proto-id 'update-older -1)
      (elfeed-protocol-clean-pending-ids proto-id)
      (setq path-opt path-init-unread)
      ;; initial update of ALL starred items (read & unread)
      (let ((action 'update-star))
        (elfeed-protocol-greader-with-fetch host-url path-starred "GET" nil
          (elfeed-protocol-greader--parse-entries host-url action callback content)
          (when continue
            (elfeed-log 'debug "init-star continuation: %s" continue)
            (elfeed-protocol-greader--do-update host-url 'update-star arg))))
      ;; initial update of items up until current
      (let ((action 'update))
        (elfeed-protocol-greader-with-fetch host-url path-nostar "GET" nil
          (elfeed-protocol-greader--parse-entries host-url action callback content)))
      (run-hook-with-args 'elfeed-update-hooks host-url))
     ;; update ALL (read, unread & starred) after latest entry
     ((eq action 'update)
      (setq arg (or arg (format "&ot=%s" entry-mark)))
      (setq path-opt (concat path-all arg)))
     ;; update non-starred items before earliest non-starred entry
     ((eq action 'update-older)
      (setq arg (or arg (format "&nt=%s" entry-mark)))
      ;; increase limit for updating older entries. 2500 entries seems
      ;; to be equiv to 128mb limit for php-fpm
      (setq limit (min (* 2 limit) 2500))
      (setq path-opt (concat path-nostar arg)))
     ;; update starred entries
     ((eq action 'update-star)
      (setq arg (or arg (format "&ot=%s" entry-mark)))
      (setq path-opt (concat path-starred arg))))
    (unless (eq action 'init)
      (elfeed-protocol-greader-with-fetch host-url path-opt "GET" data
        (elfeed-protocol-greader--parse-entries host-url action callback content)
        (elfeed-log 'debug "%s continuation: %s" action continue)
        (when continue
          (elfeed-protocol-greader--do-update host-url action arg callback continue limit)))
      (run-hook-with-args 'elfeed-update-hooks host-url))))

(defun elfeed-protocol-greader-reinit (host-url)
  "Retry initial sync operation.
Will fetch unread, starred and latest entries from Tiny Tiny RSS.
HOST-URL is the host name of Tiny Tiny RSS server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let ((elfeed-protocol-greader-maxsize 100))
    (elfeed-protocol-greader--do-update host-url 'init)))

(defun elfeed-protocol-greader-update-since-timestamp (host-url &optional timestamp)
  "Update entries since special timestamp.
HOST-URL is the host name of greader server.  TIMESTAMP is the
seconds since 1970-01-01 00:00:00 UTC, the default timestamp just
point to 1 hours ago."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (unless timestamp
    (setq timestamp (round (- (time-to-seconds) (* 1 3600)))))
  (elfeed-protocol-greader-with-fetch host-url
    (concat elfeed-protocol-greader-api-path (elfeed-protocol-greader-api-client-params))
    "GET" nil
    (elfeed-protocol-greader--parse-feeds host-url)
    (elfeed-protocol-greader--do-update host-url 'update-since-time timestamp)))

(defun elfeed-protocol-greader-update-older (host-url &optional id)
  "Fetch older entries.
HOST-URL is the host name of Tiny Tiny RSS server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((proto-id (elfeed-protocol-greader-id host-url)))
    (elfeed-protocol-greader-fetch-prepare
      host-url
      (elfeed-protocol-greader--do-update host-url 'update-older id))))

(defun elfeed-protocol-greader-update-star (host-url)
  "Fetch starred entries.
For Tiny Tiny RSS only allow fetch Maximize 200 entries each time, so if your
own much more starred entries, just run this function to fetch them all.
HOST-URL is the host name of Tiny Tiny RSS server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((proto-id (elfeed-protocol-greader-id host-url)))
    (elfeed-protocol-greader-fetch-prepare
      host-url
      (elfeed-protocol-greader--do-update host-url 'update-star))))

(defun elfeed-protocol-greader-join-ids-to-str (separate ids)
  "Convert article ids to string format, for example from (1 2) to \"1,2\".
SEPARATE is the string to be insert between each id, IDS is the target id array."
  (string-trim-right (cl-loop for id in ids concat (format "%d%s" id separate)) separate))


(defmacro elfeed-protocol-greader-update-with-fetch (host-url path method data &rest body)
  "Just like `elfeed-with-fetch' but special for greader HTTP request.
HOST-URL is the host name of Tiny Tiny RSS server, METHOD could be
\"GET\" or \"POST\", DATA is in JSON string format.  Optional argument
BODY is the rest Lisp code after operation finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl)  ; capture current value in closure
          (url (concat (elfeed-protocol-greader--get-api-url host-url) ,elfeed-protocol-greader-api-path ,path))
          (headers (elfeed-protocol-greader--init-headers ,host-url))
          (no-auth-url (elfeed-protocol-no-auth-url url))
          (cb (lambda (status)
                (if (elfeed-is-status-error status use-curl)
                    (let ((print-escape-newlines t))
                      (elfeed-handle-http-error
                       no-auth-url
                       (if use-curl elfeed-curl-error-message status)))
                  (unless use-curl
                    (elfeed-move-to-first-empty-line)
                    (set-buffer-multibyte t))
                  ;; (when elfeed-protocol-log-trace
                  ;;   (elfeed-log 'debug "elfeed-protocol-greader: %s" (buffer-string)))
                  (elfeed-protocol-greader--parse-plain ,@body)
                  (unless use-curl
                    (kill-buffer))))))
     (unless elfeed-protocol-greader-sid
       (elfeed-protocol-greader-login host-url))
     (if use-curl
         (if ,data
             (elfeed-curl-enqueue no-auth-url cb :headers headers
                                  :method "POST" :data ,data)
           (elfeed-curl-enqueue no-auth-url cb :headers headers))
       (if ,data
           (let ((url-request-extra-headers headers)
                 (url-request-method "POST")
                 (url-request-data ,data))
             (url-retrieve no-auth-url cb () t t))
         (let ((url-request-extra-headers headers))
           (url-retrieve no-auth-url cb () t t))))))

(defun elfeed-protocol-greader--update-article (host-url ids category type mode)
  "Notify multiple entries to be read/unread/starred/unstarred.
HOST-URL is the host name of Tiny Tiny RSS server.  IDS is the
target entry ids.  FIELD could be 0, 1, 2, 3 which means starred,
published, unread, and article note.  MODE could be 0, 1, 2 which
means set to false, set to true and toggle."
  ;; ensure categories are updated
  (elfeed-protocol-greader--update-categories-list host-url)
  ;; ensure we have updated api token
  (elfeed-protocol-greader-get-api-token host-url)
  (let* ((token (format "T=%s" (car elfeed-protocol-greader-api-token)))
         (path (concat elfeed-protocol-greader-edit-tag-path
                       (elfeed-protocol-greader-api-client-params)))
         (tag (elfeed-protocol-greader--get-category-id host-url category))
         (mode (symbol-name mode))
         (method (format "&%s=%s" mode tag))
         (ids (concat "&i=" (elfeed-protocol-greader-join-ids-to-str "&i=" ids)))
         (data (concat token method ids)))
    (when ids
      (elfeed-protocol-greader-update-with-fetch
        host-url path "POST" data))))

(defun elfeed-protocol-greader-mark-read-multi (host-url ids)
  "Notify multiple entries to be read.
HOST-URL is the host name of Tiny Tiny RSS server.  IDS is the target entry
ids."
  (elfeed-log 'debug "elfeed-protocol-greader: mark read, ids: %s" ids)
  (elfeed-protocol-greader--update-article
   host-url ids "read" nil 'a))

(defun elfeed-protocol-greader-mark-unread-multi (host-url ids)
  "Notify multiple entries to be unread.
HOST-URL is the host name of Tiny Tiny RSS server.  IDS is the target entry
ids."
  (elfeed-log 'debug "elfeed-protocol-greader: mark unread, ids: %s" ids)
  (elfeed-protocol-greader--update-article
   host-url ids "read" nil 'r))

(defun elfeed-protocol-greader-mark-starred-multi (host-url ids)
  "Notify multiple entries to be starred.
HOST-URL is the host name of Tiny Tiny RSS server.  IDS is the target entry
ids."
  (elfeed-log 'debug "elfeed-protocol-greader: mark starred, ids: %s" ids)
  (elfeed-protocol-greader--update-article
   host-url ids "starred" nil 'a))

(defun elfeed-protocol-greader-mark-unstarred-multi (host-url ids)
  "Notify multiple entries to be unstarred.
HOST-URL is the host name of Tiny Tiny RSS server.  IDS is the target entry
ids."
  (elfeed-log 'debug "elfeed-protocol-greader: mark unstarred, ids: %s" ids)
  (elfeed-protocol-greader--update-article
   host-url ids "starred" nil 'r))

(defun elfeed-protocol-greader-sync-pending-ids (host-url)
  "Sync pending read/unread/starred/unstarred/published/unpublished entry states to Tiny Tiny RSS server.
HOST-URL is the host name of Tiny Tiny RSS server."
  (let* ((proto-id (elfeed-protocol-greader-id host-url))
         (pending-read-ids (elfeed-protocol-get-pending-ids proto-id :pending-read))
         (pending-unread-ids (elfeed-protocol-get-pending-ids proto-id :pending-unread))
         (pending-starred-ids (elfeed-protocol-get-pending-ids proto-id :pending-starred))
         (pending-unstarred-ids (elfeed-protocol-get-pending-ids proto-id :pending-unstarred)))
    (when pending-read-ids (elfeed-protocol-greader-mark-read-multi host-url pending-read-ids))
    (when pending-unread-ids (elfeed-protocol-greader-mark-unread-multi host-url pending-unread-ids))
    (when pending-starred-ids (elfeed-protocol-greader-mark-starred-multi host-url pending-starred-ids))
    (when pending-unstarred-ids (elfeed-protocol-greader-mark-unstarred-multi host-url pending-unstarred-ids))
    (elfeed-protocol-clean-pending-ids proto-id)))

(defun elfeed-protocol-greader-append-pending-ids (host-url entries tag action)
  "Sync unread starred and published tag states to Tiny Tiny RSS server.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the
target entry objects.  TAG is the action tag, for example unread,
`elfeed-protocol-greader-star-tag' and
`elfeed-protocol-greader-publish-tag', ACTION could be add or remove."
  (when entries
    (let* ((proto-id (elfeed-protocol-greader-id host-url))
           (ids (cl-loop for entry in entries collect
                         (when (elfeed-protocol-greader-entry-p entry)
                           (elfeed-meta entry :id)))))
      (cond
       ((eq action 'add)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-unread ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-read ids))
         ((eq tag elfeed-protocol-greader-star-tag)
          (elfeed-protocol-append-pending-ids proto-id :pending-starred ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-unstarred ids))))
       ((eq action 'remove)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-read ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-unread ids))
         ((eq tag elfeed-protocol-greader-star-tag)
          (elfeed-protocol-append-pending-ids proto-id :pending-unstarred ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-starred ids))))))))

(defun elfeed-protocol-greader-pre-tag (host-url entries &rest tags)
  "Sync unread, starred and published states before tags added.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the
target entry objects.  TAGS is the tags are adding now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-greader-append-pending-ids host-url entries-modified tag 'add)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-greader-sync-pending-ids host-url)))

(defun elfeed-protocol-greader-pre-untag (host-url entries &rest tags)
  "Sync unread, starred and published states before tags removed.
HOST-URL is the host name of Tiny Tiny RSS server.  ENTRIES is the
target entry objects.  TAGS is the tags are removing now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-greader-append-pending-ids host-url entries-modified tag 'remove)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-greader-sync-pending-ids host-url)))

(defun elfeed-protocol-greader-update-subfeed (host-url feed-url &optional callback)
  "Update entries under special sub feed in Tiny Tiny RSS.
HOST-URL is the host name of Tiny Tiny RSS server, FEED-URL is the
target sub feed url, if CALLBACK is not nil will call it with the
result entries as argument."
  (interactive)
  (let* ((feed-id (elfeed-protocol-greader--get-subfeed-id host-url feed-url)))
    (when feed-id
      (elfeed-protocol-greader-fetch-prepare
        host-url
        (elfeed-protocol-greader--do-update host-url 'update-subfeed feed-id callback)))))

(defun elfeed-protocol-greader-update (host-or-subfeed-url &optional callback)
  "Tiny Tiny RSS protocol updater.
HOST-OR-SUBFEED-URL could be the host name of Tiny Tiny RSS server,
and user field authentication info is always required so could find
the related protocol feed id correctly, for example
\"https://user@myhost.com\".  And HOST-OR-SUBFEED-URL also could be the
sub feed url, too, for example
\"https://user@myhost.com::https://subfeed.com\".  If first time run,
it will initial sync operation, or will only fetch the updated entries
since last modified. if CALLBACK is not nil will call it with the
result entries as argument"
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((host-url (elfeed-protocol-host-url host-or-subfeed-url))
         (feed-url (elfeed-protocol-subfeed-url host-or-subfeed-url))
         (proto-id (elfeed-protocol-greader-id host-url)))
    (elfeed-protocol-add-unknown-feed proto-id) ; add unknown feed for fallback
    (elfeed-protocol-greader-sync-pending-ids host-url)
    (if feed-url (elfeed-protocol-greader-update-subfeed host-url feed-url callback)
      (let* ((proto-id (elfeed-protocol-greader-id host-url))
             (last-entry-id (elfeed-protocol-get-last-entry-id proto-id)))
        (elfeed-protocol-greader-fetch-prepare
          host-url
          (if (>= last-entry-id 0)
              (elfeed-protocol-greader--do-update host-url 'update nil callback)
            (elfeed-protocol-greader--do-update host-url 'init nil callback)))))))

(provide 'elfeed-protocol-greader)

;;; elfeed-protocol-greader.el ends here
