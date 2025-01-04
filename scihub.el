;;; scihub.el --- Sci-Hub integration                 -*- lexical-binding: t -*-

;; Copyright (C) 2018 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/scihub.el
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
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

;; Sci-Hub integration.  See: <URL:https://en.wikipedia.org/wiki/Sci-Hub>.

;;; Code:
(eval-when-compile
  (require 'subr-x)
  (defvar url-request-data)
  (defvar url-request-method)
  (defvar url-http-end-of-headers)
  (defvar url-request-extra-headers))

(require 'dom)

(defgroup scihub nil
  "Sci-Hub integration."
  :prefix "scihub-"
  :group 'applications)

(defcustom scihub-homepage "https://sci-hub.ru/"
  "Sci-hub homepage to be used to fetch.

Note:
    The selected mirror should NOT be behind Cloudflare or similar
    service, otherwise we will be NOT able to download the paper.

See available Sci-hub mirrors on `https://sci-hub.ru/'."
  :type '(choice
          (const :tag "sci-hub.ru"  "https://sci-hub.ru/")
          (const :tag "sci-hub.se"  "https://sci-hub.se/")
          (const :tag "sci-hub.st"  "https://sci-hub.st/")
          (const :tag "sci-hub.box" "https://sci-hub.box/")
          (const :tag "sci-hub.red" "https://sci-hub.red/")
          string)
  :group 'scihub)

(defcustom scihub-download-directory (expand-file-name "~/papers/")
  "Directory where the papers will be downloaded."
  :type '(directory :must-match t)
  :group 'scihub)

(defcustom scihub-open-after-download t
  "Whether to open pdf after download."
  :type 'boolean
  :group 'scihub)

(defcustom scihub-user-agent "Mozilla/5.0 (iPhone; CPU iPhone OS 11_3_1 like Mac OS X) AppleWebKit/603.1.30 (KHTML, like Gecko) Version/10.0 Mobile/14E304 Safari/602.1"
  "User agent string to use for `scihub'."
  :type 'string
  :group 'scihub)

(make-obsolete 'scihub-pdf-p nil "0.2")
(make-obsolete 'scihub-homepage nil "0.2")
(make-obsolete 'scihub-fetch-domains-lovescihub nil "0.2")
(make-obsolete 'scihub-fetch-domains-sci-hub.pub nil "0.2")
(make-obsolete-variable 'scihub-fetch-domain nil "0.2")

(define-error 'scihub-error "Unknown scihub error")
(define-error 'scihub-not-found "Article not found" 'scihub-error)

(defun scihub-url-filename (url)
  "Read filename from an URL."
  (let ((name (url-file-nondirectory (substring url 0 (string-match-p "#" url)))))
    (if (or (string-blank-p name) (file-exists-p (expand-file-name name scihub-download-directory)))
        (read-file-name "Save to file: " scihub-download-directory)
      (expand-file-name name scihub-download-directory))))

(defun scihub-fetch-callback (status homepage query filename)
  "Callback for Sci-Hub, check whether url STATUS erred.

HOMEPAGE is the Sci-Hub mirror used.  QUERY is the initial query
passed to `scihub', and FILENAME is the path where the PDF will
be downloaded."
  (pcase (plist-get status :error)
    (`nil
     (let* ((dom (with-current-buffer (current-buffer)
                   (goto-char (1+ url-http-end-of-headers))
                   (libxml-parse-html-region (point) (point-max))))
            (url (dom-attr (dom-by-tag dom 'embed) 'src)))
       (if (not url)
           (signal 'scihub-not-found (list query))
         (make-directory scihub-download-directory t)
         (let* ((pdf-url (if (string-prefix-p "//" url) (concat "https:" url) url))
                (pdf-file (or filename (scihub-url-filename pdf-url))))
           (message "Saving %s..." pdf-file)
           (url-copy-file pdf-url pdf-file)
           (when scihub-open-after-download
             (find-file-existing pdf-file))))))
    (`(error http 403)
     (message "The Sci-Hub mirror `%s' seems be DDoS guarded. Please, try again in a few minutes." homepage))
    (other
     (message "Fetching paper from Sci-Hub failed: %s" (error-message-string other)))))

;;;###autoload
(defun scihub (query &optional filename)
  "Download a paper from Sci-Hub with QUERY to FILENAME."
  (interactive (list (read-string "Enter URL, PMID / DOI or search string: ")
                     (and current-prefix-arg (read-file-name "Save paper PDF to file: " scihub-download-directory))))
  (let ((url-request-method "POST")
        (url-request-data (url-build-query-string `(("request" ,query))))
        (url-mime-language-string "en-US,en;q=0.5")
        (url-request-extra-headers `(("User-Agent"   . ,scihub-user-agent)
                                     ("Content-Type" . "application/x-www-form-urlencoded"))))
    (url-retrieve scihub-homepage #'scihub-fetch-callback (list scihub-homepage query filename))))

(provide 'scihub)
;;; scihub.el ends here
