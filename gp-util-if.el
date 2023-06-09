;;; gp-util-if.el --- Google Patent utilities

;;  Auther:  Katsuhito Ishida <katsuhito.ishida@gmail.com>
;;  Version: 0.1
;;; Commentary:
;;  This package provides a set of useful APIs for patent analysis.
;;  
;; 
;; -*- coding: utf-8 -*-
;; (setq patent-number "JP2003085659A")
;;
(defcustom db-path "/var/db/patent/"
  "full path to the directory where rawfiles will be stored."
  :type 'string
  )

(defcustom rawfile-name "raw.el"
  "filename for the raw files."
  :type 'string
  )

(defcustom image-aliases-name "image-aliases.org"
  "filename for the image-aliases.")

(defcustom style-name "style.org"
  "filename for the stylesheet.")

(defcustom gp-url "https://patents.google.com/patent/"
  "URL of Google Patents")

(defcustom local-image-store "./figs"
  "path to local store for image file")

(defcustom db-image-store "./images"
  "path to db store for image file")

(defcustom wget-program "/usr/bin/wget"
  "path for wget")

(defcustom copy-program "/bin/cp"
  "path for cp")

(defun gp-full-path-to-rawfile-store (PATENT-NUMBER)
  "Return the full path to a rawfile store which stores rawfile for the patent document identified by PATENT-NUMBER."
  (message "gp-full-path-to-rawfile-store")
  (concat db-path PATENT-NUMBER "/"))

(defun gp-save-patent-as-dom (PATENT-NUMBER)
  "Save a patent document identifiedby PATENT-NUMBER as a dom"
  (message "gp-save-patent-as-dom")
  (setq local-file (concat (gp-full-path-to-rawfile-store PATENT-NUMBER) rawfile-name))
  (with-temp-buffer
    (insert (pp (gp-get-patent-as-dom PATENT-NUMBER)))
    (write-file local-file)))

(defun gp-get-patent-as-dom (PATENT-NUMBER)
  (message "gp-get-patent-as-dom")  
  (gp-get-document-as-dom (concat gp-url PATENT-NUMBER)))

(defun gp-get-document-as-dom (url)
  (message "gp-get-document-as-dom")
  (gp-convert-html-to-dom (gp-get-document url)))

(defun gp-get-document (url)
  "Retrieve patent publication from Google patents"
  (message "gp-get-document %s" url)
  (with-current-buffer (url-retrieve-synchronously url)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gp-convert-html-to-dom (STRING)
  ;; Convert a html string to a dom.
  (message "gp-convert-html-to-dom")
  (with-temp-buffer 
    (insert STRING)
;;    (insert "<html><title>test</title></html>")
    (libxml-parse-html-region (point-min) (point-max))))


(provide 'gp-util-if)
