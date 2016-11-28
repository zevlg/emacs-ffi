;;; ffi-magic.el --- FFI to libmagic

;; Copyright (C) 2016 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Tue Nov 29 00:48:05 2016
;; Keywords: ffi

;; this program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; this program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'ffi-module)
(require 'ffi)

(defvar magic--shared-cookie nil
  "Shared context with preloaded magic file, to speed up things.")

(define-ffi-library libmagic "libmagic")

(define-ffi-function magic--version0 "magic_version" :int nil libmagic)
(defun magic--version ()
  (/ (magic--version0) 100.0))

(define-ffi-function magic--open "magic_open" :pointer (:int) libmagic)
(define-ffi-function magic--close "magic_close" :void (:pointer) libmagic)

(define-ffi-function magic--load-c "magic_load"
  :int (:pointer :pointer) libmagic)
(defun magic--load (mt &optional file)
  (if file
      (with-ffi-string (filecstr file)
        (magic--load-c mt filecstr))
    (magic--load-c mt (ffi-null-pointer))))

(define-ffi-function magic--file-c "magic_file"
  :pointer (:pointer :pointer) libmagic)
(defun magic--file (mt file)
  (with-ffi-string (filecstr file)
    (let ((ret (magic--file-c mt filecstr)))
      (unless (ffi-pointer-null-p ret)
        (ffi-get-c-string ret)))))

(define-ffi-function magic--buffer-c "magic_buffer"
  :pointer (:pointer :pointer :size_t) libmagic)
(defun magic--buffer (mt content)
  (with-ffi-string (contcstr content)
    (let ((ret (magic--buffer-c mt contcstr (length content))))
      (unless (ffi-pointer-null-p ret)
        (ffi-get-c-string ret)))))

(define-ffi-function magic--error0 "magic_error" :pointer (:pointer) libmagic)
(defun magic--error (mt)
  (let ((ret (magic--error0 mt)))
    (unless (ffi-pointer-null-p ret)
      (ffi-get-c-string ret))))

(defun magic--ensure-shared-cookie ()
  (unless magic--shared-cookie
    (setq magic--shared-cookie (magic--open 0))
    (magic--load magic--shared-cookie "/usr/share/misc/magic.mgc")
    (magic--load magic--shared-cookie)))
  
(defun magic-file-type (file)
  "Return as a string what type FILE is using libmagic."
  (interactive "fFile name: ")
  (magic--ensure-shared-cookie)

  (let ((ftype (magic--file magic--shared-cookie (expand-file-name file))))
    (if (called-interactively-p 'interactive)
	(message ftype)
      ftype)))

(defun magic-buffer-type (buffer)
  "Return BUFFER's file type."
  (interactive (list (current-buffer)))
  (magic--ensure-shared-cookie)

  (with-current-buffer buffer
    (let ((ftype (magic--buffer magic--shared-cookie (buffer-string))))
      (if (called-interactively-p 'interactive)
          (message ftype)
        ftype))))
  
(provide 'ffi-magic)

;;; ffi-magic.el ends here
