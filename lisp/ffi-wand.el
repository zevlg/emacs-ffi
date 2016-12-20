;;; ffi-wand.el --- FFI to libMagickWand

;; Copyright (C) 2016 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Nov 30 00:40:03 2016
;; Keywords: ffi, multimedia

;; ffi-wand.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ffi-wand.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ffi-wand.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Check Emacs is built with imagemagick support:
;;
;;    (memq 'imagemagick image-types) ==> non-nil
;;

;;; Code:
(require 'cl-macs)
(require 'ffi)

(define-ffi-library libmagickwand "libMagickWand")

(define-ffi-array MagickArray4096 :char 4096)

(defvar MagickBooleanType :long)

(define-ffi-struct MagickWand-private
  (id :type :ulong)
  (name :type MagickArray4096)
  (exception :type :pointer)
  (image-info :type :pointer)           ; ImageInfo*
  (quantize-info :type :pointer)
  (images :type :pointer)
  (active :type MagickBooleanType)
  (pend :type MagickBooleanType)
  (debug :type MagickBooleanType)
  (signature :type :ulong))

(define-ffi-struct PointInfo
  (x :type :double)
  (y :type :double))

(defvar MagickOrientationType :int)
(defconst MagickOrientationUndefined 0)
(defconst MagickOrientationTopLeft 1)
(defconst MagickOrientationTopRight 2)
(defconst MagickOrientationBottomRight 3)
(defconst MagickOrientationBottomLeft 4)
(defconst MagickOrientationLeftTop 5)
(defconst MagickOrientationRightTop 6)
(defconst MagickOrientationRightBottom 7)
(defconst MagickOrientationLeftBottom 8)

(define-ffi-function Wand:MagickWandGenesis "MagickWandGenesis" :void
  nil libmagickwand)

;; (define-ffi-function Wand:MagickWandTerminus "MagickWandTerminus" :void
;;   nil libmagickwand)

;;{{{  `-- Wand:version

(define-ffi-function Wand:GetMagickVersion "GetMagickVersion" :pointer
  (:pointer) libmagickwand)

(defun Wand:version ()
  "Return Image Magick version string."
  (with-ffi-temporary (n :ulong)
    (let ((ret (Wand:GetMagickVersion n)))
      (unless (ffi-pointer-null-p ret)
        (ffi-get-c-string ret)))))

;;}}}

;;{{{  `-- MagickWand operations

(define-ffi-function Wand:RelinquishMemory "MagickRelinquishMemory" :pointer
  (:pointer) libmagickwand)

(defvar MagickWand :pointer)

(defvar wand--free-pool nil
  "Pool of ready to use imagemagick wands")

(define-ffi-function Wand:acquire-id "AcquireWandId" :size_t
  nil libmagickwand)

(define-ffi-function Wand:relinquish-id "RelinquishWandId" :void
  (:size_t) libmagickwand)

;; Return a newly allocated MagickWand.
(define-ffi-function Wand:make-wand "NewMagickWand" MagickWand
  nil libmagickwand)

;; Clear all resources associated with the WAND.
;; This does not free the memory, i.e. @var{wand} can furtherly be used
;; as a context, see `Wand:delete-wand'."
(define-ffi-function Wand:clear-wand "ClearMagickWand" :void
  (MagickWand) libmagickwand)

;; Return a cloned copy of WAND.
(define-ffi-function Wand:copy-wand "CloneMagickWand" MagickWand
  (MagickWand) libmagickwand)

;; Gets the image at the current image index.
(define-ffi-function Wand:get-image "MagickGetImage" MagickWand
  (MagickWand) libmagickwand)

;; Delete the WAND.
;; This frees all resources associated with the WAND.
;; WARNING: Do not use WAND after calling this function!
(define-ffi-function Wand:destroy-wand "DestroyMagickWand" :void
  (MagickWand) libmagickwand)

(defun Wand:delete-wand (wand)
  ;; Workaround some ugly bug, causing
  ;; magick/semaphore.c:290: LockSemaphoreInfo: Assertion `semaphore_info != (SemaphoreInfo *) ((void *)0)' failed
  ;;  - make sure wand_semaphore is ok
  (Wand:acquire-id)
  (Wand:destroy-wand wand))

;; Return non-nil if WAND is a magick wand, nil otherwise.
(define-ffi-function Wand:wandp "IsMagickWand" MagickBooleanType
  (MagickWand) libmagickwand)

(defmacro Wand-with-wand (wand &rest forms)
  "With allocated WAND do FORMS."
  `(let ((,wand (Wand:make-wand)))
     (unwind-protect
         (progn ,@forms)
       (Wand:delete-wand ,wand))))
(put 'Wand-with-wand 'lisp-indent-function 'defun)

;; MagickIdentifyImage() identifies an image by printing its
;; attributes to the file. Attributes include the image width, height,
;; size, and others.
(define-ffi-function Wand:MagickIdentifyImage "MagickIdentifyImage" :pointer
  (MagickWand) libmagickwand)

(defun Wand:identify-image (wand)
  "Return info about the image stored in WAND."
  (let ((ii (Wand:MagickIdentifyImage wand)))
    (unless (ffi-pointer-null-p ii)
      (unwind-protect
          (ffi-get-c-string ii)
        (Wand:RelinquishMemory ii)))))

(define-ffi-function Wand:MagickReadImage "MagickReadImage" MagickBooleanType
  (MagickWand :pointer) libmagickwand)

(defun Wand:read-image (wand file)
  "Read FILE and associate it with WAND."
  (let ((fname (expand-file-name file)))
    ;; simple error catchers
    (unless (file-readable-p fname)
      (error "File unreadable %s" fname))
    (when (zerop (Wand:wandp wand))
      (wrong-type-argument 'Wand:wandp wand))

    (with-ffi-string (fncstr fname)
      (Wand:MagickReadImage wand fncstr))))

(defun Wand:read-image-data (wand data)
  (with-ffi-string (dtcstr data)
    (Wand:MagickReadImage wand dtcstr)))

(define-ffi-function Wand:MagickWriteImage "MagickWriteImage" MagickBooleanType
  (MagickWand :pointer) libmagickwand)

(defun Wand:write-image (wand file)
  "Write the image associated with WAND to FILE."
  (let ((fname (expand-file-name file)))
    ;; simple error catchers
    (unless (file-writable-p fname)
      (error "File unwritable %s" fname))
    (when (zerop (Wand:wandp wand))
      (wrong-type-argument 'Wand:wandp wand))

    (with-ffi-string (fncstr fname)
      (Wand:MagickWriteImage wand fncstr))))

(define-ffi-function Wand:GetImageBlob "MagickGetImageBlob" :pointer
  (MagickWand :pointer) libmagickwand)

;; Use `Wand:RelinquishMemory' when done with blob
(defun Wand:image-blob (wand)
  "Return WAND's direct image data according to format.
Use \(setf \(Wand:image-format w\) FMT\) to set format."
  (with-ffi-temporary (len :uint)
    (let ((data (Wand:GetImageBlob wand len))
          (llen (ffi--mem-ref len :uint)))
      (cons llen data))))

;; MagickResetImagePage() resets the Wand page canvas and position.
(define-ffi-function Wand:MagickResetImagePage "MagickResetImagePage"
  MagickBooleanType
  (MagickWand :pointer) libmagickwand)

(defun Wand:reset-image-page (wand &optional geometry)
  "Reset the WAND page canvas and position to GEOMETRY.
If GEOMETRY is ommited then 0x0+0+0 is used."
  (with-ffi-string (cgeom (or geometry "0x0+0+0"))
    (Wand:MagickResetImagePage wand cgeom)))

;; Magick Properties
(define-ffi-function Wand:GetMagickProperty "GetMagickProperty" :pointer
  (:pointer :pointer :pointer) libmagickwand)

(defun Wand:get-magick-property (wand prop)
  "From WAND get magick property PROP.
PROP can be one of: `base', `channels', `colorspace', `depth',
`directory', `extension', `height', `input', `magick', `name',
`page', `size', `width', `xresolution', `yresolution'."
  (when (member prop '("group" "kurtosis" "max" "mean"
                       "min" "output" "scene" "skewness"
                       "standard-deviation" "standard_deviation"
                       "unique" "zero"))
    (error "Unsupported magick property" prop))
  (with-ffi-string (cprop prop)
    (let ((ret (Wand:GetMagickProperty
                (ffi-null-pointer) (MagickWand-private-images wand)
                cprop)))
      (unless (ffi-pointer-null-p ret)
        (ffi-get-c-string ret)))))

(defun Wand:image-orig-width (wand)
  "Return original width of the image associated with WAND."
  (string-to-number (Wand:get-magick-property wand "width")))

(defun Wand:image-orig-height (wand)
  "Return original height of the image associated with WAND."
  (string-to-number (Wand:get-magick-property wand "height")))

;;}}}
;;{{{  `-- Image profiles

(defun Wand-fetch-relinquish-strings (strs slen)
  "Fetch strings from strings array STRS of length SLEN."
  (unless (ffi-pointer-null-p strs)
    (unwind-protect
        (loop for off from 0 below slen
              collect (ffi-get-c-string (ffi-aref strs :pointer off)))
      (Wand:RelinquishMemory strs))))

;; Profiles
(define-ffi-function Wand:MagickGetImageProfiles "MagickGetImageProfiles" :pointer
  (MagickWand :pointer :pointer) libmagickwand)

(defun Wand:image-profiles (wand pattern)
  "Get list of WAND's profiles matching PATTERN."
  (with-ffi-temporary (clen :ulong)
    (with-ffi-string (cptr pattern)
      (let ((profs (Wand:MagickGetImageProfiles wand cptr clen)))
        (Wand-fetch-relinquish-strings profs (ffi--mem-ref clen :ulong))))))

(define-ffi-function Wand:MagickGetImageProfile "MagickGetImageProfile" :pointer
  (MagickWand :pointer :pointer) libmagickwand)

(define-ffi-function Wand:MagickSetImageProfile "MagickSetImageProfile"
  MagickBooleanType
  (MagickWand :pointer :pointer :uint) libmagickwand)

(define-ffi-function Wand:MagickRemoveImageProfile "MagickRemoveImageProfile" :pointer
  (MagickWand :pointer :pointer) libmagickwand)

(defconst wand--iptc-names-table
  '((120 . caption) (25 . keyword)))

(defun Wand:image-profile-iptc (wand)
  "Fetch IPTC profile from WAND in lisp-friendly form."
  (with-ffi-temporary (cplen :uint)
    (with-ffi-string (ciptc "iptc")
      (let ((prof (Wand:MagickGetImageProfile wand ciptc cplen))
            (rlen (ffi--mem-ref cplen :uint)) (coff 0) (rv nil))
    (unless (ffi-pointer-null-p prof)
      (unwind-protect
          (cl-flet ((getbyte () (prog1
                                    (ffi-aref prof :char coff)
                                  (incf coff))))
            ;; 28 - must start any iptc header
            (while (and (< coff rlen) (= (getbyte) 28))
              (let* ((itype (getbyte)) (idset (getbyte))
                     (l1 (getbyte)) (l2 (getbyte))
                     (ln (logior (ash l1 8) l2)))
                (when (= itype 2)
                  ;; only string type supported
                  (push (cons (cdr (assq idset wand--iptc-names-table))
                              (ffi-get-c-data (ffi-pointer+ prof coff) ln))
                        rv))
                (incf coff ln)))
            rv)
        (Wand:RelinquishMemory prof)))))))

(defun Wand:image-save-iptc-profile (w iptc)
  "For wand W store IPTC profile."
  ;; TODO
  (let ((oolen (reduce #'(lambda (e1 e2)
                           (+ e1 5 (length (cdr e2))))
                       iptc :initial-value 0)))
    (when (> oolen 0)
      (let ((prof (make-ffi-object 'pointer oolen))
            (coff 0))
        (cl-flet ((savebyte (byte)
                    (prog1
                        (ffi-store prof coff 'byte byte)
                      (incf coff))))
          (loop for ipel in iptc do
            (savebyte 28) (savebyte 2)
            (savebyte (car (find (car ipel)
                                 wand--iptc-names-table :key #'cdr)))
            (let* ((ln (length (cdr ipel)))
                   (l1 (ash (logand ln #xff00) -8))
                   (l2 (logand ln #x00ff)))
              (savebyte l1) (savebyte l2)
              (ffi-store prof coff 'c-string (cdr ipel))
              (incf coff ln))))
        (Wand:MagickSetImageProfile w "iptc" prof oolen)))
    ))

(defun Wand:image-remove-profile (wand profname)
  (with-ffi-temporary (cplen :uint)
    (with-ffi-string (cpfname profname)
      (Wand:MagickRemoveImageProfile wand cpfname cplen))))

;;}}}
;;{{{  `-- Image properties

(define-ffi-function Wand:MagickGetImageProperties "MagickGetImageProperties" :pointer
  (MagickWand :pointer :pointer) libmagickwand)

(defun Wand:image-properties (w pattern)
  "Return list of image properties that match PATTERN."
  (with-ffi-temporary (clen :ulong)
    (with-ffi-string (cptr pattern)
      (let ((props (Wand:MagickGetImageProperties w cptr clen)))
        (Wand-fetch-relinquish-strings props (ffi--mem-ref clen :ulong))))))

(define-ffi-function Wand:MagickGetImageProperty "MagickGetImageProperty" :pointer
  (MagickWand :pointer) libmagickwand)

(define-ffi-function Wand:MagickSetImageProperty "MagickSetImageProperty"
  MagickBooleanType
  (MagickWand :pointer :pointer) libmagickwand)

(define-ffi-function Wand:MagickDeleteImageProperty "MagickDeleteImageProperty"
  MagickBooleanType
  (MagickWand :pointer) libmagickwand)

(defun Wand:image-property (w property)
  "Return value for PROPERTY.
Use \(setf \(Wand:image-property w prop\) VAL\) to set property."
  (with-ffi-string (cprop property)
    (let ((pv (Wand:MagickGetImageProperty w cprop)))
      (unless (ffi-pointer-null-p pv)
        (unwind-protect
            (ffi-get-c-string pv)
          (Wand:RelinquishMemory pv))))))

(defsetf Wand:image-property (w prop) (val)
  (let ((vsym (cl-gensym "vsym-"))
        (prop-c (cl-gensym))
        (vsym-c (cl-gensym)))
    `(let ((,vsym ,val))
       (with-ffi-string (,prop-c ,prop)
         (if ,vsym
             (with-ffi-string (,vsym-c ,vsym)
               (Wand:MagickSetImageProperty ,w ,prop-c ,vsym-c))
           (Wand:MagickDeleteImageProperty ,w ,prop-c))))))

(define-ffi-function Wand:MagickGetQuantumRange "MagickGetQuantumRange" :pointer
  (:pointer) libmagickwand)
(defun Wand:quantum-range ()
  (with-ffi-temporary (qr :ulong)
    (Wand:MagickGetQuantumRange qr)
    (ffi--mem-ref qr :ulong)))

;; Very simple properties editor
(defun wand-prop-editor ()
  "Run properties editor."
  (interactive)
  (let* ((iw image-wand)
         (props (cl-remove-if-not
                 #'(lambda (prop)
                     (string-match wand-properties-pattern prop))
                 (Wand:image-properties iw ""))))
    (save-window-excursion
      (with-temp-buffer
        (save-excursion
          (mapc #'(lambda (prop)
                    (insert prop ": " (Wand:image-property iw prop) "\n"))
                props))
        (pop-to-buffer (current-buffer))
        (text-mode)
        (message "Press %s when done, or %s to cancel"
                 (key-description
                  (car (where-is-internal 'exit-recursive-edit)))
                 (key-description
                  (car (where-is-internal 'abort-recursive-edit))))
        (recursive-edit)

        ;; User pressed C-M-c, parse buffer and store new props
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((st (buffer-substring (point-at-bol) (point-at-eol)))
                 (pv (split-string st ": ")))
            (setf (Wand:image-property iw (first pv)) (second pv)))
          (forward-line 1))))))

;;}}}

;;{{{  `-- Image size/orientation/other stuff

(define-ffi-function Wand:MagickGetSize "MagickGetSize" MagickBooleanType
  (MagickWand :pointer :pointer) libmagickwand)
(define-ffi-function Wand:MagickSetSize "MagickSetSize" MagickBooleanType
  (MagickWand :ulong :ulong) libmagickwand)

(defun Wand:image-size (wand)
  "Return size of the image, associated with WAND."
  (with-ffi-temporaries ((w :ulong) (h :ulong))
    (unless (zerop (Wand:MagickGetSize wand w h))
      (cons (ffi--mem-ref w :ulong) (ffi--mem-ref h :ulong)))))
(defsetf Wand:image-size (wand) (size)
  `(Wand:MagickSetSize ,wand (car ,size) (cdr ,size)))

(define-ffi-function Wand:image-height "MagickGetImageHeight" :ulong
  (MagickWand) libmagickwand)
(define-ffi-function Wand:image-width "MagickGetImageWidth" :ulong
  (MagickWand) libmagickwand)

(define-ffi-function Wand:GetImageOrientation "MagickGetImageOrientation"
  MagickOrientationType
  (MagickWand) libmagickwand)

(define-ffi-function Wand:SetImageOrientation "MagickSetImageOrientation"
  MagickBooleanType
  (MagickWand MagickOrientationType) libmagickwand)

(defun Wand:image-orientation (w)
  "Return orientation for the image hold by W.
Use \(setf \(Wand:image-orientation w\) orient\) to set new one."
  (Wand:GetImageOrientation w))

(defsetf Wand:image-orientation (w) (orient)
  `(Wand:SetImageOrientation ,w ,orient))

(defvar MagickEndianType :int)
(defconst MagickEndianUndefined 0)
(defconst MagickEndianLSB 1)
(defconst MagickEndianMSB 2)

(define-ffi-function Wand:GetImageEndian "MagickGetImageEndian"
  MagickEndianType
  (MagickWand) libmagickwand)

(define-ffi-function Wand:SetImageEndian "MagickSetImageEndian"
  MagickBooleanType
  (MagickWand MagickEndianType) libmagickwand)

(defun Wand:image-endian (w)
  "Return endian for the image hold by W.
Use \(setf \(Wand:image-endian w\) endian\) to set new one."
  (Wand:GetImageEndian w))

(defsetf Wand:image-endian (w) (endian)
  `(Wand:SetImageEndian ,w ,endian))

;;}}}

;;{{{  `-- Image format operations

(define-ffi-function Wand:MagicGetFormat "MagickGetFormat" :pointer
  (MagickWand) libmagickwand)
(define-ffi-function Wand:MagickSetFormat "MagickSetFormat" MagickBooleanType
  (MagickWand :pointer) libmagickwand)

(defun Wand:wand-format (w)
  (let ((ret (Wand:MagicGetFormat w)))
    (unless (ffi-pointer-null-p ret)
      (ffi-get-c-string ret))))

(defsetf Wand:wand-format (w) (nfmt)
  (let ((nfmtsym (cl-gensym)))
  `(with-ffi-string (,nfmtsym ,nfmt)
     (Wand:MagickSetFormat ,w ,nfmtsym))))

(define-ffi-function Wand:GetImageFormat "MagickGetImageFormat" :pointer
  (MagickWand) libmagickwand)

(define-ffi-function Wand:SetImageFormat "MagickSetImageFormat" MagickBooleanType
  (MagickWand :pointer) libmagickwand)

(defun Wand:image-format (w)
  "Return format for the image hold by W.
Use \(setf \(Wand:image-format w\) FMT\) to set new one."
  (let ((ret (Wand:GetImageFormat w)))
    (unless (ffi-pointer-null-p ret)
      (ffi-get-c-string ret))))

(defsetf Wand:image-format (w) (fmt)
  (let ((nfmtsym (cl-gensym)))
    `(with-ffi-string (,nfmtsym ,fmt)
       (Wand:SetImageFormat ,w ,nfmtsym))))

;;}}}

;;{{{  `-- Images list operations

(define-ffi-function Wand:images-num "MagickGetNumberImages" :ulong
  (MagickWand) libmagickwand)

(define-ffi-function Wand:has-next-image "MagickHasNextImage" MagickBooleanType
  (MagickWand) libmagickwand)

(define-ffi-function Wand:next-image "MagickNextImage" MagickBooleanType
  (MagickWand) libmagickwand)

(define-ffi-function Wand:has-prev-image "MagickHasPreviousImage" MagickBooleanType
  (MagickWand) libmagickwand)

(define-ffi-function Wand:prev-image "MagickPreviousImage" MagickBooleanType
  (MagickWand) libmagickwand)

(define-ffi-function Wand:iterator-index "MagickGetIteratorIndex" :long
  (MagickWand) libmagickwand)

(define-ffi-function Wand:MagickSetIteratorIndex "MagickSetIteratorIndex"
  MagickBooleanType
  (MagickWand :long) libmagickwand)

(defsetf Wand:iterator-index (w) (idx)
  `(Wand:MagickSetIteratorIndex ,w ,idx))

(define-ffi-function Wand:set-first-iterator "MagickSetFirstIterator" :void
  (MagickWand) libmagickwand)

(define-ffi-function Wand:set-last-iterator "MagickSetLastIterator" :void
  (MagickWand) libmagickwand)

;;}}}

;;{{{  `-- PixelWand operations

(defvar PixelWand :pointer)

(define-ffi-function Wand:NewPixelWand "NewPixelWand" PixelWand
  nil libmagickwand)
(define-ffi-function Wand:DestroyPixelWand "DestroyPixelWand" PixelWand
  (PixelWand) libmagickwand)

(defmacro Wand-with-pixel-wand (pw &rest forms)
  "With allocated pixel wand PW do FORMS."
  `(let ((,pw (Wand:NewPixelWand)))
     (unwind-protect
         (progn ,@forms)
       (Wand:DestroyPixelWand ,pw))))
(put 'Wand-with-pixel-wand 'lisp-indent-function 'defun)

(define-ffi-function Wand:pixel-red "PixelGetRed" :double
  (PixelWand) libmagickwand)
(define-ffi-function Wand:pixel-green "PixelGetGreen" :double
  (PixelWand) libmagickwand)
(define-ffi-function Wand:pixel-blue "PixelGetBlue" :double
  (PixelWand) libmagickwand)

(define-ffi-function Wand:PixelSetRed "PixelSetRed" :void
  (PixelWand :double) libmagickwand)
(define-ffi-function Wand:PixelSetGreen "PixelSetGreen" :void
  (PixelWand :double) libmagickwand)
(define-ffi-function Wand:PixelSetBlue "PixelSetBlue" :void
  (PixelWand :double) libmagickwand)

(defsetf Wand:pixel-red (pw) (r)
  `(Wand:PixelSetRed ,pw ,r))
(defsetf Wand:pixel-green (pw) (g)
  `(Wand:PixelSetGreen ,pw ,g))
(defsetf Wand:pixel-blue (pw) (b)
  `(Wand:PixelSetBlue ,pw ,b))

(defun Wand:pixel-rgb-components (pw)
  "Return RGB components for pixel wand PW."
  (mapcar #'(lambda (c) (int (* (funcall c pw) 65535.0)))
          '(Wand:pixel-red Wand:pixel-green Wand:pixel-blue)))

(defsetf Wand:pixel-rgb-components (pw) (rgb)
  "For pixel wand PW set RGB components."
  `(mapcar* #'(lambda (sf c) (funcall sf ,pw (/ c 65535.0)))
            '(Wand:PixelSetRed Wand:PixelSetGreen Wand:PixelSetBlue)
            ,rgb))

;; PixelGetColorAsString() returns the color of the pixel wand as a
;; string.
(define-ffi-function Wand:PixelGetColorAsString "PixelGetColorAsString" :pointer
  (PixelWand) libmagickwand)

(defun Wand:pixel-color (pw)
  (let ((pcs (Wand:PixelGetColorAsString pw)))
    (unless (ffi-pointer-null-p pcs)
      (ffi-get-c-string pcs))))

;; PixelSetColor() sets the color of the pixel wand with a string
;; (e.g. "blue", "#0000ff", "rgb(0,0,255)", "cmyk(100,100,100,10)",
;; etc.).
(define-ffi-function Wand:PixelSetColor "PixelSetColor" MagickBooleanType
  (PixelWand :pointer) libmagickwand)

(defsetf Wand:pixel-color (pw) (color)
  (let ((colcsym (cl-gensym)))
    `(with-ffi-string (,colcsym ,color)
       (Wand:PixelSetColor ,pw ,colcsym))))

;; PixelGetAlpha() returns the normalized alpha color of the pixel
;; wand.
(define-ffi-function Wand:pixel-alpha "PixelGetAlpha" :double
  (PixelWand) libmagickwand)

;; PixelSetAlpha() sets the normalized alpha color of the pixel wand.
;; The level of transparency: 1.0 is fully opaque and 0.0 is fully
;; transparent.
(define-ffi-function Wand:PixelSetAlpha "PixelSetAlpha" :void
  (PixelWand :double) libmagickwand)

(defsetf Wand:pixel-alpha (pw) (alpha)
  `(Wand:PixelSetAlpha ,pw ,alpha))

;;}}}

;;{{{  `-- Image pixels operations

(defvar MagickStorageType :int)
(defmacro MagickStorageType-get (kw)
  (cond ((eq kw :undefined-pixel) 0)
        ((eq kw :char-pixel) 1)
        (t `(raise "Unknown MagickStorageType: %s" ,kw))))

(define-ffi-function Wand:MagickExportImagePixels "MagickExportImagePixels" MagickBooleanType
  (MagickWand
   :long                                ;from-width
   :long                                ;from-height
   :ulong                               ;delta-width
   :ulong                               ;delta-height
   :pointer                             ;map (c-string)
   MagickStorageType
   :pointer)                            ;target
  libmagickwand)

;; Use `Wand:RelinquishMemory' when done
(defun Wand:get-image-pixels-internal
    (wand img-type from-width from-height delta-width delta-height)
  "Return WAND's raw string of image pixel data (RGB triples).
FROM-WIDTH, FROM-HEIGHT, DELTA-WIDTH, DELTA-HEIGHT specifies region to
fetch data from."
  (let* ((mapn-tsz (ecase img-type
                     (rgb (cons "RGB" 3))
                     (rgba (cons "RGBA" 4))
                     (bgr (cons "BGR" 3))
                     (bgra (cons "BGRA" 4))
                     (bgrp (cons "BGRP" 4))))
         (rsize (* delta-width delta-height (cdr mapn-tsz)))
         (target (ffi-allocate rsize)))
    (with-ffi-string (mapncstr (car mapn-tsz))
      (Wand:MagickExportImagePixels
       wand from-width from-height delta-width delta-height
       mapncstr (MagickStorageType-get :char-pixel) target)
      (cons rsize target))))

(defun Wand:get-image-pixels (wand)
  "Return WAND's raw string of image pixel data (RGB triples)."
  (Wand:get-image-pixels-internal
   wand 'rgb 0 0 (Wand:image-width wand) (Wand:image-height wand)))

;;}}}
;;{{{  `-- Image modification functions

(defvar MagickFilterType :int)
(defvar MagickFilterTypes
  '(("point" . 1) ("box" . 2) ("triangle" . 3) ("hermite" . 4)
    ("hanning" . 5) ("hamming" . 6) ("blackman" . 7) ("gaussian" . 8)
    ("quadratic" . 9) ("cubic" . 10) ("catrom" . 11) ("mitchell" . 12)
    ("jinc" . 13) ("sinc" . 14) ("sincfast" . 15) ("kaiser" . 16)
    ("welsh" . 17) ("parzen" . 18) ("bohman" . 19) ("bartlett" . 20)
    ("lagrange" . 21) ("lanczos" . 22) ("lanczossharp" . 23) ("lanczos2" . 24)
    ("lanczos2sharp" . 25) ("robidoux" . 26) ("robidouxsharp" . 27)
    ("cosine" . 28) ("spline" . 29) ("lanczosradius" . 30) ("sentinel" . 31)))

(defvar WandCompositeOperator :int)
(defconst WandCompositeOperators
  '(("no" . 1) ("add" . 2) ("atop" . 3) ("blend" . 4)
    ("bumpmap" . 5) ("change-mask" . 6) ("clear" . 7)
    ("color-burn" . 8) ("color-dodge" . 9) ("colorize" . 10)
    ("copy-black" . 11) ("copy-blue" . 12) ("copy" . 13)
    ("copy-cyan" . 14) ("copy-green" . 15) ("copy-magenta" . 16)
    ("copy-opacity" . 17) ("copy-red" . 18) ("copy-yellow" . 19)
    ("darken" . 20) ("dst-atop" . 21) ("dst" . 22) ("dst-in" . 23)
    ("dst-out" . 24) ("dst-over" . 25) ("difference" . 26)
    ("displace" . 27) ("dissolve" . 28) ("exclusion" . 29)
    ("hardlight" . 30) ("hue" . 31) ("in" . 32) ("lighten" . 33)
    ("linearlight" . 34) ("luminize" . 35) ("minus" . 36)
    ("modulate" . 37) ("multiply" . 38) ("out" . 39) ("over" . 40)
    ("overlay" . 41) ("plus" . 42) ("replace" . 43) ("saturate" . 44)
    ("screen" . 45) ("softlight" . 46) ("srcatop" . 47) ("src" . 48)
    ("srcin" . 49) ("srcout" . 50) ("srcover" . 51) ("subtract" . 52)
    ("threshold" . 53) ("xor" . 54) ("divide" . 55) 
    ))

(defvar MagickNoiseType :int)
(defconst MagickNoiseTypes
  '(("uniform" . 1) ("guassian" . 2) ("mult-gaussian" . 3)
    ("impulse" . 4) ("laplacian" . 5) ("poisson" . 6) ("random" . 7)))

(defvar MagickPreviewType :int)
(defconst MagickPreviewTypes
  '(("rotate" . 1) ("shear" . 2) ("roll" . 32)
    ("hue" . 4) ("saturation" . 5) ("brightness" . 6)
    ("gamma" . 7) ("spiff" . 8) ("dull" . 9) ("grayscale" . 10)
    ("quantize" . 11) ("despeckle" . 12) ("reduce-noise" . 13)
    ("add-noise" . 14) ("sharpen" . 15) ("blur" . 16)
    ("threshold" . 17) ("edgedetect" . 18) ("spread" . 19)
    ("solarize" . 20) ("shade" . 21) ("raise" . 22)
    ("segment" . 23) ("swirl" . 24) ("implode" . 25)
    ("wave" . 26) ("oilpaint" . 27) ("charcoal-drawing" . 28)
    ("jpeg" . 29)))

(define-ffi-function Wand:RotateImage "MagickRotateImage" MagickBooleanType
  (MagickWand PixelWand :double) libmagickwand)

;;Scale the image in WAND to the dimensions WIDTHxHEIGHT.
(define-ffi-function Wand:scale-image "MagickScaleImage" MagickBooleanType
  (MagickWand :ulong :ulong) libmagickwand)

;; Sample the image
(define-ffi-function Wand:sample-image "MagickSampleImage" MagickBooleanType
  (MagickWand :ulong :ulong) libmagickwand)

(define-ffi-function Wand:resize-image "MagickResizeImage" MagickBooleanType
  (MagickWand :ulong :ulong MagickFilterType
              :double                   ;blur
              )
  libmagickwand)

(ignore-errors
  (define-ffi-function Wand:liquid-rescale "MagickLiquidRescaleImage"
    MagickBooleanType
    (MagickWand :ulong :ulong
                :double                 ;delta-x
                :double                 ;rigidity
                )
    libmagickwand))

(define-ffi-function Wand:flip-image "MagickFlipImage" MagickBooleanType
  (MagickWand) libmagickwand)
(define-ffi-function Wand:flop-image "MagickFlopImage" MagickBooleanType
  (MagickWand) libmagickwand)

;; Swirl the image associated with WAND by DEGREES.
(define-ffi-function Wand:swirl-image "MagickSwirlImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

(define-ffi-function Wand:MagickPosterizeImage "MagickPosterizeImage"
  MagickBooleanType
  (MagickWand :ulong MagickBooleanType) libmagickwand)
(defun Wand:posterize-image (wand levels &optional ditherp)
  "Posterize the image associated with WAND.
that is quantise the range of used colours to at most LEVELS.
If optional argument DITHERP is non-nil use a dithering
effect to wipe hard contrasts."
  (Wand:MagickPosterizeImage wand levels (if ditherp 1 0)))

;; Tweak the image associated with WAND.
(define-ffi-function Wand:MagickModulateImage "MagickModulateImage"
  MagickBooleanType
  ;; wand brightness saturation hue
  (MagickWand :double :double :double) libmagickwand)

(cl-defun Wand:modulate-image (wand &key (brightness 100.0)
                                    (saturation 100.0)
                                    (hue 100.0))
  (Wand:MagickModulateImage wand brightness saturation hue))

;; Solarise the image associated with WAND.
(define-ffi-function Wand:solarize-image "MagickSolarizeImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

;; Perform gamma correction on the image associated with WAND.
;; The argument LEVEL is a positive float, a value of 1.00 (read 100%)
;; is a no-op.
(define-ffi-function Wand:gamma-image "MagickGammaImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

(define-ffi-function Wand:MagickRaiseImage "MagickRaiseImage" MagickBooleanType
  (MagickWand :ulong :ulong :long :long MagickBooleanType) libmagickwand)

(defun Wand:raise-image (wand &optional raise x y)
  "Raise image."
  (Wand:MagickRaiseImage
   wand (Wand:image-width wand) (Wand:image-height wand)
   (or x 10) (or y 10) (if raise 1 0)))

(define-ffi-function Wand:spread-image "MagickSpreadImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

;; Blur the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(define-ffi-function Wand:gaussian-blur-image "MagickGaussianBlurImage"
  MagickBooleanType
  (MagickWand :double :double) libmagickwand)

;; Blur the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
;; The ANGLE argument is a float and measured in degrees.
(define-ffi-function Wand:motion-blur-image "MagickMotionBlurImage"
  MagickBooleanType
  ;; wand radius sigma angle
  (MagickWand :double :double :double) libmagickwand)

;; Blur the image associated with WAND.
;; The ANGLE argument is a float and measured in degrees.
(define-ffi-function Wand:radial-blur-image "MagickRadialBlurImage"
  MagickBooleanType
  (MagickWand :double) libmagickwand)

;; Sharpen the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(define-ffi-function Wand:sharpen-image "MagickSharpenImage" MagickBooleanType
  (MagickWand :double :double) libmagickwand)

;; Simulates an image shadow
(define-ffi-function Wand:shadow-image "MagickShadowImage"
  MagickBooleanType
  ;; wand opacity(%) sigma x-offset y-offset
  (MagickWand :double :double :long :long) libmagickwand)

;; MagickTrimImage() remove edges that are the background color from
;; the image.
(define-ffi-function Wand:trim-image "MagickTrimImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

(define-ffi-function Wand:preview-images "MagickPreviewImages" MagickWand
  (MagickWand MagickPreviewType) libmagickwand)

(define-ffi-function Wand:MagickNegateImage "MagickNegateImage" MagickBooleanType
  (MagickWand MagickBooleanType) libmagickwand)
(defun Wand:negate-image (wand &optional greyp)
  "Perform negation on the image associated with WAND."
  (Wand:MagickNegateImage wand (if greyp 1 0)))

;; Crop to the rectangle spanned at X and Y by width DX and
;; height DY in the image associated with WAND."
(define-ffi-function Wand:crop-image "MagickCropImage" MagickBooleanType
  (MagickWand :ulong :ulong :ulong :ulong) libmagickwand)

;; MagickChopImage() removes a region of an image and collapses the
;; image to occupy the removed portion
(define-ffi-function Wand:chop-image "MagickChopImage" MagickBooleanType
  (MagickWand :ulong :ulong :long :long) libmagickwand)

;; Reduce the noise in the image associated with WAND by RADIUS.
(define-ffi-function Wand:reduce-noise-image "MagickReduceNoiseImage"
  MagickBooleanType
  (MagickWand :double) libmagickwand)

;; MagickAddNoiseImage() adds random noise to the image.
(define-ffi-function Wand:add-noise-image "MagickAddNoiseImage"
  MagickBooleanType
  (MagickWand MagickNoiseType) libmagickwand)

;; Composite one image COMPOSITE-WAND onto another WAND at the
;; specified offset X, Y, using composite operator COMPOSE.
(define-ffi-function Wand:image-composite "MagickCompositeImage" MagickBooleanType
  (MagickWand MagickWand WandCompositeOperator :long :long) libmagickwand)

;;; image improvements and basic image properties
(define-ffi-function Wand:MagickContrastImage "MagickContrastImage"
  MagickBooleanType
  (MagickWand MagickBooleanType) libmagickwand)

;; Non-linear contrast changer
(define-ffi-function Wand:MagickSigmoidalContrastImage "MagickSigmoidalContrastImage"
  MagickBooleanType
  (MagickWand MagickBooleanType :double :double) libmagickwand)

;; Reduce the speckle noise in the image associated with WAND.
(define-ffi-function Wand:despeckle-image "MagickDespeckleImage" MagickBooleanType
  (MagickWand) libmagickwand)

;; Enhance the image associated with WAND.
(define-ffi-function Wand:enhance-image "MagickEnhanceImage" MagickBooleanType
  (MagickWand) libmagickwand)

;; Equalise the image associated with WAND.
(define-ffi-function Wand:equalize-image "MagickEqualizeImage" MagickBooleanType
  (MagickWand) libmagickwand)

;; Normalise the image associated with WAND.
(define-ffi-function Wand:normalize-image "MagickNormalizeImage" MagickBooleanType
  (MagickWand) libmagickwand)

;; Simulate a charcoal drawing of the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(define-ffi-function Wand:charcoal-image "MagickCharcoalImage" MagickBooleanType
  (MagickWand :double :double) libmagickwand)

;; Simulate oil-painting of image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
(define-ffi-function Wand:oil-paint-image "MagickOilPaintImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

;; MagickSepiaToneImage() applies a special effect to the image,
;; similar to the effect achieved in a photo darkroom by sepia
;; toning. Threshold ranges from 0 to QuantumRange and is a measure of
;; the extent of the sepia toning. A threshold of 80 is a good
;; starting point for a reasonable tone.
(define-ffi-function Wand:sepia-tone-image "MagickSepiaToneImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

;; MagickImplodeImage() creates a new image that is a copy of an
;; existing one with the image pixels "implode" by the specified
;; percentage. It allocates the memory necessary for the new Image
;; structure and returns a pointer to the new image.
(define-ffi-function Wand:implode-image "MagickImplodeImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

;; MagickVignetteImage() softens the edges of the image in vignette
;; style.
(define-ffi-function Wand:vignette-image "MagickVignetteImage" MagickBooleanType
  ;; wand black-point white-point x y
  (MagickWand :double :double :double :double) libmagickwand)

;; Enhance the edges of the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
(define-ffi-function Wand:edge-image "MagickEdgeImage" MagickBooleanType
  (MagickWand :double) libmagickwand)

;; Emboss the image associated with WAND (a relief effect).
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(define-ffi-function Wand:emboss-image "MagickEmbossImage" MagickBooleanType
  (MagickWand :double :double) libmagickwand)

;;}}}
;;{{{ Util image, glyph and size related functions
(defun Wand:emacs-image-internal (wand x y w h)
  "Return Emacs image spec."
  (let ((pxd (Wand:get-image-pixels-internal wand 'rgb x y w h)))
    (list 'image :type 'imagemagick
          :ascent 'center
          :data (list (car pxd) (cdr pxd)
                      (make-finalizer
                       `(lambda ()
                          (message "FREEE ffi memory")
                          (ffi-free ,(cdr pxd)))))
          :format 'image/x-rgb
          :width w :height h)))

(defun Wand:emacs-image (wand)
  "Return Emacs image for the WAND."
  (Wand:emacs-image-internal
   wand 0 0 (Wand:image-width wand) (Wand:image-height wand)))

(defun Wand:fit-size (wand max-width max-height &optional scaler force)
  "Fit WAND image into MAX-WIDTH and MAX-HEIGHT.
This operation keeps aspect ratio of the image.
Use SCALER function to perform scaling, by default `Wand:scale-image'
is used.
Return non-nil if fiting was performed."
  (unless scaler (setq scaler #'Wand:scale-image))
  (let* ((width (Wand:image-width wand))
         (height (Wand:image-height wand))
         (prop (/ (float width) (float height)))
         rescale)
    (when (or force (< max-width width))
      (setq width max-width
            height (round (/ max-width prop))
            rescale t))
    (when (or force (< max-height height))
      (setq width (round (* max-height prop))
            height max-height
            rescale t))

    (when rescale
      (funcall scaler wand width height))
    rescale))

(defun Wand:correct-orientation (wand)
  "Automatically rotate WAND image according to orientation."
  (let ((angle (case (Wand:image-orientation wand)
                 (MagickOrientationRightTop 90)
                 (MagickOrientationBottomRight 180)
                 (MagickOrientationLeftBottom -90))))
    (when angle
      (setf (Wand:image-orientation wand) MagickOrientationTopLeft)
      (Wand-operation-apply 'rotate wand angle))))

(defun Wand:preview-emacs-image (wand)
  (let ((off-x (get 'preview-wand 'offset-x))
        (off-y (get 'preview-wand 'offset-y)))
    (Wand:emacs-image-internal
     wand off-x off-y
     (- (Wand:image-width wand) off-x)
     (- (Wand:image-height wand) off-y))))

(defun Wand:insert-emacs-image (image &optional keymap)
  (let ((start (point)))
    (insert " ")
    (set-text-properties
     start (point)
     (list 'display image 'keymap keymap 'pointer 'arrow))))

;;}}}


;;{{{ Custom variables for Wand-mode

(defgroup wand nil
  "Group to customize wand mode."
  :prefix "wand-")

(defcustom wand-redeye-threshold 1.6
  "*Threshold to fix red eyes."
  :type 'float
  :group 'wand)

(defcustom wand-sigma 2.0
  "*Sigma for operations such as gaussian-blur, sharpen, etc.
The standard deviation of the Gaussian, in pixels"
  :type 'float
  :group 'wand)

(defcustom wand-zoom-factor 2
  "Default zoom in/out factor."
  :type 'number
  :group 'wand)

(defcustom wand-pattern-composite-op "dst-over"
  "Default composite for 'pattern' operation."
  :type 'string
  :group 'wand)

(defcustom wand-region-outline-color "black"
  "*Color used to outline region when selecting."
  :type 'color
  :group 'wand)

(defcustom wand-region-fill-color "white"
  "*Color used to fill region when selecting."
  :type 'color
  :group 'wand)

(defcustom wand-region-outline-width 1.3
  "*Width of outline line for region when selecting."
  :type 'float
  :group 'wand)

(defcustom wand-region-outline-opacity 0.7
  "*Opacity of the outline.
1.0 - Opaque
0.0 - Transparent"
  :type 'float
  :group 'wand)

(defcustom wand-region-fill-opacity 0.35
  "*Opacity for the region when selecting.
1.0 - Opaque
0.0 - Transparent"
  :type 'float
  :group 'wand)

(defcustom wand-show-fileinfo t
  "*Non-nil to show file info on top of display."
  :type 'boolean
  :group 'wand)

(defcustom wand-show-iptc-info t
  "*Non-nil to display IPTC info if any."
  :type 'boolean
  :group 'wand)

(defcustom wand-show-operations t
  "Non-nil to show operations done on file."
  :type 'boolean
  :group 'wand)

(defcustom wand-auto-fit t
  "*Non-nil to perform fiting to window size.
You can always toggle fitting using `wand-toggle-fit' command
\(bound to \\<wand-mode-map>\\[wand-toggle-fit]\)."
  :type 'boolean
  :group 'wand)

(defcustom wand-auto-rotate t
  "*Non-nil to perform automatic rotation according to orientation.
Orientation is taken from EXIF."
  :type 'boolean
  :group 'wand)

(defcustom wand-query-for-overwrite t
  "*Non-nil to ask user when overwriting existing files."
  :type 'boolean
  :group 'wand)

(defcustom wand-properties-pattern "^exif:"
  "Pattern for properties editor."
  :type 'string
  :group 'wand)

;; History of `wand-operate' commands.
(defvar wand-operate-history nil)

(defvar wand-global-operations-list nil
  "Denotes global operations list")

(defcustom wand-scaler #'Wand:scale-image
  "Function used to scale image for \"fit to size\" operation.
You could use one of `Wand:scale-image', `Wand:sample-image' or create
your own scaler with `Wand-make-scaler'."
  :type 'function
  :group 'wand)

(defvar wand-mode-hook nil
  "Hooks to call when entering `wand-mode'.")

(defvar wand-info-hook nil
  "Hooks to call when inserting info into `wand-mode'.")

;;}}}
;;{{{ wand-mode-map

(defvar wand-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Undo/Redo operation
    (define-key map (kbd "C-/") #'wand-undo)
    (define-key map (kbd "C-_") #'wand-undo)
    (define-key map [undo] #'wand-undo)
    (define-key map (kbd "C-x C-/") #'wand-redo)
    (define-key map (kbd "C-x M-:") #'wand-edit-operations)
    (define-key map (kbd "C-.") #'wand-repeat-last-operation)

    ;; Saving
    (define-key map (kbd "C-x C-s") #'wand-save-file)
    (define-key map (kbd "C-x C-w") #'wand-write-file)

    ;; Navigation
    (define-key map [space] #'wand-next-image)
    (define-key map [backspace] #'wand-prev-image)
    (define-key map (kbd "M-<") #'wand-first-image)
    (define-key map (kbd "M->") #'wand-last-image)

    (define-key map [next] #'Wand-mode-next-page)
    (define-key map [prior] #'Wand-mode-prev-page)
    (define-key map [home] #'Wand-mode-first-page)
    (define-key map [end] #'Wand-mode-last-page)
    (define-key map [?g] #'Wand-mode-goto-page)
    (define-key map [(meta ?g)] #'Wand-mode-goto-page)

    ;; Region
    (define-key map [down-mouse-1] #'wand-select-region)
    (define-key map (kbd "C-M-z") #'wand-activate-region)

    ;; General commands
    (define-key map [mouse-3] #'wand-popup-menu)
    (define-key map (kbd "M-<mouse-1>") #'Wand-mode-drag-image)
    (define-key map (kbd "C-<mouse-1>") #'Wand-mode-drag-image)
    (define-key map "o" #'wand-operate)
    (define-key map "O" #'wand-global-operations-list)
    (define-key map "x" #'wand-toggle-fit)
    (define-key map "i" #'wand-identify)
    (define-key map "e" #'wand-prop-editor)
    (define-key map "q" #'wand-quit)
    (define-key map (kbd "C-r") #'wand-reload)
    (define-key map "p" #'wand-iptc-add-tag)

    ;; Zooming
    (define-key map "+" #'wand-zoom-in)
    (define-key map "-" #'wand-zoom-out)

    ;; Rotations
    (define-key map "r" #'wand-rotate-right)
    (define-key map "l" #'wand-rotate-left)

    ;; Region operations
    (define-key map "c" #'wand-crop)
    (define-key map "." #'wand-redeye-remove)

    map)
  "Keymap for wand-mode.")

;;}}}
;;{{{ wand-menu

(defvar wand-menu
  '("Wand"
    ["Next" wand-next-image
     :active (wand--next-file buffer-file-name)]
    ["Previous" wand-prev-image
     :active (wand--next-file buffer-file-name t)]
    ["First" wand-first-image]
    ["Last" wand-last-image]
    ("Page" :filter wand-menu-page-navigations)
    "---"
    ["Image Info" wand-identify]
    ["Reload" wand-reload]
    ["Fitting" wand-toggle-fit
     :style toggle :selected (get 'image-wand 'fitting)]
    "---"
    ["Undo" wand-undo :active operations-list]
    ["Redo" wand-redo :active undo-list]
    ["Save Image" wand-save-file]
    ["Save Image As" wand-write-file]
    "---"
    ["Zoom In" wand-zoom-in]
    ["Zoom Out" wand-zoom-out]
    "---"
    ["Rotate right" wand-rotate-right]
    ["Rotate left" wand-rotate-left]
    "---"
    ("Region" :filter wand-menu-region-operations)
    ("Transform" :filter (lambda (not-used)
                           (wand-menu-generate 'transform-operation)))
    ("Effects" :filter (lambda (not-used)
                         (wand-menu-generate 'effect-operation)))
    ("Enhance" :filter (lambda (not-used)
                         (wand-menu-generate 'enhance-operation)))
    ("F/X" :filter (lambda (not-used)
                     (wand-menu-generate 'f/x-operation)))
    "---"
    ["Quit" wand-quit])
  "Menu for Wand display mode.")

(defun wand-menu-page-navigations (not-used)
  "Generate menu for page navigation."
  (list ["Next Page" Wand-mode-next-page
         :active (Wand:has-next-image image-wand)]
        ["Previous Page" Wand-mode-prev-page
         :active (Wand:has-prev-image image-wand)]
        ["First Page" Wand-mode-first-page
         :active (/= (Wand:iterator-index image-wand) 0) ]
        ["Last Page" Wand-mode-last-page
         :active (/= (Wand:iterator-index image-wand)
                     (1- (Wand:images-num image-wand))) ]
        "-"
        ["Goto Page" Wand-mode-goto-page
         :active (/= (Wand:images-num image-wand) 1)]))

(defun wand-menu-region-operations (not-used)
  "Generate menu for region operations."
  (mapcar #'(lambda (ro)
              (vector (get ro 'menu-name) ro :active 'preview-region))
          (apropos-internal "^wand-"
                            #'(lambda (c)
                                (and (commandp c)
                                     (get c 'region-operation)
                                     (get c 'menu-name))))))

(defun wand--commands-by-tag (tag)
  "Return list of wand command for which TAG property is set."
  (apropos-internal "^wand-"
                    #'(lambda (c) (and (commandp c) (get c tag)))))

(defun wand-menu-generate (tag)
  "Generate menu structure for TAG commands."
  (mapcar #'(lambda (to)
              (vector (get to 'menu-name) to))
          (remove-if-not #'(lambda (c) (get c tag))
                         (wand--commands-by-tag 'menu-name))))

(defun wand-popup-menu (be)
  "Popup wand menu."
  (interactive "e")
  (popup-menu wand-menu be))

;;}}}

;;{{{ Operations definitions

(defmacro define-wand-operation (name args &rest body)
  "Define new operation of NAME.
ARGS specifies arguments to operation, first must always be wand."
  (let ((fsym (intern (format "wand--op-%S" name))))
    `(defun ,fsym ,args
       ,@body)))

(defmacro wand--possible-for-region (wand &rest body)
  `(if preview-region
       (let* ((iwand ,wand)
              (region (wand--image-region))
              (wand (apply #'Wand:image-region iwand region)))
         (unwind-protect
             (progn
               ,@body
               (Wand:image-composite
                iwand wand (cdr (assoc "copy" WandCompositeOperators))
                (nth 2 region) (nth 3 region)))
           (setq preview-region nil)
           (Wand:delete-wand wand)))
     ,@body))
(put 'wand--possible-for-region 'lisp-indent-function 'defun)

(define-wand-operation flip (wand)
  "Flip the image."
  (wand--possible-for-region wand
    (Wand:flip-image wand)))

(define-wand-operation flop (wand)
  "Flop the image."
  (wand--possible-for-region wand
    (Wand:flop-image wand)))

(define-wand-operation normalize (wand)
  "Normalise image."
  (wand--possible-for-region wand
    (Wand:normalize-image wand)))

(define-wand-operation despeckle (wand)
  "Despeckle image."
  (wand--possible-for-region wand
    (Wand:despeckle-image wand)))

(define-wand-operation enhance (wand)
  "Enhance image."
  (wand--possible-for-region wand
    (Wand:enhance-image wand)))

(define-wand-operation equalize (wand)
  "Equalise image."
  (wand--possible-for-region wand
    (Wand:equalize-image wand)))

(define-wand-operation gauss-blur (wand radius sigma)
  "Gauss blur image."
  (wand--possible-for-region wand
    (Wand:gaussian-blur-image wand (float radius) (float sigma))))

(define-wand-operation radial-blur (wand angle)
  "Radial blur."
  (wand--possible-for-region wand
    (Wand:radial-blur-image wand (float angle))))

(define-wand-operation motion-blur (wand radius sigma angle)
  "Motion blur."
  (wand--possible-for-region wand
    (Wand:motion-blur-image wand (float radius) (float sigma) (float angle))))

(define-wand-operation sharpen (wand radius sigma)
  "Sharpenize image."
  (wand--possible-for-region wand
    (Wand:sharpen-image wand (float radius) (float sigma))))

(define-wand-operation shadow (wand opacity sigma x y)
  "Emulate shadow in image."
  (wand--possible-for-region wand
    (Wand:shadow-image wand (float opacity) (float sigma) x y)))

(define-wand-operation negate (wand greyp)
  "Negate image."
  (wand--possible-for-region wand
    (Wand:negate-image wand greyp)))

(define-wand-operation modulate (wand mtype minc)
  "Modulate the image WAND using MTYPE by MINC."
  (wand--possible-for-region wand
    (Wand:modulate-image wand mtype (float (+ 100 minc)))))

(define-Wand-operation grayscale (wand)
  "Grayscale image."
  (Wand-possible-for-region wand
    (Wand:SetImageColorspace wand :GRAYColorspace)))

(define-wand-operation solarize (wand threshold)
  "Solarise image by THRESHOLD."
  (wand--possible-for-region wand
    (Wand:solarize-image wand (float threshold))))

(define-wand-operation swirl (wand degrees)
  "Swirl image."
  (wand--possible-for-region wand
    (Wand:swirl-image wand (float degrees))))

(define-wand-operation oil (wand radius)
  "Simulate oil-painting of image."
  (wand--possible-for-region wand
    (Wand:oil-paint-image wand (float radius))))

(define-wand-operation charcoal (wand radius sigma)
  "Simulate charcoal painting of image."
  (wand--possible-for-region wand
    (Wand:charcoal-image wand (float radius) (float sigma))))

(define-wand-operation sepia-tone (wand threshold)
  "Apply sepia tone to image by THRESHOLD."
  (wand--possible-for-region wand
    (Wand:sepia-tone-image wand (float threshold))))

(define-wand-operation implode (wand radius)
  "Implude image by RADIUS."
  (wand--possible-for-region wand
    (Wand:implode-image wand (float radius))))

(define-Wand-operation wave (wand amplitude wave-length)
  "Create wave effect for image with AMPLITUDE and WAVE-LENGTH."
  (Wand-possible-for-region wand
    (Wand:wave-image wand (float amplitude) (float wave-length))))

(define-wand-operation vignette (wand white black x y)
  "Vignette from image."
  (wand--possible-for-region wand
    (Wand:vignette-image wand (float white) (float black) (float x) (float y))))

(define-wand-operation edge (wand radius)
  "Enhance the edges of the image."
  (wand--possible-for-region wand
    (Wand:edge-image wand (float radius))))

(define-wand-operation emboss (wand radius sigma)
  "Emboss the image, i.e. add relief."
  (wand--possible-for-region wand
    (Wand:emboss-image wand (float radius) (float sigma))))

(define-wand-operation reduce-noise (wand radius)
  "Reduce noise in the image."
  (wand--possible-for-region wand
    (Wand:reduce-noise-image wand (float radius))))

(define-wand-operation add-noise (wand noise-type)
  "Add noise to image."
  (wand--possible-for-region wand
    (Wand:add-noise-image wand (cdr (assoc noise-type MagickNoiseTypes)))))

(define-wand-operation spread (wand radius)
  "Spread the image."
  (wand--possible-for-region wand
    (Wand:spread-image wand (float radius))))

(define-wand-operation trim (wand fuzz)
  "Trim the image."
  (wand--possible-for-region wand
    (Wand:trim-image wand (float fuzz))))

(define-wand-operation raise (wand raise)
  "Raise (3d button effect) the image."
  (wand--possible-for-region wand
    (Wand:raise-image wand raise)))

(define-wand-operation rotate (wand degree)
  "Rotate image by DEGREE.
This is NOT lossless rotation for jpeg-like formats."
  (Wand-with-pixel-wand pw
    (setf (Wand:pixel-color pw) "black")
    (Wand:RotateImage wand pw (float degree))))

(define-wand-operation zoom (wand factor)
  (when (< factor 0)
    (setq factor (/ 1.0 (- factor))))
  (let ((nw (* (Wand:image-width wand) (float factor)))
        (nh (* (Wand:image-height wand) (float factor))))
    (Wand:scale-image wand (round nw) (round nh))))

(define-wand-operation contrast (wand cp)
  "Increase/decrease contrast of the image."
  (wand--possible-for-region wand
    (Wand:MagickContrastImage wand (if (eq cp :increase) 1 0))))

(define-wand-operation sigmoidal-contrast (wand cp strength midpoint)
  "Increase/decrease contrast of the image.
CP - `:increase' to increase, `:decrease' to decrease.
STRENGTH - larger the number the more 'threshold-like' it becomes.
MIDPOINT - midpoint of the function as a color value 0 to QuantumRange"
  (wand--possible-for-region wand
    (Wand:MagickSigmoidalContrastImage
     wand (if (eq cp :increase) 1 0) (float strength)
     (* (Wand:quantum-range) (/ midpoint 100.0)))))

(define-wand-operation scale (wand width height)
  (Wand:scale-image wand width height))

(define-wand-operation sample (wand width height)
  (Wand:sample-image wand width height))

(defmacro wand-make-scaler (filter-type blur)
  "Create resize function, suitable with `Wand:fit-resize'.
FILTER-TYPE and BLUR specifies smothing applied after resize.
FILTER-TYPE is one of `MagickFilterTypes'
BLUR is float, 0.25 for insane pixels, > 2.0 for excessively smoth."
  `(lambda (iw x y)
     (Wand:resize-image iw x y ,(cdr (assoc filter-type MagickFilterTypes))
                        (float ,blur))))

(define-wand-operation fit-size (wand width height)
  (Wand:fit-size wand width height wand-scaler t))

(define-wand-operation liquid-rescale (wand width height)
  (Wand:liquid-rescale wand width height 0.0 0.0))

(define-wand-operation posterize (wand levels &optional ditherp)
  (Wand:posterize-image wand levels ditherp))

(define-wand-operation gamma (wand level)
  (Wand:gamma-image wand level))

(define-wand-operation crop (wand region)
  "Crop image to REGION."
  (apply #'Wand:crop-image wand region)
  (Wand:reset-image-page wand))

(define-wand-operation chop (wand region)
  "Chop REGION in the image."
  (apply #'Wand:chop-image wand region))

(define-wand-operation preview-op (wand ptype)
  "Preview operation PTYPE.
Return a new wand."
  (wand--possible-for-region wand
    (Wand:preview-images
     wand (cdr (assoc ptype MagickPreviewTypes)))))

;; TODO: pattern composition
(define-wand-operation pattern (wand pattern op)
  (Wand-with-wand cb-wand
    (setf (Wand:image-size cb-wand)
          (cons (Wand:image-width wand) (Wand:image-height wand)))
    (Wand:read-image-data cb-wand (concat "pattern:" pattern))
    (Wand:image-composite wand cb-wand
                          (cdr (assoc op WandCompositeOperators)) 0 0)))

;; TODO: other operations

;;}}}

;;{{{ wand-display, wand-mode

(defun wand--image-region ()
  "Return region in real image, according to `preview-region'."
  (unless preview-region
    (error "Region not selected"))

  (let ((off-x (get 'preview-wand 'offset-x))
        (off-y (get 'preview-wand 'offset-y))
        (xcoeff (/ (float (Wand:image-width image-wand))
                   (Wand:image-width preview-wand)))
        (ycoeff (/ (float (Wand:image-height image-wand))
                   (Wand:image-height preview-wand))))
    (mapcar #'round (list (* (nth 0 preview-region) xcoeff)
                          (* (nth 1 preview-region) ycoeff)
                          (* (+ (nth 2 preview-region) off-x) xcoeff)
                          (* (+ (nth 3 preview-region) off-y) ycoeff)))))

(defun wand--file-info ()
  "Return info about file as a string."
  (declare (special off-x))
  (declare (special off-y))
  (let ((iw (Wand:image-width image-wand))
        (ih (Wand:image-height image-wand))
        (ow (Wand:image-orig-width image-wand))
        (oh (Wand:image-orig-height image-wand)))
    (concat "File: " (file-name-nondirectory buffer-file-name)
            " (" (Wand:get-magick-property image-wand "size") "), "
            (Wand:image-format image-wand)
            " " (format "%dx%d" iw ih)
            (if (and (not (zerop ow)) (not (zerop oh))
                     (or (/= ow iw) (/= oh ih)))
                (format " (Orig: %dx%d)" ow oh)
              "")
            (if (> (Wand:images-num image-wand) 1)
                (format ", Page: %d/%d" (1+ (Wand:iterator-index image-wand))
                        (Wand:images-num image-wand))
              "")
            ;; Print offset info
            (if (and preview-wand (boundp 'off-x) (boundp 'off-y)
                     (or (positivep off-x) (positivep off-y)))
                (format ", Offset: +%d+%d" off-x off-y)
              "")
            ;; Print region info
            (if preview-region
                (apply #'format ", Region: %dx%d+%d+%d"
                       (wand--image-region))
              ""))))

(defun wand--iptc-split-keywords (tag-value)
  (mapcar #'(lambda (kw) (cons 'keyword kw))
          (nreverse
           (split-string tag-value "\\(, \\|,\\)"))))

(defun wand--iptc-from-widgets (widgets)
  "Return profile made up from WIDGETS info."
  (mapcan
   #'(lambda (widget)
       (let ((iptc-tag (widget-get widget :iptc-tag))
             (tag-value (widget-get widget :value)))
         (cond ((string= tag-value "") nil)
               ((eq iptc-tag 'keywords)
                ;; Special case for keywords
                (wand--iptc-split-keywords tag-value))
               (t (list (cons iptc-tag tag-value))))))
   widgets))

(defun wand--iptc-notify (wid &rest args)
  "Called when some IPTC info changed."
  (Wand:image-save-iptc-profile
   image-wand (wand--iptc-from-widgets (cons wid widget-field-list)))
  (wand--update-info))

(defun wand--insert-iptc-tags ()
  "Insert iptc tags info."
  (kill-local-variable 'widget-global-map)
  (kill-local-variable 'widget-field-new)
  (kill-local-variable 'widget-field-last)
  (kill-local-variable 'widget-field-was)
  (kill-local-variable 'widget-field-list)

  (let* ((iptc (Wand:image-profile-iptc image-wand))
         (cpt (cdr (assq 'caption iptc)))
         (kws (mapcar #'cdr (remove-if-not
                             #'(lambda (e) (eq 'keyword (car e)))
                             iptc))))
    (when cpt
      (widget-create 'editable-field
                     :tag "Caption"
                     :format "IPTC Caption: %v"
                     :iptc-tag 'caption
                     :notify #'wand--iptc-notify
                     cpt))
    (when kws
      (widget-create 'editable-field
                     :format "IPTC Keywords: %v"
                     :tag "Keywords"
                     :iptc-tag 'keywords
                     :notify #'wand--iptc-notify
                     (mapconcat #'identity kws ", ")))
    (widget-setup)))

(defun wand-iptc-add-tag (tag value)
  "Add TAG to ITPC profile."
  (interactive (list (completing-read
                      "IPTC Tag: " '(("caption") ("keywords")) nil t)
                     (read-string "ITPC Tag value: ")))
  (let ((tags-val (cond ((string= tag "caption")
                         (list (cons 'caption value)))
                        ((string= tag "keywords")
                         (wand--iptc-split-keywords value))
                        (t (error "Invalid IPTC tag")))))
    (Wand:image-save-iptc-profile
     image-wand (nconc (wand--iptc-from-widgets widget-field-list)
                       tags-val))
    (wand--update-info)))

(defun wand--insert-info ()
  "Insert some file informations."
  (when wand-show-fileinfo
    (insert (wand--file-info) "\n"))
  (when wand-show-iptc-info
    (wand--insert-iptc-tags))

  ;; XXX iptc may set those below again
  (let ((inhibit-read-only t)
        (before-change-functions nil)
        (after-change-functions nil))

    (when wand-show-operations
      (when operations-list
        (let ((start (point)))
          (insert (format "Operations: %S" operations-list) "\n")
          (add-text-properties start (point) '(read-only nil))))

      (when wand-global-operations-list
        (insert (format "Global operations: %S"
                        wand-global-operations-list) "\n")))

    ;; Info about pickup color
    (when (boundp 'pickup-color)
      (declare (special pickup-color))
      (let* ((cf (make-face (gensym "dcolor-") nil t))
             (place (car pickup-color))
             (color (cdr pickup-color))
             (fcol (apply #'format "#%02x%02x%02x" color)))
        (set-face-background cf fcol)
        (insert (format "Color: +%d+%d " (car place) (cdr place)))
        (insert-face "      " cf)
        (insert (format " %s R:%d, G:%d, B:%d\n" fcol
                        (car color) (cadr color) (caddr color)))))

    (run-hooks 'wand-info-hook)))

(defun wand--update-info ()
  "Only update info region."
  (let ((inhibit-read-only t)
        before-change-functions
        after-change-functions)
    (mapc #'widget-delete widget-field-list)
    (save-excursion
      (goto-char (point-min))
      (delete-region (point-at-bol)
                     (save-excursion
                       (goto-char (point-max))
                       (point-at-bol)))
      (wand--insert-info))
    (set-buffer-modified-p nil)))

(defun wand--update-file-info ()
  "Update file info."
  (when wand-show-fileinfo
    (let ((inhibit-read-only t)
          before-change-functions
          after-change-functions)
      (save-excursion
        (goto-char (point-min))
        (delete-region (point-at-bol) (point-at-eol))
        (insert (wand--file-info))))
    (set-buffer-modified-p nil)))

(defun wand--preview-with-region ()
  "Return highlighted version of `preview-wand' in case region is selected."
  (when preview-region
    (multiple-value-bind (w h x y) preview-region
      ;; Take into account current offset
      (incf x (get 'preview-wand 'offset-x))
      (incf y (get 'preview-wand 'offset-y))
      (Wand-with-drawing-wand dw
        (Wand-with-pixel-wand pw
          (setf (Wand:pixel-color pw) Wand-mode-region-outline-color)
          (Wand:DrawSetStrokeColor dw pw))
        (Wand-with-pixel-wand pw
          (setf (Wand:pixel-color pw) Wand-mode-region-fill-color)
          (setf (Wand:draw-fill-color dw) pw))
        (setf (Wand:draw-stroke-width dw) Wand-mode-region-outline-width
              (Wand:draw-stroke-opacity dw) Wand-mode-region-outline-opacity
              (Wand:draw-fill-opacity dw) Wand-mode-region-fill-opacity)
        (Wand:draw-lines dw (list (cons x y) (cons (+ x w) y)
                                  (cons (+ x w) (+ y h)) (cons x (+ y h))
                                  (cons x y)))
        (let ((nw (Wand:copy-wand preview-wand)))
          (Wand:MagickDrawImage nw dw)
          nw)))))

(defun wand--insert-preview ()
  "Display wand W at the point."
  ;; NOTE: if size not changed, then keep offset-x and offset-y
  ;; properties
  (let ((saved-w (and preview-wand (Wand:image-width preview-wand)))
        (saved-h (and preview-wand (Wand:image-height preview-wand)))
        (off-x (or (get 'preview-wand 'offset-x) 0))
        (off-y (or (get 'preview-wand 'offset-y) 0)))
    ;; Delete old preview and create new one
    (when preview-wand (Wand:delete-wand preview-wand))
    (setq preview-wand (Wand:get-image image-wand))

    ;; Rescale preview to fit the window
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer) t)
      (let ((scale-h (- (window-text-height nil t)
                        (* (line-pixel-height)
                           (count-lines (point-min) (point-max)))))
            (scale-w (window-text-width nil t)))
        (when (and (get 'image-wand 'fitting)
                   (Wand:fit-size preview-wand scale-w scale-h))
          (message "Rescale to %dx%d (fitting %dx%d)"
                   (Wand:image-width preview-wand)
                   (Wand:image-height preview-wand)
                   scale-w scale-h))))

    ;; Set offset properties
    (if (and (eq saved-w (Wand:image-width preview-wand))
             (eq saved-h (Wand:image-height preview-wand)))
        (progn (put 'preview-wand 'offset-x off-x)
               (put 'preview-wand 'offset-y off-y))
      (put 'preview-wand 'offset-x 0)
      (put 'preview-wand 'offset-y 0))

    ;; Hackery to insert invisible char, so widget-delete won't affect
    ;; preview-glyph visibility
    ;; (let ((ext (make-extent (point) (progn (insert " ") (point)))))
    ;;   (set-extent-property ext 'invisible t)
    ;;   (set-extent-property ext 'start-open t))

    (let ((pwr (wand--preview-with-region)))
      (unwind-protect
          (Wand:insert-emacs-image
           (Wand:preview-emacs-image (or pwr preview-wand))
           wand-mode-map)
        (when pwr
          (Wand:delete-wand pwr))))))

(defun wand--redisplay (&optional wand)
  "Redisplay Wand buffer with possible a new WAND."
  (when wand
    ;; A new wand in the air
    (Wand:delete-wand image-wand)
    (setq image-wand wand))

  (let ((inhibit-read-only t)
        before-change-functions
        after-change-functions)
    (erase-buffer)
    (wand--insert-info)
    (wand--insert-preview)
    (goto-char (point-min)))
  (set-buffer-modified-p nil))

;;;###autoload
(defun wand-display-noselect (file)
  (let* ((bn (format "*Wand: %s*" (file-name-nondirectory file)))
         (buf (if (and (eq major-mode 'wand-mode)
                       (not (get-buffer bn)))
                  ;; Use current buffer
                  (progn
                    (rename-buffer bn)
                    (current-buffer))
                (get-buffer-create bn))))
    (with-current-buffer buf
      (unless (eq major-mode 'wand-mode)
        ;; Initialise local variables
        (kill-all-local-variables)
        (make-variable-buffer-local 'image-wand)
        (make-variable-buffer-local 'preview-wand)
        (make-variable-buffer-local 'preview-region)
        (make-variable-buffer-local 'preview-offset)
        (make-variable-buffer-local 'last-preview-region)
        (make-variable-buffer-local 'operations-list)
        (make-variable-buffer-local 'undo-list)
        (make-variable-buffer-local 'kill-buffer-hook)
        (setq operations-list nil)
        (setq undo-list nil)
        (setq preview-wand nil)
        (setq image-wand (Wand:make-wand))
        (put 'image-wand 'fitting wand-auto-fit)

        (use-local-map wand-mode-map)
        (setq mode-name "wand")
        (setq major-mode 'wand-mode)
        (setq buffer-read-only t)
        ;; Setup menubar
        (add-submenu '() wand-menu)
        (add-hook 'kill-buffer-hook 'wand--cleanup))

      (when preview-wand
        (Wand:delete-wand preview-wand))
      (setq preview-wand nil)
      (setq preview-region nil)
      (setq preview-offset nil)
      (setq last-preview-region nil)
      (setq operations-list nil)
      (setq undo-list nil)
      (Wand:clear-wand image-wand)
      ;; Fix buffer-file-name in case of viewing directory
      (when (file-directory-p file)
        (setq file (or (Wand-next-file (concat file "/.")) file)))
      (setq buffer-file-name file)
      (setq default-directory (file-name-directory file))

      (unless (Wand:read-image image-wand file)
        (unwind-protect
            (error "Can't read file %s: %s" file (Wand:exception image-wand))
          (kill-buffer (current-buffer))))

      ;; NOTE: New IM sets iterator index to last page, we want to
      ;; start from the first page
      (setf (Wand:iterator-index image-wand) 0)

      (when wand-auto-rotate
        (Wand:correct-orientation image-wand))

      ;; Apply operations in case global operations list is used
      (mapc #'(lambda (op)
                (apply #'Wand-operation-apply
                       (car op) image-wand (cdr op)))
            wand-global-operations-list)

      (wand--redisplay)

      ;; Finally run hook
      (run-hooks 'wand-mode-hook))
    buf))

;;;###autoload
(defun wand-display (file)
  (interactive "fImage file: ")
  (switch-to-buffer (wand-display-noselect file) t))

(defun wand-mode ()
  "Start `wand-display' on filename associated with current buffer.
Bindings are:
  \\{wand-mode-map}"
  (interactive)
  (wand-display (buffer-file-name)))

;;;###autoload
(defun Wand-find-file-enable ()
  "Enable `find-file' to use `Wand-display' for supported filetypes."
  (push '(Wand-file-supported-for-read-p . Wand-display-noselect)
        find-file-magic-files-alist))

(defun wand--cleanup ()
  "Cleanup when wand buffer is killed."
  (when preview-wand
    (Wand:delete-wand preview-wand)
    (setq preview-wand nil))
  (Wand:delete-wand image-wand)
  (setq image-wand nil))

(defun wand-quit ()
  "Quit Wand display mode."
  (interactive)
  (kill-buffer (current-buffer)))

(defun wand-reload ()
  "Reload and redisplay image file."
  (interactive)
  (wand-display buffer-file-name))

(defun wand-identify ()
  "Show info about image."
  (interactive)
  (let ((iw image-wand)
        (ibuf (buffer-name (get-buffer-create "*Help: Wand:info*"))))
    (with-help-window ibuf
      (with-current-buffer ibuf
        (insert (Wand:identify-image iw))))))

(defun wand--operations-table ()
  "Return completion table for Wand operations."
  (mapcar #'(lambda (to)
              (cons (downcase (get to 'menu-name)) to))
          (wand--commands-by-tag 'menu-name)))

(defun wand-operate (op-name)
  "Operate on image."
  (interactive (list (completing-read
                      "Operation: " (wand--operations-table)
                      nil t nil wand-operate-history)))
  (let ((op (assoc op-name (wand--operations-table))))
    (let ((current-prefix-arg current-prefix-arg))
      (call-interactively (cdr op)))))

(defcustom wand-formats-cant-read
  '("a" "b" "c" "g" "h" "o" "k" "m" "r" "x" "y" "txt" "text" "pm" "logo")
  "List of formats that are not intented to be opened by Wand."
  :type '(list string)
  :group 'wand)

(defun wand-format-can-read-p (format)
  "Return non-nil if wand can read files in FORMAT."
  (unless (member (downcase format) wand-formats-cant-read)
    (let ((fi (Wand:GetMagickInfo
               format (ffi-address-of
                       (make-ffi-object 'MagickExceptionInfo)))))
      (and (not (ffi-null-p fi))
           (not (ffi-null-p (MagickInfo->decoder fi)))
           ))))
;; ImageMagick on linux treats any format to be RAW for some reason
;; We can't read raw formats
;           (not (MagickInfo->raw fi))))))

(defcustom wand-formats-cant-write
  '("html")
  "List of formats that are not intented to be written by Wand."
  :type '(list string)
  :group 'wand)

(defun wand-format-can-write-p (format)
  "Return non-nil if wand can write files in FORMAT."
  (unless (member (downcase format) wand-formats-cant-write)
    (let ((fi (Wand:GetMagickInfo
               format (ffi-address-of
                       (make-ffi-object 'MagickExceptionInfo)))))
      (and (not (ffi-null-p fi))
           (not (ffi-null-p (MagickInfo->encoder fi)))))))

;;;###autoload
(defun wand-file-can-read-p (file)
  "Return non-nil if wand can decode FILE."
  (let ((ext (file-name-extension file)))
    (and ext (wand-format-can-read-p ext))))

(defun wand-formats-list (fmt-regexp &optional mode)
  "Return names of supported formats that matches FMT-REGEXP.
Optionally you can specify MODE:
  'read  - Only formats that we can read
  'write - Only formats that we can write
  'read-write - Formats that we can and read and write
  'any or nil - Any format (default)."
  (let* ((excp (make-ffi-object 'MagickExceptionInfo))
         (num (make-ffi-object 'unsigned-long))
         (fil (Wand:GetMagickInfoList
               fmt-regexp (ffi-address-of num) (ffi-address-of excp))))
    (unless (ffi-null-p fil)
      (unwind-protect
          (loop for n from 0 below (ffi-get num)
            with minfo = nil
            do (setq minfo (ffi-aref fil n))
            if (ecase (or mode 'any)
                 (read (not (ffi-null-p (MagickInfo->decoder minfo))))
                 (write (not (ffi-null-p (MagickInfo->encoder minfo))))
                 (read-write
                  (and (not (ffi-null-p (MagickInfo->decoder minfo)))
                       (not (ffi-null-p (MagickInfo->encoder minfo)))))
                 (any t))
            collect (ffi-get (MagickInfo->name minfo) :type 'c-string))
        (Wand:RelinquishMemory fil)))))

;;}}}

;;{{{ File navigation commands

(defun wand--next-file (curfile &optional reverse-order)
  "Return next (to CURFILE) image file in the directory.
If REVERSE-ORDER is specified, then return previous file."
  (let* ((dir (file-name-directory curfile))
         (fn (file-name-nondirectory curfile))
         (dfiles (directory-files dir nil nil 'sorted-list t))
         (nfiles (cdr (member fn (if reverse-order (nreverse dfiles) dfiles)))))
    (while (and nfiles (not (Wand-file-supported-for-read-p
                             (concat dir (car nfiles)))))
      (setq nfiles (cdr nfiles)))
    (and nfiles (concat dir (car nfiles)))))

(defun wand-next-image (&optional reverse)
  "View next image."
  (interactive)
  (let ((nf (wand--next-file buffer-file-name reverse)))
    (unless nf
      (error (format "No %s file" (if reverse "previous" "next"))))
    (wand-display nf)))

(defun wand-prev-image ()
  "View previous image."
  (interactive)
  (wand-next-image t))

(defun wand-last-image (&optional reverse)
  "View last image in the directory."
  (interactive)
  (let ((rf buffer-file-name)
        (ff (wand--next-file buffer-file-name reverse)))
    (while ff
      (setq rf ff)
      (setq ff (wand--next-file rf reverse)))
    (wand-display rf)))

(defun wand-first-image ()
  "View very first image in the directory."
  (interactive)
  (wand-last-image t))

;;}}}

;;{{{ Operations list functions

(defun wand--operation-lookup (opname)
  (intern (format "wand--op-%S" opname)))

(defun wand--operation-apply (operation wand &rest args)
  "Apply OPERATION to WAND using addition arguments ARGS."
  (setq operations-list
        (append operations-list (list (cons operation args))))
  (setq undo-list nil)                  ; Reset undo
  (apply (wand--operation-lookup operation) wand args))

(defun wand--operation-list-apply (wand &optional operations)
  "Apply all operations in OPERATIONS list."
  (dolist (op (or operations operations-list))
    (apply (wand--operation-lookup (car op))
           wand (cdr op))))

;;}}}

;;{{{ Transform operations

(defun wand-flip ()
  "Flip the image."
  (interactive)
  (wand--operation-apply 'flip image-wand)
  (wand--redisplay))
(put 'wand-flip 'transform-operation t)
(put 'wand-flip 'menu-name "Flip")

(defun wand-flop ()
  "Flop the image."
  (interactive)
  (wand--operation-apply 'flop image-wand)
  (wand--redisplay))
(put 'wand-flop 'transform-operation t)
(put 'wand-flop 'menu-name "Flop")

(defun wand-trim (fuzz)
  "Trim edges the image."
  (interactive (list (read-number "Fuzz: " 0)))
  (wand--operation-apply 'trim image-wand (/ fuzz 100.0))
  (wand--redisplay))
(put 'wand-trim 'transform-operation t)
(put 'wand-trim 'menu-name "Trim Edges")

(defun wand-rotate (arg)
  "Rotate image to ARG degrees.
If ARG is positive then rotate in clockwise direction.
If negative then to the opposite."
  (interactive "nDegrees: ")
  (wand--operation-apply 'rotate image-wand arg)
  (wand--redisplay))
(put 'wand-rotate 'can-preview :RotatePreview)
(put 'wand-rotate 'transform-operation t)
(put 'wand-rotate 'menu-name "Rotate")

(defun wand-rotate-left (arg)
  "Rotate image to the left.
If ARG is specified then rotate on ARG degree."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         90)))
  (wand-rotate (- arg)))

(defun wand-rotate-right (arg)
  "Rotate image to the right.
If ARG is specified then rotate on ARG degree."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         90)))
  (wand-rotate arg))

(defun wand-raise (arg)
  "Create button-like 3d effect."
  (interactive "P")
  (wand--operation-apply 'raise image-wand arg)
  (wand--redisplay))
(put 'wand-raise 'transform-operation t)
(put 'wand-raise 'menu-name "3D Button Effect")

;;}}}

;;{{{ Effect operations

(defun wand-radial-blur (arg)
  "Blur the image radially by ARG degree."
  (interactive (list (read-number "Blur radius: " 2.0)))
  (wand--operation-apply 'radial-blur image-wand arg)
  (wand--redisplay))
(put 'wand-radial-blur 'effect-operation t)
(put 'wand-radial-blur 'menu-name "Radial Blur")

(defun wand-motion-blur (radius sigma angle)
  "Apply motion blur the image using RADIUS, SIGMA and ANGLE."
  (interactive (list (read-number "Radius: " 1)
                     (read-number "Sigma: " wand-sigma)
                     (read-number "Angle: " 2)))
  (wand--operation-apply 'motion-blur image-wand radius sigma angle)
  (wand--redisplay))
(put 'wand-motion-blur 'effect-operation t)
(put 'wand-motion-blur 'menu-name "Motion Blur")

(defun wand-gaussian-blur (radius sigma)
  "Apply gaussian blur of RADIUS and SIGMA to the image."
  (interactive (list (read-number "Radius: " 1)
                     (read-number "Sigma: " wand-sigma)))
  (wand--operation-apply 'gauss-blur image-wand radius sigma)
  (wand--redisplay))
(put 'wand-gaussian-blur 'can-preview "blur")
(put 'wand-gaussian-blur 'effect-operation t)
(put 'wand-gaussian-blur 'menu-name "Gaussian Blur")

(defun wand-shadow (opacity sigma x-off y-off)
  "Sharpen image with by RADIUS and SIGMA."
  (interactive (list (read-number "Opacity in %: " 50)
                     (read-number "Sigma: " wand-sigma)
                     (read-number "Shadow x-offset: " 4)
                     (read-number "Shadow y-offset: " 4)))
  (wand--operation-apply 'shadow image-wand opacity sigma x-off y-off)
  (wand--redisplay))
(put 'wand-shadow 'effect-operation t)
(put 'wand-shadow 'menu-name "shadow")

(defun wand-sharpen (radius sigma)
  "Sharpen image with by RADIUS and SIGMA."
  (interactive (list (read-number "Radius: " 1)
                     (read-number "Sigma: " wand-sigma)))
  (wand--operation-apply 'sharpen image-wand radius sigma)
  (wand--redisplay))
(put 'wand-sharpen 'can-preview "sharpen")
(put 'wand-sharpen 'effect-operation t)
(put 'wand-sharpen 'menu-name "Sharpen")

(defun wand-despeckle ()
  "Despeckle image."
  (interactive)
  (wand--operation-apply 'despeckle image-wand)
  (wand--redisplay))
(put 'wand-despeckle 'can-preview "despeckle")
(put 'wand-despeckle 'effect-operation t)
(put 'wand-despeckle 'menu-name "Despeckle")

(defun wand-edge (radius)
  "Enhance edges of the image by RADIUS.
Default is 1."
  (interactive (list (read-number "Radius: " 1)))
  (wand--operation-apply 'edge image-wand radius)
  (wand--redisplay))
(put 'wand-edge 'can-preview "edgedetect")
(put 'wand-edge 'effect-operation t)
(put 'wand-edge 'menu-name "Edge Detect")

(defun wand-emboss (radius sigma)
  "Emboss the image with RADIUS and SIGMA."
  (interactive (list (read-number "Radius: " 1.0)
                     (read-number "Sigma: " wand-sigma)))
  (wand--operation-apply 'emboss image-wand radius sigma)
  (wand--redisplay))
(put 'wand-emboss 'effect-operation t)
(put 'wand-emboss 'menu-name "Emboss")

(defun wand-reduce-noise (radius)
  "Reduce the noise with RADIUS.
Default is 1."
  (interactive (list (read-number "Noise reduce radius: " 1)))
  (wand--operation-apply 'reduce-noise image-wand radius)
  (wand--redisplay))
(put 'wand-reduce-noise 'can-preview :ReduceNoisePreview)
(put 'wand-reduce-noise 'effect-operation t)
(put 'wand-reduce-noise 'menu-name "Reduce Noise")

(defun wand-add-noise (noise-type)
  "Add noise of NOISE-TYPE."
  (interactive
   (list (completing-read "Noise type [poisson]: "
                          MagickNoiseTypes
                          nil t nil nil "poisson")))
  (wand--operation-apply 'add-noise image-wand noise-type)
  (wand--redisplay))
(put 'wand-add-noise 'effect-operation t)
(put 'wand-add-noise 'menu-name "Add Noise")

(defun wand-spread (radius)
  "Spread image pixels with RADIUS."
  (interactive (list (read-number "Spread radius: " 1.0)))
  (wand--operation-apply 'spread image-wand radius)
  (wand--redisplay))
(put 'wand-spread 'effect-operation t)
(put 'wand-spread 'menu-name "Spread")

;;}}}

;;{{{ Enhance operations

(defun wand-contrast (ctype)
  "Increase or decrease contrast.
By default increase."
  (interactive (list (completing-read
                      "Contrast (default increase): "
                      '(("increase") ("decrease"))
                      nil t nil nil "increase")))
  (wand--operation-apply 'contrast image-wand
                         (intern-soft (concat ":" ctype)))
  (wand--redisplay))
(put 'wand-contrast 'enhance-operation t)
(put 'wand-contrast 'menu-name "Contrast")

(defun wand-sigmoidal-contrast (ctype strength midpoint)
  "Apply sigmoidal contrast adjustement."
  (interactive (list (completing-read
                      "Contrast (default increase): "
                      '(("increase") ("decrease"))
                      nil t nil nil "increase")
                     (read-number "Strength: " 5)
                     (read-number "Midpoint in %: " 0)))
  (wand--operation-apply 'sigmoidal-contrast image-wand
                         (intern-soft (concat ":" ctype))
                         strength midpoint)
  (wand--redisplay))
(put 'wand-sigmoidal-contrast 'enhance-operation t)
(put 'wand-sigmoidal-contrast 'menu-name "Sigmoidal Contrast")

(defun wand-normalize ()
  "Normalize image."
  (interactive)
  (wand--operation-apply 'normalize image-wand)
  (wand--redisplay))
(put 'wand-normalize 'enhance-operation t)
(put 'wand-normalize 'menu-name "Normalize")

(defun wand-enhance ()
  "Enhance image."
  (interactive)
  (wand--operation-apply 'enhance image-wand)
  (wand--redisplay))
(put 'wand-enhance 'enhance-operation t)
(put 'wand-enhance 'menu-name "Enhance")

(defun wand-equalize ()
  "Equalise image."
  (interactive)
  (wand--operation-apply 'equalize image-wand)
  (wand--redisplay))
(put 'wand-equalize 'enhance-operation t)
(put 'wand-equalize 'menu-name "Equalize")

(defun wand-negate (arg)
  "Negate image.
If prefix ARG is specified then negate by grey."
  (interactive "P")
  (wand--operation-apply 'negate image-wand arg)
  (wand--redisplay))
(put 'wand-negate 'enhance-operation t)
(put 'wand-negate 'menu-name "Negate")

(defun wand-grayscale ()
  "Convert image to grayscale colorspace."
  (interactive)
  (wand--operation-apply 'grayscale image-wand)
  (wand--redisplay))
(put 'wand-grayscale 'enhance-operation t)
(put 'wand-grayscale 'menu-name "Grayscale")

(defun wand-modulate (type inc)
  "Modulate image's brightness, saturation or hue."
  (interactive (let* ((tp (completing-read
                           "Modulate [saturation]: "
                           '(("brightness") ("saturation") ("hue"))
                           nil t nil nil "saturation"))
                      (tinc (read-number (format "Increase %s in %%: " tp) 25)))
                 (list (cond ((string= tp "brightness") :brightness)
                             ((string= tp "hue") :hue)
                             (t :saturation)) tinc)))
  (wand--operation-apply 'modulate image-wand type inc)
  (wand--redisplay))
(put 'wand-modulate 'enhance-operation t)
(put 'wand-modulate 'menu-name "Modulate")

;;}}}

;;{{{ F/X operations

(defun wand-preview-op (op)
  "Preview some operation OP with 8 subnails."
  (interactive (list (completing-read "Operation: "
			MagickPreviewTypes nil t)))
  (wand--redisplay (wand--operation-apply 'preview-op image-wand op)))
(put 'wand-preview-op 'f/x-operation t)
(put 'wand-preview-op 'menu-name "Preview operation")

(defun wand-solarize (sf)
  "Solarise image with solarize factor SF."
  (interactive (list (read-number "Solarize in %: " 50)))
  (wand--operation-apply 'solarize image-wand
                         (* (Wand:quantum-range) (/ sf 100.0)))
  (wand--redisplay))
(put 'wand-solarize 'can-preview "solarize")
(put 'wand-solarize 'f/x-operation t)
(put 'wand-solarize 'menu-name "Solarize")

(defun wand-swirl (degrees)
  "Swirl the image by DEGREES."
  (interactive (list (read-number "Degrees: " 90)))
  (wand--operation-apply 'swirl image-wand degrees)
  (wand--redisplay))
(put 'wand-swirl 'f/x-operation t)
(put 'wand-swirl 'menu-name "Swirl")

(defun wand-oil-paint (radius)
  "Simulate oil painting with RADIUS for the image.
Default radius is 3."
  (interactive (list (read-number "Radius: " 2.5)))
  (wand--operation-apply 'oil image-wand radius)
  (wand--redisplay))
(put 'wand-oil-paint 'can-preview "oilpaint")
(put 'wand-oil-paint 'f/x-operation t)
(put 'wand-oil-paint 'menu-name "Oil Paint")

(defun wand-charcoal (radius sigma)
  "Simulate charcoal painting for the image.
If prefix ARG is specified then radius for charcoal painting is ARG.
Default is 1."
  (interactive (list (read-number "Radius: " 1.0)
		     (read-number "Sigma: " wand-sigma)))
  (wand--operation-apply 'charcoal image-wand radius sigma)
  (wand--redisplay))
(put 'wand-charcoal 'can-preview "charcoal-drawing")
(put 'wand-charcoal 'f/x-operation t)
(put 'wand-charcoal 'menu-name "Charcoal Draw")

(defun wand-sepia-tone (threshold)
  "Apply sepia tone to image by THRESHOLD."
  (interactive (list (read-number "Threshold in %: " 80)))
  (wand--operation-apply 'sepia-tone image-wand
                         (* (Wand:quantum-range) (/ threshold 100.0)))
  (wand--redisplay))
(put 'wand-sepia-tone 'f/x-operation t)
(put 'wand-sepia-tone 'menu-name "Sepia Tone")

(defun wand-implode (radius)
  "Implode image by RADIUS.
RADIUS range is [-1.0, 1.0]."
  (interactive (list (read-number "Radius: " 0.3)))
  (wand--operation-apply 'implode image-wand radius)
  (wand--redisplay))
(put 'wand-implode 'f/x-operation t)
(put 'wand-implode 'menu-name "Implode")

(defun wand-vignette (bw)
  "Create vignette using image."
  (interactive (list (read-number "Black/White: " 10)))
  (wand--operation-apply 'vignette image-wand bw bw 0 0)
  (wand--redisplay))
(put 'wand-vignette 'f/x-operation t)
(put 'wand-vignette 'menu-name "Vignette")

(defun Wand-mode-wave (amplitude wave-length)
  "Create wave effect on image with AMPLITUDE and WAVE-LENGTH."
  (interactive (list (read-number "Amplitude [2]: " nil "2")
		     (read-number "Wave length [10]: " nil "10")))
  (Wand-operation-apply 'wave image-wand amplitude wave-length)
  (Wand-redisplay))
(put 'Wand-mode-wave 'f/x-operation t)
(put 'Wand-mode-wave 'menu-name "Wave")

;;}}}

;;{{{ Region commands

(defsubst wand--mouse-release-p (event)
  (and (consp event) (symbolp (car event))
       (memq 'click (get (car event) 'event-symbol-elements))))

(defsubst wand--motion-event-p (event)
  (and (consp event) (symbolp (car event))
       (memq 'drag (get (car event) 'event-symbol-elements))))

;; TODO
(defun wand-select-region (event)
  "Select region."
  (interactive "e")
  (message "EVENT: %S / %S" (posn-x-y (event-start event)) (event-end event))
  (let* ((gc-cons-threshold most-positive-fixnum) ; inhibit gc
         (s-xy (posn-x-y (event-start event)))
         (sx (car s-xy)) (sy (cdr s-xy))
         (had-preview-region preview-region))
    (setq preview-region (list 0 0 (car s-xy) (cdr s-xy)))
    (track-mouse
      (while (not (wand--mouse-release-p (setq event (read-event))))
        (when (wand--motion-event-p event)
          (let* ((m-xy (posn-x-y (event-start event)))
                 (mx (car m-xy)) (my (cdr m-xy)))
            (setq preview-region
                  (list (abs (- sx mx)) (abs (- sy my))
                        (min sx mx) (min sy my)))
            ;; Update info and preview image
            (wand--update-file-info)
            (let ((pwr (wand--preview-with-region)))
              (unwind-protect
                  (progn
                    (delete-region (1- (point-max)) (point-max))
                    (Wand:insert-emacs-image
                     (Wand:preview-emacs-image pwr) wand-mode-map))
                (Wand:delete-wand pwr))))))
      )

    (if (and (positivep (nth 0 preview-region))
             (positivep (nth 1 preview-region)))
        ;; Save region
        (setq last-preview-region preview-region)

      (setq preview-region nil)
      (if had-preview-region
          (progn
            ;; Remove any regions
            (wand--update-file-info)
            (delete-region (1- (point-max)) (point-max))
            (Wand:insert-emacs-image
             (Wand:preview-emacs-image preview-wand) wand-mode-map))

        ;; Otherwise pickup color
        (let* ((col (Wand:get-rgb-pixel-at preview-wand sx sy))
               (pickup-color (cons (cons sx sy) col)))
          (declare (special pickup-color))
          (Wand-mode-update-info))))))

(defun wand-activate-region ()
  "Activate last preview-region."
  (interactive)
  (setq preview-region last-preview-region)
  (wand--redisplay))

;; TODO
(defun wand-drag-image (event)
  "Drag image to view unshown part of the image."
  (interactive "e")
  (let ((gc-cons-threshold most-positive-fixnum) ; inhibit gc
	(sx (event-glyph-x-pixel event))
	(sy (event-glyph-y-pixel event))
	(pw (Wand:image-width preview-wand))
	(ph (Wand:image-height preview-wand))
	(mouse-down t))
    (while mouse-down
      (setq event (next-event event))
      (if (or (motion-event-p event) (button-release-event-p event))
	  (let ((off-x (+ (- sx (event-glyph-x-pixel event))
			  (or (get preview-wand 'offset-x) 0)))
		(off-y (+ (- sy (event-glyph-y-pixel event))
			  (or (get preview-wand 'offset-y) 0))))
	    (when (< off-x 0) (setq off-x 0))
	    (when (< off-y 0) (setq off-y 0))
	    (Wand-mode-update-file-info)
	    (if (motion-event-p event)
		(set-extent-end-glyph
		 preview-extent (Wand:glyph-internal
				 preview-wand off-x off-y
				 (- pw off-x) (- ph off-y)))

	      ;; Button released
	      (setq mouse-down nil)
	      (put preview-wand 'offset-x off-x)
	      (put preview-wand 'offset-y off-y)))

	(dispatch-event event)))))

(defun wand-crop (region)
  "Crop image to selected region."
  (interactive (list (wand--image-region)))
  (wand--operation-apply 'crop image-wand region)
  (setq preview-region nil)
  (wand--redisplay))
(put 'wand-crop 'region-operation t)
(put 'wand-crop 'menu-name "Crop")

(defun wand-chop (region)
  "Chop region from the image."
  (interactive (list (wand--image-region)))
  (wand--operation-apply 'chop image-wand region)
  (setq preview-region nil)
  (wand--redisplay))
(put 'wand-chop 'region-operation t)
(put 'wand-chop 'menu-name "Chop")

(defun wand-redeye-remove (region)
  "Remove red from the selected region."
  (interactive (list (wand--image-region)))
  (let ((gc-cons-threshold most-positive-fixnum)) ; inhibit gc
    (wand--operation-apply 'redeye-remove image-wand region)
    (setq preview-region nil)
    (wand--redisplay)))
(put 'wand-redeye-remove 'region-operation t)
(put 'wand-redeye-remove 'menu-name "Remove red eye")

;;}}}

;;{{{ Zooming/Sampling

(defun wand-zoom (factor)
  "Zoom in/out by FACTOR."
  (interactive
   (list (read-number "Zoom by factor: " wand-zoom-factor)))

  (wand--operation-apply 'zoom image-wand factor)
  (wand--redisplay))
(put 'wand-zoom 'transform-operation t)
(put 'wand-zoom 'menu-name "Zoom")

(defun wand-zoom-in ()
  "Zoom image in by `wand-zoom-factor' factor."
  (interactive)
  (wand-zoom wand-zoom-factor))

(defun wand-zoom-out ()
  "Zoom image out by `wand-zoom-factor'."
  (interactive)
  (wand-zoom (- wand-zoom-factor)))

(defun wand-scale (w h)
  "Scale image to WxH."
  (interactive
   (list (read-number "Width: " (Wand:image-width image-wand))
         (read-number "Height: " (Wand:image-height image-wand))))
  (wand--operation-apply 'scale image-wand w h)
  (wand--redisplay))
(put 'wand-scale 'transform-operation t)
(put 'wand-scale 'menu-name "Scale")

(defun wand-sample (w h)
  "Sample image to WxH size."
  (interactive
   (list (read-number "Width: " (Wand:image-width image-wand))
         (read-number "Height: " (Wand:image-height image-wand))))
  (wand--operation-apply 'sample image-wand w h)
  (wand--redisplay))
(put 'wand-sample 'transform-operation t)
(put 'wand-sample 'menu-name "Sample")

(defun wand-fit-size (w h)
  "Resize image to fit into WxH size."
  (interactive
   (let* ((dw (read-number "Width: " (Wand:image-width image-wand)))
          (dh (round (* (Wand:image-height image-wand)
                        (/ (float dw) (Wand:image-width image-wand))))))
     (list dw (read-number "Height: " dh))))

  (wand--operation-apply 'fit-size image-wand w h)
  (wand--redisplay))
(put 'wand-fit-size 'transform-operation t)
(put 'wand-fit-size 'menu-name "Fit to size")

(defun wand-liquid-rescale (w h)
  "Rescale image to WxH using liquid rescale."
  (interactive
   (list (read-number "Width: " (Wand:image-width image-wand))
         (read-number "Height: " (Wand:image-height image-wand))))

  (wand--operation-apply 'liquid-rescale image-wand w h)
  (wand--redisplay))
(put 'wand-liquid-rescale 'transform-operation t)
(put 'wand-liquid-rescale 'menu-name "Liquid rescale")

(defun wand-posterize (levels &optional ditherp)
  "Posterize image.
Levels is a  number of color levels allowed in each channel.
2, 3, or 4 have the most visible effect."
  (interactive "nLevel: \nP")
  (wand--operation-apply 'posterize image-wand levels (not (not ditherp)))
  (wand--redisplay))
(put 'wand-posterize 'transform-operation t)
(put 'wand-posterize 'menu-name "Posterize")

(defun wand-gamma (level)
  "Perform gamma correction.
LEVEL is a positive float.
LEVEL value of 1.00 (read 100%) is no-op."
  (interactive "nLevel: ")
  (wand--operation-apply 'gamma image-wand level)
  (wand--redisplay))
(put 'wand-gamma 'transform-operation t)
(put 'wand-gamma 'menu-name "Gamma")

(defun wand-pattern (pattern &optional op)
  "Enable checkerboard as tile background."
  (interactive (list (completing-read "Pattern: " wand--patterns nil t)
                     (if current-prefix-arg
                         (completing-read "Composite Op: "
                                          WandCompositeOperators nil t)
                       wand-pattern-composite-op)))
  (wand--operation-apply 'pattern image-wand pattern op)
  (wand--redisplay))
(put 'wand-pattern 'transform-operation t)
(put 'wand-pattern 'menu-name "Pattern")

;;}}}

;;{{{ Listings

(defun wand-list-composite-ops ()
  "Show composite operations.
A-la `list-colors-display'."
  (interactive)
  (Wand-with-drawing-wand d-in
    (Wand-with-pixel-wand pw
      (setf (Wand:pixel-color pw) "red")
      (setf (Wand:draw-fill-color d-in) pw))
    (Wand:draw-rectangle d-in 0.0 4.0 26.0 26.0)

    (Wand-with-drawing-wand d-out
      (Wand-with-pixel-wand pw
        (setf (Wand:pixel-color pw) "blue")
        (setf (Wand:draw-fill-color d-out) pw))
      (Wand:draw-rectangle d-out 10.0 0.0 42.0 32.0)

      (Wand-with-wand w-out
        (setf (Wand:image-size w-out)
              (cons 80 (face-height 'default)))
        (Wand:MagickReadImage w-out "pattern:horizontal")
        (Wand:MagickDrawImage w-out d-out)

        (cl-flet ((draw-in-out (cop)
                    (Wand-with-wand w-in
                      (setf (Wand:image-size w-in)
                            (cons 80 (face-height 'default)))
                      (Wand:MagickReadImage w-in "pattern:vertical")
                      (Wand:MagickDrawImage w-in d-in)
                      (Wand:image-composite w-in w-out (cdr cop) 0 0)
                      (Wand:insert-emacs-image (Wand:emacs-image w-in))
                      (insert " " (car cop) "\n"))))
          (with-output-to-temp-buffer "*Wand-Composite-Ops*"
            (set-buffer standard-output)
            (mapc #'draw-in-out WandCompositeOperators)))))))

(defvar wand--patterns
  (mapcar (lambda (x) (list (symbol-name x)))
         '(bricks checkerboard circles crosshatch crosshatch30 crosshatch45
           fishscales gray0 gray5 gray10 gray15 gray20 gray25 gray30
           gray35 gray40 gray45 gray50 gray55 gray60 gray65 gray70
           gray75 gray80 gray85 gray90 gray95 gray100 hexagons horizontal
           horizontalsaw hs_bdiagonal hs_cross
           hs_diagcross hs_fdiagonal hs_horizontal hs_vertical left30
           left45 leftshingle octagons right30 right45 rightshingle
           smallfishscales vertical verticalbricks
           verticalleftshingle verticalrightshingle verticalsaw)))

(defun wand-list-patterns ()
  "Show available patterns in separate buffer.
A-la `list-colors-display'."
  (interactive)
  (with-output-to-temp-buffer "*Wand-Patterns*"
    (cl-flet ((draw-pattern (pat-name)
                (Wand:insert-emacs-image
                 (Wand-with-wand wand
                   (setf (Wand:image-size wand)
                         (cons 80 (line-pixel-height)))
                   (Wand:read-image-data wand (concat "pattern:" pat-name))
                   (Wand:emacs-image wand)))
                (insert " " pat-name "\n")))
      (save-excursion
        (set-buffer standard-output)
        (mapc #'draw-pattern (mapcar #'car wand--patterns))))))
(put 'wand-list-patterns 'transform-operation t)
(put 'wand-list-patterns 'menu-name "List Patterns")
;;}}}


;;{{{ Toggle fit, Undo/Redo, Saving

(defun wand-toggle-fit ()
  "Toggle autofit."
  (interactive)
  (put 'image-wand 'fitting (not (get 'image-wand 'fitting)))
  (wand--redisplay))

(defun wand-undo (&optional arg)
  "Undo last operation ARG times."
  (interactive "p")
  (unless operations-list
    (error "Nothing to undo"))
  (dotimes (n arg)
    (push (car (last operations-list)) undo-list)
    (setq operations-list (butlast operations-list)))

  ;; Update wand keeping current page
  (let ((page (Wand:iterator-index image-wand)))
    (Wand:clear-wand image-wand)
    (Wand:read-image image-wand buffer-file-name)
    (setf (Wand:iterator-index image-wand) page))

  (wand--operation-list-apply image-wand)
  (wand--redisplay)
  (message "Undo!"))

(defun wand-redo (&optional arg)
  "Redo last operations ARG times."
  (interactive "p")
  (unless undo-list
    (error "Nothing to redo"))
  (dotimes (n arg)
    (let ((op (pop undo-list)))
      (when op
        (apply #'wand--operation-apply (car op) image-wand (cdr op)))))
  (wand--redisplay)
  (message "Redo!"))

(defun wand-edit-operations ()
  "Edit and reapply operations list."
  (interactive)
  (let* ((print-level nil)
         (ops-as-string (if operations-list
                            (prin1-to-string operations-list)
                          ""))
         (new-oplist (read-from-minibuffer
                      "Operations: " ops-as-string read-expression-map
                      t ops-as-string)))

    ;; Cut&Paste from undo
    (let ((page (Wand:iterator-index image-wand)))
      (Wand:clear-wand image-wand)
      (Wand:read-image image-wand buffer-file-name)
      (setf (Wand:iterator-index image-wand) page))

    (setq operations-list new-oplist)
    (wand--operation-list-apply image-wand)
    (wand--redisplay)))

(defun wand-repeat-last-operation ()
  "Repeat last operation on image."
  (interactive)
  (let ((last-op (car (last operations-list))))
    (when last-op
      (apply #'wand--operation-apply
             (car last-op) image-wand (cdr last-op))
      (wand--redisplay))))

(defun wand-global-operations-list (arg)
  "Fix operations list to be global for all images.
If prefix ARG is supplied, then global operations list is reseted.
Useful to skim over images in directory applying operations, for
example zoom."
  (interactive "P")
  (setq wand-global-operations-list
        (and (not arg) operations-list))
  (wand--redisplay))

(defun wand-write-file (format nfile)
  "Write file using output FORMAT."
  (interactive
   (let* ((ofmt (completing-read
                 (format "Output Format [%s]: "
                         (Wand:image-format image-wand))
                 (mapcar #'list (Wand-formats-list "*" 'write))
                 nil t nil nil (Wand:image-format image-wand)))
          (nfname (concat (file-name-sans-extension buffer-file-name)
                          "." (downcase ofmt)))
          (fn (read-file-name
               "Filename: "
               (file-name-directory buffer-file-name)
               nfname nil (file-name-nondirectory nfname))))
     (list ofmt fn)))

  (unless (Wand-format-supported-for-write-p format)
    (error "Unsupported format for writing: %s" format))

  (when (or (not wand-query-for-overwrite)
            (not (file-exists-p nfile))
            (y-or-n-p (format "File %s exists, overwrite? " nfile)))
    (setf (Wand:image-format image-wand) format)
    (Wand:write-image image-wand nfile)
    (message "File %s saved" nfile)

    ;; Redisplay in case we can do it
    (if (Wand-format-supported-for-read-p format)
        (wand-display nfile)
      (find-file nfile))))

(defun wand-save-file (nfile)
  "Save current wand to file NFILE.
Output format determined by NFILE extension, and no sanity checks
performed, use `wand-write-file' if not sure."
  (interactive
   (list (read-file-name "Filename: "
                         (file-name-directory buffer-file-name)
                         buffer-file-name nil
                         (file-name-nondirectory buffer-file-name))))
  (wand-write-file
   (upcase (file-name-extension nfile)) nfile))

;;}}}

(provide 'ffi-wand)

;; now initialise the environment
(when (fboundp 'Wand:MagickWandGenesis)
  (Wand:MagickWandGenesis))

;;; ffi-wand.el ends here
