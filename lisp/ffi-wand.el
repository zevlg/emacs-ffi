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

(define-ffi-array Wand-Array4096Type :char 4096)

(defvar Wand-BooleanType :long)

(define-ffi-struct Wand-PrivateType
  (id :type :ulong)
  (name :type Wand-Array4096Type)
  (exception :type :pointer)
  (image-info :type :pointer)           ; ImageInfo*
  (quantize-info :type :pointer)
  (images :type :pointer)
  (active :type Wand-BooleanType)
  (pend :type Wand-BooleanType)
  (debug :type Wand-BooleanType)
  (signature :type :ulong))

(define-ffi-struct Wand-InfoType
  (name :type :pointer)
  (description :type :pointer)
  (version :type :pointer)
  (note :type :pointer)
  (module :type :pointer)

  (image-info :type :pointer)
  (decoder :type :pointer)
  (encoder :type :pointer)

  (magick :type :pointer)                      ; IsImageFormatHandler
  (client-date :type :pointer)

  (adjoin :type Wand-BooleanType)
  (raw :type Wand-BooleanType)
  (endian-support :type Wand-BooleanType)
  (blob-support :type Wand-BooleanType)
  (seekable-stream :type Wand-BooleanType)
  (thread-support :type :uint)
  (stealth :type Wand-BooleanType)

  ;; deprecated, use GetMagickInfoList()
  (previous :type :pointer)
  (next :type :pointer)

  (signature :type :ulong))

(defconst Wand-ExceptionType :int)
(define-ffi-struct Wand-ExceptionInfoType
  (severity :type Wand-ExceptionType)
  (error_number :type :int)
  (reason :type :pointer)
  (description :type :pointer)
  (exceptions :type :pointer)
  (relinquish :type Wand-BooleanType)
  (semaphore :type :pointer)
  (signature :type :ulong))

(define-ffi-struct Wand-PointInfoType
  (x :type :double)
  (y :type :double))

(defconst Wand-OrientationType :int)
(defmacro Wand-Orientation-get (kw)
  (ecase kw
    ((:topleft :top-left) 1)
    ((:topright :top-right) 2)
    ((:bottomright :bottom-right) 3)
    ((:bottomleft :bottom-left) 4)
    ((:lefttop :left-top) 5)
    ((:righttop :right-top) 6)
    ((:rightbottom :right-bottom) 7)
    ((:leftbottom :left-bottom) 8)))

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

(defconst Wand-WandType :pointer)

(define-ffi-function Wand:acquire-id "AcquireWandId" :size_t
  nil libmagickwand)

(define-ffi-function Wand:relinquish-id "RelinquishWandId" :void
  (:size_t) libmagickwand)

;; Return a newly allocated MagickWand.
(define-ffi-function Wand:make-wand "NewMagickWand" Wand-WandType
  nil libmagickwand)

;; Clear all resources associated with the WAND.
;; This does not free the memory, i.e. @var{wand} can furtherly be used
;; as a context, see `Wand:delete-wand'."
(define-ffi-function Wand:clear-wand "ClearMagickWand" :void
  (Wand-WandType) libmagickwand)

;; Return a cloned copy of WAND.
(define-ffi-function Wand:copy-wand "CloneMagickWand" Wand-WandType
  (Wand-WandType) libmagickwand)

;; Gets the image at the current image index.
(define-ffi-function Wand:get-image "MagickGetImage" Wand-WandType
  (Wand-WandType) libmagickwand)

;; Extracts a region of the image and returns it as a a new wand.
(define-ffi-function Wand:image-region "MagickGetImageRegion" Wand-WandType
  ;; wand dx dy x y
  (Wand-WandType :ulong :ulong :ulong :ulong) libmagickwand)

;; Delete the WAND.
;; This frees all resources associated with the WAND.
;; WARNING: Do not use WAND after calling this function!
(define-ffi-function Wand:destroy-wand "DestroyMagickWand" :void
  (Wand-WandType) libmagickwand)

(defun Wand:delete-wand (wand)
  ;; Workaround some ugly bug, causing
  ;; magick/semaphore.c:290: LockSemaphoreInfo: Assertion `semaphore_info != (SemaphoreInfo *) ((void *)0)' failed
  ;;  - make sure wand_semaphore is ok
  (Wand:acquire-id)
  (Wand:destroy-wand wand))

;; Return non-nil if WAND is a magick wand, nil otherwise.
(define-ffi-function Wand:wandp "IsMagickWand" Wand-BooleanType
  (Wand-WandType) libmagickwand)

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
  (Wand-WandType) libmagickwand)

(defun Wand:identify-image (wand)
  "Return info about the image stored in WAND."
  (let ((ii (Wand:MagickIdentifyImage wand)))
    (unless (ffi-pointer-null-p ii)
      (unwind-protect
          (ffi-get-c-string ii)
        (Wand:RelinquishMemory ii)))))

(define-ffi-function Wand:MagickGetException "MagickGetException" :pointer
  (Wand-WandType :pointer) libmagickwand)

(defun Wand:exception (wand)
  "Return reason of any error that occurs when using API."
  (with-ffi-temporary (c-ext Wand-ExceptionType)
    (ffi-get-c-string (Wand:MagickGetException wand c-ext))))

;;}}}

(define-ffi-function Wand:MagickReadImage "MagickReadImage" Wand-BooleanType
  (Wand-WandType :pointer) libmagickwand)

(defun Wand:read-image (wand file)
  "Read FILE and associate it with WAND.
Return non-nil if file has been loaded successfully."
  (let ((fname (expand-file-name file)))
    ;; simple error catchers
    (unless (file-readable-p fname)
      (error "File unreadable %s" fname))
    (when (zerop (Wand:wandp wand))
      (error "Non-wand arg: %S" wand))

    (with-ffi-string (fncstr fname)
      (when (zerop (Wand:MagickReadImage wand fncstr))
        (error "Can't read file %s: %s"
               file (Wand:exception image-wand))))
    t))

(defun Wand:read-image-data (wand data)
  (with-ffi-string (dtcstr data)
    (Wand:MagickReadImage wand dtcstr)))

(define-ffi-function Wand:MagickWriteImage "MagickWriteImage" Wand-BooleanType
  (Wand-WandType :pointer) libmagickwand)

(defun Wand:write-image (wand file)
  "Write the image associated with WAND to FILE."
  (let ((fname (expand-file-name file)))
    ;; simple error catchers
    (unless (file-writable-p fname)
      (error "File unwritable %s" fname))
    (when (zerop (Wand:wandp wand))
      (error "Non-wand arg: %S" wand))

    (with-ffi-string (fncstr fname)
      (Wand:MagickWriteImage wand fncstr))))

(define-ffi-function Wand:GetImageBlob "MagickGetImageBlob" :pointer
  (Wand-WandType :pointer) libmagickwand)

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
  Wand-BooleanType
  (Wand-WandType :pointer) libmagickwand)

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
    (error "Unsupported magick property: %s" prop))
  (with-ffi-string (cprop prop)
    (let ((ret (Wand:GetMagickProperty
                (ffi-null-pointer) (Wand-PrivateType-images wand)
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

;;{{{  `-- Image properties

(defun Wand-fetch-relinquish-strings (strs slen)
  "Fetch strings from strings array STRS of length SLEN."
  (unless (ffi-pointer-null-p strs)
    (unwind-protect
        (loop for off from 0 below slen
              collect (ffi-get-c-string (ffi-aref strs :pointer off)))
      (Wand:RelinquishMemory strs))))

(define-ffi-function Wand:MagickGetImageProperties "MagickGetImageProperties" :pointer
  (Wand-WandType :pointer :pointer) libmagickwand)

(defun Wand:image-properties (w pattern)
  "Return list of image properties that match PATTERN."
  (with-ffi-temporary (clen :ulong)
    (with-ffi-string (cptr pattern)
      (let ((props (Wand:MagickGetImageProperties w cptr clen)))
        (Wand-fetch-relinquish-strings props (ffi--mem-ref clen :ulong))))))

(define-ffi-function Wand:MagickGetImageProperty "MagickGetImageProperty" :pointer
  (Wand-WandType :pointer) libmagickwand)

(define-ffi-function Wand:MagickSetImageProperty "MagickSetImageProperty"
  Wand-BooleanType
  (Wand-WandType :pointer :pointer) libmagickwand)

(define-ffi-function Wand:MagickDeleteImageProperty "MagickDeleteImageProperty"
  Wand-BooleanType
  (Wand-WandType :pointer) libmagickwand)

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

(define-ffi-function Wand:MagickGetSize "MagickGetSize" Wand-BooleanType
  (Wand-WandType :pointer :pointer) libmagickwand)
(define-ffi-function Wand:MagickSetSize "MagickSetSize" Wand-BooleanType
  (Wand-WandType :ulong :ulong) libmagickwand)

(defun Wand:image-size (wand)
  "Return size of the image, associated with WAND."
  (with-ffi-temporaries ((w :ulong) (h :ulong))
    (unless (zerop (Wand:MagickGetSize wand w h))
      (cons (ffi--mem-ref w :ulong) (ffi--mem-ref h :ulong)))))
(defsetf Wand:image-size (wand) (size)
  `(Wand:MagickSetSize ,wand (car ,size) (cdr ,size)))

(define-ffi-function Wand:image-height "MagickGetImageHeight" :ulong
  (Wand-WandType) libmagickwand)
(define-ffi-function Wand:image-width "MagickGetImageWidth" :ulong
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:GetImageOrientation "MagickGetImageOrientation"
  Wand-OrientationType
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:SetImageOrientation "MagickSetImageOrientation"
  Wand-BooleanType
  (Wand-WandType Wand-OrientationType) libmagickwand)

(defun Wand:image-orientation (w)
  "Return orientation for the image hold by W.
Use \(setf \(Wand:image-orientation w\) orient\) to set new one."
  (Wand:GetImageOrientation w))

(defsetf Wand:image-orientation (w) (orient)
  `(Wand:SetImageOrientation ,w ,orient))

(defconst Wand-EndianType :int)
(defconst Wand-EndianUndefined 0)
(defconst Wand-EndianLSB 1)
(defconst Wand-EndianMSB 2)

(define-ffi-function Wand:GetImageEndian "MagickGetImageEndian"
  Wand-EndianType
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:SetImageEndian "MagickSetImageEndian"
  Wand-BooleanType
  (Wand-WandType Wand-EndianType) libmagickwand)

(defun Wand:image-endian (w)
  "Return endian for the image hold by W.
Use \(setf \(Wand:image-endian w\) endian\) to set new one."
  (Wand:GetImageEndian w))

(defsetf Wand:image-endian (w) (endian)
  `(Wand:SetImageEndian ,w ,endian))

(defconst Wand-ColorspaceType :int)
(defconst Wand-ColorspaceTypes
  '(("RGB" . 1) ("GRAY" . 2) ("Transparent" . 3) ("OHTA" . 4) ("Lab" . 5)
    ("XYZ" . 6) ("YCbCr" . 7) ("YCC" . 8) ("YIQ" . 9) ("YPbPr" . 10)
    ("YUV" . 11) ("CMYK" . 12) ("sRGB" . 13) ("HSB" . 14) ("HSL" . 15)
    ("HWB" . 16) ("Rec601Luma" . 17) ("Rec601YCbCr" . 18) ("Rec709Luma" . 19)
    ("Rec709YCbCr" . 20) ("Log" . 21) ("CMY" . 22)))

(define-ffi-function Wand:SetImageColorspace "MagickTransformImageColorspace"
  Wand-BooleanType
  (Wand-WandType Wand-ColorspaceType) libmagickwand)

;;}}}

;;{{{  `-- Image format operations

(define-ffi-function Wand:MagicGetFormat "MagickGetFormat" :pointer
  (Wand-WandType) libmagickwand)
(define-ffi-function Wand:MagickSetFormat "MagickSetFormat" Wand-BooleanType
  (Wand-WandType :pointer) libmagickwand)

(defun Wand:wand-format (w)
  (let ((ret (Wand:MagicGetFormat w)))
    (unless (ffi-pointer-null-p ret)
      (ffi-get-c-string ret))))

(defsetf Wand:wand-format (w) (nfmt)
  (let ((nfmtsym (cl-gensym)))
  `(with-ffi-string (,nfmtsym ,nfmt)
     (Wand:MagickSetFormat ,w ,nfmtsym))))

(define-ffi-function Wand:GetImageFormat "MagickGetImageFormat" :pointer
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:SetImageFormat "MagickSetImageFormat" Wand-BooleanType
  (Wand-WandType :pointer) libmagickwand)

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

(define-ffi-function Wand:GetMagickInfo "GetMagickInfo" :pointer
  ;; format exception
  (:pointer :pointer) libmagickwand)

(defun Wand:get-magick-info (fmt)
  (with-ffi-temporary (c-mexc Wand-ExceptionInfoType)
    (with-ffi-string (c-fmt fmt)
      (Wand:GetMagickInfo c-fmt c-mexc))))

(define-ffi-function Wand:GetMagickInfoList "GetMagickInfoList" :pointer
  ;; fmt-pattern &num exception
  (:pointer :pointer :pointer) libmagickwand)

;;}}}

;;{{{  `-- Images list operations

(define-ffi-function Wand:images-num "MagickGetNumberImages" :ulong
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:HasNextImage "MagickHasNextImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

(defsubst Wand:has-next-image (w)
  (= (Wand:HasNextImage w) 1))

(define-ffi-function Wand:next-image "MagickNextImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:has-prev-image "MagickHasPreviousImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:prev-image "MagickPreviousImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:iterator-index "MagickGetIteratorIndex" :long
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:MagickSetIteratorIndex "MagickSetIteratorIndex"
  Wand-BooleanType
  (Wand-WandType :long) libmagickwand)

(defsetf Wand:iterator-index (w) (idx)
  `(Wand:MagickSetIteratorIndex ,w ,idx))

(define-ffi-function Wand:set-first-iterator "MagickSetFirstIterator" :void
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:set-last-iterator "MagickSetLastIterator" :void
  (Wand-WandType) libmagickwand)

;;}}}

;;{{{  `-- PixelWand operations

(defvar Wand-PixelType :pointer)

(define-ffi-function Wand:NewPixelWand "NewPixelWand" Wand-PixelType
  nil libmagickwand)
(define-ffi-function Wand:DestroyPixelWand "DestroyPixelWand" Wand-PixelType
  (Wand-PixelType) libmagickwand)

(defmacro Wand-with-pixel-wand (pw &rest forms)
  "With allocated pixel wand PW do FORMS."
  `(let ((,pw (Wand:NewPixelWand)))
     (unwind-protect
         (progn ,@forms)
       (Wand:DestroyPixelWand ,pw))))
(put 'Wand-with-pixel-wand 'lisp-indent-function 'defun)

(define-ffi-function Wand:pixel-red "PixelGetRed" :double
  (Wand-PixelType) libmagickwand)
(define-ffi-function Wand:pixel-green "PixelGetGreen" :double
  (Wand-PixelType) libmagickwand)
(define-ffi-function Wand:pixel-blue "PixelGetBlue" :double
  (Wand-PixelType) libmagickwand)

(define-ffi-function Wand:PixelSetRed "PixelSetRed" :void
  (Wand-PixelType :double) libmagickwand)
(define-ffi-function Wand:PixelSetGreen "PixelSetGreen" :void
  (Wand-PixelType :double) libmagickwand)
(define-ffi-function Wand:PixelSetBlue "PixelSetBlue" :void
  (Wand-PixelType :double) libmagickwand)

(defsetf Wand:pixel-red (pw) (r)
  `(Wand:PixelSetRed ,pw ,r))
(defsetf Wand:pixel-green (pw) (g)
  `(Wand:PixelSetGreen ,pw ,g))
(defsetf Wand:pixel-blue (pw) (b)
  `(Wand:PixelSetBlue ,pw ,b))

(defun Wand:pixel-rgb-components (pw)
  "Return RGB components for pixel wand PW."
  (mapcar #'(lambda (c) (truncate (* (funcall c pw) 65535.0)))
          '(Wand:pixel-red Wand:pixel-green Wand:pixel-blue)))

(defsetf Wand:pixel-rgb-components (pw) (rgb)
  "For pixel wand PW set RGB components."
  `(mapcar* #'(lambda (sf c) (funcall sf ,pw (/ c 65535.0)))
            '(Wand:PixelSetRed Wand:PixelSetGreen Wand:PixelSetBlue)
            ,rgb))

;; PixelGetColorAsString() returns the color of the pixel wand as a
;; string.
(define-ffi-function Wand:PixelGetColorAsString "PixelGetColorAsString" :pointer
  (Wand-PixelType) libmagickwand)

(defun Wand:pixel-color (pw)
  (let ((pcs (Wand:PixelGetColorAsString pw)))
    (unless (ffi-pointer-null-p pcs)
      (ffi-get-c-string pcs))))

;; PixelSetColor() sets the color of the pixel wand with a string
;; (e.g. "blue", "#0000ff", "rgb(0,0,255)", "cmyk(100,100,100,10)",
;; etc.).
(define-ffi-function Wand:PixelSetColor "PixelSetColor" Wand-BooleanType
  (Wand-PixelType :pointer) libmagickwand)

(defsetf Wand:pixel-color (pw) (color)
  (let ((colcsym (cl-gensym)))
    `(with-ffi-string (,colcsym ,color)
       (Wand:PixelSetColor ,pw ,colcsym))))

;; PixelGetAlpha() returns the normalized alpha color of the pixel
;; wand.
(define-ffi-function Wand:pixel-alpha "PixelGetAlpha" :double
  (Wand-PixelType) libmagickwand)

;; PixelSetAlpha() sets the normalized alpha color of the pixel wand.
;; The level of transparency: 1.0 is fully opaque and 0.0 is fully
;; transparent.
(define-ffi-function Wand:PixelSetAlpha "PixelSetAlpha" :void
  (Wand-PixelType :double) libmagickwand)

(defsetf Wand:pixel-alpha (pw) (alpha)
  `(Wand:PixelSetAlpha ,pw ,alpha))

;;}}}

;;{{{  `-- DrawingWand operations

(defconst Wand-DrawingType :pointer)

(defconst Wand-PaintMethodType :int)
(defmacro Wand-PaintMethod-get (kw)
  (ecase kw
     (:point 1)
     (:replace 2)
     ((:floodfill :flood-fill) 3)
     ((:filltoborder :fill-to-border) 4)
     (:reset 5)))

;; MagickDrawImage() renders the drawing wand on the current image.
(define-ffi-function Wand:MagickDrawImage "MagickDrawImage" Wand-BooleanType
  (Wand-WandType Wand-DrawingType) libmagickwand)

(define-ffi-function Wand:MagickAnnotateImage "MagickAnnotateImage"
  Wand-BooleanType
  ;; wand draw x y angle text
  (Wand-WandType Wand-DrawingType :double :double :double :pointer) libmagickwand)

(define-ffi-function Wand:clear-drawing-wand "ClearDrawingWand" :void
  (Wand-DrawingType) libmagickwand)

(define-ffi-function Wand:copy-drawing-wand "CloneDrawingWand" Wand-DrawingType
  (Wand-DrawingType) libmagickwand)

(define-ffi-function Wand:delete-drawing-wand "DestroyDrawingWand" Wand-DrawingType
  (Wand-DrawingType) libmagickwand)

(define-ffi-function Wand:make-drawing-wand "NewDrawingWand" Wand-DrawingType
  nil libmagickwand)

(defmacro Wand-with-drawing-wand (dw &rest forms)
  "With allocated drawing wand DW do FORMS."
  `(let ((,dw (Wand:make-drawing-wand)))
     (unwind-protect
         (progn ,@forms)
       (Wand:delete-drawing-wand ,dw))))
(put 'Wand-with-drawing-wand 'lisp-indent-function 'defun)

(define-ffi-function Wand:draw-arc "DrawArc" :void
  ;; draw sx sy ex ey sd ed
  (Wand-DrawingType :double :double :double :double :double :double) libmagickwand)

(define-ffi-function Wand:draw-circle "DrawCircle" :void
  ;; draw ox oy px py
  (Wand-DrawingType :double :double :double :double) libmagickwand)

(define-ffi-function Wand:draw-rectangle "DrawRectangle" :void
  ;; draw ox oy ex ey
  (Wand-DrawingType :double :double :double :double) libmagickwand)

(define-ffi-function Wand:draw-round-rectangle "DrawRoundRectangle" :void
  ;; draw x1 y1 x2 y2 rx ry
  (Wand-DrawingType :double :double :double :double :double :double) libmagickwand)

(define-ffi-function Wand:draw-color "DrawColor" :void
  ;; draw x y paint-method
  (Wand-DrawingType :double :double Wand-PaintMethodType) libmagickwand)

(define-ffi-function Wand:DrawPolygon "DrawPolygon" :void
  ;; draw n-points (pointer PointInfo)
  (Wand-DrawingType :ulong :pointer) libmagickwand)

(define-ffi-function Wand:DrawPolyline "DrawPolyline" :void
  ;; draw n-points (pointer PointInfo)
  (Wand-DrawingType :ulong :pointer) libmagickwand)

(defmacro Wand-with-ffi-points (binding &rest body)
  (declare (indent defun))
  (let ((npo (cl-gensym)) (poi (cl-gensym)) (offseter (cl-gensym)))
    `(let (,offseter)
       (with-ffi-temporaries ((,poi PointInfo)
                              (,(car binding)
                               (* (length ,@(cdr binding))
                                  (ffi--type-size PointInfo))))
         (setq ,offseter ,(car binding))
         (dolist (,npo ,@(cdr binding))
           (setf (PointInfo-x ,poi) (float (car ,npo))
                 (PointInfo-y ,poi) (float (cdr ,npo)))
           (ffi--mem-set ,offseter PointInfo ,poi)
           (setq ,offseter
                 (ffi-pointer+ ,offseter (ffi--type-size PointInfo))))
         ,@body))))

(defun Wand:draw-lines (dw points)
  (Wand-with-ffi-points (c-pinfo points)
    (Wand:DrawPolyline dw (length points) c-pinfo)))

(define-ffi-function Wand:DrawGetFillColor "DrawGetFillColor" :void
  (Wand-DrawingType Wand-PixelType) libmagickwand)

(define-ffi-function Wand:DrawSetFillColor "DrawSetFillColor" :void
  (Wand-DrawingType Wand-PixelType) libmagickwand)

(defun Wand:draw-fill-color (dw)
  (let ((pw (Wand:NewPixelWand)))
    (Wand:DrawGetFillColor dw pw)
    pw))

(defsetf Wand:draw-fill-color (w) (p)
  `(Wand:DrawSetFillColor ,w ,p))

(define-ffi-function Wand:draw-fill-opacity "DrawGetFillOpacity" :double
  (Wand-DrawingType) libmagickwand)

(define-ffi-function Wand:DrawSetFillOpacity "DrawSetFillOpacity" :void
  (Wand-DrawingType :double) libmagickwand)

(defsetf Wand:draw-fill-opacity (w) (fo)
  `(Wand:DrawSetFillOpacity ,w ,fo))

(define-ffi-function Wand:DrawGetStrokeColor "DrawGetStrokeColor" :void
  (Wand-DrawingType Wand-PixelType) libmagickwand)

(define-ffi-function Wand:DrawSetStrokeColor "DrawSetStrokeColor" :void
  (Wand-DrawingType Wand-PixelType) libmagickwand)

(defun Wand:draw-stroke-color (dw)
  (let ((pw (Wand:NewPixelWand)))
    (Wand:DrawGetStrokeColor dw pw)
    pw))

(defsetf Wand:draw-stroke-color (w) (p)
  `(Wand:DrawSetStrokeColor ,w ,p))

(define-ffi-function Wand:draw-stroke-width "DrawGetStrokeWidth" :double
  (Wand-DrawingType) libmagickwand)

(define-ffi-function Wand:DrawSetStrokeWidth "DrawSetStrokeWidth" :void
  (Wand-DrawingType :double) libmagickwand)

(defsetf Wand:draw-stroke-width (dw) (sw)
  `(Wand:DrawSetStrokeWidth ,dw (float ,sw)))

(define-ffi-function Wand:draw-stroke-opacity "DrawGetStrokeOpacity" :double
  (Wand-DrawingType) libmagickwand)

(define-ffi-function Wand:DrawSetStrokeOpacity "DrawSetStrokeOpacity" :void
  (Wand-DrawingType :double) libmagickwand)

(defsetf Wand:draw-stroke-opacity (dw) (so)
  `(Wand:DrawSetStrokeOpacity ,dw ,so))

(define-ffi-function Wand:draw-stroke-antialias "DrawGetStrokeAntialias"
  Wand-BooleanType
  (Wand-DrawingType) libmagickwand)

(define-ffi-function Wand:DrawSetStrokeAntialias "DrawSetStrokeAntialias" :void
  (Wand-DrawingType Wand-BooleanType) libmagickwand)

(defsetf Wand:draw-stroke-antialias (dw) (aa)
  `(Wand:DrawSetStrokeAntialias ,dw (if ,aa 1 0)))

;;}}}

;;{{{  `-- Image pixels operations

(defvar Wand-StorageType :int)
(defmacro Wand-StorageType-get (kw)
  (ecase kw
    (:undefined-pixel 0)
    (:char-pixel 1)))

(define-ffi-function Wand:MagickExportImagePixels "MagickExportImagePixels" Wand-BooleanType
  (Wand-WandType
   :long                                ;from-width
   :long                                ;from-height
   :ulong                               ;delta-width
   :ulong                               ;delta-height
   :pointer                             ;map (c-string)
   Wand-StorageType
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
       mapncstr (Wand-StorageType-get :char-pixel) target)
      (cons rsize target))))

(defun Wand:get-image-pixels (wand)
  "Return WAND's raw string of image pixel data (RGB triples)."
  (Wand:get-image-pixels-internal
   wand 'rgb 0 0 (Wand:image-width wand) (Wand:image-height wand)))

(defun Wand:pixels-extract-colors (ss &optional n)
  "Extract colors from SS string.
Return list of lists of N int elements representing RBG(A) values."
  (let ((cls (cl-loop for i from 0 below (car ss)
               collect (ffi-aref (cdr ss) :uchar i)))
        (rls nil))
    (while cls
      (push (cl-subseq cls 0 (or n 3)) rls)
      (setq cls (nthcdr (or n 3) cls)))
    (nreverse rls)))

(defun Wand:get-image-rgb-pixels (wand x y w h)
  "Extract RGB pixels from WAND."
  (let ((pxd (Wand:get-image-pixels-internal
              wand 'rgb x y w h)))
    (unwind-protect
        (Wand:pixels-extract-colors pxd 3)
      (ffi-free (cdr pxd)))))

(defun Wand:get-rgb-pixel-at (wand x y)
  "Return WAND's RGB pixel at X, Y."
  (car (Wand:get-image-rgb-pixels wand x y 1 1)))

;; MagickConstituteImage() adds an image to the wand comprised of the
;; pixel data you supply. The pixel data must be in scanline order
;; top-to-bottom. The data can be char, short int, int, float, or
;; double. Float and double require the pixels to be normalized
;; [0..1], otherwise [0..Max], where Max is the maximum value the type
;; can accomodate (e.g. 255 for char). For example, to create a
;; 640x480 image from unsigned red-green-blue character data, use
(define-ffi-function Wand:MagickConstituteImage "MagickConstituteImage"
  Wand-BooleanType
  ;; width height map(string) storage-type pixels
  (Wand-WandType :ulong :ulong :pointer Wand-StorageType :pointer)
  libmagickwand)

(defun Wand:constitute-image (wand w h map pxl-type pixels)
  (let ((mapn-tsz (ecase pxl-type
                    (rgb (cons "RGB" 3))
                    (rgba (cons "RGBA" 4))
                    (bgr (cons "BGR" 3))
                    (bgra (cons "BGRA" 4))
                    (bgrp (cons "BGRP" 4)))))
    (with-ffi-string (c-map (car mapn-tsz))
      (with-ffi-temporary (c-pxls (ffi-allocate (* w h (cdr mapn-tsz))))
        (Wand:MagickConstituteImage
         wand w h c-map (Wand-StorageType-get :char-pixel) c-pxls)))))

;;}}}
;;{{{  `-- Image modification functions

(defvar Wand-FilterType :int)
(defvar Wand-FilterTypes
  '(("point" . 1) ("box" . 2) ("triangle" . 3) ("hermite" . 4)
    ("hanning" . 5) ("hamming" . 6) ("blackman" . 7) ("gaussian" . 8)
    ("quadratic" . 9) ("cubic" . 10) ("catrom" . 11) ("mitchell" . 12)
    ("jinc" . 13) ("sinc" . 14) ("sincfast" . 15) ("kaiser" . 16)
    ("welsh" . 17) ("parzen" . 18) ("bohman" . 19) ("bartlett" . 20)
    ("lagrange" . 21) ("lanczos" . 22) ("lanczossharp" . 23) ("lanczos2" . 24)
    ("lanczos2sharp" . 25) ("robidoux" . 26) ("robidouxsharp" . 27)
    ("cosine" . 28) ("spline" . 29) ("lanczosradius" . 30) ("sentinel" . 31)))

(defvar Wand-CompositeOperatorType :int)
(defconst Wand-CompositeOperators
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
    ("screen" . 45) ("softlight" . 46) ("src-atop" . 47) ("src" . 48)
    ("src-in" . 49) ("src-out" . 50) ("src-over" . 51) ("subtract" . 52)
    ("threshold" . 53) ("xor" . 54) ("divide" . 55)
    ))

(defvar Wand-NoiseType :int)
(defconst Wand-NoiseTypes
  '(("uniform" . 1) ("guassian" . 2) ("mult-gaussian" . 3)
    ("impulse" . 4) ("laplacian" . 5) ("poisson" . 6) ("random" . 7)))

(defvar Wand-PreviewType :int)
(defconst Wand-PreviewTypes
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

(define-ffi-function Wand:RotateImage "MagickRotateImage" Wand-BooleanType
  (Wand-WandType Wand-PixelType :double) libmagickwand)

;;Scale the image in WAND to the dimensions WIDTHxHEIGHT.
(define-ffi-function Wand:scale-image "MagickScaleImage" Wand-BooleanType
  (Wand-WandType :ulong :ulong) libmagickwand)

;; Sample the image
(define-ffi-function Wand:sample-image "MagickSampleImage" Wand-BooleanType
  (Wand-WandType :ulong :ulong) libmagickwand)

(define-ffi-function Wand:resize-image "MagickResizeImage" Wand-BooleanType
  (Wand-WandType :ulong :ulong Wand-FilterType
              :double                   ;blur
              )
  libmagickwand)

(ignore-errors
  (define-ffi-function Wand:liquid-rescale "MagickLiquidRescaleImage"
    Wand-BooleanType
    (Wand-WandType :ulong :ulong
                :double                 ;delta-x
                :double                 ;rigidity
                )
    libmagickwand))

(define-ffi-function Wand:flip-image "MagickFlipImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)
(define-ffi-function Wand:flop-image "MagickFlopImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

(define-ffi-function Wand:transpose-image "MagickTransposeImage"
  Wand-BooleanType
  (Wand-WandType) libmagickwand)
(define-ffi-function Wand:transverse-image "MagickTransverseImage"
  Wand-BooleanType
  (Wand-WandType) libmagickwand)

;; MagickWaveImage() creates a "ripple" effect in the image by
;; shifting the pixels vertically along a sine wave whose amplitude
;; and wavelength is specified by the given parameters.
;; The AMPLITUDE argument is a float and defines the how large
;; waves are.
;; The WAVELENGTH argument is a float and defines how often the
;; waves occur.
(define-ffi-function Wand:wave-image "MagickWaveImage" Wand-BooleanType
  (Wand-WandType :double :double) libmagickwand)

;; Swirl the image associated with WAND by DEGREES.
(define-ffi-function Wand:swirl-image "MagickSwirlImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

(define-ffi-function Wand:MagickPosterizeImage "MagickPosterizeImage"
  Wand-BooleanType
  (Wand-WandType :ulong Wand-BooleanType) libmagickwand)
(defun Wand:posterize-image (wand levels &optional ditherp)
  "Posterize the image associated with WAND.
that is quantise the range of used colours to at most LEVELS.
If optional argument DITHERP is non-nil use a dithering
effect to wipe hard contrasts."
  (Wand:MagickPosterizeImage wand levels (if ditherp 1 0)))

;; Tweak the image associated with WAND.
(define-ffi-function Wand:MagickModulateImage "MagickModulateImage"
  Wand-BooleanType
  ;; wand brightness saturation hue
  (Wand-WandType :double :double :double) libmagickwand)

(cl-defun Wand:modulate-image (wand &key (brightness 100.0)
                                    (saturation 100.0)
                                    (hue 100.0))
  (Wand:MagickModulateImage wand brightness saturation hue))

;; Solarise the image associated with WAND.
(define-ffi-function Wand:solarize-image "MagickSolarizeImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; Perform gamma correction on the image associated with WAND.
;; The argument LEVEL is a positive float, a value of 1.00 (read 100%)
;; is a no-op.
(define-ffi-function Wand:gamma-image "MagickGammaImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

(define-ffi-function Wand:MagickRaiseImage "MagickRaiseImage" Wand-BooleanType
  (Wand-WandType :ulong :ulong :long :long Wand-BooleanType) libmagickwand)

(defun Wand:raise-image (wand &optional raise x y)
  "Raise image."
  (Wand:MagickRaiseImage
   wand (Wand:image-width wand) (Wand:image-height wand)
   (or x 10) (or y 10) (if raise 1 0)))

(define-ffi-function Wand:spread-image "MagickSpreadImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; Blur the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(define-ffi-function Wand:gaussian-blur-image "MagickGaussianBlurImage"
  Wand-BooleanType
  (Wand-WandType :double :double) libmagickwand)

;; Blur the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
;; The ANGLE argument is a float and measured in degrees.
(define-ffi-function Wand:motion-blur-image "MagickMotionBlurImage"
  Wand-BooleanType
  ;; wand radius sigma angle
  (Wand-WandType :double :double :double) libmagickwand)

;; Blur the image associated with WAND.
;; The ANGLE argument is a float and measured in degrees.
(define-ffi-function Wand:radial-blur-image "MagickRadialBlurImage"
  Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; Sharpen the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(define-ffi-function Wand:sharpen-image "MagickSharpenImage" Wand-BooleanType
  (Wand-WandType :double :double) libmagickwand)

;; Simulates an image shadow
(define-ffi-function Wand:shadow-image "MagickShadowImage"
  Wand-BooleanType
  ;; wand opacity(%) sigma x-offset y-offset
  (Wand-WandType :double :double :long :long) libmagickwand)

;; MagickTrimImage() remove edges that are the background color from
;; the image.
(define-ffi-function Wand:trim-image "MagickTrimImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; Preview operations
(define-ffi-function Wand:preview-images "MagickPreviewImages" Wand-WandType
  (Wand-WandType Wand-PreviewType) libmagickwand)

;; Takes all images from the current image pointer to the end of the
;; image list and smushs them to each other top-to-bottom if the stack
;; parameter is true, otherwise left-to-right
(define-ffi-function Wand:smush-images "MagickSmushImages" Wand-WandType
  ;; wand stack offset
  (Wand-WandType Wand-BooleanType :ssize_t) libmagickwand)

(define-ffi-function Wand:MagickNegateImage "MagickNegateImage" Wand-BooleanType
  (Wand-WandType Wand-BooleanType) libmagickwand)
(defun Wand:negate-image (wand &optional greyp)
  "Perform negation on the image associated with WAND."
  (Wand:MagickNegateImage wand (if greyp 1 0)))

;; Crop to the rectangle spanned at X and Y by width DX and
;; height DY in the image associated with WAND."
(define-ffi-function Wand:crop-image "MagickCropImage" Wand-BooleanType
  (Wand-WandType :ulong :ulong :ulong :ulong) libmagickwand)

;; MagickChopImage() removes a region of an image and collapses the
;; image to occupy the removed portion
(define-ffi-function Wand:chop-image "MagickChopImage" Wand-BooleanType
  (Wand-WandType :ulong :ulong :long :long) libmagickwand)

;; Reduce the noise in the image associated with WAND by RADIUS.
(define-ffi-function Wand:reduce-noise-image "MagickReduceNoiseImage"
  Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; MagickAddNoiseImage() adds random noise to the image.
(define-ffi-function Wand:add-noise-image "MagickAddNoiseImage"
  Wand-BooleanType
  (Wand-WandType Wand-NoiseType) libmagickwand)

;; Composite one image COMPOSITE-WAND onto another WAND at the
;; specified offset X, Y, using composite operator COMPOSE.
(define-ffi-function Wand:image-composite "MagickCompositeImage"
  Wand-BooleanType
  (Wand-WandType Wand-WandType Wand-CompositeOperatorType :long :long)
  libmagickwand)

;;; image improvements and basic image properties
(define-ffi-function Wand:MagickContrastImage "MagickContrastImage"
  Wand-BooleanType
  (Wand-WandType Wand-BooleanType) libmagickwand)

;; Non-linear contrast changer
(define-ffi-function Wand:MagickSigmoidalContrastImage "MagickSigmoidalContrastImage"
  Wand-BooleanType
  (Wand-WandType Wand-BooleanType :double :double) libmagickwand)

;; Reduce the speckle noise in the image associated with WAND.
(define-ffi-function Wand:despeckle-image "MagickDespeckleImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

;; Enhance the image associated with WAND.
(define-ffi-function Wand:enhance-image "MagickEnhanceImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

;; Equalise the image associated with WAND.
(define-ffi-function Wand:equalize-image "MagickEqualizeImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

;; Normalise the image associated with WAND.
(define-ffi-function Wand:normalize-image "MagickNormalizeImage" Wand-BooleanType
  (Wand-WandType) libmagickwand)

;; Simulate a charcoal drawing of the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(define-ffi-function Wand:charcoal-image "MagickCharcoalImage" Wand-BooleanType
  (Wand-WandType :double :double) libmagickwand)

;; Simulate oil-painting of image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
(define-ffi-function Wand:oil-paint-image "MagickOilPaintImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; MagickSepiaToneImage() applies a special effect to the image,
;; similar to the effect achieved in a photo darkroom by sepia
;; toning. Threshold ranges from 0 to QuantumRange and is a measure of
;; the extent of the sepia toning. A threshold of 80 is a good
;; starting point for a reasonable tone.
(define-ffi-function Wand:sepia-tone-image "MagickSepiaToneImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; MagickImplodeImage() creates a new image that is a copy of an
;; existing one with the image pixels "implode" by the specified
;; percentage. It allocates the memory necessary for the new Image
;; structure and returns a pointer to the new image.
(define-ffi-function Wand:implode-image "MagickImplodeImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; MagickShadeImage() shines a distant light on an image to create a
;; three-dimensional effect. You control the positioning of the light
;; with azimuth and elevation; azimuth is measured in degrees off the
;; x axis and elevation is measured in pixels above the Z axis.
;;
;; GRAY - A value other than zero shades the intensity of each pixel.
;; AZIMUTH, ELEVATION - Define the light source direction.
(define-ffi-function Wand:shade-image "MagickShadeImage" Wand-BooleanType
  (Wand-WandType Wand-BooleanType :double :double) libmagickwand)

;; MagickVignetteImage() softens the edges of the image in vignette
;; style.
(define-ffi-function Wand:vignette-image "MagickVignetteImage" Wand-BooleanType
  ;; wand black-point white-point x y
  (Wand-WandType :double :double :double :double) libmagickwand)

;; MagickSketchImage() simulates a pencil sketch. We convolve the
;; image with a Gaussian operator of the given radius and standard
;; deviation (sigma). For reasonable results, radius should be larger
;; than sigma. Use a radius of 0 and SketchImage() selects a suitable
;; radius for you. Angle gives the angle of the blurring motion.
(define-ffi-function Wand:sketch-image "MagickSketchImage" Wand-BooleanType
  ;; wand radius sigma angle
  (Wand-WandType :double :double :double) libmagickwand)

;; Enhance the edges of the image associated with WAND.
;; The RADIUS argument is a float and measured in pixels.
(define-ffi-function Wand:edge-image "MagickEdgeImage" Wand-BooleanType
  (Wand-WandType :double) libmagickwand)

;; Emboss the image associated with WAND (a relief effect).
;; The RADIUS argument is a float and measured in pixels.
;; The SIGMA argument is a float and defines a derivation.
(define-ffi-function Wand:emboss-image "MagickEmbossImage" Wand-BooleanType
  (Wand-WandType :double :double) libmagickwand)

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
                          (ffi-free ,(cdr pxd)))))
          :format 'image/x-rgb
          :width w :height h)))

(defun Wand:emacs-image (wand)
  "Return Emacs image for the WAND."
  (Wand:emacs-image-internal
   wand 0 0 (Wand:image-width wand) (Wand:image-height wand)))

(cl-defun Wand:emacs-insert (wand &key (keymap nil) (offset nil)
                                  (region nil)
                                  (pointer 'arrow))
  "Insert WAND into Emacs buffer."
  (let* ((x (or (car offset) 0))
         (y (or (cdr offset) 0))
         (w (- (Wand:image-width wand) x))
         (h (- (Wand:image-height wand) y))
         (image (Wand:emacs-image-internal wand x y w h))
         (start (or (car region) (point))))
    (unless region (widget-insert " "))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (set-text-properties
       start (or (cdr region) (point))
       (list 'display image 'keymap keymap 'pointer pointer)))))

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
                 ((Wand-Orientation-get :right-top) 90)
                 ((Wand-Orientation-get :bottom-right) 180)
                 ((Wand-Orientation-get :left-bottom) -90))))
    (when angle
      (setf (Wand:image-orientation wand)
            (Wand-Orientation-get :top-left))
      (wand--operation-apply wand nil
                             'rotate angle))))

;;}}}


;;{{{ Custom variables for wand-mode

(defgroup wand nil
  "Group to customize wand mode."
  :prefix "wand-"
  :group 'multimedia)

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

(defcustom wand-pattern-composite-op "blend"
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
    (define-key map (kbd ":") #'wand-edit-operations)
    (define-key map (kbd "C-.") #'wand-repeat-last-operation)

    ;; Saving
    (define-key map (kbd "C-x C-s") #'wand-save-file)
    (define-key map (kbd "C-x C-w") #'wand-write-file)

    ;; Navigation
    (define-key map (kbd "SPC") #'wand-next-image)
    (define-key map [backspace] #'wand-prev-image)
    (define-key map (kbd "M-<") #'wand-first-image)
    (define-key map (kbd "M->") #'wand-last-image)

    (define-key map [next] #'wand-next-page)
    (define-key map [prior] #'wand-prev-page)
    (define-key map [home] #'wand-first-page)
    (define-key map [end] #'wand-last-page)
    (define-key map [?g] #'wand-goto-page)
    (define-key map [(meta ?g)] #'wand-goto-page)

    ;; Region
    (define-key map [down-mouse-1] #'wand-select-region)
    (define-key map (kbd "C-M-z") #'wand-activate-region)

    ;; General commands
    (define-key map [mouse-3] #'wand-popup-menu)
    (define-key map [(meta down-mouse-1)] #'wand-drag-image)
    (define-key map [(control down-mouse-1)] #'wand-drag-image)
    (define-key map "o" #'wand-operate)
    (define-key map "O" #'wand-global-operations-list)
    (define-key map "x" #'wand-toggle-fit)
    (define-key map "i" #'wand-identify)
    (define-key map "e" #'wand-prop-editor)
    (define-key map "q" #'wand-quit)
    (define-key map (kbd "C-r") #'wand-reload)

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
  (list ["Next Page" wand-next-page
         :active (Wand:has-next-image image-wand)]
        ["Previous Page" wand-prev-page
         :active (Wand:has-prev-image image-wand)]
        ["First Page" wand-first-page
         :active (/= (Wand:iterator-index image-wand) 0) ]
        ["Last Page" wand-last-page
         :active (/= (Wand:iterator-index image-wand)
                     (1- (Wand:images-num image-wand))) ]
        "-"
        ["Goto Page" wand-goto-page
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
          (cl-remove-if-not #'(lambda (c) (get c tag))
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

(define-wand-operation region (wand region op)
  "Apply operation OP to REGION.
Deactivates region."
  (let ((cwand (apply #'Wand:image-region wand region)))
    (setq preview-region nil)
    (unwind-protect
        (prog1
          (apply (wand--operation-lookup (car op)) cwand (cdr op))

          (Wand:image-composite
           wand cwand (cdr (assoc "copy" Wand-CompositeOperators))
           (nth 2 region) (nth 3 region)))
      (Wand:delete-wand cwand))))

(define-wand-operation flip (wand)
  (Wand:flip-image wand))

(define-wand-operation flop (wand)
  (Wand:flop-image wand))

(define-wand-operation mirror (wand how)
  (cl-ecase how
    (:vertical (Wand:transpose-image wand))
    (:horizontal (Wand:transverse-image wand))))

(define-wand-operation normalize (wand)
  (Wand:normalize-image wand))

(define-wand-operation despeckle (wand)
  (Wand:despeckle-image wand))

(define-wand-operation enhance (wand)
  (Wand:enhance-image wand))

(define-wand-operation equalize (wand)
  (Wand:equalize-image wand))

(define-wand-operation gauss-blur (wand radius sigma)
  (Wand:gaussian-blur-image wand (float radius) (float sigma)))

(define-wand-operation radial-blur (wand angle)
  (Wand:radial-blur-image wand (float angle)))

(define-wand-operation motion-blur (wand radius sigma angle)
  (Wand:motion-blur-image wand (float radius) (float sigma) (float angle)))

(define-wand-operation sketch (wand radius sigma angle)
  (Wand:sketch-image wand (float radius) (float sigma) (float angle)))

(define-wand-operation sharpen (wand radius sigma)
  (Wand:sharpen-image wand (float radius) (float sigma)))

(define-wand-operation shadow (wand opacity sigma x y)
  (Wand:shadow-image wand (float opacity) (float sigma) x y))

(define-wand-operation negate (wand greyp)
  (Wand:negate-image wand greyp))

(define-wand-operation modulate (wand mtype minc)
  (Wand:modulate-image wand mtype (float (+ 100 minc))))

(define-wand-operation grayscale (wand)
  (Wand:SetImageColorspace
   wand (cdr (assoc "GRAY" Wand-ColorspaceTypes))))

(define-wand-operation solarize (wand threshold)
  (Wand:solarize-image wand (float threshold)))

(define-wand-operation swirl (wand degrees)
  (Wand:swirl-image wand (float degrees)))

(define-wand-operation oil (wand radius)
  (Wand:oil-paint-image wand (float radius)))

(define-wand-operation charcoal (wand radius sigma)
  (Wand:charcoal-image wand (float radius) (float sigma)))

(define-wand-operation sepia-tone (wand threshold)
  (Wand:sepia-tone-image wand (float threshold)))

(define-wand-operation implode (wand radius)
  (Wand:implode-image wand (float radius)))

(define-wand-operation shade (wand grayp azimuth elevation)
  (Wand:shade-image wand (if grayp 1 0) (float azimuth) (float elevation)))

(define-wand-operation wave (wand amplitude wave-length)
  (Wand:wave-image wand (float amplitude) (float wave-length)))

(define-wand-operation vignette (wand white black x y)
  (Wand:vignette-image wand (float white) (float black) (float x) (float y)))

(define-wand-operation edge (wand radius)
  (Wand:edge-image wand (float radius)))

(define-wand-operation emboss (wand radius sigma)
  (Wand:emboss-image wand (float radius) (float sigma)))

(define-wand-operation reduce-noise (wand radius)
  (Wand:reduce-noise-image wand (float radius)))

(define-wand-operation add-noise (wand noise-type)
  (Wand:add-noise-image wand (cdr (assoc noise-type Wand-NoiseTypes))))

(define-wand-operation spread (wand radius)
  (Wand:spread-image wand (float radius)))

(define-wand-operation trim (wand fuzz)
  (Wand:trim-image wand (float fuzz)))

(define-wand-operation raise (wand raise)
  (Wand:raise-image wand raise))

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
  (Wand:MagickContrastImage wand (if (eq cp :increase) 1 0)))

(define-wand-operation sigmoidal-contrast (wand cp strength midpoint)
  (Wand:MagickSigmoidalContrastImage
   wand (if (eq cp :increase) 1 0) (float strength)
   (* (Wand:quantum-range) (/ midpoint 100.0))))

(define-wand-operation scale (wand width height)
  (Wand:scale-image wand width height))

(define-wand-operation sample (wand width height)
  (Wand:sample-image wand width height))

(defmacro wand-make-scaler (filter-type blur)
  "Create resize function, suitable with `Wand:fit-resize'.
FILTER-TYPE and BLUR specifies smothing applied after resize.
FILTER-TYPE is one of `Wand-FilterTypes'
BLUR is float, 0.25 for insane pixels, > 2.0 for excessively smoth."
  `(lambda (iw x y)
     (Wand:resize-image iw x y ,(cdr (assoc filter-type Wand-FilterTypes))
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
  (Wand:preview-images
   wand (cdr (assoc ptype Wand-PreviewTypes))))

(define-wand-operation pattern (wand pattern op)
  (Wand-with-wand cb-wand
    (setf (Wand:image-size cb-wand)
          (cons (Wand:image-width wand) (Wand:image-height wand)))
    (Wand:read-image-data cb-wand (concat "pattern:" pattern))
    (Wand:image-composite wand cb-wand
                          (cdr (assoc op Wand-CompositeOperators)) 0 0)))

;; TODO: other operations
(defun wand--redeye-fix-pixels (pixels)
  "Simple red PIXELS fixator.
Normalize pixel color if it is too 'red'."
  (let* ((rchan '(0.1 0.6 0.3))
	 (gchan '(0.0 1.0 0.0))
	 (bchan '(0.0 0.0 1.0))
	 (rnorm (/ 1.0 (apply #'+ rchan)))
	 (gnorm (/ 1.0 (apply #'+ gchan)))
	 (bnorm (/ 1.0 (apply #'+ bchan))))
    (flet ((normalize (chan norm r g b)
	     (min 255 (int (* norm (+ (* (first chan) r)
				      (* (second chan) g)
				      (* (third chan) b)))))))
      (mapcar #'(lambda (pixel-value)
		  (multiple-value-bind (r g b) pixel-value
		    (if (>= r (* Wand-mode-redeye-threshold g))
			(list (normalize rchan rnorm r g b)
			      (normalize gchan gnorm r g b)
			      (normalize bchan bnorm r g b))
		      (list r g b))))
	      pixels))))

(defun wand--redeye-blur-radius (w h)
  "Return apropriate blur radius for region of width W and height H.
It should not be too large for large regions, and it should not be
too small for small regions."
  (1- (sqrt (sqrt (sqrt (sqrt (* w h)))))))

;; TODO
(define-wand-operation redeye-remove (wand region)
  "Remove redeye in the REGION."
  (multiple-value-bind (w h x y) region
    (Wand-with-wand cw
      ;; Consitute new wand with fixed red pixels
      (Wand:constitute-image
       cw w h 'rgb (wand--redeye-fix-pixels
                    (Wand:get-image-rgb-pixels wand x y w h)))

      ;; Limit blur effect to ellipse at the center of REGION by
      ;; setting clip mask
      (let ((mask (Wand:copy-wand cw)))
	(unwind-protect
	    (progn
	      (Wand-with-drawing-wand dw
		(Wand-with-pixel-wand pw
		  (setf (Wand:pixel-color pw) "white")
		  (setf (Wand:draw-fill-color dw) pw)
		  (Wand:draw-color
                   dw 0.0 0.0 (Wand-PaintMethod-get :reset)))
		(Wand-with-pixel-wand pw
		  (setf (Wand:pixel-color pw) "black")
		  (setf (Wand:draw-fill-color dw) pw))
		(Wand:draw-ellipse
		 dw (/ w 2.0) (/ h 2.0) (/ w 2.0) (/ h 2.0) 0.0 360.0)
		(Wand:MagickDrawImage mask dw))
	      (setf (Wand:clip-mask cw) mask))
	  (Wand:delete-wand mask)))

      (Wand:gaussian-blur-image
       cw 0.0 (wand--redeye-blur-radius w h))
      (setf (Wand:clip-mask cw) nil)

      ;; Finally copy blured image to WAND
      (Wand:image-composite
       wand cw (cdr "copy" Wand-CompositeOperators) x y))))

;;}}}

;;{{{ wand-display, wand-mode

(defun wand--image-region ()
  "Return region in real image, according to `preview-region'."
  (unless preview-region
    (error "Region not selected"))

  (let ((off-x (car preview-offset))
        (off-y (cdr preview-offset))
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
            (if (and preview-wand preview-offset
                     (> (car preview-offset) 0)
                     (> (cdr preview-offset) 0))
                (format ", Offset: +%d+%d"
                        (car preview-offset) (cdr preview-offset))
              "")
            ;; Print region info
            (if preview-region
                (apply #'format ", Region: %dx%d+%d+%d"
                       (wand--image-region))
              ""))))

(defun wand--operations-action (wid &rest args)
  (wand-edit-operations
   (car (read-from-string (widget-value wid)))))

(defun wand--insert-operations ()
  (when wand-global-operations-list
    (widget-insert (format "Global operations: %S"
                           wand-global-operations-list) "\n"))

  (when operations-list
    (widget-create 'editable-field
                   :format "Operations: %v"
                   :size 40
                   :action #'wand--operations-action
                   (prin1-to-string operations-list))
    (widget-insert "\n")))

(defun wand--color-info ()
  (declare (special pickup-color))
  (let* ((cf (make-face (cl-gensym "dcolor-")))
         (place (car pickup-color))
         (color (cdr pickup-color))
         (fcol (apply #'format "#%02x%02x%02x" color))
         (spaces "      "))
    (set-face-background cf fcol)
    (add-face-text-property 0 (length spaces) cf nil spaces)
    (concat
     (format "Color: +%d+%d " (car place) (cdr place))
     spaces
     (format " %s R:%d, G:%d, B:%d" fcol
             (car color) (cadr color) (caddr color)))))

(defun wand--insert-info ()
  "Insert some file informations."
  (when wand-show-fileinfo
    (widget-insert (wand--file-info) "\n"))
  (when wand-show-operations
    (wand--insert-operations))

  ;; Info about pickup color
  (when (boundp 'pickup-color)
    (widget-insert (wand--color-info) "\n"))

  (widget-setup)
  (use-local-map wand-mode-map)

  (run-hooks 'wand-info-hook))

(defun wand--update-info ()
  "Only update info region."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
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
        (widget-insert (wand--file-info))))
    (set-buffer-modified-p nil)))

(defun wand--preview-with-region ()
  "Return highlighted version of `preview-wand' in case region is selected."
  (when preview-region
    (multiple-value-bind (w h x y) preview-region
      ;; Take into account current offset
      (incf x (car preview-offset))
      (incf y (cdr preview-offset))
      (Wand-with-drawing-wand dw
        (Wand-with-pixel-wand pw
          (setf (Wand:pixel-color pw) wand-region-outline-color)
          (Wand:DrawSetStrokeColor dw pw))
        (Wand-with-pixel-wand pw
          (setf (Wand:pixel-color pw) wand-region-fill-color)
          (setf (Wand:draw-fill-color dw) pw))
        (setf (Wand:draw-stroke-width dw) wand-region-outline-width
              (Wand:draw-stroke-opacity dw) wand-region-outline-opacity
              (Wand:draw-fill-opacity dw) wand-region-fill-opacity)
        (Wand:draw-lines dw (list (cons x y) (cons (+ x w) y)
                                  (cons (+ x w) (+ y h)) (cons x (+ y h))
                                  (cons x y)))
        (let ((nw (Wand:copy-wand preview-wand)))
          (Wand:MagickDrawImage nw dw)
          nw)))))

(defun wand--insert-preview ()
  "Display wand W at the point."
  (let ((saved-w (and preview-wand (Wand:image-width preview-wand)))
        (saved-h (and preview-wand (Wand:image-height preview-wand)))
        (off-x (or (car preview-offset) 0))
        (off-y (or (cdr preview-offset) 0)))
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

    ;; NOTE: if size not changed, then keep `preview-offset' value
    (unless (and (eq saved-w (Wand:image-width preview-wand))
                 (eq saved-h (Wand:image-height preview-wand)))
      (setq preview-offset '(0 . 0)))

    ;; Hackery to insert invisible char, so widget-delete won't affect
    ;; preview-glyph visibility
    ;; (let ((ext (make-extent (point) (progn (insert " ") (point)))))
    ;;   (set-extent-property ext 'invisible t)
    ;;   (set-extent-property ext 'start-open t))

    (let ((pwr (wand--preview-with-region)))
      (unwind-protect
          (Wand:emacs-insert
           (or pwr preview-wand) :keymap wand-mode-map :offset preview-offset)
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
    (mapc #'widget-delete widget-field-list))

  (wand--insert-info)
  (wand--insert-preview)
  (widget-setup)

  (set-buffer-modified-p nil)
  (use-local-map wand-mode-map))

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
        (make-local-variable 'image-wand)
        (make-local-variable 'preview-wand)
        (make-local-variable 'preview-region)
        (make-local-variable 'preview-offset)
        (make-local-variable 'last-preview-region)
        (make-local-variable 'operations-list)
        (make-local-variable 'undo-list)
        (setq operations-list nil)
        (setq undo-list nil)
        (setq preview-wand nil)
        (setq image-wand (Wand:make-wand))
        (put 'image-wand 'fitting wand-auto-fit)

        (use-local-map wand-mode-map)
        (setq mode-name "wand")
        (setq major-mode 'wand-mode)
        ;; Setup menubar
        (add-submenu '() wand-menu)
        ;; Add local kill-buffer-hook to cleanup stuff
        (add-hook 'kill-buffer-hook 'wand--cleanup nil t))

      (when preview-wand
        (Wand:delete-wand preview-wand))
      (setq preview-wand nil)
      (setq preview-region nil)
      (setq preview-offset nil)
      (setq last-preview-region nil)
      (setq operations-list wand-global-operations-list)
      (setq undo-list nil)
      (Wand:clear-wand image-wand)
      ;; Fix buffer-file-name in case of viewing directory
      (when (file-directory-p file)
        (setq file (or (wand--next-file (concat file "/.")) file)))
      (setq buffer-file-name file)
      (setq default-directory (file-name-directory file))

      ;; Will raise error if something wrong
      (Wand:read-image image-wand file)

      ;; NOTE: New IM sets iterator index to last page, we want to
      ;; start from the first page
      (setf (Wand:iterator-index image-wand) 0)

      (when wand-auto-rotate
        (Wand:correct-orientation image-wand))

      ;; Apply operations in case global operations list is used
      (wand--operation-list-apply image-wand)
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

(defun wand--cleanup ()
  "Cleanup when wand buffer is killed."
  (ignore-errors
    (when preview-wand
      (Wand:delete-wand preview-wand)
      (setq preview-wand nil))
    (Wand:delete-wand image-wand)
    (setq image-wand nil)))

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
    (let ((fi (Wand:get-magick-info format)))
      (and (not (ffi-pointer-null-p fi))
           (not (ffi-pointer-null-p (Wand-InfoType-decoder fi)))))))

(defcustom wand-formats-cant-write
  '("html")
  "List of formats that are not intented to be written by Wand."
  :type '(list string)
  :group 'wand)

(defun wand-format-can-write-p (format)
  "Return non-nil if wand can write files in FORMAT."
  (unless (member (downcase format) wand-formats-cant-write)
    (let ((fi (Wand:get-magick-info format)))
      (and (not (ffi-pointer-null-p fi))
           (not (ffi-pointer-null-p (Wand-InfoType-encoder fi)))))))

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
  (with-ffi-temporaries ((c-num :ulong)
                         (c-mexc Wand-ExceptionInfoType))
    (with-ffi-string (c-pat fmt-regexp)
      (let ((miflist (Wand:GetMagickInfoList c-pat c-num c-mexc)))
        (unless (ffi-pointer-null-p miflist)
          (unwind-protect
              (loop for n from 0 below (ffi-aref c-num :ulong 0)
                with fmt-name = nil
                do (setq fmt-name
                         (ffi-get-c-string
                          (Wand-InfoType-name
                           (ffi-aref miflist :pointer n))))
                if (ecase (or mode 'any)
                     (read (wand-format-can-read-p fmt-name))
                     (write (wand-format-can-write-p fmt-name))
                     (read-write
                      (and (wand-format-can-read-p fmt-name)
                           (wand-format-can-write-p fmt-name)))
                     (any t))
                collect (downcase fmt-name))
            (Wand:RelinquishMemory miflist)))))))

;;}}}

;;{{{ File navigation commands

(defun wand--next-file (curfile &optional reverse-order)
  "Return next (to CURFILE) image file in the directory.
If REVERSE-ORDER is specified, then return previous file."
  (let* ((dir (file-name-directory curfile))
         (fn (file-name-nondirectory curfile))
         (dfiles (directory-files dir))
         (nfiles (cdr (member fn (if reverse-order (nreverse dfiles) dfiles)))))
    (while (and nfiles (not (wand-file-can-read-p (concat dir (car nfiles)))))
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

;;{{{ Pages navigation commands

(defun wand-next-page ()
  "Display next image in image chain."
  (interactive)
  (unless (Wand:has-next-image image-wand)
    (error "No next image in chain"))
  (Wand:next-image image-wand)
  (wand--operation-list-apply image-wand)
  (wand--redisplay))

(defun wand-prev-page ()
  "Display previous image in image chain."
  (interactive)
  (unless (Wand:has-prev-image image-wand)
    (error "No previous image in chain"))
  (Wand:prev-image image-wand)
  (wand--operation-list-apply image-wand)
  (wand--redisplay))

(defun wand-first-page ()
  "Display first image in image chain."
  (interactive)
  (Wand:set-first-iterator image-wand)
  (wand--operation-list-apply image-wand)
  (wand--redisplay))

(defun wand-last-page ()
  "Display last image in image chain."
  (interactive)
  (Wand:set-last-iterator image-wand)
  (wand--operation-list-apply image-wand)
  (wand--redisplay))

(defun wand-goto-page (n)
  "Display last image in image chain."
  (interactive
   (list (if (numberp current-prefix-arg)
             current-prefix-arg
           (read-number "Goto page: "))))
  ;; Internally images in chain counts from 0
  (unless (setf (Wand:iterator-index image-wand) (1- n))
    (error "No such page: %d" n))
  (wand--operation-list-apply image-wand)
  (wand--redisplay))

;;}}}

;;{{{ Operations list functions

(defun wand--operation-lookup (opname)
  (intern (format "wand--op-%S" opname)))

(defun wand--operation-apply (wand region operation &rest args)
  "Apply OPERATION to WAND using addition arguments ARGS.
If REGION is non-nil then apply OPERATION to REGION."
  (let* ((baseop (cons operation args))
         (op (if region
                 (list 'region region baseop)
               baseop)))
    (setq operations-list
          (append operations-list (list op)))
    (setq undo-list nil)                ; Reset undo
    (apply (wand--operation-lookup (car op)) wand (cdr op))))

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
  (wand--operation-apply image-wand preview-region
                         'flip)
  (wand--redisplay))
(put 'wand-flip 'transform-operation t)
(put 'wand-flip 'menu-name "Flip")

(defun wand-flop ()
  "Flop the image."
  (interactive)
  (wand--operation-apply image-wand preview-region
                         'flop)
  (wand--redisplay))
(put 'wand-flop 'transform-operation t)
(put 'wand-flop 'menu-name "Flop")

(defun wand-mirror (mhow)
  "Mirror the image.
Same as combination of rotation and flip/flop."
  (interactive (list (completing-read
                      "Mirror (default horizontal): "
                      '(("vertical") ("horizontal"))
                      nil t nil nil "horizontal")))
  (wand--operation-apply image-wand preview-region
                         'mirror (intern-soft (concat ":" mhow)))
  (wand--redisplay))
(put 'wand-mirror 'transform-operation t)
(put 'wand-mirror 'menu-name "Mirror")

(defun wand-trim (fuzz)
  "Trim edges the image."
  (interactive (list (read-number "Fuzz: " 0)))
  (wand--operation-apply image-wand preview-region
                         'trim (/ fuzz 100.0))
  (wand--redisplay))
(put 'wand-trim 'transform-operation t)
(put 'wand-trim 'menu-name "Trim Edges")

(defun wand-rotate (arg)
  "Rotate image to ARG degrees.
If ARG is positive then rotate in clockwise direction.
If negative then to the opposite."
  (interactive "nDegrees: ")
  (wand--operation-apply image-wand nil
                         'rotate arg)
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
  (wand--operation-apply image-wand preview-region
                         'raise arg)
  (wand--redisplay))
(put 'wand-raise 'transform-operation t)
(put 'wand-raise 'menu-name "3D Button Effect")

;;}}}

;;{{{ Effect operations

(defun wand-radial-blur (arg)
  "Blur the image radially by ARG degree."
  (interactive (list (read-number "Blur radius: " 2.0)))
  (wand--operation-apply image-wand preview-region
                         'radial-blur arg)
  (wand--redisplay))
(put 'wand-radial-blur 'effect-operation t)
(put 'wand-radial-blur 'menu-name "Radial Blur")

(defun wand-motion-blur (radius sigma angle)
  "Apply motion blur the image using RADIUS, SIGMA and ANGLE."
  (interactive (list (read-number "Radius: " 1)
                     (read-number "Sigma: " wand-sigma)
                     (read-number "Angle: " 2)))
  (wand--operation-apply image-wand preview-region
                         'motion-blur radius sigma angle)
  (wand--redisplay))
(put 'wand-motion-blur 'effect-operation t)
(put 'wand-motion-blur 'menu-name "Motion Blur")

(defun wand-gaussian-blur (radius sigma)
  "Apply gaussian blur of RADIUS and SIGMA to the image."
  (interactive (list (read-number "Radius: " 1)
                     (read-number "Sigma: " wand-sigma)))
  (wand--operation-apply image-wand preview-region
                         'gauss-blur radius sigma)
  (wand--redisplay))
(put 'wand-gaussian-blur 'can-preview "blur")
(put 'wand-gaussian-blur 'effect-operation t)
(put 'wand-gaussian-blur 'menu-name "Gaussian Blur")

(defun wand-sketch (radius sigma angle)
  "Simulate pencil sketch.
For reasonable results, RADIUS should be larger than SIGMA.
User 0 RADIUS for autoselect.
ANGLE gives angle of blurring motion."
  (interactive (list (read-number "Radius: " 0)
                     (read-number "Sigma: " wand-sigma)
                     (read-number "Angle: " 2)))
  (wand--operation-apply image-wand preview-region
                         'sketch radius sigma angle)
  (wand--redisplay))
(put 'wand-sketch 'effect-operation t)
(put 'wand-sketch 'menu-name "Sketch")

(defun wand-shadow (opacity sigma x-off y-off)
  "Shadow."
  (interactive (list (read-number "Opacity in %: " 50)
                     (read-number "Sigma: " wand-sigma)
                     (read-number "Shadow x-offset: " 4)
                     (read-number "Shadow y-offset: " 4)))
  (wand--operation-apply image-wand preview-region
                         'shadow opacity sigma x-off y-off)
  (wand--redisplay))
(put 'wand-shadow 'effect-operation t)
(put 'wand-shadow 'menu-name "shadow")

(defun wand-sharpen (radius sigma)
  "Sharpen image with by RADIUS and SIGMA."
  (interactive (list (read-number "Radius: " 1)
                     (read-number "Sigma: " wand-sigma)))
  (wand--operation-apply image-wand preview-region
                         'sharpen radius sigma)
  (wand--redisplay))
(put 'wand-sharpen 'can-preview "sharpen")
(put 'wand-sharpen 'effect-operation t)
(put 'wand-sharpen 'menu-name "Sharpen")

(defun wand-despeckle ()
  "Despeckle image."
  (interactive)
  (wand--operation-apply image-wand preview-region
                         'despeckle)
  (wand--redisplay))
(put 'wand-despeckle 'can-preview "despeckle")
(put 'wand-despeckle 'effect-operation t)
(put 'wand-despeckle 'menu-name "Despeckle")

(defun wand-edge (radius)
  "Enhance edges of the image by RADIUS.
Default is 1."
  (interactive (list (read-number "Radius: " 1)))
  (wand--operation-apply image-wand preview-region
                         'edge radius)
  (wand--redisplay))
(put 'wand-edge 'can-preview "edgedetect")
(put 'wand-edge 'effect-operation t)
(put 'wand-edge 'menu-name "Edge Detect")

(defun wand-emboss (radius sigma)
  "Emboss the image with RADIUS and SIGMA."
  (interactive (list (read-number "Radius: " 1.0)
                     (read-number "Sigma: " wand-sigma)))
  (wand--operation-apply image-wand preview-region
                         'emboss radius sigma)
  (wand--redisplay))
(put 'wand-emboss 'effect-operation t)
(put 'wand-emboss 'menu-name "Emboss")

(defun wand-reduce-noise (radius)
  "Reduce the noise with RADIUS.
Default is 1."
  (interactive (list (read-number "Noise reduce radius: " 1)))
  (wand--operation-apply image-wand preview-region
                         'reduce-noise radius)
  (wand--redisplay))
(put 'wand-reduce-noise 'can-preview :ReduceNoisePreview)
(put 'wand-reduce-noise 'effect-operation t)
(put 'wand-reduce-noise 'menu-name "Reduce Noise")

(defun wand-add-noise (noise-type)
  "Add noise of NOISE-TYPE."
  (interactive
   (list (completing-read "Noise type [poisson]: "
                          Wand-NoiseTypes
                          nil t nil nil "poisson")))
  (wand--operation-apply image-wand preview-region
                         'add-noise noise-type)
  (wand--redisplay))
(put 'wand-add-noise 'effect-operation t)
(put 'wand-add-noise 'menu-name "Add Noise")

(defun wand-spread (radius)
  "Spread image pixels with RADIUS."
  (interactive (list (read-number "Spread radius: " 1.0)))
  (wand--operation-apply image-wand preview-region
                         'spread radius)
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
  (wand--operation-apply image-wand preview-region
                         'contrast (intern-soft (concat ":" ctype)))
  (wand--redisplay))
(put 'wand-contrast 'enhance-operation t)
(put 'wand-contrast 'menu-name "Contrast")

(defun wand-sigmoidal-contrast (ctype strength midpoint)
  "Increase/decrease contrast of the image.
CTYPE - `:increase' to increase, `:decrease' to decrease.
STRENGTH - larger the number the more 'threshold-like' it becomes.
MIDPOINT - midpoint of the function as a color value 0 to QuantumRange"
  (interactive (list (completing-read
                      "Contrast (default increase): "
                      '(("increase") ("decrease"))
                      nil t nil nil "increase")
                     (read-number "Strength: " 5)
                     (read-number "Midpoint in %: " 0)))
  (wand--operation-apply image-wand preview-region
                         'sigmoidal-contrast
                         (intern-soft (concat ":" ctype))
                         strength midpoint)
  (wand--redisplay))
(put 'wand-sigmoidal-contrast 'enhance-operation t)
(put 'wand-sigmoidal-contrast 'menu-name "Sigmoidal Contrast")

(defun wand-normalize ()
  "Normalize image."
  (interactive)
  (wand--operation-apply image-wand preview-region
                         'normalize)
  (wand--redisplay))
(put 'wand-normalize 'enhance-operation t)
(put 'wand-normalize 'menu-name "Normalize")

(defun wand-enhance ()
  "Enhance image."
  (interactive)
  (wand--operation-apply image-wand preview-region
                         'enhance)
  (wand--redisplay))
(put 'wand-enhance 'enhance-operation t)
(put 'wand-enhance 'menu-name "Enhance")

(defun wand-equalize ()
  "Equalise image."
  (interactive)
  (wand--operation-apply image-wand preview-region
                         'equalize)
  (wand--redisplay))
(put 'wand-equalize 'enhance-operation t)
(put 'wand-equalize 'menu-name "Equalize")

(defun wand-negate (arg)
  "Negate image.
If prefix ARG is specified then negate by grey."
  (interactive "P")
  (wand--operation-apply image-wand preview-region
                         'negate arg)
  (wand--redisplay))
(put 'wand-negate 'enhance-operation t)
(put 'wand-negate 'menu-name "Negate")

(defun wand-grayscale ()
  "Convert image to grayscale colorspace."
  (interactive)
  (wand--operation-apply image-wand preview-region
                         'grayscale)
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
  (wand--operation-apply image-wand preview-region
                         'modulate type inc)
  (wand--redisplay))
(put 'wand-modulate 'enhance-operation t)
(put 'wand-modulate 'menu-name "Modulate")

;;}}}

;;{{{ F/X operations

(defun wand-preview-op (op)
  "Preview some operation OP with 8 subnails."
  (interactive (list (completing-read "Operation: "
                        Wand-PreviewTypes nil t)))
  (wand--redisplay (wand--operation-apply image-wand preview-region
                                          'preview-op op)))
(put 'wand-preview-op 'f/x-operation t)
(put 'wand-preview-op 'menu-name "Preview operation")

(defun wand-solarize (sf)
  "Solarise image with solarize factor SF."
  (interactive (list (read-number "Solarize in %: " 50)))
  (wand--operation-apply image-wand preview-region
                         'solarize (* (Wand:quantum-range) (/ sf 100.0)))
  (wand--redisplay))
(put 'wand-solarize 'can-preview "solarize")
(put 'wand-solarize 'f/x-operation t)
(put 'wand-solarize 'menu-name "Solarize")

(defun wand-swirl (degrees)
  "Swirl the image by DEGREES."
  (interactive (list (read-number "Degrees: " 90)))
  (wand--operation-apply image-wand preview-region
                         'swirl degrees)
  (wand--redisplay))
(put 'wand-swirl 'f/x-operation t)
(put 'wand-swirl 'menu-name "Swirl")

(defun wand-oil-paint (radius)
  "Simulate oil painting with RADIUS for the image.
Default radius is 3."
  (interactive (list (read-number "Radius: " 2.5)))
  (wand--operation-apply image-wand preview-region
                         'oil radius)
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
  (wand--operation-apply image-wand preview-region
                         'charcoal radius sigma)
  (wand--redisplay))
(put 'wand-charcoal 'can-preview "charcoal-drawing")
(put 'wand-charcoal 'f/x-operation t)
(put 'wand-charcoal 'menu-name "Charcoal Draw")

(defun wand-sepia-tone (threshold)
  "Apply sepia tone to image by THRESHOLD."
  (interactive (list (read-number "Threshold in %: " 80)))
  (wand--operation-apply image-wand preview-region
                         'sepia-tone (* (Wand:quantum-range)
                                        (/ threshold 100.0)))
  (wand--redisplay))
(put 'wand-sepia-tone 'f/x-operation t)
(put 'wand-sepia-tone 'menu-name "Sepia Tone")

(defun wand-implode (radius)
  "Implode image by RADIUS.
RADIUS range is [-1.0, 1.0]."
  (interactive (list (read-number "Radius: " 0.3)))
  (wand--operation-apply image-wand preview-region
                         'implode radius)
  (wand--redisplay))
(put 'wand-implode 'f/x-operation t)
(put 'wand-implode 'menu-name "Implode")

(defun wand-shade (azimuth elevation grayp)
  "Shines a distant light on an image to create a three-dimensional effect.
You control the positioning of the light with azimuth and
elevation; azimuth is measured in degrees off the x axis and
elevation is measured in pixels above the Z axis."
  (interactive (list (read-number "Azimuth in Dg: " 30)
                     (read-number "Elevation in Px: " 30)
                     (not current-prefix-arg)))
  (wand--operation-apply image-wand preview-region
                         'shade grayp azimuth elevation)
  (wand--redisplay))
(put 'wand-shade 'can-preview "shade")
(put 'wand-shade 'f/x-operation t)
(put 'wand-shade 'menu-name "Shade")

(defun wand-vignette (bw)
  "Create vignette using image."
  (interactive (list (read-number "Black/White: " 10)))
  (wand--operation-apply image-wand preview-region
                         'vignette bw bw 0 0)
  (wand--redisplay))
(put 'wand-vignette 'f/x-operation t)
(put 'wand-vignette 'menu-name "Vignette")

(defun wand-wave (amplitude wave-length)
  "Create wave effect on image with AMPLITUDE and WAVE-LENGTH."
  (interactive (list (read-number "Amplitude: " 4)
                     (read-number "Wave length: " 50)))
  (wand--operation-apply image-wand preview-region
                         'wave amplitude wave-length)
  (wand--redisplay))
(put 'wand-wave 'can-preview "wave")
(put 'wand-wave 'f/x-operation t)
(put 'wand-wave 'menu-name "Wave")

;;}}}

;;{{{ Region commands

(defsubst wand--mouse-release-p (event)
  (and (consp event) (symbolp (car event))
       (or (memq 'click (get (car event) 'event-symbol-elements))
           (memq 'drag (get (car event) 'event-symbol-elements)))
       ))

(defsubst wand--motion-event-p (event)
  (and (consp event) (symbolp (car event))
       (memq 'mouse-movement (get (car event) 'event-symbol-elements))))

(defun wand-pick-color (event)
  "Pick color at click point."
  (interactive "e")
  (let* ((s-xy (posn-object-x-y (event-start event)))
         (sx (car s-xy)) (sy (cdr s-xy))
         (col (Wand:get-rgb-pixel-at preview-wand sx sy))
         (pickup-color (cons (cons sx sy) col)))
    (declare (special pickup-color))
    (wand--update-info)))

(defun wand-select-region (event)
  "Select region."
  (interactive "e")
  (let* ((gc-cons-threshold most-positive-fixnum) ; inhibit gc
         (s-xy (posn-object-x-y (event-start event)))
         (sx (car s-xy)) (sy (cdr s-xy))
         (had-preview-region preview-region))
    (setq preview-region (list 0 0 (car s-xy) (cdr s-xy)))
    (track-mouse
      (while (not (wand--mouse-release-p (setq event (read-event))))
        (when (wand--motion-event-p event)
          (let* ((m-xy (posn-object-x-y (event-start event)))
                 (mx (car m-xy)) (my (cdr m-xy)))
            (setq preview-region
                  (list (abs (- sx mx)) (abs (- sy my))
                        (min sx mx) (min sy my)))
            ;; Update info and preview image
            (wand--redisplay)))))

    (if (and (> (nth 0 preview-region) 0)
             (> (nth 1 preview-region) 0))
        ;; Save region
        (setq last-preview-region preview-region)

      (setq preview-region nil)
      (if had-preview-region
          (wand--redisplay)

        ;; Otherwise pickup color
        (wand-pick-color event)))))

(defun wand-activate-region ()
  "Activate last preview-region."
  (interactive)
  (setq preview-region last-preview-region)
  (wand--redisplay))

(defun wand-drag-image (event)
  "Drag image to view unshown part of the image."
  (interactive "e")
  (let* ((gc-cons-threshold most-positive-fixnum) ; inhibit gc
         (s-xy (posn-object-x-y (event-start event)))
         (sx (car s-xy))
         (sy (cdr s-xy))
         (pw (Wand:image-width preview-wand))
         (ph (Wand:image-height preview-wand)))
    (track-mouse
      (while (not (wand--mouse-release-p (setq event (read-event))))
        (when (wand--motion-event-p event)
          (let* ((m-xy (posn-object-x-y (event-start event)))
                 (off-x (+ (- sx (car m-xy))
                           (or (car preview-offset) 0)))
                 (off-y (+ (- sy (cdr m-xy))
                           (or (cdr preview-offset) 0))))
            (when (< off-x 0) (setq off-x 0))
            (when (< off-y 0) (setq off-y 0))
            (setq preview-offset (cons off-x off-y))
            (wand--redisplay)))))
    ))

(defun wand-crop (region)
  "Crop image to selected region."
  (interactive (list (wand--image-region)))
  (wand--operation-apply image-wand nil
                         'crop region)
  (setq preview-region nil)
  (wand--redisplay))
(put 'wand-crop 'region-operation t)
(put 'wand-crop 'menu-name "Crop")

(defun wand-chop (region)
  "Chop region from the image."
  (interactive (list (wand--image-region)))
  (wand--operation-apply image-wand nil
                         'chop region)
  (setq preview-region nil)
  (wand--redisplay))
(put 'wand-chop 'region-operation t)
(put 'wand-chop 'menu-name "Chop")

(defun wand-redeye-remove (region)
  "Remove red from the selected region."
  (interactive (list (wand--image-region)))
  (let ((gc-cons-threshold most-positive-fixnum)) ; inhibit gc
    (wand--operation-apply image-wand nil
                           'redeye-remove region)
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

  (wand--operation-apply image-wand nil
                         'zoom factor)
  (wand--redisplay))
(put 'wand-zoom 'transform-operation t)
(put 'wand-zoom 'menu-name "Zoom")

(defun wand-zoom-in (arg)
  "Zoom image in by `wand-zoom-factor' factor."
  (interactive "P")
  (wand-zoom (or (and arg (prefix-numeric-value arg))
                 wand-zoom-factor)))

(defun wand-zoom-out (arg)
  "Zoom image out by `wand-zoom-factor'."
  (interactive "P")
  (wand-zoom (- (or (and arg (prefix-numeric-value arg))
                    wand-zoom-factor))))

(defun wand-scale (w h)
  "Scale image to WxH."
  (interactive
   (list (read-number "Width: " (Wand:image-width image-wand))
         (read-number "Height: " (Wand:image-height image-wand))))
  (wand--operation-apply image-wand nil
                         'scale w h)
  (wand--redisplay))
(put 'wand-scale 'transform-operation t)
(put 'wand-scale 'menu-name "Scale")

(defun wand-sample (w h)
  "Sample image to WxH size."
  (interactive
   (list (read-number "Width: " (Wand:image-width image-wand))
         (read-number "Height: " (Wand:image-height image-wand))))
  (wand--operation-apply image-wand nil
                         'sample w h)
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

  (wand--operation-apply image-wand nil
                         'fit-size w h)
  (wand--redisplay))
(put 'wand-fit-size 'transform-operation t)
(put 'wand-fit-size 'menu-name "Fit to size")

(defun wand-liquid-rescale (w h)
  "Rescale image to WxH using liquid rescale."
  (interactive
   (list (read-number "Width: " (Wand:image-width image-wand))
         (read-number "Height: " (Wand:image-height image-wand))))

  (wand--operation-apply image-wand nil
                         'liquid-rescale w h)
  (wand--redisplay))
(put 'wand-liquid-rescale 'transform-operation t)
(put 'wand-liquid-rescale 'menu-name "Liquid rescale")

(defun wand-posterize (levels &optional ditherp)
  "Posterize image.
Levels is a  number of color levels allowed in each channel.
2, 3, or 4 have the most visible effect."
  (interactive "nLevel: \nP")
  (wand--operation-apply image-wand preview-region
                         'posterize levels (not (not ditherp)))
  (wand--redisplay))
(put 'wand-posterize 'transform-operation t)
(put 'wand-posterize 'menu-name "Posterize")

(defun wand-gamma (level)
  "Perform gamma correction.
LEVEL is a positive float.
LEVEL value of 1.00 (read 100%) is no-op."
  (interactive "nLevel: ")
  (wand--operation-apply image-wand preview-region
                         'gamma level)
  (wand--redisplay))
(put 'wand-gamma 'transform-operation t)
(put 'wand-gamma 'menu-name "Gamma")

(defun wand-pattern (pattern &optional op)
  "Enable checkerboard as tile background."
  (interactive (list (completing-read "Pattern: " wand--patterns nil t)
                     (completing-read "Composite Op: "
                                      Wand-CompositeOperators nil t
                                      nil nil wand-pattern-composite-op)))
  (wand--operation-apply image-wand preview-region
                         'pattern pattern op)
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
              (cons 80 (line-pixel-height)))
        (Wand:read-image-data w-out "pattern:horizontal")
        (Wand:MagickDrawImage w-out d-out)

        (cl-flet ((draw-in-out (cop)
                    (Wand-with-wand w-in
                      (setf (Wand:image-size w-in)
                            (cons 80 (line-pixel-height)))
                      (Wand:read-image-data w-in "pattern:vertical")
                      (Wand:MagickDrawImage w-in d-in)
                      (Wand:image-composite w-in w-out (cdr cop) 0 0)
                      (Wand:emacs-insert w-in)
                      (insert " " (car cop) "\n"))))
          (with-output-to-temp-buffer "*Wand-Composite-Ops*"
            (set-buffer standard-output)
            (mapc #'draw-in-out Wand-CompositeOperators)))))))

(defconst wand--patterns
  '(("bricks") ("checkerboard") ("circles") ("crosshatch") ("crosshatch30")
    ("crosshatch45") ("fishscales") ("gray0") ("gray5") ("gray10") ("gray15")
    ("gray20") ("gray25") ("gray30") ("gray35") ("gray40") ("gray45") ("gray50")
    ("gray55") ("gray60") ("gray65") ("gray70") ("gray75") ("gray80") ("gray85")
    ("gray90") ("gray95") ("gray100") ("hexagons") ("horizontal") ("horizontalsaw")
    ("hs_bdiagonal") ("hs_cross") ("hs_diagcross") ("hs_fdiagonal") ("hs_horizontal")
    ("hs_vertical") ("left30") ("left45") ("leftshingle") ("octagons") ("right30")
    ("right45") ("rightshingle") ("smallfishscales") ("vertical") ("verticalbricks")
    ("verticalleftshingle") ("verticalrightshingle") ("verticalsaw")))

(defun wand-list-patterns ()
  "Show available patterns in separate buffer.
A-la `list-colors-display'."
  (interactive)
  (with-output-to-temp-buffer "*Wand-Patterns*"
    (cl-flet ((draw-pattern (pat-name)
                (Wand-with-wand wand
                   (setf (Wand:image-size wand)
                         (cons 80 (line-pixel-height)))
                   (Wand:read-image-data wand (concat "pattern:" pat-name))
                   (Wand:emacs-insert wand))
                (insert " " pat-name "\n")))
      (with-current-buffer "*Wand-Patterns*"
        ;save-excursion
;        (set-buffer standard-output)
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

  (wand-edit-operations operations-list)
  (message "Undo!"))

(defun wand-redo (&optional arg)
  "Redo last operations ARG times."
  (interactive "p")
  (unless undo-list
    (error "Nothing to redo"))
  (dotimes (n arg)
    (let ((op (pop undo-list)))
      (when op
        (apply #'wand--operation-apply image-wand nil (car op) (cdr op)))))
  (wand--redisplay)
  (message "Redo!"))

(defun wand-edit-operations (new-oplist)
  "Edit and reapply operations list."
  (interactive
   (let* ((print-level nil)
          (ops-as-string (if operations-list
                             (prin1-to-string operations-list)
                           ""))
          (new-oplist (read-from-minibuffer
                       "Operations: " ops-as-string read-expression-map
                       t ops-as-string)))
     (list new-oplist)))

  ;; Cut&Paste from undo
  (let ((page (Wand:iterator-index image-wand)))
    (Wand:clear-wand image-wand)
    (Wand:read-image image-wand buffer-file-name)
    (setf (Wand:iterator-index image-wand) page))

  (setq operations-list new-oplist)
  (wand--operation-list-apply image-wand)
  (wand--redisplay))

(defun wand-repeat-last-operation ()
  "Repeat last operation on image."
  (interactive)
  (let ((last-op (car (last operations-list))))
    (when last-op
      (apply #'wand--operation-apply
             image-wand nil (car last-op) (cdr last-op))
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
                 (mapcar #'list (wand-formats-list "*" 'write))
                 nil t nil nil (Wand:image-format image-wand)))
          (nfname (concat (file-name-sans-extension buffer-file-name)
                          "." (downcase ofmt)))
          (fn (read-file-name
               "Filename: "
               (file-name-directory buffer-file-name)
               nfname nil (file-name-nondirectory nfname))))
     (list ofmt fn)))

  (unless (wand-format-can-write-p format)
    (error "Unsupported format for writing: %s" format))

  (when (or (not wand-query-for-overwrite)
            (not (file-exists-p nfile))
            (y-or-n-p (format "File %s exists, overwrite? " nfile)))
    (setf (Wand:image-format image-wand) format)
    (Wand:write-image image-wand nfile)
    (message "File %s saved" nfile)

    ;; Redisplay in case we can do it
    (if (wand-format-can-read-p format)
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
