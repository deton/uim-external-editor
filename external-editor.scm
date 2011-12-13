;;; launch external editor on selection
;;;
;;; Copyright (c) 2011 KIHARA Hideto https://github.com/deton/uim-external-editor
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(require "i18n.scm")
(require "process.scm")
(require-custom "external-editor-custom.scm")

(define external-editor-encoding "UTF-8")

(define external-editor-context-rec-spec
  (append
    context-rec-spec
    (list
      (list 'filename #f)
      (list 'undo-len 0)
      (list 'undo-str #f))))
(define-record 'external-editor-context external-editor-context-rec-spec)
(define external-editor-context-new-internal external-editor-context-new)

(define external-editor-context-new
  (lambda args
    (let ((pc (apply external-editor-context-new-internal args)))
      pc)))

(define external-editor-init-handler
  (lambda (id im arg)
    (let ((pc (external-editor-context-new id im)))
      pc)))

(define (external-editor-release-handler pc)
  (let ((filename (external-editor-context-filename pc)))
    (if filename
      (unlink filename))))

(define (external-editor-key-press-handler pc key key-state)
  (if (ichar-control? key)
    (begin
      (external-editor-context-set-undo-str! pc #f)
      (im-commit-raw pc))
    (cond
      ((external-editor-launch-key? key key-state)
        (external-editor-context-set-undo-str! pc #f)
        (external-editor-launch pc))
      ((external-editor-read-key? key key-state)
        (external-editor-read pc))
      ((external-editor-undo-key? key key-state)
        (external-editor-undo pc)
        (external-editor-context-set-undo-str! pc #f))
      (else
        (external-editor-context-set-undo-str! pc #f)
        (im-commit-raw pc)))))

(define (external-editor-key-release-handler pc key state)
  (im-commit-raw pc))

(register-im
 'external-editor
 "*"
 external-editor-encoding
 external-editor-im-name-label
 external-editor-im-short-desc
 #f
 external-editor-init-handler
 external-editor-release-handler
 context-mode-handler
 external-editor-key-press-handler
 external-editor-key-release-handler
 #f
 #f
 #f
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )

(define (external-editor-acquire-text pc)
  (and-let*
    ((ustr (im-acquire-text pc 'selection 'beginning 0 'full))
     (latter (ustr-latter-seq ustr)))
    (and (pair? latter)
         (car latter))))

(define (external-editor-write-file filename str)
  (let ((fd (file-open filename
              (file-open-flags-number '($O_WRONLY $O_CREAT))
	      (file-open-mode-number '($S_IRUSR $S_IWUSR)))))
    (and (not (null? fd))
         (< 0 fd)
         (let ((ret (file-write-string fd str)))
            (file-close fd)
            ret))))

(define (external-editor-launch pc)
  ;; process-with-daemon without daemon call.
  (define (process-spawn file . args)
    (let-optionals* args ((argv (list file)))
      (let ((pid (process-fork)))
        (cond ((< pid 0)
               (begin
                 (uim-notify-fatal (N_ "cannot fork"))
                 #f))
              ((= 0 pid) ;; child
               (if (= (process-execute file argv) -1)
                 (uim-notify-fatal (format (_ "cannot execute ~a") file)))
               (_exit 1))
              (else
               pid)))))
  (let ((filename-old (external-editor-context-filename pc))
        (filename (string-append "/tmp/uim-external-editor-" (time) ".txt"))
        (str (external-editor-acquire-text pc)))
    (if (string? str)
      (begin
        (external-editor-write-file filename str) ; TODO: check return value
        (if filename-old
          (unlink filename-old))
        (external-editor-context-set-filename! pc filename)
        (process-spawn external-editor-command
          (list external-editor-command filename))))))

(define (external-editor-read-file filename)
  ;; file-read-line without newline check.
  (define (file-read-all port)
    (let loop ((c (file-read-char port))
               (rest '()))
      (cond ((or (eof-object? c)
                 (not c))
             (list->string (reverse rest)))
            (else
             (loop (file-read-char port) (cons c rest))))))
  (let*
    ((fd (file-open filename
          (file-open-flags-number '($O_RDONLY)) 0))
     (res
      (call-with-open-file-port fd
        (lambda (port)
          (file-read-all port)))))
    res))

(define (external-editor-read pc)
  (define (count-char str)
    (string-length
      (with-char-codec external-editor-encoding
        (lambda ()
          (%%string-reconstruct! (string-copy str))))))
  (let ((filename (external-editor-context-filename pc)))
    (if filename
      (let ((str (external-editor-read-file filename)))
        (if external-editor-unlink-after-read
          (begin
            (unlink filename)
            (external-editor-context-set-filename! pc #f)))
        (if (and (string? str) (not (string=? str "")))
          (let ((sel (external-editor-acquire-text pc)))
            (external-editor-context-set-undo-str! pc
              (if (string? sel)
                sel
                ""))
            (external-editor-context-set-undo-len! pc (count-char str))
            (im-commit pc str)))))))

(define (external-editor-undo pc)
  (let ((str (external-editor-context-undo-str pc))
        (len (external-editor-context-undo-len pc)))
    (if str
      (begin
        (if (> len 0)
          (im-delete-text pc 'primary 'cursor len 0))
        (if (not (string=? str ""))
          (im-commit pc str))))))
