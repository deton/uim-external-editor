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
      (list 'primary #f) ; whether text is acquired from primary or selection
      (list 'pid #f)
      (list 'mtime #f)
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
    ;; delay candwin is not yet included in release version of uim.
    (if (symbol-bound? 'im-set-delay-activating-handler!)
      (im-set-delay-activating-handler! im
        external-editor-delay-activating-handler))
    (let ((pc (external-editor-context-new id im)))
      pc)))

(define (external-editor-release-handler pc)
  (let ((filename (external-editor-context-filename pc)))
    (if filename
      (unlink filename)))
  (let ((cleanup-zombie
          (lambda ()
            (process-waitpid -1
              (assq-cdr '$WNOHANG process-waitpid-options-alist)))))
    (let loop ((ret (cleanup-zombie)))
      (if (> (car ret) 0)
        (loop (cleanup-zombie))))))

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
      ((external-editor-switch-default-im-key? key key-state)
        (im-switch-im pc default-im-name))
      (else
        (external-editor-context-set-undo-str! pc #f)
        (im-commit-raw pc)))))

(define (external-editor-key-release-handler pc key state)
  (im-commit-raw pc))

(define (external-editor-process-exited? pid)
  (define (exited? status) (list-ref status 1))
  (define (signaled? status) (list-ref status 2))
  (define (stopped? status) (list-ref status 3))
  (define (exit-status status) (list-ref status 4))
  (let ((status (and pid
                     (process-waitpid pid
                      (assq-cdr '$WNOHANG process-waitpid-options-alist)))))
    (and status
         (or (<= pid 0)
             (= (car status) pid))
         (or (exited? status) (signaled? status)))))

(define (external-editor-delay-activating-handler pc)
  (define (file-modified? filename mtime-prev)
    (let ((mtime (guard (err (else #f))
                  (file-mtime filename))))
      (and mtime-prev mtime (not (= mtime mtime-prev)))))
  (let* ((editor? (external-editor-process-exited?
                    (external-editor-context-pid pc)))
         (filename (external-editor-context-filename pc))
         (file? (file-modified? filename (external-editor-context-mtime pc)))
         (watch?
          (cond
            ((and external-editor-read-after-editor-exit
                  external-editor-read-after-file-modify)
              (if editor?
                (begin
                  (if file?
                    (external-editor-read pc)
                    (if external-editor-remove-after-read
                      (unlink filename))) ; unlink because editor exited
                  #f) ; editor exited => not watch file modify any more
                #t))
            (external-editor-read-after-editor-exit
              (if editor?
                (begin
                  (external-editor-read pc)
                  #f)
                #t))
            ;; only check file modify for new tab on already running gedit
            (external-editor-read-after-file-modify
              (if file?
                (begin
                  (external-editor-read pc)
                  ;; XXX: only read once. Because commit cancels selection
                  ;; and next commit only appends read string not replacing
                  ;; old contents on Firefox/Chrome. It is problem that
                  ;; entire string is appended repeatedly on each modify.
                  #f)
                #t))
            (else
              #t))))
    (if watch?
      (im-delay-activate-candidate-selector pc 1))
    (list 0 0 0)))

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

(define (external-editor-acquire-text pc id)
  (and-let*
    ((ustr (im-acquire-text pc id 'beginning 0 'full))
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
  (define (launch pc str primary? filename filename-old)
    (external-editor-write-file filename str) ; TODO: check return value
    (let ((mtime (guard (err (else #f))
                  (file-mtime filename))))
      (external-editor-context-set-mtime! pc mtime))
    (if filename-old
      (unlink filename-old))
    (external-editor-context-set-filename! pc filename)
    (external-editor-context-set-primary! pc primary?)
    ;; string-split for "xterm -e vim"
    (let* ((cmd-list (string-split external-editor-command " "))
           (pid
            (process-spawn
              (car cmd-list)
              (append cmd-list (list filename)))))
      (external-editor-context-set-pid! pc pid)
      (if (and pid
               (symbol-bound? 'im-delay-activate-candidate-selector-supported?)
               (im-delay-activate-candidate-selector-supported? pc))
        (im-delay-activate-candidate-selector pc 1))))
  (let ((filename-old (external-editor-context-filename pc))
        (filename (string-append external-editor-tmpdir
                    "/uim-external-editor-" (user-name) "-" (time) ".txt"))
        (str (external-editor-acquire-text pc 'selection)))
    (if (string? str)
      (launch pc str #f filename filename-old)
      ;; If no text is selected, try to get all primary text.
      ;; XXX: may fail on Firefox or Google Chrome.
      (let ((str-primary (external-editor-acquire-text pc 'primary)))
        (launch pc (if (string? str-primary) str-primary "")
          #t filename filename-old)))))

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
  (im-deactivate-candidate-selector pc) ; cancel delay timer
  (let ((filename (external-editor-context-filename pc)))
    (if filename
      (let ((str (external-editor-read-file filename)))
        (if external-editor-remove-after-read
          (unlink filename)) ; keep filename in context for repeat read
        (if (and (string? str) (not (string=? str "")))
          (let ((undo-str
                  (external-editor-acquire-text pc
                    (if (external-editor-context-primary pc)
                      'primary
                      'selection))))
            (external-editor-context-set-undo-str! pc
              (if (string? undo-str)
                undo-str
                ""))
            (external-editor-context-set-undo-len! pc (count-char str))
            (if (external-editor-context-primary pc)
              (im-delete-text pc 'primary 'beginning 0 'full))
            (im-commit pc str))))))
  (if external-editor-switch-default-im-after-read
    (im-switch-im pc default-im-name)))

(define (external-editor-undo pc)
  (let ((str (external-editor-context-undo-str pc))
        (len (external-editor-context-undo-len pc)))
    (if str
      (begin
        (if (> len 0)
          (im-delete-text pc 'primary 'cursor len 0))
        (if (not (string=? str ""))
          (im-commit pc str))))))
