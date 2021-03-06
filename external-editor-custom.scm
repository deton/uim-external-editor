(require "i18n.scm")

(define external-editor-im-name-label (N_ "external-editor"))
(define external-editor-im-short-desc (N_ "launch external editor on selection"))

(define-custom-group 'external-editor
                     external-editor-im-name-label
                     external-editor-im-short-desc)

(define-custom 'external-editor-command "gnome-text-editor"
  '(external-editor)
  '(string ".*")
  (N_ "editor command to launch")
  (N_ "long description will be here."))

(define-custom 'external-editor-tmpdir "/tmp"
  '(external-editor)
  '(string ".*")
  (N_ "directory for temporary file")
  (N_ "long description will be here."))

(define-custom 'external-editor-launch-key '("e")
               '(external-editor)
	       '(key)
	       (N_ "[external-editor] launch editor")
	       (N_ "long description will be here"))

(define-custom 'external-editor-read-key '("r")
               '(external-editor)
	       '(key)
	       (N_ "[external-editor] read edited file")
	       (N_ "long description will be here"))

(define-custom 'external-editor-undo-key '("u")
               '(external-editor)
	       '(key)
	       (N_ "[external-editor] undo last read")
	       (N_ "long description will be here"))

(define-custom 'external-editor-switch-default-im-key '("<IgnoreShift>~")
               '(external-editor)
	       '(key)
	       (N_ "[external-editor] switch to default IM")
	       (N_ "long description will be here"))

(define-custom 'external-editor-read-after-editor-exit #t
  '(external-editor)
  '(boolean)
  (N_ "read temporary file after editor exit")
  (N_ "long description will be here."))

(define-custom 'external-editor-read-after-file-modify #t
  '(external-editor)
  '(boolean)
  (N_ "read temporary file after file modify")
  (N_ "long description will be here."))

(define-custom 'external-editor-switch-default-im-after-read #f
  '(external-editor)
  '(boolean)
  (N_ "switch to default IM after read of temporary file")
  (N_ "long description will be here."))

(define-custom 'external-editor-remove-after-read #t
  '(external-editor)
  '(boolean)
  (N_ "remove temporary file after read")
  (N_ "long description will be here."))
