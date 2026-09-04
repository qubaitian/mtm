(in-package #:mtm.prompt)

;; Store the default Prompt template for named Sessions.
(defparameter *prompt-template* "[mtm:~A] ")

;; Store the user-defined Prompt provider.
(defparameter *prompt-provider* nil)

;; Store the latest Prompt provider error.
(defparameter *prompt-error* nil)

;; Return the path of the optional user Prompt configuration.
(defun get-prompt-config-path ()
  "Return the optional user Prompt configuration path."
  (merge-pathnames ".mtm/config" (user-homedir-pathname)))

;; Return the default Prompt line for SESSION-NAME.
(defun get-prompt-default-line (session-name)
  "Return the configured default Prompt line for SESSION-NAME."
  (format nil *prompt-template* session-name))

;; Store CONDITION and report it without stopping the Session manager.
(defun set-prompt-error (condition)
  "Store and report one Prompt configuration or provider CONDITION."
  (setf *prompt-error* condition)
  (format *error-output* "MTM prompt error: ~A~%" condition)
  (finish-output *error-output*)
  condition)

;; Return the latest Prompt provider error.
(defun get-prompt-error ()
  "Return the latest Prompt provider error, or NIL."
  *prompt-error*)

;; Return true when LINE is one safe visible Prompt line.
(defun prompt-line-p (line)
  (and (stringp line)
       (every (lambda (character)
                (and (>= (char-code character) 32)
                     (/= (char-code character) 127)))
              line)))

;; Return one validated Prompt line from PROVIDER.
(defun get-prompt-provider-line (provider session-name)
  (let ((line (funcall provider session-name)))
    (unless (prompt-line-p line)
      (error "Prompt providers must return one visible string."))
    line))

;; Return the current Prompt line for SESSION-NAME.
(defun get-prompt-line (session-name)
  "Return a configured Prompt line for SESSION-NAME."
  (handler-case
      (if *prompt-provider*
          (get-prompt-provider-line *prompt-provider* session-name)
          (get-prompt-default-line session-name))
    (error (condition)
      (set-prompt-error condition)
      (get-prompt-default-line session-name))))

;; Set the default Prompt template used without a provider.
(defun set-prompt-template (template)
  "Set TEMPLATE, which receives the Session name as its FORMAT argument."
  (check-type template string)
  (format nil template "session")
  (setf *prompt-template* template
        *prompt-error* nil)
  template)

;; Set the Prompt provider used by named Session frontends.
(defun set-prompt-provider (provider)
  "Set PROVIDER, which receives a Session name and returns a Prompt line."
  (check-type provider (or null function))
  (setf *prompt-provider* provider
        *prompt-error* nil)
  provider)

;; Load the optional Prompt configuration during manager startup.
(defun set-prompt-config ()
  "Load ~/.mtm/config as trusted Common Lisp when it exists."
  (let (;; Read the optional configuration path once.
        (path (get-prompt-config-path)))
    (when (probe-file path)
      (handler-case
          (let ((*package* (find-package '#:mtm.config)))
            (load path))
        (error (condition)
          (set-prompt-error condition))))
    path))
