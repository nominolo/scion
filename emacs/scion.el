

;;;; License
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-and-compile
  (require 'cl)
  (unless (fboundp 'define-minor-mode)
    (require 'easy-mmode)
    (defalias 'define-minor-mode 'easy-mmode-define-minor-mode)))
(require 'hideshow)
(eval-when (compile)
  (require 'apropos)
  (require 'outline)
  (require 'ido)
  ;; (require 'etags)
  )

(defgroup scion nil
  "Interaction with the Superior Lisp Environment."
  :prefix "scion-"
  :group 'applications)

(defgroup scion-ui nil
  "Interaction with the Superior Lisp Environment."
  :prefix "scion-"
  :group 'scion)

(defcustom scion-kill-without-query-p nil
  "If non-nil, kill SCION processes without query when quitting Emacs.
This applies to the *inferior-lisp* buffer and the network connections."
  :type 'boolean
  :group 'scion-ui)

(defgroup scion-haskell nil
  "Haskell server configuration."
  :prefix "scion-"
  :group 'scion)

(defcustom scion-connected-hook nil
  "List of functions to call when SCION connects to Haskell."
  :type 'hook
  :group 'scion-haskell)

(defcustom scion-haskell-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'scion-haskell)

(defcustom scion-port 4005
  "Port to use as the default for `scion-connect'."
  :type 'integer
  :group 'scion-haskell)

(make-variable-buffer-local
 (defvar scion-modeline-string nil
   "The string that should be displayed in the modeline if
`scion-extended-modeline' is true, and which indicates the
current connection, package and state of a Lisp buffer.
The string is periodically updated by an idle timer."))

;;;---------------------------------------------------------------------------

(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR and
evaluate BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (destructuring-bind ((op &rest rands) &rest body) clause
                         `(,op (destructuring-bind ,rands ,operands
                                 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "Elisp destructure-case failed: %S" ,tmp))))))))

;; Interface
(defmacro* scion-with-popup-buffer ((name &optional package
                                          connection emacs-snapshot)
                                    &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
PACKAGE is the value `scion-buffer-package'.
CONNECTION is the value for `scion-buffer-connection'.
If nil, no explicit connection is associated with
the buffer.  If t, the current connection is taken.

If EMACS-SNAPSHOT is non-NIL, it's used to restore the previous
state of Emacs after closing the temporary buffer. Otherwise, the
current state will be saved and later restored."
  `(let* ((vars% (list ,(if (eq package t) '(scion-current-package) package)
                       ,(if (eq connection t) '(scion-connection) connection)
                       ;; Defer the decision for NILness until runtime.
                       (or ,emacs-snapshot (scion-current-emacs-snapshot))))
          (standard-output (scion-popup-buffer ,name vars%)))
     (with-current-buffer standard-output
       (prog1 (progn ,@body)
         (assert (eq (current-buffer) standard-output))
         (setq buffer-read-only t)
         (scion-init-popup-buffer vars%)))))

(put 'scion-with-popup-buffer 'lisp-indent-function 1)

;;;---------------------------------------------------------------------------


;; dummy definitions for the compiler
(defvar scion-net-coding-system)
(defvar scion-net-processes)
(defvar scion-default-connection)



(defun scion-connect (host port &optional coding-system)
  "Connect to a running Swank server."
  (interactive (list (read-from-minibuffer "Host: " scion-haskell-host)
                     (read-from-minibuffer "Port: " (format "%d" scion-port)
                                           nil t)))
  (when (and (interactive-p) scion-net-processes
             (y-or-n-p "Close old connections first? "))
    (scion-disconnect))
  (message "Connecting to Scion Server on port %S.." port)
  (let ((coding-system (or coding-system scion-net-coding-system)))
    (scion-check-coding-system coding-system)
    (message "Connecting to Scion Server on port %S.." port)
    (let* ((process (scion-net-connect host port coding-system))
           (scion-dispatching-connection process))
      (scion-setup-connection process))))

;;;---------------------------------------------------------------------------
;;;; Networking
;;;---------------------------------------------------------------------------
;;;
;;; This section covers the low-level networking: establishing
;;; connections and encoding/decoding protocol messages.
;;;
;;; Each SCION protocol message begins with a 3-byte length header
;;; followed by an S-expression as text. [XXX: The sexp must be readable
;;; both by Emacs and by Common Haskell, so if it contains any embedded
;;; code fragments they should be sent as strings.]
;;;
;;; The set of meaningful protocol messages are not specified
;;; here. They are defined elsewhere by the event-dispatching
;;; functions in this file and in Scion/Server/Emacs.hs.

(defvar scion-net-processes nil
  "List of processes (sockets) connected to Haskell.")

(defvar scion-net-process-close-hooks '()
  "List of functions called when a scion network connection closes.
The functions are called with the process as their argument.")

(defun scion-secret ()
  "Finds the magic secret from the user's home directory.
Returns nil if the file doesn't exist or is empty; otherwise the first
line of the file."
  nil) 					; disable for now

;;;---------------------------------------------------------------------------
;;; Interface

(defun scion-net-connect (host port coding-system)
  "Establish a connection with a Scion Server.

<hostname> <port> <coding-system> -> <network-stream-process>"
  (let* ((inhibit-quit nil)
         (proc (open-network-stream "Scion Server" nil host port))
         (buffer (scion-make-net-buffer " *scion-connection*")))
    (push proc scion-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'scion-net-filter)
    (set-process-sentinel proc 'scion-net-sentinel)
    (scion-set-query-on-exit-flag proc)
    (when (fboundp 'set-process-coding-system)
      (scion-check-coding-system coding-system)
      (set-process-coding-system proc coding-system coding-system))
    (when-let (secret (scion-secret))
      (scion-net-send secret proc))
    proc))

(defun scion-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (buffer-disable-undo))
    buffer))

(defun scion-set-query-on-exit-flag (process)
  "Set PROCESS's query-on-exit-flag to `scion-kill-without-query-p'."
  (when scion-kill-without-query-p
    ;; avoid byte-compiler warnings
    (let ((fun (if (fboundp 'set-process-query-on-exit-flag)
                   'set-process-query-on-exit-flag
                 'process-kill-without-query)))
      (funcall fun process nil))))


;;;---------------------------------------------------------------------------
;;;;; Coding system madness

(defvar scion-net-valid-coding-systems
  '((iso-latin-1-unix nil "iso-latin-1-unix")
    (iso-8859-1-unix  nil "iso-latin-1-unix")
    (binary           nil "iso-latin-1-unix")
    ;; (utf-8-unix       t   "utf-8-unix")
;;     (emacs-mule-unix  t   "emacs-mule-unix")
;;     (euc-jp-unix      t   "euc-jp-unix")
    )
  "A list of valid coding systems. 
Each element is of the form: (NAME MULTIBYTEP CL-NAME)")

(defun scion-find-coding-system (name)
  "Return the coding system for the symbol NAME.
The result is either an element in `scion-net-valid-coding-systems'
or NIL."
  (let* ((probe (assq name scion-net-valid-coding-systems)))
    (if (and probe (if (fboundp 'check-coding-system)
                       (ignore-errors (check-coding-system (car probe)))
                     (eq (car probe) 'binary)))
        probe)))

(defvar scion-net-coding-system
  (find-if 'scion-find-coding-system 
           '(iso-latin-1-unix iso-8859-1-unix binary))
  "*Coding system used for network connections.
See also `scion-net-valid-coding-systems'.")
  
(defun scion-check-coding-system (coding-system)
  "Signal an error if CODING-SYSTEM isn't a valid coding system."
  (interactive)
  (let ((props (scion-find-coding-system coding-system)))
    (unless props
      (error "Invalid scion-net-coding-system: %s. %s"
             coding-system (mapcar #'car scion-net-valid-coding-systems)))
    (when (and (second props) (boundp 'default-enable-multibyte-characters))
      (assert default-enable-multibyte-characters))
    t))

(defcustom scion-repl-history-file-coding-system 
  (cond ((scion-find-coding-system 'utf-8-unix) 'utf-8-unix)
        (t scion-net-coding-system))
  "*The coding system for the history file."
  :type 'symbol
  :group 'scion-repl)

(defun scion-coding-system-mulibyte-p (coding-system)
  (second (scion-find-coding-system coding-system)))

(defun scion-coding-system-cl-name (coding-system)
  (third (scion-find-coding-system coding-system)))

;;;---------------------------------------------------------------------------
;;; Interface

(defun scion-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC.
This is the lowest level of communication. The sexp will be READ and
EVAL'd by Lisp."
  (let* ((msg (concat (scion-prin1-to-string sexp) "\n"))
         (string (concat (scion-net-encode-length (length msg)) msg))
         (coding-system (cdr (process-coding-system proc))))
    (scion-log-event sexp)
    (cond ((scion-safe-encoding-p coding-system string)
           (process-send-string proc string))
          (t (error "Coding system %s not suitable for %S"
                    coding-system string)))))

(defun scion-safe-encoding-p (coding-system string)
  "Return true iff CODING-SYSTEM can safely encode STRING."
  (if (featurep 'xemacs)
      ;; FIXME: XEmacs encodes non-encodeable chars as ?~ automatically
      t
    (or (let ((candidates (find-coding-systems-string string))
              (base (coding-system-base coding-system)))
          (or (equal candidates '(undecided))
              (memq base candidates)))
        (and (not (multibyte-string-p string))
             (not (scion-coding-system-mulibyte-p coding-system))))))

(defun scion-net-close (process &optional debug)
  (setq scion-net-processes (remove process scion-net-processes))
  (when (eq process scion-default-connection)
    (setq scion-default-connection nil))
  (cond (debug         
         (set-process-sentinel process 'ignore)
         (set-process-filter process 'ignore)
         (delete-process process))
        (t
         (run-hook-with-args 'scion-net-process-close-hooks process)
         ;; killing the buffer also closes the socket
         (kill-buffer (process-buffer process)))))

(defun scion-net-sentinel (process message)
  (message "Connection to Scion server closed unexpectedly: %s" message)
  (scion-net-close process))

;;; Socket input is handled by `scion-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun scion-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (scion-process-available-input process))

(defun scion-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (scion-net-have-input-p)
      (let ((event (scion-net-read-or-lose process))
            (ok nil))
        (scion-log-event event)
        (unwind-protect
            (save-current-buffer
              (scion-dispatch-event event process)
              (setq ok t))
          (unless ok
            (scion-run-when-idle 'scion-process-available-input process)))))))

(defun scion-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (scion-net-decode-length))))

(defun scion-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 
         (if (featurep 'xemacs) itimer-short-interval 0) 
         nil function args))

(defun scion-net-read-or-lose (process)
  (condition-case error
      (scion-net-read)
    (error
     (debug)
     (scion-net-close process t)
     (error "net-read error: %S" error))))

(defun scion-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (scion-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)))
    (assert (plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (current-buffer)))
      (delete-region (point-min) end))))

(defun scion-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun scion-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun scion-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length 
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

;;;---------------------------------------------------------------------------


;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->Lisp networking concept.
;;;
;;; Emacs has a connection to each Lisp process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Lisps simultaneously.
;;;
;;; A connection consists of a control socket, optionally an extra
;;; socket dedicated to receiving Lisp output (an optimization), and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Lisp process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `scion-dispatching-connection' if dynamically bound, or
;;;   `scion-buffer-connection' if this is set buffer-local, or
;;;   `scion-default-connection' otherwise. 
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `scion-default-connection'. This connection can be interactively
;;; reassigned via the connection-list buffer.
;;;
;;; When a command creates a new buffer it will set
;;; `scion-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `scion-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Lisp handles commands in
;;; Lisp-mode source buffers, and scion hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.

(defvar scion-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `scion-buffer-connection' and `scion-default-connection'.")

(make-variable-buffer-local
 (defvar scion-buffer-connection nil
   "Network connection to use in the current buffer.
This overrides `scion-default-connection'."))

(defvar scion-default-connection nil
  "Network connection to use by default.
Used for all Lisp communication, except when overridden by
`scion-dispatching-connection' or `scion-buffer-connection'.")

(defun scion-current-connection ()
  "Return the connection to use for Lisp interaction.
Return nil if there's no connection."
  (or scion-dispatching-connection
      scion-buffer-connection
      scion-default-connection))

(defun scion-connection ()
  "Return the connection to use for Lisp interaction.
Signal an error if there's no connection."
  (let ((conn (scion-current-connection)))
    (cond ((and (not conn) scion-net-processes)
           (or (scion-auto-select-connection)
               (error "No default connection selected.")))
          ((not conn)
           (or (scion-auto-connect)
               (error "Not connected.")))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))

(defvar scion-auto-connect 'never)

(defun scion-auto-connect ()
  (cond ((or (eq scion-auto-connect 'always)
             (and (eq scion-auto-connect 'ask)
                  (y-or-n-p "No connection.  Start Scion? ")))
         (save-window-excursion
           (scion) ; XXX
           (while (not (scion-current-connection))
             (sleep-for 1))
           (scion-connection)))
        (t nil)))

(defvar scion-auto-select-connection 'ask)

(defun scion-auto-select-connection ()
  (let* ((c0 (car scion-net-processes))
         (c (cond ((eq scion-auto-select-connection 'always) c0)
                  ((and (eq scion-auto-select-connection 'ask)
                        (y-or-n-p 
                         (format "No default connection selected.  %s %s? "
                                 "Switch to" (scion-connection-name c0))))
                   c0))))
    (when c
      (scion-select-connection c)
      (message "Switching to connection: %s" (scion-connection-name c))
      c)))

(defun scion-select-connection (process)
  "Make PROCESS the default connection."
  (setq scion-default-connection process))

(defun scion-cycle-connections ()
  "Change current scion connection, and make it buffer local."
  (interactive)
  (let* ((tail (or (cdr (member (scion-current-connection)
                                scion-net-processes))
                   scion-net-processes))
         (p (car tail)))
    (scion-select-connection p)
    (unless (eq major-mode 'scion-repl-mode)
      (setq scion-buffer-connection p))
    (message "Lisp: %s %s" (scion-connection-name p) (process-contact p))))

(defmacro* scion-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `scion-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (scion-connection)
                           (error "No connection")))
     ,@body))

(put 'scion-with-connection-buffer 'lisp-indent-function 1)


(defun scion-compute-connection-state (conn)
  (cond ((null conn) :disconnected) 
        ;((scion-stale-connection-p conn) :stale)
        ;((scion-debugged-connection-p conn) :debugged)
        ((and (scion-use-sigint-for-interrupt conn) 
              (scion-busy-p conn)) :busy)
        ((eq scion-buffer-connection conn) :local)
        (t :connected)))

(defun scion-connection-state-as-string (state)
  (case state
    (:connected       "")
    (:disconnected    "not connected")
    (:busy            "busy..")
    (:debugged        "debugged..")
    (:stale           "stale")
    (:local           "local")
    ))

;;; Connection-local variables:

(defmacro scion-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `scion-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
        (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
         (scion-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
         `(scion-with-connection-buffer (,process)
            (setq (\, (quote (\, real-var))) (\, store))
            (\, store)))
       '(\, varname))))

(put 'scion-def-connection-var 'lisp-indent-function 2)

;; Let's indulge in some pretty colours.
(unless (featurep 'xemacs)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(scion-def-connection-var\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))

(scion-def-connection-var scion-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(scion-def-connection-var scion-pid nil
  "The process id of the Haskell process.")

(scion-def-connection-var scion-connection-name nil
  "The short name for connection.")

;;;;; Connection setup

(defvar scion-connection-counter 0
  "The number of SCION connections made. For generating serial numbers.")

;;; Interface
(defun scion-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((scion-dispatching-connection process))
    (scion-init-connection-state process)
    (scion-select-connection process)
    process))

(defun scion-init-connection-state (proc)
  "Initialize connection state in the process-buffer of PROC."
  ;; To make life simpler for the user: if this is the only open
  ;; connection then reset the connection counter.
  (when (equal scion-net-processes (list proc))
    (setq scion-connection-counter 0))
  (scion-with-connection-buffer ()
    (setq scion-buffer-connection proc))
  (setf (scion-connection-number proc) (incf scion-connection-counter))
  ;; We do the rest of our initialization asynchronously. The current
  ;; function may be called from a timer, and if we setup the REPL
  ;; from a timer then it mysteriously uses the wrong keymap for the
  ;; first command.
  (scion-eval-async '(connection-info)
                    (scion-curry #'scion-set-connection-info proc)))

(defun scion-set-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (let ((scion-dispatching-connection connection))
    (destructuring-bind (&key pid version
                              &allow-other-keys) info
      (scion-check-version version connection)
      (setf (scion-pid) pid
	    (scion-connection-name) (format "%d" pid)))
    (let ((args nil))
      (run-hooks 'scion-connected-hook))
    (message "Connected.")))

(defvar scion-protocol-version 1)

(defun scion-check-version (version conn)
  (or (equal version scion-protocol-version)
      (equal scion-protocol-version 'ignore)
      (y-or-n-p 
       (format "Versions differ: %s (scion client) vs. %s (scion server). Continue? "
               scion-protocol-version version))
      (scion-net-close conn)
      (top-level)))

(defun scion-generate-connection-name (lisp-name)
  (loop for i from 1
        for name = lisp-name then (format "%s<%d>" lisp-name i)
        while (find name scion-net-processes 
                    :key #'scion-connection-name :test #'equal)
        finally (return name)))

(defun scion-connection-close-hook (process)
  (when (eq process scion-default-connection)
    (when scion-net-processes
      (scion-select-connection (car scion-net-processes))
      (message "Default connection closed; switched to #%S (%S)"
               (scion-connection-number)
               (scion-connection-name)))))

(add-hook 'scion-net-process-close-hooks 'scion-connection-close-hook)

;;;;; Commands on connections

(defun scion-disconnect ()
  "Disconnect all connections."
  (interactive)
  (mapc #'scion-net-close scion-net-processes))

(defun scion-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (if (featurep 'xemacs)
      (car (process-id connection))
    (cadr (process-contact connection))))

(defun scion-process (&optional connection)
  "Return the Lisp process for CONNECTION (default `scion-connection').
Can return nil if there's no process object for the connection."
  nil
  ;; (let ((proc (scion-inferior-process connection)))
;;     (if (and proc 
;;              (memq (process-status proc) '(run stop)))
;;         proc))
  )

;; Non-macro version to keep the file byte-compilable. 
;; (defun scion-set-inferior-process (connection process)
;;   (setf (scion-inferior-process connection) process))

(defvar scion-inhibit-pipelining t
  "*If true, don't send background requests if Lisp is already busy.")

(defun scion-background-activities-enabled-p ()
  nil)


;;;; Communication protocol

;;;;; Emacs Lisp programming interface
;;;
;;; The programming interface for writing Emacs commands is based on
;;; remote procedure calls (RPCs). The basic operation is to ask Lisp
;;; to apply a named Lisp function to some arguments, then to do
;;; something with the result.
;;;
;;; Requests can be either synchronous (blocking) or asynchronous
;;; (with the result passed to a callback/continuation function).  If
;;; an error occurs during the request then the debugger is entered
;;; before the result arrives -- for synchronous evaluations this
;;; requires a recursive edit.
;;;
;;; You should use asynchronous evaluations (`scion-eval-async') for
;;; most things. Reserve synchronous evaluations (`scion-eval') for
;;; the cases where blocking Emacs is really appropriate (like
;;; completion) and that shouldn't trigger errors (e.g. not evaluate
;;; user-entered code).
;;;
;;; We have the concept of the "current Lisp package". RPC requests
;;; always say what package the user is making them from and the Lisp
;;; side binds that package to *BUFFER-PACKAGE* to use as it sees
;;; fit. The current package is defined as the buffer-local value of
;;; `scion-buffer-package' if set, and otherwise the package named by
;;; the nearest IN-PACKAGE as found by text search (first backwards,
;;; then forwards).
;;;
;;; Similarly we have the concept of the current thread, i.e. which
;;; thread in the Lisp process should handle the request. The current
;;; thread is determined solely by the buffer-local value of
;;; `scion-current-thread'. This is usually bound to t meaning "no
;;; particular thread", but can also be used to nominate a specific
;;; thread. The REPL and the debugger both use this feature to deal
;;; with specific threads.

(make-variable-buffer-local
 (defvar scion-current-thread t
   "The id of the current thread on the Lisp side.  
t means the \"current\" thread;
:repl-thread the thread that executes REPL requests;
fixnum a specific thread."))

(make-variable-buffer-local
 (defvar scion-buffer-package nil
   "The Lisp package associated with the current buffer.
This is set only in buffers bound to specific packages."))


(defun scion-current-package ()
  nil)

(defvar scion-accept-process-output-supports-floats 
  (ignore-errors (accept-process-output nil 0.0) t))

(defun scion-accept-process-output (&optional process timeout)
  "Like `accept-process-output' but the TIMEOUT argument can be a float."
  (cond (scion-accept-process-output-supports-floats
         (accept-process-output process timeout))
        (t
         (accept-process-output process 
                                (if timeout (truncate timeout))
                                ;; Emacs 21 uses microsecs; Emacs 22 millisecs
                                (if timeout (truncate (* timeout 1000000)))))))

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar scion-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

;;; `scion-rex' is the RPC primitive which is used to implement both
;;; `scion-eval' and `scion-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* scion-rex ((&rest saved-vars)
                      (sexp &optional 
                            (package '(scion-current-package))
                            (thread 'scion-current-thread))
                      &rest continuations)
  "(scion-rex (VAR ...) (SEXP &optional PACKAGE THREAD) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

PACKAGE is evaluated and Lisp binds *BUFFER-PACKAGE* to this package.
The default value is (scion-current-package).

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because Emacs20 cannot
deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (scion-dispatch-event 
        (list :emacs-rex ,sexp ,package ,thread
              (lambda (,result)
                (destructure-case ,result
                  ,@continuations)))))))



(defun scion-eval (sexp &optional package)
  "Evaluate EXPR on the superior Lisp and return the result."
  (when (null package) (setq package (scion-current-package)))
  (let* ((tag (gensym (format "scion-result-%d-" 
                              (1+ (scion-continuation-counter)))))
	 (scion-stack-eval-tags (cons tag scion-stack-eval-tags)))
    (apply
     #'funcall 
     (catch tag
       (scion-rex (tag sexp)
           (sexp package)
         ((:ok value)
          (unless (member tag scion-stack-eval-tags)
            (error "Reply to canceled synchronous eval request tag=%S sexp=%S"
                   tag sexp))
          (throw tag (list #'identity value)))
         ((:abort)
          (throw tag (list #'error "Synchronous Lisp Evaluation aborted"))))
       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (scion-connection)))
         (while t 
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
           (scion-accept-process-output nil 0.01)))))))

(defun scion-eval-async (sexp &optional cont package)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (scion-rex (cont (buffer (current-buffer)))
	(sexp (or package (scion-current-package)))
    ((:ok result)
     (when cont
       (set-buffer buffer)
       (funcall cont result)))
    ((:abort)
     (message "Evaluation aborted."))))



;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(scion-def-connection-var scion-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(scion-def-connection-var scion-continuation-counter 0
  "Continuation serial number counter.")

(defvar scion-event-hooks)

(defun scion-dispatch-event (event &optional process)
  (let ((scion-dispatching-connection (or process (scion-connection))))
    (or (run-hook-with-args-until-success 'scion-event-hooks event)
        (destructure-case event
;;           ((:write-string output &optional target)
;;            (scion-write-string output target))
          ((:emacs-rex form package thread continuation)
           (when (and (scion-use-sigint-for-interrupt) (scion-busy-p))
             (scion-display-oneliner "; pipelined request... %S" form))
           (let ((id (incf (scion-continuation-counter))))
             (push (cons id continuation) (scion-rex-continuations))
             (scion-send `(:emacs-rex ,form 
				      ;,package ,thread 
				      ,id))))
          ((:return value id)
           (let ((rec (assq id (scion-rex-continuations))))
             (cond (rec (setf (scion-rex-continuations)
                              (remove rec (scion-rex-continuations)))
                        (funcall (cdr rec) value))
                   (t
                    (error "Unexpected reply: %S %S" id value)))))
          ((:emacs-interrupt thread)
           (scion-send `(:emacs-interrupt ,thread)))
;;           ((:read-string thread tag)
;;            (assert thread)
;;            (scion-repl-read-string thread tag))
          ((:emacs-return-string thread tag string)
           (scion-send `(:emacs-return-string ,thread ,tag ,string)))
          ((:eval-no-wait fun args)
           (apply (intern fun) args))
;;           ((:eval thread tag form-string)
;;            (scion-check-eval-in-emacs-enabled)
;;            (scion-eval-for-lisp thread tag form-string))
          ((:emacs-return thread tag value)
           (scion-send `(:emacs-return ,thread ,tag ,value)))
          ((:ping thread tag)
           (scion-send `(:emacs-pong ,thread ,tag)))
          ((:reader-error packet condition)
           (scion-with-popup-buffer ("*Scion Error*")
             (princ (format "Invalid protocol message:\n%s\n\n%S"
                            condition packet))
             (goto-char (point-min)))
           (error "Invalid protocol message"))))))

(defun scion-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (scion-net-send sexp (scion-connection)))

(defun scion-stop-server ()
  "Stop the server we are currently connected to."
  (interactive)
  (scion-send '(:quit)))


(defun scion-use-sigint-for-interrupt (&optional connection)
  nil)

(defun scion-busy-p (&optional conn)
  "True if Haskell has outstanding requests.
Debugged requests are ignored."
  nil)

(defun scion-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (scion-oneliner msg)))))

(defun scion-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (position ?\n string) most-positive-fixnum)
                           (1- (frame-width)))))

(defun scion-curry (fun &rest args)
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun scion-rcurry (fun &rest args)
  `(lambda (&rest more) (apply ',fun (append more ',args))))


;;;;; Snapshots of current Emacs state

;;; Window configurations do not save (and hence not restore)
;;; any narrowing that could be applied to a buffer.
;;;
;;; For this purpose, we introduce a superset of a window
;;; configuration that does include the necessary information to
;;; properly restore narrowing.
;;;
;;; We call this superset an Emacs Snapshot.

(defstruct (scion-narrowing-configuration
             (:conc-name scion-narrowing-configuration.))
  narrowedp beg end)

(defstruct (scion-emacs-snapshot (:conc-name scion-emacs-snapshot.))
  ;; We explicitly store the value of point even though it's implicitly
  ;; stored in the window-configuration because Emacs provides no
  ;; way to access the things stored in a window configuration.
  window-configuration narrowing-configuration point-marker)

(defun scion-current-narrowing-configuration (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (make-scion-narrowing-configuration :narrowedp (scion-buffer-narrowed-p)
                                        :beg (point-min-marker)
                                        :end (point-max-marker))))

(defun scion-set-narrowing-configuration (narrowing-cfg)
  (when (scion-narrowing-configuration.narrowedp narrowing-cfg)
    (narrow-to-region (scion-narrowing-configuration.beg narrowing-cfg)
                      (scion-narrowing-configuration.end narrowing-cfg))))

(defun scion-current-emacs-snapshot (&optional frame)
  "Returns a snapshot of the current state of FRAME, or the
currently active frame if FRAME is not given respectively."
  (with-current-buffer
      (if frame
          (window-buffer (frame-selected-window (selected-frame)))
          (current-buffer))
    (make-scion-emacs-snapshot
     :window-configuration    (current-window-configuration frame)
     :narrowing-configuration (scion-current-narrowing-configuration)
     :point-marker            (point-marker))))

(defun scion-set-emacs-snapshot (snapshot)
  "Restores the state of Emacs according to the information saved
in SNAPSHOT."
  (let ((window-cfg    (scion-emacs-snapshot.window-configuration snapshot))
        (narrowing-cfg (scion-emacs-snapshot.narrowing-configuration snapshot))
        (marker        (scion-emacs-snapshot.point-marker snapshot)))
    (set-window-configuration window-cfg) ; restores previously current buffer.
    (scion-set-narrowing-configuration narrowing-cfg)
    (goto-char (marker-position marker))))

(defun scion-current-emacs-snapshot-fingerprint (&optional frame)
  "Return a fingerprint of the current emacs snapshot.
Fingerprints are `equalp' if and only if they represent window
configurations that are very similar (same windows and buffers.)

Unlike real window-configuration objects, fingerprints are not
sensitive to the point moving and they can't be restored."
  (mapcar (lambda (window) (list window (window-buffer window)))
          (scion-frame-windows frame)))

(defun scion-frame-windows (&optional frame)
  "Return the list of windows in FRAME."
  (loop with last-window = (previous-window (frame-first-window frame))
        for window = (frame-first-window frame) then (next-window window)
        collect window
        until (eq window last-window)))

;;;;; Temporary popup buffers

(make-variable-buffer-local
 (defvar scion-popup-buffer-saved-emacs-snapshot nil
   "The snapshot of the current state in Emacs before the popup-buffer
was displayed, so that this state can be restored later on.
Buffer local in popup-buffers."))

(make-variable-buffer-local
 (defvar scion-popup-buffer-saved-fingerprint nil
   "The emacs snapshot \"fingerprint\" after displaying the buffer."))



(defun scion-popup-buffer (name buffer-vars)
  "Return a temporary buffer called NAME.
The buffer also uses the minor-mode `scion-popup-buffer-mode'.
Pressing `q' in the buffer will restore the window configuration
to the way it is when the buffer was created, i.e. when this
function was called."
  (when (and (get-buffer name) (kill-buffer (get-buffer name))))
  (with-current-buffer (get-buffer-create name)
    (set-syntax-table lisp-mode-syntax-table)
    (prog1 (pop-to-buffer (current-buffer))
      (scion-init-popup-buffer buffer-vars))))

(defun scion-init-popup-buffer (buffer-vars)
  (scion-popup-buffer-mode 1)
  (setq scion-popup-buffer-saved-fingerprint
        (scion-current-emacs-snapshot-fingerprint))
  (multiple-value-setq (scion-buffer-package 
                        scion-buffer-connection
                        scion-popup-buffer-saved-emacs-snapshot)
    buffer-vars))

(define-minor-mode scion-popup-buffer-mode 
  "Mode for displaying read only stuff"
  nil
  (" Scion-Tmp" scion-modeline-string)
  '(("q" . scion-popup-buffer-quit-function)
    ;("\C-c\C-z" . scion-switch-to-output-buffer)
    ;; ("\M-." . scion-edit-definition)
    ))

(make-variable-buffer-local
 (defvar scion-popup-buffer-quit-function 'scion-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

(defun scion-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `scion-popup-buffer-quit-function'."
  (interactive)
  (funcall scion-popup-buffer-quit-function kill-buffer-p))

;; Interface
(defun scion-popup-buffer-quit (&optional kill-buffer-p)
  "Get rid of the current (temp) buffer without asking.
Restore the window configuration unless it was changed since we
last activated the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (scion-popup-buffer-snapshot-unchanged-p)
      (scion-popup-buffer-restore-snapshot))
    (with-current-buffer buffer
      (setq scion-popup-buffer-saved-emacs-snapshot nil) ; buffer-local var!
      (cond (kill-buffer-p (kill-buffer nil))
            (t (bury-buffer))))))

(defun scion-popup-buffer-snapshot-unchanged-p ()
  (equalp (scion-current-emacs-snapshot-fingerprint)
          scion-popup-buffer-saved-fingerprint))

(defun scion-popup-buffer-restore-snapshot ()
  (let ((snapshot scion-popup-buffer-saved-emacs-snapshot))
    (assert snapshot) 
    (scion-set-emacs-snapshot snapshot)))


;;;;; Event logging to *scion-events*
;;;
;;; The *scion-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar scion-log-events t
  "*Log protocol events to the *scion-events* buffer.")

(defvar scion-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *scion-events*.")

(defvar scion-event-buffer-name "*scion-events*"
  "The name of the scion event buffer.")

(defun scion-log-event (event)
  "Record the fact that EVENT occurred."
  (when scion-log-events
    (with-current-buffer (scion-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (scion-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
                 outline-minor-mode)
        (hide-entry))
      (goto-char (point-max)))))

(defun scion-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun scion-events-buffer ()
  (or (get-buffer scion-event-buffer-name)
      (let ((buffer (get-buffer-create scion-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (set (make-local-variable 'outline-regexp) "^(")
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) "")
          (when scion-outline-mode-in-events-buffer
            (outline-minor-mode)))
        buffer)))

;;;;; Buffer related

(defun scion-buffer-narrowed-p (&optional buffer)
  "Returns T if BUFFER (or the current buffer respectively) is narrowed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun scion-column-max ()
  (save-excursion
    (goto-char (point-min))
    (loop for column = (prog2 (end-of-line) (current-column) (forward-line))
          until (= (point) (point-max))
          maximizing column)))


;;;---------------------------------------------------------------------------

(defmacro* scion-handling-failure ((res-var) &body body)
  (let ((x (gensym))
	(val (gensym)))
  `(lambda (,x)
     (destructure-case ,x
       ((:ok ,val)
	(let ((,res-var ,val))
	  ,@body))
       ((:error ,val)
	(message "Remote command failed: %s" ,val)
	nil)))))

(defun scion-open-cabal-project (dist-dir)
  "Open a Cabal project.

The first argument is dist directory (typically <project-root>/dist/)"
  (interactive "DDist dir: ")
  (scion-eval-async `(open-cabal-project ,(expand-file-name dist-dir)) 
		    (scion-handling-failure (x)
		      (message (format "Cabal project loaded: %s" x))))
  nil)

(defun scion-load-library ()
  "Load the library of the current cabal project."
  (interactive)
  (scion-eval-async `(load-component library)
    (scion-handling-failure (x)
      (destructure-case x
        ((:ok warns)
	 (let ((num-warns (length warns)))
	   (message "Library loaded.  (%s warning(s))"
		    (if (>= num-warns 0) num-warns "no"))))
	((:error errs warns)
	 (let ((num-errs (length errs))
	       (num-warns (length warns)))
	   (message "Library failed to load.  (%s error(s), %s warning(s))"
		    num-errs num-warns)))))))

(defun scion-supported-languages ()
  ;; TODO: cache result
  (scion-eval '(list-supported-languages)))

(defun haskell-insert-language (lang)
  ;; TODO: automatically jump to or insert LANGUAGE pragma
  (interactive
   (let ((langs (scion-supported-languages)))
     (list (ido-completing-read "Language: " langs))))
  (save-excursion
    (goto-char (point-min))
    (insert "{-# LANGUAGE " lang " #-}\n")))

(defun scion-supported-pragmas ()
  ;; TODO: cache result
  (scion-eval '(list-supported-pragmas)))

(defun haskell-insert-pragma (pragma)
  (interactive (let ((choices (scion-supported-pragmas)))
		 ;; standard completing-read cannot even deal properly
		 ;; with upper-case words.
		 (list (ido-completing-read "Pragma: " choices))))
  (insert "{-# " (upcase pragma) "  #-}")
  (backward-char 4))

(defun scion-supported-flags ()
  ;; TODO: cache result
  (scion-eval '(list-supported-flags)))

(defun haskell-insert-flag (flag)
  ;; TODO: automatically insert/add OPTIONS pragma
  (interactive
   (let ((flags (scion-supported-flags)))
     (list (ido-completing-read "Flag: " flags))))
  (insert flag))

(defun scion-exposed-modules ()
  (scion-eval '(list-exposed-modules)))

(defun haskell-insert-module-name (mod)
  (interactive 
   (let ((mods (scion-exposed-modules)))
     (list (ido-completing-read "Module: " mods))))
  (insert mod))

