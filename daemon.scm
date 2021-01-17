(module daemon
  (daemon)

  (import
    scheme
    chicken.base
    chicken.foreign
    chicken.type

    (only chicken.file.posix
          duplicate-fileno
          file-close
          fileno/stderr
          fileno/stdin
          fileno/stdout
          open-input-file*
          open-output-file*
          port->fileno)
    (only chicken.port
          with-output-to-port)
    (only chicken.process
          create-pipe
          process-fork)
    (only chicken.process-context
          change-directory)
    (only chicken.process-context.posix
          create-session))

  (define-type promise (struct promise))
  (define-type std/r (or input-port string integer (procedure () input-port) promise))
  (define-type std/w (or output-port string integer (procedure () output-port) promise))

  (define eprint
    (if #f

        ; Save stderr so that debug msgs still go to the right place.
        (let ((original-stderr (current-error-port)))
          (lambda args
            (with-output-to-port
              original-stderr
              (lambda ()
                (apply print args)
                (flush-output original-stderr)))))

        (lambda _ #f)))

  ; NOTE: Share a single /dev/null port across all created daemons. Can daemons
  ;       screw with each other because of this?
  (define (/dev/null open-port)
    (let ((promise (delay (open-port "/dev/null"))))
      (cute force promise)))

  (define /dev/null-r (/dev/null open-input-file))
  (define /dev/null-w (/dev/null open-output-file))

  #>
  #include <sys/stat.h>
  #include <sys/types.h>
  <#

  ; NOTE: chicken.process-context.posix.create-session seems to be setsid.
  ;(define setsid (foreign-lambda pid_t "setsid"))
  (define setsid create-session)

  ; TODO: Define `mode` as `mode_t` in case defining it as an `integer` results
  ;       in problems. The following definition results in a compilation error.
  ;(define-foreign-type mode "mode_t")
  (define-foreign-type mode integer)
  (define umask (foreign-lambda mode "umask" mode))

  ; NOTE: From section 1.7 of http://www.faqs.org/faqs/unix-faq/programmer/faq
  ;
  ; 1.7 How do I get my program to act like a daemon?
  ; =================================================
  ; 
  ; A "daemon" process is usually defined as a background process that does not
  ; belong to a terminal session. Many system services are performed by
  ; daemons; network services, printing etc.
  ; 
  ; Simply invoking a program in the background isn't really adequate for these
  ; long-running programs; that does not correctly detach the process from the
  ; terminal session that started it. Also, the conventional way of starting
  ; daemons is simply to issue the command manually or from an rc script; the
  ; daemon is expected to put *itself* into the background.
  ; 
  ; Here are the steps to become a daemon:
  ; 
  ;   1. `fork()' so the parent can exit, this returns control to the command
  ;      line or shell invoking your program.  This step is required so that
  ;      the new process is guaranteed not to be a process group leader. The
  ;      next step, `setsid()', fails if you're a process group leader.
  ; 
  ;   2. `setsid()' to become a process group and session group leader. Since a
  ;      controlling terminal is associated with a session, and this new
  ;      session has not yet acquired a controlling terminal our process now
  ;      has no controlling terminal, which is a Good Thing for daemons.
  ; 
  ;   3. `fork()' again so the parent, (the session group leader), can exit.
  ;      This means that we, as a non-session group leader, can never regain a
  ;      controlling terminal.
  ; 
  ;   4. `chdir("/")' to ensure that our process doesn't keep any directory in
  ;      use. Failure to do this could make it so that an administrator
  ;      couldn't unmount a filesystem, because it was our current directory.
  ; 
  ;      [Equivalently, we could change to any directory containing files
  ;      important to the daemon's operation.]
  ; 
  ;   5. `umask(0)' so that we have complete control over the permissions of
  ;      anything we write. We don't know what umask we may have inherited.
  ; 
  ;      [This step is optional]
  ; 
  ;   6. `close()' fds 0, 1, and 2. This releases the standard in, out, and
  ;      error we inherited from our parent process. We have no way of knowing
  ;      where these fds might have been redirected to. Note that many daemons
  ;      use `sysconf()' to determine the limit `_SC_OPEN_MAX'.  `_SC_OPEN_MAX'
  ;      tells you the maximun open files/process. Then in a loop, the daemon
  ;      can close all possible file descriptors. You have to decide if you
  ;      need to do this or not.  If you think that there might be
  ;      file-descriptors open you should close them, since there's a limit on
  ;      number of concurrent file descriptors.
  ; 
  ;   7. Establish new open descriptors for stdin, stdout and stderr. Even if
  ;      you don't plan to use them, it is still a good idea to have them open.
  ;      The precise handling of these is a matter of taste; if you have a
  ;      logfile, for example, you might wish to open it as stdout or stderr,
  ;      and open `/dev/null' as stdin; alternatively, you could open
  ;      `/dev/console' as stderr and/or stdout, and `/dev/null' as stdin, or
  ;      any other combination that makes sense for your particular daemon.
  ; 
  ; Almost none of this is necessary (or advisable) if your daemon is being
  ; started by `inetd'.  In that case, stdin, stdout and stderr are all set up
  ; for you to refer to the network connection, and the `fork()'s and session
  ; manipulation should *not* be done (to avoid confusing `inetd').  Only the
  ; `chdir()' and `umask()' steps remain as useful.

  ;;; @brief Create a daemon process to run a procedure.
  ;;; @param thunk The thunk to run in the created daemon process.
  ;;; @param cwd The working directory @a thunk will run in. If #f doesn't
  ;;;        change the working directory; If a string, changes to the path
  ;;;        specified by that string; Otherwise, changes to "/". Defaults
  ;;;        to #t if not given, i.e., changes to "/".
  ;;; @param stdin The daemon's stdin.
  ;;; @param stdout The daemon's stdout.
  ;;; @param stderr The daemon's stderr.
  ;;; @param killothers? The `chicken.process.process-fork` optional parameter
  ;;;        `killothers?`.
  ;;; @param want-pid? If #f returns immediatly after the first `fork()`;
  ;;;        otherwise, waits for the daemon's PID. Defaults to #f.
  ;;; @returns #f if want-pid? is #f; otherwise, returns the PID of the daemon
  ;;;          process or #f.
  ;;;
  ;;; @a stdin, @a stdout, and @a stderr, if given, can be a string (filepath),
  ;;;   a port, an integer (file descriptor), a procedure, a promise, or #f. If
  ;;;   a string, a port will be opened for that file; if a port, it will be
  ;;;   used as is; if an integer, a port will be opened from it; if a
  ;;;   procedure, it'll be called with no arguments; if a promise, it'll be
  ;;;   forced; if #f, no change is made. Defaults to "/dev/null" if not given.
  ;;;
  ;;; Note that stdin, stdout, and stderr are changed *AFTER* changing the
  ;;;   working directory.
  ;;;
  ;;; TODO:
  ;;;   * Test `cwd`;
  (: daemon (procedure
              ((procedure () *) #!key (or boolean string) std/r std/w std/w * *)
              (or false fixnum)))
  (define (daemon thunk #!key (cwd #t) (stdin /dev/null-r) (stdout /dev/null-w) (stderr /dev/null-w) (killothers? #f) (want-pid? #f))
    (define (inner-fork-thunk)
      (define (change-std* fd std* current-*-port open-port open-port/fd)
        (define (argument->port arg open-port open-port/fd)
          (cond
            ((port? arg)
             arg)
            ((string? arg)
             (open-port arg))
            ((and (number? arg) (exact? arg))
             (open-port/fd arg))
            ((procedure? arg)
             (arg))
            ((promise? arg)
             (force arg))
            (else (error 'daemon "Argument not of the supported type" arg))))

        (when std*
          ; NOTE: Closing now may result in getting this same fd if opening
          ;       next, so we can skip `dup()`ing.
          (file-close fd)
          (let* ((std* (argument->port std* open-port open-port/fd))
                 (port-fd (port->fileno std*)))

            (eprint "std*: " std*)
            (eprint "std*-fd: " port-fd)

            (unless (= port-fd fd)
              (duplicate-fileno port-fd fd))
            (current-*-port std*))))

      ; Step (4)
      (eprint "step 4")
      (when cwd
        (change-directory (if (string? cwd) cwd "/")))

      ; Step (5)
      (eprint "step 5")
      (umask 0)

      ; Steps (6) & (7)
      (eprint "steps 6 & 7 (stdin)")
      (change-std* fileno/stdin stdin current-input-port open-input-file open-input-file*)

      (eprint "steps 6 & 7 (stdout)")
      (change-std* fileno/stdout stdout current-output-port open-output-file open-output-file*)

      (eprint "steps 6 & 7 (stderr)")
      (change-std* fileno/stderr stderr current-error-port open-output-file open-output-file*)

      ; Finally, run the given thunk.
      (thunk))

    (define (outer-fork-thunk)
      (process-fork inner-fork-thunk killothers?))

    (define ((outer-fork-thunk* pipe/r-fd pipe/w-fd))
      (define (send-pid pid pipe/w-fd)
        (let ((pipe/w-port (open-output-file* pipe/w-fd)))
          (write pid pipe/w-port)
          (newline pipe/w-port)
          (flush-output pipe/w-port)
          (close-output-port pipe/w-port)))

      (file-close pipe/r-fd)

      ; Step (2)
      (eprint "step 2")
      (setsid)

      ; Step (3)
      (eprint "step 3")
      (send-pid (outer-fork-thunk) pipe/w-fd))

    (define (recv-pid pipe/r-fd)
      (let ((port-r (open-input-file* pipe/r-fd)))
        (let ((pid (read port-r)))
          (close-input-port port-r)
          (and (fixnum? pid) pid))))

    ; Step (1)
    (eprint "step 1")
    (if want-pid?

        (let-values (((pipe/r-fd pipe/w-fd) (create-pipe)))
          (process-fork (outer-fork-thunk* pipe/r-fd pipe/w-fd) killothers?)
          (file-close pipe/w-fd)
          (recv-pid pipe/r-fd))

        (begin
          (process-fork outer-fork-thunk killothers?)
          #f)))
  )
