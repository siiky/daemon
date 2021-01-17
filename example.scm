(import chicken.io chicken.port)
(import srfi-42)
(import daemon)

(print "daemon starting")
(let ((stdin "stdin.txt") ; Let each process read the file
      ; Share the output files.
      (stdout (open-output-file "stdout.txt"))
      (stderr (open-output-file "stderr.txt")))
  (let ((pids
          (list-ec (:range i 0 10)
                   (daemon
                     (lambda ()
                       (print "started[" i "]")
                       (do-ec (:port ln (current-input-port) read-line)
                              (begin
                                (with-output-to-port (current-error-port)
                                                     (cute print "stderr[" i "]: " ln))
                                (print "stdout[" i "]: " ln)
                                (sleep 1))))
                     #:cwd #f
                     #:stdin stdin
                     #:stdout stdout
                     #:stderr stderr
                     #:want-pid? #t))))

    (print "daemons started (PIDs " pids ")")))
