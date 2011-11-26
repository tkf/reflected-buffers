(Given "^I enable \\(.+\\)$"
       (lambda (funcstr)
         (funcall (read funcstr))))

(Given "^I eval (\\(.+\\))$"
       (lambda (body)
         (eval (read (concat "(" body ")")))))

(Given "^I am in clean buffer \"\\(.+\\)\"$"
       (lambda (buffer)
         (let ((v (vconcat [?\C-x ?b] (string-to-vector buffer))))
           (execute-kbd-macro v))
         (with-current-buffer buffer
           (erase-buffer))
         ))

(Given "^I open a temp file in RefBufTestDir$"
       (lambda ()
         (let ((temporary-file-directory refbuf/test-dir))
           (find-file (make-temp-file "RefBufTestFile-"))
           )
         ))

(Then "^this buffer is\\( not\\|\\) modified$"
      (lambda (modified)
        (if (string= " not" modified)
            (assert (not (buffer-modified-p)) nil
                    (format "the buffer '%s' is modified"
                            (current-buffer)))
          (assert (buffer-modified-p) nil
                  (format "the buffer '%s' is not modified"
                          (current-buffer)))
          )))

(Then "^buffer \"\\(.+\\)\"\\( does not\\|\\) exists?$"
      (lambda (buffer exists)
        (if (string= " does not" exists)
            (assert (not (get-buffer buffer)) nil
                    (message (format "Buffer named \"%s\" exists"
                                     buffer)))
          (assert (get-buffer buffer) nil
                  (message (format "No buffer named \"%s\" exists"
                                   buffer)))
          )
        ))


(Then "^there is no reflected buffer of \"\\(.+\\)\"$"
      (lambda (buffer)
        (let ((reflected (loop for f in (refbuf/get-reflected buffer)
                               collect (format "\"%s\"" f))))
          (assert (not reflected) nil
                  (concat "There are remaining reflected buffer(s): "
                          (mapconcat 'identity reflected ", ")
                          )))))
