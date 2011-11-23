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

(Then "^there is no reflected buffer of \"\\(.+\\)\"$"
      (lambda (buffer)
        (let ((reflected (refbuf/get-reflected buffer)))
          (assert (not reflected) nil
                  (concat "There are remaining reflected buffer(s):"
                          (format "%s" reflected))))))
