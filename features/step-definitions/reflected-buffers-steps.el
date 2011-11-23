(Given "^I enable \\(.+\\)$"
       (lambda (funcstr)
         (funcall (read funcstr))))

(Given "^I eval (\\(.+\\))$"
       (lambda (body)
         (eval (read (concat "(" body ")")))))
