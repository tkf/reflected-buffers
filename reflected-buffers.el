;;; reflected-buffers.el --- indirect buffers w/o shared font lock

;;; Code:

(defun refbuf/with-mode (mode)
  (switch-to-buffer (refbuf/get-or-create (current-buffer)
                                          (format "*%%s (ref|mode:%s)*" mode)
                                          mode))
  )

(defun refbuf/reflect-current-buffer ()
  (interactive)
  (switch-to-buffer (refbuf/get-or-create (current-buffer)))
  )

(defun refbuf/get-or-create (original-buffer-or-name
                             &optional
                             reflected-format
                             new-buffer-callback)
  (interactive "b")
  (let* ((original (get-buffer original-buffer-or-name))
         (reflected-name
          (if reflected-format
              (format reflected-format (buffer-name original))
            (format "*%s (ref)*" (buffer-name original))))
         (reflected-old (get-buffer reflected-name))
         (reflected (if reflected-old
                        reflected-old                       ; use existing one
                      (get-buffer-create reflected-name)))  ; create new one
         )

    (when (not reflected-old)
      ;; set major mode etc.
      (with-current-buffer
          reflected
        (when new-buffer-callback (funcall new-buffer-callback)))
      ;; NOTE:
      ;;   `new-buffer-callback' must be called BEFORE setting any
      ;;   buffer local variables (e.g., `after-change-functions')
      ;;   because this hook is intended to use to set major mode for
      ;;   the reflected buffer and usually `kill-all-local-variables'
      ;;   is called when major mode is started.
      ;;   If `new-buffer-callback' is called after setting local
      ;;   hooks, `kill-all-local-variables' will remove all the
      ;;   hooks, ruining the purpose of this function!

      ;; copy: original => reflected
      (with-current-buffer
          original
        (copy-to-buffer reflected         ; buffer
                        (point-min)       ; start
                        (point-max)))     ; end

      ;; clear undo history of the `reflected' buffer, so that undo
      ;; history of the `reflected' buffer doesn't start from an
      ;; empty buffer.
      (with-current-buffer
          reflected
        (setq buffer-undo-list nil)
        )

      (refbuf/add-reflect-change-dest-list reflected original)
      (refbuf/add-reflect-change-dest-list original reflected)
      (refbuf/add-reflected-buffer-list original reflected)
      (refbuf/set-is-reflected-t reflected)

      (with-current-buffer
          reflected
        ;; buffer local keys
        (local-set-key "\C-x\C-s" (refbuf/gene-save-other-buffer original))
        )
      )
    reflected
    ))


;;; refbuf/reflect-change (and its helpers)
(defvar refbuf/reflect-change-dest-list nil
  "A buffer local variable to store the destinations of reflection")
(make-variable-buffer-local 'refbuf/reflect-change-dest-list)
(put 'refbuf/reflect-change-dest-list 'permanent-local t)

(defun refbuf/reflect-change (from to change)
  (when refbuf/reflect-change-dest-list
    (let ((src (current-buffer)))
      (loop for dest in refbuf/reflect-change-dest-list
            do (with-current-buffer
                   dest
                 (save-excursion           ; needed to use `goto-char'
                   (delete-region
                    from (+ from change))  ; remove pre-changed text
                   (goto-char from)        ; the change starts from here
                   (insert-buffer-substring
                    src from to)           ; insert the change
                   ))
            ))))
(add-hook 'after-change-functions 'refbuf/reflect-change)


;;; refbuf/{add,del}-reflect-change-dest-list
(defun refbuf/add-reflect-change-dest-list (src dest)
  (with-current-buffer
      src
    (push dest refbuf/reflect-change-dest-list)
    ))

(defun refbuf/del-reflect-change-dest-list (src dest)
  (with-current-buffer
      src
    (setq refbuf/reflect-change-dest-list
          (remq dest refbuf/reflect-change-dest-list))
    ;; (delq dest refbuf/reflect-change-dest-list) ; why this doesn't work?
    )
  (message (concat "refbuf: removed '%s' from "
                   "`refbuf/reflect-change-dest-list' of '%s' "
                   "because '%s' is killed")
           dest src dest)
  )


;;; refbuf/kill-reflected-buffers (and its helpers)
(defvar refbuf/reflected-buffer-list nil
  "A buffer local variable to store the buffers to kill

This is used to store the reflected buffers of the original
buffer (see `refbuf/kill-reflected-buffers'). When the current
buffer (original buffer) is killed, all the buffers in this
variable will be killed.")
(make-variable-buffer-local 'refbuf/reflected-buffer-list)
(put 'refbuf/reflected-buffer-list 'permanent-local t)

(defun refbuf/add-reflected-buffer-list (original reflected)
  (with-current-buffer
      original
    (push reflected refbuf/reflected-buffer-list)
    ))

(defun refbuf/kill-reflected-buffers ()
  "When `original' is killed, kill `reflected'"
  (when refbuf/reflected-buffer-list
    (loop for reflected in refbuf/reflected-buffer-list
          do (progn (kill-buffer reflected)
                    (message "refbuf: '%s' is killed because '%s' is killed"
                              reflected (current-buffer))
                    ))
    ))
(add-hook 'kill-buffer-hook 'refbuf/kill-reflected-buffers)


;;; refbuf/remove-reflection-from-original (and its helpers)
(defvar refbuf/is-reflected nil
  "A buffer local variable to store whether the buffer is a reflection")
(make-variable-buffer-local 'refbuf/is-reflected)
(put 'refbuf/is-reflected 'permanent-local t)

(defun refbuf/set-is-reflected-t (buf)
  (with-current-buffer
      buf
    (setq refbuf/is-reflected t))
  )

(defun refbuf/remove-reflection-from-original ()
  "Remove reflection to the current buffer from its original buffer

This function works only when the current buffer is an reflected
buffer, i.e. `refbuf/is-reflected' is non-`nil'.

This function removes the current buffer (reflected buffer) from
`refbuf/reflect-change-dest-list' of original buffer when
the current buffer (reflected buffer) is killed.
`reflected' is killed"
  (when refbuf/is-reflected
    (let ((reflected (current-buffer)))
      (loop for original in refbuf/reflect-change-dest-list
            do (with-current-buffer
                   original
                 (refbuf/del-reflect-change-dest-list original reflected)
                 ))))
  )
(add-hook 'kill-buffer-hook 'refbuf/remove-reflection-from-original)


;;; refbuf/save-other-buffer (and its helpers)
(defun refbuf/save-other-buffer (other-buffer)
  (with-current-buffer other-buffer (save-buffer)))


(defun refbuf/gene-save-other-buffer (other-buffer)
  `(lambda ()
     (interactive)
     (refbuf/save-other-buffer ,other-buffer)))

(provide 'reflected-buffers)

;;; reflected-buffers.el ends here
