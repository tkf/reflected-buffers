;;; reflected-buffers.el --- indirect buffers w/o shared font lock

;;; Code:

(defun refbuf/with-mode (mode)
  "Open a reflected buffer of the current buffer and set major mode"
  (switch-to-buffer (refbuf/get-or-create (current-buffer)
                                          (format "*%%s (ref|mode:%s)*" mode)
                                          mode))
  )

(defun refbuf/reflect-current-buffer ()
  "Open a reflected buffer of the current buffer"
  (interactive)
  (switch-to-buffer (refbuf/get-or-create (current-buffer)))
  )

(defun refbuf/get-or-create (original-buffer-or-name
                             &optional
                             reflected-format
                             new-buffer-callback)
  "Create a reflected buffer of ORIGINAL-BUFFER-OR-NAME"
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
      ;;   `new-buffer-callback' is called here because the first
      ;;    version of reflected-buffers.el is not resistant to
      ;;    `kill-all-local-variables'. Now, there is no need to set
      ;;    major mode here but I will keep this here to be on the
      ;;    safe side.

      ;; copy: original => reflected
      (with-current-buffer
          original
        (copy-to-buffer reflected (point-min) (point-max)))

      ;; clear undo history of the `reflected' buffer, so that undo
      ;; history of the `reflected' buffer doesn't start from an
      ;; empty buffer.
      (with-current-buffer
          reflected
        (setq buffer-undo-list nil)
        )

      ;; add buffer local hooks
      (with-current-buffer
          original
        (add-hook 'after-change-functions 'refbuf/reflect-change nil t)
        (add-hook 'kill-buffer-hook 'refbuf/kill-reflected-buffers nil t)
        (add-hook 'after-save-hook 'refbuf/reflect-modified-p nil t)
        )
      (with-current-buffer
          reflected
        (add-hook 'after-change-functions 'refbuf/reflect-change nil t)
        (add-hook 'kill-buffer-hook 'refbuf/remove-reflection-from-original
                  nil t)
        (refbuf/reflected-mode)
        )

      (refbuf/add-reflect-change-dest-list reflected original)
      (refbuf/add-reflect-change-dest-list original reflected)
      (refbuf/add-reflected-buffer-list original reflected)
      (refbuf/set-is-reflected-t reflected)
      (refbuf/set-original-buffer original reflected)
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
                   (insert-buffer-substring-no-properties
                    src from to)           ; insert the change
                   ))
            ))
    (refbuf/reflect-modified-p)))
(put 'refbuf/reflect-change 'permanent-local-hook t)


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
          (delq dest refbuf/reflect-change-dest-list))
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
(put 'refbuf/kill-reflected-buffers 'permanent-local-hook t)


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
(put 'refbuf/remove-reflection-from-original 'permanent-local-hook t)


;;; refbuf/save-original-buffer (and its helpers)
(defvar refbuf/original-buffer nil
  "A buffer local variable to store the original buffer")
(make-variable-buffer-local 'refbuf/original-buffer)
(put 'refbuf/original-buffer 'permanent-local t)

(defun refbuf/set-original-buffer (original reflected)
  "Set original buffer of the reflected buffer"
  (with-current-buffer reflected
    (setq refbuf/original-buffer original)))

(defun refbuf/save-original-buffer ()
  (interactive)
  (when refbuf/original-buffer
    (with-current-buffer refbuf/original-buffer
      (save-buffer)
      (message "refbuf: Saved original buffer '%s'" (current-buffer))
      )))

(defun refbuf/reflect-modified-p ()
  "Mark the reflected buffers (un)modified if the original is (un)modified"
  (when refbuf/reflect-change-dest-list
    (let ((src-modified-p (buffer-modified-p)))
      (loop for dest in refbuf/reflect-change-dest-list
            do (with-current-buffer
                   dest
                 (set-buffer-modified-p src-modified-p))))))
(put 'refbuf/reflect-modified-p 'permanent-local-hook t)


;;; Setup minor mode: refbuf/reflected-mode
(defvar refbuf/reflected-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'refbuf/save-original-buffer)
    map)
  "Keymap for `refbuf/reflected-mode'.")

(define-minor-mode refbuf/reflected-mode
  "Minor mode for reflected buffers."
  :init-value nil
  :keymap refbuf/reflected-mode-map
  )


(provide 'reflected-buffers)

;;; reflected-buffers.el ends here
