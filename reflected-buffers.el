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
         (original-to-reflected
          (refbuf/gene-after-change-function original reflected))
         (reflected-to-original
          (refbuf/gene-after-change-function reflected original))
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

      ;; prevent `kill-all-local-variables' to delete these hooks
      (put original-to-reflected 'permanent-local-hook t)
      (put reflected-to-original 'permanent-local-hook t)
      (message (format "original-to-reflected: %s" original-to-reflected))
      (message (format "reflected-to-original: %s" reflected-to-original))

      ;; set buffer local `after-change-functions'
      (loop for (src dest src-to-dest) in
            `((,original ,reflected ,original-to-reflected)
              (,reflected ,original ,reflected-to-original))
            do (with-current-buffer
                   src
                 (add-hook 'after-change-functions src-to-dest nil t)
                 ;; 3rd arg = nil: append
                 ;; 4th arg = t: make the hook buffer local
                 )
            (message (format
                      "refbuf: added after-change-function (copy: %s -> %s)"
                      (buffer-name src) (buffer-name dest)))
            )

      (with-current-buffer
          reflected
        ;; when `reflected' is killed,
        ;; remove `after-change-functions' from `original'
        (add-hook 'kill-buffer-hook
                  `(lambda ()
                     (with-current-buffer
                         ,original
                       (remove-hook 'after-change-functions
                                    ',original-to-reflected
                                    t) ; remove from buffer local hook
                       )
                     ;; delete symbols
                     (unintern ',original-to-reflected)
                     (unintern ',reflected-to-original)
                     (message
                      (format
                       (concat "refbuf: removed `after-change-function'"
                               " of '%s' because '%s' is killed")
                       (buffer-name ,original)
                       (buffer-name ,reflected)
                       ))
                     )
                  nil  ; append
                  t    ; make the hook buffer local
                  )
        ;; buffer local keys
        (local-set-key "\C-x\C-s" (refbuf/gene-save-other-buffer original))
        )

      (with-current-buffer
          original
        ;; when `original' is killed, kill `reflected'
        (add-hook 'kill-buffer-hook
                  `(lambda ()
                     (kill-buffer ,reflected)
                     ;; delete symbols
                     (unintern ',original-to-reflected)
                     (unintern ',reflected-to-original)
                     (message
                      (format "refbuf: '%s' is killed because '%s' is killed"
                              (buffer-name ,reflected)
                              (buffer-name ,original)
                              ))
                     )
                  nil  ; append
                  t    ; make the hook buffer local
                  )
        )
      )
    reflected
    ))


(defmacro refbuf/gene-after-change-function (src dest)
  (let ((src-name (gensym "refbuf/after-change-function-src-"))
        (dest-name (gensym "refbuf/after-change-function-dest-"))
        (func-name (gensym "refbuf/after-change-function-")))
    `(progn
       (setq ,src-name ,src)
       (setq ,dest-name ,dest)
       ;; (setq ,func-name 'dummy)
       (defun ,func-name (from to change)
         (with-current-buffer
             ,dest-name
           (save-excursion              ; needed to use `goto-char'
             (delete-region from        ; remove pre-changed text
                            (+ from change))
             (goto-char from)           ; the change starts from here
             (insert-buffer-substring ,src-name from to) ; insert the change
             )))
       )))


(defun refbuf/save-other-buffer (other-buffer)
  (with-current-buffer other-buffer (save-buffer)))


(defun refbuf/gene-save-other-buffer (other-buffer)
  `(lambda ()
     (interactive)
     (refbuf/save-other-buffer ,other-buffer)))

(provide 'reflected-buffers)

;;; reflected-buffers.el ends here
