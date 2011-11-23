(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq enclose-root-path project-directory)
  (setq enclose-util-path (expand-file-name "util" project-directory)))

(add-to-list 'load-path enclose-root-path)
(add-to-list 'load-path (expand-file-name "espuds" enclose-util-path))
(add-to-list 'load-path
             (expand-file-name
              "emacs-lisp"
              (expand-file-name
               "lisp"
               (expand-file-name
                "ert" enclose-util-path))))

(require 'reflected-buffers)
(require 'espuds)
(require 'ert)

(Before
 ;; Start with a clean slate.
 (switch-to-buffer (get-buffer-create "*RefBufTest*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (deactivate-mark)
 )

(After
 (kill-buffer (get-buffer-create "*RefBufTest*"))

 ;; Kill reflected buffers if exists.
 ;; Note that if reflected-buffers works correctly, there should be no
 ;; remaining reflected buffers.
 (loop for buf in (buffer-list)
       do (when (string-match
                 "*RefBufTest*" (buffer-name buf))
            (kill-buffer buf)
            ))
 )
