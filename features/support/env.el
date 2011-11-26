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


(setq refbuf/test-buf "*RefBufTest*")


(defun refbuf/get-reflected (original)
  (loop for buf in (buffer-list)
        when (string-match (concat ".+" (regexp-quote original) ".+")
                           (buffer-name buf))
        collect buf))


(Before
 (setq refbuf/test-dir (make-temp-file "RefBufTestDir-" t)))


(After
 (kill-buffer (get-buffer-create refbuf/test-buf))

 ;; Kill reflected buffers if exists.
 ;; Note that if reflected-buffers works correctly, there should be no
 ;; remaining reflected buffers.
 (loop for buf in (refbuf/get-reflected refbuf/test-buf)
       do (kill-buffer buf))

 (delete-directory refbuf/test-dir t)
 )
