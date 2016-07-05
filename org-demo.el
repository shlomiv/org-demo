(require 'org-element)
;;(require 'org-tree-slide)

(defvar org-demo--old-mode-line nil)
(make-variable-buffer-local 'org-demo--old-mode-line)

(defvar org-demo--original-buffer)
(defvar org-demo--presentation-buffer)
(defvar org-demo--window-configuration)
(defvar org-demo--dont-autostep nil
  "non nil value inhibits auto stepping. In order to manually step
include a call to (reveal-steps steps) in a ':control flow' block.
note, 'steps' is defined in this context by init-stepper

This should be set in an ':control enter' block using
org-demo-dont-autostep-current-slide.

e.g.:
 #+BEGIN_SRC emacs-lisp :control flow :results none
   (org-demo-dont-autostep-current-slide)
 #+END_SRC

 #+BEGIN_SRC emacs-lisp :control flow :results none
   (org-demo-block)
   (do-something)

   (reveal-steps steps)

   (org-demo-block)
   (do-something-else)
 #+END_SRC
")

(defvar org-demo--org-table-face)

(defun org-demo-showtime ()
  (interactive)
  (setq org-demo--window-configuration (current-window-configuration))
  (setq org-demo--original-buffer (current-buffer))
  (setq org-demo--presentation-buffer (clone-indirect-buffer (concat (buffer-name org-demo--original-buffer) "-org-demo") 't 't))
  (delete-other-windows)
  (org-tree-slide-mode))

(defun org-demo-start ()
  (local-set-key (kbd "q") 'org-demo-finish)
  (setq org-demo--org-table-face (list :inherit (face-attribute 'org-table :inherit)
                                       :height (face-attribute 'org-table :height)
                                       :width (face-attribute 'org-table :width)))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch :height .9 :width 'normal)
  (setq org-tree-slide-breadcrumbs nil)
  (org-demo-hide-mode-line)
  (whitespace-mode -1)
  (fringe-mode '(0 . 0))
  (setq cursor-type nil)
  (if (fboundp 'flyspell-mode)
      (flyspell-mode -1))
  (variable-pitch-mode 1))

(defun org-demo-cleanup ()
  (org-tree-slide-mode -1)
  (kill-buffer org-demo--presentation-buffer)

  (setq org-demo--original-buffer nil)
  (setq org-demo--presentation-buffer nil)
  (set-window-configuration org-demo--window-configuration)
  )

(defun org-demo-finish ()
  (interactive)
  (cleanup-steps)
  (setq cursor-type t)
  (org-demo-show-mode-line)
  ;;(restore-table-face)
  (apply 'set-face-attribute 'org-table nil org-demo--org-table-face)
  (when (fboundp 'flyspell-mode)
    (flyspell-mode t))
  (variable-pitch-mode nil)
  (text-scale-set 0)
  (local-unset-key (kbd "q"))
  ;; finish running inside org-tree-slide and terminate it
  (run-with-timer 0 nil 'org-demo-cleanup)
  )

(defun find-overlays-specifying (prop)
  (let ((overlays (overlays-in (point-min) (point-max)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun get-active-steps ()
  (reverse (find-overlays-specifying :step-type)))

(save-window-excursion)

(defmacro org-demo-with-current-window (&rest body)
  `(let ((b (current-buffer)))
     ,@body
    (select-window (get-buffer-window b))))

(defun org-demo-block()
  (org-demo-with-current-window
   (select-window (get-buffer-window org-demo--presentation-buffer))
   (setq ks (read-key-sequence ""))
   (cond
    ((and (stringp ks) (string= ks "q"))
     (progn (cleanup-steps)
            (throw :terminate 'org-demo-finish)))
    ((eq (key-binding ks) 'org-tree-slide-move-next-tree)
     (progn (cleanup-steps)
            (throw :terminate 'org-tree-slide-move-next-tree)))
    ((eq (key-binding ks) 'org-tree-slide-move-previous-tree)
     (progn (cleanup-steps)
            (throw :terminate 'org-tree-slide-move-previous-tree)))
    )))

(defun org-demo-title-page (&optional SIZE)
  (text-scale-set (or SIZE 5))

  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)


  (setq org-demo-title-page-ol (make-overlay (point-min) (+ 1 (point-min))))
  (overlay-put org-demo-title-page-ol 'after-string " ")
  (overlay-put org-demo-title-page-ol 'invisible 't))

(defun update-org-latex-fragment-scale ()
  (let ((text-scale-factor (expt text-scale-mode-step text-scale-mode-amount)))
    (plist-put org-format-latex-options :scale (* 1.5 text-scale-factor))))

(defun org-demo-start-presenting (&optional SIZE)
  (when org-demo-title-page-ol (delete-overlay org-demo-title-page-ol))

  (add-hook 'text-scale-mode-hook 'update-org-latex-fragment-scale)
  (add-hook 'org-mode-hook 'org-preview-latex-fragment)

  (setq org-tree-slide-header 't)
  (setq org-tree-slide-slide-in-effect 't)

  (text-scale-set (or SIZE 4)))
(defmacro comment (&rest a))


(defun org-tree-get-element-text-end (e)
  "Get the text end position for an element with children,
meaning without the children.."
  (or (let ((s (org-element-property :structure e)))
        (org-list-has-child-p (org-element-property :begin e) s))
      (org-element-property :end e)))

;; stepper
(defun prep-stepper ()
  "a bullet could be reveals during steps. Here are currently supported revealing types:
:c: - conceal, never show this bullet
:a: - appear, show the bullet at once after a keypress
:tw <float-number>: - type writer, animate char by char, waiting float-number seconds in between chars

This function prepares the slide by placing overlays at certain points
"
  (save-excursion
    ;;(org-with-limited-levels (outline-back-to-heading))
    (outline-back-to-heading) ;; wierd "Before first heading" error
    ;;(goto-char (point-min))
    (forward-line)
    (let ((cur-outline-level (org-outline-level))
          (steps '())
          (cont 't))
      (while cont
        (if (and (re-search-forward org-list-full-item-re nil 't);;(re-search-forward " \\+" nil 't)
                 (eq cur-outline-level (save-match-data (org-outline-level))))
            (let ((elem (org-element-at-point)))
              (cond
               ((looking-at "[[:space:]]*:tw[[:space:]]*\\([[:digit:].]*\\)[[:space:]]*:[[:space:]]?")
                (let* ((start (org-element-property :begin (org-element-property :parent elem)))
                       (end   (org-element-property :end elem))
                       (ol        (make-overlay (match-beginning 0) end))
                       (bullet-ol (make-overlay start (org-element-property :begin elem)))
                       (tw-ol     (make-overlay (match-beginning 0) (match-end 0))))
                  (overlay-put ol :step-type 'typewriter)
                  (overlay-put ol :step-delay     (if (eq (match-beginning 1) (match-end 1)) 0.01
                                                    (string-to-number
                                                     (buffer-substring (match-beginning 1) (match-end 1)))))
                  (overlay-put ol :step-text      (buffer-substring (+ (org-element-property :begin elem) (- (match-end 0) (match-beginning 0)))
                                                                    (- end 1)))

                  ;; hide bullet, let ol animation reveal it
                  (overlay-put bullet-ol :step-type 'conceal)

                  ;; we keep bullet-ol on ol so we could reveal it during animation without extra keypresses
                  (overlay-put ol :bullet-ol bullet-ol)

                  ;; hide :tw: directive
                  (overlay-put tw-ol :step-type 'conceal)

                  (add-to-list 'steps tw-ol)
                  (add-to-list 'steps bullet-ol)
                  (add-to-list 'steps ol)))

               ((looking-at "[[:space:]]*\\(:a:[[:space:]]?\\)")
                (let* ((pelem (org-element-property :parent elem))
                       (ol (make-overlay (org-element-property :begin pelem)
                                         (org-element-property :end pelem)))
                       (ap (make-overlay (match-beginning 1) (match-end 1))))

                  (overlay-put ol :step-type 'appear)
                  (overlay-put ap :step-type 'conceal)
                  (add-to-list 'steps ap)
                  (add-to-list 'steps ol)))

               ((looking-at "[[:space:]]*\\(:c:[[:space:]]?\\)")
                (let* ((pelem (org-element-property :parent elem))
                       (ol (make-overlay (org-element-property :begin pelem)
                                         (org-element-property :end pelem)))
                       (c (make-overlay (match-beginning 1) (match-end 1))))
                  (overlay-put ol :step-type 'conceal)
                  (overlay-put c :step-type 'conceal)
                  (add-to-list 'steps c)
                  (add-to-list 'steps ol)))
               ))
          (setq cont nil)))
      (reverse steps))))

(defmacro init-stepper (name)
  `(progn (make-local-variable ',name)
          (setq ,name (prep-stepper))
          (hide-steps ,name)))

(defun hide-steps (&optional steps) (dolist (i (or steps (get-active-steps))) (overlay-put i 'invisible t)))

(defun reveal-steps (&optional steps)
  (dolist (i (or steps (get-active-steps)))
    (let ((type (overlay-get i :step-type)))
      (cond
       ((eq type 'appear)     (progn
                                (org-demo-block)
                                (overlay-put i 'invisible nil)))
       ((eq type 'typewriter)
        (let* ((step-delay (overlay-get i :step-delay))
               (display-text (concat (overlay-get i :step-text) "\n"))
               (animation nil))

          (org-demo-block)

          ;; speed optimization: prepare all strings before displaying:
          (dotimes (q (length display-text))
            (add-to-list 'animation (substring display-text 0 (+ q 1))))

          ;; Show the bullet
          (overlay-put (overlay-get i :bullet-ol) 'invisible nil)

          ;; do animation
          (overlay-put i 'invisible nil)
          (dolist (frame (reverse animation))
            (overlay-put i 'display frame)
            (sit-for step-delay 't)
            )))))))

(defun cleanup-steps (&optional steps) (dolist (i (or steps (get-active-steps))) (delete-overlay i)))

;; general stuff
(defun end-of-heading ()
  (let ((cur-outline-level (org-outline-level)))
    (outline-next-heading)
    (when (< cur-outline-level (org-outline-level))
      (forward-line -1))
    (recenter -1)))

(defun embed-org-latex (&optional size)
  (when (not (org--list-latex-overlays))
    (when size (plist-put org-format-latex-options :scale size))
    (org-toggle-latex-fragment)))


(defun org-demo-dont-autostep-current-slide ()
  (setq org-demo--dont-autostep 't))

(setq org-tree-slide--on-slide-enter   nil)
(setq org-tree-slide--on-slide-flow    nil)
(setq org-tree-slide--on-slide-cleanup nil)
(add-hook 'org-tree-slide--on-slide-enter  '(lambda()
                                             (setq org-demo--dont-autostep nil)
                                             (init-stepper steps)
                                             (end-of-heading)))
(add-hook 'org-tree-slide--on-slide-flow   '(lambda()
                                             (end-of-heading)
                                             (when (not org-demo--dont-autostep)
                                               (reveal-steps steps))))
(add-hook 'org-tree-slide--on-slide-cleanup'(lambda() (cleanup-steps steps)))


(defun org-demo-hide-mode-line ()
  "Hide mode line for a particular buffer."
  (interactive)
  (when mode-line-format
    (setq org-demo--old-mode-line mode-line-format)
    (setq mode-line-format nil)))

(defun org-demo-show-mode-line ()
  "Show mode line for a particular buffer, if it was previously hidden with 'org-demo--hide-mode-line."
  (interactive)
  (if org-demo--old-mode-line
      (setq mode-line-format org-demo--old-mode-line)))


(provide 'org-demo)
