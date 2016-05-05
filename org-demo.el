(require 'org-element)
;;(require 'org-tree-slide)

(defvar org-demo--original-buffer)
(defvar org-demo--presentation-buffer)
(defvar org-demo--window-configuration)

(defun org-demo-showtime ()
  (interactive)
  (setq org-demo--window-configuration (current-window-configuration))
  (setq org-demo--original-buffer (current-buffer))
  (setq org-demo--presentation-buffer (clone-indirect-buffer (concat (buffer-name org-demo--original-buffer) "-org-demo") 't 't))
  (delete-other-windows)
  (org-tree-slide-mode))

(defun org-demo-start ()

  (set-face-attribute 'org-table nil :inherit 'fixed-pitch :height .9 :width 'normal)
  (setq org-tree-slide-breadcrumbs nil)
  (fringe-mode '(0 . 0))
  ;;(setq cursor-type nil)
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
  (cleanup-steps)
  (setq cursor-type t)
  (when (fboundp 'flyspell-mode)
    (flyspell-mode t))
  (variable-pitch-mode nil)
  (text-scale-set 0)
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


(defun org-demo-start-presenting (&optional SIZE)
  (when org-demo-title-page-ol (delete-overlay org-demo-title-page-ol))

  (defun update-org-latex-fragment-scale ()
    (let ((text-scale-factor (expt text-scale-mode-step text-scale-mode-amount)))
      (plist-put org-format-latex-options :scale (* 1.5 text-scale-factor))))

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
  (save-excursion
    (goto-char (point-min))
    (let ((cur-outline-level (org-outline-level))
          (steps '())
          (cont 't))
      (while cont
        (if (and (re-search-forward " \\+" nil 't)
                 (eq cur-outline-level (save-match-data (org-outline-level))))
            (let ((elem (org-element-at-point)))
              (when (string-prefix-p "+" (org-element-property :bullet elem))
                (cond
                 ((looking-at "[[:space:]]*:tw[[:space:]]*\\([[:digit:].]*\\)[[:space:]]*:")
                  (let ((ol (make-overlay (org-element-property :begin elem)
                                          ;; we have to be careful not to shadow internal bullets
                                          (org-tree-get-element-text-end elem)
                                          ))
                        (c (current-column)))
                    (overlay-put ol :step-type 'typewriter)
                    (overlay-put ol :step-col  (- c 1))
                    (overlay-put ol :step-delay     (if (eq (match-beginning 1) (match-end 1)) 0.01
                                                      (string-to-number
                                                       (buffer-substring (match-beginning 1) (match-end 1)))))
                    (overlay-put ol :step-text      (buffer-substring (+ (org-element-property :contents-begin elem)
                                                                         (- (match-end 0) (match-beginning 0)))
                                                                      (- (org-tree-get-element-text-end elem) 1)))
                    (add-to-list 'steps ol)))

                 ('t (let ((ol (make-overlay (org-element-property :begin elem)
                                             (org-element-property :end elem))))
                       (overlay-put ol :step-type 'appear)
                       (add-to-list 'steps ol)))

                 )))
          (setq cont nil)))
      (reverse steps))))

(defmacro init-stepper (name)
  `(progn (make-local-variable ',name)
          (setq ,name (prep-stepper))
          (hide-steps ,name)))

(defun hide-steps (&optional steps) (dolist (i (or steps (get-active-steps))) (overlay-put i 'invisible t)))
(defun reveal-steps (&optional steps)
  (dolist (i (or steps (get-active-steps)))
    ;;(read-key-sequence "")
    (org-demo-block)
    ;;(overlay-put i 'invisible nil)
    (let ((type (overlay-get i :step-type)))
      (cond
       ((eq type 'appear)     (progn (overlay-put i 'invisible nil)
                                     (message "yeah")))
       ((eq type 'typewriter)
        (let* ((step-text (overlay-get i :step-text))
               (step-col  (overlay-get i :step-col))
               (step-delay (overlay-get i :step-delay))
               (prefix    (concat (make-string step-col ?\s) "+ "))
               (prefix-len (length prefix))
               (display-text (concat prefix step-text "\n"))
               (l (length step-text))
               (animation nil))
          ;;(message "animating")
          ;; speed optimization:
          ;; prepare all strings before displaying:
          (dotimes (q (+ 1 l))
            (add-to-list 'animation (substring display-text 0 (+ prefix-len q 1))))

          (overlay-put i 'invisible nil)
          (dolist (frame (reverse animation))
            ;;(message "   frame ")
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


(provide 'org-demo)
