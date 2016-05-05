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
  "a bullet could be reveals during steps. Here are currently supported revealing types:
:c: - conceal, never show this bullet
:a: - appear, show the bullet at once after a keypress
:tw <float-number>: - type writer, animate char by char, waiting float-number seconds in between chars

This function prepares the slide by placing overlays at certain points
"
  (save-excursion
    (outline-back-to-heading)
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
       ((eq type 'show-now)   (progn
                                (overlay-put i 'invisible nil)))
       ((eq type 'appear)     (progn
                                (org-demo-block)
                                (overlay-put i 'invisible nil)))
       ((eq type 'typewriter)
        (let* ((step-text (overlay-get i :step-text))
               (step-col  (overlay-get i :step-col))
               (step-delay (overlay-get i :step-delay))
               (display-text (concat step-text "\n"))
               (l (length step-text))
               (animation nil))
          (org-demo-block)

          ;; Show the bullet
          (overlay-put (overlay-get i :bullet-ol) 'invisible nil)

          ;; speed optimization:
          ;; prepare all strings before displaying:
          (dotimes (q (+ 1 l))
            (add-to-list 'animation (substring display-text 0 (+ q 1))))

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


(provide 'org-demo)
