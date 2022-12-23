;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'ht)
(require 'lsp-ui-sideline)
(require 'flycheck)

(defun ht-equal?-rec (table1 table2)
  "Return t if TABLE1 and TABLE2 have the same keys and values.
Does not compare equality predicates."
  (declare (side-effect-free t))

  (if (and (hash-table-p table1) (hash-table-p table2))
      (let ((keys1 (ht-keys table1))
            (keys2 (ht-keys table2))
            (sentinel (make-symbol "ht-sentinel")))
        (and (equal (length keys1) (length keys2))
             (--all?
              (ht-equal?-rec (ht-get table1 it)
                             (ht-get table2 it sentinel))
              keys1)))
    (equal table1 table2)
    ))

;;;###autoload
(defun my/flycheck-filtering (err)
  ;; (message (format "%s" err))
  ;; t ;; note that returning non-nil prevents further functions being called
  nil
)

(defun lsp-diagnostic-get-related-info (diag)
  "Get the first 'related info' field of an LSP diagnostic"
  (if-let (
           (diag-related-infos (lsp:diagnostic-related-information? diag))
           (at-least-1 (> (length diag-related-infos) 0))
           )
      (aref diag-related-infos 0)))

(defun lsp-diagnostic-get-origin-range (diag)
  "Get the 'location range' field of the first 'related info' field of an LSP diagnostic."
  (if-let (
           (diag-related-info0 (lsp-diagnostic-get-related-info diag))
           )
      (lsp:location-range
       (lsp:diagnostic-related-information-location diag-related-info0))))

(defun lsp-sideline-companions-rust-diagnostic-is-companion (_ diag)
  (if-let ((diag-related-info (lsp-diagnostic-get-related-info diag)))
      (and (equal (lsp:diagnostic-related-information-message diag-related-info) "original diagnostic")
           (lsp:location-range (lsp:diagnostic-related-information-location diag-related-info)))))

(defcustom lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode
  '(
    (rustic-mode . lsp-sideline-companions-rust-diagnostic-is-companion)
    (rust-mode . lsp-sideline-companions-rust-diagnostic-is-companion)
    )
  "Alist which maps buffer major modes to a predicate which
determines if an LSP diagnostic in that buffer should be treated
as a \"companion\".

Companions are not displayed in the sideline by
`lsp-ui-sideline-mode'. Instead, they are displayed under their
corresponding source locations whenever the point is on the
related error.

Each predicate should return either `nil' to indicate that this
diagnostic should not be treated specially; or `t' to treat it as
a companion (in which case it this diagnostic should have a
\"related info\" property, and the location range of that related
info is used as the related error); or a LSP diagnostic range to
treat it as a companion (in which case the returned value
identifies the related error).
"
  :local t)

;;;###autoload
(defun my/lsp-diagnostics-partition-associated-message (filter-fn all-diags diag)
  (pcase (funcall filter-fn all-diags diag)
    (`t
     (if-let ((range (my/lsp-diagnostic-get-origin-range diag)))
         (progn
           (push (list range diag) my/lsp-associated-overlays)
           nil)
       (message "Internal error: lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode returned `t' for a diagnostic without an origin range")
       t
       )
     )

    ((and res (pred lsp-range?))
     (push (list res diag) my/lsp-associated-overlays)
     nil)

    (`nil
     t)))


;;;###autoload
(defun my/lsp-diagnostics--flycheck-start-around (fn checker callback)
  "start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

  (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)
  (my/lsp-diagnostics-pre-send-to-flycheck)

  (let* (
         (diags (lsp--get-buffer-diagnostics))
         (mode major-mode)
         (filter-fn (alist-get mode lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode))
        )
    (setq my/lsp-all-buffer-diags diags)
    (->> (if filter-fn
             (-filter (-partial #'my/lsp-diagnostics-partition-associated-message filter-fn diags) diags)
           diags)
         (-map (-lambda ((&Diagnostic :message :severity? :tags? :code? :source?
                                      :range (&Range :start (&Position :line      start-line
                                                                       :character start-character)
                                                     :end   (&Position :line      end-line
                                                                       :character end-character))))
                 (flycheck-error-new
                  :buffer (current-buffer)
                  :checker checker
                  :filename buffer-file-name
                  :message message
                  :level (lsp-diagnostics--flycheck-calculate-level severity? tags?)
                  :id code?
                  :group source?
                  :line (lsp-translate-line (1+ start-line))
                  :column (1+ (lsp-translate-column start-character))
                  :end-line (lsp-translate-line (1+ end-line))
                  :end-column (1+ (lsp-translate-column end-character)))))
         (funcall callback 'finished))
    )
  )

(defun my/lsp-diagnostics-flycheck-error-level (diag)
  (with-demoted-errors "my/lsp-diagnostics-flycheck-error-level %s"
    (-let (
           ((&Diagnostic :message :severity? :tags?) diag))
      (lsp-diagnostics--flycheck-calculate-level severity? tags?))
    )
  )

;;;###autoload
(defun get-visual-line-start-end (n)
  (save-excursion
    (goto-line 1)
    (vertical-motion n)
    (let ((line-start (point)))
      (end-of-visual-line) ; for the end of the line instead 
      (list line-start (point)))
    ))

;;;###autoload
(defun get-logical-line-start-end (n)
  (save-excursion
    (goto-line n)
    (let ((line-start (point)))
      (end-of-line)
      (list line-start (point)))
    ))


(defvar-local my/lsp-diags-overlays nil)

(defvar-local my/lsp-associated-overlays nil)

(defvar-local my/lsp-all-buffer-diags nil)

(defface lsp-ui-sideline-companions-subline-base
  '((t
     :foreground "black"
     ))
  "Face for subline text for companion messages")

(defface lsp-ui-sideline-companions-inline-highlight
  '((t
     :box (:line-width (-1 . -1)
           :color "dark slate gray"
           :style nil))
     )
  "Face for inline highlighting of affected text when displaying lsp diagnostics")

(defvar lsp-ui-sideline-companions-delay 0.0)

(defun my/lsp-diagnostics-find-exact-range (diags range)
  (-filter (lambda (i) (ht-equal?-rec (lsp:diagnostic-range i) range)) diags)
)

(defun delete-overlay-closure(o)
  (lambda() (delete-overlay o)))

(defun my/lsp-diagnostic-make-companion-overlap (origin-diag diag diag-origin-range text-properties &optional override-msg)
  (-let* (
          (mode-inline nil)
          (source-loc-offset (if mode-inline 1 1))

          ((&Range :start
                  (&Position :line line-pos
                             :character char-pos)
                  :end
                  (&Position :line end-line-pos
                             :character end-char-pos))
           (lsp:diagnostic-range diag)
           )
          ((p0 p1) (get-logical-line-start-end (+ line-pos source-loc-offset)))

          (base-msg (or override-msg (lsp:diagnostic-message diag)))
          (base-msg (concat "↑" base-msg))

          (base-msg-len (length base-msg))
          (ignore
           (progn
             (setf (plist-get text-properties 'face) (append (plist-get text-properties 'face) '(lsp-ui-sideline-companions-subline-base italic)))
             (set-text-properties 0 base-msg-len text-properties base-msg)
             )
           )

          (msg (concat
                 (apply 'concat (-repeat char-pos " "))
                base-msg
                ))

          (ov-subline (make-overlay
                       (+ 1 p1)
                       (+ 1 p1) (current-buffer) t t))
          (ov-inline (make-overlay
                      (+ char-pos p0)
                      (+ end-char-pos p0) (current-buffer) nil t))
         )
    (push (delete-overlay-closure ov-subline) my/lsp-diags-overlays)
    (push (delete-overlay-closure ov-inline) my/lsp-diags-overlays)

    (overlay-put ov-subline 'intangible t)
    (overlay-put ov-subline 'after-string (concat msg "\n"))
    (overlay-put ov-subline 'companion-original-range diag-origin-range)

    (overlay-put ov-inline 'intangible t)
    (overlay-put ov-inline 'face 'lsp-ui-sideline-companions-inline-highlight)
  ))


(defun my/lsp-diagnostics-clear-companion-overlays ()
  (-each my/lsp-diags-overlays #'funcall)
  (setq my/lsp-diags-overlays nil)
  )

(defun my/lsp-diagnostics-pre-send-to-flycheck ()
  (my/lsp-diagnostics-clear-companion-overlays)
  (setq my/lsp-associated-overlays nil)
  )

(defun my/lsp-range-contains-line (range line)
  (-let ((
          (&Range :start (&Position :line start-line)
                  :end   (&Position :line end-line))
          range))
    (and (>= line start-line) (<= line end-line))
    )
)

(defun my/lsp-diags-overlays-switch-line (original &optional text-properties)
  (if (not original)
      (my/lsp-diagnostics-clear-companion-overlays)
    (-let* (
            ;; the companion messages corresponding to the current cursor line
            ;; this is what lsp-ui-sideline does to render an overlay with the current error
            (lines (list (- (line-number-at-pos) 1)))
            (companions-for-line
             (-filter
              (lambda (it)
                (-any
                 (lambda(line)
                   (my/lsp-range-contains-line (nth 0 it) line))
                 lines))
              my/lsp-associated-overlays
              )
             )

            ;; in rare cases (for multiline diagnostic messages) we might have companion messages
            ;; with multiple different source lines, but for now we assume its just one source line
            (any-companion-for-line (car companions-for-line))
            )

      ;; render each companion which exists as a seperate diagnostic
      (-each companions-for-line
        (-lambda ((diag-origin-range diag))
          (let* (
                 (origin-diag
                  (car (my/lsp-diagnostics-find-exact-range
                        my/lsp-all-buffer-diags diag-origin-range)))
                 )

            (my/lsp-diagnostic-make-companion-overlap
             origin-diag
             diag
             diag-origin-range
             text-properties
             )
            )))

      ;; sometimes the original diagnostic is actually a multi-line diagnostic
      ;; which we treat as a "sideline" message for the first line + "subline"
      ;; message for the rest of the lines
      (if any-companion-for-line
          (-let* (
                  ((diag-origin-range diag) any-companion-for-line)
                  (origin-diag
                   (car (my/lsp-diagnostics-find-exact-range
                         my/lsp-all-buffer-diags diag-origin-range)))
                  (origin-diag-lines
                   (s-split "\n" (lsp:diagnostic-message origin-diag)))
                  )

            (if (> (length origin-diag-lines) 1)
                (-each (cdr origin-diag-lines)
                  (lambda (submsg)

                    (my/lsp-diagnostic-make-companion-overlap
                     origin-diag
                     origin-diag
                     diag-origin-range
                     text-properties
                     submsg
                     )

                    ))
              )
            ))
      )
    )
  )


(defun get-text-properties (n from props)
  (apply #'-concat (--map (list it (get-text-property n it from)) props)))

(defun copy-text-properties (n from to props)
  (set-text-properties 0 (if to (length to) 1000) (get-text-properties n from props) to)
  to
  )

(defvar-local my/lsp-ui-sideline-companions-create-closure nil)

(defun lsp-ui-sideline-companions-set-enabled (enabled)
  (interactive)
  (if lsp-ui-sideline-companions-mode
      (progn
        (my/lsp-diags-overlays-switch-line nil)
        (when (and enabled my/lsp-ui-sideline-companions-create-closure)
              (funcall my/lsp-ui-sideline-companions-create-closure))
        )
    (message "Cannot enable sideline companions - mode is disable")
    ))

(defun lsp-ui-sideline-companions-enable ()
  (interactive)
  (lsp-ui-sideline-companions-set-enabled t))

(defun lsp-ui-sideline-companions-disable ()
  (interactive)
  (lsp-ui-sideline-companions-set-enabled nil))

(defun lsp-ui-sideline-companions-toggle ()
  (interactive)
  (lsp-ui-sideline-companions-set-enabled (not (lsp-ui-sideline-companions-companions-are-shown-p))))

(defun lsp-ui-sideline-companions-companions-are-shown-p()
  (not (equal nil my/lsp-diags-overlays)))

(defun my/lsp-ui-sideline--diagnostics--after (&rest _)
  ;; disable any presently disabled sideline companions
  (lsp-ui-sideline-companions-disable)

  ;; check if the current line has companions
  (-when-let*
      ((diags-overlays
        (--filter
         (and
          (equal (overlay-get it 'kind) 'diagnostics)
          )
         lsp-ui-sideline--ovs)
        )
       (sideline-displayed-overlay (car diags-overlays))
       (overlay-text (overlay-get sideline-displayed-overlay 'after-string))
       (overlay-text-props (get-text-properties 1 overlay-text '(face display)))
       )

    ;; create the closure which displays companions later
    (setq my/lsp-ui-sideline-companions-create-closure
          (lambda() (my/lsp-diags-overlays-switch-line sideline-displayed-overlay overlay-text-props)))

    (cond
     ;; do nothing (must be enabled manually)
     ((equal lsp-ui-sideline-companions-delay nil)
      nil
      )

     ;; enable by idle timer
     ((and (numberp lsp-ui-sideline-companions-delay)
           (> lsp-ui-sideline-companions-delay 0.0))
      (-let [timer
             (run-with-idle-timer
              lsp-ui-sideline-companions-delay nil
              #'lsp-ui-sideline-companions-enable
              )]
        (push (lambda() (cancel-timer timer)) my/lsp-diags-overlays)
        )
      )

     ;; enable immediately (delay is zero or nonnil)
     (t
      (lsp-ui-sideline-companions-enable)
      )
     )
    )
  )


(defvar lsp-ui-sideline-companions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-/") #'lsp-ui-sideline-companions-toggle)
    map)
  "")

(define-minor-mode lsp-ui-sideline-companions-mode
  ""
  :init-value nil
  :keymap lsp-ui-sideline-companions-mode-map
  (cond
   (lsp-ui-sideline-companions-mode
    (advice-add 'lsp-ui-sideline--diagnostics :after #'my/lsp-ui-sideline--diagnostics--after)
    (add-hook 'flycheck-process-error-functions #'my/flycheck-filtering -50)
    (advice-add 'lsp-diagnostics--flycheck-start :around #'my/lsp-diagnostics--flycheck-start-around)
    (when flycheck-mode (flycheck-buffer))
    )
   (t
    (advice-remove 'lsp-ui-sideline--diagnostics #'my/lsp-ui-sideline--diagnostics--after)
    (remove-hook 'flycheck-process-error-functions #'my/flycheck-filtering)
    (advice-remove 'lsp-diagnostics--flycheck-start #'my/lsp-diagnostics--flycheck-start-around)

    (my/lsp-diagnostics-pre-send-to-flycheck)
    )
   )
)

(defun lsp-ui-sideline-companions-mode-toggle()
  (interactive)
  (lsp-ui-sideline-companions-mode 'toggle))

(provide 'lsp-ui-sideline-companions)
