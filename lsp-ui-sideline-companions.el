;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'ht)
(require 'lsp-ui-sideline)
(require 'flycheck)

(defgroup lsp-ui-sideline-companions nil
  "Splits lsp-ui sideline messages into multiple pieces based on relevant source locations."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui)


(defface lsp-ui-sideline-companions-subline-base
  '((t
     :foreground "black"
     ))
  "Face for subline text for companion messages"
  :group 'lsp-ui-sideline-companions)

(defface lsp-ui-sideline-companions-inline-highlight
  '((t
     :box (:line-width (-1 . -1)
           :color "dark slate gray"
           :style nil))
     )
  "Face for inline highlighting of affected text when displaying lsp diagnostics"
  :group 'lsp-ui-sideline-companions)


(defcustom lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode
  '(
    (rustic-mode . lsp-sideline-companions-rust-diagnostic-is-companion)
    (rust-mode . lsp-sideline-companions-rust-diagnostic-is-companion)
    (typescript-ts-mode . lsp-sideline-companions-generic-diagnostic-split-by-lines)
    )
  "Alist which maps buffer major modes to a predicate which
determines if an LSP diagnostic in that buffer should be treated
as a \"companion\".

Companions are not displayed in the sideline by
`lsp-ui-sideline-mode'. Instead, they are displayed under their
corresponding source locations whenever the point is on the
related error.

Each predicate should return:
 - `nil' to indicate that this diagnostic should not be treated specially (ie do nothing);

 - `t' to treat it as a companion, in which case it this diagnostic should have a
   \"related info\" property, and the location range of that related
   info is used as the related error;

 - a LSP diagnostic range (see `lsp-range?') to treat it as a
   companion, in which case the returned value identifies the
   related error;

 - a list consisting of exactly three elements: the literal
   `:new', a LSP diagnostic range, a list of LSP diagnostics, in
   which case the related error is identified by the range, the
   original diagnostic is not a companion, and the returned
   diagnostics are new diagnostic to be used as companions for
   the related error.
"
  :local t
  :group 'lsp-ui-sideline-companions)

(defcustom lsp-sideline-companions-align-companion-overlay-mode
  'pixel
  "Determines how to align companion overlays to their related error
positions.

- `column' means align to the column of the related error. This
  is the most obvious and simple method but fails in some
  cases (see other options).

- `pixel' means align to the real pixel location of the related
  error, not to the column position. This is useful when there
  are other overlays on the line which push the visual start
  position of the related away from the column position.
"
  :type
  '(choice
    (const :tag "Column Position" column)
    (const :tag "Pixel Position" pixel))
  :group 'lsp-ui-sideline-companions)

(defcustom lsp-sideline-companions-align-companion-overlay-dynamically
  t
  "If non-nil, then companion overlays are aligned dynamically, so
that if buffer text changes where the overlay would appear, the
overlay will move during redisplay. This has not-insignificant
performance cost"
  :type
  'boolean
  :group 'lsp-ui-sideline-companions)

(defcustom lsp-ui-sideline-companions-delay
  0.0
  "Determines how to display companions automatically while moving point through the buffer.

- `0.0' means display companions as soon as point moves to a line
  with a diagnostic.

- any positive number means display companions after that
  delay (in seconds) after point moves to a line with a
  diagnostic.

- `nil' means never display companions automatically based on the
  movement of point. Note that companions are still hidden if the
  point moves away from a line with a diagnostic.
"
  :type
  '(choice
    (const :tag "Never" nil)
    (number :tag "After delay (seconds)")
    (const :tag "Immediately" 0.0))
  :group 'lsp-ui-sideline-companions)


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
(defun my/copy-sequence-rec (x)
  (cond
   ((consp x)
    (cons (my/copy-sequence-rec (car x)) (my/copy-sequence-rec (cdr x))))
   ((hash-table-p x)
    (ht<-alist (ht-map (lambda(a b) (cons (my/copy-sequence-rec a) (my/copy-sequence-rec b))) x)))
   (t x)))

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


(defun count-leading-characters-p(s pred)
  (let ((pos 0))
    (while (and (< pos (length s))
                (funcall pred (seq-elt s pos)))
      (setq pos (+ 1 pos)))
    pos))

(defun count-leading-whitespace(s)
  (count-leading-characters-p s (lambda(c) (memq c (list ?\s)))))

(defun trim-shared-leading-whitespace(strs)
  (let ((c (apply #'min (mapcar #'count-leading-whitespace strs))))
    (if (> c 0)
        (--map (seq-drop it c) strs)
      strs)))

(defun lsp-sideline-companions-generic-diagnostic-split-by-lines (_ diag)
  (let* (
         (msg (lsp:diagnostic-message diag))
         (msg-lines (s-lines msg))
         new-diag
         )

    (when (> (length msg-lines) 1)
          (lsp:set-diagnostic-message diag (car msg-lines))

          (list :new
                (lsp:diagnostic-range diag)
                (list
                 (progn
                  (setq new-diag (my/copy-sequence-rec diag))
                  (lsp:set-diagnostic-message new-diag (s-join "\n" (trim-shared-leading-whitespace (cdr msg-lines))))
                  new-diag)))
          )
    )
  )

;;;###autoload
(defun my/lsp-diagnostics-partition-associated-message (filter-fn all-diags diag)
  (pcase (funcall filter-fn all-diags diag)
    (`t
     (if-let ((range (my/lsp-diagnostic-get-origin-range diag)))
         (progn
           (push (list range diag) my/lsp-associated-overlays)
           nil)
       (message "Internal error: lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode returned `t' for a diagnostic without an origin range; ignoring")
       (list diag)
       )
     )

    ((and res (pred lsp-range?))
     (push (list res diag) my/lsp-associated-overlays)
     nil)

    (`(:new ,(and range (pred lsp-range?)) ,(and new-companions (pred (-all-p #'lsp-diagnostic?))))
     (--each new-companions (push (list range it) my/lsp-associated-overlays))
     (list diag))

    (`nil
     (list diag))

    (else
     (message "Internal error: lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode returned an unrecognized value %S; ignoring" else)
     (list diag)
     )))

;;;###autoload
(cl-defun my/lsp-diagnostics--flycheck-start-around (fn checker callback)
  "start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

  (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)

  ;; if the diagnostics from LSP haven't changed, don't update the flycheck
  ;; errors!  this might cause overlays to have the wrong position. see
  ;; commentary in `my/flycheck-report-failed-syntax-check' for more details
  (when (not my/lsp-diagnostics-dirty)
    (funcall callback 'interrupted nil)

    ;; using interupted seems to inhibit hooks which normally trigger change in
    ;; sideline messages. calling this also
    (lsp-ui-sideline--diagnostics-changed)
    (cl-return-from my/lsp-diagnostics--flycheck-start-around))
  (setq my/lsp-diagnostics-dirty nil)

  ;; this clears any existing sideline overlays
  (my/lsp-diagnostics-pre-send-to-flycheck)

  (let* (
         (diags (lsp--get-buffer-diagnostics))
         (mode major-mode)
         (filter-fn (alist-get mode lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode))
        )
    (setq my/lsp-all-buffer-diags diags)
    (->> (if filter-fn
             (-mapcat (-partial #'my/lsp-diagnostics-partition-associated-message filter-fn diags) diags)
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

(defvar my/lsp-diagnostics-dirty nil)
(defvar-local my/lsp-companions-tracking-point-functions (make-hash-table :weakness 'key :test 'eq))

(defun my/lsp-diagnostics-updated-hook()
  (setq my/lsp-diagnostics-dirty t))


(defun my/lsp-diagnostics-find-exact-range (diags range)
  (-filter (lambda (i) (ht-equal?-rec (lsp:diagnostic-range i) range)) diags))

(defun delete-overlay-closure(o)
  (lambda() (delete-overlay o)))

(defun my/align-to (spec)
  "Returns an :align-to space with the given spec"
  (propertize " " 'display `(space :align-to ,spec)))

(defun my/spacing (spec)
  "Returns an :width space with the given spec"
  (propertize " " 'display `(space :width ,spec)))

(defun my/post-buffer-change-hook(&rest _args)
  (ht-aeach (funcall value key) my/lsp-companions-tracking-point-functions)
  (when (= (ht-size my/lsp-companions-tracking-point-functions) 0)
    (remove-hook 'after-change-functions #'my/post-buffer-change-hook)))

(defun my/make-var-to-function(func)
"\"Pixel Specification for Spaces\" says this about the potential values of `:align-to':
  ... If num is a symbol, symbol, its buffer-local variable binding is used; that binding can be either a number or a cons cell of the forms shown above ..

This allows us to have totally dynamic spacing via align-to (very
cool!) but also requires that we have a unique symbol for every
space we want to create in this way. This function converts a
closure to such a unique symbol, and registers the symbol in an
appropriate place so that the closure is called again on every
buffer change to update the value of that symbol.

This function the **uninterned** symbol which you can use in
`:align-to' as described in the manual."
  (-let*
      ((var (gensym "magic"))
       (fn (lambda (var_) (set var_ (funcall func))))
       )
    (set var (funcall func))
    (puthash var fn my/lsp-companions-tracking-point-functions)
    (add-hook 'after-change-functions #'my/post-buffer-change-hook nil t)
    var))

(defmacro my/make-pixel-spec-handling-mode(&rest forms)
  `(if lsp-sideline-companions-align-companion-overlay-dynamically
      (my/make-var-to-function (lambda() ,@forms))
    ,@forms
    ))

(defun my/prepend-before-lines(prefix0 prefixs str)
  (->> str
       s-lines
       (-map-indexed (lambda(i x) (concat (if (= i 0) prefix0 prefixs) x)))
       (s-join "\n")))

(defconst lsp-sideline-companions-line-identifier-string "↑")

(defun window-relative-pixel-position-x (point window)
  (when-let
      ((p (car (window-absolute-pixel-position point window))))
  (- p
     (nth 0 (window-body-pixel-edges window)))))

(defun my/column-of-marker(m)
  (with-current-buffer (marker-buffer m)
    (save-excursion (goto-char m) (current-column))))

(defun my/lsp-diagnostic-make-companion-offset-spacing (start-point)
  (pcase lsp-sideline-companions-align-companion-overlay-mode
    (`pixel
     (let* ((start-point-m (copy-marker start-point t))
            (current-window (selected-window))
            )
       (my/align-to
       `(+ left-margin
           ,(my/make-pixel-spec-handling-mode
             ;; a spec like `(numberp)' means distance in pixels
             (list
              (or (window-relative-pixel-position-x start-point-m current-window) 0))))))
     )

    (`column
     (my/spacing ;; note: for some unknown reason, align-to doesn't work with the char-width unit in font locked buffers???
      `(+ left-margin
         ,(let* ((start-point-m (copy-marker start-point t)))
            (my/make-pixel-spec-handling-mode
             ;; a spec like `numberp' means distance in char-width units
             ;; (i.e. columns) minus one because the column of point is the
             ;; column AFTER point, we want the column BEFORE point
             (max (- (my/column-of-marker start-point-m) 1) 0)
             )))
      ))))

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
          (start-point (+ char-pos p0))

          (base-msg (or override-msg (lsp:diagnostic-message diag)))
          (base-msg (concat lsp-sideline-companions-line-identifier-string base-msg))

          (base-msg-len (length base-msg))
          (ignore
           (progn
             (setf (plist-get text-properties 'face) (append (plist-get text-properties 'face) '(lsp-ui-sideline-companions-subline-base italic)))
             (set-text-properties 0 base-msg-len text-properties base-msg)
             )
           )

          (align-to-sp (my/lsp-diagnostic-make-companion-offset-spacing start-point))

          (msg (my/prepend-before-lines
                align-to-sp
                ;; adds extra space to offset to the position of the message
                ;; text, which starts after the line identifier. this isn't
                ;; really exact because the line identifier is a unicode
                ;; character with potentially non-fixed width
                (concat align-to-sp "  ")
                base-msg
                ))

          (ov-subline (make-overlay
                       (+ 1 p1)
                       (+ 1 p1) (current-buffer) t t))
          (ov-inline (make-overlay
                      start-point
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
  (--each my/lsp-diags-overlays (with-demoted-errors "Internal error %S" (funcall it)))
  (setq my/lsp-diags-overlays nil))

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

(defvar lsp-ui-sideline-companions-before-switch-line-hook nil)
(defvar lsp-ui-sideline-companions-after-switch-line-hook nil)

(defun lsp-ui-sideline-companions-set-enabled (enabled)
  (interactive)
  (if lsp-ui-sideline-companions-mode
      (progn
        (overlay-recenter (point))

        (with-demoted-errors "lsp-ui-sideline-companions-before-switch-line-hook error %S"
          (run-hooks 'lsp-ui-sideline-companions-before-switch-line-hook))

        (my/lsp-diags-overlays-switch-line nil)
        (when (and enabled my/lsp-ui-sideline-companions-create-closure)
          (funcall my/lsp-ui-sideline-companions-create-closure))

        (with-demoted-errors "lsp-ui-sideline-companions-after-switch-line-hook error %S"
          (run-hooks 'lsp-ui-sideline-companions-after-switch-line-hook))
        )
    (message "Cannot enable sideline companions - mode is disabled")
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

;; the only purpose of this is to keep the OLD errors list if Flycheck was
;; interrupted. This prevents error overlays from moving around if flycheck gets
;; retriggered but LSP diagnostics have not re-run, then the LSP diagnostics
;; refer to the old buffer positions. Then creating new flycheck errors from
;; those ends up with the flycheck overlays on wrong positions.
;;
;; It's still possible for the overlays to be wrong after any buffer changes,
;; but they're more likely to remain correct due to how emacs handles overlays
;; and how errors are typically reported (i.e. if a function call has an error,
;; then the function name is highlighted; if you change the arguments to the
;; function, the error squiggly will remain on the function name until LSP mode
;; re-runs).
;;
;; this appears to be broadly applicable to all LSP modes flychecks and has
;; little to do with this package. However its especially important here because
;; we add so many overlays.
(defun my/flycheck-report-failed-syntax-check (&optional status)
  "Report a failed Flycheck syntax check with STATUS.

STATUS is a status symbol for `flycheck-report-status',
defaulting to `errored'.

Clear Flycheck state, run `flycheck-syntax-check-failed-hook' and
report an error STATUS."

  (if (not (eq status 'interrupted)) (flycheck-clear))

  (setq flycheck-current-syntax-check nil)
  (run-hooks 'flycheck-syntax-check-failed-hook)
  (flycheck-report-status (or status 'errored)))


(defvar lsp-ui-sideline-companions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-?") #'lsp-ui-sideline-companions-toggle)
    map)
  "")

(define-minor-mode lsp-ui-sideline-companions-mode
  ""
  :init-value nil
  :keymap lsp-ui-sideline-companions-mode-map
  (cond
   (lsp-ui-sideline-companions-mode
    (advice-add 'lsp-ui-sideline--diagnostics :after #'my/lsp-ui-sideline--diagnostics--after)
    (advice-add 'lsp-diagnostics--flycheck-start :around #'my/lsp-diagnostics--flycheck-start-around)
    (add-hook 'lsp-diagnostics-updated-hook #'my/lsp-diagnostics-updated-hook)
    (add-function :override (local 'flycheck-report-failed-syntax-check) #'my/flycheck-report-failed-syntax-check)

    ;; called here so that the first update is always considered dirty
    (my/lsp-diagnostics-updated-hook)

    ;; run flycheck if we just enabled companions, which shows the overlays (eventually)
    (when flycheck-mode (flycheck-buffer))

    ;; should we just refuse to enable the minor mode here?
    (when (not (alist-get major-mode lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode))
      (message "Major mode %s does not support lsp-ui-sideline-companions-mode (see `lsp-sideline-companions-show-diagnostic-as-companion-by-major-mode')" major-mode))
    )
   (t
    (advice-remove 'lsp-ui-sideline--diagnostics #'my/lsp-ui-sideline--diagnostics--after)
    (advice-remove 'lsp-diagnostics--flycheck-start #'my/lsp-diagnostics--flycheck-start-around)
    (remove-hook 'lsp-diagnostics-updated-hook #'my/lsp-diagnostics-updated-hook)
    (advice-remove 'flycheck-report-failed-syntax-check #'my/flycheck-report-failed-syntax-check)

    (my/lsp-diagnostics-pre-send-to-flycheck)
    )
   )
)

(defun lsp-ui-sideline-companions-mode-toggle()
  (interactive)
  (lsp-ui-sideline-companions-mode 'toggle))

(provide 'lsp-ui-sideline-companions)
