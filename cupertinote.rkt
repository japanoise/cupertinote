#!/usr/bin/env racket
#lang racket
(require racket/gui/base)
(require string-searchers)

(define program-name "Cupertinote")
(define program-name-lower (string-downcase program-name))
(define program-version "1.0.0")

(define (existing-path? path) (and (path? path) (directory-exists? path)))

(define (maybe-string->path string-or-path)
  (if (string? string-or-path)
      (string->path string-or-path)
      string-or-path))

(define (my/fmt-number n)
  (cond
    [(< n 10) (format "00~v" n)]
    [(< n 100) (format "0~v" n)]
    [#t (format "~v" n)]))

(define data-dir
  (simplify-path
   (if (eq? (system-type 'os) 'windows)
       (build-path (getenv "userprofile") program-name-lower "data")
       (let ([xdg-data-home (getenv "XDG_DATA_HOME")])
         (if xdg-data-home
             (build-path
              (expand-user-path xdg-data-home) program-name-lower)
             (build-path
              (expand-user-path "~/.local/share/") program-name-lower)))) #f))

(define config-dir
  (simplify-path
   (if (eq? (system-type 'os) 'windows)
       (build-path (getenv "userprofile") program-name-lower)
       (let ([xdg-config-home (getenv "XDG_CONFIG_HOME")])
         (if xdg-config-home
             (build-path
              (expand-user-path xdg-config-home) program-name-lower)
             (build-path
              (expand-user-path "~/.config/") program-name-lower)))) #f))

(define (mkdir-p dired)
  (unless (directory-exists? dired) (make-directory dired #o755)))

(mkdir-p config-dir)
(mkdir-p data-dir)

;; ---------------- Frame; utility functions; basic layout --------------
(define (autosave) void)

(define saveup-frame%
  (class frame%
    (super-new)

    (define (on-close)
      (autosave))
    (augment on-close)))

(define frame (new saveup-frame%
                   [label program-name]
                   [width 230]
                   [height 300]
                   [min-width 230]
                   [min-height 300]))

(define (error-message msg)
  (message-box program-name msg frame (list 'ok 'stop)))

(define (about)
  (message-box (format "About ~a" program-name)
               (format "~a version ~a~n~a"
                       program-name
                       program-version
                       "Copyright (C) 2024 japanoise")
               frame (list 'ok)))

(define (grey shade) (make-object color% shade shade shade))

(application-about-handler about)

(define v-pane (new vertical-pane% [parent frame]))

;; ---------------------------- Preferences -----------------------------
(define pref-no-bell #f)

(define pref-note-corner 30)

(define (maybe-bell) (unless pref-no-bell (bell)))

(define (set-note-corner value) void)

(define pref-filename (build-path config-dir "preferences"))

(define (save-prefs)
  (with-handlers
    ([exn:fail? (lambda (e) #f)])
    (let ([fi (open-output-file pref-filename #:exists 'truncate #:permissions #o644)])
      (writeln pref-no-bell fi)
      (writeln pref-note-corner fi)
      (close-output-port fi))))

(define (load-prefs)
  (with-handlers
    ([exn:fail? (lambda (e) #f)])
    (let ([fi (open-input-file pref-filename)])
      (set! pref-no-bell (read fi))
      (set! pref-note-corner (read fi))
      (close-input-port fi))))

(load-prefs)

(define (preferences)
  (let ([dialog (new dialog%
                     [parent frame]
                     [label "Preferences"]
                     [border 12]
                     [spacing 10]
                     [stretchable-width #f]
                     [stretchable-height #f])])
    (let ([use-bell (new check-box%
                         [label "Play alert sounds?"]
                         [parent dialog]
                         [value (not pref-no-bell)])]
          [corner-size (new slider%
                            [parent dialog]
                            [min-value 20]
                            [max-value 100]
                            [init-value pref-note-corner]
                            [style '(horizontal vertical-label)]
                            [label "Note corner size:"])]
          [button-panel (new horizontal-panel%
                             [parent dialog]
                             [spacing 10]
                             [alignment '(center center)])])
          (new button%
               [label "Cancel"]
               [parent button-panel]
               [callback (lambda (b e) (send dialog show #f))])
          (new button%
               [label "OK"]
               [parent button-panel]
               [callback (lambda (b e)
                           (set! pref-no-bell
                                 (not (send use-bell get-value)))
                           (set-note-corner (send corner-size get-value))
                           (set! pref-note-corner (send corner-size get-value))
                           (save-prefs)
                           (send dialog show #f))])
      )
    (send dialog show #t)
    ))

;; ---------------------------- Text editor -----------------------------
(define editor (new editor-canvas% [parent v-pane]
                    [style '(no-hscroll auto-vscroll no-border)]))

#|
;; Uncomment this for a super buggy right-click context menu.

(define context-menu (new popup-menu%))
(append-editor-operation-menu-items context-menu #t)

(define text-with-context-menu%
  (class text%
    (super-new)
    (define/override (on-event ev)
      (if (eq? (send ev get-event-type) 'right-up)
          (send (send this get-admin) popup-menu context-menu
                (send ev get-x) (send ev get-y))
          (super on-event ev)))))

(define text (new text-with-context-menu% [auto-wrap #t]))
|#

(define text (new text% [auto-wrap #t]))
(send editor set-editor text)

;; -------------------- Data structure & operations ---------------------
(define bottom-note-bar void)

(define notebook-dir data-dir)

(define (make-filename num)
  (build-path notebook-dir (format "~a.txt" (my/fmt-number num))))

(define cur-page 0)
(define num-pages 1)

;; Loads a file and returns its contents, or returns empty string if
;; an error occurs
(define (load-file fn)
  (with-handlers ([exn:fail? (lambda (e) "")])
    (let ([fi (open-input-file fn)]
          [out (open-output-string)])
      (for ([byte (in-port read-char fi)])
        (write-char byte out))
      (close-input-port fi)
      (get-output-string out))))

;; Saves editor text to a file and returns #f or an error type
(define (save-file fn)
  (with-handlers
    ([exn:fail? (lambda (e) 'other)])
    (let ([fi (open-output-file fn #:exists 'truncate #:permissions #o644)])
      (write-string (send text get-text) fi)
      (close-output-port fi)
      #f)))

;; Saves current data to file num.txt, with num taking up at least 3 digits
(define (save-page num)
  (save-file (make-filename num)))

;; Loads a file from num.txt (as above) & snarfs the text into the editor
(define (load-page num)
  (send text erase)
  (send text insert (load-file (make-filename num))))

;; Saves current page
(set! autosave (lambda () (save-page cur-page)))

;; Quits the program
(define (quit-notes)
  (autosave)
  (send frame on-exit))

;; Primitive "move to page" operation - used in set-page to move with
;; autosave, and delete-note to move without autosave.
(define (move-page-op num)
  (set! cur-page num)
  (load-page num)
  (send bottom-note-bar refresh)
  (send editor focus))

;; Sets the current page to num, loading the file if it exists
(define (set-page num)
  (autosave)
  (move-page-op num))

(define (next-page)
  (if (< (add1 cur-page) num-pages)
      (set-page (add1 cur-page))
      (begin (maybe-bell) (send editor focus))))

(define (prev-page)
  (if (>= (sub1 cur-page) 0)
      (set-page (sub1 cur-page))
      (begin (maybe-bell) (send editor focus))))

(define (new-note)
  (set-page num-pages)
  (set! num-pages (add1 num-pages)))

(define (goto-callback note-field dialog)
  (lambda (b e)
    (let ([number (string->number
                   (send (send note-field get-editor) get-text))])
      (if (and number (< 0 number) (<= number num-pages))
          (begin
            (set-page (sub1 number))
            (send dialog show #f))
          (maybe-bell)))))

(define (goto-page)
  (let ([dialog (new dialog%
                     [parent frame]
                     [label "Go to note"]
                     [border 12]
                     [spacing 10]
                     [alignment '(left top)]
                     [stretchable-width #f]
                     [stretchable-height #f])])
    (let
        ([label (new message%
                     [parent dialog]
                     [horiz-margin 12]
                     [label (format "There are ~v notes" num-pages)])]
         [note-field (new text-field%
                          [parent dialog]
                          [init-value (number->string (add1 cur-page))]
                          [label "Go to note:"]
                          [callback (lambda (t e)
                                      (when (eq? (send e get-event-type) 'text-field-enter)
                                        (apply (goto-callback t dialog) (list t e))))])]
         [button-panel (new horizontal-panel%
                            [parent dialog]
                            [alignment '(right center)])])
        (begin
          (send note-field focus)
          (new button%
               [label "Cancel"]
               [parent button-panel]
               [callback (lambda (b e) (send dialog show #f)
                           (send editor focus))])
          (new button%
               [label "Go to"]
               [parent button-panel]
               [callback (goto-callback note-field dialog)])))
    (send dialog show #t)))

(define (delete-note)
  (if (= num-pages 1)
      (maybe-bell)
      (when
          (eq?
           (message-box
            "Delete note?"
            "Are you sure you want to permanently delete the current note? This is not undo-able."
            frame '(ok-cancel)) 'ok)
        ;; This is the only really expensive operation, so use a busy
        ;; cursor in case we're running on a slow filesystem.
        (begin-busy-cursor)
        ;; Counter-intuitively, we want to save it before we delete
        ;; it; this prevents the case where we delete a file that
        ;; doesn't exist yet. Saves us a (with-handlers) invocation.
        (autosave)
        (delete-file (make-filename cur-page))
        ;; We can avoid the re-numbering operation if we're deleting
        ;; the last page; also make sure we don't end up off the end.
        (if (= cur-page (sub1 num-pages))
            (move-page-op (sub1 cur-page))
            (begin
              (for ([num (in-range (add1 cur-page) num-pages)])
                (rename-file-or-directory
                 (make-filename num)
                 (make-filename (sub1 num))))
              ;; (move-page-op) takes care of loading in the text &
              ;; updating the page number.
              (move-page-op cur-page)))
        (set! num-pages (sub1 num-pages))
        (end-busy-cursor))))

(define search-term "")
(define search-index 0)
(define search-index-in-note 0)
(define search-all-notes #f)

(define (all-find)
  (let loop ()
    (let ([search-result
           (kmp-string-contains-ci
            (if (= search-index cur-page)
                (send text get-text)
                (load-file (make-filename
                            search-index)))
            search-term search-index-in-note)])
      (if search-result
          (begin
            (set! search-index-in-note
                  (add1 search-result))
            (unless (= cur-page search-index)
              (set-page search-index))
            (send text set-position search-result))
          (begin
            (set! search-index (add1 search-index))
            (set! search-index-in-note 0)
            (if (>= search-index num-pages)
                (maybe-bell)
                (loop)))))))

(define (in-note-find)
  (let ([search-result
         (kmp-string-contains-ci
          (send text get-text)
          search-term search-index-in-note)])
    (if search-result
        (begin
          (set! search-index-in-note (add1 search-result))
          (send text set-position search-result))
        (maybe-bell))))

(define (find-op)
  ;; I lied - finding things is also possibly slow (if you have lots
  ;; of notes, or are hitting worst-case for the algorithm we use.)
  (begin-busy-cursor)
  (if (or (string=? search-term "") (<= num-pages search-index))
      (maybe-bell)
      (if search-all-notes
          (all-find)
          (in-note-find)))
  (end-busy-cursor))

(define (find-callback text-field dialog)
  (lambda (b e)
    (send dialog show #f)
    (set! search-term (send (send text-field get-editor) get-text))
    (find-op)))

(define (start-search)
  (set! search-term "")
  (set! search-index 0)
  (set! search-index-in-note 0)
  (set! search-all-notes #f)
  (let ([dialog (new dialog%
                     [parent frame]
                     [label "Find"]
                     [border 12]
                     [spacing 10]
                     [stretchable-width #f]
                     [stretchable-height #f])])
    (let ([find-field (new text-field%
                          [parent dialog]
                          [label "Find:"]
                          [callback (lambda (t e)
                                      (when (eq? (send e get-event-type) 'text-field-enter)
                                        (apply (find-callback t dialog) (list t e))))])]
         [control-panel (new horizontal-panel%
                             [parent dialog]
                             [spacing 10]
                             [alignment '(center center)])])
      (let ([radio (new radio-box%
                        [label #f]
                        [choices (list "Just in this note" "In all the notes")]
                        [parent control-panel]
                        [callback
                         (lambda (r e)
                           (let ([sel (send r get-selection)])
                             (unless (boolean? sel)
                               (set! search-all-notes
                                     (= 1 sel)))))])]
            [button-panel (new horizontal-panel%
                            [parent control-panel]
                            [alignment '(center center)])])
          (send find-field focus)
          (new button%
               [label "Cancel"]
               [parent button-panel]
               [callback (lambda (b e) (send dialog show #f))])
          (new button%
               [label "Find"]
               [parent button-panel]
               [callback (find-callback find-field dialog)]))
      (send dialog show #t))))

(define (find-next) (find-op))

(define (load-notebook-dir dired)
  (mkdir-p dired)
  (set! notebook-dir dired)
  (set! cur-page 0)
  (set! num-pages 1)
  (load-page cur-page)
  (for-each (lambda (item)
              (when (path-has-extension? item ".txt")
                (let-values ([(base name must-be-dir?) (split-path item)])
                  (let ([basename (path->string (path-replace-extension name ""))])
                    (let ([number (string->number basename)])
                      (when (and (not (directory-exists? item))
                                 number (> (add1 number) num-pages))
                        (set! num-pages (add1 number))))))))
            (directory-list dired #:build? #t)))

(load-notebook-dir data-dir)

;; ----------------------------  Bottom bar -----------------------------
(define bottom-note-bar%
  (class canvas%
    (define corner-size pref-note-corner)
    (define height (+ corner-size 6))
    (define corner-1 (sub1 corner-size))
    (define corner-2 (sub1 corner-1))
    (define anc-y 1)
    (define (number-anc width) (- (/ width 2) 6))

    (define white (grey #xFF))
    (define black (grey #x00))
    (define light-grey (grey #xEE))
    (define dark-grey (grey #x44))

    (define pen-trans (new pen% [width 0] [style 'transparent]))
    (define note-corner
      (new brush%
           [gradient
            (new linear-gradient%
                 [x0 corner-size] [y0 0]
                 [x1 0] [y1 corner-size]
                 [stops (list (list 0.2 white)
                              (list 0.8 dark-grey))])]))

    (define/public (update-corner-size value)
      (set! corner-size value)
      (set! height (+ corner-size 6))
      (set! corner-1 (sub1 corner-size))
      (set! corner-2 (sub1 corner-1))
      (set! note-corner
            (new brush%
                 [gradient
                  (new linear-gradient%
                       [x0 corner-size] [y0 0]
                       [x1 0] [y1 corner-size]
                       [stops (list (list 0.2 white)
                                    (list 0.8 dark-grey))])]))
      (send this min-height height)
      (send this refresh))

    ;; Mouse event
    (define/override (on-event event)
      (when (send event button-up? 'left)
        (let ([x (send event get-x)]
              [y (send event get-y)]
              [width (send this get-width)])
          (when (and (< anc-y y) (< y (+ anc-y corner-size)))
            (if (< x corner-1)
                (if (< x y)
                    (prev-page)
                    (next-page))
                (when (and (< (number-anc width) x)
                           (< x (- width corner-size)))
                  (goto-page)))))))

    (define/override (on-paint)
      (let ([dc (send this get-dc)]
            [width (send this get-width)])
        (begin
          (send dc clear)
          (send dc set-pen black 1 'solid)
          (send dc set-brush light-grey 'solid)

          (send dc draw-text
                (number->string (add1 cur-page)) (number-anc width) 0)

          ;; Outlines
          (send dc draw-rectangle -1 anc-y corner-size corner-size)
          (send dc draw-rectangle -1 (+ anc-y corner-1) (+ 2 width) 8)
          (send dc set-pen light-grey 1 'solid)
          (send dc draw-line -1 (+ anc-y corner-1)
                corner-2 (+ anc-y corner-1))

          ;; Notebook pages
          (send dc set-pen dark-grey 1 'solid)
          (send dc draw-line -1 (+ anc-y corner-size 1)
                (+ 2 width) (+ anc-y corner-size 1))
          (send dc draw-line -1 (+ anc-y corner-size 3)
                (+ 2 width) (+ anc-y corner-size 3))

          ;; Page corner
          (send dc set-pen pen-trans)
          (send dc set-brush note-corner)
          (send dc draw-polygon (list
                                 (cons 0 (+ anc-y 1))
                                 (cons corner-2 (+ anc-y 1))
                                 (cons corner-2 (+ anc-y corner-1))))
          (send dc set-pen dark-grey 1 'solid)
          (send dc draw-line 0 (+ anc-y 1) corner-2 (+ anc-y corner-1)))))

    (super-new [min-height height] [stretchable-height #f])))

(set! bottom-note-bar (new bottom-note-bar% [parent v-pane]))

(set! set-note-corner
      (lambda (v) (send bottom-note-bar update-corner-size v)))

;; ------------------------------ Menu bar ------------------------------
(define menubar (new menu-bar% [parent frame]))

(define m-file (new menu% [label "&File"] [parent menubar]))
(new menu-item%
     [label "&New Note"]
     [shortcut #\n]
     [parent m-file]
     [callback (lambda (m c) (new-note))])
(new menu-item%
     [label "&Close"]
     [shortcut #\w]
     [parent m-file]
     ;; Honestly not sure what this does in the original program other
     ;; than close it... maybe it's just there for the C-w (close tab)
     ;; keybinding?
     [callback (lambda (m c) (quit-notes))])

(new separator-menu-item% [parent m-file])
(new menu-item%
     [label "&Go to note..."]
     [shortcut #\j]
     [parent m-file]
     [callback (lambda (m c) (goto-page))])
(new menu-item%
     [label "&Delete note"]
     [shortcut #\d]
     [parent m-file]
     [callback (lambda (m c) (delete-note))])

(new separator-menu-item% [parent m-file])
;; This is coming out blank on my Linux machine - might just be my printer config.
(new menu-item%
     [label "&Print..."]
     [parent m-file]
     [shortcut #\p]
     [callback (lambda (m c) (send text print))])
(new menu-item%
     [label "&Export to PDF..."]
     [parent m-file]
     [callback (lambda (m c) (send text print #t #t 'pdf))])

(new separator-menu-item% [parent m-file])
(new menu-item%
     [label "&Quit"]
     [parent m-file]
     [shortcut #\q]
     [callback (lambda (m c) (quit-notes))])


(define m-edit (new menu% [label "&Edit"] [parent menubar]))
(append-editor-operation-menu-items m-edit #t)
(send text set-max-undo-history 100)    ; This could maybe go further up?
(new separator-menu-item% [parent m-edit])
(new menu-item%
     [label "&Find..."]
     [parent m-edit]
     [shortcut #\f]
     [callback (lambda (m c) (start-search))])
(new menu-item%
     [label "Find A&gain"]
     [parent m-edit]
     [shortcut #\g]
     [callback (lambda (m c) (find-next))])
(new separator-menu-item% [parent m-edit])
(new menu-item%
     [label "&Preferences..."]
     [parent m-edit]
     [shortcut #\f]
     [callback (lambda (m c) (preferences))])

(define m-help (new menu% [label "&Help"] [parent menubar]))
(new menu-item%
     [label "&About..."]
     [parent m-help]
     [callback (lambda (m c) (about))])

(send frame show #t)
