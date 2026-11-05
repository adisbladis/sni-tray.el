;;; sni-tray.el --- StatusNotifierItem system tray in the echo area -*- lexical-binding: t -*-

;; Copyright (C) 2026 adisbladis

;; Author: adisbladis <adisbladis@gmail.com>
;; URL: https://github.com/adisbladis/sni-tray.el
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: unix, convenience
;; SPDX-License-Identifier: GPL-3.0-only

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License version 3, as published
;; by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;;; Commentary:

;; A StatusNotifierItem tray at the right end of the echo area, like
;; exwm-systemtray.  Emacs acts as both the watcher and the host.

;;; Code:

(require 'cl-lib)
(require 'dbus)
(require 'seq)
(require 'subr-x)

(defgroup sni-tray nil
  "System tray (StatusNotifierItem host) in the echo area."
  :group 'environment
  :prefix "sni-tray-")

(defcustom sni-tray-icon-size nil
  "Icon height in pixels.  nil follows the height of the font."
  :type '(choice (const :tag "Font height" nil) integer))

(defcustom sni-tray-menu-function #'sni-tray-popup-menu
  "Function that shows an item's menu, called with a keymap.
For example `sni-tray-popup-menu' or `tmm-prompt'."
  :type '(choice (function-item sni-tray-popup-menu)
                 (function-item tmm-prompt)
                 function))

(defcustom sni-tray-separator " "
  "String between tray icons."
  :type 'string)

(defcustom sni-tray-frame-predicate #'sni-tray-not-floating-p
  "Function called with a frame, non-nil if it should get a tray.
Frames without an echo area of their own never get one."
  :type '(choice (function-item sni-tray-not-floating-p)
                 (const :tag "Every frame" always)
                 function))

(defconst sni-tray--watcher-service "org.kde.StatusNotifierWatcher")
(defconst sni-tray--watcher-path "/StatusNotifierWatcher")
(defconst sni-tray--item-interface "org.kde.StatusNotifierItem")
(defconst sni-tray--menu-interface "com.canonical.dbusmenu")

(cl-defstruct (sni-tray-item (:constructor sni-tray--make-item))
  service path props image registrations)

(defvar sni-tray--items nil
  "Registered items as (KEY . ITEM), where KEY is the service and path joined.")

(defvar sni-tray--registrations nil
  "Watcher-level D-Bus registration objects, for `dbus-unregister-object'.")

(defvar sni-tray--items-property nil
  "Registration object of the RegisteredStatusNotifierItems property.")

(defvar sni-tray--icon-cache (make-hash-table :test #'equal)
  "Icon files by (NAME . THEME-PATH), or :none when there is no file.")

(defvar sni-tray--icons ""
  "The tray icons, as text with images.")

(defvar sni-tray--frames nil
  "Tray child frames as (PARENT . CHILD).")

(defvar sni-tray--timer nil
  "Timer for the next redraw of the tray frames.")

(defconst sni-tray--buffer-name " *sni-tray*")

;;; Helpers

(defun sni-tray--string (value)
  "Return VALUE when it is a non-empty string."
  (and (stringp value) (not (string-empty-p value)) value))

(defun sni-tray--prop (item name)
  "ITEM's StatusNotifierItem property NAME."
  (cdr (assoc name (sni-tray-item-props item))))

(defun sni-tray--frame ()
  "A graphical frame to measure fonts and colors on.
Child frames, such as popups, can have fonts and colors of their own."
  (seq-find (lambda (frame) (and (display-graphic-p frame) (not (frame-parent frame))))
            (cons (selected-frame) (frame-list))
            (selected-frame)))

(defun sni-tray--face-rgb (face attribute)
  "FACE's ATTRIBUTE color as (R G B) in 0..255."
  (let* ((frame (sni-tray--frame))
         (color (face-attribute face attribute frame 'default))
         (values (and (stringp color) (color-values color frame))))
    (if values
        (mapcar (lambda (v) (/ v 257)) values)
      (if (eq attribute :background) '(0 0 0) '(255 255 255)))))

(defun sni-tray--face-hex (face attribute)
  "FACE's ATTRIBUTE color as #rrggbb."
  (apply #'format "#%02x%02x%02x" (sni-tray--face-rgb face attribute)))

(defun sni-tray--icon-size ()
  "Rough icon height in pixels, for picking the closest pixmap."
  (or sni-tray-icon-size (frame-char-height (sni-tray--frame))))

(defun sni-tray--image (spec type data-p)
  "Icon image from SPEC, TYPE and DATA-P, as for `create-image'."
  ;; A fixed :scale, or large fonts scale the font-sized icon up again
  (create-image spec type data-p :height (or sni-tray-icon-size '(1 . em))
                :scale 1 :ascent 'center))

;;; Watcher

(defun sni-tray--register-watcher ()
  "Own the watcher name and serve its methods and properties."
  (let ((status (dbus-register-service :session sni-tray--watcher-service :do-not-queue)))
    (unless (memq status '(:primary-owner :already-owner))
      (error "sni-tray: %s is owned by another process (%s)"
             sni-tray--watcher-service status)))
  (dolist (method '(("RegisterStatusNotifierItem" . sni-tray--handle-register-item)
                    ("RegisterStatusNotifierHost" . sni-tray--handle-register-host)))
    (push (dbus-register-method :session sni-tray--watcher-service sni-tray--watcher-path
                                sni-tray--watcher-service (car method) (cdr method) t)
          sni-tray--registrations))
  (push (dbus-register-property :session sni-tray--watcher-service sni-tray--watcher-path
                                sni-tray--watcher-service "IsStatusNotifierHostRegistered"
                                :read t nil t)
        sni-tray--registrations)
  (push (dbus-register-property :session sni-tray--watcher-service sni-tray--watcher-path
                                sni-tray--watcher-service "ProtocolVersion"
                                :read '(:int32 0) nil t)
        sni-tray--registrations)
  (sni-tray--publish-items)
  (dbus-send-signal :session sni-tray--watcher-service sni-tray--watcher-path
                    sni-tray--watcher-service "StatusNotifierHostRegistered"))

(defun sni-tray--publish-items ()
  "Export the item list as the RegisteredStatusNotifierItems property."
  (when sni-tray--items-property
    (dbus-unregister-object sni-tray--items-property))
  (setq sni-tray--items-property
        (dbus-register-property :session sni-tray--watcher-service sni-tray--watcher-path
                                sni-tray--watcher-service "RegisteredStatusNotifierItems"
                                :read
                                (if sni-tray--items
                                    `(:array :string ,@(mapcar #'car sni-tray--items))
                                  '(:array :signature "s"))
                                t t)))

(defun sni-tray--handle-register-item (arg)
  "Handle RegisterStatusNotifierItem, where ARG is a bus name or object path."
  (let* ((sender (dbus-event-service-name last-input-event))
         (path-p (string-prefix-p "/" arg)))
    (sni-tray--add-item (if path-p sender arg)
                        (if path-p arg "/StatusNotifierItem")))
  :ignore)

(defun sni-tray--handle-register-host (_service)
  "Ignore RegisterStatusNotifierHost, as Emacs is the host."
  :ignore)

;;; Items

(defun sni-tray--add-item (service path)
  "Track the item at SERVICE PATH."
  (let ((key (concat service path)))
    (unless (assoc key sni-tray--items)
      (let ((item (sni-tray--make-item :service service :path path)))
        (setf (sni-tray-item-registrations item)
              (mapcar (lambda (signal)
                        (dbus-register-signal :session service path sni-tray--item-interface
                                              signal
                                              (lambda (&rest _) (sni-tray--refresh-item item))))
                      '("NewIcon" "NewAttentionIcon" "NewOverlayIcon"
                        "NewStatus" "NewTitle" "NewToolTip")))
        (setq sni-tray--items (append sni-tray--items (list (cons key item))))
        (sni-tray--publish-items)
        (dbus-send-signal :session sni-tray--watcher-service sni-tray--watcher-path
                          sni-tray--watcher-service "StatusNotifierItemRegistered" key)
        ;; The item may still be setting up
        (run-at-time 0 nil #'sni-tray--refresh-item item)))))

(defun sni-tray--unregister (registration)
  "Unregister REGISTRATION, which the bus may have dropped already."
  (dbus-ignore-errors (dbus-unregister-object registration)))

(defun sni-tray--remove-item (key)
  "Forget the item registered as KEY."
  (when-let* ((cell (assoc key sni-tray--items)))
    (setq sni-tray--items (delq cell sni-tray--items))
    (mapc #'sni-tray--unregister (sni-tray-item-registrations (cdr cell)))
    (sni-tray--publish-items)
    (dbus-send-signal :session sni-tray--watcher-service sni-tray--watcher-path
                      sni-tray--watcher-service "StatusNotifierItemUnregistered" key)
    (sni-tray--update)))

(defun sni-tray--handle-name-owner-changed (name _old new)
  "Drop items whose bus NAME lost its owner (NEW is empty)."
  (when (equal new "")
    (dolist (cell (copy-sequence sni-tray--items))
      (when (equal (sni-tray-item-service (cdr cell)) name)
        (sni-tray--remove-item (car cell))))))

(defun sni-tray--refresh-item (item &optional attempt)
  "Re-read ITEM's properties and redraw.
ATTEMPT counts retries while the item is still starting."
  (when (rassq item sni-tray--items)
    (let ((props (dbus-ignore-errors
                   (dbus-get-all-properties :session (sni-tray-item-service item)
                                            (sni-tray-item-path item)
                                            sni-tray--item-interface))))
      (cond
       (props
        (setf (sni-tray-item-props item) props
              (sni-tray-item-image item) (condition-case err
                                             (sni-tray--item-image item)
                                           (error (message "sni-tray: icon for %s: %s"
                                                           (sni-tray--prop item "Id") err)
                                                  nil)))
        (sni-tray--update))
       ((< (or attempt 0) 5)
        (run-at-time 0.5 nil #'sni-tray--refresh-item item (1+ (or attempt 0))))))))

;;; Icons

(defun sni-tray--item-image (item)
  "Image for ITEM, from its pixmap or its icon name."
  (let* ((attention (equal (sni-tray--prop item "Status") "NeedsAttention"))
         (pixmaps (or (and attention (sni-tray--prop item "AttentionIconPixmap"))
                      (sni-tray--prop item "IconPixmap")))
         (name (or (and attention (sni-tray--string (sni-tray--prop item "AttentionIconName")))
                   (sni-tray--string (sni-tray--prop item "IconName")))))
    (or (and (consp pixmaps) (sni-tray--pixmap-image pixmaps (sni-tray--icon-size)))
        (and name (sni-tray--named-image name (sni-tray--prop item "IconThemePath"))))))

(defun sni-tray--pixmap-image (pixmaps size)
  "Image for the (WIDTH HEIGHT ARGB) entry of PIXMAPS nearest SIZE.
The ARGB data is composited onto the default background into a PPM."
  (let* ((best (seq-reduce (lambda (a b)
                             (if (< (abs (- (nth 1 b) size)) (abs (- (nth 1 a) size))) b a))
                           (cdr pixmaps) (car pixmaps)))
         (w (nth 0 best))
         (h (nth 1 best))
         (argb (vconcat (nth 2 best)))
         (bg (sni-tray--face-rgb 'default :background)))
    (when (and (> w 0) (> h 0) (>= (length argb) (* w h 4)))
      (let ((rgb (make-string (* w h 3) 0)))
        (dotimes (i (* w h))
          (let* ((o (* i 4))
                 (a (aref argb o)))
            (dotimes (c 3)
              (aset rgb (+ (* i 3) c)
                    (/ (+ (* a (aref argb (+ o 1 c)))
                          (* (- 255 a) (nth c bg)))
                       255)))))
        (sni-tray--image (concat (string-to-unibyte (format "P6\n%d %d\n255\n" w h)) rgb)
                         'pbm t)))))

(defconst sni-tray--icon-patterns
  '("%s.png" "%s.svg"
    "*/22x22/*/%s.png" "*/24x24/*/%s.png" "*/16x16/*/%s.png" "*/32x32/*/%s.png"
    "*/48x48/*/%s.png"
    "*/*/22/%s.svg" "*/*/24/%s.svg" "*/*/16/%s.svg" "*/*/32/%s.svg"
    "*/scalable/*/%s.svg" "*/symbolic/*/%s.svg" "*/*/scalable/%s.svg" "*/*/symbolic/%s.svg"
    "*/*/*/%s.png" "*/*/*/%s.svg")
  "Globs tried in each icon directory, best first, with %s for the icon name.")

(defun sni-tray--icon-dirs (theme-path)
  "Icon directories to search: THEME-PATH, then the XDG data directories."
  (seq-filter
   #'file-directory-p
   (append (and (sni-tray--string theme-path) (list theme-path))
           (list (expand-file-name "~/.icons"))
           (mapcan (lambda (data)
                     (list (expand-file-name "icons" data)
                           (expand-file-name "pixmaps" data)))
                   (cons (or (getenv "XDG_DATA_HOME") "~/.local/share")
                         (split-string (or (getenv "XDG_DATA_DIRS")
                                           "/usr/local/share:/usr/share")
                                       ":" t))))))

(defun sni-tray--icon-file (name theme-path)
  "File for icon NAME, searching THEME-PATH and the XDG icon directories.
A -symbolic variant is tried as a fallback in both directions."
  (let* ((key (cons name theme-path))
         (cached (gethash key sni-tray--icon-cache)))
    (cond
     ((eq cached :none) nil)
     (cached cached)
     (t
      (let* ((dirs (sni-tray--icon-dirs theme-path))
             (names (if (string-suffix-p "-symbolic" name)
                        (list name (string-remove-suffix "-symbolic" name))
                      (list name (concat name "-symbolic"))))
             (hit (catch 'found
                    (dolist (candidate names)
                      (dolist (pattern sni-tray--icon-patterns)
                        (dolist (dir dirs)
                          (when-let* ((file (car (file-expand-wildcards
                                                  (expand-file-name (format pattern candidate)
                                                                    dir)))))
                            (throw 'found file)))))
                    nil)))
        (puthash key (or hit :none) sni-tray--icon-cache)
        hit)))))

(defun sni-tray--svg-data (file)
  "Contents of FILE, with symbolic icons recolored to the default foreground."
  (let ((data (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
    (if (string-match-p "-symbolic\\.svg\\'" file)
        (let ((fg (sni-tray--face-hex 'default :foreground)))
          (setq data (replace-regexp-in-string
                      "#2e3436\\|#bebebe\\|#000000\\|\\bblack\\b" fg data t t))
          ;; Shapes without a fill default to black
          (if (string-match "<svg\\b" data)
              (replace-match (format "<svg fill=\"%s\"" fg) t t data)
            data))
      data)))

(defun sni-tray--named-image (name theme-path)
  "Image for icon NAME, looked up in THEME-PATH first."
  (when-let* ((file (sni-tray--icon-file name theme-path)))
    (if (string-suffix-p ".svg" file)
        (sni-tray--image (sni-tray--svg-data file) 'svg t)
      (sni-tray--image file nil nil))))

;;; Display

(defun sni-tray--item-help (item)
  "Tooltip text for ITEM."
  (let* ((tooltip (sni-tray--prop item "ToolTip"))
         (title (or (and (consp tooltip) (sni-tray--string (nth 2 tooltip)))
                    (sni-tray--string (sni-tray--prop item "Title"))
                    (sni-tray--string (sni-tray--prop item "Id"))
                    ""))
         (text (and (consp tooltip) (sni-tray--string (nth 3 tooltip)))))
    (string-trim (replace-regexp-in-string
                  "<[^>]*>" "" (concat title (if text (concat "\n" text) ""))))))

(defun sni-tray--item-keymap (item)
  "Mouse bindings for ITEM's icon."
  (define-keymap
    ;; Or the global bindings select the tray window
    "<down-mouse-1>" #'ignore
    "<down-mouse-2>" #'ignore
    "<down-mouse-3>" #'ignore
    "<mouse-1>" (lambda () (interactive) (sni-tray-activate item))
    "<mouse-2>" (lambda () (interactive) (sni-tray-secondary-activate item))
    "<mouse-3>" (lambda () (interactive) (sni-tray-context-menu item))
    "<wheel-up>" (lambda () (interactive) (sni-tray-scroll item -1))
    "<wheel-down>" (lambda () (interactive) (sni-tray-scroll item 1))))

(defun sni-tray--item-string (item)
  "Tray text for ITEM, its icon or else its title."
  (let ((image (sni-tray-item-image item)))
    (propertize (if image
                    " "
                  (format "[%s]" (or (sni-tray--string (sni-tray--prop item "Title"))
                                     (sni-tray--string (sni-tray--prop item "Id"))
                                     "?")))
                'display image
                'help-echo (sni-tray--item-help item)
                'keymap (sni-tray--item-keymap item)
                'mouse-face 'highlight)))

(defun sni-tray--buffer ()
  "The buffer shown in the tray frames."
  (or (get-buffer sni-tray--buffer-name)
      (with-current-buffer (get-buffer-create sni-tray--buffer-name)
        (setq-local mode-line-format nil
                    header-line-format nil
                    tab-line-format nil
                    cursor-type nil
                    cursor-in-non-selected-windows nil
                    truncate-lines t
                    show-trailing-whitespace nil
                    display-line-numbers nil
                    ;; Or the frame is kept several lines tall
                    window-min-height 1
                    window-min-width 1)
        (current-buffer))))

(defun sni-tray-not-floating-p (frame)
  "Non-nil unless FRAME is one of EWM's floating frames."
  (not (frame-parameter frame 'ewm-floating)))

(defun sni-tray--parent-p (frame)
  "Non-nil when FRAME should get a tray.
It has to be graphical, have an echo area of its own and satisfy
`sni-tray-frame-predicate'."
  (and (frame-live-p frame)
       (display-graphic-p frame)
       (not (frame-parent frame))
       (eq (window-frame (minibuffer-window frame)) frame)
       (funcall sni-tray-frame-predicate frame)))

(defun sni-tray--make-frame (parent)
  "Make a hidden tray frame inside PARENT."
  (let* ((after-make-frame-functions nil)
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (terminal . ,(frame-terminal parent))
                   (minibuffer . ,(minibuffer-window parent))
                   (name . "sni-tray")
                   (visibility . nil)
                   (undecorated . t)
                   (no-accept-focus . t)
                   (no-focus-on-map . t)
                   (no-other-frame . t)
                   (no-special-glyphs . t)
                   (unsplittable . t)
                   (desktop-dont-save . t)
                   (border-width . 0)
                   (internal-border-width . 0)
                   (child-frame-border-width . 0)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (right-divider-width . 0)
                   (bottom-divider-width . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (line-spacing . 0)
                   (width . 1)
                   (height . 1))))
         (window (frame-root-window frame)))
    (set-window-parameter window 'no-other-window t)
    (set-window-buffer window (sni-tray--buffer))
    (set-window-dedicated-p window t)
    ;; Keys go to the parent if the tray ever gets focus
    (redirect-frame-focus frame parent)
    frame))

(defun sni-tray--place (parent child)
  "Fit CHILD to the icons at the bottom right of PARENT's echo area."
  (pcase-let* ((`(,text-width . ,height)
                (buffer-text-pixel-size (sni-tray--buffer) (frame-root-window child)
                                        (frame-inner-width parent)))
               ;; A column to spare, or the last icon makes way for a truncation glyph
               (width (+ text-width (frame-char-width parent)))
               (`(,_ ,_ ,right ,bottom) (window-inside-pixel-edges (minibuffer-window parent)))
               (geometry `((width . (text-pixels . ,width))
                           (height . (text-pixels . ,height))
                           (left . ,(- right width))
                           (top . ,(- bottom height)))))
    (unless (equal geometry (frame-parameter child 'sni-tray-geometry))
      (let ((frame-resize-pixelwise t))
        (modify-frame-parameters child `((sni-tray-geometry . ,geometry) ,@geometry))))
    (unless (frame-visible-p child)
      (make-frame-visible child))))

(defun sni-tray--redraw ()
  "Put the icons on every frame with an echo area, or remove them if none."
  (setq sni-tray--timer nil)
  (let ((parents (and (not (string-empty-p sni-tray--icons))
                      (seq-filter #'sni-tray--parent-p (frame-list)))))
    (pcase-dolist (`(,parent . ,child) sni-tray--frames)
      (when (and (frame-live-p child) (not (memq parent parents)))
        (delete-frame child)))
    (setq sni-tray--frames
          (delq nil (mapcar (lambda (parent)
                              (with-demoted-errors "sni-tray: %S"
                                (let ((child (cdr (assq parent sni-tray--frames))))
                                  (unless (frame-live-p child)
                                    (setq child (sni-tray--make-frame parent)))
                                  (sni-tray--place parent child)
                                  (cons parent child))))
                            parents)))))

(defun sni-tray--schedule ()
  "Redraw the tray frames once the current burst of changes is over."
  (unless sni-tray--timer
    (setq sni-tray--timer (run-with-timer 0.05 nil #'sni-tray--redraw))))

(defun sni-tray--on-frame-change (frame)
  "Move the tray when FRAME changes size, which new frames do too."
  (when (sni-tray--parent-p frame)
    (sni-tray--schedule)))

(defun sni-tray--update ()
  "Rebuild the tray from the current items."
  (let ((icons (string-join (cl-loop for (_ . item) in sni-tray--items
                                     when (and (sni-tray-item-props item)
                                               (not (equal (sni-tray--prop item "Status")
                                                           "Passive")))
                                     collect (sni-tray--item-string item))
                            sni-tray-separator)))
    (unless (equal-including-properties icons sni-tray--icons)
      (setq sni-tray--icons icons)
      (with-current-buffer (sni-tray--buffer)
        (erase-buffer)
        (insert icons)
        ;; Tray windows start at point, and would scroll right to show it
        (goto-char (point-min)))
      (sni-tray--schedule))))

;;; Interaction

(defun sni-tray--call (item method &rest args)
  "Call METHOD on ITEM with ARGS, not waiting for a reply."
  (apply #'dbus-call-method-asynchronously :session (sni-tray-item-service item)
         (sni-tray-item-path item) sni-tray--item-interface method nil args))

(defun sni-tray-activate (item)
  "Activate ITEM, or show its menu if that is all it has.
libappindicator items have no Activate method."
  (when (or (eq (sni-tray--prop item "ItemIsMenu") t)
            (condition-case nil
                (progn
                  (dbus-call-method :session (sni-tray-item-service item)
                                    (sni-tray-item-path item) sni-tray--item-interface
                                    "Activate" :timeout 1000 :int32 0 :int32 0)
                  nil)
              (dbus-error t)))
    (sni-tray-context-menu item)))

(defun sni-tray-secondary-activate (item)
  "Secondary (middle click) activation of ITEM."
  (sni-tray--call item "SecondaryActivate" :int32 0 :int32 0))

(defun sni-tray-scroll (item delta)
  "Scroll ITEM vertically by DELTA."
  (sni-tray--call item "Scroll" :int32 delta "vertical"))

(defun sni-tray-popup-menu (menu)
  "Pop up MENU where the tray was last clicked.
The menu belongs to the frame under the tray, as some window systems
can't open menus from a child frame."
  (let* ((posn (event-start last-nonmenu-event))
         (window (posn-window posn))
         (frame (if (windowp window) (window-frame window) window))
         (parent (and (framep frame) (frame-parent frame)))
         (xy (posn-x-y posn)))
    (popup-menu menu (if parent
                         (list (list (+ (frame-parameter frame 'left) (car xy))
                                     (+ (frame-parameter frame 'top) (cdr xy)))
                               parent)
                       posn))))

(defun sni-tray-context-menu (item)
  "Show ITEM's dbusmenu as an Emacs menu, or ask the item to show its own."
  (let ((menu-path (sni-tray--string (sni-tray--prop item "Menu"))))
    (if (and menu-path (not (equal menu-path "/")))
        (if-let* ((map (sni-tray--dbusmenu-keymap item menu-path)))
            (funcall sni-tray-menu-function map)
          (message "sni-tray: %s offers no menu" (sni-tray--prop item "Id")))
      (sni-tray--call item "ContextMenu" :int32 0 :int32 0))))

;;; dbusmenu

(defun sni-tray--unwrap-node (node)
  "Strip the variant wrapper around a dbusmenu layout NODE, if any."
  (if (and (consp node) (null (cdr node)) (consp (car node)) (integerp (caar node)))
      (car node)
    node))

(defun sni-tray--node-prop (props name default)
  "Value of NAME in dbusmenu PROPS with the variant unwrapped, or DEFAULT."
  (if-let* ((entry (assoc name props)))
      (let ((value (if (consp (cdr entry)) (cadr entry) (cdr entry))))
        (if (and (consp value) (null (cdr value)) (not (consp (car value))))
            (car value)
          value))
    default))

(defun sni-tray--menu-label (label)
  "LABEL without GTK mnemonic underscores."
  (replace-regexp-in-string "_\\(.\\)" "\\1" label))

(defun sni-tray--dbusmenu-keymap (item menu-path)
  "Keymap for the dbusmenu of ITEM exported at MENU-PATH, or nil."
  (let ((service (sni-tray-item-service item)))
    (dbus-ignore-errors
      (dbus-call-method :session service menu-path sni-tray--menu-interface
                        "AboutToShow" :int32 0))
    (when-let* ((layout (dbus-ignore-errors
                          (dbus-call-method :session service menu-path sni-tray--menu-interface
                                            "GetLayout" :int32 0 :int32 -1
                                            '(:array :signature "s"))))
                (root (sni-tray--unwrap-node (nth 1 layout))))
      (sni-tray--dbusmenu-node-keymap
       item menu-path root
       (or (sni-tray--string (sni-tray--prop item "Title")) "Menu")))))

(defun sni-tray--dbusmenu-node-keymap (item menu-path node title)
  "Keymap titled TITLE for the children of dbusmenu NODE of ITEM at MENU-PATH."
  (let ((map (make-sparse-keymap title))
        (n 0))
    (dolist (raw (nth 2 node))
      (let* ((child (sni-tray--unwrap-node raw))
             (id (nth 0 child))
             (props (nth 1 child))
             (key (vector (intern (format "sni-item-%d" (cl-incf n)))))
             (label (sni-tray--menu-label
                     (or (sni-tray--string (sni-tray--node-prop props "label" "")) "")))
             (enabled (sni-tray--node-prop props "enabled" t))
             (toggle-type (sni-tray--node-prop props "toggle-type" ""))
             (toggle-state (sni-tray--node-prop props "toggle-state" -1)))
        (when (sni-tray--node-prop props "visible" t)
          (define-key-after map key
            (cond
             ((equal (sni-tray--node-prop props "type" "") "separator")
              '(menu-item "--"))
             ((equal (sni-tray--node-prop props "children-display" "") "submenu")
              (list 'menu-item label
                    (sni-tray--dbusmenu-node-keymap item menu-path child label)
                    :enable enabled))
             (t
              (append
               (list 'menu-item label
                     (lambda () (interactive) (sni-tray--menu-event item menu-path id))
                     :enable enabled)
               (pcase toggle-type
                 ("checkmark" (list :button (cons :toggle (eql toggle-state 1))))
                 ("radio" (list :button (cons :radio (eql toggle-state 1))))))))))))
    map))

(defun sni-tray--menu-event (item menu-path id)
  "Tell ITEM's menu at MENU-PATH that entry ID was clicked."
  (dbus-call-method-asynchronously :session (sni-tray-item-service item) menu-path
                                   sni-tray--menu-interface "Event" nil
                                   :int32 id "clicked" '(:variant :int32 0)
                                   :uint32 (truncate (float-time))))

;;; Mode

(defun sni-tray--on-theme-change (&rest _)
  "Redraw icons for the new theme colors."
  (sni-tray-refresh))

(defun sni-tray-refresh ()
  "Re-read every item and redraw the icons."
  (interactive)
  (clrhash sni-tray--icon-cache)
  (dolist (cell sni-tray--items)
    (sni-tray--refresh-item (cdr cell))))

(defun sni-tray-list ()
  "Describe the registered tray items."
  (interactive)
  (message "%s"
           (if sni-tray--items
               (mapconcat (lambda (cell)
                            (let ((item (cdr cell)))
                              (format "%s (%s): status=%s icon=%s pixmap=%s"
                                      (sni-tray--prop item "Id") (car cell)
                                      (sni-tray--prop item "Status")
                                      (sni-tray--prop item "IconName")
                                      (and (consp (sni-tray--prop item "IconPixmap")) t))))
                          sni-tray--items "\n")
             "sni-tray: no items")))

(defun sni-tray--start ()
  "Become the watcher and host, and show the tray in the echo area."
  (unless (featurep 'dbusbind)
    (error "sni-tray: this Emacs has no D-Bus support"))
  (sni-tray--register-watcher)
  (push (dbus-register-signal :session "org.freedesktop.DBus" "/org/freedesktop/DBus"
                              "org.freedesktop.DBus" "NameOwnerChanged"
                              #'sni-tray--handle-name-owner-changed)
        sni-tray--registrations)
  (add-hook 'enable-theme-functions #'sni-tray--on-theme-change)
  (add-hook 'window-size-change-functions #'sni-tray--on-frame-change)
  (sni-tray--update))

(defun sni-tray--stop ()
  "Release the watcher name and remove the tray from the echo area."
  (remove-hook 'enable-theme-functions #'sni-tray--on-theme-change)
  (remove-hook 'window-size-change-functions #'sni-tray--on-frame-change)
  (when sni-tray--timer
    (cancel-timer sni-tray--timer))
  (setq sni-tray--icons "")
  (sni-tray--redraw)
  (when-let* ((buffer (get-buffer sni-tray--buffer-name)))
    (kill-buffer buffer))
  (dolist (cell sni-tray--items)
    (mapc #'sni-tray--unregister (sni-tray-item-registrations (cdr cell))))
  (setq sni-tray--items nil)
  (mapc #'sni-tray--unregister sni-tray--registrations)
  (setq sni-tray--registrations nil)
  (when sni-tray--items-property
    (sni-tray--unregister sni-tray--items-property)
    (setq sni-tray--items-property nil))
  (dbus-ignore-errors (dbus-unregister-service :session sni-tray--watcher-service)))

;;;###autoload
(define-minor-mode sni-tray-mode
  "Show tray icons at the right end of the echo area.
Emacs becomes the session's StatusNotifierWatcher and host.  Left click
activates an item, middle click is its secondary action, right click
shows its menu and the wheel scrolls it."
  :global t
  :group 'sni-tray
  (if sni-tray-mode
      (condition-case err
          (sni-tray--start)
        (error
         (sni-tray--stop)
         (setq sni-tray-mode nil)
         (signal (car err) (cdr err))))
    (sni-tray--stop)))

(provide 'sni-tray)
;;; sni-tray.el ends here
