; Color-Cast Removal
;
; This script will automatically color-correct the active layer,
; based on the average color of the active selection,
; or based on the whole layer if there is no selection.
;
; Note:
;
; The active layer will not be modified.  Instead a new color-correction
; layer will be created, which will be set to your chosen mode.
; After the script finishes you may like to try out a different layer mode.
;
; ===========================================================================
;
; LICENSE
;
; Copyright (C) 2020 - Sergey Goldgaber
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; ===========================================================================


(define (bucket-fill-correction-layer correction-layer)
  ; Bucket fill parameters
  (let* ((bucket-fill-opacity 100)
         (bucket-fill-threshold 255)
         (bucket-fill-sample-merged FALSE)
         (bucket-fill-x 0)
         (bucket-fill-y 0))
    (gimp-bucket-fill
     correction-layer
     BUCKET-FILL-FG
     LAYER-MODE-NORMAL
     bucket-fill-opacity
     bucket-fill-threshold
     bucket-fill-sample-merged
     bucket-fill-x
     bucket-fill-y)))


(define (create-correction-layer given-image
                                 correction-layer-mode
                                 average-selection-color
                                 given-opacity)
  (let* ((selection-lower-right-bounds (get-lower-right-bounds given-image))
         (selection-lower-right-x (car selection-lower-right-bounds))
         (selection-lower-right-y (cadr selection-lower-right-bounds))
         ; Create a new correction layer
         (correction-layer-mode
          (determine-correction-layer-mode correction-layer-mode))
         (correction-layer (car (gimp-layer-new
                                 given-image
                                 selection-lower-right-x
                                 selection-lower-right-y
                                 RGB-IMAGE
                                 "Color Correction"
                                 given-opacity
                                 correction-layer-mode)))
         ; Correction layer parameters (used for layer insertion below)
         (correction-layer-parent    0)  ;  0 = Outside any group
         (correction-layer-position -1)) ; -1 = Above active layer
    ; Make the new correction layer visible
    (gimp-image-insert-layer
     given-image
     correction-layer
     correction-layer-parent
     correction-layer-position)
    ; Bucket fill can only use fg/bg colors, so we set fg color here:
    (gimp-context-set-foreground average-selection-color)
    (bucket-fill-correction-layer correction-layer)
    ; Color correction starts with inverted color
    (gimp-drawable-invert correction-layer TRUE)
    correction-layer))


; GIMP passes us only an integer corresponding to the correction-layer-mode
; chosen by the user.
;
; This is where we convert that interger in to a meaningful variable name.
(define (determine-correction-layer-mode correction-layer-mode)
  (cond
   ((equal? correction-layer-mode 0)
    LAYER-MODE-SOFTLIGHT)
   ((equal? correction-layer-mode 1)
    LAYER-MODE-HARDLIGHT)
   ((equal? correction-layer-mode 2)
    LAYER-MODE-OVERLAY)))


; Return the lower-right-x and lower-right-y of the given selection
; or of the image, if nothing is selected
(define (get-lower-right-bounds image)
  (let* ((selection-bounds (gimp-selection-bounds image))
         (selection-non-empty (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-y (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-y (head selection-bounds)))
    (list selection-lower-right-x selection-lower-right-y)))


; Returns the average color of the active selection
; (or of the active layer, if there is no selection)
(define (get-average-selection-color given-image given-layer given-radius)
  (let* ((selection-upper-left-bounds (get-upper-left-bounds given-image))
         (selection-upper-left-x (car selection-upper-left-bounds))
         (selection-upper-left-y (cadr selection-upper-left-bounds))
         ; Create a new layer from the selection (or from whole image, if there's no selection)
         (active-drawable (car (gimp-image-get-active-drawable given-image)))
         (sample-copy-buffer (car (gimp-edit-named-copy active-drawable "color-cast-removal sample")))
         (floating-selection (car (gimp-edit-named-paste active-drawable sample-copy-buffer FALSE)))
         (ignored (gimp-floating-sel-to-layer floating-selection))
         (sample-layer (car (gimp-image-active-drawable given-image)))
         (ignored (gimp-item-set-name sample-layer "Color sample"))
         ; Find the average color of the new layer
         (sample-merged FALSE) ; FALSE = Only sample active layer
         (sample-average TRUE) ; TRUE  = Average within radius
         (radius given-radius) ; How much to average
         (average-selection-color (car (gimp-image-pick-color
                                        given-image
                                        sample-layer
                                        selection-upper-left-x
                                        selection-upper-left-y
                                        sample-merged
                                        sample-average radius))))
    (gimp-image-remove-layer given-image sample-layer)
    average-selection-color))


; Return the upper-left-x and upper-left-y of the given selection
; or of the image, if nothing is selected
(define (get-upper-left-bounds image)
  (let* ((selection-bounds (gimp-selection-bounds image))
         (selection-non-empty (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-y (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-y (head selection-bounds)))
    (list selection-upper-left-x selection-upper-left-y)))


(define (script-fu-color-cast-removal given-image given-layer correction-layer-mode given-opacity given-radius keep-selection)
  (gimp-image-undo-group-start given-image)
  (let* ((ignored (plug-in-sel2path RUN-NONINTERACTIVE given-image given-layer))
         (saved-path (vector-ref (cadr (gimp-image-get-vectors given-image)) 0))
         (average-selection-color (get-average-selection-color given-image given-layer given-radius)))
    (gimp-selection-none given-image)
    ; Saving coordinates of image
    (let* ((correction-layer (create-correction-layer
                              given-image
                              correction-layer-mode
                              average-selection-color
                              given-opacity)))
      (set-fg-to-inverted-color given-image correction-layer))
    (gimp-image-undo-group-end given-image)
    (if (equal? keep-selection TRUE)
        (gimp-image-select-item given-image CHANNEL-OP-REPLACE saved-path))
    (gimp-displays-flush)))


(define (set-fg-to-inverted-color given-image correction-layer)
  ; Find the inverted color and set the foreground color in the active pallete to it
  (let* ((selection-upper-left-bounds (get-upper-left-bounds given-image))
         (selection-upper-left-x (car selection-upper-left-bounds))
         (selection-upper-left-y (cadr selection-upper-left-bounds))
         (sample-merged FALSE) ; FALSE = Only sample active layer
         (sample-average TRUE) ; FALSE = Average within radius
         (radius 1)            ; How much to average
         (inverted-color
          (car (gimp-image-pick-color
                given-image
                correction-layer
                selection-upper-left-x
                selection-upper-left-y
                sample-merged
                sample-average
                radius))))
    (gimp-context-set-foreground inverted-color)))


(script-fu-register "script-fu-color-cast-removal"
                    "Color-Cast Removal..."
                    "Remove a color cast from the image"
                    "Sergey Goldgaber"
                    "Copyright 2020, Sergey Goldgaber"
                    "Aug 31, 2020"
                    ""
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Layer" 0
                    SF-OPTION "Correction layer mode" '("Soft-Light" "Hard-Light" "Overlay")
                    SF-ADJUSTMENT "Correction layer opacity (reduce to lessen effect)" '(100 1 100 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Radius to sample for averaging" '(10000000 1 10000000 1 1000 0 SF-SPINNER)
                    SF-TOGGLE "Keep selection?" FALSE)


(script-fu-menu-register "script-fu-color-cast-removal" "<Image>/Filters/Enhance")
