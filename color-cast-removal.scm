; Color-Cast Removal
;
; This script will automatically color-correct a the active layer,
; based on the average color of the active selection,
; or based on the whole layer if there is no selection.
;
; Note:
;
; The active layer will not be modified.  Instead a new color-correction
; layer will be created.  This layer will be set to soft-light mode.
; After the script finishes you may like to try out a different mode,
; like hard-light or overlay.
;
; ===========================================================================
;
; LICENSE
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

(define (script-fu-color-cast-removal given-image given-layer)
  (gimp-image-undo-group-start given-image)
  (let* ((selection-bounds (gimp-selection-bounds given-image))
         ; Saving coordinates of selection or image (if no selection)
         (selection-non-empty (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-upper-left-y (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-x (head selection-bounds))
         (selection-bounds (tail selection-bounds))
         (selection-lower-right-y (head selection-bounds))
         ; Create a new layer from the selection (or from whole image, if there's no selection)
         (ignored (gimp-edit-copy given-layer))
         (floating-selection (car (gimp-edit-paste given-layer FALSE)))
         (ignored (gimp-floating-sel-to-layer floating-selection))
         (correction-layer (car (gimp-image-active-drawable given-image)))
         (ignored (gimp-item-set-name correction-layer "Color correction"))
         ; Find the average color of the new layer
         (sample-merged FALSE) ; FALSE = Only sample active layer
         (sample-average TRUE) ; TRUE  = Average within radius
         (radius 10000)        ; How much to average
         (average-selection-color (car (gimp-image-pick-color
                                        given-image
                                        correction-layer
                                        selection-upper-left-x
                                        selection-upper-left-y
                                        sample-merged
                                        sample-average radius))))
    (gimp-image-remove-layer given-image correction-layer)
    (gimp-selection-none given-image)
    ; Saving coordinates of image
    (let* ((selection-bounds (gimp-selection-bounds given-image))
           (selection-non-empty (head selection-bounds))
           (selection-bounds (tail selection-bounds))
           (selection-upper-left-x (head selection-bounds))
           (selection-bounds (tail selection-bounds))
           (selection-upper-left-y (head selection-bounds))
           (selection-bounds (tail selection-bounds))
           (selection-lower-right-x (head selection-bounds))
           (selection-bounds (tail selection-bounds))
           (selection-lower-right-y (head selection-bounds))
           (correction-layer-opacity 100)
           ; Create a new correction layer
           (correction-layer (car (gimp-layer-new
                                   given-image
                                   selection-lower-right-x
                                   selection-lower-right-y
                                   RGB-IMAGE
                                   "Color Correction"
                                   correction-layer-opacity
                                   SOFTLIGHT-MODE)))
           ; Correction layer parameters (used for layer insertion below)
           (correction-layer-parent    0) ;  0 = Outside any group
           (correction-layer-position -1) ; -1 = Above active layer
           ; Bucket fill parameters
           (bucket-fill-opacity 100)
           (bucket-fill-threshold 255)
           (bucket-fill-sample-merged FALSE)
           (bucket-fill-x 0)
           (bucket-fill-y 0))
      ; Make the new correction layer visible
      (gimp-image-insert-layer
       given-image
       correction-layer
       correction-layer-parent
       correction-layer-position)
      ; Bucket fill can only use fg/bg colors, so we set fg color here:
      (gimp-context-set-foreground average-selection-color)
      (gimp-bucket-fill
       correction-layer
       BUCKET-FILL-FG
       LAYER-MODE-NORMAL
       bucket-fill-opacity
       bucket-fill-threshold
       bucket-fill-sample-merged
       bucket-fill-x
       bucket-fill-y)
      ; Color correction starts with inverted color
      (gimp-drawable-invert correction-layer TRUE)
      ; Find the inverted color and set the foreground color in the active pallete to it
      (let ((sample-merged FALSE) ; FALSE = Only sample active layer
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
        (gimp-context-set-foreground inverted-color))
    (gimp-image-undo-group-end given-image)
    (gimp-displays-flush))))

(script-fu-register "script-fu-color-cast-removal"
                    "Color-Cast Removal"
                    "Remove a color cast from the image"
                    "Sergey Goldgaber"
                    "Copyright 2020, Sergey Goldgaber"
                    "Aug 31, 2020"
                    ""
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Layer" 0)

(script-fu-menu-register "script-fu-color-cast-removal" "<Image>/Filters/Enhance")
