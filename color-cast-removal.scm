; Color-Cast Removal
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
         (correction-layer (car (gimp-image-active-drawable 1)))
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
                                        sample-average radius)))
         (new-width 100)
         (new-height 100)
         (local-origin FALSE)
         (ignored (gimp-layer-scale
                   correction-layer
                   new-width
                   new-height
                   local-origin)))
    (gimp-image-undo-group-end given-image)
    (gimp-displays-flush)))

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
