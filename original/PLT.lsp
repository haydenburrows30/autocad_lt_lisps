(defun c:PLT ()
  ;; Get the active document
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layermanager (vla-get-Layers doc))
  (setq modelspace (vla-get-ModelSpace doc))
  (setq selectedLayers '())
  (setq selectedObjects nil)

  ;; Function to get the length of all polylines and lines on a given layer
  (defun GetPolylineAndLineLengthOnLayer (layername selectedOnly)
    (setq total-length 0.0)
    (if selectedOnly
      (progn
        (setq i 0)
        (while (< i (sslength selectedObjects))
          (setq entity (vlax-ename->vla-object (ssname selectedObjects i)))
          (if (and (eq (vla-get-Layer entity) layername)
                   (or (eq (vla-get-ObjectName entity) "AcDbPolyline")
                       (eq (vla-get-ObjectName entity) "AcDb2dPolyline")
                       (eq (vla-get-ObjectName entity) "AcDb3dPolyline")
                       (eq (vla-get-ObjectName entity) "AcDbLine")))
            (setq total-length (+ total-length (vla-get-Length entity))))
          (setq i (1+ i))))
      (vlax-for entity modelspace
        (if (and (eq (vla-get-Layer entity) layername)
                 (or (eq (vla-get-ObjectName entity) "AcDbPolyline")
                     (eq (vla-get-ObjectName entity) "AcDb2dPolyline")
                     (eq (vla-get-ObjectName entity) "AcDb3dPolyline")
                     (eq (vla-get-ObjectName entity) "AcDbLine")))
          (setq total-length (+ total-length (vla-get-Length entity))))))
    total-length)

  ;; Function to collect unique layer names from selected objects
  (defun GetLayersFromSelection (ss)
    (setq layers '())
    (setq i 0)
    (while (< i (sslength ss))
      (setq entity (vlax-ename->vla-object (ssname ss i)))
      (setq entityLayer (vla-get-Layer entity))
      (if (not (member entityLayer layers))
        (setq layers (cons entityLayer layers)))
      (setq i (1+ i)))
    (acad_strlsort layers))  ; Sort the layer list

  ;; Prompt user for layer selection method
  (initget "Select All")
  (setq userChoice (getkword "\nDo you want to [Select] layers or show [All] layers? "))

  (cond
    ((equal userChoice "Select")
      (setq selectedObjects (ssget))
      (if selectedObjects
        (setq selectedLayers (GetLayersFromSelection selectedObjects))
        (progn
          (princ "\nNo objects selected.")
          (exit))))
    ((equal userChoice "All")
      (vlax-for layer layermanager
        (setq layerName (vla-get-Name layer))
        (setq selectedLayers (cons layerName selectedLayers)))
      (setq selectedLayers (acad_strlsort selectedLayers)))
    (t
      (princ "\nInvalid choice.")
      (exit)))

  ;; Prompt user to measure only selected objects or all objects on the selected layers
  (if selectedObjects
    (progn
      (initget "Selected All")
      (setq measureChoice (getkword "\nDo you want to measure [Selected] polylines and lines or [All] objects on the layers? "))
      (if (not (or (equal measureChoice "Selected") (equal measureChoice "All")))
        (progn
          (princ "\nInvalid choice.")
          (exit)))
      (setq measureSelectedOnly (equal measureChoice "Selected")))
    (setq measureSelectedOnly nil))

  ;; Prompt for table insertion point
  (setq insertpoint (getpoint "\nSpecify table insertion point: "))
  (if (not insertpoint)
    (progn
      (princ "\nNo insertion point specified.")
      (exit)))

  ;; Create table object
  (setq table (vla-AddTable (vla-get-ModelSpace doc) 
                            (vlax-3d-point (list (car insertpoint) (cadr insertpoint) 0))
                            (+ 2 (length selectedLayers))  ; Rows (1 for header)
                            3  ; Columns
                            3  ; Row height
                            10))  ; Default column width

  ;; Set column headers
  (vla-SetText table 0 0 "Layer PolyLine/Line Length Table")
  (vla-SetText table 1 0 "Layer Name")
  (vla-SetText table 1 1 "Polyline/Line Length (m)")
  (vla-SetText table 1 2 "Layer Line")

  ;; Set column widths
  (vla-SetColumnWidth table 0 60)
  (vla-SetColumnWidth table 1 15)
  (vla-SetColumnWidth table 2 30)

  ;; Fill table with layer data and add lines
  ;; centre the text in the cells
  (setq row 2)
  (foreach layer selectedLayers
    (setq polylineLength (rtos (GetPolylineAndLineLengthOnLayer layer measureSelectedOnly) 2 2))
    (vla-SetText table row 0 layer)
    (vla-SetText table row 1 polylineLength)
    (vla-SetCellAlignment table row 0 acMiddleCenter)
    (vla-SetCellAlignment table row 1 acMiddleCenter)

    ;; Draw line near each row and set its layer
    (setq rowHeight 3) ; Same as the row height of the table
    (setq lineYCoord (- (cadr insertpoint) (* row rowHeight) (+ (/ rowHeight 1.8))))
    (setq lineStartPoint (vlax-3d-point (list (+ (car insertpoint) 80) lineYCoord 0)))
    (setq lineEndPoint (vlax-3d-point (list (+ 100 (car insertpoint)) lineYCoord 0)))
    (setq lineObj (vla-AddLine modelspace lineStartPoint lineEndPoint))
    (vla-put-Layer lineObj layer)

    (setq row (1+ row)))

  (princ "\nTable created.")
  (princ))

(princ "\nType PLT to run the script.")
(princ)
