(defun c:InsertBlockWithAcCountField ( / blkName insPt blkEnt blkObj mTextObj fieldExpr)
  ;; Prompt user to select a block
  (setq blkEnt (car (entsel "\nSelect a block: ")))

  ;; Ensure a block was selected and that it is a valid block reference
  (if (and blkEnt (eq (cdr (assoc 0 (entget blkEnt))) "INSERT"))
    (progn
      ;; Get the block name
      (setq blkName (cdr (assoc 2 (entget blkEnt))))
      
      ;; Prompt user for MText insertion point
      (setq insPt (getpoint "\nSpecify insertion point for the field: "))
      
      ;; Construct the custom field expression with the resolved block name
      (setq fieldExpr (strcat "%<\\AcCount {\"key\":{},\"name\":\"" blkName "\",\"type\":\"block\"}>%"))
      
      ;; Create MText using ActiveX to prevent unwanted characters
      (setq mTextObj (vla-addMText
                       (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                       (vlax-3D-point (list (+ (car insPt) 2.0) (cadr insPt) 0.0)) ; Position
                       4.0                                              ; Width
                       fieldExpr))                                      ; Text with field

      ;; Set text height for the MText
      (vla-put-Height mTextObj 1.0) ; Set the text height
      
      ;; Success message
      (princ "\nField referencing the selected block inserted successfully.")
    )
    (princ "\nPlease select a valid block.")
  )
  (princ)
)
