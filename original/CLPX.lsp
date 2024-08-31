(defun c:CLPX (/ prefix acadDoc layers layerName newLayerName layer newLayer selectedObjects layerList)
  ;; Load the Visual LISP ActiveX support
  (vl-load-com)
  
  ;; Get the prefix for new layers
  (setq prefix (getstring "\nEnter the prefix for new layers: "))
  
  ;; Get the active document and layers collection
  (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers acadDoc))
  
  ;; Prompt the user to select objects
  (setq selectedObjects (ssget))
  
  ;; Check if objects were selected
  (if selectedObjects
    (progn
      ;; Collect unique layer names from selected objects
      (setq layerList '())
      (setq index 0)
      (while (< index (sslength selectedObjects))
        (setq obj (vlax-ename->vla-object (ssname selectedObjects index)))
        (setq layerName (vla-get-Layer obj))
        
        ;; Add layer name to list if not already present
        (if (not (member layerName layerList))
          (setq layerList (cons layerName layerList))
        )
        
        ;; Increment index
        (setq index (1+ index))
      )

      ;; Loop through the unique layers and create copies with the prefix
      (foreach layerName layerList
        (setq newLayerName (strcat prefix layerName))
        
        ;; Check if the layer with the new name already exists
        (if (not (tblsearch "LAYER" newLayerName))
          (progn
            ;; Create a new layer with the copied properties
            (setq layer (vla-item layers layerName))
            (setq newLayer (vla-add layers newLayerName))
            (vla-put-Color newLayer (vla-get-Color layer))
            (vla-put-Linetype newLayer (vla-get-Linetype layer))
            (princ (strcat "\nLayer created: " newLayerName))
          )
        )
      )
      (princ "\nSelected layer copying complete.")
    )
    (princ "\nNo objects selected.")
  )
  (princ)
)
