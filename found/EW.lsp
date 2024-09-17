(defun c:EW (/ draw-ew lay p1 p2 ang)

  (defun draw-ew (p4 p1 lay hgh / p2 p3)
    (setq p2 (polar p1 (- (angle p1 p4) (/ (* ang pi) 180)) hgh)
          p3 (polar p4 (+ (angle p4 p1) (/ (* ang pi) 180)) hgh))

    (entmakex (list '(0 . "LWPOLYLINE")
        '(100 . "AcDbEntity")
        '(100 . "AcDbPolyline")
        (cons 8 lay)
        (cons 90 4)
        '(70 . 0)            ; 1 for closed 0 overwise
        (cons 10 (trans p1 1 0))
        '(40 . 0.0)
        '(41 . 0.0)
        '(42 . 0.0)
        (cons 10 (trans p2 1 0))
        '(40 . 0.0)
        '(41 . 0.0)
        '(42 . 0.0)
        (cons 10 (trans p3 1 0))
        '(40 . 0.0)
        '(41 . 0.0)
        '(42 . 0.0)
        (cons 10 (trans p4 1 0))
        '(40 . 0.0)
        '(41 . 0.0)
        '(42 . 0.0)
        '(210 0.0 0.0 1.0))))

  ; --------------------------------------------------------------------------------------------------------------------------

  (setq lay (getvar "CLAYER"))
  (setq ang 45)

  (or (tblsearch "LAYER" lay)
      (command "_.-LAYER" "_N" lay "_C" 160 lay ""))
  
  (cond ((or (not *ew-height*)
             (not (setq p1 (getpoint (strcat "\nPick start point <change r=" (rtos (getvar 'filletrad)) ", h=" (rtos *ew-height*) "> ")))))
         (setvar 'filletrad (cond ((getdist (strcat "\nWire radius <" (rtos (getvar 'filletrad)) ">: ")))
                                  ((getvar 'filletrad))))
         (or *ew-height*
             (setq *ew-height* (getvar 'filletrad)))
         (setq *ew-height* (cond ((getdist (strcat "\nChamfer height <" (rtos *ew-height*) ">: ")))
                                 (*ew-height*)))
         (setq p1 (getpoint "\nPick start point: "))))

  (while (setq p2 (getpoint p1 "\nPick new point: "))
    (command "_.fillet" "_P" (draw-ew p1 (setq p1 p2) lay *ew-height*)))
  
  (princ)
  )