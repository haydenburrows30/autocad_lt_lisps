;;;--------------------------------------------------------------------
(defun Length1(e) (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))

(defun C:LTX ()
  (setq totalLength 0.0)
  (setq   ss  (ssget (list 
                        (cons 0 "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE")
                    )
                )
    )
  (if ss
    (progn
      (vl-load-com)
      (while (setq e (ssname ss 0))
          (setq totalLength (+ totalLength (length1 e)))
          (ssdel e ss)
      )

      ;; Retrieve the units setting
      (setq units (atof (GET_DWGUNITS))) ;; call function and convert string to integer
      (setq unitsStr (cond
                       ((= units 1) "Inches")
                       ((= units 2) "Feet")
                       ((= units 3) "mm")
                       ((= units 4) "cm")
                       ((= units 5) "Decimeters")
                       ((= units 6) "m")
                       (t "Units"))) ;; Default if unit type is unknown

      ;; Create the total length string including units
      (setq totalLengthStr (strcat "Total Length: " (rtos totalLength 2 2) unitsStr))
      (setq totalLengthm (strcat (rtos totalLength 2 2) "m")) ;;unitsStr
      (princ (strcat "\n" totalLengthStr))
      
      ;; Get insertion point for the text
      (setq insPt (getpoint "\nSpecify insertion point for total length text: "))
      (if (not insPt) (setq insPt '(0 0 0))) ;; Default to (0,0,0) if not specified
      
      ;; Create the text object
      (entmake
        (list
          (cons 0 "TEXT")
          (cons 10 insPt)
          (cons 40 1.0) ;; Text height standard, you can change this value
          (cons 1 totalLengthm)
          (cons 7 "Standard") ;; Text style name
          (cons 72 1) ;; Text alignment
          (cons 11 insPt)
        )
      )
    )
    (princ "\nNo polylines selected.")
  )
  (princ)
)

;;https://autolisps.blogspot.com/p/getdwgunits.html


;;; =================================================
;;;        GET_DWGUNITS_v1_04.LSP                                                 
;;;
;;;        Written by Andrzej Kalinowski,     www.autolisps.blogspot.com
;;;        v1.00 - 23.03.2019
;;;        v1.04 - 30.03.2019
;;;
;;;        Function gets drawing units value. 
;;;        Program checks if this variable exists - in this way the main routine will not be  limmited to work only in Autocad, where this variable is native
;;; =================================================
(defun GET_DWGUNITS ( /  old_cmdecho dwgunts )
;;;    1. Inches
;;;    2. Feet
;;;    3. Millimeters
;;;    4. Centimeters
;;;    5. Decimetersw
;;;    6. Meters
    ;-----------------------------------------
    (setq old_cmdecho (getvar "cmdecho") );setq
    (setvar "cmdecho" 1)
    (command "_-dwgunits" );command
    (command);breaking command
    (graphscr)
    (setq dwgunts (getvar "lastprompt") )
    (setvar "cmdecho" old_cmdecho)

    (setq dwgunts (substr dwgunts (+ (vl-string-search "<" dwgunts) 2) (- (vl-string-search ">" dwgunts) (vl-string-search "<" dwgunts) 1) ) )
;|
    (setq
            dwgunts (substr dwgunts (+ 2 (vl-string-position (ascii "<") dwgunts) ) (strlen dwgunts) )
            dwgunts (vl-list->string (reverse (vl-string->list dwgunts)))
            dwgunts (substr dwgunts (+ 2 (vl-string-position (ascii ">") dwgunts) ) (strlen dwgunts) )
            dwgunts (atoi (vl-list->string (reverse (vl-string->list dwgunts))))
    );setq
|;
    (princ "\n------------\n") 
    (princ dwgunts)
    ;; (princ) 
);defun

(princ "\nType LTX to run the script.")
(princ)