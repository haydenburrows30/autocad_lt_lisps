; https://forum.dwg.ru/showthread.php?t=138086&page=1

; LISP. Automatic marking of cable lines and compilation of KZ
; By valerik88

; The program has the following functions:
; cableObject - add an area where cables will be brought in (acts as cabinets, consumers, everything that is connected by cable. The area is not printed)
; cableObjectRename - rename an area
; cable - set cable parameters for a polyline
; cableMark - mark cables on the plan
; cableJ - select cables and create a cable log based on them.
; cableObjectScheme - draw a single-line diagram for the cableObject
; cableReactor - enable reactors to edit the system via a single-line diagram
; cableReactorOff - disable reactors

; When changing the cable marking, all markings on the plan are updated automatically.

(vl-load-com)
  (setq
  acad_object (vlax-get-acad-object)
  active_document (vla-get-ActiveDocument acad_object)
  model_space (vla-get-ModelSpace active_document)
  *cableMarksLayer* "CL-leaders-auto layer"
  *cableObjectsLayer* "Non-printing layer - cable objects"
  *different_str* "*Different*"
  *olSchemeTable* "ol-scheme-table"
  *olSchemeCell* "ol-scheme-cell"
  *olSchemeShina* "ol-scheme-main-qf"
  *olSchemeQF0* "ol-introductory-diagram qf"
  *olSchemeQF1* "ol-scheme-outgoing-qf"
  *olSchemePoint* "ol-connection-diagram"
  *cableReactorsOn* nil 
  );end setq

  (if (not getpropertyvalue)
	(defun getpropertyvalue(n key / )
	(vlax-get-property (vlax-ename->vla-object n) key)
	 );end defun getpropertyvalue
  );end if

(Defun c:cable( / sel n vla)
  (vla-startundomark active_document)

Select the route line
(princ "Select a Polyline\n")
(setq sel (ssget '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))
(if (not sel)
	(progn
	(princ "Error: You need to select a Polyline\n")
	);end progn

  	(progn
	  	(setCableDialog sel)
	);end progn
  );end if

  (vla-endundomark active_document)
  (princ)
);end defun cable

(Defun setCableDialog (sel / sel1 n n1 p1 p2 cableData file handle item dcl_id ddi mark
		       cableMark cableSech startCable endCable
		       stock startAdd endAdd centerAdd
		       mark_lst cableMark_lst cableSech_lst startCable_lst endCable_lst
		       stock_lst startAdd_lst endAdd_lst centerAdd_lst voltage_lst voltage coord verCnt
		       tmp tmp1)

  (setq	file   (strcat (vl-string-right-trim
			 "\\"
			 (vla-get-tempfilepath
			   (vla-get-files
			     (vla-get-preferences (vlax-get-acad-object))
			   )
			 )
		       )
		       "\\dlg.dcl"
	       ) ;_ end of strcat
	handle (open file "w")
  ) ;_ end of setq
  (foreach item
	   '("
cableMark : dialog {label = \"Cable data\";
:row{
	:column {
    :edit_box {label = \"Stage\";
              key = \"mark\";
              edit_width=60;
              edit_limit = 1000;
              }
    :edit_box {label = \"Cable Type\";
              key = \"cableSech\";
              edit_width=60;
              edit_limit = 1000;
              } 
    :edit_box {label = \"Start\";
              key = \"startCable\";
              edit_width=60;
              edit_limit = 1000;
              }
    :edit_box {label = \"End\";
              key = \"endCable\";
              edit_width=60;
              edit_limit = 1000;
              }
    :edit_box {label = \"stock, % (add to length)\";
              key = \"stock\";
              edit_width=60;
              edit_limit = 1000;
              }
    :edit_box {label = \"+ Beginning, m\";
              key = \"startAdd\";
              edit_width=60;
              edit_limit = 1000;
              }  
    :edit_box {label = \"+ End, m\";
              key = \"endAdd\";
              edit_width=60;
              edit_limit = 1000;
              }	
    :edit_box {label = \"+Lowering/rising, m\";
              key = \"centerAdd\";
              edit_width=60;
              edit_limit = 1000;
              }
    :edit_box {label = \"Lots\";
          key = \"cableMark\";
          edit_width=60;
          edit_limit = 1000;
          }
  }
}
"
"   : column { label = \"Cable Data\" ;  
                   children_alignment = left ; children_fixed_width = true ;  // column"
"     : row {                                                           "
"                                                                       "
"        : column { fixed_width = true ; width = 42; //                 "
"           : row {                                                     "
"              : text     { label = \"Line Length:\"; }                 "
"           } // end row
            : row {                                                     "
"              : text     { label = \"Units:\"; }                       "
"           } // end row       "
"       : row {                                                         "
"              : text     { label = \"Voltage:\"; }                     "
"           } // end row "
"       : row {                                                         "
"              : text     { label = \"Type:\"; }                     "
"           } // end row "
"        } // column                                                    "
"        : column {                                                     "
"           : row {                                                     "
"              : text { key = \"lineLength\";fixed_width = true ; width = 20;}   "
"           } // end row
            : row {                                                    "
"              :popup_list{
                key=\"lengthEd\";
                list = \"mm\\nm\";
                is_default = true;
                is_cancel = true;
                        }                                             "
"           } // end row                                              "
"            :row {                                                   "
"              :popup_list{
                  key=\"voltage\";
                  list = \"HV\\nLV\\nSL\";
                  alignment = left;
                  is_default = true;
                  is_cancel = true;
               }                                                      "
"           } 
            :row {                                                   "
"              :popup_list{
                  list = \"185Al3cNS\\n300Al3cNS\\n95Al3cNS\\n16Cu1cNS\";
                  key=\"cableType\";
                  alignment = left;
                  is_default = true;
                  is_cancel = true;
               }                                                      "
"           }       "
"           : spacer{height=0.2;}                                     "
"        } // column                                                  "
"                                                                     "
"     } // end row                                                    "                                                                                                                                                                                                                         
"   } // column                                                       "
"      
       "
     		"   :	row {" 
  		"	    fixed_width = true;" 
  		"	    alignment = centered;" 
  		" 	: spacer { width = 7; }" 
  		"	    ok_button;" 
   "     : button { "
   "       key = \"clear\"; "
   "       label = \"Clear\"; "			 
   "       mnemonic = \"A\"; "
   "     } "
  		" 	  cancel_button;" 
  		" 	: spacer { width = 7; }" 
  		"    }"
       "

       : spacer{height=0.2;} 
""
}"	    )
    (write-line item handle)
  ) ;_ end of foreach
  (close handle)


;Loading the dialogue
  (setq dcl_id (load_dialog file))
  (if (< dcl_id 0)
    (progn
      (alert "Failed to load application form")
      (exit)
    )
  )

  (if (not (new_dialog "cableMark" dcl_id))
    (progn
      (alert "The dialog could not be loaded!")
      (exit)
    )
  )

  (setq n (ssname sel 0))
  (if *cableFormData*
    (setq
	    startCable (cdr (assoc "startCable" *cableFormData*))
	    endCable (cdr (assoc "endCable" *cableFormData*))
	    mark (cdr (assoc "mark" *cableFormData*))
	    cableMark (cdr (assoc "cableMark" *cableFormData*))
	    cableSech (cdr (assoc "cableSech" *cableFormData*))
	    stock (cdr (assoc "stock" *cableFormData*))
	    startAdd (cdr (assoc "startAdd" *cableFormData*))
	    endAdd (cdr (assoc "endAdd" *cableFormData*))
	    centerAdd (cdr (assoc "centerAdd" *cableFormData*))
	    lengthEd (cdr (assoc "lengthEd" *cableFormData*))
      voltage (cdr (assoc "voltage" *cableFormData*))
      cableType (cdr (assoc "cableType" *cableFormData*))
    )

    (progn

  (setq startCable_lst (list ""))
  (setq endCable_lst (list ""))
  (setq mark_lst (list ""))
  (setq cableMark_lst (list ""))
  (setq cableSech_lst (list ""))
  (setq stock_lst (list ""))
  (setq startAdd_lst (list ""))
  (setq endAdd_lst (list ""))
  (setq centerAdd_lst (list ""))
  (setq lengthEd_lst (list ""))
  (setq voltage_lst (list ""))
  (setq cable_type_lst (list ""))
  
  (setq i 0)
  (repeat (sslength sel)
    (setq n (ssname sel i))
    (setq cableData (vlax-ldata-get (vlax-ename->vla-object n) "cableData"))
    (setq tmp (getCableObjectTitle n))
	    (if cableData
	    (progn	        
	      	(setq tmp1 (cdr (assoc "startCable" cableData)))
	        (if (or (eq tmp1 "")(not tmp1))
		(setq startCable_lst (cons (car tmp) startCable_lst))
		(setq startCable_lst (cons (cdr (assoc "startCable" cableData)) startCable_lst))
		);end if
	      	(setq tmp1 (cdr (assoc "endCable" cableData)))
	      	(if (or (eq tmp1 "")(not tmp1))
		(setq endCable_lst (cons (cadr tmp) endCable_lst))
		(setq endCable_lst (cons (cdr (assoc "endCable" cableData)) endCable_lst))
		);end if
		(setq mark_lst (cons (cdr (assoc "mark" cableData)) mark_lst))
		(setq cableMark_lst (cons (cdr (assoc "cableMark" cableData)) cableMark_lst))
		(setq cableSech_lst (cons (cdr (assoc "cableSech" cableData)) cableSech_lst))
	        (setq stock_lst (cons (cdr (assoc "stock" cableData)) stock_lst))
		(setq startAdd_lst (cons (cdr (assoc "startAdd" cableData)) startAdd_lst))
		(setq endAdd_lst (cons (cdr (assoc "endAdd" cableData)) endAdd_lst))
		(setq centerAdd_lst (cons (cdr (assoc "centerAdd" cableData)) centerAdd_lst))
		(setq lengthEd_lst (cons (cdr (assoc "lengthEd" cableData)) lengthEd_lst))
    (setq voltage_lst (cons (cdr (assoc "voltage" cableData)) voltage_lst))
    (setq cable_type_lst (cons (cdr (assoc "cableType" cableData)) cable_type_lst))
	       );end progn
	      ;If there is no data, calculate startCable and endCable
	      (progn
		(setq tmp1 (cdr (assoc "startCable" cableData)))
		(setq startCable_lst (cons (car tmp) startCable_lst))
		(setq tmp1 (cdr (assoc "endCable" cableData)))
		(setq endCable_lst (cons (cadr tmp) endCable_lst))
	      );end progn
	   );end if cableData
    (setq i (1+ i))
  );end repeat

  (setq startCable_lst (mapcar '(lambda(x)(cond((or (eq x "")(not x)) "")(T x))) (cdr (reverse startCable_lst))))
  (setq endCable_lst (mapcar '(lambda(x)(cond((or (eq x "")(not x)) "")(T x))) (cdr (reverse  endCable_lst))))
  (setq mark_lst (mapcar '(lambda(x)(cond((or (eq x "")(not x)) "")(T x))) (cdr (reverse  mark_lst))))
  (setq cableMark_lst (mapcar '(lambda(x)(cond((or (eq x "")(not x)) "")(T x))) (cdr (reverse  cableMark_lst))))
  (setq cableSech_lst (mapcar '(lambda(x)(cond((or (eq x "")(not x)) "")(T x))) (cdr (reverse  cableSech_lst))))
  (setq stock_lst (mapcar '(lambda(x)(cond((or (eq x "")(eq x "0")(not x)) "0")(T x))) (cdr (reverse  stock_lst))))
  (setq startAdd_lst (mapcar '(lambda(x)(cond((or (eq x "")(eq x "0")(not x)) "0")(T x))) (cdr (reverse  startAdd_lst))))
  (setq endAdd_lst (mapcar '(lambda(x)(cond((or (eq x "")(eq x "0")(not x)) "0")(T x))) (cdr (reverse  endAdd_lst))))
  (setq centerAdd_lst (mapcar '(lambda(x)(cond((or (eq x "")(eq x "0")(not x)) "")(T x))) (cdr (reverse  centerAdd_lst))))
  (setq lengthEd_lst (mapcar '(lambda(x)(cond((or (eq x "")(eq x "0")(not x)) "m")(T x))) (cdr (reverse  lengthEd_lst))))
  (setq voltage_lst (mapcar '(lambda(x)(cond((or (eq x "")(eq x "0")(not x)) "LV")(T x))) (cdr (reverse  voltage_lst))))
  (setq cable_type_lst (mapcar '(lambda(x)(cond((or (eq x "")(eq x "0")(not x)) "185Al3cNS")(T x))) (cdr (reverse  cable_type_lst))))

	(setq startCable (cond ((eqLst startCable_lst)(car startCable_lst))(T *different_str*)))
	(setq endCable (cond ((eqLst endCable_lst)(car endCable_lst))(T *different_str*)))
	(setq mark (cond ((eqLst mark_lst)(car mark_lst))(T *different_str*)))
	(setq cableMark (cond ((eqLst cableMark_lst)(car cableMark_lst))(T *different_str*)))
	(setq cableSech (cond ((eqLst cableSech_lst)(car cableSech_lst))(T *different_str*)))
	(setq stock (cond ((eqLst stock_lst)(car stock_lst))(T *different_str*)))
	(setq startAdd (cond ((eqLst startAdd_lst)(car startAdd_lst))(T *different_str*)))
	(setq endAdd (cond ((eqLst endAdd_lst)(car endAdd_lst))(T *different_str*)))
	(setq centerAdd (cond ((eqLst centerAdd_lst)(car centerAdd_lst))(T *different_str*)))
  (setq lengthEd (cond ((eqLst lengthEd_lst)(car lengthEd_lst))(T *different_str*)))
  (setq voltage (cond ((eqLst voltage_lst)(car voltage_lst))(T *different_str*)))
  (setq cableType (cond ((eqLst cable_type_lst)(car cable_type_lst))(T *different_str*)))

    );end progn
  );end if *cableFormData*
  
  ; set defaults
  (set_tile "startCable" (cond (startCable startCable)(T "")))
  (set_tile "endCable" (cond (endCable endCable)(T "")))
  (set_tile "mark" (cond (mark mark)(T "")))
  (set_tile "cableMark" (cond (cableMark cableMark)(T "")))
  (set_tile "cableSech" (cond (cableSech cableSech)(T "")))
  (set_tile "stock" (cond (stock stock)(T "1")))
  (set_tile "startAdd" (cond (startAdd startAdd)(T "1")))
  (set_tile "endAdd" (cond (endAdd endAdd)(T "1")))
  (set_tile "centerAdd" (cond (centerAdd centerAdd)(T "0")))
  (set_tile "lengthEd" (cond (lengthEd lengthEd)(T "1")))
  (set_tile "voltage" (cond (voltage voltage)(T "1")))
  (set_tile "cableType" (cond (cableType cableType)(T "0")))

  ;set line length units
  (set_tile "lineLength" (strcat (rtos (getpropertyvalue n "Length")) (cond ((eq lengthEd "1") " m")((eq lengthEd "0") " mm")(T " mm")) (cond ((eqLst lengthEd_lst) "")(T "*Different*units"))))
  (action_tile "cancel" "(done_dialog 0)")
  (action_tile "accept" "(saveCable sel)(done_dialog 1)")
  (action_tile "lengthEd" "(saveForm)(done_dialog 2)")
  (action_tile "voltage" "(saveForm)(done_dialog 2)")
  (action_tile "cableType" "(saveForm)(done_dialog 2)")
  (action_tile "clear" "(done_dialog 3)")	

  (setq ddi (start_dialog))
  (unload_dialog dcl_id)
  
  (if (= ddi 0)(princ "\n Cancelled!\n"))
  (if (= ddi 1)(princ "\n Saved!\n"))
  (if (= ddi 2)(setCableDialog sel))
  (if (= ddi 3)(clearCableData sel))

(vla-endundomark active_document)
) ;end Defun setCableDialog


;Returns the CableObjectTitle for the 1st and last point of the polyline
(defun getCableObjectTitle (n / coord verCnt p1 p2 i sel1 n1 cableObjectData title ptLst startCable endCable)
;Select all cable object polylines on the layer "Non-printing layer -CL objects"
(vla-ZoomAll acad_object)
	  	(setq sel1 (ssget "X" (list     '(-4 . "<AND")
					    	'(-4 . "<OR")
					    	'(0 . "LWPOLYLINE")
					    	'(0 . "POLYLINE")
					    	'(-4 . "OR>")
					         (cons 8 *cableObjectsLayer*)
					        '(-4 . "AND>")
					)
			    )
		   );end setq sel1

		(if sel1
		  (progn
		;Coordinates of CL points
		(setq coord (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object n) "Coordinates"))))
		;Number of route points
		(setq verCnt (/(length coord) 2))
		;Calculate the coordinates of the 1st vertex
		(setq p1 (list (nth 0 coord) (nth 1 coord) 0))
		;Calculate the coordinates of the last vertex
		(setq p2 (list (nth (- (* verCnt 2) 2) coord) (nth (- (* verCnt 2) 1) coord) 0))

			(setq i1 0)
			(repeat (sslength sel1)
			(setq n1 (ssname sel1 i1))		
			(setq title (cableGetProperty n1 "cableObjectData" "title"))				    
			(setq ptLst (mapcar '(lambda(x)(trans x 0 1))(mapcar 'cdr (vl-remove-if-not '(lambda(x)(= 10(car x)))(entget n1)))))
				(if (In_Figure (trans p1 1 0) ptLst)
				(setq startCable title)
				);end if
				(if (In_Figure (trans p2 1 0) ptLst)
				(setq endCable title)
				);end if
			(setq i1 (1+ i1))
			);end repeat
		);end progn
		);end if sel1
		;Returning the zoom to its original position
  		(vla-ZoomPrevious acad_object)
(list startCable endCable)
);end defun getCableObjectTitle

;Delete cable data
(defun clearCableData(sel /)
  (setq i 0)
  (repeat (sslength sel)
    (setq n (ssname sel i))
	 (vlax-ldata-put (vlax-ename->vla-object n) "cableData" nil)
    	 (setq i (1+ i))
   );end repeat
(princ "\n Cleared!\n")
 );end Defun clearCableData

(defun saveForm( / mark cableMark cableSech startCable endCable
		       stock startAdd endAdd centerAdd lengthEd voltage)
      (setq *cableFormData*
        (list
	(cons "startCable" (get_tile "startCable"))
	(cons "endCable" (get_tile "endCable"))
	(cons "mark" (get_tile "mark"))
	(cons "cableMark" (get_tile "cableMark"))
	(cons "cableSech" (get_tile "cableSech"))
	(cons "stock"  (get_tile "stock"))
	(cons "startAdd" (get_tile "startAdd"))
	(cons "endAdd" (get_tile "endAdd"))
	(cons "centerAdd" (get_tile "centerAdd"))
	(cons "lengthEd" (get_tile "lengthEd"))
  (cons "voltage" (get_tile "voltage"))
  (cons "cableType" (get_tile "cableType"))
     )
	    )
);end Defun saveForm

;Saving cable data
(Defun saveCable (sel / cableData oldCableData sel i n)

  (setq i 0)
  (repeat (sslength sel)
    (setq n (ssname sel i))
    (setq oldCableData (vlax-ldata-get (vlax-ename->vla-object n) "cableData")) 
    (setq cableData
    (list
	(cons "startCable" (get_tile "startCable"))
	(cons "endCable" (get_tile "endCable"))
	(cons "mark" (get_tile "mark"))
	(cons "cableMark" (get_tile "cableMark"))
	(cons "cableSech" (get_tile "cableSech"))
	(cons "stock"  (get_tile "stock"))
	(cons "startAdd" (get_tile "startAdd"))
	(cons "endAdd" (get_tile "endAdd"))
	(cons "centerAdd" (get_tile "centerAdd"))
	(cons "lengthEd" (get_tile "lengthEd"))
  (cons "voltage" (get_tile "voltage"))
  (cons "cableType" (get_tile "cableType"))
     )
    );end setq

  ;If the value is *Various*we do not save it
  (setq cableData (mapcar '(lambda(el / )(if (eq (cdr el) *different_str*)(cons (car el) (cdr (assoc (car el) oldCableData))) el)) cableData))
  
  (vlax-ldata-put (vlax-ename->vla-object n) "cableData" cableData)
  (setq i (1+ i))
  );end repeat

  (setq *cableFormData* nil)

  ;Updating callouts
  (cableMarkRenew)
  
);end Defun saveCable

;Marking a line on a multileader plan
(Defun C:cableMark ( / p1 p2 p3 ang len sel n str i cableData mark gr tmp oldLayer)
  (vla-startundomark active_document)
  
  ;Requesting a point
  (setq p1 (getpoint "Select lines to label\n"))
  (setq p2 p1)
  
  (while (or (= (car (setq gr (grread nil 5 0))) 5)
               (= (car gr) 11)
               (= (car gr) 25) ; For old version AutoCad
	       (= (car gr) 2)
               );end or
    ;Angle between points
    (setq ang (+ (angle p1 p2) pi))
    ;Distance between points
    (setq len (distance p1 p2))	
    (setq p3 (polar p1 ang len))
    (redraw)
    (drawRect p3 p2 9)
	  (cond		
	    	((= (car gr) 5)
		(setq p2 (last gr))
		)
	  );end cond
    
   );end while
  (redraw)
  ;Select all polylines in the rectangle p1 p2
  (setq sel (ssget "_C" p3 p2 '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))

  (if (not sel)
	(progn
	(princ "Error: Polyline must be selected")
	);end progn

  	(progn
	  (setq tmp (list ""))
	  (setq i 0)
	  (repeat (sslength sel)
	  	(setq n (ssname sel i))
	    	 ;Reading properties and writing to variables
  		(setq cableData (vlax-ldata-get (vlax-ename->vla-object n) "cableData"))
			(if cableData
	    			(progn
		    		(setq mark (cdr (assoc "mark" cableData)))
				(setq tmp (cons mark tmp))
				);end progn
			);end if cableData
	    	(setq i (1+ i))
	  );end repeat
	  (setq str "")
	  ;Sort and form a line
	  (setq tmp (vl-sort tmp '<))
	  (while (setq tmp (cdr tmp))
	    (setq str (strcat str (car tmp) "\n"))
          );end while

	 (if (not (eq str ""))
	   (progn

	;Remember the current layer
	(setq oldLayer (vla-get-ActiveLayer active_document))
	;If there is no layer “KL Leaders”, create it
	(cableNewLayer *cableMarksLayer* 7 1)
	
	;Switch to the Leaders layer
	(vla-put-ActiveLayer active_document (vla-Item (vla-get-Layers active_document) *cableMarksLayer*))
	  
	;Inserting a multileader
	(command "_mleader" p1 p1 str)
	;Adding points for auto-update
	(vlax-ldata-put (vlax-ename->vla-object (entlast)) "rectPoints" (list p3 p2))

	;Switch to the old layer
	(vla-put-ActiveLayer active_document oldLayer)
		  
	(while (or (= (car (setq gr (grread nil 5 0))) 5)
               (= (car gr) 11)
               (= (car gr) 25) ; For old version AutoCad
	       (= (car gr) 2)
               );end or
	  (cond		
	    	((= (car gr) 5)
		(setq p3 (last gr))
		 (if setpropertyvalue
		   (progn
		(setpropertyvalue (entlast) "TextLocation/X" (car p3))
		(setpropertyvalue (entlast) "TextLocation/Y" (cadr p3))
		   );end progn
		   (setMleaderPosition (entlast) p1 p3)		     
		   );end if setpropertyvalue
		)		
	  );end cond
	  );end while
	  );end progn
	   (princ "CL not found")
	  );end if
	  
	);end progn
  );end if

  (vla-endundomark active_document)
  (princ)
);end Defun cableMark

;Position Leader
(defun setMleaderPosition (ml p1 p2 / ent tmp)
  (setq mlead (vlax-ename->vla-object ml))
  (setq ptarr (vlax-make-variant
	      (vlax-safearray-fill
		(safearray vlax-vbdouble '(0 . 5))
		(apply 'append (list p1 p2))))
   )
	  (vla-setdoglegdirection
	  mlead
	  0
	  (vlax-3D-point
	    (list
	      (if (<= (car p1) (car p2)) 1 -1)
	      0
	      0
	      )
	    )
	  )
	(if (> (car p2) (car p1))
	  (vla-put-TextJustify mlead acAttachmentPointMiddleLeft)
	  (vla-put-TextJustify mlead acAttachmentPointMiddleRight)
	  )
(vla-setleaderlinevertices mlead 0 ptarr )
(vl-catch-all-apply 'vlax-invoke-method (list mlead 'evaluate))
(vla-update mlead)
);end defun setMleaderPosition

;Updating markings after editing a cable
(Defun cableMarkRenew( / sel sel1 n n1 i j points p1 p2 cableData mark str tmp)
  (vla-startundomark active_document)
    	
  	;Select all multileaders on the Leaders CL layer
  	(vla-ZoomAll acad_object)
  	(setq sel (ssget "X" (list '(-4 . "<AND") '(0 . "MULTILEADER") (cons 8 *cableMarksLayer*) '(-4 . "AND>"))))
  	(if sel
	  (progn
	  (setq i 0)
	  (repeat (sslength sel)
	     (setq n (ssname sel i))
		;Getting the points of the rectangle for selecting lines
	    	(if (setq points (vlax-ldata-get (vlax-ename->vla-object n) "rectPoints"))
		  (progn
			(setq p1 (car points))
		  	(setq p2 (cadr points))
		  	;Select all polylines in the rectangle p1 p2
		  	(setq sel1 (ssget "_C" p1 p2 '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))
			(if sel1
			  (progn
			    (setq tmp (list ""))
				  (setq j 0)
				  (repeat (sslength sel1)
				  	(setq n1 (ssname sel1 j))
				    	 ;Reading properties and writing to variables
			  		(setq cableData (vlax-ldata-get (vlax-ename->vla-object n1) "cableData"))
						(if cableData
				    			(progn
					    		(setq mark (cdr (assoc "mark" cableData)))
							(setq tmp (cons mark tmp))
							);end progn
						);end if cableData
				    	(setq j (1+ j))
				  );end repeat
				  (setq str "")
				  ;Sort and form a line
				  (setq tmp (vl-sort tmp '<))
				  (while (setq tmp (cdr tmp))
				    (setq str (strcat str (car tmp) "\n"))
			          );end while
			    ;Updating the text inside the callout
			    (setpropertyvalue n "MText/Contents" str)
			  );end progn
			);end if sel1
		 );end progn 
		);end if
	     (setq i (1+ i))
	  );end repeat  
	  );end progn
	  );end if
  ;Returning the zoom to its original position
  (vla-ZoomPrevious acad_object)
  (vla-endundomark active_document)
  (princ)
);end cableMarkRenew


;Cable Table
(Defun c:cableJ ( / p1 p2 sel n i j cableData mark cableMark cableSech startCable endCable
		       stock startAdd endAdd centerAdd lengthEd voltage table rows cols row len selList)
  (vla-startundomark active_document)
  
;Request a point 
  ;(setq p1 (getpoint "Select all CLs with a rectangle\n"))
  ;(setq p2 (GETCORNER p1 "Select all CLs with a rectangle\n"))
  (princ "Select CL for QOL: \n")
  (setq sel (ssget '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))

  (if (not sel)
	(progn
	(princ "Error: Polylines must be selected")
	);end progn

  	(progn

     
     
;We calculate how many objects have the cableData property, form a list of names, sort it
	  (setq rows 0)
	  (setq i 0)
	  (setq selList (list ""))
	  (repeat (sslength sel)
	    	(setq n (ssname sel i))
  		  (if (vlax-ldata-get (vlax-ename->vla-object n) "cableData")
			  (progn
			  (setq rows (1+ rows))
			  (setq selList (cons n selList))
			  );end progn
		  );end if
	    	(setq i (1+ i))
	   );end repeat

	  (setq selList (cdr (reverse selList)))
	  
	 
	  (setq selList (vl-sort selList  '(lambda(n1 n2 / )
	  	(<
		   (cableGetProperty n1 "cableData" "mark")
		   (cableGetProperty n2 "cableData" "mark")
		 );end <
	  )))

	    	
;Query the table insertion point
	  (setq p1 (getpoint "QOL table insertion point\n"))
	  
	  ;Create a table
	  (setq rows (+ 4 rows))
	  (setq cols 13)
	  (setq table (vla-AddTable 
	                    model_space 
	                    (vlax-3d-point p1)
	                    rows
	                    cols 
	                    5
	                    10)
	 );end setq
     
    ;turns table generation off, very fast!
    ;https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/performance-problem-with-vla-table/td-p/2868352
    (vla-put-regeneratetablesuppressed table :vlax-true)
     
    ;Fill the cells
	  (vla-setText table 0 0 "Cable and Duct Table")
	  (vla-setText table 1 0 "Stage")
	  (vla-setText table 1 1 "Location")
	  (vla-setText table 1 3 "Cable Information")
	  (vla-setText table 1 7 "Cable, wire")
	  (vla-setText table 2 1 "Start")
	  (vla-setText table 2 2 "End")
	  (vla-setText table 2 3 "Duct")
	  (vla-setText table 2 6 "Voltage")
	  (vla-setText table 2 7 "According to the project")
	  (vla-setText table 2 10 "Laid")
	  (vla-setText table 3 3 "Note")
	  (vla-setText table 3 4 "Cable Type")
	  (vla-setText table 3 5 "Length, m.")
	  (vla-setText table 3 7 "Note")
	  (vla-setText table 3 8 "Cable Type")
	  (vla-setText table 3 9 "Length, m.")
	  (vla-setText table 3 10 "Note")
	  (vla-setText table 3 11 "Cable Type")
	  (vla-setText table 3 12 "Length, m.")
    
    ;Set rows text height
    (vla-settextheight table acTitleRow 5) ;set title text height
    (vla-settextheight table acHeaderRow 3) ;set header text height
    (vla-settextheight table acDataRow 2) ;set data rows text height

	  ;Merge cells
	  (vla-MergeCells table 1 3 0 0)
	  (vla-MergeCells table 1 1 1 2)
	  (vla-MergeCells table 1 1 3 6)
	  (vla-MergeCells table 1 1 7 12)
	  (vla-MergeCells table 2 3 1 1)
	  (vla-MergeCells table 2 3 2 2)
	  (vla-MergeCells table 2 2 3 5)
	  (vla-MergeCells table 2 3 6 6)
	  (vla-MergeCells table 2 2 7 9)
	  (vla-MergeCells table 2 2 10 12)

	  ;Adjusting cell sizes
	  (vla-SetColumnWidth table 0 30)
	  (vla-SetColumnWidth table 1 50)
	  (vla-SetColumnWidth table 2 50)
	  (vla-SetColumnWidth table 3 30)
	  (vla-SetColumnWidth table 4 30)
	  (vla-SetColumnWidth table 5 30)
	  (vla-SetColumnWidth table 6 30)
	  (vla-SetColumnWidth table 7 35)
	  (vla-SetColumnWidth table 8 25)
	  (vla-SetColumnWidth table 9 30)
	  (vla-SetColumnWidth table 10 30)
	  (vla-SetColumnWidth table 11 30)
	  (vla-SetColumnWidth table 12 30)

	;Align everything to the center
	(setq i 0)
	(repeat 4
	(setq j 0)
  ;header and title rows height to 10
	(vla-SetRowHeight table i 10)
		(repeat 13
		(vla-setcellalignment table i j acmiddlecenter)
		(setq j (1+ j))
		);end repeat 13
	 (setq i (1+ i))
	);end repeat 3
	  
     ;mark - stage
     ;startCable - 
     ;endCable -
     ;cableMark - circuit no.
     ;cableSech - cable type
     ;len - cable length
     ;cableMark - note
	  (setq i 0)
	  (setq row 4)
	  (setq selList (cons "" selList))
	  (while (setq selList (cdr selList))

	    (setq n (car selList))	    	
	    	 ;Read properties
  		(setq cableData (vlax-ldata-get (vlax-ename->vla-object n) "cableData"))		   
	    	
			  (setq startCable (cdr (assoc "startCable" cableData)))
			  (setq endCable (cdr (assoc "endCable" cableData)))
			  (setq mark (cdr (assoc "mark" cableData)))
			  (setq cableMark (cdr (assoc "cableMark" cableData)))
			  (setq cableSech (cdr (assoc "cableSech" cableData)))
			  (setq stock (cdr (assoc "stock" cableData)))
        (setq voltage (cdr (assoc "voltage" cableData)))
        (setq cableType (cdr (assoc "cableType" cableData)))

			  (setq startCable (cond (startCable startCable)(T "")))
			  (setq endCable (cond (endCable endCable)(T "")))
			  (setq mark (cond (mark mark)(T "")))
			  (setq cableMark (cond (cableMark cableMark)(T "")))
			  (setq cableSech (cond (cableSech cableSech)(T "")))	  
        (setq voltage (cond (voltage voltage)(T "")))
        (setq cableType (cond (cableType cableType)(T "")))
     
        (setq voltage_string_list (list "HV" "LV" "SL"))
        (setq cable_type_string_list (list "185Al3cNS" "300Al3cNS" "95Al3cNS" "16Cu1cNS"))
  
        ;Convert the return ascii string value into an integer:
        ;Use this integer to return the correct item from the list:
        (setq voltage_output (nth (atoi voltage) voltage_string_list))
        (setq cableType_output (nth (atoi cableType) cable_type_string_list))

			  (vla-setText table row 0 mark)
			  (vla-setText table row 1 startCable)
			  (vla-setText table row 2 endCable)
			  (vla-setText table row 3 cableMark)
			  (vla-setText table row 4 cableType_output) ;cableSech
	    	;CL length
			  (setq len (getCableLength n cableData))
			  (vla-setText table row 5 (rtos len 2 0))
        (vla-setText table row 6 voltage_output)
			  (vla-setText table row 10 "-")
			  (vla-setText table row 11 "-")
			  (vla-setText table row 12 "-")

			  ;Align everything to the center
			  (setq j 0)
			  (repeat 13
			    (vla-setcellalignment table row j acmiddlecenter)
			    (setq j (1+ j))
			  );end repeat
			  
	    (setq row (1+ row))
	    (setq i (1+ i))
      );end while

	  );end progn
    );end if

  (vla-endundomark active_document)
  
  ;turns generation back on, very fast!
  (vla-put-regeneratetablesuppressed table :vlax-false)
  (vla-recomputetableblock table :vlax-true)
  (princ)
);end defun cableJ

;Cable length
(defun getCableLength(n cableData / stock startAdd endAdd centerAdd lengthEd)
  (setq stock (cdr (assoc "stock" cableData)))
  (setq startAdd (cdr (assoc "startAdd" cableData)))
  (setq endAdd (cdr (assoc "endAdd" cableData)))
  (setq centerAdd (cdr (assoc "centerAdd" cableData)))
  (setq lengthEd (cdr (assoc "lengthEd" cableData)))

  (setq stock (cond (stock stock)(T "0")))
  (setq startAdd (cond (startAdd startAdd)(T "0")))
  (setq endAdd (cond (endAdd endAdd)(T "0")))
  (setq centerAdd (cond (centerAdd centerAdd)(T "0")))
  
  (setq stock (/ (+ 100 (atof stock)) 100.0))
  (setq len (getpropertyvalue n "Length"))
  (if (eq lengthEd "0")(setq len (/ len 1000)));end if
  (setq len (* (+ len
          (atof startAdd)
          (atof endAdd)
          (atof centerAdd)
       )
      stock
     )
  )
  len
);end defun getCableLength

;Drawing a dotted rectangle
(Defun drawRect(p1 p2 color / x1 x2 y1 y2 param)
  (setq
	x1 (car p1)
	x2 (car p2)
	y1 (cadr p1)
	y2 (cadr p2)
	param (if (>= x1 x2) 5 0)
  )
	(grdraw  (list x1 y1)(list x1 y2) color param)
	(grdraw (list x1 y2)(list x2 y2) color param)
	(grdraw (list x2 y2)(list x2 y1) color param)
	(grdraw (list x2 y1)(list x1 y1) color param)
	
);end Defun drawRect

;Returns T if all elements of the list are the same and nil if at least one is different
(defun eqLst (lst / )
  (cond ((< (length lst) 2) T)
	(T (and (eq (car lst)(cadr lst)) (eqLst (cdr lst))))
  );end cond
);end defun eqLst

;Adds an area in which cable ends will be searched
(defun c:cableObjectAdd( / *error* title cableObject n oldLayer sel p1 p2 p3 ang len gr coord n1 ty cableObjectData tmp)
  (vla-startundomark active_document)

	  ;Error handler ESC
	    (defun *error* (msg)
	      (if n
		(entdel n)
		);end if
	     (vla-put-ActiveLayer active_document oldLayer)
	     (vla-endundomark active_document)
	     (princ "cableAddObject error\n")
	     (princ msg)
	     (princ)
	    ) ;_ end of defun

  (setq n nil)
  
  ;Remember the current layer
  (setq oldLayer (vla-get-ActiveLayer active_document))
  ;If there is no layer "Non-printing layer -CL objects", create it
  (cableNewLayer *cableObjectsLayer* 8 0)
  ;Switch to the layer "Non-printing layer -CL objects"
  (vla-put-ActiveLayer active_document (vla-Item (vla-get-Layers active_document) *cableObjectsLayer*))
  
  (command "_rectang" pause pause)
  (setq cableObject (entlast))

  ;Switch to the old layer
  (vla-put-ActiveLayer active_document oldLayer)

  ;Name option
  ;Select text and multileaders
  (setq coord (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object cableObject) "Coordinates"))))
  ;Calculate the coordinates of the 1st vertex
  (setq p1 (list (nth 0 coord) (nth 1 coord) 0))
  ;Calculate the coordinates of the 3rd vertex
  (setq p2 (list (nth 4 coord) (nth 5 coord) 0))

  ;Looking for callout
  (setq n1 (getCableObjectMark cableObject))
  (if n1
    (progn
  (setq ty (cdr (assoc 0 (entget n1))))  
  (setq title
	(cond ((eq ty "MULTILEADER")(getpropertyvalue n1 "MText/Contents"))
	      ((eq ty "TEXT")(getpropertyvalue n1 "TextString"))
	      ((eq ty "MTEXT")(getpropertyvalue n1 "Text"))
	      (T "")
	)
  );end setq title
  	);end progn
    (setq title "")
    );end if n1
  
  (setq tmp (getstring T (strcat "Object name:<" title ">\n")))
  (if (eq tmp "")(setq tmp title))

  (setq cableObjectData (list (cons "title" tmp)))
  (vlax-ldata-put (vlax-ename->vla-object cableObject) "cableObjectData" cableObjectData)

  (if (not (eq title ""))
    	;Editing the callout
	(progn
	  (if setpropertyvalue
	  (cond ((eq ty "MULTILEADER")(setpropertyvalue n1 "MText/Contents" tmp))
	      ((eq ty "TEXT")(setpropertyvalue n1 "TextString" tmp))
	      ((eq ty "MTEXT")(setpropertyvalue n1 "Text" tmp))
	      (T "")
	  );end cond
	  );end if setpropertyvalue
	);end progn
    	;If there is no callout, create it
        (progn

	 ;Remember the current layer
	(setq oldLayer (vla-get-ActiveLayer active_document))
	;If there is no “Leaders CL” layer, create it
	(cableNewLayer *cableMarksLayer* 7 1)	
	;Switch to the Leaders layer
	(vla-put-ActiveLayer active_document (vla-Item (vla-get-Layers active_document) *cableMarksLayer*))

	;Point to insert the leader
	;Angle between points
	(setq ang (angle p1 p2))
	;Segment length
	(setq len (distance p1 p2))
	;Midpoint
	(setq p1 (polar p1 ang (/ len 2)))
	
	;Insert multileader
	(command "_mleader" p1 p1 tmp)
	;Switch to the old layer
	(vla-put-ActiveLayer active_document oldLayer)
		  
	(while (or (= (car (setq gr (grread nil 5 0))) 5)
               (= (car gr) 11)
               (= (car gr) 25) ; For old version AutoCad
	       (= (car gr) 2)
               );end or
	  (cond		
	    	((= (car gr) 5)
		(setq p3 (last gr))
		 (if setpropertyvalue
		   (progn
		(setpropertyvalue (entlast) "TextLocation/X" (car p3))
		(setpropertyvalue (entlast) "TextLocation/Y" (cadr p3))
		   );end progn
		   (setMleaderPosition (entlast) p1 p3)		     
		   );end if setpropertyvalue
		)		
	  );end cond
	  );end while

	);end progn
  );end if

   ;Updating cable end and start data
  (cableUpdateByObject cableObject)

  ;Turn on the reactor
  (if *cableReactorsOn* (c:cableReactor))

  (vla-endundomark active_document)
  (princ)  
);end defun c:cableObjectAdd


;Renaming CableObject
(defun c:cableObjectRename( / *error* tmp sel n i j title oldTitle n1 ptLst
			   cableData coord verCnt p1 p2 cableObjectData
				ty
			   )
  (vla-startundomark active_document)

  	   ;Error handler ESC
	    (defun *error* (msg)
	     (vla-ZoomPrevious acad_object)
	     (vla-endundomark active_document)
	     (princ "Canceled cableObjectRename\n")
	     (princ msg)
	     (princ "\n")
	    ) ;_ end of defun

  
  (princ "Select CableObject\n")
  (setq sel (ssget "_:S" '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))
  (setq n (ssname sel 0))
  (setq title (cableGetProperty n "cableObjectData" "title"))
  (if (not title)(setq title ""))
  (setq tmp (getstring T (strcat "Object name:\n<" title ">")))
  (if (eq tmp "")(setq tmp title));end if
  (setq title tmp)

  (cableObjectRename n title)

 (vla-endundomark active_document)
  (princ)
);end defun c:cableObjectRename

;Renaming cableObject
(defun cableObjectRename (cableObject title / oldTitle n1 ty)

  ;Zoom to full screen
  (vla-ZoomAll acad_object)
  
	(setq oldTitle (cableGetProperty cableObject  "cableObjectData" "title"))	
	(cableSetProperty cableObject "cableObjectData" "title" title)
	  
	  ;Calculate the leader inside the objects and edit it
	  (setq n1 (getCableObjectMark cableObject))
	  (if n1
	    (progn
	        (setq ty (cdr (assoc 0 (entget n1))))
	        (if setpropertyvalue
		(cond ((eq ty "MULTILEADER")(setpropertyvalue n1 "MText/Contents" title))
		      ((eq ty "TEXT")(setpropertyvalue n1 "TextString" title))
		      ((eq ty "MTEXT")(setpropertyvalue n1 "Text" title))
		      (T "")
		); end cond
		);end if setpropertyvalue
	    );end progn
	  );end if n1

	  ;Updating cable end and start data
	  (cableUpdateByObject cableObject)

  ;Return the zoom to its original position
  (vla-ZoomPrevious acad_object)
 (princ (strcat "\N object \"" oldTitle "\" renamed to \"" title "\"\n"))
);end cableObjectRename

;Function for changing object properties
(defun cableSetProperty (n dataName propName propVal / vla data)
   (setq vla (vlax-ename->vla-object n))
   (setq data (vlax-ldata-get vla dataName))
   (setq data (subst  (cons propName propVal)(assoc propName data) data))
   (vlax-ldata-put vla dataName data)
);end defun cableSetProperty

;Function for getting object properties
(defun cableGetProperty (cableObject dataName propName / cableObjectData)
   (setq cableObjectData (vlax-ldata-get (vlax-ename->vla-object cableObject) dataName))
   (if cableObjectData
   (cdr (assoc propName cableObjectData))
   nil
   );end if cableObjectData
);end defun cableSetProperty

;Creating a new layer
(defun cableNewLayer (newLayer color printable / )
   (if (not (tblsearch "LAYER" newLayer))
		(entmake
			(list
				'(0 . "LAYER")
				'(100 . "AcDbSymbolTableRecord")               
				'(100 . "AcDbLayerTableRecord")
			    	 (cons 2  newLayer)			
				'(70 . 0)
				 (cons 62 color)    ;color  
				'(6 . "Continuous") ;line type
				 (cons 290 printable) ;1 - printable 0 - not printable
			)
		);end entmake
	);end if
);end defun cableNewLayer


;Enabling the reactor for text editing
(defun c:cableReactor( / *error* selCableObjects cableObject sel
		      cableObjectMarks-vla-list
		      shemaMarks-vla-list
		      shemaObjects-vla-list
		      shemaCables-vla-list
		      vla i j n n1 p1 mas title shemaSel cable str)
  
  ;Turn off the reactors
  (c:cableReactorOff)
  (setq *cableReactorsOn* T)

  ;CableObject callout reactor
  ;Get a list of all cableObjects
  (vla-ZoomAll acad_object)
  (setq selCableObjects (ssget "X" (list '(-4 . "<AND")'(-4 . "<OR")'(0 . "LWPOLYLINE")'(0 . "POLYLINE")'(-4 . "OR>") (cons 8 *cableObjectsLayer*) '(-4 . "AND>"))))
  (if selCableObjects
    (progn
  (setq i 0)
	  (repeat (sslength selCableObjects)  
	    ;Getting a callout
	    (setq n (getCableObjectMark (ssname selCableObjects i)))
	    (if n
	      (progn
	    	(setq vla (vlax-ename->vla-object n))
	    	(setq cableObjectMarks-vla-list (cons vla cableObjectMarks-vla-list))
	       );end progn
	     );end if n
	    (setq i (1+ i))
	  );end repeat
    );end progn
  );end if sel

  (if cableObjectMarks-vla-list
  (setq *vlr-cableObjectMark*
	 (vlr-object-reactor cableObjectMarks-vla-list "*vlr-cableObjectMark*" (list '(:vlr-modified . cableObjectMarkModified)))
  );end setq *vlr-cableObjectMark*
  );end if cableObjectMarks-vla-list

  ;Reactors in single-line diagrams
  (if selCableObjects
	  (progn	  
	  ;Get a list of all single-line diagrams
	  (setq sel (ssget "X" (list '(-4 . "<AND")'(0 . "INSERT") (cons 2 *olSchemeTable*) '(-4 . "AND>"))))
		  (if sel
		    (progn
		  (setq i 0)
		  (repeat (sslength sel)
		    (setq n (ssname sel i))
		    (setq vla (vlax-ename->vla-object n))
		    ;Insertion coordinate of the table block
		    (setq p1 (vlax-safearray->list (vlax-variant-value (vlax-get-property vla "InsertionPoint"))))
		    ;Scheme scale
		    (setq mas (vlax-get-property vla "XScaleFactor"))
		    ;Determine which CableObject the circuit belongs to
		    (setq cableObject nil)
		    (setq title (getSchemaTitle p1 mas))
		        (setq j 0)
	  		(repeat (sslength selCableObjects)
			  (setq cabObj (ssname selCableObjects j))
			  (if (eq (cableGetProperty cabObj "cableObjectData" "title") title)
			    (setq cableObject cabObj)
			  );end if
			(setq j (1+ j))
			);end repeat selCableObjects
		  ;If CableObject is found, assign reactors
		  (if cableObject
		    (progn
			;Looking for all CL markings (Group according to plan)
		        (setq shemaSel (getShemaMarks n))
		        (setq j 0)
		        (repeat (sslength shemaSel)
			  (setq n1 (ssname shemaSel j))
			  ;(setq str (getpropertyvalue mark "Text"))
			  ;Return the cable according to the marking
			  ;(setq cable (getCableByMark str))
			  ;If the marking corresponds to one of the cables, add it to the list for the reactor
			  ;(if cable
			    ;(progn
				(setq vla (vlax-ename->vla-object n1))
	    			(setq shemaMarks-vla-list (cons vla shemaMarks-vla-list))
			    ;);end progn
			  ;);end if cable
			(setq j (1+ j))
			);end repeat shemaSel
		        
		        ;Looking for all object markings on the single line
		        (setq shemaSel (getShemaCableObjects n))
		        (setq j 0)
		        (repeat (sslength shemaSel)
			(setq n1 (ssname shemaSel j))
				(setq vla (vlax-ename->vla-object n1))
	    			(setq shemaObjects-vla-list (cons vla shemaObjects-vla-list))
			(setq j (1+ j))
			);end repeat shemaSel

		        ;We are looking for all cable designations on the single line
		        (setq shemaSel (getShemaCableString n))
		        (setq j 0)
		        (repeat (sslength shemaSel)
			(setq n1 (ssname shemaSel j))
				(setq vla (vlax-ename->vla-object n1))
	    			(setq shemaCables-vla-list (cons vla shemaCables-vla-list))
			(setq j (1+ j))
			);end repeat shemaSel
		    );end progn
		  );end if cableObject

		  (setq i (1+ i))	
		  );end repeat sel
		    );end progn
		  );if sel
	  );end progn
  );end selCableObjects

  (if shemaMarks-vla-list
  (setq *vlr-cableShemaMark*
	 (vlr-object-reactor shemaMarks-vla-list "*vlr-cableShemaMark*" (list '(:vlr-modified . cableShemaMarkModified)))
  );end setq *vlr-cableObjectMark*
  );end if shemaMarks-vla-list

  (if shemaObjects-vla-list
  (setq *vlr-cableShemaObject*
	 (vlr-object-reactor shemaObjects-vla-list "*vlr-cableShemaObject*" (list '(:vlr-modified . cableShemaObjectModified)))
  );end setq *vlr-cableShemaObject*
  );end if shemaObjects-vla-list

  (if shemaCables-vla-list
  (setq *vlr-cableShemaCable*
	 (vlr-object-reactor shemaCables-vla-list "*vlr-cableShemaCable*" (list '(:vlr-modified . cableShemaCableModified)))
  );end setq *vlr-cableShemaCable*
  );end if shemaCables-vla-list
  
  ;Return the zoom to its original position
 (vla-ZoomPrevious acad_object)
  
 (princ)
);end c:cableReactor

;Reactor shutdown
(defun c:cableReactorOff ()
  (setq *cableReactorsOn* nil)
  
  (if *vlr-cableObjectMark* (vlr-remove *vlr-cableObjectMark*))
  (setq *vlr-cableObjectMark* nil)
  (if *vlr-cableShemaMark* (vlr-remove *vlr-cableShemaMark*))
  (setq *vlr-cableShemaMark* nil)
  (if *vlr-cableShemaObject* (vlr-remove *vlr-cableShemaObject*))
  (setq *vlr-cableShemaObject* nil)
  
  (setq *cableTMP* nil)
);end defun cableReactorOff

;Obtaining cable by marking
(defun getCableByMark (mark / cableData i sel cable)
  (setq cab nil)
;We get a list of all cables on the plan
  (setq sel (ssget "_X" '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))
  (setq i 0)
  (repeat (sslength sel)
    (setq cable (ssname sel i))
    ;Reading the cable properties
    (setq cableData (vlax-ldata-get (vlax-ename->vla-object cable) "cableData"))
	    (if cableData
	      	(progn
		(if (eq (cdr (assoc "mark" cableData)) mark)
		(setq cab cable)
		);end if
		);end progn
	     );end if
  (setq i (1+ i))
  );end repeat sel
  cab
);end defun getCableByMark

;Getting the shield name for the circuit
(defun getSchemaTitle (p mas / n title sel p1 p2)
  (setq title nil)
 ;Looking for the blue frame of the single line
  ;Search area
  (setq p1 (list (car p)(cadr p) 0))
  (setq p2 (list (+ (* mas 30)(car p))(+ (* mas 150)(cadr p)) 0))
  (vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
  (setq sel (ssget "_C" p1 p2 (list
			   '(-4 . "<AND")
			   '(-4 . "<OR")
			   '(0 . "POLYLINE")
			   '(0 . "LWPOLYLINE")
			   '(-4 . "OR>")
			   '(6 . "LINE")
			   '(-4 . "AND>")
			   )
		);end ssget
  );end setq sel
  (vla-ZoomPrevious acad_object)
  (if sel
  (progn
  (setq n (ssname sel 0))
  ;Calculate the top right point of the frame
  ;Coordinates of points
  (setq coord (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object n) "Coordinates"))))
  ;Calculate the coordinates of the 3rd vertex
  (setq p1 (list (nth 6 coord) (nth 7 coord) 0))
  (setq p2 (list (- (car p1)(* mas 50)) (- (cadr p1)(* mas 20)) 0))

  	(vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
  	;Look for the shield symbol in the upper right corner of the blue frame
	(setq sel (ssget "_C" p1 p2 (list '(0 . "MTEXT"))))
  	(if sel
	  (progn
	   (setq n (ssname sel 0))
	   (setq title (getpropertyvalue n "Text"))
	  );progn
	);end if sel
  	(vla-ZoomPrevious acad_object)
  );end progn
  );end if sel
  title
);end defun getSchemaTitle


;Obtaining Cable designations on the diagram
(defun getShemaCableString (shemaTable / vla mas n title sel p1 p2 cablesSel coord)
  (setq vla (vlax-ename->vla-object shemaTable))
  ;Insertion coordinate of the table block
  (setq p1 (vlax-safearray->list (vlax-variant-value (vlax-get-property vla "InsertionPoint"))))
  ;Scheme scale
  (setq mas (vlax-get-property vla "XScaleFactor"))
  
  (setq cablesSel nil)
  ;Looking for the blue frame of the single line
  ;Search area
  (setq p2 (list (+ (* mas 30)(car p1))(+ (* mas 150)(cadr p1)) 0))
  (vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
  (setq sel (ssget "_C" p1 p2 (list
			   '(-4 . "<AND")
			   '(-4 . "<OR")
			   '(0 . "POLYLINE")
			   '(0 . "LWPOLYLINE")
			   '(-4 . "OR>")
			   '(6 . "LINE")
			   '(-4 . "AND>")
			   )
		);end ssget
  );end setq sel
  (vla-ZoomPrevious acad_object)
  (if sel
  (progn
  (setq n (ssname sel 0))
  ;Calculate the top right point of the frame
  ;Coordinates of points
  (setq coord (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object n) "Coordinates"))))
  ;Calculate the X coordinate of the 2nd vertex
  (setq p2 (list (nth 3 coord) (nth 4 coord) 0))

  ;|
    (setq points (append p1 p2))
  (vla-AddPolyline model_space
    (vlax-safearray-fill 
    	(vlax-make-safearray vlax-vbDouble (cons 0 (1- (length points)))) 
    	points
    )
   )
   |;

  	(vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
  	;Look for designations in the line “Name of mechanism according to plan”
	(setq sel (ssget "_W" p1 p2 (list '(0 . "MTEXT"))))
  	(if sel (setq cablesSel sel))
  	(vla-ZoomPrevious acad_object)
  
  );end progn
  );end if sel
  cablesSel
);end defun getShemaCableObjects


;Getting the shield name for the circuit
(defun getShemaCableObjects (shemaTable / vla mas n title sel p1 p2 objectsSel coord)
  (setq vla (vlax-ename->vla-object shemaTable))
  ;Insertion coordinate of the table block
  (setq p1 (vlax-safearray->list (vlax-variant-value (vlax-get-property vla "InsertionPoint"))))
 ;Scheme scale
  (setq mas (vlax-get-property vla "XScaleFactor"))
  
  (setq objectsSel nil)
  ;Looking for the blue frame of the single line
  ;Search area
  (setq p2 (list (+ (* mas 30)(car p1))(+ (* mas 150)(cadr p1)) 0))
  (vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
  (setq sel (ssget "_C" p1 p2 (list
			   '(-4 . "<AND")
			   '(-4 . "<OR")
			   '(0 . "POLYLINE")
			   '(0 . "LWPOLYLINE")
			   '(-4 . "OR>")
			   '(6 . "LINE")
			   '(-4 . "AND>")
			   )
		);end ssget
  );end setq sel
  (vla-ZoomPrevious acad_object)
  (if sel
  (progn
  (setq n (ssname sel 0))
  ;Calculate the top right point of the frame
  ;Coordinates of points
  (setq coord (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object n) "Coordinates"))))
  ;Calculate the X coordinate of the 3rd vertex 
  (setq p2 (list (nth 6 coord) (- (cadr p1)(getpropertyvalue shemaTable "AcDbDynBlockPropertyDistance1")) 0))
   (setq p1 (list (car p1) (- (cadr p1)(* mas 38) 0) 0))

  ;|
    (setq points (append p1 p2))
  (vla-AddPolyline model_space
    (vlax-safearray-fill 
    	(vlax-make-safearray vlax-vbDouble (cons 0 (1- (length points)))) 
    	points
    )
   )
   |;

  	(vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
  	;Look for designations in the line “Name of mechanism according to plan”
	(setq sel (ssget "_W" p1 p2 (list '(0 . "MTEXT"))))
  	(if sel (setq objectsSel sel))
  	(vla-ZoomPrevious acad_object)
  
  );end progn
  );end if sel
  objectsSel
);end defun getShemaCableObjects

;Getting the shield name for the circuit
(defun getShemaMarks (shemaTable / title sel n p1 p2 marksSel vla coord)
  (setq vla (vlax-ename->vla-object shemaTable))
  ;Insertion coordinate of the table block
  (setq p1 (vlax-safearray->list (vlax-variant-value (vlax-get-property vla "InsertionPoint"))))
  ;Scheme scale
  (setq mas (vlax-get-property vla "XScaleFactor"))
  
  (setq marksSel nil)
 ;Looking for the blue frame of the single line
  ;Search area
  (setq p2 (list (+ (* mas 30)(car p1))(+ (* mas 150)(cadr p1)) 0))
  (vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
  (setq sel (ssget "_C" p1 p2 (list
			   '(-4 . "<AND")
			   '(-4 . "<OR")
			   '(0 . "POLYLINE")
			   '(0 . "LWPOLYLINE")
			   '(-4 . "OR>")
			   '(6 . "LINE")
			   '(-4 . "AND>")
			   )
		);end ssget
  );end setq sel
  (vla-ZoomPrevious acad_object)
  (if sel
  (progn
  (setq n (ssname sel 0))
  ;Calculate the top right point of the frame
  ;Coordinates of points
  (setq coord (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object n) "Coordinates"))))
  ;Calculate the X coordinate of the 3rd vertex
  (setq p2 (list (nth 6 coord) (- (cadr p1)(* mas 8)) 0))
;|
     (setq points (append p1 p2))
  (vla-AddPolyline model_space
    (vlax-safearray-fill 
    	(vlax-make-safearray vlax-vbDouble (cons 0 (1- (length points)))) 
    	points
    )
   )
|;
  	;Look for the marking designation in the line “Group according to plan”
 	(vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
	(setq sel (ssget "_W" p1 p2 (list '(0 . "MTEXT"))))
  	(if sel (setq marksSel sel))
  	(vla-ZoomPrevious acad_object)
  
  );end progn
  );end if sel
  marksSel
);end defun getShemaMarks

;Getting the leader object for CableObject
(defun getCableObjectMark (cableObject / ptLst sel)
    ;Polyline points
(setq ptLst (mapcar '(lambda(x)(trans x 0 1))(mapcar 'cdr (vl-remove-if-not '(lambda(x)(= 10(car x)))(entget cableObject)))))
(setq sel (ssget "_CP" ptLst '((-4 . "<OR")(0 . "MULTILEADER")(0 . "TEXT")(0 . "MTEXT")(-4 . "OR>"))))
 (if sel
	(ssname sel 0)
   	nil
 );end if sel
);end defun getCableObjectMark

;Getting cableObject by ename marking
(defun getCableObject (cableObjectMark / sel i n cableObject)
  (setq cableObject nil)
  ;Get a list of all cableObjects
  (vla-ZoomAll acad_object)
  (setq sel (ssget "X" (list '(-4 . "<AND")'(-4 . "<OR")'(0 . "LWPOLYLINE")'(0 . "POLYLINE")'(-4 . "OR>") (cons 8 *cableObjectsLayer*) '(-4 . "AND>"))))
  (if sel
    (progn
    (setq i 0)
    (repeat (sslength sel)	    
	    ;Getting a callout
	    (setq n (getCableObjectMark (ssname sel i)))
	    (if (equal n cableObjectMark)(setq cableObject (ssname sel i)));end if	     
	    (setq i (1+ i))
    );end repeat
    );end progn
  );end if sel
  ;Return the zoom to its original position
  (vla-ZoomPrevious acad_object)
cableObject
);end defun getCableObject

;Getting cableObject by String header
(defun getCableObjectByTitle (title / sel i n cableObject)
  (setq cableObject nil)
  ;Get a list of all cableObjects
  (vla-ZoomAll acad_object)
  (setq sel (ssget "X" (list '(-4 . "<AND")'(-4 . "<OR")'(0 . "LWPOLYLINE")'(0 . "POLYLINE")'(-4 . "OR>") (cons 8 *cableObjectsLayer*) '(-4 . "AND>"))))
  (if sel
    (progn
    (setq i 0)
    (repeat (sslength sel)	    
	    ;Getting a callout
	    (setq n (ssname sel i))
	    (if (equal (cableGetProperty n "cableObjectData" "title") title)(setq cableObject n));end if	     
	    (setq i (1+ i))
    );end repeat
    );end progn
  );end if sel
  ;Return the zoom to its original position
  (vla-ZoomPrevious acad_object)
cableObject
);end defun getCableObjectByTitle

;Reaction to editing a cable on a single line
(defun cableShemaCableModified (shemaCable_vla reactor lst / shemaCable mark str cable tmp p
				i words word sech len cableMark)
   (setq shemaCable (vlax-vla-object->ename shemaCable_vla))
   ;(princ "\ncableShemaCableModified\n")
   (if shemaCable
   (if (entget shemaCable)
     (progn       
   (setq str (getpropertyvalue shemaCable "Text"))   
   (setq tmp (cableTMPget (vl-prin1-to-string shemaCable)))   
   (if (not tmp) (cableTMPset (vl-prin1-to-string shemaCable) str)
	(progn
	  (if (not (eq tmp str))
	    (progn
	      ;We get the cable marking
	      (setq mark (getShemaCableMark shemaCable_vla))
	      (if mark
		(progn
		  ;We get the cable according to the marking
	  	  (setq cable (getCableByMark mark))
		  ;If a cable with this marking is found, then edit it
		  (if cable
		  (progn
		    ;Calculate the number of cores
		    ;(setq zhil (car (LM:str->lst (vl-string-subst "x" "?" (strcase cableSech T) 0)  "x")))
		    ;Split the string into words
		    (setq str (PL:String-Rep str "\n" " "))
		    (setq str (PL:String-Rep str "\r" " "))
		    (setq words (LM:str->lst str " "))
		    (setq i 0)
		    (repeat (length words)
		        (setq word (nth i words))
		        (cond
			  ;Section and cores
			  ((> (length (LM:str->lst (vl-string-subst "x" "õ" (strcase word T) 0) "x")) 1)
			   (setq sech word)
			   (cableSetProperty cable "cableData" "cableSech" word)
			  );end section and cores
			   ;length
			  ((> (length (LM:str->lst word "=")) 1)
			   (setq len word)
			  )
			  ;Otherwise brand
			  ((not cableMark)
			  (setq cableMark word)
			   (cableSetProperty cable "cableData" "cableMark" word)
			   )
			  (T nil)
			);end cond
			(setq i (1+ i))
		    );end repeat (length words)

		    (princ (strcat "Cable " mark " changed: Brand: " cableMark ", Section: " sech ", Length: " len " - Doesn't change!\n"))
		  );end progn
		  );end if cable
		);end progn
	       );end mark
	  (cableTMPset (vl-prin1-to-string shemaCable) nil)
	  );end progn
	  );end if  (not tmp)
	);end progn
   );end if not tmp
   );end progn
     );end if (entget shemaCable)
     );end if shemaCable
(princ)
);end cableShemaCableModified

;Cable marking on single line
(defun getShemaCableMark (shemaCable_vla / mark shemaCable mas textHeight p p1 p2)
  (setq shemaCable (vlax-vla-object->ename shemaCable_vla))
  ;Label insertion point
  (setq p (vlax-safearray->list (vlax-variant-value (vlax-get-property shemaCable_vla "InsertionPoint"))))
  ;Text height
  (setq textHeight (getpropertyvalue shemaCable "TextHeight"))
  ;Scale
  (setq mas (/ textHeight 3))
  ;Points to search
  (setq p1 (list (- (car p) (* 12.5 mas))(- (cadr p)(* 10 mas)) 0 ))
  (setq p2 (list (+ (car p) (* 12.5 mas))(- (cadr p)(* 18 mas)) 0 ))

  ;Look for the marking designation in the line “Group according to plan”
  (vla-ZoomWindow acad_object (vlax-3D-point p1) (vlax-3D-point p2))
  (setq sel (ssget "_W" p1 p2 (list '(0 . "MTEXT"))))
  (if sel (setq mark (getpropertyvalue (ssname sel 0) "Text")))
  (vla-ZoomPrevious acad_object)
  mark
);end defun getShemaCableMark
 

;Reaction to editing leader for CableObject
(defun cableObjectMarkModified (cableObjectMark_vla reactor lst / cableObjectMark title oldTitle cableObject cableObjectData)
 ;Getting cableObject by marking
  (setq cableObjectMark (vlax-vla-object->ename cableObjectMark_vla))
  (if cableObjectMark
  (if (entget cableObjectMark)
    (progn
  (setq cableObject (getCableObject cableObjectMark))
  (if cableObject
    (progn
      ;Get the callout text
      (setq title (getpropertyvalue cableObjectMark "MText/Contents"))
      (if (not *cableTMPvalue*) (setq *cableTMPvalue* title)
	(progn
      ;Update the object     
      (setq cableObjectData (vlax-ldata-get (vlax-ename->vla-object cableObject) "cableObjectData"))
      (setq oldTitle (cdr (assoc "title" cableObjectData)))
      (if (not (eq title oldTitle))
	(progn
      (setq cableObjectData (subst  (cons "title" title)(assoc "title" cableObjectData) cableObjectData))
      (vlax-ldata-put (vlax-ename->vla-object cableObject) "cableObjectData" cableObjectData)
      (princ (strcat "\N object \"" oldTitle "\" renamed to \"" title "\"\n"))
      ;Updating cables
      (cableUpdateByObject cableObject)
      (setq *cableTMPvalue* nil)
      );end progn (not *cableTMPvalue*)
	);end if 
        );end progn
	);end if (not (eq title oldTitle))
    );end progn
  );end if cableObject
  );end progn
    );end if (entget cableObjectMark)
    );end if cableObjectMark
  ;|
	(princ "\ncableObjectMarkModified\n")
  	(princ cableObjectMark)
  	(princ "\n")
  	(princ reactor)
  	(princ "\n")
  	(princ lst)
  	(princ "\n")
  |;
);end cable-command-end

(defun cableTMPget (key / )
	(cdr (assoc key *cableTMP*))
);end defun cableTMPget
(defun cableTMPset(key val /)
  	(if (assoc key *cableTMP*)
	  (if (not val)
	    (setq *cableTMP* (vl-remove (assoc key *cableTMP*) *cableTMP*))
	    (setq *cableTMP* (subst (cons key val)(assoc key *cableTMP*) *cableTMP*))
	    );end if (not val)
	  (progn
	   (if val (setq *cableTMP* (cons (cons key val) *cableTMP*)))
	  );end progn
	);end if (assoc key *cableTMP*)
);end defun cableTMPset

;Reaction to changing cable markings on a single-line diagram
(defun cableShemaMarkModified (shemaMark_vla reactor lst / shemaMark mark cable tmp)
   (setq shemaMark (vlax-vla-object->ename shemaMark_vla))
   ;(princ "\ncableShemaMarkModified\n")
   (if shemaMark
   (if (entget shemaMark)
     (progn       
   (setq mark (getpropertyvalue shemaMark "Text"))   
   (setq tmp (cableTMPget (vl-prin1-to-string shemaMark)))
   ;(princ (strcat "\nmark: " mark "\n"))
   ;(princ "\ntmp: ")
   ;(princ tmp)
   ;(princ "\n")
   
   (if (not tmp) (cableTMPset (vl-prin1-to-string shemaMark) mark)
	(progn
	  (if (not (eq tmp mark))
	    (progn
	  ;We receive the cable according to the old markings
	  (setq cable (getCableByMark tmp))
	  ;Check that there is no cable with a new marking	  
	  ;If a cable with this marking is found, and there is no cable with a new marking
	  (if (getCableByMark mark)
	  (progn
	      (princ (strcat "\nA cable marked \"" mark "\" already exists\n"))
	      ;Change back
	      ;(setpropertyvalue shemaMark "Text" *cableTMPvalue*)
	  );end progn
	  (if cable
		(progn
		(cableSetProperty cable "cableData" "mark" mark)
		;Updating callouts
  		(cableMarkRenew)
		(princ (strcat "\nCable marking \"" tmp "\" changed to \"" mark "\"\n"))
		);end progn
	  );end if cable
	  );end if (getCableByMark mark)
	  (cableTMPset (vl-prin1-to-string shemaMark) nil)
	  );end progn
	  );end if  (not (eq *cableTMPvalue* mark))
	  ;(cableTMPset (vl-prin1-to-string shemaMark) nil)
	);end progn
   );end if not tmp
   );end progn
     );end if (entget shemaMark)
     );end if shemaMark
(princ)
);end cableShemaMarkModified


;Reaction to changing the name of the CableObject of a single line diagram
(defun cableShemaObjectModified (shemaObject_vla reactor lst / cableObjectData shemaObject str cableObject tmp)
   (setq shemaObject (vlax-vla-object->ename shemaObject_vla))
   ;(princ "\ncableShemaObjectModified\n")
   (if shemaObject
   (if (entget shemaObject)
     (progn       
   (setq str (getpropertyvalue shemaObject "Text"))
   (setq tmp (cableTMPget (vl-prin1-to-string shemaObject)))
   
   (if (not tmp) (cableTMPset (vl-prin1-to-string shemaObject) str)
	(progn
	  (if (not (eq tmp str))
	    (progn
	  ;Get CableObject using old markings
	  (setq cableObject (getCableObjectByTitle tmp))
	  ;Check that there is no object with a new label
	  ;If an object with this marking is found, and there is no object with a new marking
	  (if (getCableObjectByTitle str)
	    (princ (strcat "\nObject \"" str "\" already exists\n"))
	    ;Renaming an object
	    (progn
	      ;(princ "test")
	    (if cableObject (cableObjectRename cableObject str));end if cableObject
	    )
	  );end if (getCableObjectByTitle str)
	  (cableTMPset (vl-prin1-to-string shemaObject) nil)
	  );end progn
	  );end if  (not (eq tmp object))
	);end progn
   );end if not tmp
   );end progn
     );end if (entget shemaObject)
     );end if shemaObject
(princ)
);end cableShemaObjectModified

;Updating cable data by cableObject
(defun cableUpdateByObject(cableObject / title sel n1 j ptLst cableData coord verCnt p1 p2)
  	  (setq title (cableGetProperty cableObject "cableObjectData" "title"))
  
   	  ;Polyline points
	  (setq ptLst (mapcar '(lambda(x)(trans x 0 1))(mapcar 'cdr (vl-remove-if-not '(lambda(x)(= 10(car x)))(entget cableObject)))))
	  ;Calculating the ends of the cables inside the object
	  (setq sel (ssget "_CP" ptLst '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))
	  (setq j 0)
	  (repeat (sslength sel)
	    (setq n1 (ssname sel j))
	    ;Reading the cable properties
  	    (setq cableData (vlax-ldata-get (vlax-ename->vla-object n1) "cableData"))
	    (if cableData
	      	(progn
		;Get the first and last CL point
		(setq coord (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object n1) "Coordinates"))))
		;Number of route points
		(setq verCnt (/(length coord) 2))
		;Calculate the coordinates of the 1st vertex
		(setq p1 (list (nth 0 coord) (nth 1 coord) 0))
		;Calculate the coordinates of the last vertex
		(setq p2 (list (nth (- (* verCnt 2) 2) coord) (nth (- (* verCnt 2) 1) coord) 0))
			(if (In_Figure (trans p1 1 0) ptLst)			 
			  (cableSetProperty n1 "cableData" "startCable" title)
			);end if
			(if (In_Figure (trans p2 1 0) ptLst)
			 (cableSetProperty n1 "cableData" "endCable" title)
			);end if
		);end progn
	    );end if cableData
	  (setq j (1+ j))
	  );end repeat
);end cableUpdateByObject

;Single line diagram for cableObject
(defun c:cableObjectSchema( / *error* cableObjectData title p1 p2 pt n n1 n2 blk_n vla
			   ptLst i coord verCnt cableData qf_i len vvod zhil cableSech
			   isBlocks isBlock blocks myBlocks str noFoundedBlocks
			   cableList oldLayer)


  	    ;Error handler ESC
	    (defun *error* (msg)
	     (vla-endundomark active_document)
	     (princ "Error c:cableObjectSchema\n")
	     (princ msg)
	     (princ)
	     (exit)
	    ) ;_ end of defun
  
 (vla-startundomark active_document)

  ;Check for the presence of the required blocks
  ;|
  *olSchemeTable*
  *olSchemeCell* 
  *olSchemeShina*
  *olSchemeQF0*
  *olSchemeQF1*
  *olSchemePoint*
  |;
  (setq myBlocks (list *olSchemeTable* *olSchemeCell* *olSchemeShina* *olSchemeQF0* *olSchemeQF1* *olSchemePoint*))
  (setq isBlocks T)
  (setq noFoundedBlocks (list ""))
  (setq blocks (vla-get-blocks active_document))
  
  ;isBlocks
  (setq i 0)
  (repeat (length myBlocks)   
  (setq str (nth i myBlocks))
  (setq isBlock nil)
  (vlax-for el blocks (if (eq (vlax-get-property el "Name") str)(setq isBlock T)))
  (if (not isBlock)
    (progn
    (setq noFoundedBlocks (cons str noFoundedBlocks))
    (setq isBlocks nil)
    );end progn
  );end if
  (setq i (1+ i))
  );end repeat

  (if (not isBlocks)
    (progn
    (princ (strcat "Not enough blocks:\n " (LM:lst->str noFoundedBlocks "\n")))
    (exit)
    );end progn
  );end if


  
  (princ "Select CableObject\n")
  (setq sel (ssget "_:S" '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))
  (setq n (ssname sel 0))
  (setq title (cableGetProperty n "cableObjectData" "title"))
 
  ;Create an anonymous block
  (setq p1 (list 0 0 0))
  (setq blk_def (vla-add (vla-get-blocks active_document)(vlax-3d-point p1) "*U"))

  ;Inserting objects into a block
  (addEntToBlock blk_def *olSchemeTable* (list 0 0 0))

   ;Calculate all outgoing and incoming lines
    ;Zoom to full screen
    (vla-ZoomAll acad_object)
    ;Calculating cable ends inside objects
    (setq ptLst (mapcar '(lambda(x)(trans x 0 1))(mapcar 'cdr (vl-remove-if-not '(lambda(x)(= 10(car x)))(entget n)))))
    (setq sel (ssget "_CP" ptLst '((-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))
    (setq i 0)
    (setq qf_i 0)
    ;Create a list of CLs and sort it by marking
    (setq cableList (list ""))
    (repeat (sslength sel)
	    (setq n (ssname sel i))
    	    (if (vlax-ldata-get (vlax-ename->vla-object n) "cableData")
		(setq cableList (cons n cableList))
	    );end if
	    	(setq i (1+ i))
    );end repeat
    (setq cableList (cdr (reverse cableList)))
    (setq cableList (vl-sort cableList  '(lambda(n1 n2 / )
	  	(<
		   (cableGetProperty n1 "cableData" "mark")
		   (cableGetProperty n2 "cableData" "mark")
		 );end <
     )));end cableList


    	  (setq i 0)
	  (repeat (length cableList)
	    (setq n2 (nth i cableList))
	    ;Reading the cable properties
  	    (setq cableData (vlax-ldata-get (vlax-ename->vla-object n2) "cableData"))
	    (if cableData
	      	(progn
		;We get the first and last CL point
		(setq coord (vlax-safearray->list (vlax-variant-value (vlax-get-property (vlax-ename->vla-object n2) "Coordinates"))))
		;Number of route points
		(setq verCnt (/(length coord) 2))
		;Calculate the coordinates of the 1st vertex
		(setq p1 (list (nth 0 coord) (nth 1 coord) 0))
		;Calculate the coordinates of the last vertex
		(setq p2 (list (nth (- (* verCnt 2) 2) coord) (nth (- (* verCnt 2) 1) coord) 0))
			(if (In_Figure (trans p1 1 0) ptLst)
			  (progn			    
			    ;Îòõîäÿùèå àâòîìàòû
			   (addQFgroup qf_i blk_def n2 cableData '(7 8 33 50 25))
			   (setq qf_i (1+ qf_i))
			  );end progn
			);end if
			(if (In_Figure (trans p2 1 0) ptLst)
			    ;Introductory machine
			    (setq vvod n2)
			);end if
		);end progn
	    );end if cableData
	  (setq i (1+ i))
	  );end repeat
  
  ;Adding 2 more reserve groups
  (addQFgroup qf_i blk_def nil nil '(7 8 33 5 25))
  (setq qf_i (1+ qf_i))
  (addQFgroup qf_i blk_def nil nil '(7 8 33 5 25))

  ;Introductory machine
  (setq vla (vla-InsertBlock model_space (vlax-3D-point (list 65 120 0)) *olSchemeQF0* 1 1 1 0))
  (setq n1 (entlast))  
  (if vvod
    (progn
	;Reading the cable properties
        (setq cableData (vlax-ldata-get (vlax-ename->vla-object vvod) "cableData"))
        (setq cableSech (cableGetProperty vvod "cableData" "cableSech"))
        (setq zhil (car (LM:str->lst (vl-string-subst "x" "õ" (strcase cableSech T) 0)  "x")))

        (LM:setdynpropvalue vla "Distance8" 40)
        ;If there are 3 wires -the machine is single-phase
	(cond
	  ((eq zhil "3")
		(LM:setdynpropvalue vla "Distance3" 3)
	   	(LM:setdynpropvalue vla "Distance7" 7)	   	
	  )
	  (T nil)
	);end cond

      ;Input cable
      (setq pt (list 30 147 0))      
      (setq str (strcat (cableGetProperty vvod "cableData" "cableMark") " " cableSech
		  "\nL=" (rtos (getCableLength vvod cableData) 2 0) "m"  ))
      (cableAddMtext str pt 3 0 4)
      (setq n2 (entlast))
      (vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n2)) blk_def)
      (entdel n2)
      
    );end progn
  );end if
  (vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
  (entdel n1)

  ;Designation of the introductory machine
  (cableAddMtext "QF" (list 60 127 0) 3.5 0 6)
  (setq n1 (entlast))
  (vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
  (entdel n1)

  ;bus
  (setq vla (vla-InsertBlock model_space (vlax-3D-point (list 18 115 0)) *olSchemeShina* 1 1 1 0))
  ;Set the wire visibility to 3L + N + PE
  (LM:SetVisibilityState vla "3L + N + PE")
  (LM:setdynpropvalue vla "Distance1" (+ 30 (* qf_i 25)))
  (setq n1 (entlast))
  (vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
  (entdel n1)

  ;Blue outline around the diagram
  (setq points (append '(3 57 0) (list (+ 48 (* qf_i 25)) 57 0) (list (+ 48 (* qf_i 25)) 138 0) '(3 138 0)))
  (setq vla (vla-AddPolyline model_space
    (vlax-safearray-fill 
    	(vlax-make-safearray vlax-vbDouble (cons 0 (1- (length points)))) 
    	points
    )
   )
  )
  (setq n1 (entlast))
  
  (vla-put-Closed vla :vlax-true)
  (vla-put-Color vla 5)

  ;Add line type "dashed"
  (addLineType "acadiso.lin" "LINE")
  (vla-put-Linetype vla "LINE")
  (vla-put-LinetypeScale vla 0.2)
  (vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
  (entdel n1)

  ;Shield name
  (cableAddMtext title (list (+ 45 (* qf_i 25)) 130 0) 4 0 6)
  (setq n1 (entlast))
  (vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
  (entdel n1)
  

  ;cells
  (setq vla (vla-InsertBlock model_space (vlax-3D-point (list 15 0 0)) *olSchemeCell* 1 1 1 0))
  (setq n (entlast))
  (command "_arrayrect" n "" "to" 1 (1+ qf_i) "I" 25 "")
  (setq n (entlast))
  (vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n)) blk_def)
  (entdel n)
  
  
  ;Return the zoom to its original position
  (vla-ZoomPrevious acad_object)

  
  (setq p1 (list 0 0 0))
  ;Insert a temporary block
  (setq vla (vla-InsertBlock model_space (vlax-3D-point p1) (vla-get-name blk_def) 1 1 1 0))
  (setq blk_n (entlast))
  
  ;Move the block to the desired location
  	(while (or (= (car (setq gr (grread nil 5 0))) 5)
               (= (car gr) 11)
               (= (car gr) 25) ; For old version AutoCad
	       (= (car gr) 2)
               );end or
	  (cond		
	    	((= (car gr) 5)
		(setq p2 (last gr))
		 (if setpropertyvalue
		   (progn
		(setpropertyvalue blk_n "Position/X" (car p2))
		(setpropertyvalue blk_n "Position/Y" (cadr p2))
		   );end progn
		   ;(setMleaderPosition (entlast) p2)		     
		   );end if setpropertyvalue
		)		
	  );end cond
	  );end while

  
  ;Break the block
  (command "_explode" blk_n)
  ;Turn on the reactor
  (if *cableReactorsOn* (c:cableReactor))

 (vla-endundomark active_document)
 (princ)
);end defun c:cableObjectSchema

;Adding a line type
(defun addLineType (file name / linetypes lineTypeExists)
  (setq linetypes (vla-get-linetypes active_document))
  (vlax-for el linetypes (if (eq (vlax-get-property el "Name") name) (setq lineTypeExists T)))
  (if (not lineTypeExists)
  (vla-load linetypes name file)
  )
);end addLineType


;Adding a group to the diagram
(defun addQFgroup(qf_i blk_def n2 cableData rast / pt str cableSech zhil)

;Insert cable data
  (if cableData
    	(progn

;Calculate the number of cable cores
(setq cableSech (cdr (assoc "cableSech" cableData)))
(setq zhil (car (LM:str->lst (vl-string-subst "x" "õ" (strcase cableSech T) 0)  "x")))
	  
(setq pt (list (+ (* 25 qf_i) 27.5) 10 0))
(setq str (strcat (cdr (assoc "cableMark" cableData)) " " cableSech
		  "\nL=" (rtos (getCableLength n2 cableData) 2 0) "m"  ))
(cableAddMtext str pt 3 (/ pi 2) 4)
(setq n1 (entlast))
(vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
(entdel n1)

(setq pt (list (car pt) -4 0))
(setq str (cdr (assoc "mark" cableData)))
(cableAddMtext str pt 3.5 0 5)
(setq n1 (entlast))
(vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
(entdel n1)

(setq pt (list (car pt) (- (cadr pt) 50) 0))
(setq str (cdr (assoc "endCable" cableData)))
	);end progn
    (progn
     (setq pt (list (+ (* 25 qf_i) 27.5) -54 0))
     (setq str "Reserve")
    );end progn
);end if cableData
(cableAddMtext str pt 3 (/ pi 2) 5)
(setq n1 (entlast))
(vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
(entdel n1)

;automatic machines
(setq pt (list (+ (* 25 qf_i) 27.5) 82 0))
(setq vla (vla-InsertBlock model_space (vlax-3D-point pt) *olSchemeQF1* 1 1 1 0))
(setq n1 (entlast))
  
;We set the distances of the machine block
(LM:setdynpropvalue vla "Distance3" (nth 0 rast))
(LM:setdynpropvalue vla "Distance4" (nth 1 rast))
(LM:setdynpropvalue vla "Distance5" (nth 2 rast))
(LM:setdynpropvalue vla "Distance6" (nth 3 rast))
(LM:setdynpropvalue vla "Distance7" (nth 4 rast))
  
;If there are 3 wires -the machine is single-phase
(cond
  ((eq zhil "3")
	(LM:setdynpropvalue vla "Distance3" 3)
   	(LM:setdynpropvalue vla "Distance4" 10)
  )
);end cond


(vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
(entdel n1)

;Insert the machine number
(setq pt (list (-(car pt) 5) (+ (cadr pt) 9) 0))
(setq str (strcat "QF" (itoa (1+ qf_i))))
(cableAddMtext str pt 3.5 0 6)
(setq n1 (entlast))
(vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n1)) blk_def)
(entdel n1)
);end defun addQFgroup


;Add mtext
(defun cableAddMtext(str pt height ang attach)
(entmake
	(list
 	 (cons 0 "MTEXT")           ;; Entity Name
 	 (cons 100 "AcDbEntity")    ;; Subclass Marker
 	 (cons 410 "Model")         ;; Space
 	 (cons 8 "0")               ;; Layer
 	 (cons 100 "AcDbMText")     ;; Subclass Marker
 	 (cons 10 pt)         	    ;; Insertion Point
 	 (cons 71 attach)           ;; Attachment Point
 	 (cons 1 str)      	        ;; Text Content
 	 (cons 50 ang)	            ;; Rotation angle in radians
 	 (cons 40 height)		        ;; TextHeight
        (cons 7 (getvar "textstyle"))   ;; Text Style
	))
);end defun cableAddMtext

;Adding a blockNameStr block to a blk_def block
(defun addEntToBlock (blk_def blockNameStr p1 / vla n)  
  (setq vla (vla-InsertBlock model_space (vlax-3D-point p1) blockNameStr 1 1 1 0))
  (setq n (entlast))
  ;Move the block to the layer for single rulers
  ;(vla-put-Layer (vlax-ename->vla-object n) layer)
  (vlax-invoke  active_document 'copyobjects (list (vlax-ename->vla-object n)) blk_def)
  (entdel n)
  vla
);end defun addEntToBlock


; == RUS
;* àëãîðèòì âçÿò íà http://algolist.manual.ru/maths/geom/belong/poly2d.php
;* Íà îñíîâå vk_IsPointInside
;* Îïóáëèêîâàíî  http://www.arcada.com.ua/forum/viewtopic.php?p=12322
;* Boundary - ñïèñîê íîðìàëèçîâàííûõ [ò.å. òîëüêî ëèáî (X Y) ëèáî (X Y Z)] òî÷åê
; == EN
;* The algorithm is taken on http://algolist.manual.ru/maths/geom/belong/poly2d.php
;* On the basis of vk_IsPointInside
;* Posted http://www.arcada.com.ua/forum/viewtopic.php?p=12322
;* Boundary - the list normalized [i.e. only or (X Y) or (X Y Z)] points
;* Point - check point
;* Boundary - list of boundary point
(defun In_Figure (Point Boundary / FarPoint Check)
;_Tests Boundary for the condition car and last are the same point
  ;_The same point checks Boundary on a condition car and last
  (if (not (equal (car Boundary)(last Boundary) 1e-6))
    (setq Boundary (append Boundary (list(car Boundary)))))
  (setq FarPoint (cons (+ (apply 'max (mapcar 'car Boundary)) 1.0)
                       (cdr Point)
                 ) ;_ end of cons
  ) ;_ end of setq
  (or
    (not
      (zerop
        (rem
          (length
            (vl-remove
              nil
              (mapcar
                (function (lambda (p1 p2) (inters Point FarPoint p1 p2))
                ) ;_ end of function
                Boundary
                (cdr Boundary)
              ) ;_ end of mapcar
            ) ;_ end of vl-remove
          ) ;_ end of length
          2
        ) ;_ end of rem
      ) ;_ end of zerop
    ) ;_ end of not
    (vl-some (function (lambda (x) x))
             (mapcar
               (function (lambda (p1 p2)
                           (or Check
                               (if (equal (+ (distance Point p1)
                                             (distance Point p2)
                                             ) ;_ end of +
                                          (distance p1 p2)
                                          1e-3) ;_ end of equal
                                 (setq Check T) nil)
                               )
                         ) ;_ end of lambda
               ) ;_ end of function
               Boundary
               (cdr Boundary)
             ) ;_ end of mapcar
    ) ;_ end of vl-some
  ) ;_ end of or
);end defun In_Figure


;; Set Dynamic Block Visibility State  -  Lee Mac
;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
;; blk - [vla] VLA Dynamic Block Reference object
;; val - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil

(defun LM:SetVisibilityState ( blk val / vis )
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)

;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil

(defun LM:getvisibilityparametername ( blk / vis )  
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)

;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions

(defun LM:getdynpropallowedvalues ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)

;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue ( blk prp val )
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)


;; String to List  -  Lee Mac
;; Separates a string using a given delimiter
;; str - [str] String to process
;; del - [str] Delimiter by which to separate the string
;; Returns: [lst] List of strings
 
(defun LM:str->lst ( str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (LM:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
)

;; List to String  -  Lee Mac
;; Concatenates each string in a supplied list, separated by a given delimiter
;; lst - [lst] List of strings to concatenate
;; del - [str] Delimiter string to separate each item

(defun LM:lst->str ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (LM:lst->str (cdr lst) del))
        (car lst)
    )
)

;Replacing a substring in a string
;;; Usage example:
;(PL:String-Rep "Test string" "st" "*REPLACE*")
;;; "Those*REPLACEMENT*new *REPLACEMENT*rock"
; Chapter 10\Book01\PL_String-Rep.lsp
(defun PL:String-Rep (_str _old _new / _pos)
  (if (setq _pos (vl-string-search _old _str)) 
;;; If an occurrence of the required substring is detected in the string, then it is remembered 
;;; its starting position in the _pos variable.
    (strcat (substr _str 1 _pos) _new 
;;; A fragment of the original line is cut off, from its beginning to the beginning 
;;; the string you are looking for. The strcat function expects to return the following
;;; recursive call. Upon completion of this call, a fragment of the original
;;; strings, new substring and return of recursive call are concatenated
;;; in one line.
      (PL:String-Rep (substr _str (+ (strlen _old) _pos 1)) _old _new))
;;; Recursive call. The first argument is a fragment
;;; source string, starting from the end of the first occurrence of the searched substring 
;;; and until the end of the original line.
    _str) 
;;; If the required substring is not found in the original, then it returns
;;; the original line is unchanged.
)