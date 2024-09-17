;Sort table by column
(defun C:tableSort (/	    ent	    vla_table	    headRows
		    list_row	    row0    col0    row	    col
		    rows    i	    str
		   )
  (vl-load-com)
  (setq acad_object (vlax-get-acad-object))
  (setq active_document (vla-get-ActiveDocument acad_object))
  (setq model_space (vla-get-ModelSpace active_document))
  (vla-startundomark active_document)

	;Choose lines to ignore (header, title)
  (setq headRows nil)
  (while (setq ent (entsel "Select all ignored lines (Title, Header)\n"))
    (setq vla_table (vlax-ename->vla-object (car ent)))
    (vla-HitTest
      vla_table
      (vlax-3d-point (cadr ent))
      (vlax-3d-point (trans (getvar 'ViewDir) 1 0))
      'row0
      'col0
    )

    (if	row0
      (if (not (member row0 headRows))
        (progn
        (if (eq row0 -1)
          (setq row0 0)
        )
        (if (not headRows)
          (setq headRows (list row0))
          (setq headRows (append headRows (list row0)))
        )				;end if		  
        (princ (strcat "Line " (itoa row0) "added to header\n")
        )
        (princ "Rows selected")
        (setq headRows (vl-sort headRows '<))
        (princ headRows)
        (princ "\n")
      )				;end progn
      (progn
        (princ (strcat "Line " (itoa row0) "already selected\n"))
        (princ "Rows selected")
        (setq headRows (vl-sort headRows '<))
        (princ headRows)
        (princ "\n")
      )				;end progn
      )					;end if not member
    )					;end if row0    
  )					;end while

  (setq ent (entsel "Select a column to sort"))
  (setq vla_table (vlax-ename->vla-object (car ent)))
  (vla-HitTest
    vla_table
    (vlax-3d-point (cadr ent))
    (vlax-3d-point (trans (getvar 'ViewDir) 1 0))
    'row0
    'col0
  )

  (setq rows (vla-get-Rows vla_table))
  (setq cols (vla-get-Columns vla_table))
  (setq row 0)

  (setq list_table nil)

  (while (< row rows)
    (if	(member row headRows)
      (progn
	(setq row (1+ row))
      )					;end progn
      (progn
	(setq col 0)
	(setq list_row nil)
	(while (< col cols)
	  (if (eq list_row nil)
	    (setq list_row (list (vla-GetText vla_table row col)))
	    (setq
	      list_row (cons (vla-GetText vla_table row col) list_row)
	    )
	  )				;end if
	  (setq col (1+ col))
	)				;end while cols
	(setq list_row (reverse list_row))
	(if (eq list_table nil)
	  (setq list_table (list list_row))
	  (setq list_table (cons list_row list_table))
	)				;end if
	(setq row (1+ row))
      )					;end progn
    )					;end if
  )					;end while rows
  (setq list_table (reverse list_table))

  ;Sort the list
	(setq sorted_table (vl-sort list_table
			     '(lambda (row1 row2 / str1 str2)
				(setq str1 (nth col0 row1))
				(setq str2 (nth col0 row2))
				(if (eq last_sort ">")
				(< str1 str2)
				(> str1 str2)
				  )
			      );end lambda
	);end vl-sort
	)

  ;Change the sorting direction
  (if (eq last_sort ">")(setq last_sort "<")(setq last_sort ">"))

  ;We interrupt the table in accordance with the sorted list
  (vla-put-RegenerateTableSuppressed vla_table :vlax-true) 
  (setq row (length headRows))
  (while (< row rows)
	(setq col 0)
	(setq list_row nil)
	(while (< col cols)
	  (setq str (nth col (nth (- row (length headRows)) sorted_table)))
	  (vla-setText vla_table row  col str)
	  (setq col (1+ col))
	)				;end while cols					
					;end if
	(setq row (1+ row))
  )

  (vla-put-RegenerateTableSuppressed vla_table :vlax-false) 
  
);end defun