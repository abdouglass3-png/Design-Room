;ACAD.LSP - Standard lisp function library by Arthur Douglass
;Used by internally developed lisp routines for IntuiCAD, LLC
;Functions are in alphabetical order or in logical groups
;Start with a comment, include author, rev. date (if any)
;Directory presets - The only location to reference drives & dir's.
;Use these variables in .LSP and .MNU files
(setq TMPDIR "c:/Icad2010/")
(setq DATDIR "c:/Intuicad/lsp/")
(setq DCLDIR "c:/Intuicad/lsp/")
(setq LSPDIR "c:/Intuicad/lsp/")
(setq PRGDIR "c:/Intuicad/lsp/")
(setq $$PATH 1)

; Error message handler
(defun *ERROR* (MSG) (princ (strcat "\n" MSG)) (prin1))

; Save/Restore current layer - ABD
(defun $SLAY () (setq $LAYER (getvar "CLAYER")))
(defun $RLAY ()
  (command ".LAYER" "S" $LAYER "")
  (setq $LAYER nil)
)

; AFTER returns a selection set ($SS) of all entities added to the database
; after the mark located by AFTMK - ABD
(defun AFTMK ()
  (command ".POINT" (getvar "VIEWCTR"))
  (setq $ (entget (setq $S (entlast))))
  (setq	$SS nil
	$IT (cdr (assoc -1 $))
  )
)
(defun AFTER ()
  (setq $SS (ssadd $IT))
  (setq $ $IT)
  (while $
    (progn (setq $ (entnext $))
	   (if $
	     (setq $SS (ssadd $ $SS))
	   )
    )
  )
  (ssdel $IT $SS)
  (entdel $S)
  (setq $S nil)
)

; Drops all spaces from a string - ABD
(defun DROPSP (STR)
  (setq	A ""
	B 1
  )
  (repeat (strlen STR)
    (if	(= (setq C (substr STR B 1)) " ")
      (setq C "")
    )
    (setq A (strcat A C)
	  B (1+ B)
    )
  )
  (setq STR A)
)

; Degrees <-> radians
(defun DTR (A) (* PI (/ A 180.0)))
(defun RTD (A) (/ (* A 180.0) PI))

;Get name till 1st space in string - ABD
(defun GETNAM (N / A C L)
  (setq	A ""
	L "1"
	C 1
  )
  (while (/= L "")
    (if	(not (= (setq L (substr N C 1)) " "))
      (setq A (strcat A L)
	    C (1+ C)
      )
      (setq L "")
    )
  )
  (setq N A)
  N
)

;Inches <-> mm - ABD
(defun I2M (A) (* A 25.4))
(defun M2I (A) (/ A 25.4))

;Make list from supplied datafile - ABD
(defun MAKLST (F1 / F L R)
  (setq	F (open (strcat DATDIR F1) "r")
	L nil
  )
  (while (setq R (read-line F))
    (if	(/= (substr R 1 1) "*")
      (setq L (append L (list R)))
    )
  )
  (close F)
  L
)

;Make list from supplied datafile (First word) - ABD
(defun MKFLST (F1 / F L R)
  (setq	F (open (strcat DATDIR F1) "r")
	L nil
  )
  (while (setq R (read-line F))
    (if	(/= (substr R 1 1) "*")
      (setq L (append L (list (GETNAM R))))
    )
  )
  (close F)
  L
)

; Meeting point of two lines - ABD
(defun MEET ()
  (setq	A (entget (car (entsel "\nSelect 1st line: ")))
	B (entget (car (entsel "\nSelect 2nd line: ")))
  )
  (inters (cdr (assoc 10 A))
	  (cdr (assoc 11 A))
	  (cdr (assoc 10 B))
	  (cdr (assoc 11 B))
	  nil
  )
)

; Midpoint between two points - ABD
(defun MID ()
  (setq	A (getpoint "\nMid of 1st pt: ")
	B (getpoint "\n2nd pt: " A)
  )
  (polar A (angle A B) (/ (distance A B) 2.0))
)

; Cmdecho off/on - ABD
(defun OFF () (setvar "CMDECHO" 0))
(defun ON ()
  (setvar "CMDECHO" 1)
  (setvar "HIGHLIGHT" 1)
  (princ)
)

; Reference point - ABD
(defun REF ()
  (setvar "LASTPOINT" (getpoint "Reference Pt: "))
  (getpoint "\nEnter relative/polar coordinates (with @): ")
)

; Load and run a lisp routine - ABD 
; Filename and function name must be the same
(defun RUN ($1)
  (if
    (if	(findfile (strcat PRGDIR $1 ".fas"))
      (setq A PRGDIR)
      (if (findfile (strcat LSPDIR $1 ".fas"))
	(setq A LSPDIR)
	(setq A nil)
      )
    )
     (progn
       (setq D (open (strcat TMPDIR "TEMP.SCR") "w"))
       (write-line (STRCAT "(LOAD \42" A $1 "\42)") D)
       (write-line $1 D)
       (write-line (chr 26) D)
       (close D)
       (command "SCRIPT" (strcat TMPDIR "TEMP"))
     )
     (prompt (strcat "Missing " $1 ".fas"))
  )
)

; Load and run a lisp routine - ABD - Ver.2
; Use when file and function names are not identical (1 file/multiple functions)
; Parameters:  $1 - filename,  $2 - function name
(defun RUN2 ($1 $2)
  (if
    (if	(findfile (strcat PRGDIR $1 ".fas"))
      (setq A PRGDIR)
      (if (findfile (strcat LSPDIR $1 ".fas"))
	(setq A LSPDIR)
	(setq A nil)
      )
    )
     (progn
       (setq D (open (strcat TMPDIR "TEMP.SCR") "w"))
       (write-line (STRCAT "(LOAD \42" A $1 "\42)") D)
       (write-line $2 D)
       (write-line (chr 26) D)
       (close D)
       (command "SCRIPT" (strcat TMPDIR "TEMP"))
     )
     (prompt (strcat "Missing " $1 ".fas"))
  )
)

; String to real - ABD
(defun STOR (X)
  (if (= (substr X (setq Y (+ (strlen (rtos (atoi X) 2 0)) 1)) 1)
	 "'"
      )
    (setq X (* (+ (atof X) (/ (atof (substr X (+ Y 1))) 12)) 12))
    (setq X (atof X))
  )
)

; Select object underneath another - ABD
(defun UND (/ X)
  (if (not (setq X (cadr (entsel "Pick object UNDER: "))))
    (prompt "\nNo object found.")
    (if	(= (sslength
	     (setq
	       SS (ssdel (ssname (ssget X) 0)
			 (ssget "C" (polar X 3.9 0.1) (polar X 0.8 0.1))
		  )
	     )
	   )
	   0
	)
      (prompt "\nNothing underneath.")
      (eval SS)
    )
  )
)

; Window pick layer - ABD
(defun WPL ()
  (setq	P (getpoint "\n1st Pt:")
	Q (getcorner P "\n2nd Pt:")
	L (cdr (assoc 8 (entget (car (entsel "\nPick a Layer:")))))
	S (ssget "W" P Q)
	M (sslength S)
	C 0
	R (ssadd)
  )
  (while (< C M)
    (if	(eq (cdr (assoc 8 (entget (ssname S C)))) L)
      (ssadd (ssname S C) R)
    )
    (setq C (1+ C))
  )
  (setq R R)
)

; Default variable presets used by other functions 
(setq TIMSAV (getvar "DATE")
      TXSIZE (getvar "TEXTSIZE")
      BSCL   (setq SCL (getvar "DIMSCALE"))
      TXROTA "0"
      ROTA   "0"
)

;* UKWORD User key word. DEF, if any, must match one of the KWD strings
;* BIT (1 for no null, 0 for none) and KWD key word ("" for none) are same as 
;* for INITGET. MSG is the prompt string, to which a default string is added as
;* <DEF> (nil or "" for none), and a : is added. 
;*
(defun ukword (bit kwd msg def / inp)
  (if (and def (/= def ""))		;test for both nil and null string
    (setq msg (strcat "\n" msg "<" def ">: ") ;string'em  with default
	  bit (* 2 (fix (/ bit 2)))	;a default and no null bit code conflict so
    )					;setq                          ;this reduces bit by 1 if odd, to allow null
    (if	(= " " (substr msg (strlen msg) 1))
					;no def, if last char is space
      (setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
					;then strip space
      (setq msg (strcat "\n" msg ": "))	;else msg OK
    )
  )					;if,if
  (initget bit kwd)			;initialize the key words
  (setq inp (getkword msg))		;and use the GET command
  (if inp
    inp
    def
  )					;compare the results, return appropriate value
)					;defun

;* USTR User interface string
;* If BIT=1 no null "" input allowed, 0 for none, BIT ignored if DEF present.
;* MSG is the prompt string, to which a default string is added as <DEF> (nil
;* or "" for none), and a : is added. If SPFLAG T, spaces are allowed in string.
;*
(defun ustr (bit msg def spflag / inp nval)
  (if (and def (/= def ""))		;test for both nil and null string
    (setq msg (strcat "\n" msg "<" def ">: ")
;then include the default string
	  inp (getstring msg spflag)	;get input, ignore no null bit
	  inp (if (= inp "")
		def
		inp
	      )				;if null input, return default
    )					;setq
    (progn
      (if (= " " (substr msg (strlen msg) 1))
					;no def, if last char is space
	(setq msg (strcat "\n" (substr msg 1 (1- (strlen msg))) ": "))
					;then strip space
	(setq msg (strcat "\n" msg ": ")) ;else msg OK
      )					;if
      (if (= bit 1)			;if no null bit set to 1
	(while (= "" (setq inp (getstring msg spflag)))
					;then get input, no ""
	  (prompt "\nInvalid string.")
	)
	(setq inp (getstring msg spflag)) ;else get input, "" ok
      )
    )					;progn&if
  )					;if
  inp
)					;defun


					; Command macros
					; Try to use the (RUN "???") functions to keep this file small
(setvar "modemacro" "...because it's definitely about time!")
(setvar "mtjigstring" "SCM")
(setvar "hpdraworder" 1)
(setvar "visretain" 1)
(defun C:BF () (command ".BREAK" PAUSE "F")(prin1))
(defun C:BY () (RUN "BY"))
(defun C:DD () (RUN "DD"))
(defun C:HL () (RUN "HL"))
(defun C:HT () (RUN "HT"))
(defun C:LC () (RUN "LC"))
(defun C:LDP () (RUN "LDP"))
(defun C:LF () (RUN "LF"))
(defun C:LIS () (RUN "LIS"))
(defun C:LM () (RUN "LM"))
(defun C:LS () (RUN "LS"))
(defun C:LX () (RUN "LX"))
(defun C:ML () (RUN "ML"))
(defun C:OO () (command "OSNAP" "NONE"))
(defun C:TXCON () (RUN "TXCON"))
(defun C:TXM () (RUN "TXM"))
(defun C:TXSWAP () (RUN "TXSWAP"))
(defun C:ZA () (command "ZOOM" "A"))
(defun C:ZE () (command "ZOOM" "E"))
(defun C:ZO () (RUN2 "ZOOMS" "ZO"))
(defun C:ZP () (command "ZOOM" "P"))
(defun C:Y () (RUN (GETSTRING "Lisp to load: ")))
(prompt "s ")
