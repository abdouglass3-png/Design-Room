;Created by Arthur B Douglass III exclusively for IntuiCAD, LLC
;09-17-2009 All rights reserved.

;;;IntuiCAD Electrical Menu block insertion routine
;;Main function--obtains insertion point and current dimscale value
(defun c:scmmenu (#TIT / p1 scx tx1 sht oldAttdia)
  (setq scl (getvar "dimscale"))
  (setq p1 (getpoint "\nInsertion point: "))

  ;;Ground Bar--Layer
  (if (= #TIT "gndbar")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "gndbar" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Ground Cross--Layer
  (if (= #TIT "gndcrss")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "gndcrss" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Ground Pigtail--Layer
  (if (= #TIT "gndpgtl")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "gndpgtl" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Ground Tee--Layer
  (if (= #TIT "gndtee")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "gndtee" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Ground Triad--Layer
  (if (= #TIT "gndtri")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "gndtri" P1 SCL "" "0")
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Ground Weld--Layer
  (if (= #TIT "gndwld")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "gndwld" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Ground Tee to Pigtail--Layer
  (if (= #TIT "pgtltee")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "pgtltee" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Ground Rod to Column--Layer
  (if (= #TIT "rd2clmn")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "rd2clmn" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Ground Tee to Column--Layer
  (if (= #TIT "tee2clmn")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "tee2clmn" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Heat Trace 90 Bend--Layer
  (if (= #TIT "htbnd")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "htbnd" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Heat Trace Circuit Tag--Layer, Color, Supress Attributes
  (if (= #TIT "htckt")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "htckt" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Heat Trace Terminator--Layer, Color
  (if (= #TIT "htend")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "htend" P1 SCL "" PAUSE)
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "3" "")
      ) ;_ end of if
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Heat Trace Pump--Layer, Color
  (if (= #TIT "htend")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "htpmp" P1 SCL "" PAUSE)
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "3" "")
      ) ;_ end of if
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Heat Trace Panel--Layer, Color, Supress Attributes
  (if (= #TIT "htpnl")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "htpnl" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Heat Trace Controller--Layer, Color
  (if (= #TIT "htstrt")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "htstrt" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "3" "")
      ) ;_ end of if
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Heat Trace Tee Connection--Layer, Color
  (if (= #TIT "httee")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "httee" P1 SCL "" PAUSE)
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Heat Trace Valve--Layer, Color
  (if (= #TIT "htvlv")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "htstrt" P1 SCL "" PAUSE)
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "3" "")
      ) ;_ end of if
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Heat Trace Wall--Layer, Color
  (if (= #TIT "htwall")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command "-layer" "s" "E-HT" "")
      ) ;_ end of if
      (command ".insert" "htwall" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-HT") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;2x2 Flourescent--Layer, Color, Supress Attributes
  (if (= #TIT "2x2flo")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "2x2flo" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;2x4 Flourescent--Layer, Color, Supress Attributes
  (if (= #TIT "2x4flo")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "2x4flo" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Can Light--Layer, Color, Supress Attributes
  (if (= #TIT "can")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "can" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Emergency Light--Layer, Color, Supress Attributes
  (if (= #TIT "emer")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "emer" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Round LED--Layer, Color, Supress Attributes
  (if (= #TIT "ledrnd")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "ledrnd" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Pole Flood Light--Layer, Color, Supress Attributes
  (if (= #TIT "fldpole")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "fldpole" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Striplight--Layer, Color, Supress Attributes
  (if (= #TIT "strp")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "strp" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Switch--Layer, Color, Supress Attributes
  (if (= #TIT "swtch")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "swtch" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Wall FLood Light--Layer, Color, Supress Attributes
  (if (= #TIT "fldwll")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "fldwll" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Wallpack--Layer, Color, Supress Attributes
  (if (= #TIT "wllpck")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command "-layer" "s" "E-Lighting" "")
      ) ;_ end of if
      (command ".insert" "wllpck" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Lighting") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Cable Tray Bend--Layer
  (if (= #TIT "ctbnd")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-CableTray") NIL)
	    (command "-layer" "s" "E-CableTray" "")
      ) ;_ end of if
      (command ".insert" "ctbnd" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Cable Tray Cross--Layer
  (if (= #TIT "ctcrss")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-CableTray") NIL)
	    (command "-layer" "s" "E-CableTray" "")
      ) ;_ end of if
      (command ".insert" "ctcrss" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Cable Tray Tee--Layer
  (if (= #TIT "ctbnd")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-CableTray") NIL)
	    (command "-layer" "s" "E-CableTray" "")
      ) ;_ end of if
      (command ".insert" "cttee" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Vertical Cable Tray--Layer
  (if (= #TIT "ctvrt")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-CableTray") NIL)
	    (command "-layer" "s" "E-CableTray" "")
      ) ;_ end of if
      (command ".insert" "ctvrt" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Combo Starter--Layer, Color
  (if (= #TIT "cmbstrtr")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command "-layer" "s" "E-Device" "")
      ) ;_ end of if
      (command ".insert" "cmbstrtr" P1 SCL "" PAUSE)
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Conduit Seal--Layer
  (if (= #TIT "conseal")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command "-layer" "s" "E-Device" "")
      ) ;_ end of if
      (command ".insert" "conseal" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Conduit Tee--Layer
  (if (= #TIT "contee")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command "-layer" "s" "E-Device" "")
      ) ;_ end of if
      (command ".insert" "contee" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Disconnect--Layer, Color, Supress Attributes
  (if (= #TIT "dscnct")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command "-layer" "s" "E-Device" "")
      ) ;_ end of if
      (command ".insert" "dscnct" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;Flex Tube--Layer
  (if (= #TIT "flxtb")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command "-layer" "s" "E-Device" "")
      ) ;_ end of if
      (command ".insert" "flxtb" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Horizontal Motor--Layer
  (if (= #TIT "mtrhrz")
    (progn
      (setq ULAYER (getvar "clayer"))
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command "-layer" "s" "E-Device" "")
      ) ;_ end of if
      (command ".insert" "mtrhrz" P1 SCL "" PAUSE)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if

  ;;Receptacle--Layer, Color, Supress Attributes
  (if (= #TIT "rcptcl")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command "-layer" "s" "E-Device" "")
      ) ;_ end of if
      (command ".insert" "rcptcl" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Device") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Frame Continuation--Layer, Color, Supress Attributes
  (if (= #TIT "mcccont")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "rcptcl" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Elevation--Layer, Color, Supress Attributes
  (if (= #TIT "mccelev")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "mccelev" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Frame--Layer, Color, Supress Attributes
  (if (= #TIT "mccfrm")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "mccfrm" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Protective Device--Layer, Color, Supress Attributes
  (if (= #TIT "mccpd")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "mccpd" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Motor Controls--Layer, Color, Supress Attributes
  (if (= #TIT "mtrctrl")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "mtrctrl" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Disconnect--Layer, Color, Supress Attributes
  (if (= #TIT "mcccont")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "rcptcl" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Motor--Layer, Color, Supress Attributes
  (if (= #TIT "slmtr")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "slmtr" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Panel--Layer, Color, Supress Attributes
  (if (= #TIT "slpnl")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "slpnl" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Starter/VFD--Layer, Color, Supress Attributes
  (if (= #TIT "slstrtr")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "slstrtr" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Welding Outlet--Layer, Color, Supress Attributes
  (if (= #TIT "slwlddsc")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "slwlddsc" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Transformer--Layer, Color, Supress Attributes
  (if (= #TIT "slxfmr")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "slxfmr" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  ;;MCC Wall Starter--Layer, Color, Supress Attributes
  (if (= #TIT "wllstrtr")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command "-layer" "s" "E-Equip" "")
      ) ;_ end of if
      (command ".insert" "wllstrtr" P1 SCL "" "0")
      (if (/= (tblsearch "layer" "E-Equip") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
  
  
  
  
  ;;TEMPLATE
  (if (= #TIT "gndbar")
    (progn
      (setq ULAYER (getvar "clayer"))
      (setq OLDATTREQ (getvar "attreq"))
      (setq OLDATTDIA (getvar "attdia"))
      (setvar "attdia" 0)
      (setvar "attreq" 0)
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command "-layer" "s" "E-Ground" "")
      ) ;_ end of if
      (command ".insert" "scut" P1 SCL "" PAUSE)
      (if (/= (tblsearch "layer" "E-Ground") NIL)
	    (command ".CHANGE" "l" "" "P" "C" "2" "")
      ) ;_ end of if
      (setvar "attdia" OLDATTDIA)
      (setvar "attreq" OLDATTREQ)
      (setvar "clayer" ULAYER)
    ) ;_ end of progn
  ) ;_ end of if
  
) ;_ end of program