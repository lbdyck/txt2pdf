        /* --------------------  rexx procedure  -------------------- */
         ver = "1.16"
        /* Name:      txt2pdfi                                        *
         *                                                            *
         * Function:  ISPF Front-End for TXT2PDF                      *
         *                                                            *
         * Syntax:    %txt2pdfi option                                *
         *                                                            *
         *            The only supported option at this point is      *
         *            CONFIG_ONLY (abbreviated CONFIG) which will     *
         *            be used to create a configuration file only.    *
         *                                                            *
         * Author:    Lionel B. Dyck                                  *
         *            Kaiser Permanente Information Technology        *
         *            Walnut Creek, CA 94598                          *
         *            (925) 926-5332                                  *
         *            Internet: lionel.b.dyck@kp.org                  *
         *                                                            *
         * License:   This EXEC and related components are released   *
         *            under terms of the GPLV3 License. Please        *
         *            refer to the LICENSE file for more information. *
         *            Or for the latest license text go to:           *
         *                                                            *
         *              http://www.gnu.org/licenses/                  *
         *                                                            *
         * History:                                                   *
         *         2008-09-04 v1.16 - added missing "otherwise"       *
         *                          - add COND code                   *
         *                          - add Format PDF to XMITIP option *
         *                          - flexibile generation of JCL     *
         *                               find *customize*             *
         *         2004-06-07 v1.15 - Correct STEPLIB dsn             *
         *         2003-08-25 v1.14 - Add OUTREC support              *
         *                          - Add DINFO support               *
         *         2003-08-04 v1.13 - Add MAXOSIZE support            *
         *         2003-08-02 v1.12 - Remove OUT from saved config    *
         *         2003-05-13 v1.11 - Change //HOLD to Default=N      *
         *         2002-10-31 v1.10 - Fix duplicate %txt2pdf (sigh)   *
         *         2002-10-22 v1.9  - Correct issue of line too long  *
         *         2002-10-08 v1.8  - Add Entry of CONFIG_ONLY        *
         *         2002-10-07 v1.7  - Add Compression option          *
         *                            arrived with txt2pdf v2.279     *
         *         2002-09-18 v1.6a - Add GreenBar (missed before)    *
         *         2002-09-17 v1.6  - move init of c value            *
         *         2002-09-16 v1.5  - removed mhlq option as unused   *
         *         2002-09-16 v1.4  - fix missing sysexec and jcl     *
         *                            hlq                             *
         *         2002-09-16 v1.3  - clean up jcl                    *
         *                          - add popup prompt if output d/s  *
         *                            already exists                  *
         *         2002-09-12 v1.2  - fix rc test for qlibdef         *
         *         2002-09-12 v1.1  - Correct missing EXEC            *
         *                            (thx Ugur Cilesiz)              *
         *                          - correct get_dsn for ispllib     *
         *                            if libdef'd                     *
         *         2002-09-11 v1.0  - Declared production ready       *
         *         2002-09-09 v0.9  - Pickup defaults from txt2pdfd   *
         *         2002-09-08 v0.8  - Minor cleanup                   *
         *         2002-09-06 v0.7  - Add STEPLIB generation if the   *
         *                            T2PINIT is in ISPLLIB for       *
         *                            the batch jcl.                  *
         *         2002-09-04 v0.6  - Add check for input and output  *
         *                            datasets                        *
         *         2002-09-04 v0.5  - Add BG (Textmark only) support  *
         *         2002-09-03 v0.4  - Don't allow *bar colors for text*
         *         2002-09-01 v0.3  - Minor cleanup                   *
         *         2002-08-30 v0.2  - Add more features/cleanup       *
         *         2002-08-29 v0.1  - Creation                        *
         *                                                            *
         * ---------------------------------------------------------- */

        /* --------------------------- *
         * Pickup any passed paramters *
         * --------------------------- */
         arg options

        /* --------------------------------------------------------- *
         * Setup ISPF Addressing                                     *
         * --------------------------------------------------------- */
        Address ISPEXEC

        /* --------------------------------------------------------- *
         * Set ISPF Message Alarm and other defaults                 *
         * --------------------------------------------------------- */
         zerralrm = "YES"
         "Vput (Zerralrm) Shared"
         parse value "" with zerrtype zerrlm null input output ,
                             co cofile
         wdd = "T2P"random(9999)

        /* ----------------------------------------------------- *
         * Test for ISPF Applid of T2P and if not then recurse   *
         * with that Applid.                                     *
         * ----------------------------------------------------- */
        "Control Errors Return"
        "VGET ZAPPLID"
          if zapplid <> "T2P" then do
             "Select CMD(%"sysvar('sysicmd') options ") Newappl(T2P)" ,
                 "passlib scrname(T2PIP)"
             x_rc = rc
             if x_rc > 4 then do
                zerrsm = zerrtype
                zerrlm = zerrlm
               "Setmsg Msg(isrz002)"
                end
             Exit 0
             end

        /* ---------------------------- *
         * Pickup Installation Defaults *
         * ---------------------------- */
         parse value txt2pdfd() with ,
               "UNITS"   unit_of_measure,
               "PAPER"   paper_size"/"paper_type"/"paper_style,
               "ORIENT"  text_orientation,
               "LPI"     text_lines_per_inch,
               "LM"      left_margin,
               "RM"      right_margin,
               "TM"      top_margin,
               "BM"      bottom_margin,
               "FONT"    font_size"/"font_name"/"font_zoom,
               "COLOR"   foreground_color"/"background_color,
               "BROWSE"  browse,
               "CONFIRM" confirm,
               "MSGID"   message_id

         "Vget (psize ptype pstyle morient mlpi" ,
               "mtm mbm mrm mlm" ,
               "fsize fname fzoom maxosize morec" ,
               "mconf cf cb) Profile"

         if fsize    = null then fsize   = font_size
         if fname    = null then fname   = font_name
         if fzoom    = null then fzoom   = font_zoom
         if psize    = null then psize   = paper_size
         if ptype    = null then psize   = paper_type
         if pstyle   = null then psize   = paper_style
         if morient  = null then morient = text_orientation
         if mlpi     = null then mlpi    = text_lines_per_inch
         if mtm      = null then mtm     = top_margin
         if mbm      = null then mbm     = bottom_margin
         if mlm      = null then mlm     = left_margin
         if mrm      = null then mrm     = right_margin
         if cf       = null then cf      = foreground_color
         if cb       = null then cb      = background_color
         if mconf    = null then mconf   = confirm
         units = strip(unit_of_measure)
         if left(units,1) = "I"
            then units = "Inches"
         if left(units,1) = "C"
            then units = "Centimeters"

        /* ------------------------------- *
         * Process any provided parameters *
         * ------------------------------- */
         Select
           When abbrev("CONFIG_ONLY",options,6) then
                co = 1
           Otherwise nop
           end

         if co = null then panel = "T2P"
                      else panel = "T2PCO"

        /* ----------------------------------------- *
         * Now begin the actual Front-end Processing *
         * ----------------------------------------- */
         Start:
         do forever
            "Display Panel("panel")"
            if rc > 3 then exit
            if co = null then
               "Vput (input output annot margin color misc encrypt",
                     "outline backgrnd dinfo font page valid) Profile"
            else
               "Vput (cofile annot margin color misc encrypt",
                     "outline backgrnd dinfo font page valid) Profile"

           /* -------------------------------- *
            * Verify input and output datasets *
            * -------------------------------- */
            if co = null then do
               if sysdsn(input) <> "OK" then do
                  zerrsm = "Error"
                  zerrlm = "Input dataset" input ,
                           "error:" sysdsn(input)
                 "Setmsg msg(isrz002)"
                 signal start
                 end
               if sysdsn(output) = "OK" then do
                  "AddPop"
                  "Display Panel(t2ppu)"
                  x_rc = rc
                  "RemPop"
                  if x_rc > 0 then signal start
                  if t2prep <> "Yes" then signal start
                  end
               end

           /* ---------------------------------- *
            * Setup the inital command variable. *
            * ---------------------------------- */
            parse value "" with cmd. cmd config_cards.
            cmd_cnt    = 0
            config_cnt = 0
            call add_cmd "%txt2pdf BROWSE Y IN" input
            call add_cmd "OUT" output
            if valid <> null then do
               call add_cmd "VOnly Y"
               end

           /* ------------------------------ *
            * Now Call Each Selected Process *
            * ------------------------------ */
            if annot    <> null then call do_annot
            if backgrnd <> null then call do_backgrnd
            if color    <> null then call do_color
            if dinfo    <> null then call do_dinfo
            if encrypt  <> null then call do_encrypt
            if font     <> null then call do_font
            if misc     <> null then call do_misc
            if outline  <> null then call do_outline
            if page     <> null then call do_page
            cmd.0       =  cmd_cnt
            Address TSO
            call build_txt2pdf
            Address ISPExec
            end
            return

        /* -------------------- *
         * Build Up the Command *
         * -------------------- */
         Add_Cmd:
           parse arg lcmd
           cmd.cmd_cnt = cmd.cmd_cnt "+"
           cmd_cnt = cmd_cnt + 1
           cmd.cmd_cnt = lcmd
           cmd = cmd lcmd
           if left(lcmd,1) = "%" then do
              config_cnt = config_cnt + 1
              config_cards.config_cnt = "* TXT2PDF Configuration" ,
                    "File created on" date() time() "by %txt2pdfi"
              return
              end
           if left(lcmd,4) = "OUT " then return
           config_cnt = config_cnt + 1
           config_cards.config_cnt = lcmd
           config_cards.0 = config_cnt
           return

        /* ------------------- *
         * Process Annotations *
         * ------------------- */
         Do_Annot:
         "Display Panel(T2p1)"
         if rc > 3 then signal Start
         wcmd = "TEXT/"apage"/"arow"/"acol"/"astate"/"
         atext1= strip(atext1)
         atext2= strip(atext2)
         atext3= strip(atext3)
         if length(atext2) + length(atext3) > 0 then do
            atext = '"'wcmd
            call add_cmd "ANNOT "atext" -"
            call add_cmd atext1" -"
            if length(atext3) > 0 then do
               call add_cmd atext2" -"
               call add_cmd atext3'"'
               end
               else call add_cmd atext2'"'
            end
         else do
              if pos(" ",atext1) > 0 then do
                 call add_cmd 'ANNOT "'wcmd" -"
                 call add_cmd atext1'"'
                 end
               else do
                 call add_cmd 'ANNOT' wcmd" -"
                 call add_cmd atext1
                 end
              end
         "Vput (apage arow acol astate atext1 atext2 atext3) Profile"
         return

        /* ------------------------- *
         * Process Color Information *
         * ------------------------- */
         Do_Color:
         "Display Panel(T2p2)"
         if rc > 3 then signal Start
         call fix_color cf
         cf = new_color
         call fix_color cb
         cb = new_color
         if pos("BAR",translate(cf)) > 0 then
            cf = "*"cf
         if pos("*",cf cb) > 0 then do
            zerrsm = "Error"
            zerrlm = "You specified an invalid color. Try again."
            "Setmsg msg(isrz002)"
            signal do_color
            end
         call add_cmd "Color" cf"/"cb
         "Vput (cf cb) Profile"
         return

        /* ------------------------------ *
         * Process BackGround Information *
         * ------------------------------ */
         Do_BackGrnd:
         "Display Panel(T2p8)"
         if rc > 3 then signal Start
         call fix_color btc
         btc  = new_color
         call fix_color bfc
         bfc  = new_color
         if pos("BAR",translate(btc)) > 0 then
            btc = "*"btc
         if pos("BAR",translate(bfc)) > 0 then
            bfc = "*"bfc
         if pos("*",btc bfc) > 0 then do
            zerrsm = "Error"
            zerrlm = "You specified an invalid color. Try again."
            "Setmsg msg(isrz002)"
            signal do_backgrnd
            end
         wcmd = "Textmark/"bgs"/"btc"/"bfc"/"bfo"/"btext
         if pos(" ",btext) > 0 then
            wcmd = '"'wcmd'"'
         call add_cmd "BG" wcmd
         "Vput (bgs btc bfc bfo btext) Profile"
         return

        /* ---------------------------- *
         * Process Document Inforamtion *
         * ---------------------------- */
         Do_Dinfo:
         if output <> null then do
            parse value output with left".PDF"right
            dinfods = left".PDFINFO"right
            end
          else if cofile <> null then do
               if left(cofile,1) = "'" then
                  dinfods = substr(cofile,2,length(cofile)-2)
               else
                  dinfods = cofile
               dinfods = dinfods".PDFINFO"
               if left(cofile,1) = "'" then
                  dinfods = "'"dinfods"'"
               end
         if sysdsn(dinfods) = "OK" then do
            Address TSO
            "Alloc f("wdd") ds("dinfods") shr"
            "Execio * diskr" wdd "(finis stem dip."
            "Free  f("wdd")"
            Address ISPExec
              zerrsm = "DocInfo Loaded"
              zerrlm = "Document Information Loaded from existing file."
             "Setmsg Msg(isrz002)"
            do dic = 1 to dip.0
               if translate(word(dip.dic,1)) = "TITLE" then
                  dititle = subword(dip.dic,2)
               if translate(word(dip.dic,1)) = "SUBJECT" then
                  disubj = subword(dip.dic,2)
               if translate(word(dip.dic,1)) = "AUTHOR" then
                  diauthor = subword(dip.dic,2)
               if translate(word(dip.dic,1)) = "KEYWORDS" then
                  dikeywd = subword(dip.dic,2)
              end
            end
         "Display Panel(T2p9)"
         if rc > 3 then signal Start
         drop di.
         di.1 = "Title" dititle
         di.2 = "Subject" disubj
         di.3 = "Author" diauthor
         di.4 = "Keywords" dikeywd
         "Vput (dititle disubj diauthor dikeywd) Profile"
         Address TSO
         if sysdsn(dinfods) = "OK" then
            "Alloc f("wdd") ds("dinfods") shr"
         else
            "Alloc f("wdd") ds("dinfods") new spa(1,1) tr",
                  "Recfm(f b) lrecl(80) blksize(6160)"
         "Execio * diskw" wdd "(finis stem di."
         "Free  f("wdd")"
         Address ISPExec
           zerrsm = "DocInfo Saved"
           zerrlm = "Document Information has been saved in:" dinfods
          "Setmsg Msg(isrz002)"
         call add_cmd "DINFO" dinfods
         return

        /* ------------------------------ *
         * Process Encryption Information *
         * ------------------------------ */
         Do_Encrypt:
         "Display Panel(T2p3)"
         if rc > 3 then signal Start
         wcmd = "ENCRYPT ST/"eowner"/"euser"/"elen
         if enp  <> null then wcmd = wcmd"/NP"
         if ene  <> null then wcmd = wcmd"/NE"
         if enc  <> null then wcmd = wcmd"/NC"
         if enen <> null then wcmd = wcmd"/NEN"
         if ensa <> null then wcmd = wcmd"/NSA"
         if enfs <> null then wcmd = wcmd"/NFS"
         if enas <> null then wcmd = wcmd"/NAS"
         if enhp <> null then wcmd = wcmd"/NHP"
         call add_cmd wcmd
         "Vput (eowner euser elen enp ene enc",
               "enen ensa enfa enas enhp) Profile"
         return

        /* ------------------------ *
         * Process Font Information *
         * ------------------------ */
         Do_Font:
         "Display Panel(T2p4)"
         if rc > 3 then signal Start
         call add_cmd "FONT" fsize"/"fname"/"fzoom
         "Vput (fsize fname fzoom) Profile"
         return

        /* --------------------------------- *
         * Process Miscellaneous Information *
         * --------------------------------- */
         Do_Misc:
         "Display Panel(T2p5)"
         if rc > 3 then signal Start
         if mcc      <> null then call add_cmd "CC" mcc
         if mcp      <> null then call add_cmd "COMPRESS" mcp
         if mconf    <> null then call add_cmd "CONFIRM" mconf
         if maxosize <> null then
            if maxosize > 0 then
               call add_cmd "MAXOSIZE" maxosize
         if morec    <> null then
            if morec > 0 then
               call add_cmd "OUTREC" morec
         "Vput (mcc mcp mconf maxosize morec) Profile"
         return

        /* --------------------------- *
         * Process Outline Information *
         * --------------------------- */
         Do_Outline:
         "Display Panel(T2p6)"
         if rc > 3 then signal Start
         wcmd = "OUTLINE"
         Select
           When ost = 1 then do
                wcmd = wcmd "RC/"orow"/"ocol"/"olen
                end
           When ost = 2 then do
                if pos(" ",otext) > 0
                then
                   wcmd = wcmd '"SR/'orow"/"ocol"/"olen"/"otext'"'
                else
                   wcmd = wcmd "SR/"orow"/"ocol"/"olen"/"otext
                end
           When ost = 3 then do
                if pos(" ",otext) > 0
                then
                   wcmd = wcmd '"SC/'ocol"/"olen"/"otext'"'
                else
                   wcmd = wcmd "SC/"ocol"/"olen"/"otext
                end
           When ost = 4 then do
                if pos(" ",otext) > 0
                then
                   wcmd = wcmd '"S/'ocol"/"olen"/"otext'"'
                else
                   wcmd = wcmd "S/"ocol"/"olen"/"otext
                end
           Otherwise nop
           end
         call add_cmd wcmd
         "Vput (ost orow ocol olen otext) Profile"

        /* ------------------------------------- *
         * Now process the Outline Options Panel *
         * ------------------------------------- */
         Do_Out2:
         "Display Panel(T2p6A)"
         if rc > 3 then signal Start
         if length(olsort) > 0 then
            call add_cmd "OLSort" olsort
         if length(oltoken) > 0 then do
            if pos(" ",oltoken) > 0 then
               oltok = '"'oltoken'"'
               else oltok = oltoken
            call add_cmd "OLTOK" oltok
            end
         wcmd = null
         if length(olcol) > 0 then do
            call fix_color olcol
            olcol = new_color
            if pos("*",olcol) > 0 then do
               zerrsm = "Error"
               zerrlm = "You specified an invalid color. Try again."
               "Setmsg msg(isrz002)"
               signal do_out2
               end
            wcmd = strip(wcmd"Color/"olcol"/")
            end
         if length(olalt) > 0 then do
            call fix_color olalt
            olalt = new_color
            if pos("*",olalt) > 0 then do
               zerrsm = "Error"
               zerrlm = "You specified an invalid alternate" ,
                        "color. Try again."
               "Setmsg msg(isrz002)"
               signal do_out2
               end
            wcmd = strip(wcmd"AltColor/"olalt"/")
            end
         if olbold <> null then
            wcmd = strip(wcmd"Bold/")
         if olfull <> null then
            wcmd = strip(wcmd"Full/")
         if oldup  <> null then
            wcmd = strip(wcmd"Dups/")
         if olit  <> null then
            wcmd = strip(wcmd"Italic/")
         if ole   <> null then
            wcmd = strip(wcmd"Expand/")
         if ols   <> null then
            wcmd = strip(wcmd"Show/")
         if length(wcmd) > 0 then do
            wlen = length(wcmd)
            wcmd = left(wcmd,wlen-1)
            wcmd = "OLOPT" wcmd
            call add_cmd wcmd
            end
         "Vput (olcol olalt olsort oltoken olbold olfull",
               "oldup olit ole ols) Profile"
         return

        /* ---------------------------------- *
         * Process Page and Paper Information *
         * ---------------------------------- */
         Do_Page:
         "Display Panel(T2p7)"
         if rc > 3 then signal Start
         if mlpi    <> null then call add_cmd "LPI" mlpi
         wcmd = null
         if mtm <> null then wcmd = strip(wcmd "TM" mtm)
         if mbm <> null then wcmd = strip(wcmd "BM" mbm)
         if mlm <> null then wcmd = strip(wcmd "LM" mlm)
         if mrm <> null then wcmd = strip(wcmd "RM" mrm)
         if length(wcmd) > 0 then
            call add_cmd wcmd
         if morient <> null then call add_cmd "ORIENT" morient
         if length(ppl) > 0 then do
            call fix_layout ppl
            ppl = layout
            if pos("*",ppl) > 0 then do
               zerrsm = "Error"
               zerrlm = "Invalid page layout specified - try again."
               "Setmsg msg(isrz002)"
               signal do_page
               end
            end
         if length(ppm)+length(ppl) > 0 then
            call add_cmd "PAGE" ppm"/"ppl
         if length(ptype) > 0 then do
            call fix_colorbar ptype
            ptype = new_color
            if pos("*",ptype) > 0 then do
               zerrsm = "Error"
               zerrlm = "Invalid paper type specified - try again."
               "Setmsg msg(isrz002)"
               signal do_page
               end
            end
         if length(psize)+length(ptype)+length(pstyle) > 0 then
            call add_cmd "PAPER" psize"/"ptype"/"pstyle
         "Vput (ppm ppl psize ptype pstyle morient" ,
               "mtm mbm mlm mrm mlpi) Profile"
         return

        /* ----------------- *
         * Fixup Page Layout *
         * ----------------- */
         Fix_Layout: Procedure Expose layout
         arg ppl
         Select
           when abbrev("SP",ppl,1)              then layout = "SinglePage"
           when abbrev("OC",ppl,1)              then layout = "OneColumn"
           when abbrev("TCL",ppl,3)             then layout = "TwoColumnLeft"
           when abbrev("TCR",ppl,3)             then layout = "TwoColumnRight"
           when abbrev("SINGLEPAGE",ppl,2)      then layout = "SinglePage"
           when abbrev("ONECOLUMN",ppl,2)       then layout = "OneColumn"
           when abbrev("TWOCOLUMNLEFT",ppl,10)  then layout = "TwoColumnLeft"
           when abbrev("TWOCOLUMNRIGHT",ppl,10) then layout = "TwoColumnRight"
           otherwise ppl = "*"ppl
           end
         return

        /* ---------------- *
         * Fixup All Colors *
         * ---------------- */
         Fix_Color: Procedure Expose new_color
         arg color
         Select
           when abbrev("AQUA",color,1)     then new_color = "Aqua"
           when abbrev("BLACK",color,3)    then new_color = "Black"
           when abbrev("BLUE",color,3)     then new_color = "Blue"
           when abbrev("FUCHSIA",color,1)  then new_color = "Fuchsia"
           when abbrev("GRAY",color,3)     then new_color = "Gray"
           when abbrev("GREEN",color,3)    then new_color = "Green"
           when abbrev("LIME",color,1)     then new_color = "Lime"
           when abbrev("MAROON",color,1)   then new_color = "Maroon"
           when abbrev("NAVY",color,1)     then new_color = "Navy"
           when abbrev("OLIVE",color,1)    then new_color = "Olive"
           when abbrev("PURPLE",color,1)   then new_color = "Purple"
           when abbrev("RED",color,1)      then new_color = "Red"
           when abbrev("SILVER",color,1)   then new_color = "Silver"
           when abbrev("TEAL",color,1)     then new_color = "Teal"
           when abbrev("WHITE",color,1)    then new_color = "White"
           when abbrev("YELLOW",color,1)   then new_color = "Yellow"
           when abbrev("BLUEBAR",color,5)   then new_color = "BlueBar"
           when abbrev("GRAYBAR",color,5)   then new_color = "GrayBar"
           when abbrev("GREENBAR",color,6)  then new_color = "GreenBar"
           when abbrev("ORANGEBAR",color,2) then new_color = "OrangeBar"
           when abbrev("WHITEBAR",color,6)  then new_color = "WhiteBar"
           when abbrev("YELLOWBAR",color,7) then new_color = "YellowBar"
           Otherwise new_color = "*"color
           end
         return

        /* ---------------- *
         * Fixup ColorBars  *
         * ---------------- */
         Fix_ColorBar: Procedure Expose new_color
         arg color
         Select
           when abbrev("BLUEBAR",color,5)   then new_color = "BlueBar"
           when abbrev("GRAYBAR",color,5)   then new_color = "GrayBar"
           when abbrev("GREENBAR",color,6)  then new_color = "GreenBar"
           when abbrev("ORANGEBAR",color,2) then new_color = "OrangeBar"
           when abbrev("WHITEBAR",color,6)  then new_color = "WhiteBar"
           when abbrev("YELLOWBAR",color,7) then new_color = "YellowBar"
           Otherwise new_color = "*"color
           end
         return

        /* ---------------------------------------------------- *
         * Generate the Batch JCL and Control statements        *
         * Then prompt the user to:                             *
         *    Browse, Copy, Edit, Submit or eXecute             *
         * ---------------------------------------------------- */
         Build_Txt2PDF: Procedure expose cmd cmd. null zerralrm ,
                                         ver input output valid ,
                                         config_cards. co cofile

         parse source x y xcmd dd .
         call get_dsn dd xcmd
         xdsn = return_dsn

         Address ISPExec "QLibdef ISPLLIB Type(type) Id(id)"
         if rc > 0 then do
            call get_dsn "ISPLLIB" "T2PINIT"
            ldsn = return_dsn
            end
         else do
              if type = "DATASET" then
                 ldsn = strip(translate(id," ","',"))
                 else do
                      call get_dsn id "T2PINIT"
                      ldsn = return_dsn
                      end
              end

         Address ISPExec
         call do_jobcard

         call get_date

         head  = "TXT2PDF JCL generated:" ,
                  t2pdate
         trail = "TXT2PDF ISPF Interface Version" ver

         job.1  = xjc1
         job.2  = xjc2
         job.3  = xjc3
         job.4  = xjc4
         job.5  = "//*----------------------------------------------*"
         job.6  = "//*"left(head,46)"*"
         job.7  = "//*                                              *"
         job.8  = "//* Statements 1-4 are reserved for the JOB Card *"
         job.9  = "//* Verify all dsnames in the command if not     *"
         job.10 = "//* running under the generating userid.         *"
         job.11 = "//*                                              *"
         job.12 = "//*"left(trail,46)"*"
         job.13 = "//*----------------------------------------------*"
         job.0  = 13

                               /* add JCL cards - *customize*       */
         incl.0    = 0         /* count for JCL includes statements */
         exec.0    = 0         /* count for additional EXECs        */
         _parm_    = ""        /* parm for IKJEFT1B                 */

         _customize_ = "NO"
         if _customize_ = "YES" ,
         then do ;
                /* sample for additional JCL cards          */
                _parm_ = "* %ZBDDTCP"
                custchar = MVSVAR("SYMDEF","CUSTCHAR")
                _dsnpref_ = left(custchar"I",1)
                exec.0 = 3
                exec.1 = ""_dsnpref_"ZBP.XMITIP.EXEC"
                exec.2 = ""_dsnpref_"ZBP.TOOLS.EXEC"
                exec.3 = "ABCDEF.GHIJKLM.NOPQR.EXEC"
                incl.0 = 1
                incl.1 = "* ZTDDTCP"
              end;

         tso.0  = 0
         _x_ = tso("//TXT2PDF  EXEC PGM=IKJEFT1B,DYNAMNBR=50," )
         if left(_parm_,1) = "*" ,
         then do;
                        parse var _parm_ 1 "*" _parm_
                        _pre_ = left("//* ",4)
              end;
         else           _pre_ = left("//  ",4)
         _parm_ = strip(_parm_)
         _x_ = tso(""_pre_"PARM=('"_parm_"'),"                 )
         _x_ = tso("//  COND=(0,NE)"                           )
         _x_ = tso("//*"                                       )
         if ldsn <> null ,
         then do
                _x_ = tso("//STEPLIB  DD  DISP=(SHR),DSN="ldsn  )
                _x_ = tso("//*"                                 )
              end
         do i = 1 to incl.0
                _inclmem_ = incl.i
                if left(_inclmem_,1) = "*" ,
                then do;
                        parse var _inclmem_ 1 "*" _inclmem_
                        _pre_ = left("//* ",4)
                     end;
                else    _pre_ = left("//  ",4)
                _inclmem_ = strip(_inclmem_)
                _x_ = tso(""_pre_"INCLUDE MEMBER="_inclmem_)
         end

         _x_ = tso("//SYSEXEC  DD  DISP=(SHR),DSN="xdsn      )
         do i = 1 to exec.0
                exec.i = strip(translate(exec.i,"",",'"))
                if sysdsn("'"exec.i"'") = "OK" ,
                then _x_ = tso("//         DD  DISP=(SHR),DSN="exec.i)
                else _x_ = tso("//*        DD  DISP=(SHR),DSN="exec.i)
         end
         _x_ = tso("//SYSPRINT DD  SYSOUT=*"                   )
         _x_ = tso("//SYSTSPRT DD  SYSOUT=*"                   )
         _x_ = tso("//SYSTSIN  DD  *"                          )

         jcl.0 = 0
         do i = 1 to job.0
            idx = jcl.0 + 1
            jcl.0 = idx
            jcl.idx = job.i
         end

         do i = 1 to tso.0
            idx = jcl.0 + 1
            jcl.0 = idx
            jcl.idx = tso.i
         end

         do i = 1 to cmd.0
            idx = jcl.0 + 1
            jcl.0 = idx
            jcl.idx = " "cmd.i
         end

         idx = jcl.0 + 1
         jcl.0 = idx
         jcl.idx = "/*"

         Select
            When sysvar("syspref") = null
               then hlq = userid()"."
            When sysvar("syspref") <> userid()
               then hlq = sysvar("syspref")"."userid()"."
            Otherwise hlq = userid()"."
            end

         jcldd = "t2pjc"random(99)
         t2pjcl = "'"hlq"TXT2PDF.jcl'"

         call build_jcl t2pjcl

         if co <> null then do
            t2pconf = cofile
            call create_config
            return
            end

         do forever
            zcmd = null
            "Display Panel(t2pgs)"
            if rc > 3 then leave
            Select
              When zcmd = "B" then do
                   "Browse Dataset("t2pjcl")"
                   zerrsm = "Browse Complete"
                   zerrlm = "Browse of the Job and TXT2PDF" ,
                          "complete."
                   "Setmsg msg(isrz002)"
                   end
              When zcmd = "E" then do
                   "Edit Dataset("t2pjcl")"
                   Address TSO
                   "Alloc f("wdd") shr ds("t2pjcl")"
                   "Execio * diskr" wdd "(finis stem jcl."
                   "Free  f("wdd")"
                   xjc1 = jcl.1
                   xjc2 = jcl.2
                   xjc3 = jcl.3
                   xjc4 = jcl.4
                   Address ISPExec
                   "Vput (xjc1 xjc2 xjc3 xjc4) Profile"
                   hit = 0
                   cmd = null
                   do i = 1 to jcl.0
                      if left(jcl.i,1) = "%" then hit = 1
                      if left(jcl.i,1) = "/" then hit = 0
                      if hit = 1 then do
                         temp = strip(jcl.i)
                         if right(temp,1) = "+" then
                            temp = left(temp,length(temp)-1)
                         cmd = strip(cmd temp)
                         end
                      end
                   zerrsm = "Edit Complete"
                   zerrlm = "Edit complete and job card variables" ,
                             "updated if changed and the command" ,
                             "has been updated as well if changed."
                   "Setmsg msg(isrz002)"
                   end
              When zcmd = "C" then do
                   call copy_jcl
                   zerrsm = "Complete"
                   zerrlm = "Copy Operation Completed."
                   "Setmsg msg(isrz002)"
                   end
              When zcmd = "J" then do
                   call do_jobcard2
                   call build_jcl t2pjcl
                   zerrsm = "Complete"
                   zerrlm = "Job Statement Updated or Reviewed."
                   "Setmsg msg(isrz002)"
                   end
              When zcmd = "SC" then call create_config
              When zcmd = "S" then do
                   Address TSO "Submit" t2pjcl
                   zerrsm = "Job Submitted"
                   parse value xjc1 with "//"jobname .
                   zerrlm = "Job" jobname "has been submitted for execution."
                   "Setmsg msg(isrz002)"
                   end
              When zcmd = "F" then do
                   zcmd = null
                   "Display Panel(t2ppf)"
                   if rc = 0 then do
                      "Vput (target host userid) Profile"
                      call build_ftpjcl
                      Address TSO
                      "Alloc f("wdd") shr ds("t2pjcl")"
                      "Execio * diskw" wdd "(finis stem jcl."
                      "Free  f("wdd")"
                      Address ISPExec
                      end
                   end
              When zcmd = "M" then do
                   zcmd = null
                   "Display Panel(t2ppm)"
                   if rc = 0 then do
                      "Vput (t2pto t2pfrom t2psubj t2patt" ,
                            "t2pmsg) Profile"
                      call build_mailjcl
                      Address TSO
                      "Alloc f("wdd") shr ds("t2pjcl")"
                      "Execio * diskw" wdd "(finis stem jcl."
                      "Free  f("wdd")"
                      Address ISPExec
                      end
                   end
              When zcmd = "X" then do
                   Address TSO cmd
                   if rc > 0 then do
                     zerrsm = "Failed:" rc
                     zerrlm = "TXT2PDF failed with return code" rc
                     "Setmsg msg(isrz002)"
                     end
                   else do
                     zerrsm = "Completed"
                     if valid = null then
                     zerrlm = "TXT2PDF Processing completed for",
                              input "to" output
                     else
                     zerrlm = "TXT2PDF Validation processing" ,
                              "completed successfully."
                     "Setmsg msg(isrz002)"
                     if valid = null then
                        call post_process
                     end
                   end
              Otherwise nop
              end
            end

         Address TSO
         call msg "off"
         "Delete" t2pjcl
         return

        /* ----------------------------- *
         * Create the Configuration File *
         * ----------------------------- */
         Create_Config:
         mcpdd = "mcp"random(9999)
         if "DATASET NOT FOUND" = sysdsn(t2pconf) then do
            Address TSO
            if pos("(",t2pconf) > 0 then dir = "Dir(12)"
                                    else dir = null
            "Alloc f("mcpdd") new spa(15,15) tr" dir ,
                  "recfm(f b) lrecl(80) blksize(0)",
                  "ds("t2pconf")"
            "execio * diskw" mcpdd "(finis stem config_cards."
            "Free  f("mcpdd")"
            Address ISPExec
            end
         else do
              Address TSO
              "Alloc f("mcpdd") shr ds("t2pconf")"
              "execio * diskw" mcpdd "(finis stem config_cards."
              "Free  f("mcpdd")"
              Address ISPExec
              end
         zerrlm = "TXT2PDF Configuration saved in:" t2pconf
         "Setmsg msg(isrz002)"
         "Vput (t2pconf) Profile"
         return

        /* --------------------------------------------------------- *
         * Copy the Generated JCL to a target Data Set               *
         *      - test for target d/s exist                          *
         * --------------------------------------------------------- */
         Copy_JCL: Procedure expose t2pjcl jcl. null zerralrm
           do forever
              "Display Panel(t2pgc)"
              if rc > 3 then return
              Address TSO
              indd  = "t2pi"random(99)
              outdd = "t2po"random(99)
              dsn_rc = 1
              mem    = null
              if pos("(",t2p2jcl) > 0 then do
                 if sysdsn(t2p2jcl) = "OK"
                    then dsn_rc = 0
                 if sysdsn(t2p2jcl) = "MEMBER NOT FOUND"
                    then dsn_rc = 0
                 parse value t2p2jcl with t2p2ds"("mem")" q
                 if q = "'" then t2p2ds = t2p2ds"'"
                 end
              else do
                   if sysdsn(t2p2jcl) = "OK"
                      then dsn_rc = 0
                   t2p2ds = t2p2jcl
                   end
              if dsn_rc = 1 then do
                 outdd = "t2pc"random(99)
                 "Alloc f("outdd") new spa(1,1) tr recfm(f b) lrecl(80)" ,
                    "blksize(6160) ds("t2p2jcl")"
                 end
              else do
                   "Alloc f("outdd") ds("t2p2jcl") shr reuse"
                   end
              "Alloc f("indd") ds("t2pjcl") shr reuse"
              "Execio * diskw" outdd "(finis stem jcl."
              "Free  f("outdd indd")"
              Address ISPExec
              smsg = "Copied"
              lmsg = t2pjcl "copied to" t2p2jcl
              "Setmsg msg(isrz002)"
              if mem <> null then do
                 "lminit dataid(memid) dataset("t2p2ds")"
                 "lmopen dataid("memid")"
                 uid = sysvar("sysuid")
                 "lmmstats dataid("memid") member("mem") user("uid")"
                 "lmclose dataid("memid")"
                 "lmfree  dataid("memid")"
                 end
              end
           return

        /* --------------------------------------------------------- *
         * Build the JCL Data Set                                    *
         * --------------------------------------------------------- */
        Build_JCL:
           Address TSO
           call msg "off"
           "Delete" t2pjcl
           "Alloc f("jcldd") ds("t2pjcl") new spa(5,5) Tr",
              "recfm(f b) lrecl(80) blksize(6160)"
           "Execio * diskw" jcldd "(finis stem jcl."
           "Free f("jcldd")"
           Address ISPExec
           return

        /* --------------------------------------------------------- *
         * Ask for and update job statements                         *
         * --------------------------------------------------------- */
         do_jobcard2:
            jcupdate = 1
         do_jobcard:
            "Vget (xjc1 xjc2 xjc3 xjc4) Profile"
           if length(xjc1) = 0 then xjc1 = "//*"
           parse value xjc1 with "//"jname jrest
           sjname = left(jname,length(jname)-1)
           if sjname = sysvar("sysuid") then do
              call get_jobid
              jname = sjname""jobsuf
              xjc1 = "//"jname jrest
              end
            if strip(xjc1) = "//*" then do
               call get_jobid
               xjc1 = "//"sysvar("sysuid")""jobsuf "JOB" ,
                      "'name',NOTIFY=&SYSUID,"
               xjc2 = "//      MSGCLASS=A,MSGLEVEL=(1,1)"
               xjc3 = "//HOLD OUTPUT" ,
                      "JESDS=ALL,DEFAULT=N,OUTDISP=(HOLD,HOLD)"
               jcupdate = 1
               end
            if jcupdate = 1 then do
               zxjc1 = xjc1
               zxjc2 = xjc2
               zxjc3 = xjc3
               zxjc4 = xjc4
               xrc = 0
               do until xrc = 1
                  zcmd = null
                  "Display Panel(t2pgj)"
                  if abbrev("CANCEL",zcmd,3) = 1 then return
                  if rc > 3 then do
                     xjc1 = strip(zxjc1)
                     xjc2 = strip(zxjc2)
                     xjc3 = strip(zxjc3)
                     xjc4 = strip(zxjc4)
                     if xjc1 = null then xjc1 = "//*"
                     if xjc2 = null then xjc2 = "//*"
                     if xjc3 = null then xjc3 = "//* "
                     if xjc4 = null then xjc4 = "//* "
                     "Vput (xjc1 xjc2 xjc3 xjc4) Profile"
                     jcl.1 = xjc1
                     jcl.2 = xjc2
                     jcl.3 = xjc3
                     jcl.4 = xjc4
                     xrc = 1
                     end
                  end
               end
               return

        /* -------------------------- *
         * Get a unique jobid by      *
         * bumping the last char      *
         * -------------------------- */
         Get_JobID:
         Address ISPExec ,
             "VGET (JOBSUF) PROFILE"
         if length(jobsuf) = 0 then jobsuf = "A"
            else
            jobsuf = translate(jobsuf, ,
                      'BCDEFGHIJKLMNOPQRSTUVWXYZA1234567890', ,
                      'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
         Address ISPExec ,
            "VPUT (JOBSUF) PROFILE"
         return

        /* ------------------------------- *
         * Build FTP JCL for the Batch Job *
         * ------------------------------- */
         Build_FTPJCL: Procedure expose jcl. target host ,
                       output pw userid
         c = jcl.0
         c = c + 1
         jcl.c = "//*----------------------------------------------*"
         c = c + 1
         jcl.c = "//* TCP/IP File Transfer Protocol (FTP) Step     *"
         c = c + 1
         jcl.c = "//*----------------------------------------------*"
         c = c + 1
         jcl.c = "//FTPSTEP  EXEC PGM=FTP,PARM='"host " (EXIT'"
         c = c + 1
         jcl.c = "//SYSPRINT DD SYSOUT=*"
         c = c + 1
         jcl.c = "//INPUT    DD *"
         c = c + 1
         jcl.c = userid pw
         c = c + 1
         jcl.c = "binary"
         c = c + 1
         jcl.c = "put" output target
         c = c + 1
         jcl.c = "quit"
         c = c + 1
         jcl.c = "/*"
         jcl.0 = c
         return

        /* ------------------------------ *
         * Build E-Mail JCL for Batch JOB *
         * ------------------------------ */
         Build_MailJCL: Procedure expose jcl. t2pto t2pfrom ,
                        t2psubj tso. output ,
                        t2patt t2pmsg null
         c = jcl.0
         c = c + 1
         jcl.c = "//*----------------------------------------------*"
         c = c + 1
         jcl.c = "//* Batch TSO Monitor Program (TMP) to execute   *"
         c = c + 1
         jcl.c = "//* the XMITIP application to e-mail the report. *"
         c = c + 1
         jcl.c = "//*----------------------------------------------*"

         do i = 1 to tso.0
            _jclline_ = tso.i
            if left(_jclline_,2)""word(_jclline_,2) = "//EXEC" ,
            then do;
                     parse value _jclline_ with "//"step rest
                     _jclline_ = "//MAILSTEP" strip(rest)
                 end;
            c = c + 1
            jcl.c = _jclline_
            end
         c = c + 1
         jcl.c = " xmitip" t2pto "+"
         c = c + 1
         jcl.c = "   From" t2pfrom "+"
         c = c + 1
         jcl.c = "   Subject '"t2psubj"'  +"
         c = c + 1
         jcl.c = "   File" output "+"
         if t2patt <> null then do
            c = c + 1
            jcl.c = "   Filename" t2patt "+"
            end
         c = c + 1
         jcl.c = "   Format pdf +"
         c = c + 1
         if strip(t2pmsg) <> null then do
            jcl.c = "   MSGT '"t2pmsg"'"
            end
         else do
              jcl.c = "   NOMSG"
              end
         c = c + 1
         jcl.c = "/*"
         jcl.0 = c
         return

        /* ------------------------------------------- *
         * Post Process the online generated PDF file. *
         *   - Download                                *
         *   - FTP                                     *
         *   - E-Mail                                  *
         * ------------------------------------------- */
         Post_Process: Procedure expose output null
         do forever
            zcmd = null
            "Display Panel(T2PP)"
            if rc > 3 then leave
            Select
              When zcmd = "D" then call do_download
              When zcmd = "F" then call do_ftp
              When zcmd = "M" then call do_mail
              otherwise nop
              end
            end
         return

        /* --------------------------------------------------------- *
         * Do Mail.                                                  *
         * Invoke XMITIP ISPF interface using the XMITIPFE interface *
         * command.                                                  *
         * --------------------------------------------------------- */
         Do_Mail:
         call get_date
          "Select cmd(%xmitipfe file("output") format(pdf)" ,
                  "subject(PDF Report" t2pdate time()")" ,
                  "filename(report.pdf)" ,
                  ")"
         return

        /* ----------------------------------------------------- *
         * Do FTP.                                               *
         * Prompt user for target file name, host, userid and pw *
         * then invoke ftp.                                      *
         * ----------------------------------------------------- */
         Do_FTP:
           Do forever
              zcmd = null
              "Display Panel(t2ppf)"
              if rc > 3 then leave
              ftp.1 = userid pw
              ftp.2 = "Binary"
              ftp.3 = "Put" output target
              ftp.4 = "quit"
              Address TSO
              "Alloc f(input) new spa(1,1) tr recfm(f b)" ,
                      "lrecl(80) blksize(6160)"
              "Execio * diskw input (finis stem ftp."
              Address ISPExec
              "Select PGM(FTP) PARM("host"(EXIT)"
              x_rc = rc
              Address TSO "Free F(input)"
              "Vput (target host userid) Profile"
              zerrsm = "Completed rc:" x_rc
              zerrlm = "FTP Completed with return code" x_rc
              "Setmsg msg(isrz002)"
              end
         return

        /* ------------------------------------------------------- *
         * Do File Download                                        *
         *   - support IND$FILE (normal terminal emulator tranfer) *
         *   - support FTP (from the users workstation)            *
         * ------------------------------------------------------- */
         Do_Download:
            call get_tcpip
            do forever
               zcmd = null
               "Display panel(t2ppd)"
               xrc = rc
               if xrc = 0 then
               if zcmd <> null then do
                  Address TSO zcmd
                  xrc = 1
                  end
               if xrc > 0 then leave
               end
            if xrc = 1 then do
               zedsmsg = "Complete."
               zedlmsg = "File transfer request for" output ,
                         "has been completed."
               end
            else do
               zerrsm = "Cancelled"
               zerrlm = "File transfer request for" ext_dsn ,
                        "has been cancelled."
               end
            "Setmsg msg(isrz002)"
            return

        /* ----------------------------------- *
         * Get current TCP/IP Host information *
         * ----------------------------------- */
         Get_Tcpip:
            res    = Socket('Initialize','ANYNAME')
            host   = Socket('GetHostId')
            name   = word(Socket('GetHostname'),2)
            domain = word(Socket('GetDomainName'),2)
            res    = Socket('Terminate')
            ftphost = name"."domain
            return

        /* --------------------------------- *
         * Get Current Date in Usable Format *
         * --------------------------------- */
         Get_Date: Procedure Expose t2pdate
         month = date('m')
         work  = date('s')
         day   = substr(work,7,2)
         year  = left(work,4)
         t2pdate = month day"," year
         return

        /* -------------------------------------------- *
         * Get Data Set Name for supplied DD and Member *
         * -------------------------------------------- */
         Get_DSN: Procedure Expose return_dsn null
         arg dd cmd
         return_dsn = null

         call outtrap 'trap.'
         "lista sta"
         call outtrap 'off'

         hit = 0
         cnt = 0

         do i = 1 to trap.0
            if tdd = dd then hit = 1
            if hit = 1 then
               if tdd <> dd then leave
            if left(trap.i,2) = "--" then iterate
            if left(trap.i,1) <> " " then do
               dsn = word(trap.i,1)
               end
            else do
                 if left(trap.i,3) = "   " then do
                 if tdd <> dd then iterate
                 cnt = cnt + 1
                 dsn.cnt = tdd dsn
                 end
               else do
                    tdd = word(trap.i,1)
                    if tdd <> dd then iterate
                    cnt = cnt + 1
                    dsn.cnt = tdd dsn
                    end
               end
            end

         do i = 1 to cnt
            if "OK" = sysdsn("'"word(dsn.i,2)"("cmd")'") then do
               return_dsn = word(dsn.i,2)
               leave
               end
            end
         return

        /* -------------------------------------------- *
         * Build step JCL statements                    *
         * -------------------------------------------- */
   tso: procedure expose tso.
     parse arg _jclline_
     idx = tso.0 + 1
     tso.0   = idx
     tso.idx = _jclline_
    return 0
