/*****************************   REXX   ******************************/
/*                                                                   */
/*  member name:    XLATCOMP                       (C) Atos Origin   */
/*                                                                   */
/*  member type:                                                     */
/*                                                                   */
/*  Author:         A100740  BECKMANN              DATE: 2008-11-20  */
/*                                                                   */
/*  Description:    compare 2 translation tables __________________  */
/*                  parms: table1 Ätable2-default XLATE000Ü _______  */
/*                  name must start with XLAT* (or must be TXT2PDFX) */
/*                  _______________________________________________  */
/*                  default: (without parms) ______________________  */
/*                      compare XLATE000 TXT2PDFX _________________  */
/*                  If you don't have STEMVIEW installed please      */
/*                  trap the output for better readability / analyze */
/*                                                                   */
/*  History:                                                         */
/*  __date__    ver comment                                          */
/*  2008-12-15  1.2 - bug fixes                                      */
/*                  - add EDIT macro support                         */
/*  2008-11-21  1.1 - check length of xlate strings (512)            */
/*                  - allow TXT2PDFX as a table name                 */
/*                  - add STEMVIEW                                   */
/*                  - check for duplicate HEX values                 */
/*                  - add help                                       */
/*  2008-11-20  1.0 - _initial_ (HB)                                 */
/*********************************************************************/
  _x_ = TRACE("n")
  signal on novalue name sub_novalue
  _x_ = sub_init() ;
  parse upper arg _all_parms_
  if _sysispf_ = "ACTIVE" ,
  then do;
           ADDRESS ISPexec
           "control errors return"
           ADDRESS ISREDIT
           "Macro (parms) NOPROCESS"
           rcode = rc
           if rcode = 0 ,
           then do;
                   "(dataset) = dataset"
                   "(member)  = member"
                   if words(parms) = 1 ,
                   then _all_parms_ = parms" "member
                   else _all_parms_ = parms
                end;
       end;
  _all_parms_ = translate(_all_parms_)
  parse var       _all_parms_ 1 tab1 tab2 .
  _x_ = sub_msg(""left("translation table compare tool",30),
                ""right("Version: "_ver_"",35)"");
  _x_ = sub_msg("starting with parms: "_all_parms_"");
  _x_ = sub_msg(" ");

  select;
    when ( abbrev(_all_parms_,"?") = 1 ) then rcode = sub_help();
    when ( abbrev(_all_parms_,"H") = 1 ) then rcode = sub_help();
    otherwise                                 rcode = sub_main();
  end;

_x_ = sub_msg(" ")
_x_ = sub_msg("ending with RCODE="rcode".")
_rc_ = sub_stemview() ;
exit rcode


       /* ----------------------------------------- *
        * Trap uninitialized variables              *
        * ----------------------------------------- */
        sub_novalue:
         Say "Variable" ,
            condition("Description") "undefined in line" sigl":"
         Say sourceline(sigl)
         _rcode_ = 8
         if _sysenv_ = "FORE" ,
         then do;
                 say "Report the error in this application",
                     "along with the syntax used."
              end;
         exit _rcode_

 /* ----------------------------------------------------- *
  * sub_init                                              *
  ------------------------------------------------------- */
 sub_init:
   parse value "" with null
   /* to get the correct name for MSGID don't use other cmds before */
   parse source ,
     rexxenv rexxinv rexxname rexxdd rexxdsn . rexxenv addrspc .
   myname = rexxname
   if myname = "?" ,
   then do ;
            myname = sysvar("sysicmd")
            if length(myname) = 0 ,
            then  myname = sysvar("syspcmd")
        end;
   msgid = left(myname":",10)

   "ISPQRY"
   ispfcc  = rc
   if ispfcc = 0 ,
   then _sysispf_ = "ACTIVE"
   else _sysispf_ = "NOT ACTIVE"

   _ver_ = "1.1 2008-11-21"
   _ver_ = "1.2 2008-12-15"
   summary.0 = 2
   summary.1 = 0
   summary.2 = 0
   do i = 1 to summary.0
      summary.i.1 = 0
      summary.i.2 = ""
      summary.i.3 = "skipped"
   end
   msg.0     = 0
   global_vars = "_null_ msgid myname msg. _ver_ summary. _sysispf_"
  return 0

 sub_msg: procedure expose (global_vars)
   parse arg _msgtext_
   idx = msg.0 + 1
   msg.0 = idx
   msg.idx = ""msgid""_msgtext_
  return 0

 /* checking must be done in subroutine */
 sub_stemview:
   stemview_rc = 8
   signal   on syntax Name sub_stemview_no
   CALL STEMVIEW "BROWSE","msg.",1,msg.0,""myname
   stemview_rc = 0
  sub_stemview_no:
   signal   off syntax
   if stemview_rc = 0 ,
   then nop;
   else do;
            _x_   = sub_msg_gen(" ");
            _x_   = sub_msg_gen("STEMVIEW not active")
        end;
   return stemview_rc

 sub_main:
  maxrc = 0
  rcode = 0
  tab1 = strip(tab1)
  tab2 = strip(tab2)
  if rcode = 0 ,
  then do;
          select;
            when ( tab1 = "XLATCOMP" ) ,
              then do;
                       _err_reserved_ = "YES TAB1 "tab1
                   end;
            when ( tab2 = "XLATCOMP" ) ,
              then do;
                       _err_reserved_ = "YES TAB2 "tab2
                   end;
            otherwise  _err_reserved_ = "NO"
          end
       end;
       if _err_reserved_ = "NO" ,
       then nop;
       else do;
                   parse var _err_reserved_ 1 . _text_
                   _x_ = sub_msg("ERROR - reserved name used",
                            "- "_text_"",
                            "")
                   rcode = 8
            end;
  if rcode = 0 ,
  then do;
          select;
            when ( tab1 = "" ) ,
              then do;
                       tab1 = "XLATE000"
                   end;
            when ( tab1 = "TXT2PDFX"       ) ,
              then nop
            when ( abbrev(tab1,"XLAT") = 1 ) ,
              then nop
            otherwise do;
                   _x_ = sub_msg("ERROR - 1st table >>> name incorrect",
                            "- found: <"tab1">",
                            "")
                   _x_ = sub_msg("   table name must start with XLAT",
                            "or use special name TXT2PDFX.",
                            "")
                   rcode = 8
               end;
          end;
       end;
  if rcode = 0 ,
  then do;
          select;
            when ( tab2 = "" ) ,
              then do;
                       tab2 = "TXT2PDFX"
                   end;
            when ( tab2 = "TXT2PDFX"       ) ,
              then nop
            when ( abbrev(tab2,"XLAT") = 1 ) ,
              then nop
            otherwise do;
                   _x_ = sub_msg("ERROR - 2nd table >>> name incorrect",
                            "- found: <"tab2">",
                            "")
                   _x_ = sub_msg("   table name must start with XLAT",
                            "or use special name TXT2PDFX.",
                            "")
                   rcode = 8
               end;
          end;
       end;
 if rcode = 0 ,
 then do;
          _cmd_ = "xlattab1 = "tab1"()"
          rcode = sub_interpret_command(""_CMD_"")
          if rcode = 0 ,
          then do;
                  xl = length(xlattab1)
                  if xl = 512 ,
                  then nop
                  else do;
                           _x_ = sub_msg("ERROR - "tab1 ">>>",
                                    "length of xlate string",
                                    "must be 512, found "xl".",
                                    "")
                           rcode = 8
                       end;
               end;
      end;
 if rcode = 0 ,
 then do;
          _cmd_ = "xlattab2 = "tab2"()"
          rcode = sub_interpret_command(""_CMD_"")
          if rcode = 0 ,
          then do;
                  xl = length(xlattab2)
                  if xl = 512 ,
                  then nop
                  else do;
                           _x_ = sub_msg("ERROR - "tab2 ">>>",
                                    "length of xlate string",
                                    "must be 512, found "xl".",
                                    "")
                           rcode = 8
                       end;
               end;
      end;

if rcode = 0 ,
then do;
        start.1 = 1
        start.2 = start.1 + 256
        start.0 = 2
        _x_ = sub_msg(" compare tables ")
        _x_ = sub_msg(""left(" ",09)""tab1" - "tab2"")
        do idx = 1 to start.0
                select;
                  when ( idx = 1 ) then _info_ = "Text-Part"
                  when ( idx = 2 ) then _info_ = "Data-Part"
                  otherwise             _info_ = "unknown"
                end
                rcode = 0
                xlat_tab1 = substr(xlattab1,start.idx,256)
                xlat_tab2 = substr(xlattab2,start.idx,256)
                if rcode = 0 ,
                then do;
                       rcode = sub_compare(""idx" "_info_) ;
                     end;
                select
                  when ( rcode = 0 ) ,
                    then nop
                  when ( rcode < 5 ) ,
                    then do;
                         _x_ = sub_msg(" ")
                         _x_ = sub_msg("WARNING -",
                                       "please check messages")
                         end;
                  otherwise do;
                         _x_ = sub_msg(" ")
                         _x_ = sub_msg("ERROR -",
                                       "please check messages")
                         leave
                         end;
                end;
        end;
     end;
  if rcode < 5 ,
  then do;
          _x_ = sub_msg(" Summary");
          do i = 1 to summary.0
             if summary.i.1 = 0 ,
             then nop
             else maxrc = max(maxrc,4)
             if summary.i.3 = "skipped" ,
             then nop
             else ,
             _x_ = sub_msg("",
                           ""left(summary.i.2,10)" ",
                           ""right(summary.i.1,3)" differences",
                           "");
          end
       end;
   maxrc = max(maxrc,rcode)
  return maxrc

 sub_compare:
   parse arg sum_idx sum_idx_text
   summary.sum_idx.3 = ""
   _x_ = sub_msg(" ")
   _x_ = sub_msg(""_info_)
   rcode = 0
   if rcode < 5 ,
   then do;
            drop E2ATab
            E2ATab = xlat_tab1
            rcode  = sub_check_e2a_code()
            drop E2ATab
        end;
   if rcode < 5 ,
   then do;
            drop E2ATab
            E2ATab = xlat_tab2
            rcode  = sub_check_e2a_code()
            drop E2ATab
        end;

   if rcode < 5 ,
   then do;
           _x_ = sub_msg("nnn dec hex char")
           _count_ = 0
           do i = 1 to length(xlat_tab1)
                x1 = substr(xlat_tab1,i,1)
                x2 = substr(xlat_tab2,i,1)
                if x1 = x2 ,
                then nop
                else do;
                         j = i - 1
                         _count_ = _count_ + 1
                         _x_val_ = right("0"d2x(j),2)
                         _pre_ = right(_count_,3,0)"",
                                 right(j,3,0)" "left(_x_val_,3)"",
                                 ""left("("x2c(d2x(j))")",4),
                                 ""
                        _x_ = sub_msg(""left(_pre_,18),
                             ""x1"="c2x(x1)"       "x2"="c2x(x2)"",
                             "");
                     end
           end
           summary.sum_idx.1 = _count_
           summary.sum_idx.2 = sum_idx_text
           _x_ = sub_msg(" --> result: "right(_count_,3)" differences")
           _x_ = sub_msg(" ")
        end;
  return rcode

 sub_interpret_command:
   parse arg _command_to_interpret_
   rcode = 20
   signal on  syntax Name sub_interpret_command_err
   interpret ""_command_to_interpret_
   rcode = 0
  sub_interpret_command_err:
   signal off syntax
   if rcode = 20 ,
   then do
           parse var _command_to_interpret_ 1 . "=" _func_ "()" .
           _func_ = strip(_func_)
           _x_ = sub_msg("ERROR calling function with name "_func_"")
           _x_ = sub_msg("                     (table name wrong?)")
        end
  return rcode

 sub_check_e2a_code: procedure expose (global_vars) E2ATab
   rcode = 0
   E2A.  = ""
   do       P = 1 to 256
            HexCode = C2X(SubStr(E2ATab,P,1))
            DecCode = X2D(HexCode)
            if E2A.DecCode = "" ,
            then E2A.DecCode = HexCode
            else do
               _x_ = sub_msg("WARNING -",
                             "multiple used E2A-Code:" HexCode)
               rcode = 4
            end
   end
   drop E2A.
  return rcode

 sub_help:
    _x_ = sub_msg(" ")
    _x_ = sub_msg(""myname" ...               ")
    _x_ = sub_msg("  parms:                   ")
    _x_ = sub_msg("    (no parms)  - compare XLATE000 - TXT2PDFX",
                     "(default)" );
    _x_ = sub_msg("    1st table   - compare 1st tab  - TXT2PDFX ") ;
    _x_ = sub_msg("    1st 2nd     - compare 1st tab  - 2nd tab  ") ;
    _x_ = sub_msg(" ")
    _x_ = sub_msg("    ? or H(elp) - get this HELP infos          ") ;
    _x_ = sub_msg(" ")
    _x_ = sub_msg(" ")
    _x_ = sub_msg("This tool can be used to check TXT2PDF",
                  "translation tables.")
    _x_ = sub_msg(" - show all differences in TEXT and DATA parts")
    _x_ = sub_msg(" - check length (2 * 256 = 512 required)",
                  "of the translation TABs");
    _x_ = sub_msg(" - check all HEX values of the TABs",
                  "(find duplicates)")
  return 0

