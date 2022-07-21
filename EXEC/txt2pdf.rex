/*                                Rexx                                */
  ver = "09.336"

/*
-- =====================================================================
-- Name:      txt2pdf
--
-- Function:  Convert a text file to a PDF file
--
-- Usage:     TXT2PDF ?
--              or
--            TXT2PDF IN <input filespec>
--                    OUT <output filespec>
--                    ANNOT <type> / <type spec>
--                    BG <type> / <type spec>
--                    BM <bottom margin>
--                    BROWSE <browse messages>
--                    CC <carriage control>
--                    COLOR <fore> / <back >
--                    COMPRESS <level>
--                    CONFIG <config filespec>
--                    CONFIRM <level>
--                    DEFCFG <default config filespec>
--                    DELIM <character>
--                    DINFO <document info filespec>
--                    DRAW <type> / <specs>
--                    ENCODING <func> / <specs>
--                    ENCRYPT <type> / <opts>
--                    FONT <size> / <name> / <zoom>
--                    HLQ <hlq>
--                    IFEMPTY <option>
--                    IMAGE <func> / <name> / <specs>
--                    LM <left margin>
--                    LPI <line per inch>
--                    MAG <magnification>
--                    MAXOSIZE <max output size>
--                    MSGID <prefix messages>
--                    MSGLVL <msgnum> / <msglvl>
--                    OLOPT <option/...>
--                    OLSORT <direction>
--                    OLTOK <token>
--                    ORIENT <orientation>
--                    OUTLINE <type> / <args>
--                    OUTREC <len>
--                    PAGE <mode> / <layout>
--                    PAPER <size> / <type> / <style>
--                    RM <right margin>
--                    TM <top margin>
--                    TEXT <type> / <args>
--                    TRANS <style> / <dur> / <opts/...>
--                    UNITS <unit of measure>
--                    VIEWER <flags/...>
--                    VONLY <opt>
--                    XFONT <type> / <opts>
--                    XLATE <execname>
--
-- License:   TXT2PDF - Text to PDF Conversion Utility
--            Copyright (C) 2000 - 2009 Leland Lucius
--
--            This program is free software; you can redistribute it
--            and/or modify it under the terms of the GNU General Public
--            License as published by the Free Software Foundation;
--            either version 2 of the License, or (at your option) any
--            later version.
--
--            This program is distributed in the hope that it will be
--            useful, but WITHOUT ANY WARRANTY; without even the implied
--            warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--            PURPOSE.  See the GNU General Public License for more
--            details.
--
--            You should have received a copy of the GNU General Public
--            License along with this program; if not, write to the Free
--            Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--            Boston, MA  02110-1301, USA.
--
-- ---------------------------------------------------------------------
--
-- Please refer to the end of the EXEC for documentation, change
-- history and acknowledgements.
--
-- =====================================================================
*/

  /* ===================================================================
  -- This has proven VERY useful
  */
  SIGNAL ON NOVALUE

  /* ===================================================================
  -- Go initialize the universe
  */
  CALL Initialize

  /* ===================================================================
  -- Get command arguments (need them to be mixed case )
  */
  PARSE ARG gsArgs
  gsSaveArgs = gsArgs

  /* ===================================================================
  -- Arguments specified?
  */
  gbNoArgs = gbFalse
  IF Strip( gsArgs ) = gsNull THEN DO
    gbNoArgs = gbTrue
  END

  /* ===================================================================
  -- Display ISPF interface if under ISPF and no arguments given.
  */
  IF gbNoArgs & gsAddr = "ISPF" THEN DO
    ADDRESS ISPEXEC "Select cmd(%txt2pdfi)"
    SIGNAL Done
  END

  /* ===================================================================
  -- User request help?
  */
  IF gsArgs = "?" THEN DO
    CALL Usage
    SIGNAL Done
  END

  /* ===================================================================
  -- If the only option is VER then return the active version.
  */
  IF Abbrev( Translate( gsArgs ), "VER" ) THEN DO
    PARSE SOURCE . rexxinv rexxname .
    IF rexxinv = "COMMAND" THEN DO
      SAY ""rexxname": VER="ver
      EXIT 0
    END

    RETURN VER
  END

  /* ===================================================================
  -- Attempt to pull in user defaults
  */
  SIGNAL ON SYNTAX NAME NOCFG
  gsArgs = Txt2PdfD() gsArgs
  NOCFG:
  SIGNAL OFF SYNTAX

  /* ===================================================================
  -- Command argument keywords
  */
  gsKeys = "ANNOT    BG       BM       BROWSE   CC      ",
           "COLOR    COMPRESS CONFIG   CONFIRM  DINFO   ",
           "DRAW     ENCRYPT  FONT     HLQ      IDX     ",
           "IFEMPTY  IMAGE    IN       LM       LPI     ",
           "MAG      MAXOSIZE MSGID    MSGLVL   OLOPT   ",
           "OLSORT   OLTOK    ORIENT   OUT      OUTLINE ",
           "OUTREC   PAGE     PAPER    RM       TM      ",
           "TRANS    UNITS    VIEWER   VONLY    XFONT   ",
           "XLATE    DEFCFG   TEXT     ENCODING DELIM   "

  /* ===================================================================
  -- Process the arguments
  */
  DO WHILE gsArgs <> gsNull

    /* =================================================================
    -- Isolate a keyword and remove from remaining arguments
    */
    PARSE VAR gsArgs gsKey gsArgs

    /* =================================================================
    -- Convert keyword to uppercase
    */
    gsKey = Translate( gsKey )

    /* =================================================================
    -- Is it a valid keyword?
    */
    lnP = Wordpos( gsKey, gsKeys )
    IF lnP = 0 THEN DO
      CALL Issue 005, gsKey
      SIGNAL Done
    END

    /* =================================================================
    -- Get (possibly quoted) value
    */
    IF Left( gsArgs, 1 ) = '"' THEN DO
      PARSE VAR gsArgs 2 gsVal '"' gsArgs
    END
    ELSE DO
      PARSE VAR gsArgs gsVal gsArgs
    END

    /* =================================================================
    -- Go process it
    */
    CALL SpringBoard
  END

  /* ===================================================================
  -- Initialize control variables now that translation tables are loaded
  */
  CALL InitializeCntl

  /* ===================================================================
  -- Running as a PIPE stage within CMS?
  */
  IF gsSystem = "CMS" & gsEnv = "?" THEN DO

    /* =================================================================
    -- Get state of input stream
    */
    "STREAMSTATE INPUT"

    /* =================================================================
    -- Input stream connected?
    */
    IF RC = 12 THEN DO
      /* ===============================================================
      -- No input stream so IN is required
      */
      IF gsFileI = gsNull THEN DO
        CALL Issue 156
        SIGNAL Done
      END

      /* ===============================================================
      -- Add an input stage (convient to put here...)
      */
      "ADDPIPE <" gsFileI "| *.INPUT:"
      gsFileI = gsNull
    END
    ELSE DO
      /* ===============================================================
      -- Have an input stream so IN must be omitted
      */
      IF gsFileI <> gsNull THEN DO
        CALL Issue 157
        SIGNAL Done
      END
    END

    /* =================================================================
    -- Get state of output stream
    */
    "STREAMSTATE OUTPUT"

    /* =================================================================
    -- Output stream connected?
    */
    IF RC = 12 THEN DO
      /* ===============================================================
      -- No output stream so OUT is required
      */
      IF gsFileO = gsNull THEN DO
        CALL Issue 158
        SIGNAL Done
      END

      /* ===============================================================
      -- Add an output stage (convient to put here...)
      */
      "ADDPIPE *.OUTPUT: | >" gsFileO
      gsFileO = gsNull
    END
    ELSE DO
      /* ===============================================================
      -- Have an output stream so OUT must be omitted
      */
      IF gsFileO <> gsNull THEN DO
        CALL Issue 159
        SIGNAL Done
      END
    END
  END
  ELSE IF gsSystem = "OMVS" THEN DO
    /* =================================================================
    -- Use standard streams if either IN or OUT not specified
    */
    IF gsFileI = gsNull THEN DO
      gsFileI = "/dev/fd0"
    END
    IF gsFileO = gsNull THEN DO
      gsFileO = "/dev/fd1"
    END
  END
  ELSE IF gsSystem = "REGINA" THEN DO
    /* =================================================================
    -- Use standard streams if either IN or OUT not specified
    */
    IF gsFileI = gsNull THEN DO
      gsFileI = "<stdin>"
    END
    IF gsFileO = gsNull THEN DO
      gsFileO = "<stdout>"
    END
  END
  ELSE IF gsSystem = "UNI-REXX" THEN DO
    /* =================================================================
    -- Use standard streams if either IN or OUT not specified
    -- (Null values will use stdin/stdout so this is a NOP)
    */
    NOP
  END
  ELSE IF gsSystem = "OOREXX" THEN DO
    /* =================================================================
    -- Use standard streams if either IN or OUT not specified
    */
    IF gsFileI = gsNull THEN DO
      gsFileI = "STDIN"
    END
    IF gsFileO = gsNull THEN DO
      gsFileO = "STDOUT"
    END
  END
  ELSE DO
    /* =================================================================
    -- All the rest require both specifications
    */
    IF gsFileI = gsNull | gsFileO = gsNull THEN DO
      CALL Issue 006
      SIGNAL Done
    END
  END

  /* ===================================================================
  -- We're done if we're only validating parameters
  */
  IF gbVOnly THEN DO
    gnRC = 0
    SIGNAL Done
  END

  /* ===================================================================
  -- Default font set?
  */
  IF gnFont = 0 THEN DO
    gnFont = FontSpec( gsFont )
  END

  /* ===================================================================
  -- If CC not specified, set default to NO or base on RECFM
  -- under TSO.
  */
  IF gsCC = gsNull THEN DO
    gsCC = "NO"
    IF gsSystem = "TSO" THEN DO
      IF Left( Translate( gsFileI ), 3 ) = "DD:" THEN DO
        CALL Listdsi Substr( gsFileI, 4 ) "FILE"
      END
      ELSE DO
        CALL Listdsi gsFileI
      END
      IF SYSREASON = 0 & Verify( "AM", SYSRECFM, "M" ) > 0 THEN DO
        gsCC = "YES"
      END
    END
  END

  /* ===================================================================
  -- Display execution information
  */
  CALL Issue 007, gsVersion
  CALL Issue 008, gsFileI
  CALL Issue 009, gsFileO
  CALL Issue 111, gsUnits
  IF gnCompLvl <> 0 THEN DO
    CALL Issue 124, gnCompLvl
  END
  CALL Issue 010, Format( gnPaperW / gnUnits, ,1 )
  CALL Issue 011, Format( gnPaperH / gnUnits, ,1 )
  IF gsPaperT <> gsNull THEN DO
    CALL Issue 012, gsPaperT
  END
  IF gsPaperS <> gsNull THEN DO
    CALL Issue 013, gsPaperS
  END
  IF gsOrient = "L" THEN DO
    CALL Issue 014, "Landscape"
  END
  ELSE DO
    CALL Issue 014, "Portrait"
  END
  CALL Issue 016, Format( gnMarginL / gnUnits, ,1 )
  CALL Issue 017, Format( gnMarginR / gnUnits, ,1 )
  CALL Issue 018, Format( gnMarginT / gnUnits, ,1 )
  CALL Issue 019, Format( gnMarginB / gnUnits, ,1 )
  DO lnI = 1 TO gnBackC
    CALL Issue 020, gaBackT.lnI
    CALL Issue 021, gaBackP.lnI
  END
  CALL Issue 022, gaFontb.gnFont
  CALL Issue 023, gnPoints
  CALL Issue 024, gnZoom
  CALL Issue 025, gnLPI
  CALL Issue 026, gsColorFT
  CALL Issue 027, gsColorBT
  IF gsCC <> "NO" THEN DO
    CALL Issue 028, gsCC
  END
  IF gbHasOL THEN DO
    CALL Issue 030, gsOType
  END
  IF gsDInfo <> gsNull THEN DO
    CALL Issue 031, gsDInfo
  END
  IF gsHLQ <> gsNull THEN DO
    CALL Issue 032, gsHLQ
  END
  IF gbEncrypt THEN DO
    CALL Issue 029, gnCryptLen * 8, gsPPerms
    IF gsGennedPw <> gsNull THEN DO
      CALL Issue 062, gsGennedPw
    END
  END
  IF gaCfgs.0 <> 0 THEN DO
    CALL Issue 113
  END
  DO lnI = 1 TO gnImgCnt
    CALL Issue 132, Substr( gaImgReg.lnI, 2 ),,
                    gaImgT.lnI,,
                    gaImgW.lnI,,
                    gaImgH.lnI
  END

  /* ===================================================================
  -- Convert it
  */
  CALL Convert

  /* ===================================================================
  -- Display post conversion information
  */
  CALL Issue 033, gnPageL
  CALL Issue 034, gnPageR
  CALL Issue 035, gnPageT
  CALL Issue 036, gnPageB
  CALL Issue 037, gnPageW
  CALL Issue 038, gnPageH

  CALL Issue 039, gnCropL
  CALL Issue 040, gnCropR
  CALL Issue 041, gnCropT
  CALL Issue 042, gnCropB
  CALL Issue 043, gnCropW
  CALL Issue 044, gnCropH

  CALL Issue 045, gnTextL
  CALL Issue 046, gnTextR
  CALL Issue 047, gnTextT
  CALL Issue 048, gnTextB
  CALL Issue 049, gnTextW
  CALL Issue 050, gnTextH

  CALL Issue 051, gnLeading
  CALL Issue 052, gnLPP

  CALL Issue 053, gsBarColorF
  CALL Issue 054, gsBarColorB

  CALL Issue 077

  lsCPct = gsNull
  IF gnCompLvl <> 0 THEN DO
    lsCPct = "(" ||,
             100 - ( ( gnOffset * 100 ) % gnTotBytes ) ||,
             "% reduction)"
  END

  CALL Issue 091, gnOffset, lsCPct, gnContentObjs

  /* ===================================================================
  -- Set good completion and exit
  */
  gnRC = 0
  SIGNAL Done

/*
-- =====================================================================
-- Routine:   Usage            : Displays usage information
-- Arguments: (none)           : none required
-- Return:                     : none required
-- =====================================================================
*/
Usage:

  DO lnuI = 1 TO Sourceline()
    IF Word( Sourceline( lnuI ), 2 ) = "Usage:" THEN DO
      LEAVE
    END
  END

  DO lnuI = lnuI TO Sourceline() UNTIL lsuL = gsNull
    lsuL = Substr( Strip( Sourceline( lnuI ) ), 3 )
    CALL Issue gnMsgE, lsuL
  END

RETURN

/*
-- =====================================================================
-- Routine:   Done             : Display messages and exit
-- Arguments: (none)           : none required
-- Return:                     : none required
-- =====================================================================
*/
Done:

  CALL DisplayMsgs

  /* ===================================================================
  -- Terminate environment if under MVS et al
  */
  IF gsSystem = "TSO" | gsSystem = "OMVS" THEN DO
    CALL T2PTerm
  END

EXIT gnRC

/*
-- =====================================================================
-- Routine:   SpringBoard      : Jumps to parameter validator
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
SpringBoard:
  SIGNAL VALUE "ARG" || gsKey

/*
-- =====================================================================
-- Routine:   ArgIN            : Process IN command parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgIN:
  gsFileI = gsVal

  /* ===================================================================
  -- Remove periods under CMS
  */
  IF gsSystem = "CMS" THEN DO
    gsFileI = Translate( gsFileI, " ", "." )
  END

RETURN

/*
-- =====================================================================
-- Routine:   ArgOUT           : Validate OUT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgOUT:
  gsFileO = gsVal

  /* ===================================================================
  -- Remove periods and possibly add filemode under CMS
  */
  IF gsSystem = "CMS" THEN DO
    gsFileO = Translate( gsFileO, " ", "." )

    IF Words( gsFileO ) = 2 THEN DO
      gsFileO = gsFileO "A"
    END
  END

RETURN

/*
-- =====================================================================
-- Routine:   ArgANNOT         : Validate ANNOT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgANNOT:

  PARSE VAR gsVal lsTyp (gsDelim) lsPag (gsDelim) lsRow (gsDelim),
                  lsCol (gsDelim) lsSt8 (gsDelim) lsTxt

  /* ===================================================================
  -- Process the type argument
  */
  lsNames = "TEXT T"

  lnW = Wordpos( Translate( lsTyp ), lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 100, lsTyp, lsNames
    SIGNAL Done
  END
  gsAnnTyp = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

  /* ===================================================================
  -- Process the page argument
  */
  IF lsPag <> gsNull THEN DO
    lsNames = "FIRST F LAST L ALL A"
    lnW = Wordpos( Translate( lsPag ), lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 101, lsPag, lsNames
      SIGNAL Done
    END
    gsAnnPag = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )
  END

  /* ===================================================================
  -- Process the row argument
  */
  IF lsRow <> gsNull THEN DO
    IF Datatype( lsRow, "W" ) = gbFalse THEN DO
      CALL Issue 102, lsRow
      SIGNAL Done
    END
    gnAnnRow = lsRow
  END

  /* ===================================================================
  -- Process the column argument
  */
  IF lsCol <> gsNull THEN DO
    IF Datatype( lsCol, "W" ) = gbFalse THEN DO
      CALL Issue 103, lsCol
      SIGNAL Done
    END
    gnAnnCol = lsCol
  END

  /* ===================================================================
  -- Process the state argument
  */
  IF lsSt8 <> gsNull THEN DO
    lsNames = "OPEN O CLOSED C"
    lsVals  = "true false"

    lnW = Wordpos( Translate( lsSt8 ), lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 104, lsSt8, lsNames
      SIGNAL Done
    END
    gsAnnSt8 = Word( lsVals, ( lnW + 1 ) % 2 )
  END

  /* ===================================================================
  -- Process the text argument
  */
  gsAnnTxt = lsTxt

  DROP lsNames lsVals lsTyp lsPag lsRow lsCol lsTxt

RETURN

/*
-- =====================================================================
-- Routine:   ArgBG            : Validate BG parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgBG:

  PARSE VAR gsVal lsType (gsDelim) lsParm

  /* ===================================================================
  -- Process the type argument
  */
  lsType = Translate( lsType )
  lsNames = "STREAM TEXTMARK TMARK"

  lnW = Wordpos( lsType, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 064, lsType, lsNames
    SIGNAL Done
  END

  /* ===================================================================
  -- Dealing with a TEXTMARK
  */
  IF lsType = "TEXTMARK" | lsType = "TMARK" THEN DO

    /* =================================================================
    -- Parse based on type.  (TMARK allows font size or name/size)
    */
    lsN = "Courier"
    lsP = 100
    IF lsType = "TEXTMARK" THEN DO
        PARSE VAR lsParm lsS (gsDelim) lsF (gsDelim) lsB (gsDelim),
                         lsO (gsDelim) lsT
    END
    ELSE DO
        PARSE VAR lsParm lsS (gsDelim) lsF (gsDelim) lsB (gsDelim),
                         lsO (gsDelim) lsP (gsDelim) lsT

        /* =============================================================
        -- Check for new font name argument.
        */
        IF Datatype( lsP, "N" ) = gbFalse THEN DO
          PARSE VAR lsT lsX1 (gsDelim) lsX2
          IF Datatype( lsX1, "N" ) = gbTrue THEN DO
            lsN = lsP
            lsP = lsX1
            lsT = lsX2
          END
        END

        lsType = "TEXTMARK"
    END

    /* =================================================================
    -- Process the style argument
    */
    IF lsS = gsNull THEN DO
      lsS = "BU"
    END

    lsS = Translate( lsS )
    lsNames = "BOTTOMUP BU BUBOXED BUB TOPDOWN TD TDBOXED TDB"

    lnW = Wordpos( lsS, lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 065, lsS, lsNames
      SIGNAL Done
    END
    lsS = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

    /* =================================================================
    -- Process the foreground color argument
    */
    IF lsF <> gsNull THEN DO
      lsF = ColorSpec( lsF )
    END

    /* =================================================================
    -- Process the background color argument
    */
    IF lsB <> gsNull THEN DO
      lsB = ColorSpec( lsB )
    END

    /* =================================================================
    -- Process the opacity (alpha) argument
    */
    IF lsO <> gsNull THEN DO
      IF Datatype( lsO, "N" ) = gbFalse THEN DO
        CALL Issue 066, lsO
        SIGNAL Done
      END

      IF lsO < 0 | lsO > 100 THEN DO
        CALL Issue 147, lsO
        SIGNAL Done
      END

      gsAlphaBG = AddAlpha( lsO )
    END

    /* =================================================================
    -- Font name
    */
    IF lsN <> gsNull THEN DO
      lsN = FontSpec( lsN )
    END

    /* =================================================================
    -- Process the font size argument
    */
    IF Datatype( lsP, "N" ) = gbFalse THEN DO
      CALL Issue 129, lsP
      SIGNAL Done
    END

    /* =================================================================
    -- Set the default color if neither is specified
    */
    IF lsF = gsNull & lsB = gsNull THEN DO
      lsB = ColorSpec( "Gray" )
    END

    lsParm = lsS || (gsDelim) || lsF || (gsDelim) || lsB ||,
             (gsDelim) || lsP || (gsDelim) || lsN ||,
             (gsDelim) || lsT
  END
  ELSE DO
    gbBackS = gbTrue
  END

  gnBackC = gnBackC + 1
  gaBackT.gnBackC = lsType
  gaBackP.gnBackC = lsParm

  DROP lsNames lnW lsType lsParm lsD lsF lsB lsP lsT

RETURN

/*
-- =====================================================================
-- Routine:   ArgBM            : Validate BM parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgBM:

  IF Datatype( gsVal, "N" ) = gbFalse THEN DO
    CALL Issue 070, gsVal
    SIGNAL Done
  END

  gnMarginB = Points( gsVal )

RETURN

/*
-- =====================================================================
-- Routine:   ArgBROWSE        : Validate BROWSE parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgBROWSE:

  PARSE UPPER VAR gsVal lsBrowse

  lsNames = "YES Y NO N"

  lnW = Wordpos( lsBrowse, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 108, lsBrowse, lsNames
    SIGNAL Done
  END

  gbBrowse = ( Left( lsBrowse, 1 ) = "Y" )

  DROP lsNames lnW lsBrowse

RETURN

/*
-- =====================================================================
-- Routine:   ArgCC            : Validate CC parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgCC:

  PARSE UPPER VAR gsVal lsCC

  lsNames = "YES Y NO N ASCII A"

  lnW = Wordpos( lsCC, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 072, lsCC, lsNames
    SIGNAL Done
  END

  gsCC = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

  DROP lsNames lnW lsCC

RETURN

/*
-- =====================================================================
-- Routine:   ArgCOLOR         : Validate COLOR parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgCOLOR:

  PARSE VAR gsVal lsFore (gsDelim) lsBack

  /* ===================================================================
  -- Process the foreground color
  */
  IF lsFore <> gsNull THEN DO
    gsColorF = ColorSpec( lsFore )
    gsColorFT = lsFore
  END

  /* ===================================================================
  -- Process the background color
  */
  IF lsBack <> gsNull THEN DO
    gsColorB = ColorSpec( lsBack )
    gsColorBT = lsBack
  END

  DROP lsFore lsBack

RETURN

/*
-- =====================================================================
-- Routine:   ArgCOMPRESS      : Validate COMPRESS parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgCOMPRESS:

  PARSE VAR gsVal lsL

  IF Datatype( lsL, "W" ) = gbFalse THEN DO
    CALL Issue 096, lsL
    SIGNAL Done
  END

  IF lsL < 0 | lsL > 9 THEN DO
    CALL Issue 097, lsL
    SIGNAL Done
  END

  IF lsL > 0 & Wordpos( gsSystem, "TSO OMVS CMS" ) = 0 THEN DO
    CALL Issue 095
    SIGNAL Done
  END

  gnCompLvl = lsL

  DROP lsL

RETURN

/*
-- =====================================================================
-- Routine:   ArgCONFIG        : Configuration filespec
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgCONFIG:

  /* ===================================================================
  -- Remember name
  */
  gaCfgs.0 = gaCfgs.0 + 1
  lnacI = gaCfgs.0
  gaCfgs.lnacI = gsVal

  /* ===================================================================
  -- Open the file
  */
  lsacH = ReadOpen( gsVal )

  /* ===================================================================
  -- Prefix arg string with config file contents
  */
  lsacCfg = gsNull
  lsacCont = " "
  DO UNTIL laacCfg.0 = 0
    CALL ReadFile lsacH, "laacCfg."

    DO lnacI = 1 TO laacCfg.0
      laacCfg.lnacI = Strip( laacCfg.lnacI )

      IF Left( laacCfg.lnacI, 1 ) = "*" THEN DO
        ITERATE
      END

      lsacCfg = lsacCfg || lsacCont || laacCfg.lnacI
      lsacCont = " "
      IF Right( lsacCfg, 2 ) = " -" THEN DO
        lsacCfg = Left( lsacCfg, Length( lsacCfg ) - 2 )
        lsacCont = gsNull
      END
      ELSE IF Right( lsacCfg, 1 ) = "+" THEN DO
        lsacCfg = Left( lsacCfg, Length( lsacCfg ) - 1 )
        lsacCont = gsNull
      END

    END
  END
  gsArgs = lsacCfg gsArgs

  /* ===================================================================
  -- Close it
  */
  CALL ReadClose lsacH

  DROP lsacH lnacI laacCfg. lsacCfg lsacCont

RETURN

/*
-- =====================================================================
-- Routine:   ArgCONFIRM       : Validate CONFIRM parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgCONFIRM:

  PARSE UPPER VAR gsVal lsConfirm

  lsNames = "YES Y NO N VERBOSE V DEBUG D"
  lsVals = gnMsgI gnMsgN gnMsgV

  lnW = Wordpos( lsConfirm, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 105, lsConfirm, lsNames
    SIGNAL Done
  END

  gnConfirm = Word( lsVals, ( lnW + 1 ) % 2 )

  DROP lsNames lnW lsConfirm lsLevels

RETURN

/*
-- =====================================================================
-- Routine:   ArgDEFCFG        : Validate DEFCFG parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgDEFCFG:

  /* ===================================================================
  -- Just ignore if the file isn't there or args were specified
  */
  IF gbNoArgs = gbTrue | Exists( gsVal ) = gbFalse THEN DO
    RETURN
  END

  /* ===================================================================
  -- Pass the ball to CONFIG
  */
  CALL ArgCONFIG

RETURN

/*
-- =====================================================================
-- Routine:   ArgDELIM         : Validate DELIM parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgDELIM:

  /* ===================================================================
  -- Set the new delimiter
  */
  IF Length( gsVal ) <> 1 THEN DO
    CALL Issue 171
    SIGNAL Done
  END

  gsDelim = gsVal

RETURN

/*
-- =====================================================================
-- Routine:   ArgDINFO         : Validate DINFO parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgDINFO:

  /* ===================================================================
  -- Save the name
  */
  gsDInfo = gsVal

  /* ===================================================================
  -- Open the file
  */
  lsadH = ReadOpen( gsDInfo )

  /* ===================================================================
  -- Just load the contents
  */
  lnadJ = 0
  DO UNTIL laadDInfo.0 = 0
    CALL ReadFile lsadH, "laadDInfo."
    DO lnadI = 1 TO laadDInfo.0
      lnadJ = lnadJ + 1
      gaDInfo.lnadJ = laadDInfo.lnadI
    END
  END
  gaDInfo.0 = lnadJ

  /* ===================================================================
  -- Close it
  */
  CALL ReadClose lsadH

  DROP lsadS lnadI lnadJ laadDInfo.

RETURN

/*
-- =====================================================================
-- Routine:   ArgDRAW          : Validate DRAW parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgDRAW:

  PARSE VAR gsVal lsType (gsDelim) lsParm

  /* ===================================================================
  -- Process the type argument
  */
  lsType = Translate( lsType )
  lsNames = "LINE RECT TEXT STRING"

  lnW = Wordpos( lsType, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 187, lsType, lsNames
    SIGNAL Done
  END

  /* ===================================================================
  -- Type specific processing
  */
  SELECT
    /* =================================================================
    -- Handle the LINE type
    */
    WHEN lsType = "LINE" THEN DO
      PARSE VAR lsParm lnSX (gsDelim) lnSY (gsDelim),
                       lnEX (gsDelim) lnEY (gsDelim),
                       lnO  (gsDelim) lsC  (gsDelim),
                       lnT  (gsDelim) lsCS (gsDelim),
                       lnDN (gsDelim) lnDF (gsDelim)

      /* ===============================================================
      -- Save graphics state
      */
      lsQ = "q"

      /* ===============================================================
      -- SX coordinate
      */
      IF Datatype( lnSX, "N" ) = gbFalse THEN DO
        CALL Issue 139, lnSX
        SIGNAL Done
      END

      /* ===============================================================
      -- SY coordinate
      */
      IF Datatype( lnSY, "N" ) = gbFalse THEN DO
        CALL Issue 140, lnSY
        SIGNAL Done
      END

      /* ===============================================================
      -- EX coordinate
      */
      IF Datatype( lnEX, "N" ) = gbFalse THEN DO
        CALL Issue 139, lnEX
        SIGNAL Done
      END

      /* ===============================================================
      -- EY coordinate
      */
      IF Datatype( lnEY, "N" ) = gbFalse THEN DO
        CALL Issue 140, lnEY
        SIGNAL Done
      END

      /* ===============================================================
      -- Opacity
      */
      IF lnO <> gsNull THEN DO
        IF Datatype( lnO, "N" ) = gbFalse THEN DO
          CALL Issue 066, lnO
          SIGNAL Done
        END
        IF lnO < 0 | lnO > 100 THEN DO
          CALL Issue 147, lnR
          SIGNAL Done
        END
        IF lnO <> 100 THEN DO
          lsQ = lsQ AddAlpha( lnO ) "gs"
        END
      END

      /* ===============================================================
      -- Color
      */
      IF lsC <> gsNull THEN DO
        lsC = ColorSpec( lsC )
        IF lsC <> ColorSpec( "black" ) THEN DO
          lsQ = lsQ lsC "RG"
        END
      END

      /* ===============================================================
      -- Line thickness
      */
      IF lnT <> gsNull THEN DO
        IF Datatype( lnT, "N" ) = gbFalse THEN DO
          CALL Issue 150, lnT
          SIGNAL Done
        END
        IF lnT < 0 THEN DO
          CALL Issue 151, lnT
          SIGNAL Done
        END
        IF lnT <> 0 THEN DO
          lsQ = lsQ lnT "w"
        END
      END

      /* ===============================================================
      -- Cap style
      */
      IF lsCS <> gsNull THEN DO
        lsCS = Translate( lsCS )
        lsNames = "BUTT ROUND SQUARE"

        lnW = Wordpos( lsCS, lsNames )
        IF lnW = 0 THEN DO
          CALL Issue 152, lsCS, lsNames
          SIGNAL Done
        END
        IF lnW <> 1 THEN DO
          lsQ = lsQ lnW - 1 "J"
        END
      END

      /* ===============================================================
      -- Dots on
      */
      IF lnDN <> gsNull THEN DO
        IF Datatype( lnDN, "N" ) = gbFalse THEN DO
          CALL Issue 153, lnDN
          SIGNAL Done
        END
        lnDN = lnDN * 1.0
      END
      ELSE DO
        lnDN = 0.0
      END

      /* ===============================================================
      -- Dots off
      */
      IF lnDF <> gsNull THEN DO
        IF Datatype( lnDF, "N" ) = gbFalse THEN DO
          CALL Issue 154, lnDF
          SIGNAL Done
        END
        lnDF = lnDF * 1.0
      END

      lsDA = lnDN lnDF
      IF lsDA <> "0.0" & lsDA <> "0.0 0.0" THEN DO
        lsQ = lsQ "[" lsDA "] 0 d"
      END

      /* ===============================================================
      -- Draw, stroke, and restore state
      */
      lsQ = lsQ lnSX lnSY "m",
                lnEX lnEY "l",
                "s",
                "Q"
    END

    /* =================================================================
    -- Handle the RECT type
    */
    WHEN lsType = "RECT" THEN DO
      PARSE VAR lsParm lnX  (gsDelim) lnY  (gsDelim),
                       lnWi (gsDelim) lnHe (gsDelim),
                       lnO  (gsDelim) lsF  (gsDelim),
                       lsB  (gsDelim) lnT  (gsDelim),
                       lsJS (gsDelim) lnDN (gsDelim),
                       lnDF (gsDelim)

      /* ===============================================================
      -- Save graphics state
      */
      lsQ = "q"

      /* ===============================================================
      -- X coordinate
      */
      IF Datatype( lnX, "N" ) = gbFalse THEN DO
        CALL Issue 139, lnX
        SIGNAL Done
      END

      /* ===============================================================
      -- Y coordinate
      */
      IF Datatype( lnY, "N" ) = gbFalse THEN DO
        CALL Issue 140, lnY
        SIGNAL Done
      END

      /* ===============================================================
      -- Width
      */
      IF Datatype( lnWi, "N" ) = gbFalse THEN DO
        CALL Issue 148, lnWi
        SIGNAL Done
      END

      /* ===============================================================
      -- Height
      */
      IF Datatype( lnHe, "N" ) = gbFalse THEN DO
        CALL Issue 149, lnHe
        SIGNAL Done
      END

      /* ===============================================================
      -- Opacity
      */
      IF lnO <> gsNull THEN DO
        IF Datatype( lnO, "N" ) = gbFalse THEN DO
          CALL Issue 066, lnO
          SIGNAL Done
        END
        IF lnO < 0 | lnO > 100 THEN DO
          CALL Issue 147, lnR
          SIGNAL Done
        END
        IF lnO <> 100 THEN DO
          lsQ = lsQ AddAlpha( lnO ) "gs"
        END
      END

      /* ===============================================================
      -- Foreground color
      */
      lnF = 0
      lnB = 0
      IF lsF <> gsNull THEN DO
        lsF = ColorSpec( lsF )
        IF lsF <> ColorSpec( "black" ) THEN DO
          lsQ = lsQ lsF "RG"
        END
        lnF = 1
      END

      /* ===============================================================
      -- Background color
      */
      IF lsB <> gsNull THEN DO
        lsB = ColorSpec( lsB )
        IF lsB <> ColorSpec( "black" ) THEN DO
          lsQ = lsQ lsB "rg"
        END
        lnB = 2
      END

      /* ===============================================================
      -- Painting operator
      */
      lsPaint = Word( "s s f b", lnF + lnB + 1 )

      /* ===============================================================
      -- Line thickness
      */
      IF lnT <> gsNull THEN DO
        IF Datatype( lnT, "N" ) = gbFalse THEN DO
          CALL Issue 150, lnT
          SIGNAL Done
        END
        IF lnT < 0 THEN DO
          CALL Issue 151, lnT
          SIGNAL Done
        END
        IF lnT <> 0 THEN DO
          lsQ = lsQ lnT "w"
        END
      END

      /* ===============================================================
      -- Join style
      */
      IF lsJS <> gsNull THEN DO
        lsJS = Translate( lsJS )
        lsNames = "MITER ROUND BEVEL"

        lnW = Wordpos( lsJS, lsNames )
        IF lnW = 0 THEN DO
          CALL Issue 155, lsJS, lsNames
          SIGNAL Done
        END
        IF lnW <> 1 THEN DO
          lsQ = lsQ lnW - 1 "j"
        END
      END

      /* ===============================================================
      -- Dots on
      */
      IF lnDN <> gsNull THEN DO
        IF Datatype( lnDN, "N" ) = gbFalse THEN DO
          CALL Issue 153, lnDN
          SIGNAL Done
        END
        lnDN = lnDN * 1.0
      END
      ELSE DO
        lnDN = 0.0
      END

      /* ===============================================================
      -- Dots off
      */
      IF lnDF <> gsNull THEN DO
        IF Datatype( lnDF, "N" ) = gbFalse THEN DO
          CALL Issue 154, lnDF
          SIGNAL Done
        END
        lnDF = lnDF * 1.0
      END

      lsDA = lnDN lnDF
      IF lsDA <> "0.0" & lsDA <> "0.0 0.0" THEN DO
        lsQ = lsQ "[" lsDA "] 0 d"
      END

      /* ===============================================================
      -- Draw, stroke, and restore state
      */
      lsQ = lsQ lnX lnY lnWi lnHe "re",
                lsPaint,
                "Q"
    END

    /* =================================================================
    -- Handle the TEXT type
    */
    WHEN lsType = "TEXT" | lsType = "STRING" THEN DO
      PARSE VAR lsParm lnX  (gsDelim) lnY  (gsDelim),
                       lsFn (gsDelim) lnFs (gsDelim),
                       lnO  (gsDelim) lsF  (gsDelim),
                       lsB  (gsDelim) lnT  (gsDelim),
                       lsJS (gsDelim) lnR  (gsDelim),
                       lnZ  (gsDelim) lsText

      /* ===============================================================
      -- Save graphics state and start text state
      */
      lsQ = "q BT"

      /* ===============================================================
      -- X coordinate
      */
      IF Datatype( lnX, "N" ) = gbFalse THEN DO
        CALL Issue 139, lnX
        SIGNAL Done
      END

      /* ===============================================================
      -- Y coordinate
      */
      IF Datatype( lnY, "N" ) = gbFalse THEN DO
        CALL Issue 140, lnY
        SIGNAL Done
      END

      /* ===============================================================
      -- Font name
      */
      IF lsFn = gsNull THEN DO
        lsFn = "Courier"
      END

      lnFn = FontSpec( lsFn )

      /* ===============================================================
      -- Font size
      */
      IF lnFs <> gsNull THEN DO
        IF Datatype( lnFs, "N" ) = gbFalse THEN DO
          CALL Issue 060, lnFs
          SIGNAL Done
        END
      END
      ELSE DO
        lnFs = 9
      END

      /* ===============================================================
      -- Opacity
      */
      IF lnO <> gsNull THEN DO
        IF Datatype( lnO, "N" ) = gbFalse THEN DO
          CALL Issue 066, lnO
          SIGNAL Done
        END
        IF lnO < 0 | lnO > 100 THEN DO
          CALL Issue 147, lnR
          SIGNAL Done
        END
        IF lnO <> 100 THEN DO
          lsQ = lsQ AddAlpha( lnO ) "gs"
        END
      END

      /* ===============================================================
      -- Foreground color
      */
      lnRm = 2
      IF lsF <> gsNull THEN DO
        lsF = ColorSpec( lsF )
        IF lsF <> ColorSpec( "black" ) THEN DO
          lsQ = lsQ lsF "RG"
        END
        IF lsB = gsNull THEN DO
          lnRm = 1
        END
      END

      /* ===============================================================
      -- Background color
      */
      IF lsB <> gsNull THEN DO
        lsB = ColorSpec( lsB )
        IF lsB <> ColorSpec( "black" ) THEN DO
          lsQ = lsQ lsB "rg"
        END
        IF lsF = gsNull THEN DO
          lnRm = 0
        END
      END

      /* ===============================================================
      -- Rendering mode
      */
      IF lnRm <> 0 THEN DO
        lsQ = lsQ lnRm "Tr"
      END

      /* ===============================================================
      -- Line thickness
      */
      IF lnT <> gsNull THEN DO
        IF Datatype( lnT, "N" ) = gbFalse THEN DO
          CALL Issue 150, lnT
          SIGNAL Done
        END
        IF lnT < 0 THEN DO
          CALL Issue 151, lnT
          SIGNAL Done
        END
        IF lnT <> 0 THEN DO
          lsQ = lsQ lnT "w"
        END
      END

      /* ===============================================================
      -- Join style
      */
      IF lsJS <> gsNull THEN DO
        lsJS = Translate( lsJS )
        lsNames = "MITER ROUND BEVEL"

        lnW = Wordpos( lsJS, lsNames )
        IF lnW = 0 THEN DO
          CALL Issue 155, lsJS, lsNames
          SIGNAL Done
        END
        IF lnW <> 1 THEN DO
          lsQ = lsQ lnW - 1 "j"
        END
      END

      /* ===============================================================
      -- Rotation
      */
      lnCos = 1
      lnSin = 0
      IF lnR <> gsNull THEN DO
        IF Datatype( lnR, "N" ) = gbFalse THEN DO
          CALL Issue 141, lnR
          SIGNAL Done
        END
        IF lnR < -360 | lnR > 360 THEN DO
          CALL Issue 142, lnR
          SIGNAL Done
        END
        lnCos = Cos( Rad( lnR ) )
        lnSin = Sin( Rad( lnR ) )
      END

      /* ===============================================================
      -- Rotation and position
      */
      lsQ = lsQ lnCos lnSin (-lnSin) lnCos lnX lnY "Tm"

      /* ===============================================================
      -- Zoom
      */
      IF lnZ <> gsNull THEN DO
        IF Datatype( lnZ, "N" ) = gbFalse THEN DO
          CALL Issue 063, lnZ
          SIGNAL Done
        END
        lsQ = lsQ lnZ "Tz"
      END

      /* ===============================================================
      -- Text
      */
      lsText = Strftime( lsText )
      IF Strip( lsText ) = gsNull THEN DO
        CALL Issue 160
        SIGNAL Done
      END

      /* ===============================================================
      -- Rest is based on type
      */
      IF lsType = "TEXT" THEN DO
        /* =============================================================
        -- Add text
        */
        lsQ = lsQ || "@" || lnFn lnFs lsText
      END
      ELSE DO
        /* =============================================================
        -- Track STRINGs separately
        */
        gnStrCnt = gnStrCnt + 1
        gaString.gnStrCnt = lsQ || "@" || lnFn lnFs lsText
        lsQ = gsNull
      END
    END

    /* =================================================================
    -- Force an error
    */
    OTHERWISE DO
      SAY did_not_account_for_all_cases_in_select
    END
  END

  /* ===================================================================
  -- Store
  */
  gnDrawCnt = gnDrawCnt + 1
  gaDrawT.gnDrawCnt = lsType
  gaDraw.gnDrawCnt = lsQ

  DROP lsFunc lsName lsF lnN lnX lnY lnO lnR lncX lncY lnkX lnkY

RETURN

/*
-- =====================================================================
-- Routine:   ArgENCODING      : Validate ENCODING parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgENCODING:

  PARSE VAR gsVal lsType (gsDelim) lsParm

  /* ===================================================================
  -- Process the type argument
  */
  lsType = Translate( lsType )
  lsNames = "DEFINE"

  lnW = Wordpos( lsType, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 177, lsType, lsNames
    SIGNAL Done
  END

  /* ===================================================================
  -- Type specific processing
  */
  SELECT
    WHEN lsType = "DEFINE" THEN DO
      PARSE VAR lsParm lsName (gsDelim) lsFile

      CALL DEncoding lsName, lsFile

      DROP lsName lsFile
    END

    WHEN lsType = "USE" THEN DO
      CALL SEncoding lsParm
    END

    /* =================================================================
    -- Force an error
    */
    OTHERWISE DO
      SAY did_not_account_for_all_cases_in_select
    END
  END

  DROP lsType lsParm lsNames lnW

RETURN

/*
-- =====================================================================
-- Routine:   ArgENCRYPT       : Validate ENCRYPT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgENCRYPT:

  IF Wordpos( gsSystem, "TSO OMVS CMS" ) = 0 THEN DO
    CALL Issue 084
    SIGNAL Done
  END

  PARSE VAR gsVal lsT (gsDelim) lsO (gsDelim) lsU (gsDelim),
                  lsL (gsDelim) lsF

  IF lsT <> gsNull THEN DO
    lsT = Translate( lsT )
    lsNames = "STANDARD ST"

    lnW = Wordpos( lsT, lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 080, lsT, lsNames
      SIGNAL Done
    END
  END

  IF lsO <> gsNull THEN DO
    gsOwnerPw = lsO
  END

  IF lsU <> gsNull THEN DO
    gsUserPw = lsU
  END

  IF lsL <> gsNull THEN DO
    IF Datatype( lsL, "W" ) = gbFalse THEN DO
      CALL Issue 081, lsL
      SIGNAL Done
    END

    IF lsL <> 40 & lsL <> 128 THEN DO
      CALL Issue 082, lsL
      SIGNAL Done
    END

    gnCryptLen = lsL / 8
    gnCryptLenX = gnCryptLen + 5
  END

  lsNames = "NOPRINT       NP ",
            "NOEDIT        NE ",
            "NOCOPY        NC ",
            "NOEDITNOTES   NEN",
            "NOSAVEAS      NSA",
            "NOFILLANDSIGN NFS",
            "NOTACCESSIBLE NAC",
            "NOASSEMBLY    NAS",
            "NOHIRESPRINT  NHP",
            "NONE          N  ",

  lsVals = gnEPPrint    ,
           gnEPEdit     ,
           gnEPCopy     ,
           gnEPNotes    ,
           gnEPSaveAs   ,
           gnEPFill     ,
           gnEPAccess   ,
           gnEPAssemble ,
           gnEPHires    ,
           gnEPAll

  DO WHILE lsF <> gsNull
    PARSE VAR lsF lsOpt (gsDelim) lsF

    lnW = Wordpos( Translate( lsOpt ), lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 083, lsOpt, lsNames
      SIGNAL Done
    END

    lnOpt = Word( lsVals, ( lnW + 1 ) % 2 )
    gsPerms = Bitand( gsPerms, D2c( -1 - lnOpt, 4 ) )
  END

  lsPerms = "Print",
            "Edit",
            "Copy",
            "EditNotes",
            "SaveAs",
            "FillAndSign",
            "Accessible",
            "Assembly",
            "HiresPrint"

  DO lnW = 1 TO Words( lsVals ) - 1
    lsOpt = D2c( Word( lsVals, lnW ), 4 )
    IF Bitand( gsPerms, lsOpt ) == lsOpt THEN DO
      gsPPerms = gsPPerms || Word( lsPerms, lnW ) || " "
    END
  END

  IF gsPPerms = gsNull THEN DO
    gsPPerms = "None"
  END

  gbEncrypt = gbTrue

  CALL CryptInit

  DROP lsNames lsPretty lsVals lnOpt lnW lsT lsO lsU lsL lsF

RETURN

/*
-- =====================================================================
-- Routine:   ArgFONT          : Validate FONT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgFONT:

  PARSE UPPER VAR gsVal lnSize (gsDelim) lsFont (gsDelim) lnZoom

  /* ===================================================================
  -- Font size
  */
  IF lnSize <> gsNull THEN DO
    IF Datatype( lnSize, "N" ) = gbFalse THEN DO
      CALL Issue 060, lnSize
      SIGNAL Done
    END

    gnPoints = lnSize
  END

  /* ===================================================================
  -- Font name
  */
  IF lsFont <> gsNull THEN DO
    gnFont = FontSpec( lsFont )
  END

  /* ===================================================================
  -- Font zoom
  */
  IF lnZoom <> gsNull THEN DO
    IF Datatype( lnZoom, "N" ) = gbFalse THEN DO
      CALL Issue 063, lnZoom
      SIGNAL Done
    END

    gnZoom = lnZoom
  END

  DROP lsFont lnSize lnZoom lsName

RETURN

/*
-- =====================================================================
-- Routine:   ArgHLQ           : Validate HLQ parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgHLQ:

  PARSE UPPER VAR gsVal lsHLQ

  gsHLQ = lsHLQ

  DROP lsHLQ

RETURN

/*
-- =====================================================================
-- Routine:   ArgIDX           : Validate IDX parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgIDX:

  gsArgs = "OUTLINE rowcol/" || gsVal gsArgs

RETURN

/*
-- =====================================================================
-- Routine:   ArgIFEMPTY       : Validate IFEMPTY parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgIFEMPTY:

  /* ===================================================================
  -- Just save the value.  It will be processed later.
  */
  gsEmpty = gsVal

RETURN

/*
-- =====================================================================
-- Routine:   ArgIMAGE         : Validate IMAGE parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgIMAGE:

  PARSE VAR gsVal lsFunc (gsDelim) lsName (gsDelim) lsParm

  /* ===================================================================
  -- Process the function argument
  */
  lsFunc = Translate( lsFunc )
  lsNames = "LOAD DRAW TILE"

  lnW = Wordpos( lsFunc, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 134, lsFunc, lsNames
    SIGNAL Done
  END

  /* ===================================================================
  -- Must have a name
  */
  IF lsName = gsNull THEN DO
    CALL Issue 135
    SIGNAL Done
  END
  lsName = "!" || lsName
  lnN = gaImgReg.lsName

  /* ===================================================================
  -- Function specific processing
  */
  SELECT
    /* =================================================================
    -- Process the "LOAD" function
    */
    WHEN lsFunc = "LOAD" THEN DO
      PARSE VAR lsParm lsF

      /* ===============================================================
      -- Already have one with this name?
      */
      IF lnN <> 0 THEN DO
        CALL Issue 136, Substr( lsName, 2 )
        SIGNAL Done
      END

      /* ===============================================================
      -- Register it
      */
      lnN = gnImgCnt + 1
      gnImgCnt = lnN
      gaImgReg.lsName = lnN
      gaImgReg.lnN = lsName

      /* ===============================================================
      -- Load the image
      */
      CALL LoadFile lsF, "gaImg."lnN

      /* ===============================================================
      -- Verify it's a type we can handle (only JPEG for now)
      */
      IF IsJPEG( lnN ) = gbFalse THEN DO
        IF IsBMP( lnN ) = gbFalse THEN DO
          CALL Issue 137, Substr( lsName, 2 )
          SIGNAL Done
        END
      END
    END

    /* =================================================================
    -- Process the "DRAW" function
    */
    WHEN lsFunc = "DRAW" THEN DO
      PARSE VAR lsParm lnX  (gsDelim) lnY  (gsDelim),
                       lnO  (gsDelim) lnR  (gsDelim),
                       lncX (gsDelim) lncY (gsDelim),
                       lnkX (gsDelim) lnkY (gsDelim),
                       lnS  (gsDelim) lnI

      /* ===============================================================
      -- This name registered?
      */
      IF lnN = 0 THEN DO
        CALL Issue 138, Substr( lsName, 2 )
        SIGNAL Done
      END

      /* ===============================================================
      -- X coordinate
      */
      IF Datatype( lnX, "N" ) = gbFalse THEN DO
        CALL Issue 139, lnX
        SIGNAL Done
      END

      /* ===============================================================
      -- Y coordinate
      */
      IF Datatype( lnY, "N" ) = gbFalse THEN DO
        CALL Issue 140, lnY
        SIGNAL Done
      END

      /* ===============================================================
      -- Opacity
      */
      IF lnO <> gsNull THEN DO
        IF Datatype( lnO, "N" ) = gbFalse THEN DO
          CALL Issue 066, lnO
          SIGNAL Done
        END
        IF lnO < 0 | lnO > 100 THEN DO
          CALL Issue 147, lnO
          SIGNAL Done
        END
      END
      ELSE DO
        lnO = 100
      END

      /* ===============================================================
      -- Rotation
      */
      IF lnR <> gsNull THEN DO
        IF Datatype( lnR, "N" ) = gbFalse THEN DO
          CALL Issue 141, lnR
          SIGNAL Done
        END
        IF lnR < -360 | lnR > 360 THEN DO
          CALL Issue 142, lnR
          SIGNAL Done
        END
      END
      ELSE DO
        lnR = 0
      END

      /* ===============================================================
      -- X scale
      */
      IF lncX <> gsNull THEN DO
        IF Datatype( lncX, "N" ) = gbFalse THEN DO
          CALL Issue 143, lncX
          SIGNAL Done
        END
      END
      ELSE DO
        lncX = 100
      END

      /* ===============================================================
      -- Y scale
      */
      IF lncY <> gsNull THEN DO
        IF Datatype( lncY, "N" ) = gbFalse THEN DO
          CALL Issue 144, lncY
          SIGNAL Done
        END
      END
      ELSE DO
        lncY = 100
      END

      /* ===============================================================
      -- X skew
      */
      IF lnkX <> gsNull THEN DO
        IF Datatype( lnkX, "N" ) = gbFalse THEN DO
          CALL Issue 145, lnkX
          SIGNAL Done
        END
      END
      ELSE DO
        lnkX = 0
      END

      /* ===============================================================
      -- Y skew
      */
      IF lnkY <> gsNull THEN DO
        IF Datatype( lnkY, "N" ) = gbFalse THEN DO
          CALL Issue 146, lnkY
          SIGNAL Done
        END
      END
      ELSE DO
        lnkY = 0
      END

      /* ===============================================================
      -- Starting page
      */
      IF lnS <> gsNull THEN DO
        IF Datatype( lnkY, "W" ) = gbFalse THEN DO
          CALL Issue 161, lnS
          SIGNAL Done
        END
      END
      ELSE DO
        lnS = 1
      END

      /* ===============================================================
      -- And every X pages
      */
      IF lnI <> gsNull THEN DO
        IF Datatype( lnI, "W" ) = gbFalse THEN DO
          CALL Issue 162, lnI
          SIGNAL Done
        END
      END
      ELSE DO
        lnI = 1
      END

      /* ===============================================================
      -- Store
      */
      IF lnS = 1 & lnI = 1 THEN DO
        gnDrawCnt = gnDrawCnt + 1
        gaDrawT.gnDrawCnt = "PICT"
        gaDraw.gnDrawCnt = lnN lnX lnY lnO lnR lncX lncY,
                           lnkX lnkY
      END
      ELSE DO
        gnDICnt = gnDICnt + 1
        gaDIStart.gnDICnt = lnS
        gaDINext.gnDICnt = lnI
        gaDrawIfT.gnDICnt = "PICT"
        gaDrawIf.gnDICnt = lnN lnX lnY lnO lnR lncX lncY,
                           lnkX lnkY
      END
    END

    /* =================================================================
    -- Process the "TILE" function
    */
    WHEN lsFunc = "TILE" THEN DO
      PARSE VAR lsParm lnX  (gsDelim) lnY  (gsDelim),
                       lnWi (gsDelim) lnHe (gsDelim),
                       lnO  (gsDelim)

      /* ===============================================================
      -- This name registered?
      */
      IF lnN = 0 THEN DO
        CALL Issue 138, Substr( lsName, 2 )
        SIGNAL Done
      END

      /* ===============================================================
      -- X coordinate
      */
      IF Datatype( lnX, "N" ) = gbFalse THEN DO
        CALL Issue 139, lnX
        SIGNAL Done
      END

      /* ===============================================================
      -- Y coordinate
      */
      IF Datatype( lnY, "N" ) = gbFalse THEN DO
        CALL Issue 140, lnY
        SIGNAL Done
      END

      /* ===============================================================
      -- Width
      */
      IF Datatype( lnWi, "N" ) = gbFalse THEN DO
        CALL Issue 148, lnWi
        SIGNAL Done
      END

      /* ===============================================================
      -- Height
      */
      IF Datatype( lnHe, "N" ) = gbFalse THEN DO
        CALL Issue 149, lnHe
        SIGNAL Done
      END

      /* ===============================================================
      -- Opacity
      */
      IF lnO <> gsNull THEN DO
        IF Datatype( lnO, "N" ) = gbFalse THEN DO
          CALL Issue 066, lnO
          SIGNAL Done
        END
        IF lnO < 0 | lnO > 100 THEN DO
          CALL Issue 147, lnO
          SIGNAL Done
        END
      END
      ELSE DO
        lnO = 100
      END

      /* ===============================================================
      -- Store
      */
      gnDrawCnt = gnDrawCnt + 1
      gaDrawT.gnDrawCnt = "TILE"
      gaDraw.gnDrawCnt = lnN lnX lnY lnWi lnHe lnO
    END

    /* =================================================================
    -- Force an error
    */
    OTHERWISE DO
      SAY did_not_account_for_all_cases_in_select
    END
  END

  DROP lsFunc lsName lsF lnN lnX lnY lnO lnR lncX lncY lnkX lnkY
  DROP lnS lnI lnWi lnHe

RETURN

/*
-- =====================================================================
-- Routine:   ArgLM            : Validate LM parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgLM:

  IF Datatype( gsVal, "N" ) = gbFalse THEN DO
    CALL Issue 067, gsVal
    SIGNAL Done
  END

  gnMarginL = Points( gsVal )

RETURN

/*
-- =====================================================================
-- Routine:   ArgLPI           : Validate LPI parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgLPI:

  IF Datatype( gsVal, "N" ) = gbFalse THEN DO
    CALL Issue 071, gsVal
    SIGNAL Done
  END

  gnLPI = gsVal

RETURN

/*
-- =====================================================================
-- Routine:   ArgMAG           : Validate MAG parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgMAG:

  PARSE UPPER VAR gsVal lsMag

  lsNames = "FITWIDTH FW FITINWINDOW FI FITVISIBLE FV"

  IF Datatype( lsMag, "N" ) THEN DO
    gsMag = "XYZ -32768 -32768" lsMag / 100
  END
  ELSE DO
    lnW = Wordpos( Translate( lsMag ), lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 133, lsOpt, lsNames
      SIGNAL Done
    END

    lsOpt = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

    SELECT
      WHEN lsOpt = "FITWIDTH" THEN DO
        gsMag = "FitH -32768"
      END

      WHEN lsOpt = "FITINWINDOW" THEN DO
        gsMag = "Fit"
      END

      WHEN lsOpt = "FITVISIBLE" THEN DO
        gsMag = "FitBH -32768"
      END

      OTHERWISE DO
        SAY did_not_account_for_all_cases_in_select
      END
    END
  END

  DROP lsNames lnW lsOpt

RETURN

/*
-- =====================================================================
-- Routine:   ArgMAXOSIZE      : Validate MAXOSIZE parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgMAXOSIZE:

  IF Datatype( gsVal, "N" ) = gbFalse THEN DO
    CALL Issue 071, gsVal
    SIGNAL Done
  END

  gnMaxSize = gsVal

RETURN

/*
-- =====================================================================
-- Routine:   ArgMSGID         : Validate MSGID parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgMSGID:

  PARSE UPPER VAR gsVal lsMsgid

  /* ===================================================================
  -- Process the msgid argument
  */
  lsNames = "YES Y NO N"

  lnW = Wordpos( lsMsgid, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 106, lsMsgid, lsNames
    SIGNAL Done
  END

  gbMsgid = ( Left( lsMsgid, 1 ) = "Y" )

  DROP lsNames lnW lsMsgid

RETURN

/*
-- =====================================================================
-- Routine:   ArgMSGLVL        : Validate MSGLVL parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgMSGLVL:

  PARSE UPPER VAR gsVal lsMsg (gsDelim) lsLvl

  /* ===================================================================
  -- Process the message number argument
  */
  IF Datatype( lsMsg, "W" ) = gbFalse THEN DO
    CALL Issue 125, lsMsg
    SIGNAL Done
  END

  /* ===================================================================
  -- Pad it
  */
  lsMsg = Right( lsMsg, 3, "0" )

  /* ===================================================================
  -- Process the level argument
  */
  lsNames = "ERROR E INFO I VERBOSE V"

  lnW = Wordpos( lsLvl, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 126, lsLvl, lsNames
    SIGNAL Done
  END

  /* ===================================================================
  -- Set message level override
  */
  gaMsgTOver.lsMsg = Left( lsLvl, 1 )

  DROP lsNames lnW lsMsg lsLvl

RETURN

/*
-- =====================================================================
-- Routine:   ArgOLOPT         : Validate OLOPT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgOLOPT:

  DO UNTIL gsVal = gsNull
    PARSE VAR gsVal lsOpt (gsDelim) gsVal

    lsNames = "COLOR C ALTCOLOR A ITALIC I",
              "BOLD B EXPAND E FULL F DUPS D",
              "SHOW S"

    lnW = Wordpos( Translate( lsOpt ), lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 085, lsOpt, lsNames
      SIGNAL Done
    END

    lsOpt = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

    SELECT
      WHEN lsOpt = "COLOR" THEN DO
        PARSE VAR gsVal lsColor (gsDelim) gsVal

        gaOColor.1 = ColorSpec( lsColor )
        gaOColor.2 = gaOColor.1
      END

      WHEN lsOpt = "ALTCOLOR" THEN DO
        PARSE VAR gsVal lsColor (gsDelim) gsVal

        gaOColor.2 = ColorSpec( lsColor )
      END

      WHEN lsOpt = "ITALIC" THEN DO
        gnOFlags = Bitor( gnOFlags, 1 )
      END

      WHEN lsOpt = "BOLD" THEN DO
        gnOFlags = Bitor( gnOFlags, 2 )
      END

      WHEN lsOpt = "EXPAND" THEN DO
        gnOExpand = 1
      END

      WHEN lsOpt = "FULL" THEN DO
        gbOFull = gbTrue
      END

      WHEN lsOpt = "DUPS" THEN DO
        gbODups = gbTrue
      END

      WHEN lsOpt = "SHOW" THEN DO
        gbOShow = gbTrue
      END

      OTHERWISE DO
        SAY did_not_account_for_all_cases_in_select
      END
    END
  END

RETURN

/*
-- =====================================================================
-- Routine:   ArgOLSORT        : Validate OLSORT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgOLSORT:

  PARSE UPPER VAR gsVal lsDir

  IF lsDir <> "A" & lsDir <> "D" THEN DO
    CALL Issue 086, lsDir
    SIGNAL Done
  END

  gbOSort = gbTrue
  gbOSDir = ( lsDir = "D" )

  DROP lsDir

RETURN

/*
-- =====================================================================
-- Routine:   ArgOLTOK         : Validate OLTOK parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgOLTOK:

  /* ===================================================================
  -- Verify that the value isn't null (by using "" as arg).  Be
  -- careful here.  The "==" is intentional.
  */
  IF gsVal == gsNull THEN DO
    CALL Issue 087
    SIGNAL Done
  END

  gsOTok = gsVal
  gnOTokLen = Length( gsOTok )

RETURN

/*
-- =====================================================================
-- Routine:   ArgORIENT        : Validate ORIENT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgORIENT:

  PARSE UPPER VAR gsVal lsOrient

  lsNames = "LANDSCAPE LAND L PORTRAIT PORT P"

  lnW = Wordpos( lsOrient, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 059, lsOrient, lsNames
    SIGNAL Done
  END

  gsOrient = Left( lsOrient, 1 )

  DROP lsNames lnW lsOrient

RETURN

/*
-- =====================================================================
-- Routine:   ArgOUTLINE       : Validate OUTLINE parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgOUTLINE:

  PARSE VAR gsVal lsType (gsDelim) lsCntl

  lsNames = "ROWCOL RC SCANROW SR SCANCOL SC SCAN S"

  lnW = Wordpos( Translate( lsType ), lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 073, lsType, lsNames
    SIGNAL Done
  END

  gsOType = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )
  SELECT
    WHEN gsOType = "ROWCOL" THEN DO

      PARSE VAR lsCntl lnRow (gsDelim) lnCol (gsDelim) lnLen

      IF Datatype( lnRow, "W" ) = gbFalse THEN DO
        CALL Issue 074, lnRow
        SIGNAL Done
      END

      IF Datatype( lnCol, "W" ) = gbFalse THEN DO
        CALL Issue 075, lnCol
        SIGNAL Done
      END

      IF Datatype( lnLen, "W" ) = gbFalse THEN DO
        CALL Issue 076, lnLen
        SIGNAL Done
      END

      gnORow = lnRow
      gnOCol = lnCol
      gnOLen = lnLen
      gnODCol = lnCol
      gnODLen = lnLen
    END

    /* =================================================================
    -- Look for key in given column on any row
    */
    WHEN gsOType = "SCANCOL" THEN DO

      PARSE VAR lsCntl lnCol (gsDelim) lnDCol (gsDelim),
                       lnDLen (gsDelim) lsKey

      IF Datatype( lnCol, "W" ) = gbFalse THEN DO
        CALL Issue 075, lnCol
        SIGNAL Done
      END

      IF Datatype( lnDCol, "W" ) = gbFalse THEN DO
        CALL Issue 078, lnDCol
        SIGNAL Done
      END

      IF Datatype( lnDLen, "W" ) = gbFalse THEN DO
        CALL Issue 079, lnDLen
        SIGNAL Done
      END

      gbOColRel = ( Left( lnDCol, 1 ) = "+" |,
                    Left( lnDCol, 1 ) = "-" )

      gnOCol = lnCol
      gnODCol = lnDCol + 1
      gnODLen = lnDLen
      gsOKey = lsKey
      gnOKeyL = Length( gsOKey )
    END

    /* =================================================================
    -- Look for key in any column on given row
    */
    WHEN gsOType = "SCANROW" THEN DO

      PARSE VAR lsCntl lnRow (gsDelim) lnDCol (gsDelim),
                       lnDLen (gsDelim) lsKey

      IF Datatype( lnRow, "W" ) = gbFalse THEN DO
        CALL Issue 074, lnRow
        SIGNAL Done
      END

      IF Datatype( lnDCol, "W" ) = gbFalse THEN DO
        CALL Issue 078, lnDCol
        SIGNAL Done
      END

      IF Datatype( lnDLen, "W" ) = gbFalse THEN DO
        CALL Issue 079, lnDLen
        SIGNAL Done
      END

      gbOColRel = ( Left( lnDCol, 1 ) = "+" |,
                    Left( lnDCol, 1 ) = "-" )

      gnORow = lnRow
      gnODCol = lnDCol + 1
      gnODLen = lnDLen
      gsOKey = lsKey
      gnOKeyL = Length( gsOKey )
    END

    /* =================================================================
    -- Look for key in any column on any row
    */
    WHEN gsOType = "SCAN" THEN DO

      PARSE VAR lsCntl lnDCol (gsDelim) lnDLen (gsDelim) lsKey

      IF Datatype( lnDCol, "W" ) = gbFalse THEN DO
        CALL Issue 078, lnDCol
        SIGNAL Done
      END

      IF Datatype( lnDLen, "W" ) = gbFalse THEN DO
        CALL Issue 079, lnDLen
        SIGNAL Done
      END

      gbOColRel = ( Left( lnDCol, 1 ) = "+" |,
                    Left( lnDCol, 1 ) = "-" )

      gnODCol = lnDCol + 1
      gnODLen = lnDLen
      gsOKey = lsKey
      gnOKeyL = Length( gsOKey )
    END

    OTHERWISE DO
      SAY did_not_account_for_all_cases_in_select
    END
  END

  gbHasOL = gbTrue

  DROP lnRow lnCol lnLen

  DROP lsType lsCntl lsNames lnW

RETURN

/*
-- =====================================================================
-- Routine:   ArgOUTREC        : Validate OUTREC parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgOUTREC:

  IF Datatype( gsVal, "W" ) = gbFalse THEN DO
    CALL Issue 088, gsVal
    SIGNAL Done
  END

  gnORLen = gsVal

RETURN

/*
-- =====================================================================
-- Routine:   ArgPAGE          : Validate PAGE parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgPAGE:

  PARSE UPPER VAR gsVal lsMode (gsDelim) lsLayout

  lsNames = "NONE N OUTLINE O THUMBS T FULL F"
  lsVals  = "UseNone UseOutlines UseThumbs FullScreen"

  IF lsMode <> gsNull THEN DO
    lnW = Wordpos( lsMode, lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 089, lsMode, lsNames
      SIGNAL Done
    END

    gsPageM = Word( lsVals, ( lnW + 1 ) % 2 )
  END

  IF lsLayout <> gsNull THEN DO
    lsNames = "SINGLEPAGE SP ONECOLUMN OC TWOCOLUMNLEFT TCL",
               "TWOCOLUMNRIGHT TCR"
    lsVals  = "SinglePage OneColumn",
              "TwoColumnLeft TwoColumnRight"

    lnW = Wordpos( lsLayout, lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 090, lsLayout, lsNames
      SIGNAL Done
    END

    gsPageL = Word( lsVals, ( lnW + 1 ) % 2 )
  END

  DROP lsNames lsVals lnW lsMode lsLayout

RETURN

/*
-- =====================================================================
-- Routine:   ArgPAPER         : Validate PAPER parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgPAPER:

  PARSE UPPER VAR gsVal lsSize (gsDelim) lsType (gsDelim) lsStyle

  /* ===================================================================
  -- Paper size
  */
  lsNames = "LETTER LET LEGAL LEG  A4  A4 AUTO AUTO"

  lnW = Wordpos( lsSize, lsNames )
  IF lnW > 0 THEN DO

    lsP = "612 792 612 1008 595 842 9999 9999"
    gnPaperW = Word( lsP, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )
    gnPaperH = Word( lsP, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 2 )

    IF lsSize = "AUTO" THEN DO
      gbPaperA = gbTrue
    END
  END
  ELSE IF lsSize <> gsNull THEN DO
    PARSE UPPER VAR lsSize lnWidth "X" lnHeight

    IF Datatype( lnWidth, "N" ) = gbFalse THEN DO
      CALL Issue 055, lnWidth
      SIGNAL Done
    END

    IF Datatype( lnHeight, "N" ) = gbFalse THEN DO
      CALL Issue 056, lnHeight
      SIGNAL Done
    END

    gnPaperW = Points( lnWidth )
    gnPaperH = Points( lnHeight )

  END

  /* ===================================================================
  -- Paper Type
  */
  IF lsType <> gsNull THEN DO
    lsNames = "BLUEBAR GRAYBAR GREENBAR ORANGEBAR WHITEBAR",
              "YELLOWBAR"
    IF Wordpos( lsType, lsNames ) = 0 THEN DO
      CALL Issue 057, lsType, lsNames
      SIGNAL Done
    END

    gsPaperT = lsType
  END

  /* ===================================================================
  -- Paper Style
  */
  IF lsStyle <> gsNull THEN DO
    lsNames = "HOLED"
    IF Wordpos( lsStyle, lsNames ) = 0 THEN DO
      CALL Issue 058, lsStyle, lsNames
      SIGNAL Done
    END

    gsPaperS = lsStyle
  END

  DROP lsNames lnW lsSizes lsSize lsType lsStyle

RETURN

/*
-- =====================================================================
-- Routine:   ArgRM            : Validate RM parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgRM:

  IF Datatype( gsVal, "N" ) = gbFalse THEN DO
    CALL Issue 069, gsVal
    SIGNAL Done
  END

  gnMarginR = Points( gsVal )

RETURN

/*
-- =====================================================================
-- Routine:   ArgTEXT          : Validate TEXT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgTEXT:

  PARSE VAR gsVal lsType (gsDelim) lsParm

  lsNames = "ATTR A"

  lnW = Wordpos( Translate( lsType ), lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 163, lsType, lsNames
    SIGNAL Done
  END
  lsType = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

  gnTACnt = gnTACnt + 1
  gaTextA.gnTACnt = lsType

  SELECT
    WHEN lsType = "ATTR" THEN DO

      PARSE VAR lsParm lsFn (gsDelim) lnFs (gsDelim) lsF (gsDelim),
                       lsType (gsDelim) lsT

      /* ===============================================================
      -- Font name
      */
      lnFn = gsNull
      IF lsFn <> gsNull THEN DO
        lnFn = FontSpec( lsFn )
      END

      /* ===============================================================
      -- Font size
      */
      IF lnFs <> gsNull THEN DO
        IF Datatype( lnFs, "N" ) = gbFalse THEN DO
          CALL Issue 060, lnFs
          SIGNAL Done
        END
      END

      /* ===============================================================
      -- Foreground color
      */
      IF lsF <> gsNull THEN DO
        lsF = ColorSpec( lsF )
      END

      gaTextA.gnTACnt = gaTextA.gnTACnt "/" lnFn "/" lnFs "/" lsF

      /* ===============================================================
      -- Process scan arguments
      */
      lsNames = "ROWCOL RC SCAN S"

      lnW = Wordpos( Translate( lsType ), lsNames )
      IF lnW = 0 THEN DO
        CALL Issue 164, lsType, lsNames
        SIGNAL Done
      END
      lsType = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

      gaTextC.gnTACnt = lsType

      SELECT
        WHEN lsType = "SCAN" THEN DO

          IF lsT = gsNull THEN DO
            CALL Issue 160
            SIGNAL Done
          END

          gaTextC.gnTACnt = gaTextC.gnTACnt,
                            Length( lsT ),
                            lsT
        END

        WHEN lsType = "ROWCOL" THEN DO
          PARSE VAR lsT lnRow (gsDelim) lnCol (gsDelim) lnLen

          IF Datatype( lnRow, "W" ) = gbFalse THEN DO
            CALL Issue 166, lnRow
            SIGNAL Done
          END

          IF Datatype( lnCol, "W" ) = gbFalse THEN DO
            CALL Issue 167, lnCol
            SIGNAL Done
          END

          IF Datatype( lnLen, "W" ) = gbFalse THEN DO
            CALL Issue 168, lnLen
            SIGNAL Done
          END

          gaTextC.gnTACnt = gaTextC.gnTACnt,
                            lnRow,
                            lnCol,
                            lnLen

          DROP lnRow lnCol lnLen
        END

        OTHERWISE DO
          SAY did_not_account_for_all_cases_in_select
        END
      END

      DROP lnFn lnFs lsF lsT
    END

    OTHERWISE DO
      SAY did_not_account_for_all_cases_in_select
    END
  END

  DROP lsType lsNames lnW

RETURN

/*
-- =====================================================================
-- Routine:   ArgTM            : Validate TM parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgTM:

  IF Datatype( gsVal, "N" ) = gbFalse THEN DO
    CALL Issue 068, gsVal
    SIGNAL Done
  END

  gnMarginT = Points( gsVal )

RETURN

/*
-- =====================================================================
-- Routine:   ArgTRANS         : Validate TRANS parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgTRANS:

  PARSE UPPER VAR gsVal lsS (gsDelim) lsD (gsDelim) lsOpts

  lsNames = "BLINDS BL BOX BO DISSOLVE D GLITTER G",
            "REPLACE R SPLIT S WIPE W"
  lsVals  = "Blinds Box Disolve Glitter R Split Wipe"

  lnW = Wordpos( lsS, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 092, lsS, lsNames
    SIGNAL Done
  END

  gsTransS = Word( lsVals, ( lnW + 1 ) % 2 )

  IF lsD <> gsNull THEN DO
    IF Datatype( lsD, "N" ) = gbFalse THEN DO
      CALL Issue 093, lsD
      SIGNAL Done
    END

    gsTransD = lsD
  END

  DO WHILE lsOpts <> gsNull
    PARSE UPPER VAR lsOpts lsOpt (gsDelim) lsOpts

    SELECT
      WHEN gsTransS = "Split" THEN DO
        lsNames = "HORIZONTAL H VERTICAL V INWARD I OUTWARD O"
        IF Wordpos( lsOpt, lsNames ) = 0 THEN DO
            CALL Issue 094, lsOpt, gsTrans, lsNames
            SIGNAL Done
        END

        lsOpt = Left( lsOpt, 1 )
        IF lsOpt = "I" | lsOpt = "O" THEN DO
          gsTransM = lsOpt
        END
        ELSE DO
          gsTransDm = lsOpt
        END
      END

      WHEN gsTransS = "Blinds" THEN DO
        lsNames = "HORIZONTAL H VERTICAL V"
        IF Wordpos( lsOpt, lsNames ) = 0 THEN DO
          CALL Issue 094, lsOpt, gsTransS, lsNames
          SIGNAL Done
        END

        gsTransDm = Left( lsOpt, 1 )
      END

      WHEN gsTransS = "Box" THEN DO
        lsNames = "INWARD I OUTWARD O"
        IF Wordpos( lsOpt, lsNames ) = 0 THEN DO
          CALL Issue 094, lsOpt, gsTransS, lsNames
          SIGNAL Done
        END

        gsTransM = Left( lsOpt, 1 )
      END

      WHEN gsTransS = "Wipe" THEN DO
        lsNames = "0 L2R 90 B2T 180 R2L 270 T2B"
        lnW = Wordpos( lsOpt, lsNames )
        IF lnW = 0 THEN DO
          CALL Issue 094, lsOpt, gsTransS, lsNames
          SIGNAL Done
        END

        gsTransDi = Word( lsNames,,
                          ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )
      END

      WHEN gsTransS = "Glitter" THEN DO
        lsNames = "0 L2R 270 T2B 315 TL2BR"
        lnW = Wordpos( lsOpt, lsNames )
        IF lnW = 0 THEN DO
          CALL Issue 094, lsOpt, gsTransS, lsNames
          SIGNAL Done
        END

        gsTransDi = Word( lsNames,,
                          ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )
      END

      OTHERWISE DO
        NOP
      END
    END
  END

  DROP lsNames lsVals lnW lsS lsD lsOpts lsOpt

RETURN

/*
-- =====================================================================
-- Routine:   ArgUNITS         : Validate UNITS parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgUNITS:

  PARSE UPPER VAR gsVal lsOpt

  lsNames = "INCHES I CENTIMETERS C"

  lnW = Wordpos( lsOpt, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 110, lsOpt, lsNames
    SIGNAL Done
  END

  gsUnits = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

  gnUnits = 72
  IF Left( gsUnits, 1 ) = "C" THEN DO
    gnUnits = gnUnits * ( 1 / 2.54 )
  END

  DROP lsNames lnW lsOpt

RETURN

/*
-- =====================================================================
-- Routine:   ArgVIEWER        : Validate VIEWER parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgVIEWER:

  lsNames = "HIDETOOLBAR HT HIDEMENUBAR HM HIDEWINDOWUI HW",
            "FITWINDOW FW CENTERWINDOW CW DISPLAYDOCTITLE DD",
            "NONFULLSCREEN NF"
  lsVals  = "HideToolbar HideMenubar HideWindowUI FitWindow",
            "CenterWindow DisplayDocTitle NonFullScreenPageMode"

  lsNFNames = "NONE N OUTLINES O THUMBS T"
  lsNFVals  = "UseNone UseOutlines UseThumbs"

  DO UNTIL gsVal = gsNull
    PARSE UPPER VAR gsVal lsVal (gsDelim) gsVal

    lnW = Wordpos( lsVal, lsNames )
    IF lnW = 0 THEN DO
      CALL Issue 099, lsVal, lsNames
      SIGNAL Done
    END

    lsVal = Word( lsVals, ( lnW + 1 ) % 2 )
    IF lsVal = "NonFullScreenPageMode" THEN DO
      PARSE VAR gsVal lsVal (gsDelim) gsVal

      lnW = Wordpos( lsVal, lsNFNames )
      IF lnW = 0 THEN DO
        CALL Issue 128, lsVal, lsNFNames
        SIGNAL Done
      END

      gsViewer = gsViewer,
                 "/NonFullScreenPageMode/" ||,
                 Word( lsNFVals, ( lnW + 1 ) % 2 )
    END
    ELSE DO
      gsViewer = gsViewer,
                 "/" || Word( lsVals, ( lnW + 1 ) % 2 ),
                 "true"
    END
  END

  DROP lsNames lsVals lsNFNames lsNFVals lnW lsView

RETURN

/*
-- =====================================================================
-- Routine:   ArgVONLY         : Validate VONLY parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgVONLY:

  PARSE UPPER VAR gsVal lsOpt

  lsNames = "YES Y NO N"

  lnW = Wordpos( lsOpt, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 107, lsOpt, lsNames
    SIGNAL Done
  END

  gbVOnly = ( Left( lsOpt, 1 ) = "Y" )

  DROP lsNames lnW lsOpt

RETURN

/*
-- =====================================================================
-- Routine:   ArgXFONT         : Validate XFONT parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgXFONT:

  PARSE VAR gsVal lsType (gsDelim) lsParm

  /* ===================================================================
  -- Process the type argument
  */
  lsType = Translate( lsType )
  lsNames = "EXTERNAL E INTERNAL I TRUETYPE TT"

  lnW = Wordpos( lsType, lsNames )
  IF lnW = 0 THEN DO
    CALL Issue 098, lsType, lsNames
    SIGNAL Done
  END

  lsType = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )

  /* ===================================================================
  -- Font type
  */
  SELECT
    WHEN lsType = "EXTERNAL" | lsType = gsNull THEN DO
      PARSE VAR lsParm lsName (gsDelim) lsPath

      IF lsPath = gsNull THEN DO
        CALL Issue 127
        SIGNAL Done
      END

      CALL DFont lsName, gsNull, lsType, lsEnc, gsNull, lsPath

      DROP lsName lsPath
    END

    WHEN lsType = "INTERNAL" THEN DO
      PARSE VAR lsParm lsName (gsDelim) lsEnc (gsDelim) lsPath

      CALL DFont lsName, gsNull, lsType, lsEnc, gsNull, lsPath

      DROP lsName lsEnc lsPath
    END

    WHEN lsType = "TRUETYPE" THEN DO
      PARSE VAR lsParm lsName (gsDelim) lsEnc (gsDelim) ,
                       lsFlag (gsDelim) lsPath

      IF lsFlag <> gsNull THEN DO
        lsFlag = Translate( lsFlag )
        lsNames = "NOSUBSET NS NOEMBED NE"

        lnW = Wordpos( lsFlag, lsNames )
        IF lnW = 0 THEN DO
          CALL Issue 186, lsFlag, lsNames
          SIGNAL Done
        END

        lsFlag = Word( lsNames, ( ( ( lnW - 1 ) % 2 ) * 2 ) + 1 )
      END

      IF lsPath = gsNull THEN DO
        CALL Issue 127
        SIGNAL Done
      END

      CALL DFont lsName, gsNull, lsType, lsEnc, lsFlag, lsPath

      DROP lsName lsPath
    END

    OTHERWISE DO
      SAY did_not_account_for_all_cases_in_select
    END
  END

  DROP lsType lsNames lsParm lnW

RETURN

/*
-- =====================================================================
-- Routine:   ArgXLATE         : Validate XLATE parameter
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
ArgXLATE:

  /* ===================================================================
  -- Go define codepage tables
  */
  CALL DefineCPTables gsVal, gbTrue

RETURN

/*
-- =====================================================================
-- Routine:   Convert          : Convert the file
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
Convert:

  /* ===================================================================
  -- Paper Type
  */
  SELECT
    WHEN Right( gsPaperT, 3 ) = "BAR" THEN DO
      gbHasBG = gbTrue
      gbHasBars = gbTrue
      gsBarColorF = Colorspec( gsPaperT )
      PARSE VALUE gsBarColorF WITH lnR lnG lnB
      gsBarColorB = lnR - 0.1 lnG - 0.1 lnB - 0.1
    END

    OTHERWISE DO
      NOP
    END
  END

  /* ===================================================================
  -- Paper Style
  */
  SELECT
    WHEN gsPaperS = "HOLED" THEN DO
      gbHasBG = gbTrue
      gbHasHoles = gbTrue
    END

    OTHERWISE DO
      NOP
    END
  END

  /* ===================================================================
  -- External backgrounds?
  */
  IF gnBackC > 0 THEN DO
    gbHasBG = gbTrue
  END

  /* ===================================================================
  -- Color
  */
  IF gsColorB <> Colorspec( "White" ) THEN DO
    gbHasBG = gbTrue
  END

  /* ===================================================================
  -- Images
  */
  IF gnDrawCnt <> 0 | gnDICnt <> 0 THEN DO
    gbHasBG = gbTrue
  END

  /* ===================================================================
  -- Build transition dictionary
  */
  IF gsTransS <> gsNull THEN DO
    gsTrans = "/Type /Trans /S /" || gsTransS

    IF gsTransD <> gsNull THEN DO
      gsTrans = gsTrans "/D" gsTransD
    END

    IF gsTransDm <> gsNull THEN DO
      gsTrans = gsTrans "/Dm /" || gsTransDm
    END

    IF gsTransM <> gsNull THEN DO
      gsTrans = gsTrans "/M /" || gsTransM
    END

    IF gsTransDi <> gsNull THEN DO
      gsTrans = gsTrans "/Di" gsTransDi
    END
  END

  /* ===================================================================
  -- Go calculate boxen boundaries
  */
  CALL Boundaries

  /* ===================================================================
  -- Calculate various text metrics
  */
  gnLeading = 72 / gnLPI
  gnLPP = Trunc( gnTextH / gnLeading )

  /* ===================================================================
  -- Generate default text attribute
  */
  gaTextA.0 = gnFont gnPoints gsColorF

  /* ===================================================================
  -- Generate any additional attributes
  */
  DO lncI = 1 TO gnTACnt
    PARSE VAR gaTextA.lncI lscType "/" lscParm

    SELECT
      WHEN lscType = "ATTR" THEN DO

        PARSE VAR lscParm lncFn "/" lncFs "/" lscF

        /* =============================================================
        -- Font name and size
        */
        IF lncFn = gsNull THEN DO
          lncFn = gnFont
        END
        IF lncFs = gsNull THEN DO
          lncFs = gnPoints
        END

        /* =============================================================
        -- Text color
        */
        IF lscF = gsNull THEN DO
          lscF = gsColorF
        END

        /* =============================================================
        -- Replace attributes
        */
        gaTextA.lncI = lncFn lncFs lscF
      END

      OTHERWISE DO
        SAY did_not_account_for_all_cases_in_select
      END
    END
  END

  /* ===================================================================
  -- Open the output file
  */
  CALL WriteOpen

  /* ===================================================================
  -- Start the file
  */
  CALL QueueCntl "%PDF-1.4"
  CALL QueueCntl "%" || 'BBBFBFBB'x    /* <- Carefully chosen */

  /* ===================================================================
  -- Open the input file
  */
  lspcH = ReadOpen( gsFileI )
  lapcIn.0 = 0

  /* ===================================================================
  -- Does the file contain carriage control?
  */
  IF gsCC = "YES" THEN DO

    /* =================================================================
    -- Send the file while processing the CC characters.
    */
    DO lnpcI = 1 BY 1

      /* ===============================================================
      -- Read a set of records from the file or leave if none
      */
      IF lnpcI > lapcIn.0 THEN DO
        CALL ReadFile lspcH, "lapcIn."
        IF lapcIn.0 = 0 THEN DO
          LEAVE
        END
        lnpcI = 1
      END

      /* ===============================================================
      -- Strip and split
      */
      lsRec = Strip( lapcIn.lnpcI, "T" )
      PARSE VAR lsRec lsCC 2 lsRec

      /* ===============================================================
      -- Process the carriage control characters
      --
      -- Refer to "APPENDIX1.3.1  Machine Code" in "SC26-7337"
      -- for a description of the machine carriage controls.
      */
      SELECT
        /* =============================================================
        -- ISO/ANSI - Skip to channel 1 - Print
        */
        WHEN lsCC == "1" THEN DO
          CALL NewPage

          gsFeed = gsNextLine        /* Wrong, but compatable */
          gnLineCount = gnLineCount + 1

          CALL QueueText lsRec
        END

        /* =============================================================
        -- Machine Code - Print - Skip to channel 1
        */
        WHEN lsCC == '89'x THEN DO

          /* ===========================================================
          || Only print the line if we've started a page.  This
          || prevents an extra blank page at the beginning of a
          || run.
          */
          IF gbInPage THEN DO
            CALL QueueText lsRec
          END

            CALL NewPage
        END

        /* =============================================================
        -- Machine Code - No print - Skip to channel 1
        */
        WHEN lsCC == '8B'x THEN DO
          CALL NewPage
        END

        /* =============================================================
        -- ISO/ANSI - Space 3 lines - Print
        */
        WHEN lsCC == "-" THEN DO
          gsFeed = gsFeed || gsNextLine || gsNextLine || gsNextLine
          gnLineCount = gnLineCount + 3

          CALL QueueText lsRec
        END

        /* =============================================================
        -- Machine code - Print - Space 3 lines
        */
        WHEN lsCC == '19'x THEN DO
          CALL QueueText lsRec

          gsFeed = gsFeed || gsNextLine || gsNextLine || gsNextLine
          gnLineCount = gnLineCount + 3
        END

        /* =============================================================
        -- Machine code - No print - Space 3 lines
        */
        WHEN lsCC == '1B'x THEN DO
          gsFeed = gsFeed || gsNextLine || gsNextLine || gsNextLine
          gnLineCount = gnLineCount + 3
        END

        /* =============================================================
        -- ISO/ANSI - Space 2 lines - Print
        */
        WHEN lsCC == "0" THEN DO
          gsFeed = gsFeed || gsNextLine || gsNextLine
          gnLineCount = gnLineCount + 2

          CALL QueueText lsRec
        END

        /* =============================================================
        -- Machine code - Print - Space 2 lines
        */
        WHEN lsCC == '11'x THEN DO
          CALL QueueText lsRec

          gsFeed = gsFeed || gsNextLine || gsNextLine
          gnLineCount = gnLineCount + 2
        END

        /* =============================================================
        -- Machine code - No Print - Space 2 lines
        */
        WHEN lsCC == '13'x THEN DO
          gsFeed = gsFeed || gsNextLine || gsNextLine
          gnLineCount = gnLineCount + 2
        END

        /* =============================================================
        -- ISO/ANSI - Suppress space - Print
        */
        WHEN lsCC == "+" THEN DO
          CALL QueueText lsRec
        END

        /* =============================================================
        -- Machine code - Print - Suppress space
        */
        WHEN lsCC == '01'x THEN DO
          CALL QueueText lsRec
        END

        /* =============================================================
        -- Machine code - Suppress space - No print
        */
        WHEN lsCC == '03'x THEN DO
          /* CALL Queue gsReturn */
          NOP
        END

        /* =============================================================
        -- Single line - Print - Space 1 line
        */
        WHEN lsCC == '09'x THEN DO
          CALL QueueText lsRec

          gsFeed = gsFeed || gsNextLine
          gnLineCount = gnLineCount + 1
        END

        /* =============================================================
        -- Single line - No print - Space 1 line
        */
        WHEN lsCC == '0B'x THEN DO
          gsFeed = gsFeed || gsNextLine
          gnLineCount = gnLineCount + 1
        END

        /* =============================================================
        -- Single line - Space 1 line - Print
        */
        WHEN lsCC == " " THEN DO
          gsFeed = gsFeed || gsNextLine
          gnLineCount = gnLineCount + 1

          CALL QueueText lsRec
        END

        /* =============================================================
        -- Single line - Space 1 line - Print
        -- (Should distinguish between ASA and machine)
        */
        OTHERWISE DO
          gsFeed = gsFeed || gsNextLine
          gnLineCount = gnLineCount + 1

          CALL QueueText lsRec
        END
      END
    END
  END
  ELSE IF gsCC = "ASCII" THEN DO

    /* =================================================================
    -- Send the file while processing ASCII CC characters.
    */
    DO lnpcI = 1 BY 1

      /* ===============================================================
      -- Read a set of records from the file or leave if none
      */
      IF lnpcI > lapcIn.0 THEN DO
        CALL ReadFile lspcH, "lapcIn."
        IF lapcIn.0 = 0 THEN DO
          LEAVE
        END
        lnpcI = 1
      END

      /* ===============================================================
      -- Strip and terminate
      */
      lspcRec = Strip( lapcIn.lnpcI, "T", " " ) || '0A'x
      lspcLeft = gsNull

      /* ===============================================================
      -- Find all carriage control characters
      */
      DO UNTIL lnpcP = 0

        /* =============================================================
        -- Find the next character
        */
        lnpcP = Verify( lspcRec, '0C0D0A'x, "M" )
        IF lnpcP <> 0 THEN DO

          /* ===========================================================
          -- Break it up
          */
          lspcLeft = lspcLeft || Left( lspcRec, lnpcP - 1 )
          lspcC = Substr( lspcRec, lnpcP, 1 )
          lspcRec = Substr( lspcRec, lnpcP + 1 )

          /* ===========================================================
          -- Process CC
          */
          SELECT
            /* =========================================================
            -- Top of form - '0C'x
            */
            WHEN lspcC == '0C'x THEN DO
              IF lspcLeft <> gsNull THEN DO
                CALL QueueText lspcLeft

                gsFeed = gsFeed || gsNextLine
                gnLineCount = gnLineCount + 1

                lspcLeft = gsNull
              END

              CALL NewPage
            END

            /* =========================================================
            -- Carriage return - '0D'x
            */
            WHEN lspcC == '0D'x THEN DO
              NOP
            END

            /* =========================================================
            -- Line feed - '0A'x
            */
            WHEN lspcC == '0A'x THEN DO
              CALL QueueText lspcLeft

              gsFeed = gsFeed || gsNextLine
              gnLineCount = gnLineCount + 1

              lspcLeft = gsNull
            END

            /* =========================================================
            -- Ignore it
            */
            OTHERWISE DO
              NOP
            END
          END
        END
      END
    END
  END
  ELSE DO

    /* =================================================================
    -- Send each line of the file ASIS
    */
    DO lnpcI = 1 BY 1

      /* ===============================================================
      -- Read a set of records from the file or leave if none
      */
      IF lnpcI > lapcIn.0 THEN DO
        CALL ReadFile lspcH, "lapcIn."
        IF lapcIn.0 = 0 THEN DO
          LEAVE
        END
        lnpcI = 1
      END

      /* ===============================================================
      -- Write it
      */
      CALL QueueText Strip( lapcIn.lnpcI, "T" )

      gsFeed = gsFeed || gsNextLine
      gnLineCount = gnLineCount + 1
    END
  END

  /* ===================================================================
  -- Close the input file
  */
  CALL ReadClose lspcH

  /* ===================================================================
  -- Empty input?
  */
  IF gnContentObjs = 0 THEN DO
    SELECT
      WHEN Translate( gsEmpty ) = "ERROR" THEN DO
        CALL Issue 015, gsFileI
        SIGNAL Done
      END

      WHEN Translate( gsEmpty ) = "BLANK" THEN DO
        CALL Issue 130, gsFileI
        CALL QueueText ""
      END

      OTHERWISE DO
        CALL Issue 130, gsFileI
        CALL QueueText Strip( gsEmpty, "T" )
      END
    END
  END

  /* ===================================================================
  -- End the last page
  */
  CALL EndPage

  /* ===================================================================
  -- Generate background if needed
  */
  IF gbHasBG THEN DO
    CALL Background
  END

  /* ===================================================================
  -- Build image objects, if needed
  */
  IF gnImgCnt <> 0 THEN DO
    CALL Images
  END

  /* ===================================================================
  -- Build font objects
  */
  lscFonts = ""
  DO lnI = 1 TO gnFonts
    IF gbBackS = gbFalse & gaFontu.lnI = gbFalse THEN DO
      ITERATE
    END

    /* ===================================================================
    -- Write the ToUnicode cmap
    */
    CALL EmitEncoding gaFonte.lnI, gbTrue

    /* =================================================================
    -- Output the font objects
    */
    SELECT
      WHEN gaFontt.lnI = "TRUETYPE" THEN DO
        IF gaFont_Descriptor.lnI = 0 THEN DO
          CALL LoadTT gaFontp.lnI, lnI
        END

        /* =============================================================
        -- Create a new object
        */
        lncO = NewObj()

        CALL QueueCntl "<<"
        CALL QueueCntl "/Type /Font"
        CALL QueueCntl "/Subtype /TrueType"
        CALL QueueCntl "/FontDescriptor" gaFont_Descriptor.lnI "0 R"
        CALL QueueCntl "/BaseFont /" || gaFont_BaseFont.lnI
        CALL QueueCntl "/FirstChar" gaFont_FirstChar.lnI
        CALL QueueCntl "/LastChar" gaFont_LastChar.lnI

        lsWidths = ""
        DO lnJ = gaFont_FirstChar.lnI TO gaFont_LastChar.lnI
          lsWidths = lsWidths gaFont_Glyf_Width.lnI.lnJ
        END
        CALL QueueCntl "/Widths [" || Strip( lsWidths ) || "]"

        CALL EmitEncoding gaFonte.lnI, gbFalse

        CALL QueueCntl ">>"

        CALL EndObj
      END

      WHEN gaFontt.lnI = "EXTERNAL" THEN DO
        /* =============================================================
        -- Create a new object
        */
        lncO = NewObj()

        /* =============================================================
        -- Open the file
        */
        lscH = ReadOpen( gaFontp.lnI )

        /* =============================================================
        -- Copy file to output stream
        */
        DO UNTIL lacStream.0 = 0
          CALL ReadFile lscH, "lacStream."
          DO lncJ = 1 TO lacStream.0
            CALL QueueCntl lacStream.lncJ
          END
        END

        /* =============================================================
        -- Close it
        */
        CALL ReadClose lscH

        CALL EndObj

        DROP lscH lncJ lacStream. lscH
      END
      OTHERWISE DO
        /* =============================================================
        -- Create a new object
        */
        lncO = NewObj()

        CALL QueueCntl "<<"
        CALL QueueCntl "/Type /Font"
        CALL QueueCntl "/Subtype /Type1"
        CALL QueueCntl "/BaseFont /" || gaFontb.lnI
        CALL EmitEncoding gaFonte.lnI, gbFalse
        CALL QueueCntl ">>"

        CALL EndObj
      END
    END

    /* =================================================================
    -- Generate the font name
    */
    lscFonts = lscFonts || "/F" || lnI lncO "0 R"

    /* =================================================================
    -- Stream background style used?
    */
    IF gbBackS = gbFalse THEN DO
      ITERATE
    END

    /* =================================================================
    -- Generate aliases as well
    */
    DO lncJ = 1 TO Words( gaFontn.lnI )
      lscFonts = lscFonts || "/F" ||,
                 Word( gaFontn.lnI, lncJ ) lncO "0 R"
    END
  END

  /* ===================================================================
  -- Generate a resource object and save ID
  */
  gnResourceObj = NewObj()
  CALL QueueCntl "<<"
  CALL QueueCntl "/ProcSet [/PDF /Text /ImageC]"

  /* ===================================================================
  -- Don't need these just yet...
  --
  IF gbRGBSpace | gbGraySpace THEN DO
    CALL QueueCntl "/ColorSpace <<"
    IF gbRGBSpace THEN DO
      CALL QueueCntl "  /CsRGB /DeviceRGB"
    END
    IF gbRGBSpace THEN DO
      CALL QueueCntl "  /CsGray /DeviceGray"
    END
    CALL QueueCntl ">>"
  END
  */

  CALL QueueCntl "/Font <<" lscFonts ">>"

  /* ===================================================================
  -- Generate graphics states
  */
  IF gnAlphaCnt <> 0 THEN DO
    CALL QueueCntl "/ExtGState <<"
    DO lnI = 1 TO gnAlphaCnt
      CALL QueueCntl "  /Alpha" || lnI "<<"
      CALL QueueCntl "    /Type /ExtGState"
      CALL QueueCntl "    /ca" gaAlpha.lnI / 100
      CALL QueueCntl "    /CA" gaAlpha.lnI / 100
      CALL QueueCntl "  >>"
    END
    CALL QueueCntl ">>"
  END

  /* ===================================================================
  -- Generate X objects and patterns
  */
  IF gnImgCnt <> 0 THEN DO

    /* =================================================================
    -- Count number of used images and patterns
    */
    lnP = 0
    lnX = 0
    DO lnI = 1 TO gnImgCnt
      IF gaImgU.lnI THEN DO
        lnX = lnX + 1
      END
      IF gaImgPU.lnI THEN DO
        lnP = lnP + 1
      END
    END

    /* =================================================================
    -- Generate X objects
    */
    IF lnX <> 0 THEN DO
      CALL QueueCntl "/XObject <<"
      DO lnI = 1 TO gnImgCnt
        IF gaImgU.lnI THEN DO
          CALL QueueCntl "/XOImg" || lnI gaImgO.lnI "0 R"
        END
      END
      CALL QueueCntl ">>"
    END

    /* =================================================================
    -- Generate patterns
    */
    IF lnP <> 0 THEN DO
      CALL QueueCntl "/Pattern <<"
      DO lnI = 1 TO gnImgCnt
        IF gaImgPO.lnI <> 0 THEN DO
          CALL QueueCntl "/P" || lnI gaImgPO.lnI " 0 R"
        END
      END
      CALL QueueCntl ">>"
    END
  END

  CALL QueueCntl ">>"
  CALL EndObj

  /* ===================================================================
  -- Generate an annotation object
  */
  IF gsAnnTyp <> gsNull THEN DO
    gnAnnotObj = NewObj()
    CALL QueueCntl "<<"
    CALL QueueCntl "/Type /Annot"
    IF gsAnnTyp = "TEXT" THEN DO
      CALL QueueCntl "/Subtype /Text"
      CALL Queue Cntl( "/Contents " ) ||,
                 String( gsAnnTxt )
      CALL QueueCntl "/Open" gsAnnSt8
    END
    lnL = gnAnnCol * ( ( gnPoints * 600 ) / 1000 )
    lnT = gnTextT - ( gnAnnRow * gnLeading )
    CALL QueueCntl "/Rect [",
                   lnL,
                   lnT - 100,
                   lnL + 100,
                   lnT,
                   "]"
    CALL QueueCntl ">>"
    CALL EndObj

    DROP lnL lnT
  END

  /* ===================================================================
  -- Generate a pages object and save ID.
  --
  -- Note:  This will generate the kids array based on the
  -- assumption that the "page" objects will immediately follow
  -- the "pages" object.
  */
  gnPagesObj = NewObj()
  CALL QueueCntl "<<"
  CALL QueueCntl "/Type /Pages"
  IF gbPaperA = gbFalse THEN DO
    CALL QueueCntl "/MediaBox [ 0 0" gnPageW gnPageH "]"
  END
  CALL QueueCntl "/Count" gnContentObjs
  CALL QueueCntl "/Kids ["
  DO numI = 1 TO gnContentObjs
    CALL QueueCntl gnPagesObj + numI "0 R"
  END
  CALL QueueCntl "]"
  CALL QueueCntl ">>"
  CALL EndObj

  /* ===================================================================
  -- Generate a page object for each content object.
  */
  DO numI = 1 TO gnContentObjs
    gnContentObj = Word( gsContentObjs, numI )
    gnPageObj = NewObj()
    CALL QueueCntl "<<"
    CALL QueueCntl "/Type /Page"
    IF gbPaperA THEN DO
      CALL QueueCntl "/MediaBox [ 0 0",
                     gaMediaW.numI gaMediaH.numI "]"
    END

    /* =================================================================
    -- Include transition if requested
    */
    IF gsTrans <> gsNull THEN DO
      CALL QueueCntl "/Trans <<" gsTrans ">>"
    END

    CALL QueueCntl "/Parent" gnPagesObj "0 R"
    CALL QueueCntl "/Resources" gnResourceObj "0 R"

    IF gsAnnTyp <> gsNull THEN DO
      IF ( gsAnnPag = "FIRST" & numI = 1 ) |,
         ( gsAnnPag = "LAST" & numI = gnContentObjs ) |,
         ( gsAnnPag = "ALL" ) THEN DO
        CALL QueueCntl "/Annots [" gnAnnotObj "0 R ]"
      END
    END

    CALL QueueCntl "/Contents ["

    IF gbHasBG THEN DO
      CALL QueueCntl gnBackObj "0 R"
    END

    CALL QueueCntl gnContentObj "0 R"
    CALL QueueCntl "]"

    CALL QueueCntl ">>"
    CALL EndObj

    IF gbHasOL THEN DO
      gaIdxPage.gnContentObj = gnPageObj
    END
  END

  /* ===================================================================
  -- Generate the outline if we have any indexes.
  */
  IF gnIdxCnt > 0 THEN DO

    /* =================================================================
    -- Sort the index?
    */
    IF gbOSort THEN DO
      CALL QSort "gaIdxText.", gbOSDir, 1, gnIdxCnt
    END

    /* =================================================================
    -- Initialize
    */
    laOParent. = gsNull
    laOPrev. = gsNull
    laONext. = gsNull
    laOFirst. = gsNull
    laOLast. = gsNull
    laODest. = gsNull
    laOCount. = 0
    laOTitle. = gsNull
    laOIndex. = gsNull
    laOBranch. = gsNull
    laOKids. = gsNull
    lnLPos = 0
    lnFirst = gnObjs + 1
    lnNodes = lnFirst
    lsLeft = gsNull
    lnTokLen = 1

    /* =================================================================
    -- Scan all entries to be added to the family tree
    */
    DO lnI = 1 TO gnIdxCnt

      /* ===============================================================
      -- Separate the burial plot address and the name
      */
      lnPos = Lastpos( "-", gaIdxText.lnI )
      PARSE VAR gaIdxText.lnI lsIndex =(lnPos) "-" lsODest
      lsIndex = Strip( lsIndex, "T" )

      /* ===============================================================
      -- Start at the top of the tree
      */
      lnParent = lnFirst

      /* ===============================================================
      -- Walk the tree (well, kinda skip through it...)
      */
      lnPos = 0
      DO UNTIL lnPos = 0

        /* =============================================================
        -- Get the name
        */
        lsLeft = lsIndex

        /* =============================================================
        -- Discard duplicate indexes?
        */
        IF gbODups = gbFalse & laOIndex.lsLeft <> gsNull THEN DO
          ITERATE
        END

        /* =============================================================
        -- Are we building a family tree?
        --
        -- This weird test is due to not having a "strictly not
        -- equal" operator that doesn't contain "\" or logical
        -- not.  Spaces are a valid token so we must be careful
        -- not to exclude that case.
        */
        IF "." || gsOTok || "." <> ".." THEN DO

          /* ===========================================================
          -- Remember the start of this branch and find the end.
          */
          lnLPos = lnPos
          lnPos = Pos( gsOTok, lsIndex, lnPos + lnTokLen )
          lnTokLen = gnOTokLen
          IF lnPos <> 0 THEN DO
            lsLeft = Left( lsIndex, lnPos - 1 )
          END

          /* ===========================================================
          -- If this branch of the tree already exists, then we
          -- want to follow that parent's lineage.
          */
          IF laOIndex.lsLeft <> gsNull THEN DO
            IF lnPos <> 0 THEN DO
              IF laOBranch.lsLeft <> gsNull THEN DO
                lnParent = laOBranch.lsLeft
                ITERATE
              END
            END
          END
        END

        /* =============================================================
        -- A new kid is born
        */
        lnNodes = lnNodes + 1
        lnNode = lnNodes

        /* =============================================================
        -- Give the kid a name
        */
        IF gbOFull | lnLPos = 0 THEN DO
          laOTitle.lnNode = lsLeft
        END
        ELSE DO

          /* ===========================================================
          -- Allow the token to be included in the name
          */
          IF gbOShow THEN DO
            lnTokLen = 0
          END

          /* ===========================================================
          -- This the last or only kid?
          */
          IF lnPos = 0 THEN DO

            /* =========================================================
            -- Yes, he gets what's left of the name
            */
            laOTitle.lnNode = Substr( lsLeft,,
                                      lnLpos + lnTokLen )
          END
          ELSE DO

            /* =========================================================
            -- No, so pull his name from the middle
            */
            laOTitle.lnNode = Substr( lsLeft,,
                                      lnLpos + lnTokLen,,
                                      lnPos - lnLPos )
          END

          lnTokLen = gnOTokLen
        END

        /* =============================================================
        -- Register the full name with immigration
        */
        laOIndex.lsLeft = lnNode

        /* =============================================================
        -- If this kid is gonna be the last in his lineage
        -- then give him a burial plot, otherwise remember
        -- that he's given the family tree a new branch.
        */
        IF lnPos = 0 THEN DO
          laODest.lnNode = lsODest
        END
        ELSE DO
          IF laODest.lnNode = gsNull THEN DO
            laODest.lnNode = lsODest
          END
          laOBranch.lsLeft = lnNode
        END

        /* =============================================================
        -- Tell the kid who his parent is
        */
        laOParent.lnNode = lnParent

        /* =============================================================
        -- Is this the parent's first born?
        */
        IF laOFirst.lnParent = gsNull THEN DO
          laOFirst.lnParent = lnNode
        END

        /* =============================================================
        -- Introduce this kid to his siblings
        */
        IF laOLast.lnParent <> gsNull THEN DO

          /* ===========================================================
          -- Who was the last born?
          */
          lnLast = laOLast.lnParent

          /* ===========================================================
          -- Tell him about his new brother
          */
          laONext.lnLast = lnNode

          /* ===========================================================
          -- Tell the new kid about his older brother
          */
          laOPrev.lnNode = lnLast
        END

        /* =============================================================
        -- Remember that this kid is the youngest now
        */
        laOLast.lnParent = lnNode

        /* =============================================================
        -- Give the parent a new tax write off
        */
        laOKids.lnParent = laOKids.lnParent lnNode

        /* =============================================================
        -- Add the kid to the family tree
        */
        DO UNTIL lnParent = lnFirst | lnParent = gsNull
          laOCount.lnParent = laOCount.lnParent + 1
          lnParent = laOParent.lnParent
        END

        /* =============================================================
        -- This kid grows up and becomes the parent
        */
        lnParent = lnNode
      END
    END

    /* =================================================================
    -- Dump outline nodes.
    */
    IF gnConfirm >= gnMsgD THEN DO
      DO lnI = lnFirst to lnNodes
        lsLeft = laoTitle.lnI
        SAY "node"                  lnI
        SAY "    index"             laOIndex.lsLeft
        SAY "    branch"            laOBranch.lsLeft
        SAY "    next"              laONext.lnI
        SAY "    prev"              laOPrev.lnI
        SAY "    dest"              laODest.lnI
        SAY "    title"             laOTitle.lnI
        SAY "    parent"            laOParent.lnI
        SAY "    last"              laOLast.lnI
        SAY "    kids"              laOKids.lnI
        SAY "    count"             laOCount.lnI
      END
    END

    /* =================================================================
    -- Generate an outlines object and save ID.
    --
    -- Note:  This will generate the outlines object based on
    --        the assumption that the outline objects will
    --        immediately follow this object.
    */
    gnOutlinesObj = NewObj()
    lnFirstOutline = Word( laOKids.lnFirst, 1 )
    lnLastOutline = Word( laOKids.lnFirst,,
                    Words( laOKids.lnFirst ) )
    CALL QueueCntl "<<"
    CALL QueueCntl "/Type /Outlines"
    CALL QueueCntl "/First" lnFirstOutline "0 R"
    CALL QueueCntl "/Last" lnLastOutline "0 R"
    CALL QueueCntl "/Count" lnNodes - lnFirst
    CALL QueueCntl ">>"
    CALL EndObj

    /* =================================================================
    -- Generate the outline objects.
    */
    DO lnI = lnFirst + 1 TO lnNodes

      /* Title */
      gnOutlineObj = NewObj()
      CALL QueueCntl "<<"
      CALL Queue Cntl( "/Title " ) ||,
                 String( laOTitle.lnI )
      CALL QueueCntl "/F" gnOFlags
      CALL QueueCntl "/C [" gaOColor.gnOCIdx "]"
      gnOCIdx = 2 / gnOCidx

      /* Parent */
      IF laOParent.lnI <> gsNull THEN DO
        CALL QueueCntl "/Parent" laOParent.lnI "0 R"
      END

      /* Prev */
      IF laOPrev.lnI <> gsNull THEN DO
        CALL QueueCntl "/Prev" laOPrev.lnI "0 R"
      END

      /* Next */
      IF laONext.lnI <> gsNull THEN DO
        CALL QueueCntl "/Next" laONext.lnI "0 R"
      END

      /* First */
      IF laOFirst.lnI <> gsNull THEN DO
        CALL QueueCntl "/First" laOFirst.lnI "0 R"
      END

      /* Last */
      IF laOLast.lnI <> gsNull THEN DO
        CALL QueueCntl "/Last" laOLast.lnI "0 R"
      END

      /* Count */
      IF laOCount.lnI <> 0 THEN DO
        CALL QueueCntl "/Count" laOCount.lnI * gnOExpand
      END

      /* Destination */
      IF laODest.lnI <> gsNull THEN DO
        PARSE VAR laODest.lnI lnC lnL

        CALL QueueCntl "/Dest [",
                       gaIdxPage.lnC,
                       "0 R /XYZ 0",
                       gnTextT - ( ( lnL - 1 ) * gnLeading ),
                       "0 ]"
      END

      CALL QueueCntl ">>"
      CALL EndObj
    END
  END

  /* ===================================================================
  -- Generate the encryption object and save ID
  */
  IF gbEncrypt THEN DO
    gnEncryptObj = NewObj()
    CALL QueueCntl "<<"
    CALL QueueCntl "/Filter /Standard"
    CALL QueueCntl "/V " || gnCryptVer
    CALL QueueCntl "/R " || gnCryptRev
    CALL QueueCntl "/Length " || gnCryptLen * 8
    CALL QueueCntl "/O <" || C2x( gsOwnerKey ) || ">"
    CALL QueueCntl "/U <" || C2x( gsUserKey ) || ">"
    CALL QueueCntl "/P " || X2d( C2x( gsPerms ), 8 )
    CALL QueueCntl ">>"
    CALL EndObj
  END

  /* ===================================================================
  -- Generate the catalog object and save ID
  */
  gnCatalogObj = NewObj()
  CALL QueueCntl "<<"
  CALL QueueCntl "/Type /Catalog"
  CALL QueueCntl "/Pages " || gnPagesObj || " 0 R"

  IF gnIdxCnt > 0 THEN DO
    CALL QueueCntl "/Outlines " || gnOutlinesObj || " 0 R"
    IF gsPageM = gsNull THEN DO
      CALL QueueCntl "/PageMode /UseOutlines"
    END
  END

  IF gsPageM <> gsNull THEN DO
    CALL QueueCntl "/PageMode /" || gsPageM
  END

  IF gsPageL <> gsNull THEN DO
    CALL QueueCntl "/PageLayout /" || gsPageL
  END

  IF gsViewer <> gsNull THEN DO
    CALL QueueCntl "/ViewerPreferences <<" gsViewer ">>"
  END

  IF gsMag <> gsNull THEN DO
    CALL QueueCntl "/OpenAction<</S/GoTo/D[0/" || gsMag || "]>>"
  END

  CALL QueueCntl ">>"
  CALL EndObj

  /* ===================================================================
  -- Generate the document information object and save ID
  */
  gnDInfoObj = NewObj()
  CALL QueueCntl "<<"
  CALL Queue Cntl( "/Producer " ) ||,
             String( gsProducer gsVersion "(c) 2000-2009" gsAuthor )

  IF gsDInfo <> gsNull THEN DO
    lnNames = "TITLE SUBJECT AUTHOR KEYWORDS"
    lnKeys = "Title Subject Author Keywords"
    DO lnI = 1 TO gaDInfo.0
      lnKey = Word( gaDInfo.lnI, 1 )
      lnVal = Strip( Subword( gaDInfo.lnI, 2 ) )
      lnP = Wordpos( Translate( lnKey), lnNames )
      IF lnP > 0 THEN DO
        lnKey = Word( lnKeys, lnP )
      END
      CALL Queue Cntl( "/" || lnKey || " " ) ||,
                 String( lnVal )
    END

    DROP lnNames lnKeys lnI lnKey lnVal lnP
  END

  CALL Queue Cntl( "/CreationDate " ) ||,
                 String( "D:" ||,
                       Date( "S" ) ||,
                       Space( Translate( Time(), " ", ":" ),,
                              0) )

  CALL QueueCntl ">>"
  CALL EndObj

  /* ===================================================================
  -- Generate the XREF table
  */
  numIrefOff = gnOffset
  CALL QueueCntl "xref"
  CALL QueueCntl "0 " || gnObjs + 1
  CALL QueueCntl "0000000000 65535 f "
  DO numI = 1 TO gnObjs
    CALL QueueCntl Right( gaOffsets.numI, 10, "0" ),
                   "00000 n "
  END

  /* ===================================================================
  -- Generate trailer
  */
  CALL QueueCntl "trailer"
  CALL QueueCntl "<<"
  CALL QueueCntl "/Size " || gnObjs + 1
  CALL QueueCntl "/Root " || gnCatalogObj || " 0 R"
  CALL QueueCntl "/Info " || gnDInfoObj || " 0 R"
  IF gbEncrypt THEN DO
    CALL QueueCntl "/Encrypt " || gnEncryptObj || " 0 R"
    CALL QueueCntl "/ID [<" ||,
                   C2x( gsFileID ) ||,
                   "><" ||,
                   C2x( gsFileID ) ||,
                   ">]"
  END
  CALL QueueCntl ">>"
  CALL QueueCntl "startxref"
  CALL QueueCntl numIrefOff
  CALL QueueCntl "%%EOF"

  /* ===================================================================
  -- Write (and flush) it
  */
  CALL WriteClose

RETURN

/*
-- =====================================================================
-- Routine:   Images           : Creates "image" XObjects
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
Images:

  /* ===================================================================
  -- Process draw information
  */
  DO lniI = 1 TO gnImgCnt

    /* =================================================================
    -- Only need to emit it if used
    */
    IF gaImgU.lniI = gbFalse THEN DO
      ITERATE
    END

    /* =================================================================
    -- Only need to emit it once
    */
    IF gaImgO.lniI <> 0 THEN DO
      ITERATE
    END

    /* =================================================================
    -- Call image specific functions
    */
    SELECT
      WHEN gaImgT.lniI = "JPEG" THEN DO
        CALL ImageJPEG lniI
      END

      WHEN gaImgT.lniI = "BMP" THEN DO
        CALL ImageBMP lniI
      END

      OTHERWISE DO
        SAY did_not_account_for_all_cases_in_select
      END
    END

    /* =================================================================
    -- Emit a pattern based on image if needed
    */
    IF gaImgPU.lniI = gbFalse THEN DO
      ITERATE
    END

    /* =================================================================
    -- Get image dimensions
    */
    lniW = gaImgW.lniI
    lniH = gaImgH.lniI

    /* =================================================================
    -- Create a new pattern object
    */
    gaImgPO.lniI = NewObj()

    CALL QueueCntl "<<"
    CALL QueueCntl "/Type /Pattern"
    CALL QueueCntl "/PatternType 1"
    CALL QueueCntl "/PaintType 1"
    CALL QueueCntl "/TilingType 1"
    CALL QueueCntl "/BBox [0 0 "lniW lniH"]"
    CALL QueueCntl "/XStep "lniW"/YStep "lniH
    CALL QueueCntl "/Resources <</XObject<</XOImg1",
                   gaImgO.lniI,
                   "0 R>>>>"
    CALL QueueCntl "/Matrix [1 0 0 1 0 0]"

    CALL StartStream

    /* =================================================================
    -- Relocate and rotate it
    */
    IF gaImgM.lniI <> gsNULL THEN DO
      CALL QueueCntl gaImgM.lniI
    END

    /* =================================================================
    -- Scale it
    */
    CALL QueueCntl lniW 0 0 lniH 0 0 "cm"

    /* =================================================================
    -- Draw it
    */
    CALL QueueCntl "/XOImg" || lniI "Do"

    /* =================================================================
    -- End the stream
    */
    CALL EndStream
  END

  DROP lniI lniObj lniC lniP lniD
RETURN

/*
-- =====================================================================
-- Routine:   ImageJPEG        : Creates "JPEG" XObjects
-- Arguments: lnijN            : image index
-- Return:    (none)           : none required
-- =====================================================================
*/
ImageJPEG:
  PARSE ARG lnijN

  /* ===================================================================
  -- Create a new object
  */
  gaImgO.lnijN = NewObj()

  /* ===================================================================
  -- Get image information
  */
  PARSE VAR gaImg.lnijN lnijP lnijC lsijD

  /* ===================================================================
  -- Queue stream dictionary (forward point to length object)
  */
  CALL QueueCntl "<<"
  CALL QueueCntl "/Type /XObject"
  CALL QueueCntl "/Subtype /Image"
  CALL QueueCntl "/Width" gaImgW.lnijN
  CALL QueueCntl "/Height" gaImgH.lnijN
  CALL QueueCntl "/ColorSpace" lnijC
  CALL QueueCntl "/BitsPerComponent" lnijP

  /* ===================================================================
  -- Start the stream
  */
  CALL StartStream

  /* ===================================================================
  -- Write the image data
  */
  CALL Queue lsijD

  /* ===================================================================
  -- End the stream
  */
  CALL EndStream "/DCTDecode"

  DROP lnijN lnijP lnijC lsijD

RETURN

/*
-- =====================================================================
-- Routine:   ImageBMP         : Creates "BMP" XObjects
-- Arguments: lnibN            : image index
-- Return:    (none)           : none required
-- =====================================================================
*/
ImageBMP:
  PARSE ARG lnibN

  /* ===================================================================
  -- Cache width, height, image, and init output
  */
  lnibW = gaImgW.lnibN
  lnibH = gaImgH.lnibN
  lsibD = gaImg.lnibN
  lsibO = gsNull

  /* ===================================================================
  -- Get bits per plane...1, 4, 8, 24 only
  */
  lnibP = C2d( Reverse( Substr( gaImg.lnibN, 29, 2 ) ) )

  /* ===================================================================
  -- Get displacement to image data
  */
  lnibD = C2d( Reverse( Substr( gaImg.lnibN, 11, 4 ) ) ) + 1

  /* ===================================================================
  -- Handle indexed image
  */
  IF lnibP < 24 THEN DO

    /* =================================================================
    -- Process the color data.                                        --
    ================================================================= */

    /* =================================================================
    -- Get or calc number of colors
    */
    lnibC = C2d( Reverse( Substr( gaImg.lnibN, 47, 4 ) ) )
    IF lnibC = 0 THEN DO
      lnibC = 2 ** lnibP
    END

    /* =================================================================
    -- Convert from abgr (little-endian) quads to rgb triplets
    */
    lsibCS = gsNull
    lnibX = 55
    DO lnibI = 1 TO lnibC
      PARSE VAR gaImg.lnibN =(lnibX) lsibT +3 .

      lsibCS = lsibCS || Reverse( lsibT )
      lnibX = lnibX + 4
    END

    /* =================================================================
    -- Create a new colortable object
    */
    lnibCSO = NewObj()

    CALL QueueCntl "<<"

    /* =================================================================
    -- Start the stream
    */
    CALL StartStream

    /* =================================================================
    -- Pad and write the color table (it can be short)
    */
    CALL Queue Left( lsibCS, ( 2 ** lnibP ) * 3, '00'x )

    /* =================================================================
    -- End the stream
    */
    CALL EndStream

    /* =================================================================
    -- Generate the colorspace value
    */
    lsibSpace = "[/Indexed /DeviceRGB",
                ( 2 ** lnibP ) - 1,
                lnibCSO,
                "0 R]"

    /* =================================================================
    -- Process the image data.                                        --
    ================================================================= */

    /* =================================================================
    -- Calc number of quads in a row
    */
    lnibCL = 8 / lnibP
    lnibCL = ( lnibW % lnibCL ) + ( ( lnibW // lnibCL ) <> 0 )
    lnibCL = C2d( Bitand( D2c( lnibCL + 3, 3 ), 'FFFFFC'x ) )

    /* =================================================================
    -- Expand 1 and 4 bit images
    */
    IF lnibP < 8 THEN DO

      IF lnibP = 1 THEN DO
        /* =============================================================
        || Make each bit visible
        */
        lsibB = X2b( C2x( Substr( lsibD, lnibD ) ) )
        lnibRW = lnibCL * 8
      END
      ELSE DO
        /* =============================================================
        || Make each bit visible
        */
        lsibB = C2x( Substr( lsibD, lnibD ) )
        lnibRW = lnibCL * 2
      END

      /* ===============================================================
      -- Get rid of padding bytes and reverse rows
      */
      lnibD = 1
      DO lnibR = 1 TO lnibH
        lsibO = Substr( lsibB, lnibD, lnibW ) || lsibO
        lnibD = lnibD + lnibRW
      END

      /* ===============================================================
      -- Convert to 2 digit hex values
      */
      DO lnibC = Length( lsibO ) - 1 TO 0 BY -1
        lsibO = Insert( "0", lsibO, lnibC )
      END

      /* ===============================================================
      -- Finally convert back to character
      */
      lsibO = X2c( lsibO )
    END
    ELSE DO
      /* ===============================================================
      -- Get rid of padding bytes and reverse rows
      */
      DO lnibR = 1 TO lnibH
        lsibO = Substr( lsibD, lnibD, lnibW ) || lsibO
        lnibD = lnibD + lnibCL
      END
    END

    /* =================================================================
    -- Cleanup
    */
    DROP lnibC lsibCS lnibX lnibCSO lnibR lsibB lnibC lnibR
  END
  ELSE DO
    /* =================================================================
    -- Image data is its own color table.                             --
    ================================================================= */

    /* =================================================================
    -- Generate the colorspace value
    */
    lsibSpace = "[/DeviceRGB]"

    /* =================================================================
    -- Process the image data.                                        --
    ================================================================= */

    /* =================================================================
    -- Reverse rgb triplets and strip padding
    */
    lnibRW = lnibW * 3
    lnibCL = lnibRW + ( lnibW // 4 )

    DO lnibR = 1 to lnibH
      lsibO = Substr( lsibD, lnibD, lnibRW ) || lsibO
      lnibD = lnibD + lnibCL
    END

    /* =================================================================
    -- Cleanup
    */
    DROP lnibR lnibC
  END

  /* ===================================================================
  -- Create the image object
  */
  gaImgO.lnibN = NewObj()

  /* ===================================================================
  -- Queue stream dictionary (forward point to length object)
  */
  CALL QueueCntl "<<"
  CALL QueueCntl "/Type /XObject"
  CALL QueueCntl "/Subtype /Image"
  CALL QueueCntl "/Width" gaImgW.lnibN
  CALL QueueCntl "/Height" gaImgH.lnibN
  CALL QueueCntl "/ColorSpace" lsibSpace
  CALL QueueCntl "/BitsPerComponent 8"

  /* ===================================================================
  -- Start the stream
  */
  CALL StartStream

  /* ===================================================================
  -- Write the image data
  */
  CALL Queue Reverse( lsibO )

  /* ===================================================================
  -- End the stream
  */
  CALL EndStream

  DROP lnibN lnibW lnibH lsibD lnibP lnibCL lnibD lsibO
RETURN

/*
-- =====================================================================
-- Routine:   Background       : Draws the page background
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
Background:

  /* ===================================================================
  -- Create a new object
  */
  gnBackObj = NewObj()

  /* ===================================================================
  -- Queue stream dictionary ( forward point to length object )
  */
  CALL QueueCntl "<<"

  /* ===================================================================
  -- Start the stream
  */
  CALL StartStream

  /* ===================================================================
  -- First apply any background color
  */
  IF gsColorB <> Colorspec( "White" ) THEN DO

    CALL QueueCntl "q",
                   gsColorB "rg",
                   gnCropL gnCropB gnCropW gnCropH "re",
                   "f",
                   "Q"

  END

  /* ===================================================================
  -- Draw the holes
  --
  -- Holes are 1/8"
  -- Spaced at 1/2" on center
  -- Starting at 1/4" from paper top
  -- Drawn in the Page box
  --
  -- (Start at top since the paper my not end at an exact 1/2"
  -- boundary.  Starting at bottom would leave a partial hole at
  -- the top of the page.)
  */
  IF gbHasHoles THEN DO

    lnX1   = gnHoleSize / 2
    lnh0X1 = gnHoleSize * .00 + lnX1
    lnh1X1 = gnHoleSize * .25 + lnX1
    lnh2X1 = gnHoleSize * .50 + lnX1
    lnh3X1 = gnHoleSize * .75 + lnX1
    lnh4X1 = gnHoleSize * 1.0 + lnX1

    lnX2   = gnPageW - lnX1 - gnHoleSize
    lnh0X2 = gnHoleSize * .00 + lnX2
    lnh1X2 = gnHoleSize * .25 + lnX2
    lnh2X2 = gnHoleSize * .50 + lnX2
    lnh3X2 = gnHoleSize * .75 + lnX2
    lnh4X2 = gnHoleSize * 1.0 + lnX2

    lnH    = 72 * .5
    lnY    = gnPageH - ( lnH / 2 ) - ( gnHoleSize / 2 )
    lnh0Y  = gnHoleSize * .00 + lnY
    lnh1Y  = gnHoleSize * .25 + lnY
    lnh2Y  = gnHoleSize * .50 + lnY
    lnh3Y  = gnHoleSize * .75 + lnY
    lnh4Y  = gnHoleSize * 1.0 + lnY

    CALL QueueCntl "q",
                   "1 w",
                   "0.95 0.95 0.95 rg",
                   "0.85 0.85 0.85 RG"

    DO WHILE lnh4Y > gnCropB
      CALL QueueCntl lnh2X1 lnh4Y "m",
                     lnh1X1 lnh4Y lnh0X1 lnh3Y lnh0X1 lnh2Y "c",
                     lnh0X1 lnh1Y lnh1X1 lnh0Y lnh2X1 lnh0Y "c",
                     lnh3X1 lnh0Y lnh4X1 lnh1Y lnh4X1 lnh2Y "c",
                     lnh4X1 lnh3Y lnh3X1 lnh4Y lnh2X1 lnh4Y "c"

      CALL QueueCntl lnh2X2 lnh4Y "m",
                     lnh1X2 lnh4Y lnh0X2 lnh3Y lnh0X2 lnh2Y "c",
                     lnh0X2 lnh1Y lnh1X2 lnh0Y lnh2X2 lnh0Y "c",
                     lnh3X2 lnh0Y lnh4X2 lnh1Y lnh4X2 lnh2Y "c",
                     lnh4X2 lnh3Y lnh3X2 lnh4Y lnh2X2 lnh4Y "c"

      lnh0Y = lnh0Y - lnH
      lnh1Y = lnh1Y - lnH
      lnh2Y = lnh2Y - lnH
      lnh3Y = lnh3Y - lnH
      lnh4Y = lnh4Y - lnH
    END

    CALL QueueCntl "B",
                   "Q"

    DROP lnH lnX1 lnX2 lnY
  END

  /* ===================================================================
  -- Draw the bars
  --
  -- Bars are 1/2"
  -- Extend the width of the Crop box
  -- Spaced at 1" intervals
  -- Start at top of crop box
  --
  -- (Start at top since the paper my not end at an exact 1"
  -- boundary.  Starting at bottom would leave a partial bar at
  -- the top of the page.)
  */
  IF gbHasBars THEN DO

    lnY = gnCropT - ( 72 * .5 )

    CALL QueueCntl "q",
                   gsBarColorF "rg",
                   gsBarColorB "RG"

    DO lnY = lnY BY -72 UNTIL lnY < gnCropB

      CALL QueueCntl gnCropL lnY gnCropW ( 72 * .5 ) "re"

    END

    CALL QueueCntl "B",
                   "Q"

    DROP lnY
  END

  /* ===================================================================
  -- Handle any backgrounds
  */
  DO lnbI = 1 TO gnBackC

    /* =================================================================
    -- PDF text stream?
    */
    IF gaBackT.lnbI = "STREAM" THEN DO

      /* ===============================================================
      -- Save graphics state
      */
      CALL QueueCntl "q"

      /* ===============================================================
      -- Open the file
      */
      lsbH = ReadOpen( gaBackP.lnbI )

      /* ===============================================================
      -- Copy stream to output
      */
      DO UNTIL labStream.0 = 0
        CALL ReadFile lsbH, "labStream."
        DO lnbJ = 1 TO labStream.0
          CALL QueueCntl labStream.lnbJ
        END
      END

      /* ===============================================================
      -- Close it
      */
      CALL ReadClose lsbH

      /* ===============================================================
      -- Restore graphics state
      */
      CALL QueueCntl "Q"

      DROP lsbH lnbCnt lnbJ labStream.
    END

    /* =================================================================
    -- Simulate a text style watermark
    */
    IF gaBackT.lnbI = "TEXTMARK" THEN DO

      /* ===============================================================
      -- Get parameters.
      --
      -- NOTE:  The blanks are added to keep the left and
      --        right characters from falling off the paper
      --        edge.
      */
      PARSE VAR gaBackP.lnbI lsS (gsDelim) lsF (gsDelim) lsB (gsDelim),
                             lsP (gsDelim) lnN (gsDelim) lsT

      /* ===============================================================
      -- Set character metrics
      */
      lnFontN = lnN
      lnFontS = lsP
      lnCharW = 600 / 1000

      /* ===============================================================
      -- Determine the box dimensions
      */
      lnW = gnCropW
      lnH = gnCropH

      /* ===============================================================
      -- Calc distance between LL and UR corners
      */
      lnD = Sqrt( ( lnW ** 2 ) + ( lnH ** 2 ) )

      /* ===============================================================
      -- Calc ratios
      */
      lnrW = lnD / lnW
      lnrH = lnD / lnH

      /* ===============================================================
      -- Get the slope at which the text will be placed
      */
      lnS = Atan( lnH / lnW )

      /* ===============================================================
      -- Calc the sine and cosine
      */
      lnSin = Sin( lnS )
      lnCos = Cos( lnS )

      /* ===============================================================
      -- Calc the "height" of the text rect
      */
      lntX = lnFontS * -lnSin
      lntY = lnFontS * lnCos

      /* ===============================================================
      -- Calc lower right X,Y of text rect
      */
      IF lnW < lnH THEN DO
        lnllX = Abs( lntX )
        lnllY = ( lnFontS / 2 ) * lnrW - lntY
      END
      ELSE IF lnW > lnH THEN DO
        lnllX = ( lnFontS / 2 ) * lnrH
        lnllY = 0
      END
      ELSE DO
        lnllX = Abs( lntX )
        lnllY = 0
      END

      /* ===============================================================
      -- Calc upper left X,Y of text rect
      */
      lnulX = lntX + lnllX
      lnulY = lntY + lnllY

      /* ===============================================================
      -- Calc upper right X,Y of text rect
      */
      lnurX = lnW - lnllX
      lnurY = lnH - lnllY

      /* ===============================================================
      -- Calc lower right X,Y of text rect
      */
      lnlrX = lnurX - lntX
      lnlrY = lnurY - lntY

      /* ===============================================================
      -- Calc width of text rect
      */
      lnBoxW = Sqrt( ( ( lnlrX - lnllX ) ** 2 ) +,
                     ( ( lnlrY - lnllY ) ** 2 ) )

      /* ===============================================================
      -- Calc scaling factor
      */
      lnTw = Length( lsT ) * lnCharW
      lnZoom = ( lnBoxW / ( lnTw * lnFontS ) ) * 100

      /* ===============================================================
      -- Calc text X offset
      */
      lntX = lnllX + ( ( lnulX - lnllX ) / 2 )
      lntY = lnllY + ( ( lnulY - lnllY ) / 2 )
      lnD  = Sqrt( ( lntX ** 2 ) + ( lntY ** 2 ) )

      /* ===============================================================
      -- Draw an a bounding box
      */
      IF lsS = "TDBOXED" THEN DO
          CALL QueueCntl "q",
                         "0 0 1 RG",
                         "0" lnH "m",
                         lnW "0 l",
                         lnllX ( lnH - lnllY ) "m",
                         lnulX ( lnH - lnulY ) "l",
                         lnurX ( lnH - lnurY ) "l",
                         lnlrX ( lnH - lnlrY ) "l",
                         lnllX ( lnH - lnllY ) "l",
                         "s",
                         "Q"
      END
      ELSE IF lsS = "BUBOXED" THEN DO
          CALL QueueCntl "q",
                         "0 0 1 RG",
                         "0 0 m",
                         lnW lnH "l",
                         lnllX lnllY "m",
                         lnulX lnulY "l",
                         lnurX lnurY "l",
                         lnlrX lnlrY "l",
                         lnllX lnllY "l",
                         "s",
                         "Q"
      END

      /* ===============================================================
      -- Save graphics state
      */
      CALL QueueCntl "q"

      /* ===============================================================
      -- Start the text object and set font, color, and mode
      */
      CALL QueueCntl gsAlphaBG "gs"
      CALL QueueCntl "BT"

      /* ===============================================================
      -- Queue the text matrix
      */
      IF lsS = "TOPDOWN" | lsS = "TDBOXED" THEN DO
        CALL QueueCntl lnCos,
                       ( -lnSin ),
                       lnSin,
                       lnCos,
                       "0 " lnH " Tm"
      END
      ELSE DO
        CALL QueueCntl lnCos,
                       lnSin,
                       ( -lnSin ),
                       lnCos,
                       "0 0 Tm"
      END

      /* ===============================================================
      -- Set font, displacement, and zoom
      */
      CALL QueueCntl PushFont( lnFontN lnFontS )
      CALL QueueCntl lnD ( -.250 * lnFontS ) "Td"
      CALL QueueCntl lnZoom "Tz"

      /* ===============================================================
      -- Set color and mode
      */
      lnR = 2
      IF lsF <> gsNull THEN DO
        CALL QueueCntl lsF "RG"
        IF lsB = gsNull THEN DO
          lnR = 1
        END
      END

      IF lsB <> gsNull THEN DO
        CALL QueueCntl lsB "rg"
        IF lsF = gsNull THEN DO
          lnR = 0
        END
      END
      CALL QueueCntl lnR "Tr"

      /* ===============================================================
      -- Queue the text
      */
      CALL Queue Text( lsT ) ||,
                 gsTxtOut

      /* ===============================================================
      -- Restore font
      */
      CALL PullFont

      /* ===============================================================
      -- End the text object
      */
      CALL QueueCntl "ET"

      /* ===============================================================
      -- Restore graphics state
      */
      CALL QueueCntl "Q"

      DROP lsC lsT lnFontN lnFontS lnCharW,
           lnW lnH lnS lnZ
    END
  END

  /* ===================================================================
  -- Handle any images
  */
  DO lnbI = 1 TO gnDrawCnt

    /* =================================================================
    -- Based on type
    */
    SELECT
      WHEN gaDrawT.lnbI = "PICT" THEN DO
        CALL DrawPict gaDraw.lnbi
      END

      WHEN gaDrawT.lnbI = "TILE" THEN DO
        CALL DrawTile gaDraw.lnbi
      END

      WHEN gaDrawT.lnbI = "LINE" THEN DO
        CALL QueueCntl gaDraw.lnbI
      END

      WHEN gaDrawT.lnbI = "RECT" THEN DO
        CALL QueueCntl gaDraw.lnbI
      END

      WHEN gaDrawT.lnbI = "TEXT" THEN DO
        PARSE VAR gaDraw.lnbI lsbO "@" lnbF lnbS lsbT
        CALL Queue Cntl( lsbO || PushFont( lnbF lnbS ) ) ||,
                   Text( lsbT ) ||,
                   gsTxtOut ||,
                   Cntl( " ET Q" )
        CALL PullFont
      END

      OTHERWISE DO
        NOP
      END
    END
  END

  /* ===================================================================
  -- End the stream
  */
  CALL EndStream

  DROP lnbI

RETURN

/*
-- =====================================================================
-- Routine:   DrawPict         : Adds a picture to the BG stream
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
DrawPict:

  /* ===================================================================
  -- Get draw info
  */
  PARSE ARG lnbN lnbX lnbY lnbO lnbR,
            lnbcX lnbcY,
            lnbkX lnbkY

  /* ===================================================================
  -- Mark as used
  */
  gaImgU.lnbN = gbTrue

  /* ===================================================================
  -- Fixup parameters
  */
  lnbW = gaImgW.lnbN
  lnbH = gaImgH.lnbN
  lnbcX = lnbcX * lnbW
  lnbcY = lnbcY * lnbH

  /* ===================================================================
  -- Adjust mediabox
  */
  CALL Mediabox lnbW lnbH

  /* ===================================================================
  -- Start and position
  */
  CALL QueueCntl "q",
                 "1 0 0 1" lnbX lnbY "cm"

  /* ===================================================================
  -- Relocate and rotate it
  */
  IF gaImgM.lnbN <> gsNULL THEN DO
    CALL QueueCntl gaImgM.lnbN
  END

  /* ===================================================================
  -- Fade it
  */
  IF lnbO <> 100 THEN DO
    CALL QueueCntl AddAlpha( lnbO ) "gs"
  END

  /* ===================================================================
  -- Rotate it
  */
  IF lnbR <> 0 THEN DO
    lnbCos = Cos( Rad( lnbR ) )
    lnbSin = Sin( Rad( lnbR ) )
    CALL QueueCntl lnbCos lnbSin (-lnbSin) lnbCos "0 0 cm"
  END

  /* ===================================================================
  -- Scale it
  */
  CALL QueueCntl lnbcX / 100 "0 0" lnbcY / 100 "0 0 cm"

  /* ===================================================================
  -- Skew it
  */
  IF lnbkX <> 0 | lnbkY <> 0 THEN DO
    lnbkX = Tan( Rad( lnbkX ) )
    lnbkY = Tan( Rad( lnbkY ) )
    CALL QueueCntl "1" lnbkX lnbkY "1 0 0 cm"
  END

  /* ===================================================================
  -- And draw it
  */
  CALL QueueCntl "/XOImg" || lnbN "Do",
                 "Q"

RETURN

/*
-- =====================================================================
-- Routine:   DrawTile         : Adds a tiled picture to the BG
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
DrawTile:

  /* ===================================================================
  -- Get draw info
  */
  PARSE ARG lndtN lndtX lndtY lndtW lndtH lndtO

  /* ===================================================================
  -- Mark target image as used
  */
  gaImgU.lndtN = gbTrue

  /* ===================================================================
  -- Need to emit a pattern object
  */
  gaImgPU.lndtN = gbTrue

  /* ===================================================================
  -- Set the pattern colorspace
  */
  CALL QueueCntl "q /Pattern cs /P"lndtN" scn"

  /* ===================================================================
  -- Fade it
  */
  IF lndtO <> 100 THEN DO
    CALL QueueCntl AddAlpha( lndtO ) "gs"
  END

  /* ===================================================================
  -- Draw the bounding box and fill with pattern
  */
  CALL QueueCntl lndtX lndtY lndtW lndtH "re",
                 "f Q"

RETURN

/*
-- =====================================================================
-- Routine:   Mediabox         : Adjusts size of media box
-- Arguments: lnmW             : width
--            lnmH             : height
-- Return:    (none)           : none required
-- =====================================================================
*/
Mediabox:

  PARSE ARG lnmW lnmH

  /* ===================================================================
  -- Remember the maximum width and height for this page
  */
  IF gbPaperA THEN DO
    IF gaMediaW.gnContentObjs < lnmW THEN DO
      gaMediaW.gnContentObjs = lnmW
    END

    IF gaMediaH.gnContentObjs < lnmH THEN DO
      gaMediaH.gnContentObjs = lnmH
    END
  END

RETURN

/*
-- =====================================================================
-- Routine:   NewPage          : Start a page (content object)
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
NewPage:

  /* ===================================================================
  -- Reset indexing row counter
  */
  gnRow = 1

  /* ===================================================================
  -- End last page if needed
  */
  IF gbInPage THEN DO
    CALL EndPage
  END

  /* ===================================================================
  -- Create a new object
  */
  gnContentObj = NewObj()

  /* ===================================================================
  -- Save object number and number of tracked
  */
  gsContentObjs = gsContentObjs gnContentObj
  gnContentObjs = gnContentObjs + 1

  /* ===================================================================
  -- Queue stream dictionary ( forward point to length object )
  */
  CALL QueueCntl "<<"

  /* ===================================================================
  -- Start the stream
  */
  CALL StartStream

  /* ===================================================================
  -- Handle any conditional images
  */
  DO lnnpI = 1 TO gnDICnt
    IF gaDIStart.lnnpI = gnContentObjs THEN DO
      CALL DrawPict gaDrawIf.lnnpI
      gaDIStart.lnnpI = gaDIStart.lnnpI + gaDINext.lnnpI
    END
  END

  /* ===================================================================
  -- Handle any strings
  */
  DO lnnpI = 1 TO gnStrCnt
    PARSE VAR gaString.lnnpI lsnpQ "@" lnnpF lnnpS lsnpT

    lsnpT = Strfdyn( lsnpT )

    CALL Queue Cntl( lsnpQ || PushFont( lnnpF lnnpS ) ) ||,
               Text( lsnpT ) ||,
               gsTxtOut ||,
               Cntl( " ET Q" )
    CALL PullFont
  END

  /* ===================================================================
  -- Start the text object and set color and font
  */
  CALL QueueCntl "BT"
  PARSE VAR gaTextA.0 lnnpF lnnpS lsnpF
  CALL QueueCntl PushFont( gaTextA.0 ) lsnpF "rg"

  IF gnZoom <> 100 THEN DO
    CALL QueueCntl gnZoom "Tz"
  END

  /* ===================================================================
  -- Queue the text matrix:
  --
  -- 1 = x-axis scaling       --\
  -- 0 = x-axis skew (slant)    - Rotation
  -- 0 = y-axis skew (slant)    -
  -- 1 = y-axis scaling       --/
  -- 0 = x-axis translation (offset)
  -- 0 = y-axis translation (offset)
  */
  CALL QueueCntl "1 0 0 1" gnTextL gnTextT "Tm"

  /* ===================================================================
  -- Specify the leading units
  */
  CALL QueueCntl gnLeading "TL"

  /* ===================================================================
  -- Reset line count
  */
  gnLineCount = 0

  /* ===================================================================
  -- Set paging indicator
  */
  gbInPage = gbTrue

RETURN

/*
-- =====================================================================
-- Routine:   EndPage          : End a page (content object)
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
EndPage:

  /* ===================================================================
  -- Bypass if not needed
  */
  IF gbInPage = gbFalse THEN DO
    RETURN
  END

  /* ===================================================================
  -- Pull font off stack
  */
  CALL PullFont

  /* ===================================================================
  -- End the text object
  */
  CALL QueueCntl "ET"

  /* ===================================================================
  -- End the stream
  */
  CALL EndStream

  /* ===================================================================
  -- Clear paging indicator
  */
  gbInPage = gbFalse

RETURN

/*
-- =====================================================================
-- Routine:   NewObj           : Starts a new object and tracks
--                             : offsets
-- Arguments: (none)           : none required
-- Return:    gnObjs           : ID of object
-- =====================================================================
*/
NewObj:

  /* ===================================================================
  -- Increment number of objects and store its' offset
  */
  gnObjs = gnObjs + 1
  gaOffsets.gnObjs = gnOffset

  /* ===================================================================
  -- Queue the object id
  */
  CALL QueueCntl gnObjs "0 obj"

RETURN gnObjs

/*
-- =====================================================================
-- Routine:   EndObj           : Completes an object
--                             :   (rather simplistic)
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
EndObj:

  /* ===================================================================
  -- Queue the "endobj" keyboard
  */
  CALL QueueCntl "endobj"

RETURN

/*
-- =====================================================================
-- Routine:   StartStream      : Starts a new stream and tracks
--                             : offsets
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
StartStream:

  /* ===================================================================
  -- Show that we're in a stream
  */
  gbInStream = gbTrue
  gsStream = ""

RETURN

/*
-- =====================================================================
-- Routine:   EndStream        : Completes a stream and queues
--                             : length object
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
EndStream:

  PARSE ARG lsesFilters

  /* ===================================================================
  -- Compressing?
  */
  IF gnCompLvl <> 0 THEN DO
    gnTotBytes = gnTotBytes + Length( gsStream )
    gsStream = T2PComp( gsStream, gnCompLvl )

    lsesFilters = "/FlateDecode" || lsesFilters
  END

  /* ===================================================================
  -- Encrypting?
  */
  IF gbEncrypt THEN DO
    gsStream = Crypt( gsStream )
  END

  /* ===================================================================
  -- Not in a stream anymore
  */
  gbInStream = gbFalse

  /* ===================================================================
  -- Queue any filters we may have
  */
  IF lsesFilters <> gsNull THEN DO
    CALL QueueCntl "/Filter[" || lsesFilters || "]"
  END

  /* ===================================================================
  -- Queue the length of the stream content
  */
  IF Length( gsStream ) = 0
  Then CALL QueueCntl "/Length" Length( gsStream )
  Else CALL QueueCntl "/Length" Length( gsStream ) - 1

  /* ===================================================================
  -- Finish the dictionary
  */
  CALL QueueCntl ">>"

  /* ===================================================================
  -- Start the stream
  */
  CALL QueueCntl "stream"

  /* ===================================================================
  -- Write the data
  */
  CALL WriteFile gsStream

  /* ===================================================================
  -- Terminiate the stream and object
  */
  CALL QueueCntl "endstream"
  CALL EndObj

RETURN

/*
-- =====================================================================
-- Routine:   QueueCntl        : Queues control record & tracks
--                             : file offset
-- Arguments: lsqcData         : Record to queue
-- Return:    (none)           : none required
--
-- Note:      The Queue() and Cntl() functions have been inlined
--            and local variables are not dropped to improve
--            performance.
-- =====================================================================
*/
QueueCntl:

  PARSE ARG lsqcData

  /* ===================================================================
  -- Processing a stream?
  */
  IF gbInStream THEN DO

    /* =================================================================
    -- Accumulate stream data
    */
    gsStream = gsStream || Translate( lsqcData, gsCPCntl ) || gsCrlf

  END
  ELSE DO

    /* =================================================================
    -- Write the stream data
    */
    CALL WriteFile Translate( lsqcData, gsCPCntl ) || gsCrlf

  END

RETURN

/*
-- =====================================================================
-- Routine:   QueueText        : Queues a line while tracking
--                             : linecount
-- Arguments: lsqtData         : Record to queue
-- Return:    (none)           : none required
--
-- Note:      The Queue() and String() functions have been inlined
--            and local variables are not dropped to improve
--            performance.
-- =====================================================================
*/
QueueText:

  PARSE ARG lsqtData

  /* ===================================================================
  -- End the page when full
  */
  IF gnLineCount > gnLPP THEN DO
    CALL EndPage
  END

  /* ===================================================================
  -- Start a page if needed
  */
  IF gbInPage = gbFalse THEN DO
    CALL NewPage
    gnLineCount = gnLineCount + 1
  END

  /* ===================================================================
  -- Outlining?
  */
  IF gbHasOL THEN DO
    lsIdx = gsNull

    SELECT
      /* ===============================================================
      -- Look for any string in given column on any row
      */
      WHEN gsOType = "SCANCOL" THEN DO
        IF Substr( lsqtData, gnOCol, gnOKeyL ) = gsOKey THEN DO
          IF gbOColRel THEN DO
            lsIdx = Substr( lsqtData,,
                            gnOCol + gnODCol,,
                            gnODLen )
          END
          ELSE DO
            lsIdx = Substr( lsqtData, gnODCol, gnODLen )
          END
        END
      END

      /* ===============================================================
      -- Look for any string in any column on given row
      */
      WHEN gsOType = "SCANROW" THEN DO
        IF gnORow = gnRow THEN DO
          lnP = Pos( gsOKey, lsqtData )
          IF lnP <> 0 THEN DO
            IF gbOColRel THEN DO
              lsIdx = Substr( lsqtData, lnP + gnODCol, gnODLen )
            END
            ELSE DO
              lsIdx = Substr( lsqtData, gnODCol, gnODLen )
            END
          END
        END
      END

      /* ===============================================================
      -- Look for any string in any column on any row
      */
      WHEN gsOType = "SCAN" THEN DO
        lnP = Pos( gsOKey, lsqtData )
        IF lnP <> 0 THEN DO
          IF gbOColRel THEN DO
            lsIdx = Substr( lsqtData, lnP + gnODCol, gnODLen )
          END
          ELSE DO
            lsIdx = Substr( lsqtData, gnODCol, gnODLen )
          END
        END
      END

      /* ===============================================================
      -- Get index from given row and column
      */
      WHEN gsOType = "ROWCOL" THEN DO
        IF gnORow = 0 | gnORow = gnRow THEN DO
          lsIdx = Substr( lsqtData, gnODCol, gnODLen )
        END
      END

      OTHERWISE DO
        SAY did_not_account_for_all_cases_in_select
      END
    END

    lsIdx = Strip( lsIdx, "T" )
    IF lsIdx <> gsNull THEN DO
      gsIdxLast = lsIdx
      gnIdxCnt = gnIdxCnt + 1
      gaIdxText.gnIdxCnt = lsIdx ||,
                           "-" ||,
                           gnContentObj,
                           gnLineCount
    END
  END

  /* ===================================================================
  -- Limiting output record length?
  */
  IF Length( lsqtData ) > gnORLen THEN DO
    lsqtData = Left( lsqtData, gnORLen )
  END

  IF gnTACnt <> 0 THEN DO
    CALL TextAttr lsqtData
  END
  ELSE DO
    /* =================================================================
    -- Queue
    */
    gsStream = gsStream ||,
               gsFeed   ||,
               gsReturn ||,
               Text( lsqtData ) ||,
               gsTxtPut ||,
               gsCrlf

  END

  /* ===================================================================
  -- Clear linefeeds
  */
  gsFeed = gsNull

  /* ===================================================================
  -- Track current row
  */
  gnRow = gnRow + 1

RETURN

/*
-- =====================================================================
-- Routine:   TextAttr         : Add attributes to text
-- Arguments: lstaData         : Data to scan
-- Return:    (none)           : none required
--
-- Note:      Local variables are not dropped to improve
--            performance.
-- =====================================================================
*/
TextAttr:

  PARSE ARG lstaData

  /* ===================================================================
  -- Set default to represent global attributes
  */
  lataAttr. = 0
  lbtaFound = gbFalse

  /* ===================================================================
  -- Scan line and build attribute array
  */
  DO lntaI = 1 TO gnTACnt

    /* =================================================================
    -- Ignore null entries (see Convert)
    */
    IF gaTextA.lntaI = gsNull THEN DO
      ITERATE
    END

    /* =================================================================
    -- Get key and length
    */
    PARSE VAR gaTextC.lntaI lntaType lntaParm

    SELECT
      WHEN lntaType = "ROWCOL" THEN DO
        PARSE VAR lntaParm lntaRow lntaCol lntaLen

        /* =============================================================
        -- Scan line for key
        */
        IF lntaRow = 0 | lntaRow = gnRow THEN DO

          /* ===========================================================
          -- Set attribute index in attribute array
          */
          lntaL = lntaCol + lntaLen - 1
          DO lntaA = lntaCol TO lntaL
            lataAttr.lntaA = lntaI
          END

          lbtaFound = gbTrue
        END
      END

      WHEN lntaType = "SCAN" THEN DO
        PARSE VAR lntaParm lntaKeyL lstaKey

        /* =============================================================
        -- Scan line for key
        */
        lntaA = 0
        lntaL = 1
        lntaP = Pos( lstaKey, lstaData )
        DO WHILE lntaP <> 0

          /* ===========================================================
          -- Calc length of run
          */
          lntaN = lntaP + lntaKeyL - 1

          /* ===========================================================
          -- Set attribute index in attribute array
          */
          DO lntaA = lntaP TO lntaN
            lataAttr.lntaA = lntaI
          END

          /* ===========================================================
          -- Scan for next
          */
          lntaP = Pos( lstaKey, lstaData, lntaN + 1 )
            END

        IF lntaA <> 0 THEN DO
          lbtaFound = gbTrue
        END
      END

      OTHERWISE DO
        SAY did_not_account_for_all_cases_in_select
      END
    END
  END

  /* ===================================================================
  -- Emit as one and get out if no matches found
  */
  IF lbtaFound = gbFalse THEN DO
    /* =================================================================
    -- Queue
    */
    gsStream = gsStream ||,
               gsFeed   ||,
               gsReturn ||,
               Text( lstaData ) ||,
               gsTxtPut ||,
               gsCrlf
    RETURN
  END

  /* ===================================================================
  -- Start the line
  */
  gsStream = gsStream ||,
             gsFeed   ||,
             gsReturn

  /* ===================================================================
  -- Segment line based on attribute array
  */
  lntaL = 0
  DO lntaI = 1 TO Length( lstaData )

    /* =================================================================
    -- Grab the attribute index
    */
    lntaA = lataAttr.lntaI

    /* =================================================================
    -- Look for end of run
    */
    DO lntaJ = lntaI TO Length( lstaData )
      IF lataAttr.lntaJ <> lntaA THEN DO
        LEAVE
      END
    END

    /* =================================================================
    -- Extract the segment from the line
    */
    lstaSeg = Substr( lstadata, lntaI, lntaJ - lntaI )

    /* =================================================================
    -- Emit the attributes if this is or last was non-global
    */
    IF lntaA <> 0 | lntaL <> 0 THEN DO
      PARSE VAR gaTextA.lntaA lntaF lntaS lstaC

      gsStream = gsStream ||,
                 Cntl( PushFont( lntaF lntaS ) lstaC "rg" )
    END

    /* =================================================================
    -- Emit the segment
    */
    gsStream = gsStream ||,
               Text( lstaSeg ) ||,
               gsTxtPut

    /* =================================================================
    -- Restore font
    */
    IF lntaA <> 0 | lntaL <> 0 THEN DO
      CALL PullFont
    END

    /* =================================================================
    -- Remember this attribute index
    */
    lntaL = lntaA

    /* =================================================================
    -- Start of next run
    */
    lntaI = lntaJ - 1
  END

  /* ===================================================================
  -- Reset to default if last was different
  */
  IF lntaL <> 0 THEN DO
    PARSE VAR gaTextA.0 . . lstaC
    gsStream = gsStream ||,
               Cntl( GetFont() lstaC "rg" )
  END

  /* ===================================================================
  -- Add line terminator
  */
  gsStream = gsStream ||,
             gsCrlf

RETURN

/*
-- =====================================================================
-- Routine:   Queue            : Queues translated line
-- Arguments: lsqData          : Record to queue
-- Return:    (none)           : none required
--
-- Note:      Local variables are not dropped to improve
--            performance.
-- =====================================================================
*/
Queue:

  PARSE ARG lsqData

  /* ===================================================================
  -- Processing a stream?
  */
  IF gbInStream THEN DO

    /* =================================================================
    -- Accumulate stream data
    */
    gsStream = gsStream || lsqData || gsCrlf

  END
  ELSE DO
    /* =================================================================
    -- Write the stream data
    */
    CALL WriteFile lsqData || gsCrlf

  END

RETURN

/*
-- =====================================================================
-- Routine:   Escape           : Escapes characters within text
-- Arguments: lseData          : Data to scan
-- Return:    lseData          : Escaped data
--
-- Note:      This routine scans translated data
--
-- Note:      Local variables are not dropped to improve
--            performance.
-- =====================================================================
*/
Escape:

  PARSE ARG lseData

  /* ===================================================================
  -- Prefix characters with a backslash (\)
  */
  lnePos = 1
  DO FOREVER
    lnePos =  Verify( lseData, gsEscChars, "M", lnePos )
    IF lnePos = 0 THEN DO
      LEAVE
    END
    lseData = Insert( gsEsc, lseData, lnePos - 1 )
    lnePos = lnePos + 2
  END

RETURN lseData

/*
-- =====================================================================
-- Routine:   Cntl             : Translate control strings
-- Arguments: lscData          : Data to convert
-- Return:    lscData          : Converted data
-- =====================================================================
*/
Cntl:

  PARSE ARG lscData

  lscData = Translate( lscData, gsCPCntl )

RETURN lscData

/*
-- =====================================================================
-- Routine:   String           : Translate and escape strings
-- Arguments: lstData          : String value
-- Return:    lstData          : Converted and escaped string
--
-- Note:      This function is used outside of page objects and doesn't
--            track subsetting information.
--
-- Note:      The Escape() function has been inlined and local
--            variables are not dropped to improve performance.
-- =====================================================================
*/
String:

  PARSE ARG lssData

  /* ===================================================================
  -- Convert to ASCII
  */
  lssData = Translate( lssData, gsCPCntl )

  /* ===================================================================
  -- Encrypting?
  */
  IF gbEncrypt THEN DO

    /* =================================================================
    -- Encrypt and escape
    */
    lssData = gsBegHex ||,
              Translate( C2x( Crypt( lssData ) ), gsCPCntl ) ||,
              gsEndHex

  END
  ELSE DO

    /* =================================================================
    -- Prefix characters with a backslash (\)
    */
    lnsPos = 1
    DO FOREVER
      lnsPos =  Verify( lssData, gsEscChars, "M", lnsPos )
      IF lnsPos = 0 THEN DO
        LEAVE
      END
      lssData = Insert( gsEsc, lssData, lnsPos - 1 )
      lnsPos = lnsPos + 2
    END

    /* =================================================================
    -- Wrap text
    */
    lssData = gsBegStr ||,
              lssData ||,
              gsEndStr
  END

RETURN lssData

/*
-- =====================================================================
-- Routine:   Text             : Translate and escape text
-- Arguments: lstData          : String value
-- Return:    lstData          : Converted and escaped string
--
-- Note:      This function should be used for any text within a page
--            object to allow tracking of subsetting information.
--
-- Note:      The Escape() function has been inlined and local
--            variables are not dropped to improve performance.
-- =====================================================================
*/
Text:

  PARSE ARG lstData

  /* ===================================================================
  -- Convert to ASCII
  */
  lstData = Translate( lstData, gsCPText )

  /* ===================================================================
  -- Remember which characters were used
  */
  gaFontSubset.gnFont = Translate( gaFontSubset.gnFont, ,lstData, '00'x )

  /* ===================================================================
  -- Prefix characters with a backslash (\)
  */
  lntPos = 1
  DO FOREVER
    lntPos =  Verify( lstData, gsEscChars, "M", lntPos )
    IF lntPos = 0 THEN DO
      LEAVE
    END
    lstData = Insert( gsEsc, lstData, lntPos - 1 )
    lntPos = lntPos + 2
  END

  /* ===================================================================
  -- Wrap text
  */
  lstData = gsBegStr ||,
            lstData ||,
            gsEndStr

RETURN lstData

/*
-- =====================================================================
-- Routine:       Prec         : Output with limited precision
-- Arguments:     lnVal        : Number
-- =====================================================================
*/
Prec:
RETURN Format( Arg( 1 ), , 9, 0 )

/*
-- =====================================================================
-- Routine:       Deg          : Convert radians to degrees
-- Arguments:     lnD          : Number
-- =====================================================================
*/
Deg:
PROCEDURE ; ARG lnR
  NUMERIC DIGITS 30
RETURN Prec( ( 180 / PI() ) * lnR )

/*
-- =====================================================================
-- Routine:       Rad          : Convert degrees to radians
-- Arguments:     lnD          : Number
-- =====================================================================
*/
Rad:
PROCEDURE ; ARG lnD
  NUMERIC DIGITS 30
RETURN Prec( ( PI() / 180 ) * lnD )

/*
-- =====================================================================
-- Routine:       Atan         : Calculate arc tangent
-- Arguments:     lnX          : Number
-- =====================================================================
*/
Atan:
PROCEDURE ; ARG lnX
  NUMERIC DIGITS 30

  lnCnt = Trunc( 10 * Abs( lnX ) + 14 )

  lnX2 = lnX * lnX

  lnRes = 0

  DO lnNdx = lnCnt TO 1 BY -1
    lnRes = ( lnNdx * lnNdx * lnX2 ) /,
            ( lnNdx + lnNdx + 1 + lnRes )
  END

RETURN Prec( lnX / ( 1 + lnRes ) )

/*
-- =====================================================================
-- Routine:       Tan          : Calculate tangent
-- Arguments:     lnX          : Number
-- =====================================================================
*/
Tan:
PROCEDURE
  NUMERIC DIGITS 30
RETURN Prec( Sin( Arg( 1 ) ) / Cos( Arg( 1 ) ) )

/*
-- =====================================================================
-- Routine:       Cos          : Calculate cosine
-- Arguments:     lnX          : Number
-- =====================================================================
*/
Cos:
PROCEDURE ; ARG lnX
  NUMERIC DIGITS 30

  lnPi = PI()
  lnSignum = 1
  lnX = Abs( lnX )

  lnPim2 = lnPi * 2
  lnX = lnX // lnPim2
  lnPid2 = lnPi / 2

  IF lnX > lnPi THEN DO
    lnX = lnX - lnPi
    lnSignum = -lnSignum
  END

  IF lnX > lnPid2 THEN DO
    lnX = lnPi - lnX
    lnSignum = -lnSignum
  END

  lnTerm = 1
  lnXsup2 = lnX * lnX
  lnSum = 1
  lnF = 1

  DO lnJ = 2 by 2
    lnTerm = -lnTerm * lnXsup2 / ( lnJ * ( lnJ - 1 ) )
    lnNewSum = lnSum + lnTerm

    IF lnNewSum = lnSum THEN DO
      LEAVE
    END

    lnSum = lnNewSum
  END

RETURN Prec( lnSignum * lnSum )

/*
-- =====================================================================
-- Routine:       Sin          : Calculate sine
-- Arguments:     lnX          : Number
-- =====================================================================
*/
Sin:
PROCEDURE ; ARG lnX
  NUMERIC DIGITS 30

  lnPi = PI()
  lnSignum = 1

  IF lnX < 0 THEN DO
    lnSignum = -1
    lnX = -lnX
  END

  lnPim2 = lnPi * 2
  lnX = lnX // lnPim2
  lnPid2 = lnPi / 2

  IF lnX > lnPi THEN DO
    lnX = lnX - lnPi
    lnSignum = -lnSignum
  END

  IF lnX > lnPid2 THEN DO
    lnX = lnPi - lnX
  END

  lnTerm = lnX
  lnXsup2 = lnX * lnX
  lnSum = lnX
  lnF = 1

  DO lnJ = 3 BY 2
    lnTerm = -lnTerm * lnXsup2 / ( lnJ * ( lnJ - 1 ) )
    lnNewSum = lnSum + lnTerm

    IF lnNewSum = lnSum THEN DO
      LEAVE
    END

    lnSum = lnNewSum
  END

RETURN Prec( lnSignum * lnSum )

/*
-- =====================================================================
-- Routine:       Sqrt         : Calculate square root
-- Arguments:     lnN          : Number
-- =====================================================================
*/
Sqrt:
PROCEDURE ; ARG lnN
  NUMERIC DIGITS 30

  PARSE VALUE Format( lnN, , , , 0 ) WITH lnN "E" lnExp

  IF lnExp = "" THEN DO
    lnExp = 0
  END

  IF ( lnExp // 2 ) <> 0 THEN DO
    IF lnExp > 0 THEN DO
      lnN = lnN * 10
      lnExp = lnExp - 1
    END
    ELSE DO
      lnN = lnN / 10
      lnExp = lnExp + 1
    END
  END

  lnX = 0.5 * ( lnN + 1 )

  DO FOREVER
    lnNewX = 0.5 * ( lnX + lnN / lnX )

    IF lnX = lnNewX THEN DO
      LEAVE
    END

    lnX = lnNewX
  END

RETURN Prec( lnX * 10 ** ( lnExp % 2 ) )

/*
-- =====================================================================
-- Routine:       PI           : Calculate PI
-- Arguments:     (none)       : none required
-- =====================================================================
*/
PI:
PROCEDURE

  lnX = Sqrt( 2 )
  lnPi = 2 + lnX
  lnY = Sqrt( lnX )
  lnX = lnY

  DO FOREVER
    lnX = 0.5 * ( lnX + 1 / lnX )
    lnNewPi = lnPi * ( lnX + 1 ) / ( lnY + 1 )

    IF lnPi = lnNewPi THEN DO
      LEAVE
    END

    lnPi = lnNewPi
    lnX = Sqrt( lnX )
    lnY = ( lnY * lnX + 1 / lnX ) / ( lnY + 1 )
  END

RETURN Prec( lnPi )

/*
-- =====================================================================
-- Routine:   QSort           : Quicksort a stem
-- Arguments: laqsN           : Stem name
--            lbqsD           : Direction: 0 = Asc, 1 = Dec
--            lnqsL           : Left (starting) entry
--            lnqsR             : Right (ending) entry
-- =====================================================================
*/
QSort:
  PARSE ARG laqsN, lbqsD, lnqsL, lnqsR

  PUSH lnqsL lnqsR
  DO WHILE QUEUED() > 0
    PULL lnqsL lnqsR

    IF lnqsL < lnqsR THEN DO
      lsqsX = Value( laqsN || lnqsL )
      lnqsI = lnqsL - 1
      lnqsQ = lnqsR + 1
      DO FOREVER
        IF lbqsD THEN DO
          DO UNTIL Value( laqsN || lnqsQ ) >= lsqsX
            lnqsQ = lnqsQ - 1
          END
          DO UNTIL Value( laqsN || lnqsI ) <= lsqsX
            lnqsI = lnqsI + 1
          END
        END
        ELSE DO
          DO UNTIL Value( laqsN || lnqsQ ) <= lsqsX
            lnqsQ = lnqsQ - 1
          END
          DO UNTIL Value( laqsN || lnqsI ) >= lsqsX
            lnqsI = lnqsI + 1
          END
        END
        IF lnqsI < lnqsQ THEN DO
          CALL Value laqsN || lnqsQ,,
               Value( laqsN || lnqsI, Value( laqsN || lnqsQ ) )
        END
        ELSE DO
          LEAVE
        END
      END

      PUSH lnqsL lnqsQ
      PUSH lnqsQ + 1 lnqsR
    END
  END

RETURN

/*
-- =====================================================================
-- Routine:   Initialize       : Initialize the world
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
Initialize:

  /* ===================================================================
  -- Some misc variables first
  */
  gsNull        = ""
  gbTrue        = 1
  gbFalse       = 0

  gbMsgid       = gbTrue
  gnMsgN        = 0
  gnMsgE        = 1
  gnMsgI        = 2
  gnMsgV        = 3
  gnMsgD        = 4
  gnConfirm     = gnMsgI
  gbBrowse      = gbFalse
  gnRC          = 0
  gbMsg         = gbFalse
  gaMsg.        = gsNull
  gaMsgs.       = gsNull
  gaMsgs.0      = 0
  gaMsgTOver.   = gsNull
  gsMsgLang     = "ENU"
  gsArgs        = gsNull
  gsSaveArgs    = gsNull
  gsKey         = gsNull
  gsVal         = gsNull
  gsHLQ         = gsNull
  gsUnits       = "INCHES"
  gnUnits       = 72

  /* ===================================================================
  -- Document information
  */
  gsProducer    = "TXT2PDF"         /* Our prefered name */
  gsAuthor      = "Leland Lucius"   /* The culprits */
  gsVersion     = "v" || ver        /* Our very own version # */
  gsDInfo       = gsNull            /* DocInfo path */
  gaDInfo.      = gsNull            /* DocInfo data */

  /* ===================================================================
  -- Go determine environment
  */
  CALL Environ

  /* ===================================================================
  -- Go define codepage tables
  */
  CALL DefineCPTables "TXT2PDFX", gbFalse

  /* ===================================================================
  -- Encoding support (must be initialize before font support)
  */
  gnEncodings     = 0
  gaEncodingName. = gsNull
  gaEncodingPath. = gsNull
  gaEncodingIndx. = 0
  gaEncodingObj.  = 0
  gaEncoding.     = gsNull
  gaDiffsObj.     = 0
  CALL DEncoding "WinAnsi"
  CALL DEncoding "MacRoman"
  CALL WinAnsiEncoding
  CALL MacRomanEncoding

  /* ===================================================================
  -- Font support
  */
  gnFonts = 0
  gaFontb. = gsNull
  gaFontu. = gbFalse
  gaFontt. = gsNull
  gaFontn. = gsNull
  gaFontp. = gsNull
  gaFonti. = 0
  gaFonte. = 0
  gaFont_Descriptor. = 0
  gaFontSubset. = Xrange( '00'x, 'FF'x )
  gaFontStack. = 0

  /* ===================================================================
  -- Here for reference.  Not initializing since I want to get
  -- an error if one is referenced without being previously set.
  --

  gaFont_FontName.
  gaFont_BaseFont.
  gaFont_BBox.
  gaFont_Flags.
  gaFont_ItalicAngle.
  gaFont_Ascent.
  gaFont_Descent.
  gaFont_CapHeight.
  gaFont_StemV.
  gaFont_FirstChar.
  gaFont_LastChar.
  gaFont_MissingWidth.
  gaFont_Glyf_Width.
  --
  */

  CALL DFont "Courier             C  ", "Courier"
  CALL DFont "CourierBold         CB ", "Courier-Bold"
  CALL DFont "CourierItalic       CI ", "Courier-Oblique"
  CALL DFont "CourierBoldItalic   CBI", "Courier-BoldOblique"
  CALL DFont "Helvetica           H  ", "Helvetica"
  CALL DFont "HelveticaBold       HB ", "Helvetica-Bold"
  CALL DFont "HelveticaItalic     HI ", "Helvetica-Oblique"
  CALL DFont "HelveticaBoldItalic HBI", "Helvetica-BoldOblique"
  CALL DFont "Times               T  ", "Times-Roman"
  CALL DFont "TimesBold           TB ", "Times-Bold"
  CALL DFont "TimesItalic         TI ", "Times-Italic"
  CALL DFont "TimesBoldItalic     TBI", "Times-BoldItalic"
  CALL DFont "Symbol              S  ", "Symbol"
  CALL DFont "ZapfDingbats        Z  ", "ZapfDingbats"

  /* ===================================================================
  -- Color support
  */
  gsColors = gsNull
  gaColors. = gsNull
  gaColorn. = gsNull
  CALL DColor "YellowBar    ", "1.0  1.0  0.95"
  CALL DColor "WhiteBar     ", "1.0  1.0  1.0 "
  CALL DColor "OrangeBar    ", "1.0  0.97 0.93"
  CALL DColor "GreenBar     ", "0.95 1.0  0.95"
  CALL DColor "GrayBar      ", "0.95 0.95 0.95"
  CALL DColor "BlueBar      ", "0.95 1.0  1.0 "
  CALL DColor "Yellow    Y  ", "1.0  1.0  0.0 "
  CALL DColor "White     W  ", "1.0  1.0  1.0 "
  CALL DColor "Teal      T  ", "0.0  0.5  0.5 "
  CALL DColor "Silver    S  ", "0.75 0.75 0.75"
  CALL DColor "Red       R  ", "1.0  0.0  0.0 "
  CALL DColor "Purple    P  ", "0.5  0.0  0.5 "
  CALL DColor "Olive     O  ", "0.5  0.5  0.0 "
  CALL DColor "Navy      N  ", "0.0  0.0  0.5 "
  CALL DColor "Maroon    M  ", "0.5  0.0  0.0 "
  CALL DColor "Lime      L  ", "0.0  1.0  0.0 "
  CALL DColor "Green     Gre", "0.0  0.5  0.0 "
  CALL DColor "Gray      Gra", "0.5  0.5  0.5 "
  CALL DColor "Fuchsia   F  ", "1.0  0.0  1.0 "
  CALL DColor "Blue      Blu", "0.0  0.0  1.0 "
  CALL DColor "Black     Bla", "0.0  0.0  0.0 "
  CALL DColor "Aqua      A  ", "0.0  1.0  1.0 "

  /* ===================================================================
  -- Paper attributes
  */
  gbPaperA      = gbFalse           /* Automatic paper size */
  gnPaperW      = Points( 8.5 )     /* Paper width */
  gnPaperH      = Points( 11 )      /* Paper height */
  gsPaperT      = gsNull            /* Paper type */
  gsPaperS      = gsNull            /* Paper style */
  gsOrient      = "P"               /* Paper orientation */

  /* ===================================================================
  -- Background attributes
  */
  gbHasBG       = gbFalse           /* Has background */
  gnBackObj     = 0
  gbHasBars     = gbFalse           /* Has color bars */
  gnBarObj      = 0                 /* Bar object ID */
  gsBarColorF   = gsNull            /* Bar foreground color */
  gsBarColorB   = gsNull            /* Bar background color */
  gbHasHoles    = gbFalse           /* Has fake holes */
  gnHoleObj     = 0                 /* Hole object ID */
  gnHoleSize    = 9                 /* Hole size */
  gbBackS       = gbFalse           /* STREAM style used */
  gnBackC       = 0                 /* # of background specs */
  gaBackT.      = gsNull            /* Background types */
  gaBackP.      = gsNull            /* Background paths */

  /* ===================================================================
  -- Page attributes
  */
  gaMediaH.     = 0                 /* Page Mediabox Height */
  gaMediaW.     = 0                 /* Page Mediabox Width */
  gnMarginL     = Points( 0.5 )     /* Left Margin */
  gnMarginR     = Points( 0.5 )     /* Right Margin */
  gnMarginT     = Points( 0.5 )     /* Top Margin */
  gnMarginB     = Points( 0.5 )     /* Bottom Margin */

  /* ===================================================================
  -- Text attributes
  */
  gnFont        = 0                 /* Font index */
  gsFont        = "Courier"         /* Font name */
  gnPoints      = 9                 /* Font size */
  gnZoom        = 100               /* Font zoom */
  gnLPI         = 8                 /* Line per inch */
  gsCC          = gsNull            /* Carriage control */
  gnORLen       = 999999            /* Output record length */
  gnLeading     = 0                 /* Line "height" */

  /* ===================================================================
  -- Transition attributes
  */
  gsTrans       = gsNull            /* Transition dictionary */
  gsTransS      = gsNull            /* Transition */
  gsTransD      = gsNull            /* Duration */
  gsTransDm     = gsNull            /* Dimension */
  gsTransM      = gsNull            /* Motion */
  gsTransDi     = gsNull            /* Direction */

  /* ===================================================================
  -- Color attributes
  */
  gsColorFT     = "Black"           /* Specified color */
  gsColorF      = Colorspec( "Black" ) /* Foreground color */
  gsColorBT     = "White"           /* Specified color */
  gsColorB      = Colorspec( "White" ) /* Background color */

  /* ===================================================================
  -- Alpha state
  */
  gnAlphaCnt    = 0                 /* Number of alpha states */
  gaAlpha.      = 0                 /* Alpha states */
  gsAlphaBG     = gsNull            /* Background alpha */

  /* ===================================================================
  -- Outlining
  */
  gbHasOL       = gbFalse           /* Outlining? */
  gsOType       = gsNull            /* Outline type */
  gsOTok        = gsNull            /* Token separator */
  gnOTokLen     = 0                 /* Token length */
  gbODups       = gbFalse           /* Allow duplicates */
  gbOShow       = gbFalse           /* Include token */
  gbOFull       = gbFalse           /* Register full name */
  gbOSort       = gbFalse           /* Sort outline */
  gbOSDir       = gbFalse           /* Sort direction */
  gnOExpand     = -1                /* Expand outline */
  gnOFlags      = 0                 /* Outline flags */
  gaOColor.1    = Colorspec( "Black" ) /* Text color */
  gaOColor.2    = Colorspec( "Black" ) /* Alt. text color */
  gnOCIdx       = 1                 /* Color index */
  gnIdxCnt      = 0                 /* # of indexes */
  gaIdxText.    = gsNull            /* Index text */
  gaIdxPage.    = 0                 /* Index object IDs */
  gsIdxLast     = gsNull            /* Last index seen */
  gnRow         = 1                 /* Page row */

  /* ===================================================================
  -- I/O related variables
  */
  gnReadC       = 1000
  gnWriteC      = 1000
  gsFileI       = gsNull
  gsFileO       = gsNull
  gsOutH        = gsNull
  gsOutDD       = gsNull
  gsOutFn       = gsNull
  gsOutFt       = gsNull
  gsOutFm       = gsNull
  gaOut.        = gsNull
  gnOut         = 0
  gnMaxSize     = 0
  gaCMSfh.0     = 0
  gaOMVSfh.     = gsNULL

  /* ===================================================================
  -- Image related variables
  */
  gnImgCnt      = 0                /* count           */
  gaImg.        = gsNull           /* image data      */
  gaImgU.       = gbFalse          /* used            */
  gaImgO.       = 0                /* object          */
  gaImgPU.      = gbFalse          /* has pattern     */
  gaImgPO.      = 0                /* pattern object  */
  gaImgM.       = gsNull           /* Transform       */
  gaImgT.       = gsNull           /* type            */
  gaImgH.       = 0                /* height          */
  gaImgW.       = 0                /* width           */
  gaImgReg.     = 0                /* name registry   */

  /* ===================================================================
  -- Draw related variables
  */
  gnDrawCnt     = 0                /* count           */
  gaDrawT.      = gsNull           /* type            */
  gaDraw.       = gsNull           /* type specific   */

  /* ===================================================================
  -- Conditional draw related variables
  */
  gnDICnt       = 0                /* conditional cnt */
  gaDIStart.    = gsNull           /* starting page   */
  gaDINext.     = gsNull           /* next page       */
  gaDrawIfT.    = gsNull           /* type            */
  gaDrawIf.     = gsNull           /* type specific   */

  /* ===================================================================
  -- Text Attribute related variables
  */
  gnTACnt       = 0                /* count           */
  gaTextA.      = gsNull           /* attributes      */
  gaTextC.      = gsNull           /* conditions      */

  /* ===================================================================
  -- String related variables
  */
  gnStrCnt      = 0                /* count           */
  gaString.     = gsNull           /* string text     */

  /* ===================================================================
  -- Annotation variables
  */
  gsAnnTyp      = gsNull
  gsAnnPag      = "ALL"
  gnAnnRow      = 1
  gnAnnCol      = 1
  gsAnnSt8      = "false"
  gsAnnTxt      = gsNull

  /* ===================================================================
  -- Encryption related
  */
  gnEPNone      = 0
  gnEPOpen      = 1
  gnEPSecure    = 2
  gnEPPrint     = 4
  gnEPEdit      = 8
  gnEPCopy      = 16
  gnEPNotes     = 32
  gnEPSaveAs    = 64
  gnEPExt       = 128
  gnEPFill      = 256
  gnEPAccess    = 512
  gnEPAssemble  = 1024
  gnEPHires     = 2048
  gnEPRev2      = 4 + 8 + 16 + 32 + 64 + 128
  gnEPRev3      = 256 + 512 + 1024 + 2048
  gnEPAll       = -1 - ( 1 + 2 )
  gsPerms       = D2c( gnEPAll, 4 )
  gsPPerms      = gsNull
  gnCryptLen    = ( 40 / 8 )
  gnCryptLenX   = gnCryptLen + 5
  gbGennedPw    = gsNull
  gsOwnerPw     = gsNull
  gsUserPw      = gsNull
  gsOwnerKey    = gsNull
  gsUserKey     = gsNull
  gsEncryptKey  = gsNull
  gsFileID      = gsNull
  gbEncrypt     = gbFalse
  gsGennedPw    = gsNull

  /* ===================================================================
  -- Initialize misc variables
  */
  gsDelim       = "/"
  gsPageM       = gsNull
  gsPageL       = gsNull
  gsViewer      = gsNull
  gsMag         = gsNull
  gnLineCount   = 0
  gnTotBytes    = 0
  gnOffset      = 0
  gaOffsets.    = gsNull
  gnObjs        = 0
  gsContentObjs = gsNull
  gnContentObjs = 0
  gnMaxRecLen   = 0
  gnMaxLrecl    = 4096
  gbInPage      = gbFalse
  gbFirstPage   = gbFalse
  gbInStream    = gbFalse
  gbVOnly       = gbFalse
  gnCompLvl     = 0
  gsEmpty       = "ERROR"
  gaCfgs.       = gsNull
  gaCfgs.0      = 0
  gsConfig      = "c"
  gsFeed        = gsNull

RETURN

/*
-- =====================================================================
-- Routine:   InitializeCntl   : Initialize vars based on xlate tables
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
InitializeCntl:

  /* ===================================================================
  -- Verify that the Cntl table defines what we need
  */
  lsicH = '2022252728292a2b2c2d2e2f303132333435363738'x ||,
          '393a3c3e4142434445464748494a4b4c4d4e4f5051'x ||,
          '52535455565758595a5b5c5d616263646566676869'x ||,
          '6a6b6c6d6e6f707172737475767778797a7b7c7d'x
  lsicC = " ""%'()*+,-./0123456789:<>ABCDEFGHIJKLMNOPQ" ||,
          "RSTUVWXYZ[\]abcdefghijklmnopqrstuvwxyz{|}"
  lsicC = Cntl( lsicC )

  IF lsicC <> lsicH THEN DO
    CALL Issue 169
    DO lnicI = 1 TO Length( lsicH )
      lsicHC = Substr( lsicH, lnicI, 1 )
      lsicCC = Substr( lsicC, lnicI, 1 )
      lsicM = gsNull
      IF lsicHC <> lsicCC THEN DO
        lsicM = "- mismatch"
      END
      CALL Issue 170, C2x( lsicHC ), lsicHC,,
                      C2x( lsicCC ), lsicCC,,
                      lsicM
    END
    SIGNAL Done
  END

  /* ===================================================================
  -- Initialize misc variables
  */
  gsCrlf        = '0A'x
  gsBegHex      = Cntl( "<" )
  gsEndHex      = Cntl( ">" )
  gsBegStr      = Cntl( "(" )
  gsEndStr      = Cntl( ")" )
  gsTxtOut      = Cntl( "'" )
  gsTxtPut      = Cntl( "Tj" )
  gsNextLine    = Cntl( "T* " )
  gsReturn      = Cntl( "0 0 Td" )
  gsEsc         = Cntl( "\" )
  gsEscChars    = gsBegStr || gsEndStr || gsEsc

RETURN

/*
-- =====================================================================
-- Routine:   DEncoding        : Define encoding
-- Arguments: lsdeName         : Encoding name(s)
--            lsdePath         : UCM path name
-- Return:    (none)           : none required
-- =====================================================================
*/
DEncoding:
  PARSE ARG lsdeName, lsdePath

  lsdeN = Translate( lsdeName )
  IF gaEncodingIndx.lsdnN <> 0 THEN DO
    CALL Issue 172, lsdeName
    SIGNAL Done
  END

  IF lsdePath <> gsNull THEN DO
    IF Exists( lsdePath ) = gbFalse THEN DO
      CALL Issue 173, lsdePath
      SIGNAL Done
    END
  END

  gnEncodings = gnEncodings + 1
  gaEncodingName.gnEncodings = lsdeName
  gaEncodingPath.gnEncodings = lsdePath
  gaEncodingIndx.lsdeN = gnEncodings

  DROP lsdeName lsdePath lsdeN lndeI

RETURN

/*
-- =====================================================================
-- Routine:   DFont            : Define font
-- Arguments: lsdfName         : Font name(s)
--            lsdfBase         : Font base
--            lsdfType         : Font type
--            lsdfEnc          : Font encoding
--            lsdfFlag         : Font handling flag
--            lsdfPath         : Font file name
-- Return:    (none)           : none required
-- =====================================================================
*/
DFont:
  PARSE ARG lsdfName, lsdfBase, lsdfType, lsdfEnc, lsdfFlag, lsdfPath

  IF lsdfType <> gsNull & Datatype( lsdfName, "A" ) = gbFalse THEN DO
    CALL Issue 114, lsdfName
    SIGNAL Done
  END

  IF lsdfType = gsNull THEN DO
    lsdfType = "INTERNAL"
  END

  IF lsdfEnc = gsNull THEN DO
    lsdfEnc = "WINANSI"
  END

  DO lndfI = Words( lsdfName ) TO 1 BY -1
    lsdfN = Translate( Word( lsdfName, lndfI ) )
    IF gaFonti.lsdfN <> 0 THEN DO
      CALL Issue 174, Word( lsdfName, lndfI )
      SIGNAL Done
    END
  END

  lsdfEnc = Translate( lsdfEnc )
  lndfEnc = gaEncodingIndx.lsdfEnc
  IF lndfEnc = 0 THEN DO
    CALL Issue 175, lsdfEnc
    SIGNAL Done
  END

  IF lsdfType <> "INTERNAL" & lsdfPath <> gsNull THEN DO
    IF Exists( lsdfPath ) = gbFalse THEN DO
      CALL Issue 176, lsdfPath
      SIGNAL Done
    END
  END

  gnFonts = gnFonts + 1
  gaFontn.gnFonts = lsdfName
  gaFontt.gnFonts = lsdfType
  gaFonte.gnFonts = lndfEnc
  gaFontf.gnFonts = lsdfFlag
  gaFontb.gnFonts = lsdfBase
  gaFontp.gnFonts = lsdfPath

  DO lndfI = Words( lsdfName ) TO 1 BY -1
    lsdfN = Translate( Word( lsdfName, lndfI ) )
    gaFonti.lsdfN = gnFonts
  END

  DROP lsdfName lsdfType lsdfEnc lsdfPath lndfI lsdfN lndfEnc

RETURN

/*
-- =====================================================================
-- Routine:   FontSpec         : Validate font and return index
-- Arguments: lsfsFont         : Font name
-- Return:                     : font index
-- =====================================================================
*/
FontSpec:
  PARSE UPPER ARG lsfsFont

  lnfsI = gaFonti.lsfsFont

  IF lnfsI = 0 THEN DO
    CALL Issue 061, lsfsFont
    SIGNAL Done
  END

  DROP lsfsFont

RETURN lnfsI

/*
-- =====================================================================
-- Routine:   EncodingList     : Lists valid encodings
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
EncodingList:

  DO lnelI = 1 TO gnEncodings
    CALL Issue gnMsgE, gaEncodingName.lnelI
  END

  DROP lnelI

RETURN

/*
-- =====================================================================
-- Routine:   FontList         : Lists valid fonts
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
FontList:

  DO lnflI = 1 TO gnFonts
    CALL Issue gnMsgE, gaFontn.lnflI
  END

  DROP lnflI

RETURN

/*
-- =====================================================================
-- Routine:   DColor           : Define color values
-- Arguments: lsdcName         : Color name(s)
--            lsdcRGB          : Color RGB values
-- Return:    (none)           : none required
-- =====================================================================
*/
DColor:
  PARSE ARG lsdcName, lsdcRGB

    DO lsdcI = Words( lsdcName ) TO 1 BY -1
      lsdcN = Translate( Word( lsdcName, lsdcI ) )
      gaColors.lsdcN   = Space( lsdcRGB, 1 )
      gaColorn.lsdcRGB = lsdcN
      gsColors = lsdcN gsColors
    END

    DROP lsdcName lsdcRGB lsdcI lsdcN
RETURN

/*
-- =====================================================================
-- Routine:   ColorSpec        : Retrieve color spec
-- Arguments: lscsColor        : Color name or hex RGB value
-- Return:                     : color spec
-- =====================================================================
*/
ColorSpec:
  PARSE UPPER ARG lscsColor

  IF Length( lscsColor ) = 6 &,
    Datatype( lscsColor, "X" ) THEN DO

    PARSE VAR lscsColor lscsR 3 lscsG 5 lscsB

    lscsR = X2d( lscsR ) / 256
    lscsG = X2d( lscsG ) / 256
    lscsB = X2d( lscsB ) / 256

    lscsColor = lscsR lscsG lscsB

    DROP lscsR lscsG lscsB
  END
  ELSE DO

    IF gaColors.lscsColor = gsNull THEN DO
      CALL Issue 109, lscsColor, gsColors
      SIGNAL Done
    END

    lscsColor = gaColors.lscsColor
  END

RETURN lscsColor

/*
-- =====================================================================
-- Routine:   DefineCPTables   : Define codepage tables
-- Arguments: lsdcName         : Codepage table name
--            lbdcFail         : True=fail if error
-- Return:    (none)           : none required
-- =====================================================================
*/
DefineCPTables:
  ARG lsdcName, lbdcFail

  /* ===================================================================
  -- Attempt to call external routine to get translation tables
  */
  SIGNAL ON SYNTAX NAME NOXLATE
  lsdcTab = gsNull
  INTERPRET "CALL" lsdcName
  lsdcTab = RESULT
  NOXLATE:
  SIGNAL OFF SYNTAX

  /* ===================================================================
  -- Fail if requested and no table returned
  */
  IF lbdcFail & lsdcTab = gsNull THEN DO
    CALL Issue 165, lsdcName
    SIGNAL Done
  END

  /* ===================================================================
  -- Define codepage tables based on platform
  */
  IF Wordpos( gsSystem, "TSO OMVS CMS" ) <> 0 THEN DO

    /* =================================================================
    -- Define ECBDIC to ASCII translation table and ECBDIC
    -- character set
    --
    -- The PDF file is created by wrapping PDF control records
    -- round the input text, and translating the lot to
    -- ASCII.
    --
    -- Two translate tables are defined.  One translates control
    -- characters generated by this exec.  One translates the
    -- input text.  If you use a non-US code page on your
    -- mainframe you should customise this second translate
    -- table suitably.  Do not touch the first translate table.
    */
    gsCPCntl =  '000102039C09867F978D8E0B0C0D0E0F'x || ,
                '101112139D0A08871819928F1C1D1E1F'x || ,
                '808182838485171B88898A8B8C050607'x || ,
                '909116939495960498999A9B14159E1A'x || ,
                '20A0E2E4E0E1E3E5E7F1A22E3C282B7C'x || ,
                '26E9EAEBE8EDEEEFECDF21242A293B5E'x || ,
                '2D2FC2C4C0C1C3C5C7D1A62C255F3E3F'x || ,
                'F8C9CACBC8CDCECFCC603A2340273D22'x || ,
                'D8616263646566676869ABBBF0FDFEB1'x || ,
                'B06A6B6C6D6E6F707172AABAE6B8C6A4'x || ,
                'B57E737475767778797AA1BFD05BDEAE'x || ,
                'ACA3A5B7A9A7B6BCBDBEDDA8AF5DB4D7'x || ,
                '7B414243444546474849ADF4F6F2F3F5'x || ,
                '7D4A4B4C4D4E4F505152B9FBFCF9FAFF'x || ,
                '5CF7535455565758595AB2D4D6D2D3D5'x || ,
                '30313233343536373839B3DBDCD9DA9F'x

    /* =================================================================
    -- Define default input text table
    */
    gsCPText =  '000102039C09867F978D8E0B0C0D0E0F'x || ,
                '101112139D0A08871819928F1C1D1E1F'x || ,
                '808182838485171B88898A8B8C050607'x || ,
                '909116939495960498999A9B14159E1A'x || ,
                '20A0E2E4E0E1E3E5E7F1A22E3C282B7C'x || ,
                '26E9EAEBE8EDEEEFECDF21242A293B5E'x || ,
                '2D2FC2C4C0C1C3C5C7D1A62C255F3E3F'x || ,
                'F8C9CACBC8CDCECFCC603A2340273D22'x || ,
                'D8616263646566676869ABBBF0FDFEB1'x || ,
                'B06A6B6C6D6E6F707172AABAE6B8C6A4'x || ,
                'B57E737475767778797AA1BFD05BDEAE'x || ,
                'ACA3A5B7A9A7B6BCBDBEDDA8AF5DB4D7'x || ,
                '7B414243444546474849ADF4F6F2F3F5'x || ,
                '7D4A4B4C4D4E4F505152B9FBFCF9FAFF'x || ,
                '5CF7535455565758595AB2D4D6D2D3D5'x || ,
                '30313233343536373839B3DBDCD9DA9F'x
  END
  ELSE DO

    /* =================================================================
    -- For all other platforms, simple generate tables that will
    -- cause no translation to take place.
    */
    gsCPCntl = Xrange( '00'x, 'FF'x )
    gsCPText = Xrange( '00'x, 'FF'x )
  END

  /* ===================================================================
  -- Use input text table supplied by user, if any
  */
  IF Length( lsdcTab ) >= 256 THEN DO
    gsCPText = Left( lsdcTab, 256 )
  END

  /* ===================================================================
  -- Use program data table supplied by user, if any
  */
  IF Length( lsdcTab ) = 512 THEN DO
    gsCPCntl = Right( lsdcTab, 256 )
  END

  DROP lsdcTab

RETURN

/*
-- =====================================================================
-- Routine:   Environ         : Determine where we are running
-- Arguments: (none)          : none required
-- Return:    (none)          : none required
-- ---------------------------------------------------------------------
-- Various responses of platforms to which I have access:
--
-- VERSION: OBJREXX 6.00 18 May 1999
-- SOURCE: Windows95 COMMAND <filename>
-- or
-- SOURCE: WindowsNT COMMAND <filename>
--
-- VERSION: REXX-ooRexx_3.2.0(MT) 6.02 30 Oct 2007
-- SOURCE: MACOSX COMMAND INSTORE
--
-- VERSION: REXX-Regina_3.0.1 4.95 12 May 2002
-- SOURCE: UNIX COMMAND <stdin>
--
-- VERSION: REXX370 3.48 01 May 1992
-- SOURCE: TSO COMMAND ./tr PATH ./tr ? SH OMVS OpenMVS
--
-- VERSION: REXX370 3.48 01 May 1992
-- SOURCE: TSO COMMAND INTEREXX SYSPROC ? ? TSO ISPF ?
--
-- VERSION: REXX370 3.48 01 May 1992
-- SOURCE: TSO COMMAND INTEREXX SYSPROC ? ? TSO TSO/E ?
--
-- VERSION: REXX370 3.48 01 May 1992
-- SOURCE: TSO COMMAND Z SYSEXEC ? Z MVS MVS ?
--
-- VERSION: REXX370 4.02 01 Dec 1998
-- SOURCE: CMS COMMAND P2P EXEC A1 p2p CMS
--
-- VERSION: REXX370 4.02 01 Dec 1998
-- SOURCE: CMS COMMAND P2P EXEC A1 p2p ?
*/
Environ:
  /* ===================================================================
  -- Get the system type on which we're running
  */
  PARSE SOURCE lsSource
  PARSE VAR lsSource gsSystem . . . . . gsEnv gsAddr .

  /* ===================================================================
  -- Get the language under which we're running
  */
  PARSE VERSION lsVer
  PARSE VAR lsVer gsLang .

  /* ===================================================================
  -- Normalize
  */
  SELECT
    WHEN gsSystem = "TSO" THEN DO

      /* ===============================================================
      -- Initialize environment
      */
      CALL T2PInit

      /* ===============================================================
      -- Determine high level qualifier
      */
      gsHLQ = Userid()

      /* ===============================================================
      -- Act like a Unix prog when running under USS
      */
      IF gsAddr = "OMVS" THEN DO
        CALL Syscalls( "ON" )
        gsSystem = "OMVS"
      END
    END

    WHEN gsSystem = "CMS" THEN DO
      gsPipe = "PIPE"
      IF gsEnv = "?" THEN DO
        gsPipe = "CALLPIPE"
      END
    END

    WHEN Pos( "REGINA", Translate( gsLang ) ) > 0 THEN DO
      gsSystem = "REGINA"
    END

    WHEN Pos( "OBJREXX", Translate( gsLang ) ) > 0 THEN DO
      gsSystem = "REGINA"
    END

    WHEN Pos( "UNI-REXX", Translate( lsVer ) ) > 0 THEN DO
      gsSystem = "UNI-REXX"
    END

    WHEN Pos( "OOREXX", Translate( lsVer ) ) > 0 THEN DO
      gsSystem = "OOREXX"
    END

    OTHERWISE DO
      CALL Issue 112, lsSource, lsVer
      SIGNAL Done
    END
  END

RETURN

/*
-- =====================================================================
-- Routine:   Boundaries       : Calculate box boundaries
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
Boundaries:

  /* ===================================================================
  -- The Page box
  */
  gnPageL = 0
  IF gsOrient = "L" THEN DO
    gnPageR = gnPaperH
    gnPageT = gnPaperW
  END
  ELSE DO
    gnPageR = gnPaperW
    gnPageT = gnPaperH
  END
  gnPageB = 0
  gnPageW = gnPageR - gnPageL
  gnPageH = gnPageT - gnPageB

  /* ===================================================================
  -- The Crop box
  */
  gnCropL = gnPageL - 0
  gnCropR = gnPageR - 0
  IF gbHasHoles THEN DO
    gnCropL = gnCropL + ( gnHoleSize * 2 )
    gnCropR = gnCropR - ( gnHoleSize * 2 )
  END
  gnCropT = gnPageT - 0
  gnCropB = gnPageB - 0
  gnCropW = gnCropR - gnCropL
  gnCropH = gnCropT - gnCropB

  /* ===================================================================
  -- The Text box
  */
  gnTextL = gnCropL + gnMarginL
  gnTextR = gnCropR - gnMarginR
  gnTextT = gnCropT - gnMarginT
  gnTextB = gnCropB + gnMarginB
  gnTextW = gnTextR - gnTextL
  gnTextH = gnTextT - gnTextB

RETURN

/*
-- =====================================================================
-- Routine:   Points           : Convert arg to current unit
-- Arguments: value            : Value to convert
-- Return:    result           : Converted value
-- =====================================================================
*/
Points:
  RETURN Arg( 1 ) * gnUnits

/*
-- =====================================================================
-- Routine:   NOVALUE          : Uninitialized variable handler
-- Arguments: (none)           : none required
-- Return:    20               : none required
--
-- Note:      This routine terminates the application.
-- =====================================================================
*/
NOVALUE:
  SAY "Uninitialized variable used:"
  SAY
  SAY sigl":" Sourceline( sigl )
EXIT 20

/*
-- =====================================================================
-- Routine:   AddAlpha         : Adds alpha graphics state
-- Arguments: lnaaO            : Opacity percent
-- Return:    index            : state index
-- =====================================================================
*/
AddAlpha:
  PARSE ARG lnaaO

  DO lnaaI = 1 TO gnAlphaCnt
    IF gaAlpha.lnaaI = lnaaO THEN DO
      RETURN "/Alpha" || lnaaI
    END
  END

  gnAlphaCnt = gnAlphaCnt + 1
  gaAlpha.gnAlphaCnt = lnaaO

  DROP lnaaI lnaaO
RETURN "/Alpha" || gnAlphaCnt

/*
-- =====================================================================
-- Routine:   IsJPEG           : Parse and verify JPEG file
-- Arguments: lnijN            : Index into gaImg. stem of file
-- Return:    true/false       : true (yes) or false (no)
-- =====================================================================
*/
IsJPEG:
  PARSE ARG lnijN

  /* ===================================================================
  -- Not even probable if no SOI
  */
  IF Left( gaImg.lnijN, 2 ) <> 'FFD8'x THEN DO
    DROP lnijN
    RETURN 0
  END

  /* ===================================================================
  -- Setup
  */
  lbijValid = gbFalse
  lsijSAMark = "D0 D1 D2 D3 D4 D5 D6 D7 D8 D9 01"

  /* ===================================================================
  --
  */
  lnijI = 3
  DO WHILE lnijI < Length( gaImg.lnijN )

    /* =================================================================
    -- Markers must start with 'FF'x
    */
    IF Substr( gaImg.lnijN, lnijI, 1 ) <> 'FF'x THEN DO
      DROP lnijI lnijN lnijLen lsijMarker lsijSAMark
      RETURN 0
    END

    /* =================================================================
    -- Get the second byte of the marker
    */
    lsijMarker = C2x( Substr( gaImg.lnijN, lnijI + 1, 1 ) )
    lnijI = lnijI + 2

    /* =================================================================
    -- Don't bother continuing if we hit EOI
    */
    IF lsijMarker = 'D9' THEN DO
      LEAVE
    END

    /* =================================================================
    -- Grab the segment length if it's a segment
    */
    lnijLen = 0
    IF Wordpos( lsijMarker, lsijSAMark ) = 0 THEN DO
      lnijLen = C2d( Substr( gaImg.lnijN, lnijI, 2 ) ) - 2
      lnijI = lnijI + 2
    END

    /* =================================================================
    -- All we care about is SOF0 or SOF2
    */
    IF lsijMarker = "C0" | lsijMarker = "C2" THEN DO
      gaImgT.lnijN = "JPEG"
      gaImgH.lnijN = C2d( Substr( gaImg.lnijN, lnijI + 1, 2 ) )
      gaImgW.lnijN = C2d( Substr( gaImg.lnijN, lnijI + 3, 2 ) )
      lnijP = C2d( Substr( gaImg.lnijN, lnijI, 1 ) )
      lsijC = "/DeviceGray"
      IF C2d( Substr( gaImg.lnijN, lnijI + 5, 1 ) ) > 1 THEN DO
        lsijC = "/DeviceRGB"
      END
      gaImg.lnijN = lnijP lsijC gaImg.lnijN
      lbijValid = gbTrue
      LEAVE
    END

    lnijI = lnijI + lnijLen
  END

  DROP lnijI lnijN lnijLen lsijMarker lsijSAMark
RETURN lbijValid

/*
-- =====================================================================
-- Routine:   IsBMP            : Parse and verify BMP file
-- Arguments: lnijN            : Index into gaImg. stem of file
-- Return:    true/false       : true (yes) or false (no)
-- =====================================================================
*/
IsBMP:
  PARSE ARG lnibN

  /* ===================================================================
  -- We only support BMPs
  */
  IF Left( gaImg.lnibN, 2 ) <> '424D'x THEN DO
    DROP lnibN
    RETURN 0
  END

  /* ===================================================================
  -- Get header size..."new" format Windows BMPs only
  */
  lnibHs = C2d( Reverse( Substr( gaImg.lnibN, 15, 4 ) ) )
  IF lnibHs <> 40 THEN DO
    RETURN 0
  END

  /* ===================================================================
  -- Get number of planes...1 only
  */
  IF C2d( Reverse( Substr( gaImg.lnibN, 27, 2 ) ) ) <> 1 THEN DO
    RETURN 0
  END

  /* ===================================================================
  -- Get type of compression...no compression allowed
  */
  IF C2d( Reverse( Substr( gaImg.lnibN, 31, 4 ) ) ) <> 0 THEN DO
    RETURN 0
  END

  /* ===================================================================
  -- Get bits per plane...1, 4, 8, 16, 24, 32 only
  */
  lnibP = C2d( Reverse( Substr( gaImg.lnibN, 29, 2 ) ) )
  IF Wordpos( lnibP, "1 4 8 16 24 32" ) = 0 THEN DO
    RETURN 0
  END

  /* ===================================================================
  -- Get dimensions
  */
  gaImgT.lnibN = "BMP"
  gaImgW.lnibN = C2d( Reverse( Substr( gaImg.lnibN, 19, 4 ) ) )
  gaImgH.lnibN = C2d( Reverse( Substr( gaImg.lnibN, 23, 4 ) ) )

  /* ===================================================================
  -- Set the transformation matrix
  */
  gaImgM.lnibN = "-1 0 0 -1" gaImgW.lnibN gaImgH.lnibN "cm"

  DROP lnibC lnibP lnibN
RETURN gbTrue

/*
-- =====================================================================
-- Routine:   LoadFile         : Returns contents of file
-- Arguments: lslfFile         : File ID
--            lslfVar          : Variable to receive contents
-- Return:    (none)           : none required
-- =====================================================================
*/
LoadFile:
  PARSE ARG lslfFile, lslfVar

  CALL Value lslfVar, gsNull

  SELECT

    /* =================================================================
    -- Handle TSO
    */
    WHEN gsSystem = "TSO" THEN DO
      /* ===============================================================
      -- Open the file
      */
      lslfH = ReadOpen( lslfFile, "binary" )

      /* ===============================================================
      -- Just load the contents
      */
      lslfC = gsNull
      DO UNTIL lalfC.0 = 0
        CALL ReadFile lslfH, "lalfC."
        DO lnlfI = 1 TO lalfC.0
          lslfC = lslfC || lalfC.lnlfI
        END
      END

      /* ===============================================================
      -- Close it
      */
      CALL ReadClose lslfH

      /* ===============================================================
      -- Set variable
      */
      CALL Value lslfVar, lslfC

      DROP lslfH lslfC lalfC. lnlfI
    END

    /* =================================================================
    -- Handle CMS
    */
    WHEN gsSystem = "CMS" THEN DO
      gsPipe "<" Translate( lslfFile, " ", "." ),
             "| JOIN * | VAR" lslfVar
    END

    /* =================================================================
    -- Handle REGINA, UNI-REXX, and OOREXX
    */
    WHEN Wordpos( gsSystem, "REGINA OOREXX UNI-REXX" ) > 0 THEN DO
      lslfH = ReadOpen( lslfFile )

      CALL Value lslfVar, Charin( lslfH, 1, Chars( lslfH ) )

      CALL ReadClose lslfH

      DROP lslfH
    END

    /* =================================================================
    -- Handle OMVS
    */
    WHEN gsSystem = "OMVS" THEN DO
      ADDRESS SYSCALL "open (lslfFile)" O_RDONLY
      IF RC <> 0 | RETVAL = -1 THEN DO
        CALL Issue 115, lslfFile, ERRNO
        SIGNAL Done
      END
      lslfH = RETVAL

      ADDRESS SYSCALL
      "fstat (lslfH) lslfSt."
      IF RC = 0 & RETVAL <> -1 THEN DO
        "read (lslfH)" lslfVar lslfSt.ST_SIZE
      END

      lslfErrno = 0
      IF RC <> 0 | RETVAL = -1 THEN DO
        lslfErrno = ERRNO
      END

      "close (lslsH)"

      ADDRESS

      IF lslfErrno <> 0 THEN DO
        CALL Issue 116, lslfFile, lslfErrno
        SIGNAL Done
      END

      DROP lslfH lslfSt. lslfErrno
    END

   /* =================================================================
    -- Force an error
    */
    OTHERWISE DO
      SAY did_not_account_for_all_cases_in_select
    END
  END

  DROP lslfFile lslfVar
RETURN

/*
-- =====================================================================
-- Font Routines
-- =====================================================================
*/

/*
-- =====================================================================
-- Routine:   PushFont         : Push current font and set new one
-- Arguments: lnpfFont         : Font index
--            lnpfSize         : Font size
-- Return:    operator         : Font operator
-- =====================================================================
*/
PushFont:
  PARSE ARG lnpfFont lnpfSize .

  lnpfI = gaFontStack.0 + 1
  gaFontStack.0 = lnpfI
  gaFontStack.lnpfI = gnFont gnPoints

  gnFont = lnpfFont
  gnPoints = lnpfSize
  gaFontu.gnFont = gbTrue

  DROP lnpfI lnpfFont lnpfSize

RETURN "/F" || gnFont gnPoints "Tf"

/*
-- =====================================================================
-- Routine:   PullFont         : Pull previous font and make it current
-- Arguments: (none)           : none required
-- Return:    operator         : Font operator
-- =====================================================================
*/
PullFont:

  lnpfNdx = gaFontStack.0
  IF lnpfNdx = 0 THEN DO
    CALL Issue 178
    SIGNAL Done
  END

  PARSE VAR gaFontStack.lnpfNdx gnFont gnPoints

  gaFontStack.0 = lnpfNdx - 1

  DROP lnpfNdx

RETURN "/F" || gnFont gnPoints "Tf"

/*
-- =====================================================================
-- Routine:   GetFont          : Get current font and size
-- Arguments: (none)           : none required
-- Return:    operator         : Font operator
-- =====================================================================
*/
GetFont:
RETURN "/F" || gnFont gnPoints "Tf"

/*
-- =====================================================================
-- Routine:   LoadTT           : Process a Truetype font
-- Arguments: lslttFile        : Filename
--            lnlttFont        : Font index
-- Return:    lbeX             : true/false
-- =====================================================================
*/
LoadTT:
  PARSE ARG lslttFile, lnlttFont

  /* ===================================================================
  -- Load the entire font.  (Sorry...piggish for big unicode fonts)
  */
  CALL LoadFile lslttFile, "lslttFont"

  /* ===================================================================
  -- Define interesting table tags
  */
  lslttTagCmap = '636D6170'x
  lslttTagCvt  = '63767420'x
  lslttTagFpgm = '6670676D'x
  lslttTagGlyf = '676C7966'x
  lslttTagHead = '68656164'x
  lslttTagHhea = '68686561'x
  lslttTagHmtx = '686D7478'x
  lslttTagLoca = '6C6F6361'x
  lslttTagMaxp = '6D617870'x
  lslttTagName = '6E616D65'x
  lslttTagPost = '706F7374'x
  lslttTagPrep = '70726570'x
  lslttTagOs2  = '4F532F32'x

  /* ===================================================================
  -- Parse the Offset table:
  --
  --  Offset  Type    Name
  --       0  Fixed   sfnt version
  --       4  USHORT  numTables
  --       6  USHORT  searchRange
  --       8  USHORT  entrySelector
  --      10  USHORT  rangeShift
  --
  -- Length: 12
  */
  PARSE VAR lslttFont  1 lslttSfntVersion,
                      +4 lnlttNumTables,
                      +2 .

  IF lslttSfntVersion <> '00010000'x THEN DO
    CALL Issue 179, lslttFile, C2x( lslttSfntVersion )
    SIGNAL Done
  END

  lnlttNumTables = C2d( lnlttNumTables )

  /* ===================================================================
  -- Parse the Table Directory table:
  --
  --  Offset  Type    Name
  --       0  ULONG   tag
  --       4  ULONG   checkSum
  --       8  ULONG   offset
  --      12  ULONG   length
  --
  -- Length: 16
  */
  lalttInOffset. = 0
  lalttInLength. = 0

  lnlttNdx = 13
  DO lnlttI = 1 TO lnlttNumTables
    PARSE VAR lslttFont =(lnlttNdx) lslttTag,
                        +4          .,
                        +4          lnlttOffset,
                        +4          lnlttLength,
                        +4          .

    lalttInOffset.lslttTag = C2d( lnlttOffset ) + 1
    lalttInLength.lslttTag = C2d( lnlttLength )

    lnlttNdx = lnlttNdx + 16
  END

  /* ===================================================================
  -- Define list of tables that we require
  */
  lslttNeed = lslttTagName,
              lslttTagHead,
              lslttTagHhea,
              lslttTagPost,
              lslttTagGlyf,
              lslttTagCmap,
              lslttTagMaxp,
              lslttTagHmtx,
              lslttTagLoca

  /* ===================================================================
  -- Verify that we have all of the required tables
  */
  DO WHILE lslttNeed <> ""
    PARSE VAR lslttNeed lslttTag lslttNeed
    IF lalttInOffset.lslttTag = 0 THEN DO
      CALL Issue 180, lslttFile, C2x( lslttTag )
      SIGNAL Done
    END
  END

  /* ===================================================================
  -- Initialize
  */
  lblttIsSymbolic = gbFalse
  lblttIsFixedPitch = gbFalse
  lblttIsBold = gbFalse
  lblttIsItalic = gbFalse
  lblttIsAllCap = gbFalse
  lblttIsSmallCap = gbFalse

  /* ===================================================================
  -- Go process each table (don't change the order!)
  */
  CALL TT_ProcessHead
  CALL TT_ProcessHhea
  CALL TT_ProcessName

  IF gaFontf.lnlttFont = gsNull THEN DO
    CALL TT_ProcessCmap
    CALL TT_Subset
  END

  CALL TT_ProcessCmap
  CALL TT_ProcessHmtx
  CALL TT_ProcessPost

  IF lalttInOffset.lslttTagOs2 <> 0 THEN DO
    CALL TT_ProcessOs2
  END

  /* ===================================================================
  -- Calculate the Font Descriptor flags
  */
  lnlttFlags = 0

  IF lblttIsFixedPitch THEN DO
    lnlttFlags = lnlttFlags + ( 2 ** 0 )
  END

  IF lblttIsSymbolic THEN DO
    lnlttFlags = lnlttFlags + ( 2 ** 2 )
  END
  ELSE DO
    lnlttFlags = lnlttFlags + ( 2 ** 5 )
  END

  IF lnlttItalicAngle < 0 | lblttIsItalic THEN DO
    lnlttFlags = lnlttFlags + ( 2 ** 6 )
  END

  IF lblttIsAllCap THEN DO
    lnlttFlags = lnlttFlags + ( 2 ** 16 )
  END

  IF lblttIsSmallCap THEN DO
    lnlttFlags = lnlttFlags + ( 2 ** 17 )
  END

  gaFont_Flags.lnlttFont = lnlttFlags

  /* ===================================================================
  -- FIXME: These need to be calculated
  */
  gaFont_FirstChar.lnlttFont = 32
  gaFont_LastChar.lnlttFont = 255

  /* ===================================================================
  -- Start a new fontfile object
  */
  IF gaFontf.lnlttFont <> "NOEMBED" THEN DO

    gaFontq.lnI = NewObj()

    CALL QueueCntl "<<"

    CALL QueueCntl "/Length1" Length( lslttFont )

    /* =================================================================
    -- Start the stream
    */
    CALL StartStream

    /* =================================================================
    -- Write the font
    */
    CALL Queue lslttFont

    /* =================================================================
    -- End the stream
    */
    CALL EndStream
  END

  /* ===================================================================
  -- Output the font descriptor
  */
  gaFont_Descriptor.lnI = NewObj()
  CALL QueueCntl "<<"
  CALL QueueCntl "/Type /FontDescriptor"
  CALL QueueCntl "/FontName /" || gaFont_FontName.lnI
  IF gaFontf.lnI <> "NOEMBED" THEN DO
    CALL QueueCntl "/FontFile2" gaFontq.lnI "0 R"
  END
  CALL QueueCntl "/Flags" gaFont_Flags.lnI
  CALL QueueCntl "/FontBBox [" gaFont_BBox.lnI "]"
  CALL QueueCntl "/MissingWidth" gaFont_MissingWidth.lnI
  CALL QueueCntl "/StemV" gaFont_StemV.lnI
  CALL QueueCntl "/ItalicAngle" gaFont_ItalicAngle.lnI
  CALL QueueCntl "/CapHeight" gaFont_CapHeight.lnI
  CALL QueueCntl "/Ascent" gaFont_Ascent.lnI
  CALL QueueCntl "/Descent" gaFont_Descent.lnI
  CALL QueueCntl ">>"
  CALL EndObj

  /* ===================================================================
  -- A bit ridiculous, but oh well...
  */
  DROP lslttFile lnlttFont lslttFont lslttSfntVersion lnlttNumTables,
       lalttInOffset. lalttInLength. lnlttNdx lslttTag lnlttChecksum,
       lnlttOffset lnlttLength lslttTagCmap lslttTagCvt lslttTagFpgm,
       lslttTagGlyf lslttTagHead lslttTagHhea lslttTagHmtx lslttTagLoca,
       lslttTagMaxp lslttTagName lslttTagOs2 lslttTagPost lslttNeed,
       lslttCopy lslttTag lblttIsSymbolic lblttIsFixedPitch lblttIsBold,
       lblttIsItalic lblttIsAllCap lblttIsSmallCap lnlttFlags

  /* ===================================================================
  -- Semi global values defined in subordinate routines
  */
  DROP lnlttScale lnlttItalicAngle lnlttNumberOfHMetrics,

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessMaxp   : Process the "maxp" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessMaxp:

  /* ===================================================================
  -- Parse the Maximum Profile table:
  --
  --  Offset  Type          Name
  --       0  Fixed         Table version number
  --       4  USHORT        numGlyphs
  --
  --  Length: 6
  */

  lnlttOffset = lalttInOffset.lslttTagMaxp
  PARSE VAR lslttFont =(lnlttOffset) .,
                      +4             lnlttNumGlyphs,
                      +2             .

  lnlttNumGlyphs = C2d( lnlttNumGlyphs )

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessHead   : Process the "head" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessHead:

  /* ===================================================================
  -- Parse the Font Header table:
  --
  --  Offset  Type          Name
  --       0  USHORT        format
  --       2  Fixed         Table version number
  --       6  Fixed         fontRevision
  --      10  ULONG         checkSumAdjustment
  --      14  ULONG         magicNumber
  --      18  USHORT        flags
  --      20  USHORT        unitsPerEm
  --      22  LONGDATETIME  created
  --      30  LONGDATETIME  modified
  --      38  SHORT         xMin
  --      40  SHORT         yMin
  --      42  SHORT         xMax
  --      44  SHORT         yMax
  --      46  USHORT        macStyle
  --      48  USHORT        lowestRecPPEM
  --      50  SHORT         fontDirectionHint
  --      52  SHORT         indexToLocFormat
  --      54  SHORT         glyphDataFormat
  --
  --  Length: 56
  */

  lnlttOffset = lalttInOffset.lslttTagHead
  PARSE VAR lslttFont =(lnlttOffset) .,
                      +18            lnlttUnitsPerEm,
                      +2             .,
                      +16            lnlttXmin,
                      +2             lnlttYmin,
                      +2             lnlttXmax,
                      +2             lnlttYmax,
                      +2             lslttMacStyle,
                      +2             .,
                      +4             lnlttLocaFmt,
                      +2             .

  /* ===================================================================
  -- Calc size index to location entry sizes
  */
  lnlttLocaSize = ( C2d( lnlttLocaFmt ) + 1 ) * 2
  lnlttLocaMult = 2 / ( lnlttLocaSize % 2 )

  /* ===================================================================
  -- Need to set the scale factor VERY early on
  */
  lnlttUnitsPerEm = C2d( lnlttUnitsPerEm )
  lnlttScale = 1000 / lnlttUnitsPerEm

  /* ===================================================================
  -- Get the fonts' bounding box.
  */
  gaFont_BBox.lnlttFont = TT_Scale( C2d( lnlttXmin, 2 ) ),
                          TT_Scale( C2d( lnlttYmin, 2 ) ),
                          TT_Scale( C2d( lnlttXmax, 2 ) ),
                          TT_Scale( C2d( lnlttYmax, 2 ) )

  /* ===================================================================
  -- Check styles
  */
  lblttIsBold = Bitand( lslttMacStyle, '01'x ) <> '00'x
  lblttIsItalic = Bitand( lslttMacStyle, '02'x ) <> '00'x

  /* ===================================================================
  -- FIXME:  Need to work out the right way to calculate this
  */
  gaFont_StemV.lnlttFont = 0

  DROP lnlttLocaFmt lnlttUnitsPerEm lnlttXmin lnlttTmin lnlttXmax,
       lnlttYmax lsttMacStyle

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessPost   : Process the "post" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessPost:

  /* ===================================================================
  -- Parse the Postscript table:
  --
  --  Offset  Type          Name
  --       0  Fixed         Table version number
  --       4  Fixed         italicAngle
  --       8  FWORD         underlinePosition
  --      10  FWORD         underlineThickness
  --      12  ULONG         isFixedPitch
  --
  --  Length: 16
  */

  lnppOffset = lalttInOffset.lslttTagPost
  PARSE VAR lslttFont =(lnppOffset) .,
                      +4            lnlttItalicAngle,
                      +4            .,
                      +4            lblttIsFixedPitch,
                      +4            .

  lnlttItalicAngle = C2d( Left( lnlttItalicAngle, 2 ), 2 ) +,
                     ( C2d( Right( lnlttItalicAngle, 2 ) ) / 65536 )
  lblttIsFixedPitch = C2d( lblttIsfixedPitch ) <> 0

  gaFont_ItalicAngle.lnlttFont = lnlttItalicAngle

  DROP lnppOffset

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessHhea   : Process the "hhea" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessHhea:

  /* ===================================================================
  -- Parse the Horizontal Header table:
  --
  --  Offset  Type          Name
  --       0  Fixed         Table version number
  --       4  FWORD         Ascender
  --       6  FWORD         Descender
  --       8  FWORD         LineGap
  --      10  UFWORD        advanceWidthMax
  --      12  FWORD         minLeftSideBearing
  --      14  FWORD         minRightSideBearing
  --      16  FWORD         xMaxExtent
  --      18  SHORT         caretSlopeRise
  --      20  SHORT         caretSlopeRun
  --      22  SHORT         caretOffset
  --      24  SHORT         (reserved)
  --      26  SHORT         (reserved)
  --      28  SHORT         (reserved)
  --      30  SHORT         (reserved)
  --      32  SHORT         metricDataFormat
  --      34  USHORT        numberOfHMetrics
  --
  --  Length: 36
  */

  lnphOffset = lalttInOffset.lslttTagHhea
  PARSE VAR lslttFont =(lnphOffset) .,
                      +4             lnphAscender,
                      +2             lnphDescender,
                      +2             .,
                      +26            lnlttNumberOfHMetrics,
                      +2             .

  lnlttNumberOfHMetrics = C2d( lnlttNumberOfHMetrics )

  gaFont_Ascent.lnlttFont = TT_Scale( C2d( lnphAscender, 2 ) )
  gaFont_Descent.lnlttFont = TT_Scale( C2d( lnphDescender, 2 ) )

  /* ===================================================================
  -- This guesstimate will get used if there's no OS/2 table.
  */
  gaFont_CapHeight.lnlttFont = ,
    Format( gaFont_Ascent.lnlttFont * .93, , 0 )

  DROP lnphOffset lnphAscender lnphDescender

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessHmtx   : Process the "hmtx" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessHmtx:

  /* ===================================================================
  -- Extract table to improve speed...at the cost of extra memory usage
  */
  PARSE VAR lslttFont =(lalttInOffset.lslttTagHmtx) lsphTable,
                      +(lalttInLength.lslttTagHmtx) .

  /* ===================================================================
  -- The last entry applies to any glyphs beyond the end.  Monospaced
  -- fonts only need 1 entry specifying the width of all glyphs.
  */
  lnphOffset = ( ( lnlttNumberOfHMetrics - 1 ) * 4 ) + 1

  /* ===================================================================
  -- Parse the Horizontal Metrics table:
  --
  --  Offset  Type          Name
  --       0  USHORT        advanceWidth
  --       2  SHORT         lsb
  --
  --  Length: 4
  */
  PARSE VAR lsphTable =(lnphOffset) lnphAdvanceWidth,
                      +2            .

  gaFont_MissingWidth.lnlttFont = TT_Scale( C2d( lnphAdvanceWidth ) )

  /* ===================================================================
  -- Extract widths
  */
  DO lnphI = 0 TO 255
    /* =================================================================
    -- Get the glyph index
    */
    lnphNdx = lalttGndx.lnphI

    /* =================================================================
    -- The last entry applies to any glyph indexes beyond the number of
    -- metrics.  Monospaced fonts only need 1 entry as all glyphs have
    -- the same width
    */
    IF lnphNdx <> -1 & lnphNdx < lnlttNumberOfHMetrics THEN DO
      lnphOffset = ( lnphNdx * 4 ) + 1
      PARSE VAR lsphTable =(lnphOffset) lnphAdvanceWidth,
                          +2            lnphLsb,
                          +2            .
    END
    ELSE DO
      IF lnphNdx = -1 THEN DO
        lnphNdx = 0
      END

      lnphOffset = ( ( lnlttNumberOfHMetrics - 1 ) * 4 ) + 1
      PARSE VAR lsphTable =(lnphOffset) lnphAdvanceWidth,
                          +2            lnphLsb,
                          +2            .

      lnphOffset = lnphOffset + 4 + ,
                   ( ( lnphNdx - lnlttNumberOfHMetrics ) * 2 )

      IF lnphOffset < Length( lsphTable ) THEN DO
        PARSE VAR lsphTable =(lnphOffset) lnphLsb,
                            +2            .
      END
    END

    gaFont_Glyf_Width.lnlttFont.lnphI = ,
      TT_Scale( C2d( lnphAdvanceWidth ) )
  END

  DROP lnphOffset lnphAdvanceWidth lnphI lnphNdx lsphTable

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessCmap   : Process the "cmap" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessCmap:

  /* ===================================================================
  -- Extract table to improve speed...at the cost of extra memory usage
  */
  PARSE VAR lslttFont =(lalttInOffset.lslttTagCmap) lspcTable,
                      +(lalttInLength.lslttTagCmap) .

  /* ===================================================================
  -- Parse the Character Mapping table:
  --
  --  Offset  Type        Name
  --       0  USHORT      version
  --       2  USHORT      numTables
  --
  --  Length: 4
  */

  lnpcOffset = 1
  PARSE VAR lspcTable =(lnpcOffset) .,
                      +2            lnpcNumTables,
                      +2            .

  lnpcNumTables = C2d( lnpcNumTables )

  /* ===================================================================
  -- Initialize offsets
  */
  lnpcMSOffset = 0
  lnpcFmt0Offset = 0
  lnpcFmt4Offset = 0

  lnpcOffset = lnpcOffset + 4
  DO lnpcI = 1 TO lnpcNumTables

    /* =================================================================
    -- Parse the Encoding Record table:
    --
    --  Offset  Type        Name
    --       0  USHORT      platformID
    --       2  USHORT      encodingID
    --       4  ULONG       offset
    --
    --  Length: 8
    */
    PARSE VAR lspcTable =(lnpcOffset) lnpcPlatformID,
                        +2            lnpcEncodingID,
                        +2            lnpcSubOffset,
                        +4

    lnpcPlatformID = C2d( lnpcPlatformID )
    lnpcEncodingID = C2d( lnpcEncodingID )
    lnpcSubOffset = C2d( lnpcSubOffset ) + 1

    /* =================================================================
    -- Get the table format
    */
    PARSE VAR lspcTable =(lnpcSubOffset) lnpcFormat +2

    lnpcFormat = C2d( lnpcFormat )

    /* =================================================================
    -- Find offsets of the first format 4 and format 0 tables.
    */
    IF lnpcFormat = 4 THEN DO
      /* ===============================================================
      -- Handle Microsoft fmt4 table differently since we need to check
      -- to see if it may contain symbol references.
      */
      IF lnpcPlatformID = 3 THEN DO
        IF lnpcMSOffset = 0 THEN DO
          lnpcMSOffset = lnpcSubOffset
        END

        /* =============================================================
        -- Treat all non-Unicode types as symbolic
        */
        IF lnpcEncodingID <> 1 THEN DO
          IF lbpcIsSymbolic = gbFalse THEN DO
            lbpcIsSymbolic = gbTrue
          END
        END
      END
      ELSE IF lnpcFmt4Offset = 0 THEN DO
        lnpcFmt4Offset = lnpcSubOffset
      END
    END
    ELSE IF lnpcFormat = 0 THEN DO
      IF lnpcFmt0Offset = 0 THEN DO
        lnpcFmt0Offset = lnpcSubOffset
      END
    END

    /* =================================================================
    -- Bump to next record
    */
    lnpcOffset = lnpcOffset + 8
  END

  /* ===================================================================
  -- Go process the different formats
  */
  DROP lalttGndx.
  lalttGndx. = 0
  SELECT
    WHEN lnpcMSOffset <> 0 | lnpcFmt4Offset <> 0 THEN DO
      IF lnpcMSOffset <> 0 THEN DO
        CALL TT_ProcessFmt4 lnpcMSOffset
      END
      ELSE DO
        CALL TT_ProcessFmt4 lnpcFmt4Offset
      END
    END

    WHEN lnpcFmt0Offset <> 0 THEN DO
      CALL TT_ProcessFmt0 lnpcFmt0Offset
    END

    OTHERWISE DO
      CALL Issue 181, lslttFile
      SIGNAL Done
    END
  END

  DROP lnpcNumTables lnpcMSOffset lnpcFmt0Offset lnpcFmt4Offset,
       lnpcI lnpcPlatformID lnpcEncodingID lnpcSubOffset,
       lnpcFormat lspcTable

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessFmt0   : Process a format 0 encoding table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessFmt0:
  PARSE ARG lnpf0Offset

  /* ===================================================================
  -- Parse the format 0 Byte Encoding table:
  --
  --  Offset  Type        Name
  --       0  USHORT      format
  --       2  USHORT      length
  --       4  USHORT      language
  --       6  BYTE        glyphIdArray[256]
  --
  --  Length: 262
  */
  PARSE VAR lspcTable =(lnpf0Offset) .,
                      +2             lnpf0Length,
                      +2             .

  lnpf0Count = C2d( lnpf0Length ) - 6

  lnpf0Offset = lnpf0Offset + 6

  DO lnpf0I = 0 to 255
    PARSE VAR lspcTable =(lnpf0Offset) lnpf0Ndx +1

    lalttGndx.lnpf0I = C2d( lnpf0Ndx )
    lnpf0Offset = lnpf0Offset + 1
  END

  DROP lnpf0Offset lnpf0Count lnpf0I lnpf0Ndx

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessFmt4   : Process a format 4 encoding table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessFmt4:
  PARSE ARG lnpf4Offset

  /* ===================================================================
  -- Parse the Maximum Profile table:
  --
  --  Offset  Type          Name
  --       0  Fixed         Table version number
  --       4  USHORT        numGlyphs
  --
  --  Length: 6
  */

  PARSE VAR lslttFont =(lalttInOffset.lslttTagMaxp) .,
                      +4                            lnpf4NumGlyphs,
                      +2                            .

  lnpf4NumGlyphs = C2d( lnpf4NumGlyphs )

  /* ===================================================================
  -- Parse the format 4 Segment Mapping to Delta Values table:
  --
  --  Offset  Type        Name
  --       0  USHORT      format
  --       2  USHORT      length
  --       4  USHORT      language
  --       6  USHORT      segCountX2
  --       8  USHORT      searchRange
  --      10  USHORT      entrySelector
  --      12  USHORT      rangeShift
  --      14  USHORT      endCount[segCount]
  --       ?  USHORT      reservedPad
  --       ?  USHORT      startCount[segCount]
  --       ?  SHORT       idDelta[segCount]
  --       ?  USHORT      idRangeOffset[segCount]
  --       ?  USHORT      glyphIdArray[ ]
  --
  --  Length: variable
  */
  PARSE VAR lspcTable =(lnpf4Offset) .,
                      +6             lnpf4SegCountX2,
                      +2             lnpf4SearchRange,
                      +2             lnpf4EntrySelector,
                      +2             lnpf4RangeShift,
                      +2             .

  lnpf4SegCountX2 = C2d( lnpf4SegCountX2 )
  lnpf4SearchRange = C2d( lnpf4SearchRange )
  lnpf4EntrySelector = C2d( lnpf4EntrySelector )

  /* ===================================================================
  -- Calculate offsets to the arrays
  */
  lnpf4EndCount = lnpf4Offset + 14
  lnpf4StartCount = lnpf4EndCount + lnpf4SegCountX2 + 2
  lnpf4IdDelta = lnpf4StartCount + lnpf4SegCountX2
  lnpf4RangeOffset = lnpf4IdDelta + lnpf4SegCountX2
  lnpf4GlyphIdArray = lnpf4RangeOffset + lnpf4SegCountX2

  /* ===================================================================
  -- Look up glyph indexes for each character code
  */
  DO lnpf4I = 0 TO 255

    /* ===================================================================
    -- Get the code to look up
    */
    lnpf4C = C2d( gaEncoding.lnpf4I )

    /* ===================================================================
    -- Scan the segments for one containing the code
    */
    DO lnpf4J = 0 TO lnpf4SegCountX2 - 2 BY 2

      /* =================================================================
      -- Get the starting and ending codes within this segment
      */
      lnpf4Offset1 = lnpf4EndCount + lnpf4J
      lnpf4Offset2 = lnpf4StartCount + lnpf4J
      PARSE VAR lspcTable =(lnpf4Offset1) lnpf4End +2,
                          =(lnpf4Offset2) lnpf4Start +2

      /* ===============================================================
      -- Go to next segment if the code is past the upper bound
      */
      lnpf4End = C2d( lnpf4End )
      IF lnpf4End < lnpf4C THEN DO
        ITERATE
      END

      /* ===============================================================
      -- A code before the lower bound of this segment represents a
      -- missing glyph
      */
      lnpf4Start = C2d( lnpf4Start )
      IF lnpf4Start > lnpf4C THEN DO
        /* ===============================================================
        -- Hit end of mapping?
        */
        IF lnpf4Start = 65535 THEN DO
          /* ===============================================================
          -- Mark as not defined for ProcessHmtx
          */
          lalttGndx.lnpf4I = -1
        END
        ELSE DO
          /* ===============================================================
          -- Map to glyph 0
          */
          lalttGndx.lnpf4I = 0
        END
        LEAVE
      END

      /* ===============================================================
      -- Get the delta and range offset for this code
      */
      lnpf4Offset1 = lnpf4IdDelta + lnpf4J
      lnpf4Offset2 = lnpf4RangeOffset + lnpf4J
      PARSE VAR lspcTable =(lnpf4Offset1) lnpf4Delta +2,
                          =(lnpf4Offset2) lnpf4Range +2

      lnpf4Delta = C2d( lnpf4Delta, 2 )
      lnpf4Range = C2d( lnpf4Range )

      /* ===============================================================
      -- Calculate the glyph index
      */
      IF lnpf4Range = 0 THEN DO
        lnpf4Ndx = lnpf4C + lnpf4Delta
      END
      ELSE DO
        lnpf4Offset = lnpf4GlyphIdArray + ,
                      lnpf4J - lnpf4SegCountX2 + lnpf4Range + ,
                      ( ( lnpf4C - lnpf4Start ) * 2 )

        PARSE VAR lspcTable =(lnpf4Offset) lnpf4Ndx +2

        lnpf4Ndx = C2d( lnpf4Ndx )

        /* =============================================================
        -- An index of 0 indicates a missing glyph
        */
        IF lnpf4Ndx = 0 THEN DO
          LEAVE
        END

        lnpf4Ndx = lnpf4Ndx + lnpf4Delta
      END

      /* ===============================================================
      -- The index is out of bounds (or there's a bug) so treat it as a
      -- missing glyph
      */
      IF lnpf4Ndx < 0 | lnpf4Ndx >= lnpf4NumGlyphs THEN DO
        LEAVE
      END

      /* ===============================================================
      -- Found the index, so record and get out to process next code
      */
      lalttGndx.lnpf4I = lnpf4Ndx

      LEAVE
    END
  END

  DROP lnpf4Offset lnpf4SegCountX2 lnpf4SearchRange lnpf4EntrySelector,
       lnpf4EndCount lnpf4StartCount lnpf4IdDelta lnpf4RangeOffset,
       lnpf4GlyphIdArray lnpf4I lnpf4C lnpf4J lnpf4End lnpf4Start,
       lnpf4Delta lnpf4Range lnpf4Offset lnpf4Ndx lnpf4Offset1,
       lnpf4Offset2 lnpf4NumGlyphs

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessName   : Process the "name" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessName:

  /* ===================================================================
  -- Extract table to improve speed...at the cost of extra memory usage
  */
  PARSE VAR lslttFont =(lalttInOffset.lslttTagName) lspnTable,
                      +(lalttInLength.lslttTagName) .

  /* ===================================================================
  -- Parse the Naming table:
  --
  --  Offset  Type        Name
  --       0  USHORT      format
  --       2  USHORT      count
  --       4  USHORT      stringOffset
  --
  --  Length: 6
  */

  lnpnOffset = 1
  PARSE VAR lspnTable =(lnpnOffset) .,
                      +2            lnpnCount,
                      +2            lnpnStorageOffset,
                      +2            .

  lnpnCount = C2d( lnpnCount )
  lnpnStorageOffset = C2d( lnpnStorageOffset ) + lnpnOffset

  /* ===================================================================
  -- Record all names
  */
  lbpnFound = 0
  lapnNames. = ""
  lnpnOffset = lnpnOffset + 6
  DO lnpnI = 1 TO lnpnCount

    /* =================================================================
    -- Parse a Naming Record table:
    --
    --  Offset  Type        Name
    --       0  USHORT      platformID
    --       2  USHORT      encodingID
    --       4  USHORT      languageID
    --       6  USHORT      nameID
    --       8  USHORT      length
    --      10  USHORT      offset
    --
    --  Length: 12
    */
    PARSE VAR lspnTable =(lnpnOffset) lnpnPlatformID,
                        +2             lnpnEncodingID,
                        +2             lnpnLanguageID,
                        +2             lnpnNameID,
                        +2             lnpnLength,
                        +2             lnpnStringOffset,
                        +2             .

    lnpnPlatformID = C2d( lnpnPlatformID )
    lnpnEncodingID = C2d( lnpnEncodingID )
    lnpnLanguageID = C2d( lnpnLanguageID ) // 256
    lnpnNameID = C2d( lnpnNameID )
    lnpnLength = C2d( lnpnLength )
    lnpnStringOffset = C2d( lnpnStringOffset ) + ,
                        lnpnStorageOffset

    /* =================================================================
    -- Extract the name
    */
    IF ( lapnNames.lnpnNameID = "" & lnpnPlatformID = 1 ) |,
      lnpnPlatformID = 3 THEN DO
      IF lnpnLanguageID = 0 | lnpnLanguageID = 9 THEN DO

        lspnName = gsNull

        /* ===========================================================
        -- Get each character and convert it to platform encoding
        -- NOTE:  This may not be necessary
        */
        lnpnLength = lnpnLength - 1
        DO lnpnJ = 0 TO lnpnLength
          PARSE VAR lspnTable =(lnpnStringOffset) .,
                              +(lnpnJ)            lspnChar ,
                              +1                  .

          IF lspnChar < '20'x | lspnChar > '7E'x THEN DO
            ITERATE
          END

          lnpnP = Pos( lspnChar, gsCPCntl )
          IF lnpnP > 0 THEN DO
            lspnName = lspnName || D2c( lnpnP - 1 )
          END
        END

        lapnNames.lnpnNameID = lspnName
      END
    END

    /* =================================================================
    -- Bump to next record
    */
    lnpnOffset = lnpnOffset + 12
  END

  /* ===================================================================
  -- Use the font family name if we don't have a full font name
  */
  IF lapnNames.4 = "" THEN DO
    lapnNames.4 = lapnNames.1
  END

  /* ===================================================================
  -- Use the full font name if we don't have a postscript name or set to
  -- "Unknown" if we don't have either one.
  */
  IF lapnNames.6 = "" THEN DO
    lapnNames.6 = lapnNames.4
    IF lapnNames.6 = "" THEN DO
      lapnNames.6 = "Unknown"
    END
  END

  /* ===================================================================
  -- First character can't be a number
  */
  lspnChar = Left( lapnNames.6, 1 )
  IF Datatype( lspnChar, "W" ) = gbTrue THEN DO
    lapnNames.6 = Overlay( Substr( "ABCDEFGHIJ", lspnChar + 1, 1 ),,
                            lapnNames.6 )
  END

  /* ===================================================================
  -- Replace invalid characters
  */
  DO lnpnI = 1 TO Length( lapnNames.6 )
    PARSE VAR lapnNames.6 =(lnpnI) lspnChar +1 .
    IF Datatype( lspnChar, "A" ) = gbFalse | lspnChar = "_" THEN DO
      lapnNames.6 = Overlay( "-", lapnNames.6, lnpnI )
    END
  END

  /* ===================================================================
  -- Generate a unique subset prefix if embedding
  */
  lspnName = gsNull
  IF gaFontf.lnlttFont <> "NOEMBED" THEN DO
    lnpnI = ( lnlttFont % 26 ) + 1
    lnpnJ = ( lnlttFont // 26 ) + 1
    lspnChar = Substr( "ABCDEFGHIJKLMNOPQRSTUVWXYZ", lnpnJ, 1 )
    lspnName = Overlay( lspnChar, "AAAAAA", lnpnI ) || "+"
  END

  /* ===================================================================
  -- Set names
  */
  gaFont_FontName.lnlttFont = lspnName || lapnNames.6
  gaFont_BaseFont.lnlttFont = lspnName || lapnNames.6

  /* ===================================================================
  -- Check the font subfamily for styles
  */
  lspnName = Translate( lapnNames.2 )
  IF lblttIsBold <> gbFalse THEN DO
    lblttIsBold = Wordpos( "BOLD", lspnName ) <> 0
  END

  IF lblttIsItalic <> gbFalse THEN DO
    lblttIsItalic = Wordpos( "OBLIQUE", lspnName ) <> 0
  END

  /* ===================================================================
  -- Try to glean styles from (our version of) the postscript name
  */
  lspnName = Translate( lapnNames.6 )
  SELECT
    WHEN Pos( "CAP", lspnName ) & Pos( "SMALL", lspnName ) > 0 THEN DO
      lblttIsSmallCap = gbTrue
    ENd

    WHEN Right( lspnName, 2 ) = "SC" THEN DO
      lblttIsSmallCap = gbTrue
    END

    WHEN Pos( "CAP", lspnName ) & Pos( "ALL", lspnName ) > 0 THEN DO
      lblttIsAllCap = gbTrue
    END

    OTHERWISE DO
      NOP
    END
  END

  DROP lnpnCount lnpnStorageOffset lbpnFound lapnNames. lnpnI,
       lnpnPlatformID lnpnEncodingID lnpnLanguageID lnpnNameID,
       lnpnLength lnpnStringOffset lspnName lnpnJ lspnChar lnpnP,
       lspnChar lspnOffset lspnTable

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessLoca   : Process the "loca" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
--
-- Not used ... merged into TT_Subset ... keep just in case
-- =====================================================================
*/
TT_ProcessLoca:

  /* ===================================================================
  -- Extract table to improve speed...at the cost of extra memory usage
  */
  PARSE VAR lslttFont =(lalttInOffset.lslttTagLoca) lsplTable,
                      +(lalttInLength.lslttTagLoca) .

  /* ===================================================================
  -- Parse the Index to Location table:
  --
  --  Offset  Type          Name
  --       0  USHORT        offsets[n]
  --  or
  --       0  ULONG         offsets[n]
  --
  --  Length: variable
  */

  NUMERIC DIGITS 10

  lnplCnt = ( lalttInLength.lslttTagLoca / lnlttLocaSize ) - 1
  lnplOffset = 1
  DO lnplI = 0 TO lnplCnt - 1
    PARSE VAR lsplTable =(lnplOffset)     lnplGoff,
                        +(lnlttLocaSize)  lnplNoff,
                        +(lnlttLocaSize)  .

    gaFont_Glyf_Loca.lnlttFont.lnplI = lalttInOffset.lslttTagGlyf + ,
      ( C2d( lnplGoff ) * lnlttLocaMult )

    gaFont_Glyf_Size.lnlttFont.lnplI = ( lalttInOffset.lslttTagGlyf + ,
      ( C2d( lnplNoff ) * lnlttLocaMult ) ) - ,
      gaFont_Glyf_Loca.lnlttFont.lnplI

    lnplOffset = lnplOffset + lnlttLocaSize
  END

  DROP lnplCnt lnplOffset lnplI lnplGoff lnplNoff lsplTable

RETURN

/*
-- =====================================================================
-- Routine:   TT_ProcessOs2    : Process the "OS/2" table
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_ProcessOs2:

  /* ===================================================================
  -- Parse the OS/2 and Windows Metrics table
  --
  --  == all versions ==
  --  Offset  Type        Name
  --       0  USHORT      version
  --       2  SHORT       xAvgCharWidth
  --       4  USHORT      usWeightClass
  --       6  USHORT      usWidthClass
  --       8  USHORT      fsType
  --      10  SHORT       ySubscriptXSize
  --      12  SHORT       ySubscriptYSize
  --      14  SHORT       ySubscriptXOffset
  --      16  SHORT       ySubscriptYOffset
  --      18  SHORT       ySuperscriptXSize
  --      20  SHORT       ySuperscriptYSize
  --      22  SHORT       ySuperscriptXOffset
  --      24  SHORT       ySuperscriptYOffset
  --      26  SHORT       yStrikeoutSize
  --      28  SHORT       yStrikeoutPosition
  --      30  SHORT       sFamilyClass
  --      32  BYTE        panose[10]
  --      42  ULONG       ulUnicodeRange1
  --      46  ULONG       ulUnicodeRange2
  --      50  ULONG       ulUnicodeRange3
  --      54  ULONG       ulUnicodeRange4
  --      58  CHAR        achVendID[4]
  --      62  USHORT      fsSelection
  --      64  USHORT      usFirstCharIndex
  --      66  USHORT      usLastCharIndex
  --      68  SHORT       sTypoAscender
  --      70  SHORT       sTypoDescender
  --      72  SHORT       sTypoLineGap
  --      74  USHORT      usWinAscent
  --      76  USHORT      usWinDescent
  --  == version 1 added ==
  --      78  ULONG       ulCodePageRange1
  --      82  ULONG       ulCodePageRange2
  --  == version 2 added ==
  --      86  SHORT       sxHeight
  --      88  SHORT       sCapHeight
  --      90  USHORT      usDefaultChar
  --      92  USHORT      usBreakChar
  --      94  USHORT      usMaxContext
  --
  --  Length: variable
  */

  lnpoOffset = lalttInOffset.lslttTagOs2

  PARSE VAR lslttFont =(lnpoOffset) lnpoVersion,
                      +2            .,
                      +66           lnpoTypoAscender,
                      +2            lnpoTypoDescender,
                      +2            .
/*
  gaFont_Ascent.lnlttFont = TT_Scale( C2d( lnpoTypoAscender, 2 ) )
  gaFont_Descent.lnlttFont = TT_Scale( C2d( lnpoTypoDescender, 2 ) )
*/

  IF C2d( lnpoVersion ) > 1 THEN DO
    PARSE VAR lslttFont =(lnpoOffset) .,
                        +88           lnpoCapHeight,
                        +2            .

    gaFont_CapHeight.lnlttFont = TT_Scale( C2d( lnpoCapHeight, 2 ) )

    DROP lnpoCapHeight
  END

  DROP lnpoOffset lnpoVersion lnpoTypoAscender lnpoTypoDescender

RETURN

/*
-- =====================================================================
-- Routine:   TT_Scale          : Scale value by font units
-- Arguments: (none)            : none required
-- Return:    (none)            : none required
-- =====================================================================
*/
TT_Scale:
RETURN Format( Arg( 1 ) * lnlttScale, , 0 )

/*
-- =====================================================================
-- Routine:   TT_Subset        : Create a subset of original font
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_Subset:

  /* ===================================================================
  -- Define list of tables that we simply copy (for subsetting)
  */
  lsssCopy = lslttTagCvt,
             lslttTagFpgm,
             lslttTagPrep,
            lslttTagName,
             lslttTagOs2

  /* ===================================================================
  -- Define list of tables that we require
  */
  lsssReq = lsssCopy,
            lslttTagMaxp,
            lslttTagCmap,
            lslttTagLoca,
            lslttTagGlyf,
            lslttTagHhea,
            lslttTagHmtx,
            lslttTagPost,
            lslttTagHead

  lassSubOff. = gsNull
  lassSubLen. = 0

  /* ===================================================================
  -- Calc log2(number of subtables)
  */
  lnssCnt = Words( lsssReq )
  DO lnssI = 1 BY 1 WHILE 2 ** lnssI < lnssCnt
    lnssLog2 = lnssI
  END

  /* ===================================================================
  -- Build offset header and reserve space for font directory
  */
  lsssFont = '00010000'x ||,
             D2c( lnssCnt, 2 ) ||,
             D2c( ( 2 ** lnssLog2 ) * 16, 2 ) ||,
             D2c( lnssLog2, 2 ) ||,
             D2c( ( lnssCnt * 16 ) - ( ( 2 ** lnssLog2 ) * 16 ), 2 ) ||,
             Copies( '00'x, Words( lsssReq ) * 16 )
  lnssStart = Length( lsssFont )

  /* ===================================================================
  -- Copy over tables that do not need to be modified
  */
  DO WHILE lsssCopy <> ""
    PARSE VAR lsssCopy lsssTag +4 +1 lsssCopy

    IF lalttInOffset.lsssTag <> 0 THEN DO
      PARSE VAR lslttFont =(lalttInOffset.lsssTag) lsssTable,
                          +(lalttInLength.lsssTag) .

      CALL TT_AddTable lsssTag, lsssTable
    END
  END

  /* ===================================================================
  -- Build the "head" table                                           --
  =================================================================== */

  PARSE VAR lslttFont =(lalttinOffset.lslttTagHead) lsssTable,
                      +(lalttInLength.lslttTagHead) .

  /* ===================================================================
  -- Clear checksum adjustment
  */
  lsssTable = Overlay( D2c( 0, 4 ), lsssTable, 9 )

  CALL TT_AddTable lslttTagHead, lsssTable

  /* ===================================================================
  -- Determine used unicode characters and glyph indexes
  */
  lassUC. = gbFalse
  lassCCs. = gsNull
  lnssCCs = 0
  lnssOffset = 1
  DO FOREVER
    /* =================================================================
    -- Find the next seen character
    */
    lnssOffset = lnssOffset + 1
    lnssOffset = Pos( '00'x, gaFontSubset.lnlttFont, lnssOffset )
    IF lnssOffset = 0 THEN DO
      LEAVE
    END

    /* =================================================================
    -- Record the unicode character and glyph index if not already seen
    */
    lnssI = lnssOffset - 1
    lnssUC = C2d( gaEncoding.lnssI )
    IF lassUC.lnssUC = gbFalse THEN DO
      lassUC.lnssUC = gbTrue
      lnssCCs = lnssCCs + 1
      lassCCs.lnssCCs = Right( lnssUC, 5, "0" ) lalttGndx.lnssI
    END
  END

  /* =================================================================
  -- Must be in order by unicode character
  */
  CALL QSort "lassCCs.", gbFalse, 1, lnssCCs

  /* =================================================================
  -- Find the ranges and starting glyph index
  */
  lassBeg. = ""
  lassEnd. = ""
  lassIdx. = ""
  lnssLC   = ""
  lnssCnt  = 0
  lsssNeed = "0"
  DO lnssI = 1 TO lnssCCs
    PARSE VAR lassCCs.lnssI lnssUC lnssIdx

    lsssNeed = lsssNeed lnssIdx

    IF lnssUC - 1 <> lnssLC THEN DO
      lnssCnt = lnssCnt + 1
      lassBeg.lnssCnt = lnssUC
      lassIdx.lnssCnt = lnssI - lnssUC
    END
    lassEnd.lnssCnt = lnssUC
    lnssLC = lnssUC
  END

  /* ===================================================================
  -- Add the end marker
  */
  lnssCnt = lnssCnt + 1
  lassBeg.lnssCnt = 65535
  lassEnd.lnssCnt = 65535
  lassIdx.lnssCnt = 1

  /* ===================================================================
  -- Build the "cmap" table                                           --
  =================================================================== */

  /* ===================================================================
  -- Calc log2( segment count )
  */
  DO lnssI = 0 BY 1 WHILE 2 ** lnssI < lnssCnt
    lnssLog2 = lnssI
  END

  /* ===================================================================
  -- Calc search parameters
  */
  lnssSegCountX2 = lnssCnt * 2
  lnssSearchRange = 2 * ( 2 ** lnssLog2 )
  lnssEntrySelector = lnssSearchRange / 2
  lnssRangeShift = ( lnssCnt * 2 ) - lnssSearchRange

  /* ===================================================================
  -- Start cmap table
  */
  lsssTable = D2c( 0, 2 ) ||,                     /* version */
              D2c( 1, 2 ) ||,                     /* numTables */
              D2c( 3, 2 ) ||,                     /* platformID */
              D2c( 1, 2 ) ||,                     /* encodingID */
              D2c( 12, 4 ) ||,                    /* offset */
              D2c( 4, 2 ) ||,                     /* format */
              D2c( ( lnssCnt * 8 ) + 16, 2 ) ||,  /* length */
              D2c( 0, 2 ) ||,                     /* language */
              D2c( lnssSegCountX2, 2 ) ||,        /* segCountX2 */
              D2c( lnssSearchRange, 2 ) ||,       /* searchRange */
              D2c( lnssEntrySelector, 2 ) ||,     /* entrySelector */
              D2c( lnssRangeShift, 2 )            /* rangeShift */

  /* ===================================================================
  -- Add "endCount" codes
  */
  DO lnssI = 1 TO lnssCnt
    lsssTable = lsssTable || D2c( lassEnd.lnssI, 2 )
  END

  /* ===================================================================
  -- Reserved field
  */
  lsssTable = lsssTable || D2c( 0, 2 )

  /* ===================================================================
  -- Add "startCount" codes
  */
  DO lnssI = 1 TO lnssCnt
    lsssTable = lsssTable || D2c( lassBeg.lnssI, 2 )
  END

  /* ===================================================================
  -- Add "idDelta" values
  */
  DO lnssI = 1 TO lnssCnt
    lsssTable = lsssTable || D2c( lassIdx.lnssI, 2 )
  END

  /* ===================================================================
  -- Add "idRangeOffset" values (not used)
  */
  lsssTable = lsssTable || Copies( '0000'x, lnssCnt )

  /* ===================================================================
  -- Add it to the subset
  */
  CALL TT_AddTable lslttTagCmap, lsssTable

  /* ===================================================================
  -- Build the "loca", "glyph", and "hmtx" tables                     --
  =================================================================== */

  /* ===================================================================
  -- Extract tables to improve speed...at the cost of extra memory usage
  */
  PARSE VAR lslttFont =(lalttInOffset.lslttTagLoca) lsssFloca,
                      +(lalttInLength.lslttTagLoca) .
  PARSE VAR lslttFont =(lalttInOffset.lslttTagHmtx) lsssFhmtx,
                      +(lalttInLength.lslttTagHmtx) .

  lnssAdvanceWidthMax = 0
  lnssMinLsb = 0
  lsssMinRsb = 0
  lnssXmaxExtent = 0
  lnssNumHmtx = 0
  lsssHmtx = gsNull
  lsssLoca = gsNull
  lsssGlyf = gsNull

  NUMERIC DIGITS 10

  /* ===================================================================
  -- Process all glyph indexes (composites may add more)
  */
  DO lnssG = 1 UNTIL lnssG = Words( lsssNeed )
    lnssI = Word( lsssNeed, lnssG )

    /* =================================================================
    -- Add the glyph to the hmtx table                                --
    ================================================================= */

    /* =================================================================
    -- The last entry applies to any glyph indexes beyond the number of
    -- metrics.  Monospaced fonts only need 1 entry as all glyphs have
    -- the same width
    */
    IF lnssI <> -1 & lnssI < lnlttNumberOfHMetrics THEN DO
      lnssOffset = ( lnssI * 4 ) + 1
      PARSE VAR lsssFhmtx =(lnssOffset) lnssAdvanceWidth,
                          +2            lnssLsb,
                          +2            .
    END
    ELSE DO
      IF lnssI = -1 THEN DO
        lnssI = 0
      END

      lnssOffset = ( ( lnlttNumberOfHMetrics - 1 ) * 4 ) + 1
      PARSE VAR lsssFhmtx =(lnssOffset) lnssAdvanceWidth,
                          +2            lnssLsb,
                          +2            .

      lnssOffset = lnssOffset + 4 + ,
                   ( ( lnssI - lnlttNumberOfHMetrics ) * 2 )

      IF lnssOffset < Length( lnssFhmtx ) THEN DO
        PARSE VAR lsssFhmtx =(lnssOffset) lnssLsb,
                            +2            .
      END
    END

    lsssHmtx = lsssHmtx ||,
               lnssAdvanceWidth ||,
               lnssLsb
    lnssNumHmtx = lnssNumHmtx + 1

    lnssAdvanceWidth = C2d( lnssAdvanceWidth )
    lnssAdvanceWidthMax = Max( lnssAdvanceWidthMax, lnssAdvanceWidth )

    /* =================================================================
    -- Get offset to glyph in source font
    */
    lnssOffset = 1 + ( lnssI * lnlttLocaSize )
    PARSE VAR lsssFloca =(lnssOffset)   lnssGoff,
                        +(lnlttLocaSize) lnssNoff,
                        +(lnlttLocaSize) .

    lnssSoff = lalttInOffset.lslttTagGlyf + ,
               ( C2d( lnssGoff ) * lnlttLocaMult )

    lnssSlen = ( lalttInOffset.lslttTagGlyf + ,
               ( C2d( lnssNoff ) * lnlttLocaMult ) ) - lnssSoff

    /* =================================================================
    -- Extract the glyph from the source font
    */
    IF lnssSlen = 0 THEN DO
      lsssGlyph = gsNull
      lnssNum = 0
      lnssXmin = 0
      lnssYmin = 0
      lnssXmax = 0
      lnssYmax = 0
    END
    ELSE DO
      PARSE VAR lslttFont =(lnssSoff) lsssGlyph,
                          +(lnssSlen) .,
                          =(lnssSoff) lnssNum,
                          +2          lnssXmin,
                          +2          lnssYmin,
                          +2          lnssXmax,
                          +2          lnssYmax,
                          +2          .
      lnssNum = C2d( lnssNum, 2 )
      lnssXmin = C2d( lnssXmin, 2 )
      lnssYmin = C2d( lnssYmin, 2 )
      lnssXmax = C2d( lnssXmax, 2 )
      lnssYmax = C2d( lnssYmax, 2 )
    END

    /* =================================================================
    -- Only glyphs with contours get included in these calcs
    */
    IF lnssNum > 0 THEN DO
      lnssLsb = C2d( lnssLsb, 2 )
      lnssExtent = lnssXmax - lnssXmin
      lnssRsb = lnssAdvanceWidth - lnssLsb - lnssExtent

      lnssMinLsb = Min( lnssMinLsb, lnssLsb )
      lsssMinRsb = Min( lsssMinRsb, lnssRsb )
      lnssXmaxExtent = Max( lnssXmaxExtent, lnssLsb + lnssExtent )
    END

    /* =================================================================
    -- Add the glyph to the loca and glyph tables                     --
    ================================================================= */

    /* =================================================================
    -- Handle a composite glyph
    */
    IF lnssNum = -1 THEN DO

      /* ===============================================================
      -- Start at beginning of composite description
      */
      lnssOffset = 11

      DO FOREVER
        /* =============================================================
        -- Get the composite flags and real glyph index
        */
        PARSE VAR lsssGlyph =(lnssOffset) lsssFlags,
                            +2            lnssI,
                            +2            .

        /* =============================================================
        -- Determine real glyph index in dest font
        */
        lnssI = C2d( lnssI )
        lnssNum = WordPos( lnssI, lsssNeed )
        IF lnssNum = 0 THEN DO
          lsssNeed = lsssNeed lnssI
          lnssI = Words( lsssNeed )
        END
        ELSE DO
          lnssI = lnssNum
        END

        /* =============================================================
        -- Replace index in composite description
        */
        lsssGlyph = Overlay( D2c( lnssI - 1, 2 ),,
                             lsssGlyph,,
                             lnssOffset + 2 )

        /* =============================================================
        -- Done if MORE_COMPONENTS flag isn't set
        */
        IF Bitand( '0020'x, lsssFlags ) = '0000'x THEN DO
          LEAVE
        END

        /* =============================================================
        -- Get length of arguments (ARG_1_AND_2_ARE_WORDS)
        */
        IF Bitand( '0001'x, lsssFlags ) = '0000'x THEN DO
          lnssNum = 2
        END
        ELSE DO
          lnssNum = 4
        END

        /* =============================================================
        -- Get length of optional scale
        */
        IF Bitand( '0008'x, lsssFlags ) <> '0000'x THEN DO
          lnssNum = lnssNum + 2;
        END
        ELSE IF Bitand( '0040'x, lsssFlags ) <> '0000'x THEN DO
          lnssNum = lnssNum + 4;
        END
        ELSE IF Bitand( '0080'x, lsssFlags ) <> '0000'x THEN DO
          lnssNum = lnssNum + 8;
        END

        /* =============================================================
        -- Bump past static portion, arguments, and optional scale
        */
        lnssOffset = lnssOffset + 4 + lnssNum

        /* =============================================================
        -- Bypass optional instructions (WE_HAVE_INSTR)
        */
        IF Bitand( '0100'x, lsssFlags ) <> '0000'x THEN DO
          PARSE VAR lsssGlyph =(lnssOffset) lnssNum,
                             +2            .

          /* ===========================================================
          -- Bump past instructions
          */
          lnssOffset = lnssOffset + 2 + C2d( lnssNum )
        END
      END
    END

    /* =================================================================
    -- Add the offset of this glyph to the loca table
    */
    lsssLoca = lsssLoca ||,
               D2c( Length( lsssGlyf ) / lnlttLocaMult, lnlttLocaSize )

    /* =================================================================
    -- Add the (possibily modified) glyph to the glyf table
    */
    lsssGlyf = lsssGlyf || lsssGlyph

  END

  /* ===================================================================
  -- Add the final offset to the end of the last glyph
  */
  lsssLoca = lsssLoca ||,
             D2c( Length( lsssGlyf ) / lnlttLocaMult, lnlttLocaSize )

  /* ===================================================================
  -- Update number of horizontal metrics
  */
  lnlttNumberOfHMetrics = lnssNumHmtx

  /* ===================================================================
  -- Add tables to the subset
  */
  CALL TT_AddTable lslttTagHmtx, lsssHmtx
  CALL TT_AddTable lslttTagLoca, lsssLoca
  CALL TT_AddTable lslttTagGlyf, lsssGlyf

  DROP lsssHmtx lsssLoca lsssGlyf lsssFloca lsssFhmtx

  /* ===================================================================
  -- Build the "hhea" table                                           --
  =================================================================== */

  PARSE VAR lslttFont =(lalttinOffset.lslttTagHhea) lsssTable,
                      +(lalttInLength.lslttTagHhea) .
IF 0 THEN DO
  lsssTable = Overlay( D2c( lnssAdvanceWidthMax, 2 ), lsssTable, 11 )
  lsssTable = Overlay( D2c( lnssMinLsb, 2 ), lsssTable, 13 )
  lsssTable = Overlay( D2c( lsssMinRsb, 2 ), lsssTable, 15 )
  lsssTable = Overlay( D2c( lnssXmaxExtent, 2 ), lsssTable, 17 )
END
  lsssTable = Overlay( D2c( lnssNumHmtx, 2 ), lsssTable, 35 )

  CALL TT_AddTable lslttTagHhea, lsssTable

  /* ===================================================================
  -- Build the "maxp" table                                           --
  =================================================================== */

  PARSE VAR lslttFont =(lalttinOffset.lslttTagMaxp) lsssTable,
                      +(lalttInLength.lslttTagMaxp) .

  lsssTable = Overlay( D2c( lnssNumHmtx, 2 ), lsssTable, 5 )

  CALL TT_AddTable lslttTagMaxp, lsssTable

  /* ===================================================================
  -- Build the "post" table                                           --
  =================================================================== */

  /* ===================================================================
  -- All we do is extract the header and change the version to 3 so we
  -- don't have to include the character names.
  */
  PARSE VAR lslttFont =(lalttinOffset.lslttTagPost) lsssTable,
                      +32 .

  lsssTable = Overlay( '00030000'x, lsssTable, 1 )

  CALL TT_AddTable lslttTagPost, lsssTable

  /* ===================================================================
  -- Build the "name" table                                           --
  =================================================================== */
IF 0 THEN DO
  lsssTable = '0000'x ||,  /* format */
              '0001'x ||,  /* count */
              '0012'x ||,  /* stringOffset */
              '0001'x ||,  /* platformID */
              '0000'x ||,  /* encodingID */
              '0000'x ||,  /* languageID */
              '0006'x ||,  /* nameID */
              D2c( Length( gaFont_FontName.lnlttFont ), 2 ) ||, /*len*/
              '0000'x ||,  /* offset */
              gaFont_FontName.lnlttFont

  CALL TT_AddTable lslttTagName, lsssTable
END
  /* ===================================================================
  -- Finally build the font directory                                 --
  =================================================================== */

  NUMERIC DIGITS 10

  lnssOffset = 13
  DO lnssI = 1 TO Words( lsssReq )

    /* =================================================================
    -- Get tag and calculate checksum of table
    */
    lsssTag = Left( Word( lsssReq, lnssI ), 4 )
    lnssSum = TT_Checksum( lassSubOff.lsssTag, lassSubLen.lsssTag )

    /* =================================================================
    -- Build directory entry
    */
    lsssTable = lsssTag ||,
                D2c( lnssSum, 4 ) ||,
                D2c( lassSubOff.lsssTag, 4 ) ||,
                D2c( lassSubLen.lsssTag, 4 )

    /* =================================================================
    -- Replace the reserved entry
    */
    lsssFont = Overlay( lsssTable, lsssFont, lnssOffset )

    /* =================================================================
    -- Replace source font offset and length
    */
    lalttInOffset.lsssTag = lassSubOff.lsssTag + 1
    lalttInLength.lsssTag = lassSubLen.lsssTag

    /* =================================================================
    -- Bump to next entry
    */
    lnssOffset = lnssOffset + 16
  END

  /* ===================================================================
  -- Calculate the font's checksum adjustment
  */
  lnssSum = X2d( "B1B0AFBA" ) - TT_Checksum( 0, Length( lsssFont ) )

  /* ===================================================================
  -- Store in "head" table and replace source font with subset
  */
  lslttFont = Overlay( D2c( lnssSum, 4 ),,
                            lsssFont,,
                            lassSubOff.lslttTagHead + 9 )

  DROP lassUC. lnssLC lnssUC lassBeg. lnssAdvanceWidthMax lassEnd.,
       lassCCs. lassIdx. lnssCCs lsssTag lnssLog2 lnssIdx lnssCnt,
       lnssRsb lsssReq lnssNum lnssSegCountX2 lnssRangeShift,
       lsssNeed lnssGoff lsssLoca lnssNoff lnssSoff lnssSlen,
       lsssGlyf lnssXmin lnssYmin lnssXmax lnssYmax lsssCopy,
       lsssHmtx lnssXmaxExtent lnssSearchRange lsssTable lsssGlyph,
       lnssStart lnssAdvanceWidth lassSubOff. lassSubLen. lnssI,
       lnssMinLsb lnssOffset lsssMinRsb lnssExtent lnssG,
       lnssEntrySelector lnssNumHmtx lnssLsb lsssTag lnssSum,
       lsssFont
/*
Call SysDumpVariables
*/
RETURN

/*
-- =====================================================================
-- Routine:   TT_AddTable      : Adds a table and maintain alignment
-- Arguments: lnatTag          : Table tag
--            lnatTable        : Table content
-- Return:    (none)           : none required
-- =====================================================================
*/
TT_AddTable:

  PARSE ARG lsatTag, lsatTable

  lassSubOff.lsatTag = Length( lsssFont )
  lassSubLen.lsatTag = Length( lsatTable )

  lsssFont = lsssFont || lsatTable

  lsssFont = Left( lsssFont,,
                      ( ( Length( lsssFont ) + 3 ) % 4 ) * 4, '00'x )

  DROP lsatTag lsatTable

RETURN

/*
-- =====================================================================
-- Routine:   TT_Checksum      : Calculates a checksum
-- Arguments: lncOffset        : Offset where to start summing
--            lncLength        : Length (in bytes)
-- Return:    lncSum           : Calculated checksum
-- =====================================================================
*/
TT_Checksum:

  PARSE ARG lncOffset, lncLength

  lncSum = 0
  lncOffset = lncOffset + 1
  lncCnt = ( lncLength + 3 ) % 4

  DO lncI = 1 TO lncCnt
    PARSE VAR lsssFont =(lncOffset) lncLong +4 .

    lncSum = ( lncSum + C2d( lncLong ) ) // (2 ** 32)
    lncOffset = lncOffset + 4
  END

  DROP lncOffset lncLength lncCnt lncLong

RETURN lncSum

/*
-- =====================================================================
-- Routine:   WinAnsiEncoding   : Init WinAnsiEncoding
-- Arguments: (none)            : none required
-- Return:    (none)            : none required
-- =====================================================================
*/
WinAnsiEncoding:

  /* ===================================================================
  -- Initialize WinAnsiEncoding
  */
  DO lnwaeI = 0 TO 255
    gaWinAnsi.lnwaeI = D2c( lnwaeI )
  END

  gaWinAnsi.127 = '2022'x /* bullet */
  gaWinAnsi.128 = '20AC'x /* Euro */
  gaWinAnsi.129 = '2022'x /* bullet */
  gaWinAnsi.130 = '201A'x /* quotesinglbase */
  gaWinAnsi.131 = '0192'x /* florin */
  gaWinAnsi.132 = '201E'x /* quotedblbase */
  gaWinAnsi.133 = '2026'x /* ellipsis */
  gaWinAnsi.134 = '2020'x /* dagger */
  gaWinAnsi.135 = '2021'x /* daggerdbl */
  gaWinAnsi.136 = '02C6'x /* circumflex */
  gaWinAnsi.137 = '2030'x /* perthousand */
  gaWinAnsi.138 = '0160'x /* Scaron */
  gaWinAnsi.139 = '2039'x /* guilsinglleft */
  gaWinAnsi.140 = '0152'x /* OE */
  gaWinAnsi.141 = '2022'x /* bullet */
  gaWinAnsi.142 = '017D'x /* Zcaron */
  gaWinAnsi.143 = '2022'x /* bullet */
  gaWinAnsi.144 = '2022'x /* bullet */
  gaWinAnsi.145 = '2018'x /* quoteleft */
  gaWinAnsi.146 = '2019'x /* quoteright */
  gaWinAnsi.147 = '201C'x /* quotedblleft */
  gaWinAnsi.148 = '201D'x /* quotedblright */
  gaWinAnsi.149 = '2022'x /* bullet */
  gaWinAnsi.150 = '2013'x /* endash */
  gaWinAnsi.151 = '2014'x /* emdash */
  gaWinAnsi.152 = '02DC'x /* tilde */
  gaWinAnsi.153 = '2122'x /* trademark */
  gaWinAnsi.154 = '0161'x /* scaron */
  gaWinAnsi.155 = '203A'x /* guilsinglright */
  gaWinAnsi.156 = '0153'x /* oe */
  gaWinAnsi.157 = '2022'x /* bullet */
  gaWinAnsi.158 = '017E'x /* zcaron */
  gaWinAnsi.159 = '0178'x /* Ydieresis */
  gaWinAnsi.160 = '0020'x /* space */
  gaWinAnsi.173 = '002D'x /* hyphen */

  DROP lnwaeI

RETURN

/*
-- =====================================================================
-- Routine:   MacRomanEncoding  : Init MacRomanEncoding
-- Arguments: (none)            : none required
-- Return:    (none)            : none required
-- =====================================================================
*/
MacRomanEncoding:

  /* ===================================================================
  -- Initialize MacRomanEncoding
  */
  DO lnmreI = 0 TO 255
    gaMacRoman.lnmreI = D2c( lnmreI )
  END

  gaMacRoman.128 = '00C4'x /* Adieresis */
  gaMacRoman.129 = '00C5'x /* Aring */
  gaMacRoman.130 = '00C7'x /* Ccedilla */
  gaMacRoman.131 = '00C9'x /* Eacute */
  gaMacRoman.132 = '00D1'x /* Ntilde */
  gaMacRoman.133 = '00D6'x /* Odieresis */
  gaMacRoman.134 = '00DC'x /* Udieresis */
  gaMacRoman.135 = '00E1'x /* aacute */
  gaMacRoman.136 = '00E0'x /* agrave */
  gaMacRoman.137 = '00E2'x /* acircumflex */
  gaMacRoman.138 = '00E4'x /* adieresis */
  gaMacRoman.139 = '00E3'x /* atilde */
  gaMacRoman.140 = '00E5'x /* aring */
  gaMacRoman.141 = '00E7'x /* ccedilla */
  gaMacRoman.142 = '00E9'x /* eacute */
  gaMacRoman.143 = '00E8'x /* egrave */
  gaMacRoman.144 = '00EA'x /* ecircumflex */
  gaMacRoman.145 = '00EB'x /* edieresis */
  gaMacRoman.146 = '00ED'x /* iacute */
  gaMacRoman.147 = '00EC'x /* igrave */
  gaMacRoman.148 = '00EE'x /* icircumflex */
  gaMacRoman.149 = '00EF'x /* idieresis */
  gaMacRoman.150 = '00F1'x /* ntilde */
  gaMacRoman.151 = '00F3'x /* oacute */
  gaMacRoman.152 = '00F2'x /* ograve */
  gaMacRoman.153 = '00F4'x /* ocircumflex */
  gaMacRoman.154 = '00F6'x /* odieresis */
  gaMacRoman.155 = '00F5'x /* otilde */
  gaMacRoman.156 = '00FA'x /* uacute */
  gaMacRoman.157 = '00F9'x /* ugrave */
  gaMacRoman.158 = '00FB'x /* ucircumflex */
  gaMacRoman.159 = '00FC'x /* udieresis */
  gaMacRoman.160 = '2020'x /* dagger */
  gaMacRoman.161 = '00B0'x /* degree */
  gaMacRoman.164 = '00A7'x /* section */
  gaMacRoman.165 = '2022'x /* bullet */
  gaMacRoman.166 = '00B6'x /* paragraph */
  gaMacRoman.167 = '00DF'x /* germandbls */
  gaMacRoman.168 = '00AE'x /* registered */
  gaMacRoman.170 = '2122'x /* trademark */
  gaMacRoman.171 = '00B4'x /* acute */
  gaMacRoman.172 = '00A8'x /* dieresis */
  gaMacRoman.173 = '2260'x /* notequal */
  gaMacRoman.174 = '00C6'x /* AE */
  gaMacRoman.175 = '00D8'x /* Oslash */
  gaMacRoman.176 = '221E'x /* infinity */
  gaMacRoman.178 = '2264'x /* lessequal */
  gaMacRoman.179 = '2265'x /* greaterequal */
  gaMacRoman.180 = '00A5'x /* yen */
  gaMacRoman.182 = '2202'x /* partialdiff */
  gaMacRoman.183 = '2211'x /* summation */
  gaMacRoman.184 = '220F'x /* product */
  gaMacRoman.185 = '03C0'x /* pi */
  gaMacRoman.186 = '222B'x /* integral */
  gaMacRoman.187 = '00AA'x /* ordfeminine */
  gaMacRoman.188 = '00BA'x /* ordmasculine */
  gaMacRoman.189 = '2126'x /* Omega */
  gaMacRoman.190 = '00E6'x /* ae */
  gaMacRoman.191 = '00F8'x /* oslash */
  gaMacRoman.192 = '00BF'x /* questiondown */
  gaMacRoman.193 = '00A1'x /* exclamdown */
  gaMacRoman.194 = '00AC'x /* logicalnot */
  gaMacRoman.195 = '221A'x /* radical */
  gaMacRoman.196 = '0192'x /* florin */
  gaMacRoman.197 = '2248'x /* approxequal */
  gaMacRoman.198 = '2206'x /* Delta */
  gaMacRoman.199 = '00AB'x /* guillemotleft */
  gaMacRoman.200 = '00BB'x /* guillemotright */
  gaMacRoman.201 = '2026'x /* ellipsis */
  gaMacRoman.202 = '0020'x /* space */
  gaMacRoman.203 = '00C0'x /* Agrave */
  gaMacRoman.204 = '00C3'x /* Atilde */
  gaMacRoman.205 = '00D5'x /* Otilde */
  gaMacRoman.206 = '0152'x /* OE */
  gaMacRoman.207 = '0153'x /* oe */
  gaMacRoman.208 = '2013'x /* endash */
  gaMacRoman.209 = '2014'x /* emdash */
  gaMacRoman.210 = '201C'x /* quotedblleft */
  gaMacRoman.211 = '201D'x /* quotedblright */
  gaMacRoman.212 = '2018'x /* quoteleft */
  gaMacRoman.213 = '2019'x /* quoteright */
  gaMacRoman.214 = '00F7'x /* divide */
  gaMacRoman.215 = '25CA'x /* lozenge */
  gaMacRoman.216 = '00FF'x /* ydieresis */
  gaMacRoman.217 = '0178'x /* Ydieresis */
  gaMacRoman.218 = '2044'x /* fraction */
  gaMacRoman.219 = '00A4'x /* currency */
  gaMacRoman.220 = '2039'x /* guilsinglleft */
  gaMacRoman.221 = '203A'x /* guilsinglright */
  gaMacRoman.222 = 'FB01'x /* fi */
  gaMacRoman.223 = 'FB02'x /* fl */
  gaMacRoman.224 = '2021'x /* daggerdbl */
  gaMacRoman.225 = '00B7'x /* periodcentered */
  gaMacRoman.226 = '201A'x /* quotesinglbase */
  gaMacRoman.227 = '201E'x /* quotedblbase */
  gaMacRoman.228 = '2030'x /* perthousand */
  gaMacRoman.229 = '00C2'x /* Acircumflex */
  gaMacRoman.230 = '00CA'x /* Ecircumflex */
  gaMacRoman.231 = '00C1'x /* Aacute */
  gaMacRoman.232 = '00CB'x /* Edieresis */
  gaMacRoman.233 = '00C8'x /* Egrave */
  gaMacRoman.234 = '00CD'x /* Iacute */
  gaMacRoman.235 = '00CE'x /* Icircumflex */
  gaMacRoman.236 = '00CF'x /* Idieresis */
  gaMacRoman.237 = '00CC'x /* Igrave */
  gaMacRoman.238 = '00D3'x /* Oacute */
  gaMacRoman.239 = '00D4'x /* Ocircumflex */
  gaMacRoman.240 = 'F8FF'x /* apple */
  gaMacRoman.241 = '00D2'x /* Ograve */
  gaMacRoman.242 = '00DA'x /* Uacute */
  gaMacRoman.243 = '00DB'x /* Ucircumflex */
  gaMacRoman.244 = '00D9'x /* Ugrave */
  gaMacRoman.245 = '0131'x /* dotlessi */
  gaMacRoman.246 = '02C6'x /* circumflex */
  gaMacRoman.247 = '02DC'x /* tilde */
  gaMacRoman.248 = '00AF'x /* macron */
  gaMacRoman.249 = '02D8'x /* breve */
  gaMacRoman.250 = '02D9'x /* dotaccent */
  gaMacRoman.251 = '02DA'x /* ring */
  gaMacRoman.252 = '00B8'x /* cedilla */
  gaMacRoman.253 = '02DD'x /* hungarumlaut */
  gaMacRoman.254 = '02DB'x /* ogonek */
  gaMacRoman.255 = '02C7'x /* caron */

  DROP lbmreI

RETURN

/*
-- =====================================================================
-- Routine:   EmitEncoding     : Write encoding to output stream
-- Arguments: lbeeEmitMap      : TRUE = write the cmap
-- Return:    (none)           : none required
-- =====================================================================
*/
EmitEncoding:

  PARSE ARG lneeNdx, lbeeEmitMap

  /* ===================================================================
  -- Default to WinAnsiEncoding
  */
  IF lneeNdx = 0 THEN DO
    lneeNdx = gaEncodingIndx.WINANSI
  END

  /* ===================================================================
  -- Only write the /Font encoding dictionary entry
  */
  IF lbeeEmitMap = gbFalse THEN DO
    SELECT
      WHEN gaEncodingName.lneeNdx = "WinAnsi" THEN DO
        CALL QueueCntl "/Encoding /WinAnsiEncoding"
      END

      WHEN gaEncodingName.lneeNdx = "MacRoman" THEN DO
        CALL QueueCntl "/Encoding /MacRomanEncoding"
      END

      OTHERWISE DO
        IF gaEncodingObj.lneeNdx <> 0 THEN DO
          CALL QueueCntl "/ToUnicode" gaEncodingObj.lneeNdx "0 R"
        END

        /* =============================================================
        -- NOTE TO SELF: Don't need to do this once we have
        -- subsetting since we move the glyph to the char code
        */
        IF gaDiffsObj.lneeNdx <> 0 THEN DO
          CALL QueueCntl "/Encoding" gaDiffsObj.lneeNdx "0 R"
        END
      END
    END
  END
  ELSE DO 1
    SELECT
      WHEN gaEncodingName.lneeNdx = "WinAnsi" THEN DO
        /* =============================================================
        -- Set current encoding
        */
        DO lneeI = 0 TO 255
          gaEncoding.lneeI = gaWinAnsi.lneeI
        END
        LEAVE
      END

      WHEN gaEncodingName.lneeNdx = "MacRoman" THEN DO
        /* =============================================================
        -- Set current encoding
        */
        DO lneeI = 0 TO 255
          gaEncoding.lneeI = gaMacRoman.lneeI
        END
        LEAVE
      END

      OTHERWISE DO
        NOP
      END
    END

    /* =================================================================
    -- Load the user encoding
    */
    CALL LoadEncoding gaEncodingPath.lneeNdx

    /* =================================================================
    -- Find all ranges whose code values differ from WinAnsiEncoding
    */
    laeeDiffs. = gsNull
    laeeBeg. = 0
    laeeEnd. = 0
    laeeUC.  = ""
    lneeLC   = ""
    lneeCnt  = 0
    lbeeInRange = gbFalse
    lneeDiffs = 0
    lneeLastI = 0
    DO lneeI = 0 TO 255
      lneeUC = C2d( gaEncoding.lneeI )
      IF lneeUC <> C2d( gaWinAnsi.lneeI ) THEN DO
        IF lbeeInRange = gbFalse | lneeUC - 1 <> lneeLC THEN DO
          lbeeInRange = gbTrue
          lneeCnt = lneeCnt + 1
          laeeBeg.lneeCnt = lneeI
          laeeUC.lneeCnt = lneeUC
          IF lneeI - 1 <> lneeLastI THEN DO
            lneeDiffs = lneeDiffs + 1
            laeeDiffs.lneeDiffs = lneeI || " "
          END
        END
        lneeLastI = lneeI
        laeeEnd.lneeCnt = lneeI
        laeeDiffs.lneeDiffs = laeeDiffs.lneeDiffs || ,
                              "/uni" || D2x( lneeUC, 4 )
      END
      ELSE DO
        lbeeInRange = gbFalse
      END
      lneeLC = lneeUC
    END

    /* =================================================================
    -- Generate single byte, multibyte, and differences entries
    */
    lneeRanges = 0
    lneeChars = 0
    DO lneeI = 1 TO lneeCnt
      lneeRS = laeeBeg.lneeI
      lneeRE = laeeEnd.lneeI
      lneeUC = laeeUC.lneeI

      /* ===============================================================
      -- Single byte and multibyte ranges get written to the CMap in
      -- different tables
      */
      IF lneeRS <> lneeRE THEN DO
        lneeRanges = lneeRanges + 1
        laeeRanges.lneeRanges = "<" || D2x( lneeRS, 2 ) || ">" ||,
                                "<" || D2x( lneeRE, 2 ) || ">" ||,
                                "<" || D2x( lneeUC, 4 ) || ">"
      END
      ELSE DO
        lneeChars = lneeChars + 1
        laeeChars.lneeChars = "<" || D2x( lneeRS, 2 ) || ">" ||,
                              "<" || D2x( lneeUC, 4 ) || ">"
      END
    END

    /* =================================================================
    -- Only emit the encoding and differences objects if needed
    */
    IF lneeDiffs <> 0 THEN DO
      /* ===============================================================
      -- Write the ToUnicode CMap
      */
      gaEncodingObj.lneeNdx = NewObj()

      CALL QueueCntl "<<"

      CALL StartStream
      CALL QueueCntl "/CIDInit /ProcSet findresource begin"
      CALL QueueCntl "12 dict begin"
      CALL QueueCntl "begincmap"
      CALL QueueCntl "/CIDSystemInfo <<"
      CALL QueueCntl "  /Registry (Adobe)"
      CALL QueueCntl "  /Ordering (UCS)"
      CALL QueueCntl "  /Supplement 0"
      CALL QueueCntl ">> def"
      CALL QueueCntl "/CMapName /Adobe-Identity-UCS def"
      CALL QueueCntl "/CMapType 2 def"
      CALL QueueCntl "1 begincodespacerange"
      CALL QueueCntl "<00><FF>"
      CALL QueueCntl "endcodespacerange"

      /* ===============================================================
      -- Emit the single byte CMap table in 100 entry segments
      */
      DO lneeI = 1 TO lneeChars BY 100
        lneeCount = Min( lneeChars - lneeI + 1, 100 )
        CALL QueueCntl lneeCount "beginbfchar"
        DO lneeJ = lneeI TO lneeI + lneeCount - 1
          CALL QueueCntl laeeChars.lneeJ
        END
        CALL QueueCntl "endbfchar"
      END

      /* ===============================================================
      -- Emit the multibyte CMap table in 100 entry segments
      */
      DO lneeI = 1 TO lneeRanges BY 100
        lneeCount = Min( lneeRanges - lneeI + 1, 100 )
        CALL QueueCntl lneeCount "beginbfrange"
        DO lneeJ = lneeI TO lneeI + lneeCount - 1
          CALL QueueCntl laeeRanges.lneeJ
        END
        CALL QueueCntl "endbfrange"
      END

      CALL QueueCntl "endcmap"
      CALL QueueCntl "CMapName currentdict /CMap defineresource pop"
      CALL QueueCntl "end"
      CALL QueueCntl "end"
      CALL EndStream

      /* ===============================================================
      -- Write the Encoding object
      */
      gaDiffsObj.lneeNdx = NewObj()

      CALL QueueCntl "<<"
      CALL QueueCntl "/Type /Encoding"
      CALL QueueCntl "/BaseEncoding /WinAnsiEncoding"
      CALL QueueCntl "/Differences"
      CALL QueueCntl "["

      DO lneeI = 1 TO lneeDiffs
        CALL QueueCntl laeeDiffs.lneeI
      END

      CALL QueueCntl "]"
      CALL QueueCntl ">>"

      CALL EndObj
    END

    DROP laeeBeg. laeeEnd. laeeUC. lneeLC lneeCnt lneeI,
         lneeUC lneeRanges lneeChars lneeDiffs lneeRS lneeRE laeeRanges.,
         laeeChars. laeeDiffs. lseeDiffs lneeJ lneeCount lneeNdx,
          lneeLastI

  END

  DROP lneeNdx lbeeEmitMap

RETURN

/*
-- =====================================================================
-- Routine:   LoadEncoding      : Load user defined encoding
-- Arguments: lsueName          : Name of UCM file
-- Return:    (none)            : none required
-- =====================================================================
*/
LoadEncoding:
  PARSE ARG lsueName

  /* ===================================================================
  -- Initialize
  */
  DO lnueI = 0 TO 255
    gaEncoding.lnueI = lnueI
  END

  lbueCmap = gbFalse
  lbueSbcs = gbFalse
  lnueLine = 0

  /* ===================================================================
  -- Open the file
  */
  lsueH = ReadOpen( lsueName )

  /* ===================================================================
  -- Parse the UCM
  */
  DO UNTIL laueL.0 = 0
    CALL ReadFile lsueH, "laueL."
    DO lnueI = 1 TO laueL.0

      /* ===============================================================
      -- Track line number for messages
      */
      lnueLine = lnueLine + 1

      /* ===============================================================
      -- Ignore comment lines
      */
      IF Left( laueL.lnueI, 1 ) = "#" THEN DO
        ITERATE
      END

      /* ===============================================================
      -- Ignore blank lines
      */
      IF Strip( laueL.lnueI, "B" ) = "" THEN DO
        ITERATE
      END

      /* ===============================================================
      -- Extract the first two words
      */
      PARSE VAR laueL.lnueI lsue1 lsue2 .

      /* ===============================================================
      -- Remember its an SBCS mapping
      */
      IF lsue1 lsue2 = '<uconv_class> "SBCS"' THEN DO
        lbueSbcs = gbTrue
        ITERATE
      END

      /* ===============================================================
      -- Reached the start of the actual mapping?
      */
      IF lsue1 = "CHARMAP" THEN DO
        /* =============================================================
        -- We only support SBCS mappings
        */
        IF lbueSbcs <> gbTrue THEN DO
          CALL Issue 182, lsueName
          SIGNAL Done
        END

        lbueCmap = gbTrue
        ITERATE
      END

      /* ===============================================================
      -- We're done if we've Reached the end of the mapping
      */
      IF lsue1 lsue2 = "END CHARMAP" THEN DO
        LEAVE
      END

      /* ===============================================================
      -- Ignore the line if we haven't reached the mappings
      */
      IF lbueCmap = gbFalse THEN DO
        ITERATE
      END

      /* ===============================================================
      -- Parse the mapping line
      */
      PARSE VAR laueL.lnueI '<' lsueUS +2 lnueUC +4 lsueUE .,
                            '\' lsueCS +2 lnueCC .,
                            '|' lsueFS +1 lsueFI

      /* ===============================================================
      -- Make sure we got all the delimiters we expect
      */
      IF lsueUS lsueUE lsueCS lsueFS <> "<U > \x |" THEN DO
        CALL Issue 183, lnueLine, lsueName, laueL.lnueI
        SIGNAL Done
      END

      /* ===============================================================
      -- Make sure the unicode value looks right
      */
      IF Datatype( lnueUC, "X" ) <> gbTrue THEN DO
        CALL Issue 184, lnueUC, lnueLine, lsueName
        SIGNAL Done
      END

      /* ===============================================================
      -- Make sure the character code looks right
      */
      IF Datatype( lnueCC, "X" ) <> gbTrue THEN DO
        CALL Issue 185, lnueCC, lnueLine, lsueName
        SIGNAL Done
      END

      /* ===============================================================
      -- Make sure they didn't try to sneak in a bogus character code
      */
      lnueCC = X2d( lnueCC )
      IF lnueCC < 0 | lnueCC > 255 THEN DO
        CALL Issue 185, D2x( lnueCC ), lnueLine, lsueName
        SIGNAL Done
      END

      /* ===============================================================
      -- Only use the exact mapping
      */
      IF lsueFI = "0" THEN DO
        gaEncoding.lnueCC = X2c( lnueUC )
      END
    END
  END

  /* ===================================================================
  -- Close it
  */
  CALL ReadClose lsueH

  DROP lsueName lbueCmap lbueSbcs lsueH laueL. lnueI lsue1 lsue2 lsueUS,
       lnueUC lsueUE lsueCS lnueCC lsueFS lsueFI

RETURN

/*
-- =====================================================================
-- Routine:   Exists           : Check for existence of a file
-- Arguments: lseF             : File ID
-- Return:    lbeX             : true/false
-- =====================================================================
*/
Exists:
  PARSE ARG lseF

  SELECT
    /* =================================================================
    -- Handle TSO
    */
    WHEN gsSystem = "TSO" THEN DO
      /* ===============================================================
      -- Ensure "DD:" is uppercase
      */
      IF Translate( Left( lseF, 3 ) ) = "DD:" THEN DO
        lbeX = ( ListDSI( Substr( lseF, 4 ) "FILE" ) = 0 )
      END
      ELSE DO
        /* =============================================================
        -- To Qualify with HLQ or Not?
        */
        IF Left( lseF, 1 ) <> "'" THEN DO
           lseF = "'" || gsHLQ || "." || lseF || "'"
        END

        /* =============================================================
        -- Is it there?
        */
        lbeX = ( ListDSI( lseF "RECALL" ) = 0 )
      END
    END

    /* =================================================================
    -- Handle OMVS
    */
    WHEN gsSystem = "OMVS" THEN DO
      ADDRESS SYSCALL "stat (lseF) lseSt."
      lbeX = ( RC = 0 & RETVAL <> -1 )
    END

    /* =================================================================
    -- Handle CMS
    */
    WHEN gsSystem = "CMS" THEN DO
      lsef = Translate( Translate( lsef, " ", "." ) )
      lbeX = ( Stream( lseF, "C", "QUERY EXISTS" ) <> gsNull )
    END

    /* =================================================================
    -- Handle Regina
    */
    WHEN gsSystem = "REGINA" THEN DO
      lbeX = ( Stream( lseF, "C", "QUERY EXISTS" ) <> gsNull )
    END

    /* =================================================================
    -- Handle uni-REXX
    */
    WHEN gsSystem = "UNI-REXX" THEN DO
      lbeX = ( Stream( lseF, "C", "QUERY EXISTS" ) <> gsNull )
    END

    /* =================================================================
    -- Handle ooRexx
    */
    WHEN gsSystem = "OOREXX" THEN DO
      lbeX = ( Stream( lseF, "C", "QUERY EXISTS" ) <> gsNull )
    END

    OTHERWISE DO
      lbeX = gbFalse
    END
  END

  DROP lseF

RETURN lbeX

/*
-- =====================================================================
-- Routine:   ReadOpen         : Opens a file for reading
-- Arguments: lsroFile         : File ID
-- Return:    lsroH            : File handle
-- =====================================================================
*/
ReadOpen:
  PARSE ARG lsroFile, lsroMode

  SELECT
    /* =================================================================
    -- Handle TSO
    */
    WHEN gsSystem = "TSO" THEN DO
      /* ===============================================================
      -- Ensure "DD:" is uppercase
      */
      lsroDD = Translate( Left( lsroFile, 3 ) )
      IF lsroDD = "DD:" THEN DO
        lsroFile = Overlay( lsroDD, lsroFile )
      END

      /* ===============================================================
      -- To Qualify with HLQ or Not?
      */
      IF Left( lsroFile, 1 ) <> "'" THEN DO
         IF lsroDD <> "DD:" THEN DO
            lsroFile = "'" || gsHLQ || "." || lsroFile || "'"
         END
      END

      IF lsroDD <> "DD:" THEN DO
        lsroDD = FindDD( "#T2P#" )

        "ALLOC DD(" || lsroDD || ")",
              "DA(" || lsroFile || ")",
              "SHR"

        lsroH = lsroDD
      END
      ELSE DO
        lsroDD = Substr( lsroFile, 4 )
        lsroH = lsroFile
      END

      "EXECIO 0 DISKR" lsroDD "(OPEN)"
      IF RC <> 0 THEN DO
        CALL Issue 115, lsroFile, RC
        SIGNAL Done
      END

      DROP lsroDD
    END

    /* =================================================================
    -- Handle OMVS
    */
    WHEN gsSystem = "OMVS" THEN DO
      lsroH = lsroFile
    END

    /* =================================================================
    -- Handle CMS
    */
    WHEN gsSystem = "CMS" THEN DO
      lsroH = gaCMSfh.0 + 1
      gaCMSfh.0 = lsroH
      gaCMSfh.lsroH = Translate( lsroFile, " ", "." )
    END

    /* =================================================================
    -- Handle Regina
    */
    WHEN gsSystem = "REGINA" THEN DO
      lsroH = lsroFile
      IF lsroH <> "<stdin>" THEN DO
        CALL Stream lsroH, "C", "OPEN READ"
        IF RESULT <> "READY:" THEN DO
          CALL Issue 115, lsroFile, RESULT
          SIGNAL Done
        END
      END
    END

    /* =================================================================
    -- Handle uni-REXX
    */
    WHEN gsSystem = "UNI-REXX" THEN DO
      lsroH = lsroFile
      IF lsroH <> gsNull THEN DO
        CALL Stream lsroH, "C", "OPEN"
        IF RESULT <> "READY:" THEN DO
          CALL Issue 115, lsroFile, RESULT
          SIGNAL Done
        END
      END
    END

    /* =================================================================
    -- Handle ooRexx
    */
    WHEN gsSystem = "OOREXX" THEN DO
      lsroH = lsroFile
      IF lsroH <> "STDIN" THEN DO
        CALL Stream lsroH, "C", "OPEN READ"
        IF RESULT <> "READY:" THEN DO
          CALL Issue 115, lsroFile, RESULT
          SIGNAL Done
        END
      END
    END

    OTHERWISE DO
      NOP
    END
  END

  DROP lsroFile

RETURN lsroH

/*
-- =====================================================================
-- Routine:   ReadFile         : Reads a file into a given stem
-- Arguments: lsrfH            : Handle from ReadOpen()
--            lsrfStem         : Target stem
-- Return:    (none)           : (EOF when lsrfStem0 = 0)
-- =====================================================================
*/
ReadFile:
  PARSE ARG lsrfH, lsrfStem

  /* ===================================================================
  -- Preset to 0
  */
  CALL Value lsrfStem || 0, 0

  SELECT
    /* =================================================================
    -- Handle TSO
    */
    WHEN gsSystem = "TSO" THEN DO
      lsrfDD = lsrfH
      IF Left( lsrfH, 3 ) = "DD:" THEN DO
        lsrfDD = Substr( lsrfH, 4 )
      END

      "EXECIO" gnReadC "DISKR" lsrfDD "(STEM" lsrfStem ")"
      IF RC <> 0 & RC <> 2 THEN DO
        CALL Issue 116, lsrfH, RC
        SIGNAL Done
      END

      DROP lsrfDD
    END

    /* =================================================================
    -- Handle OMVS
    */
    WHEN gsSystem = "OMVS" THEN DO
      IF gaOMVSfh.lsrfH = gsNull THEN DO
        ADDRESS SYSCALL "readfile (lsrfH)" lsrfStem
        IF RC <> 0 | RETVAL = -1 THEN DO
          CALL Issue 116, lsrfH, ERRNO
          SIGNAL Done
        END
      END

      gaOMVSfh.lsrfH = lsrfH
    END

    /* =================================================================
    -- Handle CMS
    */
    WHEN gsSystem = "CMS" THEN DO
      /* ===============================================================
      -- Input is from a CMS PIPE
      */
      IF gaCMSfh.lsrfH = gsNull THEN DO

        DO lnrfI = 1 BY 1 WHILE lnrfI < gnReadC
          "READTO" lsrfStem || lnrfI
          IF RC <> 0 THEN DO
            LEAVE
          END
        END
        CALL Value lsrfStem || 0, lnrfI - 1

        DROP lnrfI
      END
      /* ===============================================================
      -- Read from a plain file
      */
      ELSE DO
        IF gaCMSfh.lsrfH = 0 THEN DO
          DROP lsrfH lsrfStem
          RETURN
        END

        gsPipe "<" gaCMSfh.lsrfH "| UNPACK | STEM" lsrfStem

        gaCMSfh.lsrfH = 0
      END
    END

    /* =================================================================
    -- Handle Regina, uni-REXX, and ooRexx
    --
    -- Note:  As of 3.2.0 of ooRexx, the Line*() functions do
    --        not work when working with stdin on Mac OS X.
    --
    --        As of 11/06/2008, a build of current development
    --        code (trunk) is even worse.
    */
    WHEN Wordpos( gsSystem, "REGINA UNI-REXX OOREXX" ) > 0 THEN DO
      DO lnrfI = 1 BY 1 WHILE lnrfI < gnReadC &,
                              Lines( lsrfH ) > 0 &,
                              Stream( lsrfH, "S" ) = "READY"
        CALL Value lsrfStem || lnrfI, Linein( lsrfH )
      END

      CALL Value lsrfStem || 0, lnrfI - 1

      DROP lnrfI
    END

    OTHERWISE DO
      NOP
    END
  END

  DROP lsrfH lsrfStem
RETURN

/*
-- =====================================================================
-- Routine:   ReadClose        : Close a file
-- Arguments: lsrcH            : Handle from ReadOpen()
-- Return:    (none)           : none required
-- =====================================================================
*/
ReadClose:
  PARSE ARG lsrcH

  SELECT
    /* =================================================================
    -- Handle TSO
    */
    WHEN gsSystem = "TSO" THEN DO

      lsrcDD = lsrcH
      IF Left( lsrcH, 3 ) = "DD:" THEN DO
        lsrcDD = Substr( lsrcH, 4 )
      END

      "EXECIO 0 DISKR" lsrcDD "(FINIS)"

      IF Left( lsrcH, 3 ) <> "DD:" THEN DO
        "FREE DD(" || lsrcDD || ")"
      END
    END

    /* =================================================================
    -- Handle OMVS
    */
    WHEN gsSystem = "OMVS" THEN DO
      NOP
    END

    /* =================================================================
    -- Handle CMS
    */
    WHEN gsSystem = "CMS" THEN DO
      NOP
    END

    /* =================================================================
    -- Handle Regina
    */
    WHEN gsSystem = "REGINA" THEN DO
      IF lsrcH <> "<stdin>" THEN DO
        CALL Stream Substr( lsrcH, 2 ), "C", "CLOSE"
      END
    END

    /* =================================================================
    -- Handle uni-REXX
    */
    WHEN gsSystem = "UNI-REXX" THEN DO
      IF lsrcH <> gsNull THEN DO
        CALL Stream Substr( lsrcH, 2 ), "C", "CLOSE"
      END
    END

    /* =================================================================
    -- Handle ooRexx
    */
    WHEN gsSystem = "OOREXX" THEN DO
      IF lsrcH <> "STDIN" THEN DO
        CALL Stream Substr( lsrcH, 2 ), "C", "CLOSE"
      END
    END

    OTHERWISE DO
      NOP
    END
  END

  DROP lsrcH

RETURN

/*
-- =====================================================================
-- Routine:   WriteOpen        : Opens the output file
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
WriteOpen:

  SELECT
    /* =================================================================
    -- Handle TSO
    */
    WHEN gsSystem = "TSO" THEN DO
      gsFileO = Translate( gsFileO )

      IF Left( gsFileO, 3 ) <> "DD:" THEN DO
        gsOutH = gsFileO
        gnWriteC = 999999999

        /* =============================================================
        -- Get existing record length
        */
        IF ListDSI( gsOutH ) = 0 THEN DO
          gnMaxLrecl = SYSLRECL
          IF Left( SYSRECFM, 1 ) = "V" THEN DO
            gnMaxLrecl = gnMaxLrecl - 4
          END
        END
      END
      ELSE DO
        gsOutH = gsFileO
        gsOutDD = Substr( gsFileO, 4 )

        "EXECIO 0 DISKW" gsOutDD "(OPEN)"
        IF RC <> 0 THEN DO
          CALL Issue 117, gsFileO, RC
          SIGNAL Done
        END

        /* =============================================================
        -- Get existing record length
        */
        IF ListDSI( gsOutDD "FILE" ) = 0 THEN DO
          gnMaxLrecl = SYSLRECL
          IF Left( SYSRECFM, 1 ) = "V" THEN DO
            gnMaxLrecl = gnMaxLrecl - 4
          END
        END
      END
    END

    /* =================================================================
    -- Handle OMVS
    */
    WHEN gsSystem = "OMVS" THEN DO
      gsOutH = 1
      IF gsOutH <> "/dev/fd1" THEN DO
        ADDRESS SYSCALL "open (gsFileO)" O_WRONLY+O_CREAT+O_TRUNC 0644
        IF RC <> 0 | RETVAL = -1 THEN DO
          CALL Issue 117, gsFileO, ERRNO
          SIGNAL Done
        END

        gsOutH = RETVAL
      END
    END

    /* =================================================================
    -- Handle CMS
    */
    WHEN gsSystem = "CMS" THEN DO
      gsOutH = gsFileO
      IF gsOutH <> gsNull THEN DO
        "PIPE LITERAL | >" gsOutH
      END
    END

    /* =================================================================
    -- Handle Regina
    */
    WHEN gsSystem = "REGINA" THEN DO
      gsOutH = gsFileO
      IF gsOutH <> "<stdout>" THEN DO
        CALL Stream gsOutH, "C", "OPEN WRITE REPLACE"
        IF RESULT <> "READY:" THEN DO
          CALL Issue 117, gsFileO, RESULT
          SIGNAL Done
        END
        gnMaxLrecl = 2**24
      END
    END

    /* =================================================================
    -- Handle uni-REXX
    */
    WHEN gsSystem = "UNI-REXX" THEN DO
      gsOutH = gsFileO
      IF gsOutH <> gsNull THEN DO
        CALL Stream gsOutH, "C", "OPEN"
        IF RESULT <> "READY:" THEN DO
          CALL Issue 117, gsFileO, RESULT
          SIGNAL Done
        END
      END
    END

    /* =================================================================
    -- Handle ooRexx
    */
    WHEN gsSystem = "OOREXX" THEN DO
      gsOutH = gsFileO
      IF gsOutH <> "STDOUT" THEN DO
        CALL Stream gsOutH, "C", "OPEN WRITE REPLACE"
        IF RESULT <> "READY:" THEN DO
          CALL Issue 117, gsFileO, RESULT
          SIGNAL Done
        END
        gnMaxLrecl = 2**24
      END
    END

    OTHERWISE DO
      NOP
    END
  END

RETURN

/*
-- =====================================================================
-- Routine:   WriteFile        : Writes the output data
-- Arguments: lswfDAta         : Data to be written
-- Return:    (none)           : none required
-- =====================================================================
*/
WriteFile:
  PARSE ARG lswfData

  /* ===================================================================
  -- Get length
  */
  lnwfLen = Length( lswfData )

  /* ===================================================================
  -- Track max record length
  */
  IF lnwfLen > gnMaxRecLen THEN DO
    gnMaxRecLen = lnwfLen
    IF gnMaxRecLen > gnMaxLrecl THEN DO
      gnMaxRecLen = gnMaxLrecl
    END
  END

  /* ===================================================================
  -- Output the data in chunks
  */
  DO lnwfI = 1 TO lnwfLen BY gnMaxLrecl
    /* =================================================================
    -- Bump number of lines queued
    */
    gnOut = gnOut + 1

    /* =================================================================
    -- Calc the chunk length
    */
    lnwfL = Min( lnwfLen - lnwfI + 1, gnMaxLrecl )

    /* =================================================================
    -- Queue
    */
    gaOut.gnOut = Substr( lswfData, lnwfI, lnwfL )

    /* =================================================================
    -- Calculate new file offset
    */
    gnOffset = gnOffset + lnwfL

    /* =================================================================
    -- If requested, limit maximum output size
    */
    IF gnMaxSize <> 0 & gnOffset >= gnMaxSize THEN DO
      CALL Issue 131, gnOffset, gnMaxSize
      SIGNAL Done
    END

    /* =================================================================
    -- Write output
    */
    IF gnOut < gnWriteC THEN DO
      ITERATE
    END

    SELECT
      /* ===============================================================
      -- Handle TSO
      */
      WHEN gsSystem = "TSO" THEN DO
        IF gsOutDD <> gsNull THEN DO
          "EXECIO" gnOut "DISKW" gsOutDD "(STEM gaOut.)"
          IF RC <> 0 THEN DO
            IF RC = 1 THEN DO
              CALL Issue 121, gnMaxRecLen
            END
            ELSE DO
              CALL Issue 118, gsFileO, RC
            END
            SIGNAL Done
          END
        END
      END

      /* ===============================================================
      -- Handle OMVS
      */
      WHEN gsSystem = "OMVS" THEN DO
        DO lnwfJ = 1 TO gnOut
          lnwfL = Length( gaOut.lnwfJ )
          DO lnwfK = 1 TO lnwfL BY 4096
            lnwfS = Substr( gaOut.lnwfJ,,
                            lnwfK,,
                            Min( 4096, lnwfL - lnwfK + 1 ) )
            ADDRESS SYSCALL 'write (gsOutH) lnwfS'
          END
        END
      END

      /* ===============================================================
      -- Handle CMS
      */
      WHEN gsSystem = "CMS" THEN DO
        IF gsOutH = gsNull THEN DO
          DO lnwfJ = 1 TO gnOut
            "OUTPUT" gaOut.lnwfJ
          END
        END
        ELSE DO
          gaOut.0 = gnOut
          "PIPE STEM gaOut. | >>" gsOutH
        END
      END

      /* ===============================================================
      -- Handle Regina and uni-REXX
      */
      WHEN Wordpos( gsSystem, "REGINA UNI-REXX OOREXX" ) > 0 THEN DO
        DO lnwfJ = 1 TO gnOut
          CALL Charout gsOutH, gaOut.lnwfJ
        END
      END

      OTHERWISE DO
        NOP
      END
    END

    /* =================================================================
    -- Reset output table
    */
    gnOut = 0
  END

  DROP lswfData lnwfLen lnwfI lnwfJ lnwfL
RETURN

/*
-- =====================================================================
-- Routine:   WriteClose       : Closes the output file
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
WriteClose:

  SELECT
    /* =================================================================
    -- Handle TSO
    */
    WHEN gsSystem = "TSO" THEN DO

      IF gsOutDD = gsNull THEN DO
        gaOut.0 = gnOut
        gsOutDD = AllocOutput( gsFileO,,
                               "gaOut.",,
                               gnOffset,,
                               gnMaxRecLen )
      END

      "EXECIO" gnOut "DISKW" gsOutDD "(STEM gaOut. FINIS)"
      IF RC <> 0 THEN DO
        IF RC = 1 THEN DO
          CALL Issue 121, gnMaxRecLen
        END
        ELSE DO
          CALL Issue 119, gsFileO, RC
        END
        SIGNAL Done
      END

      IF Left( gsFileO, 3 ) <> "DD:" THEN DO
        "FREE DD(" || gsOutDD || ")"
      END

    END

    /* =================================================================
    -- Handle OMVS
    */
    WHEN gsSystem = "OMVS" THEN DO

      DO lnwcI = 1 TO gnOut
        lnwcL = Length( gaOut.lnwcI )
        DO lnwcJ = 1 TO lnwcL BY 4096
          lnwcS = Substr( gaOut.lnwcI,,
                          lnwcJ,,
                          Min( 4096, lnwcL - lnwcJ + 1 ) )
          ADDRESS SYSCALL 'write (gsOutH) lnwcS'
          if RC<>0 then do
              CALL Issue 118, gsOutH, RC
              gnRC = 118
              signal DONE;  end
          if RETVAL<> length( lnwcS ) then do
              CALL Issue 118, gsOutH, length( lnwcS )':'RETVAL
              gnRC = 118
              signal DONE;  end
        END
      END

      IF gsFileO <> "/dev/fd1" THEN DO
        ADDRESS SYSCALL "close (gsOutH)"
      END
    END

    /* =================================================================
    -- Handle CMS
    */
    WHEN gsSystem = "CMS" THEN DO
      IF gnOut <> 0 THEN DO
        IF gsEnv = "?" THEN DO
          DO lnwcI = 1 TO gnOut
            "OUTPUT" gaOut.lnwcI
          END
        END
        ELSE DO
          gaOut.0 = gnOut
          "PIPE STEM gaOut. | >>" gsOutH
        END
      END
    END

    /* =================================================================
    -- Handle Regina
    */
    WHEN gsSystem = "REGINA" THEN DO
      DO lnwcI = 1 TO gnOut
        CALL Charout gsOutH, gaOut.lnwcI
      END
      IF gsOutH <> "<stdout>" THEN DO
        CALL Stream gsOutH, "C", "CLOSE"
      END
    END

    /* =================================================================
    -- Handle uni-REXX
    */
    WHEN gsSystem = "UNI-REXX" THEN DO
      DO lnwcI = 1 TO gnOut
        CALL Charout gsOutH, gaOut.lnwcI
      END
      IF gsOutH <> gsNull THEN DO
        CALL Stream gsOutH, "C", "CLOSE"
      END
    END

    /* =================================================================
    -- Handle ooRexx
    */
    WHEN gsSystem = "OOREXX" THEN DO
      DO lnwcI = 1 TO gnOut
        CALL Charout gsOutH, gaOut.lnwcI
      END
      IF gsOutH <> "STDOUT" THEN DO
        CALL Stream gsOutH, "C", "CLOSE"
      END
    END

    OTHERWISE DO
      NOP
    END
  END

RETURN

/*
-- =====================================================================
-- Routine:   DisplayMsgs      : Displays (or browses) messages
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
DisplayMsgs:

  SELECT
    /* =================================================================
    -- Handle TSO
    */
    WHEN gbBrowse & gsAddr = "ISPF" THEN DO

      /* ===============================================================
      -- Generate file name
      */
      lsbmFile = gsProducer || ".report"

      /* ===============================================================
      -- Allocate it
      */
      lsbmDD = AllocOutput( lsbmFile, "gaMsgs." )

      /* ===============================================================
      -- Write messages to it
      */
      "EXECIO" gaMsgs.0 "DISKW" lsbmDD "(STEM gaMsgs. FINIS)"
      IF RC <> 0 THEN DO
        CALL Issue 120, RC
        SIGNAL Done
      END

      /* ===============================================================
      -- Browse it
      */
      ADDRESS ISPEXEC "BROWSE DATASET(" || lsbmFile || ")"

      /* ===============================================================
      -- Free and delete it
      */
      "FREE DD(" || lsbmDD || ") DELETE"

      DROP lsbmFile lsbmDD
    END

    /* =================================================================
    -- Handle OMVS
    */
    WHEN gsSystem = "OMVS" THEN DO
      ADDRESS SYSCALL "writefile /dev/fd2 000 gaMsgs."
    END

    /* =================================================================
    -- Handle the rest
    */
    OTHERWISE DO
      DO lnbmI = 1 TO gaMsgs.0
        IF gsSystem = "CMS" & gsEnv = "?" THEN DO
          "MESSAGE" gaMsgs.lnbmI
        END
        ELSE IF gsSystem = "REGINA" THEN DO
          CALL Lineout "<stderr>", gaMsgs.lnbmI
        END
        ELSE IF gsSystem = "UNI-REXX" THEN DO
          CALL Lineout "stderr:", gaMsgs.lnbmI
        END
        ELSE IF gsSystem = "OOREXX" THEN DO
          CALL Lineout "STDERR", gaMsgs.lnbmI
        END
        ELSE DO
          SAY gaMsgs.lnbmI
        END
      END

      DROP lnbmI
    END
  END

RETURN

/*
-- =====================================================================
-- Routine:   AllocOutput      : Allocates output file on MVS
-- Arguments: lsaoFile         : File ID
--            lsaoStem         : Target stem
--            lnaoLen          : Length to write (or 0)
--            lnaoRec          : Length of records (or 0)
-- Return:    lsaoDD           : DDNAME used
-- =====================================================================
*/
AllocOutput:
  PARSE ARG lsaoFile, lsaoStem, lnaoLen, lnaoRec

  /* ===================================================================
  -- Determine maximum record length if not specified
  */
  IF lnaoRec = gsNull | lnaoRec = 0 THEN DO
    lnaoRec = 0
    lnaoCnt = Value( lsaoStem || "0" )
    DO lnaoI = 1 TO lnaoCnt
      lnaoL = Length( Value( lsaoStem || lnaoI ) )
      IF lnaoL > lnaoRec THEN DO
        lnaoRec = lnaoL
      END
    END
  END

  /* ===================================================================
  -- Get next available DD name
  */
  lsaoDD = FindDD( "#T2P#" )

  /* ===================================================================
  -- To Qualify with HLQ or Not?
  */
  IF Left( lsaoFile, 1 ) <> "'" THEN DO
     lsaoFile = "'" || gsHLQ || "." || lsaoFile || "'"
  END

  /* ===================================================================
  -- Existing dataset?
  */
  IF ListDSI( lsaoFile "RECALL" ) = 0 THEN DO

    /* =================================================================
    -- Record length big enough?
    */
    IF SYSLRECL < lnaoRec THEN DO
      CALL Issue 121, lnaoRec
      SIGNAL Done
    END

    /* =================================================================
    -- Verify DSORG/member combination
    */
    IF Pos( "(", lsaoFile ) > 0 THEN DO
      IF SYSDSORG <> "PO" THEN DO
        CALL Issue 122
        SIGNAL Done
      END
    END
    ELSE DO
      IF SYSDSORG <> "PS" THEN DO
        CALL Issue 123
        SIGNAL Done
      END
    END

    /* =================================================================
    -- Allocate the file
    */
    "ALLOC DD(" || lsaoDD || ")",
          "DA(" || lsaoFile || ")",
          "SHR"
  END
  ELSE DO

    /* =================================================================
    -- Get number of records
    */
    lnaoI = Value( lsaoStem || 0 )

    /* =================================================================
    -- Calculate number of bytes if not known
    */
    IF lnaoLen = gsNull | lnaoLen = 0 THEN DO
      lnaoLen = 0
      DO lnaoI = lnaoI TO 1 BY -1
        lnaoLen = lnaoLen + Length( Value( lsaoStem || lnaoI ) )
      END
    END

    /* =================================================================
    -- Calculate number of blocks needed (use 23476 as base)
    */
    lnaoLen = lnaoLen + ( lnaoI * 4 )
    lnaoBlks = lnaoLen % 23476
    lnaoBlks = ( ( ( lnaoBlks * 4 ) + lnaoLen ) % 23476 ) + 1

    /* =================================================================
    -- Creating a PDS?
    */
    lnaoDir = 0
    IF Pos( "(", lsaofile ) > 0 THEN DO
      lnaoDir = 1
    END

    /* =================================================================
    -- Allocate the file
    */
    "ALLOC DD(" || lsaoDD || ")",
          "DA(" || lsaoFile || ")",
          "LRECL(" || lnaoRec + 4 || ")",
          "SPACE(" || lnaoBlks || "," || lnaoBlks || ")",
          "DIR(" || lnaoDir || ")",
          "RECFM(V B)",
          "BLKSIZE(0)",
          "NEW",
          "TRacks" ,
          "CATALOG",
          "RELEASE"

    DROP lnaoBlks lnaoDir
  END

  DROP lsaoFile lsaoStem lnaoLen lnaoI lnaoRec lnaoCnt lnaoL

RETURN lsaoDD

/*
-- =====================================================================
-- Routine:   FindDD           : Finds available DDNAME
-- Arguments: lsfdPfx          : DDNAME prefix
-- Return:    lsfdDD           : available name
-- =====================================================================
*/
FindDD:
  PARSE UPPER ARG lsfdPfx

  lsfdDD = gsNull
  DO lnfdI = 1 BY 1 UNTIL lnfdRC <> 0
    lnfdRC = ListDSI( lsfdPfx || lnfdI "FILE" )
  END
  IF lnfdI <> 0 THEN DO
    lsfdDD = lsfdPfx || lnfdI
  END

  DROP lsfdPfx lnfdI lnfdRC
RETURN lsfdDD

/*
-- =====================================================================
-- Routine:   Crypt            : Encrypt give data
-- Arguments: lscData          : Data to be encrypted
-- Return:    lscData          : Encrypted data
-- =====================================================================
*/
Crypt:
  PARSE ARG lscData

  /* ===================================================================
  -- Copy and extend encryption key (3.1-2)
  */
  lsnoTemp = Left( gsCryptKey, gnCryptLen) ||,
             Reverse( Right( D2c( gnObjs ), 5, '00'x ) )

  /* ===================================================================
  -- Digest it (3.1-3)
  */
  lsnoDigest = T2Pmd5( "", Left( lsnoTemp, gnCryptLen + 5 ) )
  lsnoDigest = Left( lsnoDigest, gnCryptLenX )

  /* ===================================================================
  -- And encrypt the data (3.1-4)
  */
  lscData = T2Parc4( "lscCtx", lsnoDigest, lscData )

RETURN lscData


/*
-- =====================================================================
-- Routine:   CryptInit        : Intializes encryption state
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
CryptInit:

  /* ===================================================================
  -- Determine security handler version and revision
  */
  gnCryptVer = 1
  gnCryptRev = 2
  IF gnCryptLen > 5 THEN DO
    gnCryptVer = 2
    gnCryptRev = 3
  END

  /* ===================================================================
  -- PDF padding bytes
  */
  lsPad = '28BF4E5E4E758A4164004E56FFFA0108'x ||,
          '2E2E00B6D0683E802F0CA9FE6453697A'x

  /* ===================================================================
  -- Generate a file ID
  */
  DO lnI = 1 TO 8
    CALL T2Pmd5 "lsMD5", Random( 0, 99999 )
  END
  gsFileID = T2Pmd5( "lsMD5" )

  /* ===================================================================
  -- If no owner password, use the user password instead
  */
  IF gsOwnerPw = gsNull THEN DO
    gsOwnerPw = gsUserPw
  END

  /* ===================================================================
  -- If still no owner password, generate one
  */
  IF gsOwnerPw = gsNull THEN DO
    DO lnI = 1 TO 8
      CALL T2Pmd5 "lsMD5", Random( 0, 99999 )
    END
    gsOwnerPw = C2x( T2Pmd5( "lsMD5" ) )
    gsGennedPw = gsOwnerPw
  END

  /* ===================================================================
  -- Convert passwords to ASCII
  */
  gsOwnerPw = Translate( gsOwnerPw, gsCPCntl )
  gsUserPw = Translate( gsUserPw, gsCPCntl )

  /* ===================================================================
  -- Compute the /O(wner password) entry (3.3)                        --
  =================================================================== */

  /* ===================================================================
  -- Pad owner password (3.3-1/3.2-1)
  */
  lsOwner = Left( gsOwnerPw || lsPad, 32 )

  /* ===================================================================
  -- Hash the owner password (3.3-2)
  */
  lsDigest = T2Pmd5( "", lsOwner )

  /* ===================================================================
  -- Digest it 50 more times... (3.3-3)
  */
  IF gnCryptRev = 3 THEN DO
    DO lnI = 1 TO 50
      lsDigest = T2Pmd5( "", lsDigest )
    END
  END
  lsDigest = Left( lsDigest, gnCryptLen )

  /* ===================================================================
  -- Pad user password (3.3-5/3.2-1)
  */
  lsUser = Left( gsUserPw || lsPad, 32 )

  /* ===================================================================
  -- Create key and encrypt user password (3.3-4 & 3.3-6)
  */
  lsUser = T2Parc4( "lsA4", lsDigest, lsUser)

  /* ===================================================================
  -- Do 19 times... (3.3-7)
  */
  IF gnCryptRev = 3 THEN DO
    DO lnI = 1 TO 19
      lsKey = BitXOR( lsDigest,,
                      D2c( lnI ),,
                      D2c( lnI ) )
      lsUser = T2Parc4( "lsA4", lsKey, lsUser )
    END
  END

  /* ===================================================================
  -- Store as the owner entry (3.3-8)
  */
  gsOwnerKey = lsUser

  /* ===================================================================
  --  Compute the /U(ser password) encryption key (3.4)/(3.5)         --
  =================================================================== */

  /* ===================================================================
  -- Pad user password (3.[45]-1/3.2-1)
  */
  lsUser = Left( gsUserPw || lsPad, 32 )

  /* ===================================================================
  -- Hash the owner password (3.[45]-1/3.2-2)
  */
  CALL T2Pmd5 "lsM5", lsUser

  /* ===================================================================
  -- Pass the /O(wner) entry value (3.[45]-1/3.2-3)
  */
  CALL T2Pmd5 "lsM5", gsOwnerKey

  /* ===================================================================
  -- Pass the /P(ermission) bits (3.[45]-1/3.2-4)
  */
  CALL T2Pmd5 "lsM5", Reverse( gsPerms )

  /* ===================================================================
  -- Pass the file ID (3.[45]-1/3.2-5)
  */
  CALL T2Pmd5 "lsM5", gsFileID
  lsDigest = T2Pmd5( "lsM5" )

  /* ===================================================================
  -- Digest it 50 more times... (3.[45]-1/3.2-6)
  */
  IF gnCryptRev = 3 THEN DO
    DO lnI = 1 TO 50
      lsDigest = T2Pmd5( "", lsDigest )
    END
  END

  /* ===================================================================
  -- Final digest becomes the encryption key (3.[45]-1/3.2-7)
  */
  gsCryptKey = Left( lsDigest, gnCryptLen )

  /* ===================================================================
  -- Compute the /U(ser password) entry (3.4)                         --
  =================================================================== */

  IF gnCryptRev = 2 THEN DO

    /* =================================================================
    -- Encrypt padding string using user key (3.4-2)
    */
    gsUserKey = T2Parc4( "lsA4", gsCryptKey, lsPad )

  END

  /* ===================================================================
  -- Compute the /U(ser password) entry (3.5)                         --
  =================================================================== */

  IF gnCryptRev = 3 THEN DO

    /* =================================================================
    -- Hash the owner password (3.5-2)
    */
    CALL T2Pmd5 "lsM5", lsPad

    /* =================================================================
    -- Pass the File ID (3.5-3)
    */
    CALL T2Pmd5 "lsM5", gsFileID
    lsDigest = T2Pmd5( "lsM5" )

    /* =================================================================
    -- Encrypt digest (3.5-4)
    */
    lsDigest = T2Parc4( "lsA4", gsCryptKey, lsDigest )

    /* =================================================================
    -- Do 19 times... (3.5-5)
    */
    IF gnCryptRev = 3 THEN DO
      DO lnI = 1 TO 19
        lsKey = BitXOR( gsCryptKey,,
                        D2c( lnI ),,
                        D2c( lnI ) )
        lsDigest = T2Parc4( "lsA4", lsKey, lsDigest )
      END
    END

    /* =================================================================
    -- Final digest becomes /U(ser) entry value (3.5-6)
    */
    gsUserKey = lsDigest || lsDigest

  END

  /* ===================================================================
  -- Precalculate the extended key length for 3.1-2 algorithm.
  */
  gnCryptLenX = gnCryptLen + 5
  IF gnCryptLenX > 16 THEN DO
    gnCryptLenX = 16
  END

RETURN

/*
-- =====================================================================
-- Routine:   Dump             : Dump data in hex
-- Arguments: lsText           : Text to preceed dump
--            lsData           : Data to dump
-- Return:    (None)           : none required
-- =====================================================================
*/
Dump:
PROCEDURE ; PARSE ARG lsText, lsData

  lnLen = Length( lsData )
  lnOff = 0
  lsX = C2X( lsData )
  SAY lsText || ":"
  DO lnI = 1 TO Length( lsX ) BY 32
    SAY Right( D2X( lnOff ), 4, "0" ) || ":",
        Substr( lsX, lnI +  0, 8 ),
        Substr( lsX, lnI +  8, 8 ),
        Substr( lsX, lnI + 16, 8 ),
        Substr( lsX, lnI + 24, 8 )
    lnOff = lnOff + 16
  END

RETURN

/*
-- =====================================================================
-- Routine:   Strftime         : Poor cousin to strftime()
-- Arguments: lssfText         : Text to format
-- Return:    lssfOut          : Formatted text
-- =====================================================================
*/
Strftime:
  PARSE ARG lssfText

  lssfOut = ""
  DO FOREVER

    /* =================================================================
    -- Look for start of sequence
    */
    PARSE VAR lssfText lssfLeft "%" +0 lssfC1 +1 lssfC2,
              +1 lssfText

    /* =================================================================
    -- Append anything to the left of it
    */
    lssfOut = lssfOut || lssfLeft

    /* =================================================================
    -- We're done if none found
    */
    IF lssfC1 = "" THEN DO
      LEAVE
    END

    /* =================================================================
    -- Handle field specifier
    */
    SELECT
      /* ===============================================================
      -- Ignore start of sequence?
      */
      WHEN lssfC1 = lssfC2 THEN DO
        lssfOut = lssfOut || lssfC2
      END

      /* ===============================================================
      -- Emit abbreviated weekday name
      */
      WHEN lssfC2 = "a" THEN DO
        lssfOut = lssfOut || Left( Date( "W" ), 3 )
      END

      /* ===============================================================
      -- Emit weekday name
      */
      WHEN lssfC2 = "A" THEN DO
        lssfOut = lssfOut || Date( "W" )
      END

      /* ===============================================================
      -- Emit abbreviated month name
      */
      WHEN lssfC2 = "b" THEN DO
        lssfOut = lssfOut || Left( Date( "M" ), 3 )
      END

      /* ===============================================================
      -- Emit month name
      */
      WHEN lssfC2 = "B" THEN DO
        lssfOut = lssfOut || Date( "M" )
      END

      /* ===============================================================
      -- Emit date in format "Sun Oct 26 02:00:00 2003"
      */
      WHEN lssfC2 = "c" THEN DO
        PARSE VALUE Date( "N" ) WITH lssfD lssfM lssfY

        lssfOut = lssfOut || Left( Date( "W" ), 3 ),
                             lssfM,
                             Right( lssfD, 2, "0" ),
                             Time( "N" ),
                             lssfY

        DROP lssfD lssfM lssfY
      END

      /* ===============================================================
      -- Emit 2 digit day of month (zero padded)
      */
      WHEN lssfC2 = "d" THEN DO
        lssfOut = lssfOut || Right( Word( Date( "N" ), 1 ),,
                  2, "0" )
      END

      /* ===============================================================
      -- Emit 2 digit day of month (space padded)
      */
      WHEN lssfC2 = "e" THEN DO
        lssfOut = lssfOut || Right( Word( Date( "N" ), 1 ), 2 )
      END

      /* ===============================================================
      -- Emit 2 digit (24) hour
      */
      WHEN lssfC2 = "H" THEN DO
        lssfOut = lssfOut || Right( Time( "H" ), 2, "0" )
      END

      /* ===============================================================
      -- Emit 2 digit (12) hour
      */
      WHEN lssfC2 = "I" THEN DO
        PARSE VALUE Time( "C" ) WITH lssfH ":" .

        lssfOut = lssfOut || Right( lssfH, 2, "0" )

        DROP lssfH
      END

      /* ===============================================================
      -- Emit 3 digit day of year
      */
      WHEN lssfC2 = "j" THEN DO
        lssfOut = lssfOut || Right( Date( "D" ), 3 )
      END

      /* ===============================================================
      -- Emit 2 digit day of month
      */
      WHEN lssfC2 = "m" THEN DO
        lssfOut = lssfOut || Left( Date( "U" ), 2 )
      END

      /* ===============================================================
      -- Emit 2 digit minute
      */
      WHEN lssfC2 = "M" THEN DO
        lssfOut = lssfOut || Substr( Time( "N" ), 4, 2 )
      END

      /* ===============================================================
      -- Emit meridian
      */
      WHEN lssfC2 = "p" THEN DO
        lssfOut = lssfOut || Translate( Right( Time( "C" ), 2 ))
      END

      /* ===============================================================
      -- Emit 2 digit second
      */
      WHEN lssfC2 = "S" THEN DO
        lssfOut = lssfOut || Substr( Time( "N" ), 7, 2 )
      END

      /* ===============================================================
      -- Emit 1 digit day of the week (0=Sunday)
      */
      WHEN lssfC2 = "w" THEN DO
        lssfOut = lssfOut || ( Date( "B" ) + 1 ) // 7
      END

      /* ===============================================================
      -- Emit date in format MM/DD/YY
      */
      WHEN lssfC2 = "x" THEN DO
        lssfOut = lssfOut || Date( "U" )
      END

      /* ===============================================================
      -- Emit time in format HH:MM:SS
      */
      WHEN lssfC2 = "X" THEN DO
        lssfOut = lssfOut || Time( "N" )
      END

      /* ===============================================================
      -- Emit 2 digit year
      */
      WHEN lssfC2 = "y" THEN DO
        lssfOut = lssfOut || Right( Date( "N" ), 2 )
      END

      /* ===============================================================
      -- Emit 4 digit year
      */
      WHEN lssfC2 = "Y" THEN DO
        lssfOut = lssfOut || Right( Date( "N" ), 4 )
      END

      /* ===============================================================
      -- Unrecognized...ignore it
      */
      OTHERWISE DO
        lssfOut = lssfOut || lssfC1 || lssfC2
      END
    END
  END

RETURN lssfOut

/*
-- =====================================================================
-- Routine:   Strfdyn          : Format dynamic data
-- Arguments: lssfText         : Text to format
-- Return:    lssfOut          : Formatted text
-- =====================================================================
*/
Strfdyn:
  PARSE ARG lssfText

  lssfOut = ""
  DO FOREVER

    /* =================================================================
    -- Look for start of sequence
    */
    PARSE VAR lssfText lssfLeft "@" +0 lssfC1 +1 lssfC2,
              +1 lssfText

    /* =================================================================
    -- Append anything to the left of it
    */
    lssfOut = lssfOut || lssfLeft

    /* =================================================================
    -- We're done if none found
    */
    IF lssfC1 = "" THEN DO
      LEAVE
    END

    /* =================================================================
    -- Ignore start of sequence?
    */
    IF lssfC1 = lssfC2 THEN DO
      lssfOut = lssfOut || lssfC2
      ITERATE
    END

    /* =================================================================
    -- Handle field width specifier
    -- (Silently ignored for formats that don't support it)
    */
    lbtfZ = 0
    lnsfW = 0
    IF Datatype( lssfC2, "W" ) THEN DO
      lssfText = lssfC2 || lssfText
      lbtfZ = ( lssfC2 = "0" )
      lnsfP = Verify( lssfText, "0123456789" )

      /* ===============================================================
      -- Rest of text is numeric...not a valid specification
      */
      IF lnsfP = 0 THEN DO
        lssfOut = lssfOut || lssfC1 || lssfC2 || lssfText
        lssfText = ""
        ITERATE
      END

      /* ===============================================================
      -- Get the width (includes zero pad, but doesn't matter)
      */
      PARSE VAR lssfText lnsfW =(lnsfP) lssfC2 +1 lssfText

      /* ===============================================================
      -- Width is 0...not a valid specification
      */
      IF lnsfW = 0 THEN DO
        lssfOut = lssfOut || lssfC1 || lnsfW || lssfC2
        ITERATE
      END
    END

    /* =================================================================
    -- Handle field specifier
    */
    SELECT
      /* ===============================================================
      -- Emit the current page number
      */
      WHEN lssfC2 = "p" THEN DO
        IF lnsfW > 0 THEN DO
          IF lbtfZ THEN DO
            lssfOut = lssfOut ||,
                      Right( gnContentObjs, lnsfW, "0" )
          END
          ELSE DO
            lssfOut = lssfOut || Right( gnContentObjs, lnsfW )
          END
        END
        ELSE DO
          lssfOut = lssfOut || gnContentObjs
        END
      END

      /* ===============================================================
      -- Unrecognized...ignore it
      */
      OTHERWISE DO
        lssfOut = lssfOut || lssfC1 || lssfC2
      END
    END
  END

RETURN lssfOut

/*
-- =====================================================================
-- Routine:   Issue            : Issue a message
-- Arguments: lsiMsgN          : 3 character message id
--            ...              : Any args required by message
-- Return:    (None)           : none required
-- =====================================================================
*/
Issue:

  /* ===================================================================
  -- Locate the message table if we haven't already done so
  */
  IF gbMsg = gbFalse THEN DO
    CALL DefineMessageTable
    gbMsg = gbTrue
  END

  /* ===================================================================
  -- Get the message number for which to issue
  */
  lsiN = Right( Arg( 1 ), 3, "0" )

  /* ===================================================================
  -- Extract message level
  */
  IF gaMsgTOver.lsiN <> gsNull THEN DO
    lsiL = gaMsgTOver.lsiN
  END
  ELSE DO
    lsiL = Word( gaMsg.lsiN, 1 )
  END

  /* ===================================================================
  -- Now, get the text
  */
  lsiT = Subword( gaMsg.lsiN, 2 )

  /* ===================================================================
  -- Set RC if it's an error level message
  */
  IF lsiL = "E" THEN DO
    gnRC = 8
  END

  /* ===================================================================
  -- Bypass if message level lower than requested
  */
  IF Wordpos( lsiL, "N E I V D" ) - 1 > gnConfirm THEN DO
    RETURN
  END

  /* ===================================================================
  -- Set the message ID
  */
  lsiMid = gsNull
  IF gbMsgid THEN DO
    IF gsSystem = "CMS" THEN DO
      IF gsEnv = "?" THEN DO
        lsiMid = "T2PPIP" || lsiN || lsiL || ": "
      END
      ELSE DO
        lsiMid = "T2PCMS" || lsiN || lsiL || ": "
      END
    END
    ELSE DO
      lsiMid = "T2P" || lsiN || lsiL || ": "
    END
  END

  /* ===================================================================
  -- Perform substitution
  */
  lniCnt = gaMsgs.0 + 1
  gaMsgs.lniCnt = lsiMid

  DO WHILE lsiT <> gsNull

    PARSE VAR lsiT lsiLeft "%" +1 lsiC +1 lsiT

    gaMsgs.lniCnt = gaMsgs.lniCnt || lsiLeft

    SELECT
      /* ===============================================================
      -- Substitute the numbered argument
      */
      WHEN Datatype( lsiC, "NUMBER" ) THEN DO
        gaMsgs.lniCnt = gaMsgs.lniCnt || Arg( lsiC + 1 )
      END

      /* ===============================================================
      -- Our version of a new line
      */
      WHEN lsiC = "_" THEN DO
        lniCnt = lniCnt + 1
        gaMsgs.lniCnt = lsiMid
      END

      /* ===============================================================
      -- Substitute value of named variable or stem
      */
      WHEN lsiC = "(" THEN DO
        PARSE VAR lsiT lsiV ")" lsiT

        IF Right( lsiV, 1 ) = "." THEN DO
          DO lniN = 1 TO Value( lsiV || "0" )
            gaMsgs.lniCnt = gaMsgs.lniCnt ||,
                            Value( lsiV || lniN )
            IF lniN < Value( lsiV || "0" ) THEN DO
              lniCnt = lniCnt + 1
              gaMsgs.lniCnt = lsiMid ||,
                              Copies( " ", Length( lsiLeft ) )
            END
          END
        END
        ELSE DO
          gaMsgs.lniCnt = gaMsgs.lniCnt || Value( lsiV )
        END
      END

      /* ===============================================================
      -- Call named subroutine
      */
      WHEN lsiC = "[" THEN DO
        PARSE VAR lsiT lsiV "]" lsiT

        gaMsgs.0 = lniCnt

        PUSH lsiN lsiMid lsiT
        CALL IssueCall lsiV
        PARSE PULL lsiN lsiMid lsiT

        lniCnt = gaMsgs.0
      END

      /* ===============================================================
      -- Prevents addition of "%" to end of text, but a side
      -- effect is that a "%" at the end of a message will be
      -- lost.  Oh well, too lazy to fix it.
      */
      WHEN lsiC = gsNull THEN DO
        LEAVE
      END

      /* ===============================================================
      -- Not a substitution...Add it back to the message
      */
      OTHERWISE DO
        gaMsgs.lniCnt = gaMsgs.lniCnt || "%" || lsiC
      END
    END
  END

  gaMsgs.0 = lniCnt

RETURN

/*
-- =====================================================================
-- Routine:   IssueCall        : Jumps to message routine
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
IssueCall:
  PARSE UPPER ARG lsicRtn
  SIGNAL VALUE lsicRtn

/*
-- =====================================================================
-- Routine:   SpringBoard      : Jumps to parameter validator
-- Arguments: (none)           : none required
-- Return:    (none)           : none required
-- =====================================================================
*/
DefineMessageTable:
  SIGNAL VALUE "MESSAGES_" || gsMsgLang

/* =====================================================================
-- American English - Mixed Case
*/
Messages_ENU:
  gaMsg.001 = "E %1"
  gaMsg.002 = "I %1"
  gaMsg.003 = "V %1"
  gaMsg.004 = "D %1"
  gaMsg.005 = "E Unrecognized parameter: %1%_%[Usage]"
  gaMsg.006 = "E Both IN and OUT parameters are required"
  gaMsg.007 = "I TXT2PDF - Text to PDF Conversion Utility %(gsVersion)%_" ||,
                "Copyright (C) 2000 -" Left( Date( "S" ), 4 ) "Leland Lucius"
  gaMsg.008 = "I Input File:         %1"
  gaMsg.009 = "I Output File:        %1"
  gaMsg.010 = "I Paper Width:        %1"
  gaMsg.011 = "I Paper Height:       %1"
  gaMsg.012 = "I Paper Type:         %1"
  gaMsg.013 = "I Paper Style:        %1"
  gaMsg.014 = "I Orientation:        %1"
  gaMsg.015 = "E Input file '%1' is empty"
  gaMsg.016 = "I Left Margin:        %1"
  gaMsg.017 = "I Right Margin:       %1"
  gaMsg.018 = "I Top Margin:         %1"
  gaMsg.019 = "I Bottom Margin:      %1"
  gaMsg.020 = "I Background type:    %1"
  gaMsg.021 = "I Background path:    %1"
  gaMsg.022 = "I Font name:          %1"
  gaMsg.023 = "I Font size:          %1"
  gaMsg.024 = "I Zoom percent:       %1"
  gaMsg.025 = "I Lines per inch:     %1"
  gaMsg.026 = "I Foreground color:   %1"
  gaMsg.027 = "I Background color:   %1"
  gaMsg.028 = "I Carriage Control:   %1"
  gaMsg.029 = "I Encryption:         Standard%_" || ,
                "  Length:           %1-bit%_" ||,
                "  Permissions:      %2"
  gaMsg.030 = "I Outline type:       %1"
  gaMsg.031 = "I Document info:      %1"
  gaMsg.032 = "I HLQ:                %1"
  gaMsg.033 = "V PageLeft:           %1"
  gaMsg.034 = "V PageRight:          %1"
  gaMsg.035 = "V PageTop:            %1"
  gaMsg.036 = "V PageBottom:         %1"
  gaMsg.037 = "V PageWidth:          %1"
  gaMsg.038 = "V PageHeight:         %1"
  gaMsg.039 = "V CropLeft:           %1"
  gaMsg.040 = "V CropRight:          %1"
  gaMsg.041 = "V CropTop:            %1"
  gaMsg.042 = "V CropBottom:         %1"
  gaMsg.043 = "V CropWidth:          %1"
  gaMsg.044 = "V CropHeight:         %1"
  gaMsg.045 = "V TextLeft:           %1"
  gaMsg.046 = "V TextRight:          %1"
  gaMsg.047 = "V TextTop:            %1"
  gaMsg.048 = "V TextBottom:         %1"
  gaMsg.049 = "V TextWidth:          %1"
  gaMsg.050 = "V TextHeight:         %1"
  gaMsg.051 = "V Leading:            %1"
  gaMsg.052 = "V Lines Per Page:     %1"
  gaMsg.053 = "V Bar Foreground:     %1"
  gaMsg.054 = "V Bar Background:     %1"
  gaMsg.055 = "E Paper width '%1' not numeric"
  gaMsg.056 = "E Paper height '%1' not numeric"
  gaMsg.057 = "E Paper type '%1' not one of the following:%_%2"
  gaMsg.058 = "E Paper style '%1' not one of the following:%_%2"
  gaMsg.059 = "E Orientation '%1' not one of the following:%_%2"
  gaMsg.060 = "E Font size '%1' not numeric"
  gaMsg.061 = "E Font name '%1' not one of the following:%_%[FontList]"
  gaMsg.062 = "I   Genned Owner Pwd: %1"
  gaMsg.063 = "E Text zoom '%1' not numeric"
  gaMsg.064 = "E Background type '%1' not one of the following:%_%2"
  gaMsg.065 = "E TextMark style '%1' not one of the following:%_%2"
  gaMsg.066 = "E Opacity percent '%1' not numeric"
  gaMsg.067 = "E Left margin '%1' not numeric"
  gaMsg.068 = "E Top margin '%1' not numeric"
  gaMsg.069 = "E Right margin '%1' not numeric"
  gaMsg.070 = "E Bottom margin '%1' not numeric"
  gaMsg.071 = "E Lines per inch '%1' not numeric"
  gaMsg.072 = "E Carriage control '%1' not one of the following:%_%2"
  gaMsg.073 = "E Outline type '%1' not one of the following:%_%2"
  gaMsg.074 = "E Index row '%1' not a whole number"
  gaMsg.075 = "E Index column '%1' not a whole number"
  gaMsg.076 = "E Index length '%1' not a whole number"
  gaMsg.077 = "I %_Conversion completed successfully"
  gaMsg.078 = "E Index data column '%1' not a whole number"
  gaMsg.079 = "E Index data length '%1' not a whole number"
  gaMsg.080 = "E Encryption type '%1' not one of the following:%_%2"
  gaMsg.081 = "E Encryption key length '%1' not numeric"
  gaMsg.082 = "E Encryption key length must be 40 or 128"
  gaMsg.083 = "E Encryption option '%1' not one of the following:%_%2"
  gaMsg.084 = "E Encryption only available under TSO, OMVS, and CMS"
  gaMsg.085 = "E Outline option '%1' not one of the following:%_%2"
  gaMsg.086 = "E Sort direction '%1' not 'A' or 'D'"
  gaMsg.087 = "E Outline token must not be null"
  gaMsg.088 = "E Output record length '%1' not a whole number"
  gaMsg.089 = "E Page mode '%1' not one of the following:%_%2"
  gaMsg.090 = "E Page layout '%1' not one of the following:%_%2"
  gaMsg.091 = "V Total output bytes: %1 %2%_" ||,
                "             pages: %3"
  gaMsg.092 = "E Transition '%1' not one of the following:%_%2"
  gaMsg.093 = "E Duration '%1' not numeric"
  gaMsg.094 = "E Option '%1' not valid for '%2' transition, use:%_%3"
  gaMsg.095 = "E Compression only available under TSO, OMVS, and CMS"
  gaMsg.096 = "E Compression level '%1' not numeric"
  gaMsg.097 = "E Compression level '%1' not within 0 to 9"
  gaMsg.098 = "E External font type '%1' not one of the following:%_%2"
  gaMsg.099 = "E Viewer option '%1' not one of the following:%_%2"
  gaMsg.100 = "E Annotation type '%1' not one of the following:%_%2"
  gaMsg.101 = "E Annotation page spec '%1' not one of:%_%2"
  gaMsg.102 = "E Annotation row '%1' not a whole number"
  gaMsg.103 = "E Annotation colume '%1' not a whole number"
  gaMsg.104 = "E Annotation state '%1' not one of the following:%_%2"
  gaMsg.105 = "E Confirmation '%1' not one of the following:%_%2"
  gaMsg.106 = "E Message ID '%1' not one of the following:%_%2"
  gaMsg.107 = "E Validate '%1' not one of the following:%_%2"
  gaMsg.108 = "E Browse '%1' not one of the following:%_%2"
  gaMsg.109 = "E Color '%1' not a hex RGB value (RRGGBB) or:%_%2"
  gaMsg.110 = "E Units '%1' not one of the following:%_%2"
  gaMsg.111 = "I Units:              %1"
  gaMsg.112 = "E Unrecognized environment...aborting!"
  gaMsg.112 = "E Source: %1"
  gaMsg.112 = "E Version: %2"
  gaMsg.113 = "I Config file(s):     %(gaCfgs.)"
  gaMsg.114 = "E Font name '%1' not alphanumeric"
  gaMsg.115 = "E Error opening input file '%1' - RC: %2"
  gaMsg.116 = "E Error reading '%1' - RC: %2"
  gaMsg.117 = "E Error opening output file '%1' - RC: %2"
  gaMsg.118 = "E Error writing '%1' - RC: %2"
  gaMsg.119 = "E Error closing '%1' - RC: %2"
  gaMsg.120 = "E Error writing to temporary browse file - RC: %1"
  gaMsg.121 = "E Output LRECL is too small, at least '%1' is needed"
  gaMsg.122 = "E Member specified for non-PDS"
  gaMsg.123 = "E Seq output requested for non-PS"
  gaMsg.124 = "I Compression:        %1"
  gaMsg.125 = "E Message number '%1' not a whole number"
  gaMsg.126 = "E Message level '%1' not one of the following:%_%2"
  gaMsg.127 = "E External font path is missing"
  gaMsg.128 = "E NonFullScreen option '%1' not one of:%_%2"
  gaMsg.129 = "E Font size '%1' not numeric"
  gaMsg.130 = "W Input file '%1' is empty...proceeding"
  gaMsg.131 = "E Output file size %1 exceeded maximum of %2"
  gaMsg.132 = "V Image:              %1%_" ||,
                "  Type:             %2%_" ||,
                "  Width:            %3%_" ||,
                "  Height:           %4"
  gaMsg.133 = "E Magnification '%1' not a percentage or one of:%_%2"
  gaMsg.134 = "E Image function '%1' not one of the following:%_%2"
  gaMsg.135 = "E Image name is required"
  gaMsg.136 = "E Image name '%1' already used"
  gaMsg.137 = "E Image '%1' is not a supported type"
  gaMsg.138 = "E Image name '%1' not found"
  gaMsg.139 = "E X coordinate '%1' not numeric"
  gaMsg.140 = "E Y coordinate '%1' not numeric"
  gaMsg.141 = "E Rotation '%1' not numeric"
  gaMsg.142 = "E Rotation '%1' not within -360 - 360"
  gaMsg.143 = "E X scale '%1' not numeric"
  gaMsg.144 = "E Y scale '%1' not numeric"
  gaMsg.145 = "E X skew '%1' not numeric"
  gaMsg.146 = "E Y skew '%1' not numeric"
  gaMsg.147 = "E Opacity '%1' not within 0 - 100"
  gaMsg.148 = "E Width '%1' not numeric"
  gaMsg.149 = "E Height '%1' not numeric"
  gaMsg.150 = "E Thickness '%1' not numeric"
  gaMsg.151 = "E Thickness '%1' must be greater than 0"
  gaMsg.152 = "E Cap style '%1' not one of the following:%_%2"
  gaMsg.153 = "E Dots on '%1' not numeric"
  gaMsg.154 = "E Dots off '%1' not numeric"
  gaMsg.155 = "E Join style '%1' not one of the following:%_%2"
  gaMsg.156 = "E IN parm is required when no input stream connected"
  gaMsg.157 = "E IN parm not allowed with a connected input stream"
  gaMsg.158 = "E OUT parm is required when no output stream connected"
  gaMsg.159 = "E OUT parm not allowed with a connected output stream"
  gaMsg.160 = "E TEXT argument must not be null"
  gaMsg.161 = "E Starting page '%1' not a whole number"
  gaMsg.162 = "E Successive page '%1' not a whole number"
  gaMsg.163 = "E TEXT action '%1' not one of the following:%_%2"
  gaMsg.164 = "E TEXT scan type '%1' not one of the following:%_%2"
  gaMsg.165 = "E Translation table EXEC '%1' failed to return data"
  gaMsg.166 = "E Attr row '%1' not a whole number"
  gaMsg.167 = "E Attr column '%1' not a whole number"
  gaMsg.168 = "E Attr length '%1' not a whole number"
  gaMsg.169 = "E Data translation table doesn't map ASCII characters",
                "required by TXT2PDF"
  gaMsg.170 = "E   expected 0x%1 '%2' - got 0x%3 '%4' %5"
  gaMsg.171 = "E DELIM limited to 1 character"
  gaMsg.172 = "E Encoding '%1' already defined"
  gaMsg.173 = "E Encoding file '%1' doesn't exist"
  gaMsg.174 = "E Font '%1' is already defined"
  gaMsg.175 = "E Encoding name '%1' not one of the following:%_",
             "%[EncodingList]"
  gaMsg.176 = "E Font path '%1' doesn't exist"
  gaMsg.177 = "E Encoding function '%1' not one of the following:%_%2"
  gaMsg.178 = "E Font stack underflow...this is a bug...please report"
  gaMsg.179 = "E '%1' contains unrecognized TrueType version x'%2'"
  gaMsg.180 = "E '%1' missing required x'%2' table"
  gaMsg.181 = "E Unable to locate a recognized CMAP table in '%1'"
  gaMsg.182 = "E UCM file '%1' doesn't specify SBCS class"
  gaMsg.183 = "E Line %1 in UCM file '%2' is unrecognized:%_%3"
  gaMsg.184 = "E Invalid unicode value '%1' on line %2 in UCM file '%3'"
  gaMsg.185 = "E Invalid char code '%1' on line %2 in UCM file '%3'"
  gaMsg.186 = "E Font flag '%1' not one of the following:%_%2"
  gaMsg.187 = "E Draw function '%1' not one of the following:%_%2"

RETURN

/* ************************* End Of Program ************************* */

/*
-- =====================================================================
-- Documentation and change history
-- =====================================================================
*/

/* =====================================================================
Author:

    Leland Lucius
    Internet: pdf@homerow.net

------------------------------------------------------------------------
Contributers:

    Lionel B. Dyck
    Andy W Robertson
    A. Harry Williams
    Neal E K Gooch
    Felipe Cvitanich
    Len Steele
    Alan Mingen
    Mitko Iakimov
    John Bos
    Joe Bruns
    Frank M Ramaekers
    Perry Ruiter
    Ibrahim Aykol
    Hartmut Beckmann
    Charles B. Whitaker, Jr.
    Larry Martin
    Joe Smidel

------------------------------------------------------------------------
Acknowledgements:

    The Cos(), Pi(), Sin(), Sqrt() and QSort() routines were culled
    from the "Album of Algorithms and Techniques" by Vladimir
    Zabrodsky.  If you use Rexx, you HAVE to check this out:

    The Atan() function was posted to sci.math.num-analysis in 1995
    by Peter Butler (pbutler1@ix.netcom.com).

------------------------------------------------------------------------
Variable naming conventions:

    First character      : g = Global variable
                         : l = local variable (no protection)
    Second character     : s = string
                         : n = number
                         : a = stem
                         : b = bool
    Optional characters  : Local protection identifier
    Remaining characters : Variable name starting with capital

    If the stem tail represents a variable name, it should follow
    the above conventions, but scope indicators are optional.

    The local protection id should be used when a variable needs
    to remain intact when calling other routines.  It is usually
    a combination of significant characters from the name of the
    owning routine.

------------------------------------------------------------------------
Drawing areas:

    Our page is based on 3 boxes: Page, Crop, and Text.  With the
    exception of the CropBox and MediaBox, the PDF boxes don't
    really fit our needs.  The CropBox isn't used since the PDF spec
    recommends that it be omitted or remain the same as the
    MediaBox.

      ------------------------------
      |  ------------------------  |
      |  |  ------------------  |  |
      |  |  |                |  |  |
      |  |  |                |  |  |
      |  |  |      Text      |  |  |
      |  |  ------------------  |  |
      |  |         Crop         |  |
      |  ------------------------  |
      |            Page            |
      ------------------------------

    The Page box represents the physical dimensions of the page.
    This box corresponds to the PDF MediaBox.

    The Crop box represents the logical dimensions of the page.
    Most drawing functions occur within these boundaries.  Note that
    this is not a true crop box...it does not impose any clipping.

    The Text box represnets the area where text will be drawn.  This
    is where the users margin settings are used.

    The drawing origin (0,0) is the bottom left corner of each box
    and extends upwards and to the right.

------------------------------------------------------------------------
Language specific messages:

    All messages issued by TXT2PDF can now be customized to your native
    language by placing them in a routine called "Messages_" followed by
    the language code.

    The format of an entry within the table is:

        gaMsg.III = "L TEXT"

        Where:  "III" is the message ID.  It should be padded to 3 bytes
                with leading zeros.

                "L" is the severity level of the message.  Valid values
                are E(rror), I(nfo), V(erbose), and D(ebug).  They
                correspond to the levels set with the CONFIRM
                parameter.

                "TEXT" is the text of the message.  It may contain
                anything you like.

                Substitutions may be included within the message text.
                They are introduced by specifying a percert sign (%)
                and one of the following:

                1 - 9    A single digit corresponding to the message
                         specific parameters.

                _        An underscore character forces any prior
                         text to be written and a new line started.

                (var)    A variable name enclosed within parens.  If the
                         name represents a stem, then var.0 must contain
                         the number of members within the set.

                         The variable must be defined within the EXEC
                         prior to being used in a message.

                [label]  A label enclosed within brackets representing
                         a routine within the EXEC that will be called
                         to perform any action including the issuing of
                         additional messages.

------------------------------------------------------------------------
History:

    12/02/09 - LBD - Updates from Mario Robitaille to resolve an
                     issue when using Draw_String

    05/26/09 - PG  - Updates from Paul Gilmartin to correctly work under
                     OMVS.

    04/17/09 - LLL - Added TrueType font embedding and subsetting.  The
                       XFONT keyword was updated to specify the fonts.
                     Added the DELIM keyword to allow using characters
                       other the "/" as the parameter separator.
                     Added the ENCODING keyword to provide support for
                       Unicode.
                     Added new font name parameter to the TMARK type
                       of the BG keyword.
                     Added ROWCOL subtype to the ATTR type of the TEXT
                       keyword.
                     Object lengths are no longer forward references to
                       separate objects, somewhat reducing PDF size.
                     Fixed control character initialization so that it
                       they are translated properly.
                     Made sure all SELECTs had an OTHERWISE to make the
                       REXX compiler happier.
                     More minor cleanup.

    11/31/08 - LLL - Changed license to GPLv2 from QPL.  There were
                     companies wanting to distribute TXT2PDF with their
                     product and the QPL was too restrictive.

                     Even though the GPL allows it, I hope the companies
                     do not try to make money off of TXT2PDF since I
                     was kind enough to make it easier to redistribute.
                     May a pox be on them if they do.  :-)

    11/06/08 - LLL - Added support for ooRexx (based on Lionel's code)
                     Limit parameter precision in image positioning
                       since Acrobat Reader 9 can't deal with more than
                       20 digits to the right of the decimal.
                     Fixed Regina/Uni-rexx stdin/stdout handling.
                     Converted message table to stem for easier
                       compiling. (Joe Smidel)
                     Conversion table usage was reversed.
                     Added COMPRESS to TXT2PDFD.
                     Minor cosmetic clean up here and there.

    06/09/05 - LLL - Completely redid the carriage control handling
                     to correct the botched job I did with machine
                     CCs.  I believe it's finally right, but WATCH OUT!

    03/17/04 - LLL - Fixed stdin/stdout I/O for Regina and Uni-rexx.
                     Added 1, 4, 8, and 24 bit UNCOMPRESSED BMP image
                       file support.
                     Added new TILE action to the IMAGE parameter.

    03/03/04 - LLL - Added OMVS support. (Larry)

    02/16/04 - LLL - Further suggestions from Hartmut to allow easier
                     use of TXT2PDF on "non-English" systems.

                     While this change was "minor" in complexity, it
                     did involve some global (hahaha) changes.  So, you
                     might want to do a little extra testing before
                     using in production.

                     Those changes were to replace all "\" characters
                     and make locating "|" characters easier.

    02/14/04 - LLL - Contribution from Lionel to base CC on RECFM under
                       TSO instead of forcing to NO.  This is the way
                       it SHOULD have been all along, so it's my fault
                       for the behavior change.

                     NOTE:  Since this changes prior behavior, make
                            CERTAIN you check your jobs to verify that
                            either you have specified CC or you are
                            okay with allowing TXT2PDF to decide for
                            you.

                     Added 2 arguments to the IMAGE DRAW paramenter that
                       allow you to specify the page number and interval
                       at which the image should be drawn. (Perry)
                     Added automatic page sizing based on the largest
                       IMAGE printed on the page. (Perry)
                     Added (unverified) Turkish cset support. (Ibrahim)
                     Changed the way barred and holed paper is drawn.
                       Acrobat's TABLE SELECTION TOOL doesn't work when
                       a PDF contains Form XObjects.  (Well that's what
                       I "think" the reason is.)  'Sides, the new way
                       produces smaller PDFs.  (Len)
                     Large images brought out a bug that's probably
                       been there since the 03/01/2002 update.
                       (Thanks to Perry for reporting this one!)
                     Added DEFCFG parameter to allow specification of
                       a default CONFIG file name.  This would be used
                       in your local TXT2PDFD EXEC. (Perry)
                     Added XLATE parameter to allow you to change the
                       translation tables at runtime.  This allows
                       you to override any tables set with the TXT2PDFX
                       EXEC. (Hartmut)
                     Added TEXT parameter for changing the appearance
                       of the input text. (Charles)
    10/26/03 - LLL - Fixed missing font error when using DRAW TEXT and
                       no background
                     Replace CMS file instead of appending (Frank)
                     Added MAG keyword for setting initial level (Len)
                     Added STRING type to DRAW keyword (Len)
                     Added ability to specify date/time formats in
                       TEXT type of DRAW keyword (Len)
                     Couple of minor comment and variable cleanups
    10/09/03 - LLL - Modified to support COMPRESS on the CMS platform
                     (Real credit goes to Frank!  He did all the work.)
                     New files:  T2PCOMP ASSEMBLE/MODULE
    10/08/03 - FMR - Modified to support ENCRYPT on the CMS platform
                     New files:  T2PARC4 ASSEMBLE/MODULE
                                 T2PMD5  ASSEMBLE/MODULE
                                 T2PMAC  MACLIB (support above)
    08/12/03 - LLL - Fixed Textmark (keyed GB instead of BG...GRRR!!!)
                     Added TEXT type to the new DRAW keyword.
                     OS/390 files name can be mixed case now (old bug!)
    08/10/03 - LLL - Fixed $(stem.) processing in Issue() (extra line)
                     Cleaned up use of quotes and aposts
                     Added DRAW keyword for drawing rectangles and lines
                     Added IMAGE keyword to support use of JPEG images
                     IN and OUT are now optional on platforms that have
                       stdin and stdout
                     Messages are written to stderr on platforms that
                       have it
                     Can now run as a PIPELINE stage under CMS
                     When running as a PIPE stage, messages are issued
                       using the MESSAGE subcommand
                     Minor cosmetic changes (comments, blank lines, ...)
    08/06/03 - LLL - Fixed loop when reading files under CMS
                     Added UNPACK stage to file input under CMS
    08/04/03 - LLL - Added "+" as additional continuation method in
                     config files
    07/22/03 - LBD - Fix Input file DD:
    07/07/03 - LBD - Add HLQ if needed for Input and Ouput files
    05/28/03 - LBD - Add secondary space allocation to AllocOutput
    05/05/03 - LLL - Added MAXOSIZE keyword and ability to limit size
                     of output files. (Joe Bruns)
                     Added RECALL to LISTDSI when allocating output DSN
                     to prevent an allocation error.
    04/25/03 - LLL - Reworked write routine to handle large page sizes
                     and any LRECL from existing files. (John Bos)
    03/08/03 - LLL - Added IFEMPTY to control behavour when the input
                     file is empty. (Len Steele)
    12/02/02 - LLL - Fixed the TEXTMARK background type.  It now figures
                     dimensions and offsets correctly for all page sizes
                     above the font size chosen.
                     Added TMARK background type.  It's the same as the
                     TEXTMARK type, but allows the font size to be
                     specified.  (Sorry, still can't change the font.)
    11/22/02 - LLL - Added NonFullScreen argument to Viewer keyword
                     Fixed an incorrect node count for Outlines (Len S.)
                     Fixed an incorrect offset for Outline destination
                     Fixed an incorrect line count for paging
    11/21/02 - LLL - Fixed relative column error when extracting text
                     for SCAN* outline types.  (Thanks to Len Steele)
    11/09/02 - LLL - Added rudimentary external font support
                       (Thanks to Mitko's request and testing)
                     Added ability to change message level
                     Externalized the internal translation table to
                       allow correct translation of internal program
                       text.  (Wow!  Did that make sense.  :-))
                     Suppressed initial page break for '89'x machine
                       carriage control character.  (What is the right
                       way to do machine CC?!?!?!)
    10/22/02 - LLL - Added (minimal) ASCII carriage control support
    10/21/02 - LLL - Removed erroneous TRACE statement (bad, bad dog!)
    10/07/02 - LLL - Added line continuation support to config files
                     Added comment support in config files
                     Added compression results
                     Added return code of 8 for errors
                     Added 2 more blanks to textmark to fix cropping
                     Display which config files were used
    10/06/02 - LLL - Added compression support (TSO et al again)
    09/30/02 - LLL - Added uni-REXX support
    09/27/02 - LBD - Remove 4 lines per Leland
    09/16/02 - LLL - Fully qualify the same as XMITIP
               LBD - Removed the qualify routine as unused
    09/14/02 - LBD - Do NOT fully qualify the output data set
    09/08/02 - LLL - Added UNITS keyword
                     Added TXT2PDFD defaults EXEC
                     Allow centimeters for paper size and margins
    09/06/02 - LBD - Fix undefined variable gsGennedPw
    09/05/02 - LBD - Call TXT2PDFI if No parms and under ISPF
    09/04/02 - LLL - Added specific check and message for truncated
                       output.
    09/01/02 - LLL - Relocated calls to T2PINIT and T2PTERM routines
    08/30/02 - LLL - Added calls to new T2PINIT and T2PTERM routines to
                       improve encryption performance
                     Generate owner password if neither is specified
                     Added conversion complete message
                     Change external translate table routine to TXT2PDFX
                     Allow external translate table on all platforms
                     Allow use of embedded translate table if external
                       routine not found
                     Some minor cleanup (indents and comments)
    08/27/02 - LLL - Fixed NoHiresPrint encryption permission
                     Display encryption length and permissions
                     Cleaned up some comments
    08/16/02 - LLL - Fixed text string bug (rpted by Len Steele)
    08/12/02 - LLL - Put argument handlers in alphabetical order
                     Made some doc and comment updates
                     Setup defaults for ANNOT keyword
    08/11/02 - LLL - Added BU and TD aliases to the TEXTMARK stream type
                     Added encryption support (ONLY) for TSO et al
                     (All platforms will be supported in the C version)
    08/03/02 - LLL - Added support for language specific message tables
    08/01/02 - LLL - Added SHOW OLOPT option (The Lenster)
                     Added HLQ option for Lionel
                     Fixed BROWSE (only browsing 1st line)
                     Fixed double/triple line counting problem
                     (Reported by: Sergey Makogonov)

                     Reread the manual about machine CC and removed
                     the printing of the line for immediate commands.
                     (Let me know if I'm too dense to get it...)

    07/12/02 - LLL - Added OUTREC parameter (Yep, Len)
    07/18/02 - LLL - Fixed OLTOK critter (Guess who?)
                     Added a dest to the intermediate Outline nodes
    07/12/02 - LLL - Added OUTREC parameter (Yep, Len)
    07/11/02 - LLL - Added DUPS OLOPT option (Len again)
    07/10/02 - LLL - Several critter complaints from Len Steele fixed
    07/06/02 - LLL - Wow!  Found a bug that has existed since before
                     this became a part XMITIP.  Acrobat didn't have a
                     problem with it, but the pdfinfo ute in the Xpdf
                     package pointed it out.  Checked the spec and,
                     sure enough, it was a bug.  What was it?

                     There was an extra blank at the end of the first
                     entry in the "xref" table.  (Oh, the shame!!!)

    07/05/02 - LLL - Added VONLY option
                     Added opacity argument to TEXTMARK stream
    06/30/02 - LLL - Added ANNOT option
                     Added VIEWER option
                     Added PAGE option
                     Added TRANS option
                     Added OUTLINE option
                     Added OLOPT option
                     Added OLSORT option
                     Added OLTOK option
                     Added TEXTMARK STREAM type
                     Standardized color handling
                     Added HTML colorspec capability
                     Routed IDX function through OUTLINE option
                     Changed all "<>" to "\="...why?  Felt like it. ;-)
                     Moved and updated the usage information
                     Cleaned up a few variable names

                     (PAGE and TEXTMARK additions prompted
                       by discussions with Len Steele.)

                     (OUTLINE option inspired by Neal
                       Gooch's multiple level indexing.)

    06/24/02 - LLL - Corrected document info.  Thx to:
                       Len Steele
    05/10/02 - LLL - Corrected IDX keyword usage info
    05/09/02 - NEG - Moved some initialisation to before
                       Environ check
                     Added support for IBM's Object Rexx
                     Added multiple level indexing
    04/26/02 - LLL - Last input record was ignored - fixed
    03/25/02 - LBD - Add TRACKS to output alloc
    03/12/02 - LLL - Standardized font and color handling
                     Print color names instead of values
                     Supress print of unspecified arguments
    03/04/02 - LLL - Swapped font name and size in argument
    03/01/02 - LLL - Split read/write I/O granularity
                     Forced queuing of entire file if not to
                       DD under TSO
    02/28/02 - LLL - Added I/O granularity (gnReadC controls it)
                     Rewrote the Escape() routine
                     Performance tuning
    02/19/02 - LLL - Many changes including:

                     Converted to standalone app
                     Renamed to TXT2PDF (better ideas?)
                     Fixed some bugs (and added some :-))
                     Restructured codepage translation
                     Reworked document info
                     Reworked multi-platform support
                     Reworked all variable names
                     Introduced drawing areas
                     Added indexing
                     Added color support
                     Added "barred" paper background
                     Added "holed" paper background
                     Added font selection
                     Added font zoom
                     Added bold and italic support
                     Added pregenerated backgraounds
                     Added config file support
                     Added document information support

    02/06/02 - LLL - Several fixes from Leland to fix the
                     generated PDF
    10/26/01 - LBD - fix for data with cc but not in the data.
    10/19/01 - LBD - drop some variable to save stg
    10/09/01 - LBD - correction to newpage if no cc
    09/27/01 - LBD - changed rlse to release (sigh)
    09/26/01 - LBD - add rlse to alloc
    09/25/01 - LBD - add dsorg to alloc
    08/31/01 - LBD - update for machine cc
    04/07/01 - AHW - support CMS and Regina
    04/07/01 - AHW - support Document Info Dictionary
    02/06/01 - AWR - rework escaping
    01/03/01 - LBD - add call for encoding
    11/28/00 - LBD - change parse to add spaces
    11/21/00 - LBD - fit  9-72 cols
    11/15/00 - AWR - modified for two xlate tables
    11/15/00 - AWR - modified to strip trailing blank
    11/08/00 - LBD - moved translate table to XMITIPTR
    07/13/00 - LBD - corrections
    07/12/00 - LLL - update by Leland to Machine CC
    07/12/00 - LBD - conversion completed - it works now
    06/18/00 - LBD - converted for XMITIP usage

------------------------------------------------------------------------
Usage:

    The only required parameters are IN and OUT.

    All parameters consist of a keyword followed by its argument.
    If the argument contains blanks, you must enclose the whole
    argument in double (") quotes.

    Aside from those where it makes sense, no arguments are case
    sensitive.  Filespecs and text values are notable exceptions.

    Some arguments accept multiple options and this is indicated
    using an elipse (...) in the parameter descriptions.

    Most numeric arguments can be specified with any number of
    decimal places.  Others like the column and row values must be
    specified as whole numbers.  This is not indicated below, so use
    common sense.  :-)

    Several parameters accept a filespec or colorspec.  These are
    described here:

    << FILESPEC >>

        Filespec syntax is determined by the system on which TXT2PDF
        is executing.  With the exception of TSO, TXT2PDF simply
        passes the spec to the system specific I/O routine.

        Under TSO, you may specify a partially or fully qualified
        dataset name.  A preallocated DD name may be specified by
        passing "DD:<name>" where "<name>" is the actual DD name.

    << COLORSPEC >>

        All colors can be specified using either the following
        names or you may specify the exact RGB values by using 6
        hexadecimal digits formatted as RRGGBB.

            Aqua     A     Black    Bla   Blue     Blu   Fuchsia  F
            Gray     Gra   Green    Gre   Lime     L     Maroon   M
            Navy     N     Olive    O     Purple   P     Red      R
            Silver   S     Teal     T     White    W     Yellow   Y

        Though intended for use with the barred paper style, the
        following colors may also be used:

            BlueBar        GrayBar        GreenBar
            OrangeBar      WhiteBar       YellowBar

Parameters:

    IN:
      <filespec>            Name of input file.

                            DEFAULT: none

    OUT:
      <filespec>            Name of output file

                            DEFAULT: none

    ANNOT:
     <type>                 Annotations are used to convey some sort
                            of information to the user.  Currently
                            defined types are:

                            Text T - Good old "sticky note"

                            DEFAULT: none

     TEXT:
      <page>                Specifies the page(s) upon which the
                            sticky note should be placed.  Defined
                            values are:

                            First F - Only on first page
                            Last  L - Only on last page
                            All   A - On all pages

                            DEFAULT: All

      <row>                 The row where the sticky note will be
                            placed.

                            DEFAULT: 1

      <col>                 The column where the sticky note will be
                            placed.

                            This is only an approximation since
                            TXT2PDF doesn't have access to the font
                            metrics.  Therefore, it assumes a fixed
                            pitch, 600dpi font for calculating the
                            column location.

                            DEFAULT: 1

      <state>               The initial display state of the sticky
                            note.  Defined values are:

                            Open   O - Sticky note is open
                            Closed C - Sticky note is closed

                            DEFAULT: Closed

      <text>                The text of the sticky note.

                            DEFAULT: none

    BG:
      <type>                A background will be drawn before the
                            text of the document and you may specify
                            as many backgrounds as you like.  They
                            will be placed in the order you specify.

                            Currently defined types are:

                            Stream   - PDF stream content
                            Textmark - Simulated "watermark"

                            DEFAULT: none

      STREAM:
        <filespec>          Name of file whose contents will be
                            embedded into a PDF XObject object.
                            This file can contain any PDF content
                            that is valid between the "stream" and
                            "endsream" keywords.  The graphic state
                            will be saved before the content and
                            restored after.

                            The stream content may use any of the
                            fonts supported by TXT2PDF.  Just use
                            the abbreviated name, e.g., CB for
                            Courier-Bold.

                            DEFAULT: none

      TEXTMARK:
      TMARK:
        <style>             A TEXTMARK is similar to a watermark.
                            The text will be drawn according to the
                            following styles:

                            TopDown  BU - upper left to bottom right
                            BottomUp TD - bottom left to upper right

                            DEFAULT: BottomUp

        <stroke colorspec>  The text will be drawn with this color
                            in an outline fashion.  If you do not
                            specify the fill colorspec, then the
                            text will appear hollow.

                            DEFAULT: none

        <fill colorspec>    Specifying a fill color will cause the
                            text to appear solid.  If you specify a
                            color different from the stroke color
                            you will get solid, outlined text.

                            DEFAULT: Gray (only if neither color
                                          is specified)

        <opacity percent>   Specifies the opaqueness of the test.
                            A value of 100 produces solid text and
                            a value of 0 produces transparent text.

                            DEFAULT: 100

        <font name>         Font defined via XFONT parameter or one of
                            the following builtin PDF font names:

                            Times               T
                            TimesBold           TB
                            TimesItalic         TI
                            TimesBoldItalic     TBI
                            Helvetica           H
                            HelveticaBold       HB
                            HelveticaItalic     HI
                            HelveticaBoldItalic HBI
                            Courier             C
                            CourierBold         CB
                            CourierItalic       CI
                            CourierBoldItalic   CBI
                            Symbol              S
                            ZapfDingbats        Z

                            DEFAULT: Courier

        <font size>         (TMARK ONLY) Set the size of the font
                            in points.

                            DEFAULT: 100

        <text>              The text to draw.  A blank character is
                            added to each end of the text to keep
                            the tops and bottoms of the characters
                            from being clipped.  This happens due
                            to the rotation of the text.

                            DEFAULT: none

    BM:
      <bottom margin>       Offset in inches from bottom of page
                            where the text will end.

                            DEFAULT: 0.5"

    BROWSE:
     <browse messages>      If running under ISPF, you may view any
                            messages issued by the program in a
                            Browse session.  In other environments,
                            the messages are simply typed to the
                            screen.  Valid values are:

                            Yes Y - Browse messages
                            No  N - Don't browse

                            DEFAULT: No

    CC:
      <carriage control>    Indicates if input file contains machine,
                            ASA, or ASCII carriage control characters.
                            Valid values are:

                            Yes   Y - First column contains machine or
                                      ASA CC characters
                            No    N - No (or ignore) CC characters
                            Ascii A - File contains ASCII CC characters

                            DEFAULT: No

    COLOR:
      <fore colorspec>      Foreground (text) color.

                            DEFAULT: Black

      <back colorspec>      Background (page) color.

                            DEFAULT: White

    COMPRESS:
      <level>               The level of compression desired.  Specify
                            0 to turn compression off or 1 through 9
                            for fastest to best compression.

                            DEFAULT: 0

    CONFIG:
      <filespec>            Name of file containing additional
                            parameters.  Everything in this file
                            will be processed as if it had been
                            included as arguments immediately
                            following the CONFIG argument.  Any
                            arguments specified on the command line
                            after the CONFIG argument will override
                            those from the CONFIG file.

                            Full line comments are supported by
                            placing an asterisk in column 1.

                            To continue a line in the CONFIG file,
                            add a space followed by a hyphen to the
                            end of the line being continued.  Here's
                            an example:

                            line 1 contents that are cont -
                            inued on another line

                            DEFAULT: none

    CONFIRM:
     <level>                Sets level of verbosity for message
                            display.  Valid values are:

                            Yes     Y - Issue standard messages
                            No      N - Be quiet
                            Verbose V - More messages

                            DEFAULT: Yes

    DEFCFG:
     <def. cfg filespec>    Name of a default config file as used by
                            the CONFIG parameter.  This only sets the
                            default name.

                            If a value is set and no command line
                            parameters are specifed, the file will be
                            treated as if it had been specified using
                            the CONFIG parameter.

                            You would usually use this in your local
                            TXT2PDFD EXEC if you want this ability.

                            DEFAULT: none

    DELIM:
     <character>            Allows you to change the delimiter used
                            between arguments.

                            DEFAULT: / (slash)

    DINFO:
     <doc. info filespec>   Name of file whose contents include
                            lines with a one name and value pair per
                            line.  The pairs will be added to the
                            Document Information dictionary within
                            the PDF file.  You may include any name,
                            but the standard PDF names directly
                            supported by TXT2PDF are:

                            Title    - The document's title
                            Subject  - The subject of the document
                            Author   - Who created the document
                            Keywords - Associated keywords

                            DEFAULT: none

    DRAW:
      <type>                The type of object to draw.

                            Currently defined types are:

                            Line   - Line
                            Rect   - Rectangle
                            Text   - Text
                            String - String

                            DEFAULT: none

                            NOTE:  To provide the most flexibility, all
                                   coordinates and sizes are specified
                                   in user space units which are 1/72 of
                                   an inch.

                                   The origin is the lower left corner
                                   of the page and margins are ignored.

      LINE:
        <start x>           The lower left x-axis cooridinate.

                            DEFAULT: none

        <start y>           The lower left y-axis cooridinate.

                            DEFAULT: none

        <end x>             The upper right x-axis cooridinate.

                            DEFAULT: none

        <end y>             The upper right y-axis cooridinate.

                            DEFAULT: none

        <opacity percent>   Specifies the opaqueness of the line.
                            A value of 100 produces an opaque line and
                            a value of 0 produces a transparent line.

                            DEFAULT: 100

        <colorspec>         The line will be drawn with this color.

                            DEFAULT: Black

        <thickness>         The thickness of the line.  Note that 0 is
                            a valid thickness and produces (in Acrobat
                            anyway) a very thin line.

                            DEFAULT: 0

        <cap style>         The type of endcaps that will be placed at
                            the beginning and end of the line.

                            The following types are available:

                            BUTT   - Square, no projection past end
                            ROUND  - Round, projection past end
                            SQUARE - Square, projection past end

                            DEFAULT: BUTT

        <dot on>            These 2 values work together to create lines
        <dot off>           with different dash patterns.  Used with the
                            ROUND cap style, you can also achieve dotted
                            lines.

                            The "dot on" value specifies the number of
                            units dashes within the pattern will be.  If
                            you do not specify "dot on", then a value of
                            0 will be used.

                            The "dot off" value specifies the number of
                            units the gap between dashes will be.  If
                            you do not specify a "dot off" value, it
                            will default to the "dot on" value.  This
                            produces an even "dash/gap" pattern.

                            If neither value is specifed, a solid line
                            will be produced.

                            DEFAULT: none (solid line)

      RECT:
        <x>                 The lower left x-axis cooridinate.

                            DEFAULT: none

        <y>                 The lower left y-axis cooridinate.

                            DEFAULT: none

        <width>             The width of the rectangle.

                            DEFAULT: none

        <height>            The height of the rectangle.

                            DEFAULT: none

        <opacity percent>   Specifies the opaqueness of the rectangle.
                            A value of 100 produces an opaque line and
                            a value of 0 produces a transparent line.

                            DEFAULT: 100

        <stroke colorspec>  The rectangle will be drawn with this color
                            in an outline fashion.  If you do not
                            specify the fill colorspec, then the
                            rectangle will appear hollow.

                            DEFAULT: Black

        <fill colorspec>    Specifying a fill color will cause the
                            rectangle to appear solid.  If you specify a
                            color different from the stroke color
                            you will get a solid, outlined rectangle.

                            DEFAULT: none

        <thickness>         The thickness of the line surrounding the
                            rectangle.

                            DEFAULT: 0

        <join style>        The shape used for the corners of the
                            rectangle.

                            The following types are available:

                            MITER  - Pointed
                            ROUND  - Rounded
                            BEVEL  - Chopped

                            DEFAULT: MITER

        <dot on>            These 2 values work together to create lines
        <dot off>           with different dash patterns.

                            The "dot on" value specifies the number of
                            units dashes within the pattern will be.  If
                            you do not specify "dot on", then a value of
                            0 will be used.

                            The "dot off" value specifies the number of
                            units the gap between dashes will be.  If
                            you do not specify a "dot off" value, it
                            will default to the "dot on" value.  This
                            produces an even "dash/gap" pattern.

                            If neither value is specifed, a solid line
                            will be produced.

                            DEFAULT: none (solid line)

      TEXT:
      STRING:
        <x>                 The lower left x-axis cooridinate.

                            DEFAULT: none

        <y>                 The lower left y-axis cooridinate.

                            DEFAULT: none

        <font name>         Font defined via XFONT parameter or one of
                            the following builtin PDF font names:

                            Times               T
                            TimesBold           TB
                            TimesItalic         TI
                            TimesBoldItalic     TBI
                            Helvetica           H
                            HelveticaBold       HB
                            HelveticaItalic     HI
                            HelveticaBoldItalic HBI
                            Courier             C
                            CourierBold         CB
                            CourierItalic       CI
                            CourierBoldItalic   CBI
                            Symbol              S
                            ZapfDingbats        Z

                            DEFAULT: Courier

        <font size>         Set the size of the font in points.

                            DEFAULT: 9

        <opacity percent>   Specifies the opaqueness of the text.  A
                            value of 100 produces opaque text and
                            a value of 0 produces transparent text.

                            DEFAULT: 100

        <stroke colorspec>  The text will be drawn with this color in an
                            outline fashion.  If you do not specify the
                            fill colorspec, then the text will appear
                            hollow.

                            DEFAULT: Black

        <fill colorspec>    Specifying a fill color will cause the
                            text to appear solid.  If you specify a
                            color different from the stroke color
                            you will get solid, outlined text.

                            DEFAULT: none

        <thickness>         The thickness of the line surrounding the
                            text.

                            DEFAULT: 0

        <join style>        The shape used for the corners of the
                            glyphs.

                            The following types are available:

                            MITER  - Pointed
                            ROUND  - Rounded
                            BEVEL  - Chopped

                            DEFAULT: MITER

        <rotation>          Specifies the rotation of the text.  The
                            value is given in degrees.  Negative values
                            rotate the image clockwise.

                            DEFAULT: 0

        <zoom>              Zoom factor in percent of original font
                            size.  Specifying a percentage less than
                            100 will cause the font to shrink and a
                            percentage greater than 100 will cause
                            the font to enlarge.

                            DEFAULT: 100%

        <text>              The text to draw.

                            The value may contain certain format
                            sequences that get replaced with different
                            values.  These sequences fall into 2
                            categories, date/time and dynamic.

                            Date and time sequences can be used in
                            both TEXT and STRING types.  The date and
                            time used is that at time of parsing.

                            The sequences start with a percent (%) and
                            are followed by one of:

                            % - literal percent
                            a - abbreviated weekday name
                            A - full weekday name
                            b - abbreviated month name
                            B - full month name
                            c - format: Sun Oct 26 02:00:00 2003
                            d - 2 digit day of month (zero padded)
                            e - 2 digit day of month (space padded)
                            H - 2 digit (24) hour
                            I - 2 digit (12) hour
                            j - 3 digit day of year
                            m - 2 digit month of year
                            M - 2 digit minute
                            p - meridian
                            S - 2 digit second
                            w - 1 digit day of the week (0=Sunday)
                            x - date format: 10/26/03
                            X - time format: 02:00:00
                            y - 2 digit year
                            Y - 4 digit year

                            Dynamic sequences can only be used with
                            the STRING type.  The values for these
                            sequences change during processing.

                            The sequences start with an at sign (@)
                            and are followed by one of:

                            @ - literal at sign
                            p - PDF page number (1 based)

                            DEFAULT: none

    ENCODING:
                            The ENCODING parameter is used to define a
                            character set encoding and is an alternative
                            to using the translation tables.

      <func>                The function to perform.

                            Functions are:

                            Define - Define a new encoding

                            DEFAULT: None

      DEFINE:
                            Defines an encoding, assigning a name and
                            filespec.

        <name>              The name to assign to the encoding.

                            DEFAULT: none

        <filespec>          Name of the file containing the UCM format
                            encoding.

                            DEFAULT: none

    ENCRYPT:
      <type>                The type of security to use for encryption.
                            Only the PDF standard security is currently
                            supported.

                            Currently defined types are:

                            Standard ST - Standard PDF security

                            DEFAULT: Standard

      STANDARD:
        <Owner password>    The password you wish to use to control
                            full access to the document.  Having
                            owner access gives complete control.

                            Per the PDF spec, only the first 32
                            characters are used.

                            The default is not to assign an owner
                            password which gives full control to
                            anyone with user access.

                            DEFAULT: none

        <User password>     This password gives users access to the
                            document according to permissions you
                            specify (or let default).

                            Per the PDF spec, only the first 32
                            characters are used.

                            The default is not to assign a user
                            password which gives a user almost
                            all permissions.  It does not allow
                            to change the security settings if
                            an owner password has been assigned.

                            DEFAULT: none

        <key length>        The key length controls the level of
                            encryption desired.  The only supported
                            levels are:

                            40  - 40-bit (Acrobat v4 and below)
                            128 - 128-bit (Acrobat v5 and above)

                            DEFAULT: 40

        <permissions/...>   By default, all permissions ara allowed,
                            You use as many of the following options
                            separated by the "/" characters as is
                            needed to remove the indicated permission.

                            The following are currently defined with a
                            brief description of each.  (The PDF spec
                            dedicates an entire page describing these,
                            so this is by far not complete.)

                            NOPRINT       NP  - No printing
                            NOEDIT        NE  - No editing
                            NOCOPY        NC  - No copying
                            NOEDITNOTES   NEN - No editing notes/forms
                            NOSAVEAS      NSA - Don't allow Save As
                            NOFILLANDSIGN NFS - No fill/sign forms
                            NOTACCESSIBLE NAC - Not accessible (BAD!)
                            NOASSEMBLY    NAS - No insert/rotate/...
                            NOHIRESPRINT  NHP - No Hires printing
                            NONE          N   - No permissions

                            DEFAULT: (all permissions)

    FONT:
      <size>                Size of font in points.  1 point is 1/72
                            of an inch.

                            DEFAULT: 9

      <name>                Font defined via XFONT parameter or one of
                            the following builtin PDF font names:

                            Times               T
                            TimesBold           TB
                            TimesItalic         TI
                            TimesBoldItalic     TBI
                            Helvetica           H
                            HelveticaBold       HB
                            HelveticaItalic     HI
                            HelveticaBoldItalic HBI
                            Courier             C
                            CourierBold         CB
                            CourierItalic       CI
                            CourierBoldItalic   CBI
                            Symbol              S
                            ZapfDingbats        Z

                            DEFAULT: Courier

      <zoom>                Zoom factor in percent of original font
                            size.  Specifying a percentage less than
                            100 will cause the font to shrink and a
                            percentage greater than 100 will cause
                            the font to enlarge.

                            DEFAULT: 100

    HLQ:
      <hlq>                 High level qualifier for OS/390+ dataset
                            names.  It will be prepended to any DSN
                            not beginning with a single quote (') or
                            the special "DD:" syntax.

                            Under TSO, the default value will be the
                            prefix from the current profile.  If one
                            is not assigned, then the current userid
                            will be used.  If the current prefix and
                            userid are both assigned AND they aren't
                            the same, both will be used separated by
                            a period.

                            Under IRXJCL, the default value will be
                            the current userid if available.

                            For all other environments, the default
                            will be null.

                            DEFAULT: <see above>

    IFEMPTY:
      <option>              Supports different handling in the event
                            an empty input file is detected.

                            Specifying "ERROR" will cause a error
                            level message to be issued.

                            Specifying "BLANK" will issue a warning
                            message and create a 1 page, blank PDF
                            file.

                            Specifying anything else will cause a
                            warning to be issued and a 1 page PDF
                            with the text you specify as the argument.

                            DEFAULT: ERROR

    IMAGE:
                            The IMAGE keyword allows you to include
                            different images on the page.  Currently,
                            only JPEG and BMP images are supported.

      <func>                The function to perform on an image.

                            Functions are:

                            Load - Load the image and assign a name
                            Draw - Draw a previously loaded image
                            Tile - Tile a previously loaded image

                            DEFAULT: none

                            NOTE:  To provide the most flexibility, all
                                   coordinates and sizes are specified
                                   in user space units which are 1/72 of
                                   an inch.

                                   The origin is the lower left corner
                                   of the page and margins are ignored.

      LOAD:
                            Loads an image and assigns it a name for
                            use by the other IMAGE functions.  You may
                            load as many files as you like.

        <name>              The name to assign to the image.

                            DEFAULT: none

        <filespec>          Name of the file containing the image.

                            DEFAULT: none

      DRAW:
                            Draws a previously loaded image.  You may
                            draw an image as many times as is needed
                            without reloading.

                            NOTE:  The order of operations on the
                                   image is rotate, scale, and skew.

        <name>              The name assigned with on the LOAD of the
                            image.

                            DEFAULT: none

        <x>                 The lower left x-axis cooridinate.

                            DEFAULT: none

        <y>                 The lower left y-axis cooridinate.

                            DEFAULT: none

        <opacity percent>   Specifies the opaqueness of the image.  A
                            value of 100 produces an opaque image and a
                            value of 0 produces a transparent image.

                            DEFAULT: 100%

        <rotation>          Specifies the rotation of the image.  The
                            value is given in degrees.  Negative values
                            rotate the image clockwise.

                            DEFAULT: 0

        <x scale>           Specifies the scaling factor of the images'
                            x-axis.  The value is a percentage of the
                            original width of the image.

                            DEFAULT: 100% (original size)

        <y scale>           Specifies the scaling factor of the images'
                            y-axis.  The value is a percentage of the
                            original height of the image.

                            DEFAULT: 100% (original size)

        <x skew>            Specifies the skew or slant of the image
                            along the x-axis.  It is given in degrees.

                            DEFAULT: 0

        <y skew>            Specifies the skew or slant of the image
                            along the y-axis.  It is given in degrees.

                            DEFAULT: 0

        <start>             Specifies the first page number on which
                            the image will be drawn.

                            DEFAULT: 1

        <interval>          Specifies the page interval at which the
                            image will be drawn.

                            Use a value of 0 to draw the image only
                            on the <start> page and not any others.

                            DEFAULT: 1

      TILE:
                            Tiles a previously loaded image within
                            the specified constraints.

        <name>              The name assigned with on the LOAD of the
                            image.

                            DEFAULT: none

        <x>                 The lower left x-axis cooridinate.

                            DEFAULT: none

        <y>                 The lower left y-axis cooridinate.

                            DEFAULT: none

        <w>                 The width of the constraining box.

                            DEFAULT: none

        <h>                 The height of the constraining box.

                            DEFAULT: none

        <opacity percent>   Specifies the opaqueness of the image.  A
                            value of 100 produces an opaque image and a
                            value of 0 produces a transparent image.

                            DEFAULT: 100%

    LM:
      <left margin>         Offset in inches from left side of page
                            where the text will start.

                            DEFAULT: 0.5"

    LPI:
      <lines per inch>      Number of lines per inch.

                            DEFAULT: 8

    MAG:
     <level>                Sets the initial magnification level.

                            You may specify a percentage or one of
                            the following values:

                            FitInWindow FI - Fit entirely in window
                            FitWidth    FW - Fit to width of page
                            FitVisible  FV - Fit to contents of page

                            DEFAULT: none

    MAXOSIZE:
      <max output size>     Allows setting a limit on the size of the
                            generated PDF.  If the size is greater than
                            the value specified, an error is issued.

                            Setting the value to 0 will disable this
                            feature.

                            DEFAULT: 0

    MSGID:
     <prefix messages>      Enables/disables prefixing of all
                            messages with program name.  Valid
                            values are:

                            Yes Y - Prefix each message
                            No  N - Don't prefix the messages

                            DEFAULT: Yes

    MSGLVL:
     <msgnum/level>         Overrides internal level of a message.

                            This allows you to suppress a message by
                            making the message a lower level than
                            the current "CONFIRM" value.

                            You may also affect the final return code
                            by changing an error level message to one
                            of the other values.

                            Possible values are:

                            Error   E - Error level (produces RC=8)
                            Info    I - Info level
                            Verbose V - Verbose level

                            DEFAULT: none

    OLOPT:
      <opt/...>             Allows setting of options that relate to
                            all outlining types.

                            Specify as many of the following options
                            as you like, separated with the "/"
                            character:

                            Color    C - Primary outline text color
                                         (Next option must be a valid
                                          colorspec)
                            AltColor A - Alternate text color
                                         (Next option must be a valid
                                          colorspec)
                            Italic   I - Italicized text
                            Bold     B - Boldfaced text
                            Expand   E - Expanded all levels
                            Full     F - Display full lineage at each
                                         level of the hierarchy
                            Dups     D - Allow duplicate indexes
                            Show     S - Includes the token as part of
                                         the index

                            DEFAULT: none

    OLSORT:
      <direction>           Allows you to sort the outline.  Specify
                            the direction as follows:

                            A - Ascending sequence
                            D - Descending sequence

                            DEFAULT: unsorted

    OLTOK:
      <token>               Specifying an outline token allows you
                            to generate a multi-level outline.  The
                            token is one or more characters that will
                            be used to split the selected outline
                            text into one or more segments.  Each
                            segment will becomes a node in the tree.

                            DEFAULT: none

    ORIENT:
      <orientation>         Orientation of the image on the paper.
                            Valid values are:

                            Landscape L - Along length of paper
                            Portrait  P - Along width of paper

                            DEFAULT: PORTRAIT

    OUTLINE:
      <type>                Outlining allows you to select text from
                            the document you're converting and use
                            it to build an outline.  Viewer apps
                            display the outline as a multi-level
                            tree.  Selecting the leaf nodes will
                            reposition the display to the location
                            that index was found in the document.

                            There are several scan types that you can
                            use to generate the outline.  They are:

                            RowCol  RC - Text at a specific row/col
                            ScanRow SR - Scan a specific row
                            ScanCol SC - Scan a specific column
                            Scan    S  - Scan entire document

                            The RowCol type is equivalent to the
                            original indexing scheme that was
                            specified with the IDX parameter.

                            DEFAULT: none

      ROWCOL:
        <row>               The row where the text is located or 0
                            which means any line of the page.

                            DEFAULT: none

        <col>               The column where the text starts.

                            DEFAULT: none

        <len>               The length of the text.

                            DEFAULT: none

      SCANROW:
        <row>               The row that will be scanned for
                            a match to the "text" argument.

                            DEFAULT: none

        <data col>          The column where the data starts.  This
                            may be a specified as an absolute column
                            within the selected row or as being
                            relative to the start of the located
                            text.  You specify a relative location
                            by prefixing the "data col" argument
                            with a plus (+) or minus (-) sign.

                            DEFAULT: none

        <data len>          The length of the data.

                            DEFAULT: none

        <text>              The text for which to search.

                            DEFAULT: none

      SCANCOL:
        <col>               The column that will be scanned for
                            a match to the "text" argument.

                            DEFAULT: none

        <data col>          The column where the data starts.  While
                            this may be a relative column location
                            as described under ScanRow, it doesn't
                            make much sense the scan column is known
                            ahead of time.

                            This should really be a "data row"
                            argument, but the scanning routine does
                            not have access to all rows at once.

                            DEFAULT: none

        <data len>          The length of the data.

                            DEFAULT: none

        <text>              The text for which to search.

                            DEFAULT: none

      SCAN:
        <data col>          The column where the data starts.  This
                            may be a specified as an absolute column
                            within the selected row or as being
                            relative to the start of the located
                            text.  You specify a relative location
                            by prefixing the "data col" argument
                            with a plus (+) or minus (-) sign.

                            DEFAULT: none

        <data len>          The length of the data.

                            DEFAULT: none

        <text>              The text for which to search.  Every row
                            will be scanned for this text.

                            DEFAULT: none

    OUTREC:
     <len>                  Forces truncation of output records to
                            the given length.

                            DEFAULT: 999999

    PAGE:
     <mode>                 Sets the recommended display mode.  This
                            is only a recommendation as the viewer
                            application may choose otherwise.
                            Currently defined values are:

                            None    N - Don't display thumbnails or
                                        outline
                            Outline O - Display the document outline
                            Thumbs  T - Display the document thumbs
                            Full    F - Full screen mode

                            The defualt setting is to allow the
                            viewer app to decide the mode.  This is
                            different than the "None" option since
                            the "None" option could override what
                            the user currently has selected.

                            However, if you are creating an outline
                            then the default will be "Outline".  If
                            that isn't desired, use this option to
                            override it.

                            DEFAULT: (let viewer app decide)

     <layout>               Set the recommended display layout.
                            Currently defined values are:

                            SinglePage     SP  - One page at a time
                            OneColumn      OC  - Pages in one column
                            TwoColumnLeft  TCL - Pages in two columns,
                                                 odd page on left
                            TwoColumnRight TCR - Pages in two columns,
                                                 odd page on right

                            DEFAULT: (let viewer app decide)

    PAPER:
      <size>                Size of the paper to use.  You may
                            specify the exact width and height in
                            inches, e.g., "8.5x11" or one of the
                            following values:

                            Letter Let - 8.5x11
                            Legal Leg  - 8.5x14
                            A4         - 8.27x11

                            DEFAULT: 8.5x11

      <type>                Type or design of paper.  Valid values
                            are:

                            Bluebar     Graybar    Greenbar
                            Orangebar   Whitebar   Yellowbar

                            DEFAULT: none

      <style>               Attributes specific to the <type>.
                            Valid values are:

                            Holed - Simulated holes will be drawn
                                    at the left and right edges of
                                    the paper.

                            DEFAULT: none

    RM:
      <right margin>        Offset in inches from right side of page
                            where the text will end.

                            DEFAULT: 0.5"

    TEXT:
      <type>                Allows you to modify the input text in
                            different ways.

                            So far there's only one type:

                            Attr A - Set text attributes

                            DEFAULT: none

      ATTR:
        <font name>         Font defined via XFONT parameter or one of
                            the following builtin PDF font names:

                            Times               T
                            TimesBold           TB
                            TimesItalic         TI
                            TimesBoldItalic     TBI
                            Helvetica           H
                            HelveticaBold       HB
                            HelveticaItalic     HI
                            HelveticaBoldItalic HBI
                            Courier             C
                            CourierBold         CB
                            CourierItalic       CI
                            CourierBoldItalic   CBI
                            Symbol              S
                            ZapfDingbats        Z

                            DEFAULT: FONT value (or its default)

        <font size>         Set the size of the font in points.

                            DEFAULT: FONT value (or its default)

        <fore colorspec>    Foreground (text) color.

                            DEFAULT: COLOR value (or its default)

        <scan type>         Method used to identify the text to
                            which these attributes will be applied.

                            Only one type is defined:

                            RowCol  RC - Text at a specific row/col
                            Scan    S  - Scan entire document

                            DEFAULT: none

        SCAN:
          <text>            The text for which to search.

        ROWCOL:
          <row>             The row where the text is located or 0
                            which means any line of the page.

                            DEFAULT: none

          <col>             The column where the text starts.

                            DEFAULT: none

          <len>             The length of the text.

                            DEFAULT: none

    TM:
      <top margin>          Offset in inches from top of page where
                            the text will begin.

                            DEFAULT: 0.5"

    TRANS:
     <style>                When displaying a document in fullscreen
                            or slide show modes, a transition effect
                            may be used when moving from page to
                            page.  The transition styles that may be
                            used are:

                            Blinds   BL - (Try me...)
                            Box      BO - (Try me...)
                            Dissolve D  - (Try me...)
                            Glitter  G  - (Try me...)
                            Replace  R  - (Try me...)
                            Split    S  - (Try me...)
                            Wipe     W  - (Try me...)

                            DEFAULT: none

     <dur>                  The duration of the effect in seconds.

                            DEFAULT: 1

     <opts/...>             Allows you to modify the behavior of the
                            effect.  The options are different for
                            each effect, but they aren't described
                            individually.  Just try them...  :-)

                            Specify as many of the following flags
                            as is appropriate for the transition
                            style:

                            Horizontal      H   - Split / Blinds
                            Vertical        V   - Split / Blinds
                            Inward          I   - Split / Box
                            Outward         O   - Split / Box
                            L2R             0   - Wipe / Glitter
                                                  (Left to right)
                            B2T             90  - Wipe
                                                  (Bottom to top)
                            R2L             180 - Wipe
                                                  (Right to left)
                            T2B             270 - Wipe / Glitter
                                                  (Top to bottom)
                            TL2BR           315 - Glitter
                                                  (Top left to
                                                   bottom right)

                            DEFAULT: none

    UNITS:
     <unit of measure>      Allows you to specify certain parameters
                            using centimeters or inches.  The values
                            affected by this are the paper size and
                            margins.  You may specify:

                            Inches      I
                            Centimeters C

                            DEFAULT: INCHES

    VIEWER:
     <flag/...>             Allows you to specify how the viewer
                            should behave when displaying the file.
                            Again, these are only recommendations
                            and the viewer may simply ignore them.

                            Specify as many of the following flags
                            as you like, separated with the "/"
                            character:

                            CenterWindow    CW - Center window on
                                                 screen
                            DisplayDocTitle DD - Document title in
                                                 title bar instead of
                                                 filename
                            FitWindow       FW - Resize window to
                                                 first page size
                            HideMenuBar     HM - Hide the menu bar
                            HideToolBar     HT - Hide the title bar
                            HideWindowUI    HW - Hide all UI elements
                            NonFullScreen   NF - Page mode after exiting
                                                 full screen.  Following
                                                 option must be one of:
                                                 None     N
                                                 Outlines O
                                                 Thumbs   T

                            DEFAULT: none

    VONLY:
     <validate only>        Allows validation of parameters without
                            performing the conversion.  If specified,
                            the VONLY parameter should precede all
                            others.

                            Yes Y - Prefix each message
                            No  N - Don't prefix the messages

                            DEFAULT: No

    XFONT:
      <type>                Allows you to use fonts other than the
                            14 standard Adobe fonts.  Please read the
                            the documentation for this keyword since
                            its use is fairly complex.

                            Currently supported types are:

                            External E - External font "program"
                            Internal I - Internal font
                            TrueType TT - TrueType font

                            DEFAULT: none

      EXTERNAL:
                            Note that this does not embed the real
                            font.  It simply allows you to reference
                            a preinstalled one.

        <name>              The name to give the font so it may be
                            referenced anywhere font names are allowed.

                            DEFAULT: none

        <filespec>          Name of file whose contents will be
                            embedded into the PDF file as a font.
                            This file must contain valid syntax
                            for a PDF font object.

                            DEFAULT: none

      INTERNAL:
                            Allows you to control handling of the
                            14 standard fonts.

        <name>              The name to give the font so it may be
                            referenced anywhere font names are allowed.

                            DEFAULT: none

        <encoding>          The name of a previously defined encoding
                            to be associated with this font name.

                            DEFAULT: none

        <fontspec>          The name of the builtin font to associate
                            with this name.

                            DEFAULT: none

      TRUETYPE:
                            !!!IMPORTANT!!!

                            Many TrueType fonts are copyrighted and
                            may not be legally embedded, so make sure
                            you do your homework.

                            On z/VM and z/OS, the fonts can reside in
                            variable or fixed record data sets of any
                            record length and may reside in a PDS.

        <name>              The name to give the font so it may be
                            referenced anywhere font names are allowed.

                            DEFAULT: none

        <encoding>          The name of a previously defined encoding
                            to be associated with this font name.

                            DEFAULT: WinAnsiEncoding

        <flag>              Allows control how fonts are embedded.  The
                            default is to embed and subset fonts:  You
                            may specify:

                            NoSubset    NS
                            NoEmbed     NE

        <filespec>          Name of the TrueType file.

                            DEFAULT: none

    XLATE:
     <execname>             Allows you to specify a translation table
                            EXEC name other than the default TXT2PDFX.
                            (You may have input text in different
                            codepages.)

                            The execname specifies a standard REXX
                            EXEC that you can model after TXT2PDFX or
                            one of the XLATEnnn samples.

                            DEFAULT: none

===================================================================== */
