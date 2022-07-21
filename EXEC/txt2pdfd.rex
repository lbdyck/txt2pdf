        /* --------------------  rexx procedure  -------------------- *
         * Name:      TXT2PDFD                                        *
         *                                                            *
         * Function:  Customization defintions for TXT2PDF            *
         *                                                            *
         * Syntax:    Invoked as a rexx function                      *
         *                                                            *
         *            x = txt2pdfd()                                  *
         *                                                            *
         *            Must reside in the sysproc or sysexec           *
         *            concatenation.  However, if not present,        *
         *            internal defaults within TXT2PDF will be        *
         *            used.                                           *
         *                                                            *
         * Author:    Leland Lucius                                   *
         *            Internet: pdf@homerow.net                       *
         *                                                            *
         * History:                                                   *
         *            09/07/02 - Created from TXT2PDFC                *
         *                                                            *
         * ---------------------------------------------------------- */

        parse value "" with null              /* For convenience     */

        /* --------------------------------------------------------- *
         * Unit of measure                                           *
         * --------------------------------------------------------- */
        unit_of_measure     = "Inches"        /* keyword: UNITS      */

        /* --------------------------------------------------------- *
         * Paper                                                     *
         * --------------------------------------------------------- */
        paper_size          = "8.5x11"        /* keyword: PAPER      */
        paper_type          = null            /*                     */
        paper_style         = null            /*                     */

        /* --------------------------------------------------------- *
         * Margins                                                   *
         * --------------------------------------------------------- */
        left_margin         = .5              /* keyword: LM         */
        right_margin        = .5              /* keyword: RM         */
        top_margin          = .5              /* keyword: TM         */
        bottom_margin       = .5              /* keyword: BM         */

        /* --------------------------------------------------------- *
         * Text                                                      *
         * --------------------------------------------------------- */
        text_orientation    = "Portrait"      /* keyword: ORIENT     */
        text_lines_per_inch = 8               /* keyword: LPI        */

        /* --------------------------------------------------------- *
         * Font                                                      *
         * --------------------------------------------------------- */
        font_name           = "Courier"       /* keyword: FONT       */
        font_size           = 9               /*                     */
        font_zoom           = 100             /*                     */

        /* --------------------------------------------------------- *
         * Colors                                                    *
         * --------------------------------------------------------- */
        foreground_color    = "Black"         /* keyword: COLOR      */
        background_color    = "White"         /*                     */

        /* --------------------------------------------------------- *
         * Miscellaneous                                             *
         * --------------------------------------------------------- */
        browse              = "No"            /* keyword: BROWSE     */
        confirm             = "Yes"           /* keyword: CONFIRM    */
        message_id          = "Yes"           /* keyword: MSGID      */
        compress            = 0               /* keyword: COMPRESS   */

        /* --------------------------------------------------------- *
         * As you can see, we simply build a pseudo command line to  *
         * pass back to TXT2PDF.  You may extend what is here with   *
         * any other keywords and values as long as the result is    *
         * valid TXT2PDF syntax.                                     *
         * --------------------------------------------------------- */
        return "UNITS"    unit_of_measure,
               "PAPER"    paper_size"/"paper_type"/"paper_style,
               "ORIENT"   text_orientation,
               "LPI"      text_lines_per_inch,
               "LM"       left_margin,
               "RM"       right_margin,
               "TM"       top_margin,
               "BM"       bottom_margin,
               "FONT"     font_size"/"font_name"/"font_zoom,
               "COLOR"    foreground_color"/"background_color,
               "BROWSE"   browse,
               "CONFIRM"  confirm,
               "MSGID"    message_id,
               "COMPRESS" compress

        /* --------------------------------------------------------- *
         * The End                                                   *
         * --------------------------------------------------------- */
