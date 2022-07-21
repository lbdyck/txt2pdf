        /* ========================== REXX =============================
        || Name:      XLATE001
        ||
        || Function:  Original translate tables
        ||
        || Text:      IBM-1047 -> Modified TCPIP.AEZAXLT1(TELNET)
        ||
        || Data:      IBM-1047 -> Modified TCPIP.AEZAXLT1(TELNET)
        ||
        || Author:    Leland Lucius (pdf@homeroww.net>
        ||
        || History:   Nov-06-2002 - Created
        ||
        || Note:      This was the original translation tables used in
        ||            TXT2PDF.  To be honest, I can't remember why I
        ||            used the IP stacks TELNET table as a base and
        ||            then modify it.  I should have used standard code
        ||            pages from the outset.
        ||
        ||            If your documents require these, just copy this
        ||            to your EXEC search path as TXT2PDFX.
        ||
        */

        /* =============================================================
        || Convert input text from IBM-1047 to something else.  ;-)
        */

               /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
        Text =  '000102031A091A7F1A1A1A0B0C0D0E0F'x || , /* 00 */
                '101112131A0A081A18191A1A1C1D1E1F'x || , /* 10 */
                '1A1A1C1A1A0A171B1A1A1A1A1A050607'x || , /* 20 */
                '1A1A161A1A1E1A041A1A1A1A14151A1A'x || , /* 30 */
                '20A6E180EB909FE2AB8B9B2E3C282B7C'x || , /* 40 */
                '26A9AA9CDBA599E3A89E21242A293B5E'x || , /* 50 */
                '2D2FDFDC9ADDDE989DACBA2C255F3E3F'x || , /* 60 */
                'D78894B0B1B2FCD6FB603A2340273D22'x || , /* 70 */
                'F861626364656667686996A4F3AFAEC5'x || , /* 80 */
                '8C6A6B6C6D6E6F7071729787CE93F1FE'x || , /* 90 */
                'C87E737475767778797AEFC0DA5BF2F9'x || , /* A0 */
                'B5B6FDB7B8B9E6BBBCBD8DD9BF5DD8C4'x || , /* B0 */
                '7B414243444546474849CBCABEE8ECED'x || , /* C0 */
                '7D4A4B4C4D4E4F505152A1ADF5F4A38F'x || , /* D0 */
                '5CE7535455565758595AA0858EE9E4D1'x || , /* E0 */
                '30313233343536373839B3F7F0FAA7FF'x      /* F0 */

        /* =============================================================
        || Convert program data from IBM-1047 to something else.  ;-)
        */

               /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
        Data =  '000102031A091A7F1A1A1A0B0C0D0E0F'x || , /* 00 */
                '101112131A0A081A18191A1A1C1D1E1F'x || , /* 10 */
                '1A1A1C1A1A0A171B1A1A1A1A1A050607'x || , /* 20 */
                '1A1A161A1A1E1A041A1A1A1A14151A1A'x || , /* 30 */
                '20A6E180EB909FE2AB8B242E3C282B7C'x || , /* 40 */
                '26A9AA9CDBA599E3A89E21A32A293B5E'x || , /* 50 */
                '2D2FDFDC9ADDDE989DACBA2C255F3E3F'x || , /* 60 */
                'D78894B0B1B2FCD6FB603A2340273D22'x || , /* 70 */
                'F861626364656667686996A4F3AFAEC5'x || , /* 80 */
                '8C6A6B6C6D6E6F7071729787CE93F1FE'x || , /* 90 */
                'C87E737475767778797AEFC0DA5BF2F9'x || , /* A0 */
                'B5B6FDB7B8B9E6BBBCBD8DD9BF5DD8C4'x || , /* B0 */
                '7B414243444546474849CBCABEE8ECED'x || , /* C0 */
                '7D4A4B4C4D4E4F505152A1ADF5F49B8F'x || , /* D0 */
                '5CE7535455565758595AA0858EE9E4D1'x || , /* E0 */
                '30313233343536373839B3F7F0FAA7FF'x      /* F0 */

        return Text || Data
