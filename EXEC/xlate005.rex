        /* ========================== REXX =============================
        || Name:      XLATE004
        ||
        || Function:  Turkish (Latin-5) translation tables
        ||
        || Text:      IBM-1026 -> ISO-8859-9
        ||
        || Data:      IBM-1026 -> ISO-8859-9
        ||
        || Author:    Leland Lucius (pdf@homeroww.net>
        ||
        || History:   Jan-08-2004 - Created
        ||
        || Note:      See the XFONT002 file for an XFONT descriptor that
        ||            can be used with these translation tables for full
        ||            Turkish support.
        ||
        ||            See additional notes in XLATE000.
        ||
        */

        /* =============================================================
        || Convert input text from IBM-1026 to ISO-8859-9.
        */

               /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
        Text =  '000102039C09C67FD78D8E0B0C0D0E0F'x || , /* 00 */
                '101112139D0A08C71819D28F1C1D1E1F'x || , /* 10 */
                '80C1C2C3C4C5171BC8C98A8B8C050607'x || , /* 20 */
                '90D116D3D4D5D604D8D99A9B14159E1A'x || , /* 30 */
                '20A0E2E4E0E1E3E57BF1C72E3C282B21'x || , /* 40 */
                '26E9EAEBE8EDEEEFECDFD0DD2A293B5E'x || , /* 50 */
                '2D2FC2C4C0C1C3C55BD1FE2C255F3E3F'x || , /* 60 */
                'F8C9CACBC8CDCECFCCFD3AD6DE273DDC'x || , /* 70 */
                'D8616263646566676869ABBB7D60E6B1'x || , /* 80 */
                'B06A6B6C6D6E6F707172AABAE6B8C6E4'x || , /* 90 */
                'B5F6737475767778797AA1BF5D2440AE'x || , /* A0 */
                'E2E3E5B7E9E7B6BCBDBEAC7CAFE8B4D7'x || , /* B0 */
                'E7414243444546474849ADF47EF2F3F5'x || , /* C0 */
                'F04A4B4C4D4E4F505152B9FB5CF9FAFF'x || , /* D0 */
                'FCF7535455565758595AB2D423D2D3D5'x || , /* E0 */
                '30313233343536373839B3DB22D9DA9F'x      /* F0 */

        /* =============================================================
        || Convert program data from IBM-1026 to ISO-8859-9.
        */

               /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
        Data =  '000102039C09C67FD78D8E0B0C0D0E0F'x || , /* 00 */
                '101112139D0A08C71819D28F1C1D1E1F'x || , /* 10 */
                '80C1C2C3C4C5171BC8C98A8B8C050607'x || , /* 20 */
                '90D116D3D4D5D604D8D99A9B14159E1A'x || , /* 30 */
                '20A0E2E4E0E1E3E57BF1C72E3C282B21'x || , /* 40 */
                '26E9EAEBE8EDEEEFECDFD0DD2A293B5E'x || , /* 50 */
                '2D2FC2C4C0C1C3C55BD1FE2C255F3E3F'x || , /* 60 */
                'F8C9CACBC8CDCECFCCFD3AD6DE273DDC'x || , /* 70 */
                'D8616263646566676869ABBB7D60E6B1'x || , /* 80 */
                'B06A6B6C6D6E6F707172AABAE6B8C6E4'x || , /* 90 */
                'B5F6737475767778797AA1BF5D2440AE'x || , /* A0 */
                'E2E3E5B7E9E7B6BCBDBEAC7CAFE8B4D7'x || , /* B0 */
                'E7414243444546474849ADF47EF2F3F5'x || , /* C0 */
                'F04A4B4C4D4E4F505152B9FB5CF9FAFF'x || , /* D0 */
                'FCF7535455565758595AB2D423D2D3D5'x || , /* E0 */
                '30313233343536373839B3DB22D9DA9F'x      /* F0 */

        return Text || Data
