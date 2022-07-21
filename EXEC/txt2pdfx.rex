        /* ========================== REXX =============================
        || Name:      XLATE000
        ||
        || Function:  Default translation tables
        ||
        || Text:      IBM-1047 -> ISO8859-1
        ||
        || Data:      IBM-1047 -> ISO8859-1
        ||
        || Author:    Leland Lucius (pdf@homeroww.net>
        ||
        || History:   Nov-06-2002 - Created
        ||
        || Note:      The first table is used to convert the input text
        ||            to the codepage used to view the PDF document.
        ||
        ||            The second table is used to convert text internal
        ||            to the program from whatever codepage the program
        ||            is stored in to the required ISO8859-1 codepage.
        ||            Most likely the program will be stored using the
        ||            IBM-1047 codepage.  However, if it is translated
        ||            to some other codepage during installation, you
        ||            will need to change this table to guarantee the
        ||            text gets converted to ISO8859-1.
        ||
        */

        /* =============================================================
        || Convert input text from IBM-1047 to ISO8859-1.
        */

               /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
        Text =  '000102039C09867F978D8E0B0C0D0E0F'x || , /* 00 */
                '101112139D0A08871819928F1C1D1E1F'x || , /* 10 */
                '808182838485171B88898A8B8C050607'x || , /* 20 */
                '909116939495960498999A9B14159E1A'x || , /* 30 */
                '20A0E2E4E0E1E3E5E7F1A22E3C282B7C'x || , /* 40 */
                '26E9EAEBE8EDEEEFECDF21242A293B5E'x || , /* 50 */
                '2D2FC2C4C0C1C3C5C7D1A62C255F3E3F'x || , /* 60 */
                'F8C9CACBC8CDCECFCC603A2340273D22'x || , /* 70 */
                'D8616263646566676869ABBBF0FDFEB1'x || , /* 80 */
                'B06A6B6C6D6E6F707172AABAE6B8C6A4'x || , /* 90 */
                'B57E737475767778797AA1BFD05BDEAE'x || , /* A0 */
                'ACA3A5B7A9A7B6BCBDBEDDA8AF5DB4D7'x || , /* B0 */
                '7B414243444546474849ADF4F6F2F3F5'x || , /* C0 */
                '7D4A4B4C4D4E4F505152B9FBFCF9FAFF'x || , /* D0 */
                '5CF7535455565758595AB2D4D6D2D3D5'x || , /* E0 */
                '30313233343536373839B3DBDCD9DA9F'x      /* F0 */

        /* =============================================================
        || Convert program data from IBM-1047 to ISO8859-1.
        */

               /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
        Data =  '000102039C09867F978D8E0B0C0D0E0F'x || , /* 00 */
                '101112139D0A08871819928F1C1D1E1F'x || , /* 10 */
                '808182838485171B88898A8B8C050607'x || , /* 20 */
                '909116939495960498999A9B14159E1A'x || , /* 30 */
                '20A0E2E4E0E1E3E5E7F1A22E3C282B7C'x || , /* 40 */
                '26E9EAEBE8EDEEEFECDF21242A293B5E'x || , /* 50 */
                '2D2FC2C4C0C1C3C5C7D1A62C255F3E3F'x || , /* 60 */
                'F8C9CACBC8CDCECFCC603A2340273D22'x || , /* 70 */
                'D8616263646566676869ABBBF0FDFEB1'x || , /* 80 */
                'B06A6B6C6D6E6F707172AABAE6B8C6A4'x || , /* 90 */
                'B57E737475767778797AA1BFD05BDEAE'x || , /* A0 */
                'ACA3A5B7A9A7B6BCBDBEDDA8AF5DB4D7'x || , /* B0 */
                '7B414243444546474849ADF4F6F2F3F5'x || , /* C0 */
                '7D4A4B4C4D4E4F505152B9FBFCF9FAFF'x || , /* D0 */
                '5CF7535455565758595AB2D4D6D2D3D5'x || , /* E0 */
                '30313233343536373839B3DBDCD9DA9F'x      /* F0 */

        return Text || Data
