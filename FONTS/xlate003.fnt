        /* ========================== REXX =============================
        || Name:      XLATE003
        ||
        || Function:  Cyrillic translation tables
        ||
        || Text:      IBM-1025 -> Windows-1251
        ||
        || Data:      IBM-1025 -> Windows-1251
        ||
        || Author:    Leland Lucius (pdf@homeroww.net>
        ||
        || History:   Nov-06-2002 - Created
        ||
        || Note:      See the XFONT000 file for an XFONT descriptor that
        ||            can be used with these translation tables for full
        ||            Cyrillic support.
        ||
        ||            See additional notes in XLATE000.
        ||
        */

        /* =============================================================
        || Convert input text from IBM-1025 to Windows-1251.
        */

               /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
        Text =  '000102039C09867F978D8E0B0C0D0E0F'x || , /* 00 */
                '101112139D0A08871819928F1C1D1E1F'x || , /* 10 */
                '808182838485171B88898A8B8C050607'x || , /* 20 */
                '909116939495960498999A9B14159E1A'x || , /* 30 */
                '20A09083B8BABEB3BFBCA22E3C282B7C'x || , /* 40 */
                '269A9C9E9DA29FDAB98021242A293B5E'x || , /* 50 */
                '2D2F81A8AABDB2AFA38AA62C255F3E3F'x || , /* 60 */
                '8C8E8DADA18FFEE0E1603A2340273D22'x || , /* 70 */
                'F6616263646566676869E4E5F4E3F5E8'x || , /* 80 */
                'E96A6B6C6D6E6F707172EAEBECEDEEEF'x || , /* 90 */
                'FF7E737475767778797AF0F1F2F3E6E2'x || , /* A0 */
                'FCFBE7F8FDF9F7FADEC0C1D6C4C5D4C3'x || , /* B0 */
                '7B414243444546474849D5C8C9CACBCC'x || , /* C0 */
                '7D4A4B4C4D4E4F505152CDCECFDFD0D1'x || , /* D0 */
                '5CA7535455565758595AD2D3C6C2DCDB'x || , /* E0 */
                '30313233343536373839C7D8DDD9D79F'x      /* F0 */

        /* =============================================================
        || Convert program data from IBM-1025 to Windows-1251.
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
