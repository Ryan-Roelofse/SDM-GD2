*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/GDA/SDM_MAIN_SE
*   generation date: 12.03.2019 at 10:09:18
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/GDA/SDM_MAIN_SE   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
