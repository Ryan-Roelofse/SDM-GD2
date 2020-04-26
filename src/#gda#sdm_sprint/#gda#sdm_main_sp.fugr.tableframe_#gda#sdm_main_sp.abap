*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/GDA/SDM_MAIN_SP
*   generation date: 30.01.2019 at 09:59:18
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/GDA/SDM_MAIN_SP   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
