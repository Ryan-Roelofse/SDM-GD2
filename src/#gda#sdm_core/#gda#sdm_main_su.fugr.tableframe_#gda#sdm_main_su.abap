*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/GDA/SDM_MAIN_SU
*   generation date: 27.12.2018 at 21:18:40
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/GDA/SDM_MAIN_SU   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
