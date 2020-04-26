*---------------------------------------------------------------------*
*    program for:   VIEWPROC_/GDA/SDM_DS03_V
*   generation date: 28.08.2019 at 18:39:24 by user RROELOFSE
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION VIEWPROC_/GDA/SDM_DS03_V      .
*----------------------------------------------------------------------*
* Initialization: set field-symbols etc.                               *
*----------------------------------------------------------------------*
   IF LAST_VIEW_INFO NE VIEW_NAME.
ASSIGN /GDA/SDM_DS03_V TO <TABLE1>.
ASSIGN */GDA/SDM_DS03_V TO <INITIAL>.
ASSIGN STATUS_/GDA/SDM_DS03_V TO <STATUS>.
     PERFORM INITIALISIEREN.
   ENDIF.
   PERFORM JUSTIFY_ACTION_MODE.
   MOVE: VIEW_ACTION TO MAINT_MODE,
         CORR_NUMBER TO CORR_NBR.

*----------------------------------------------------------------------*
* Get data from database                                               *
*----------------------------------------------------------------------*
  IF FCODE EQ READ OR FCODE EQ READ_AND_EDIT.
    PERFORM PREPARE_READ_REQUEST.
    IF X_HEADER-FRM_RP_GET NE SPACE.
            PERFORM (X_HEADER-FRM_RP_GET) IN PROGRAM.
    ELSE.
PERFORM GET_DATA_/GDA/SDM_DS03_V.
    ENDIF.
    IF FCODE EQ READ_AND_EDIT. FCODE = EDIT. ENDIF.
  ENDIF.

  CASE FCODE.
    WHEN  EDIT.                          " Edit read data
      PERFORM CALL_DYNPRO.
      PERFORM CHECK_UPD.
*....................................................................*

    WHEN SAVE.                           " Write data into database
      PERFORM PREPARE_SAVING.
      IF <STATUS>-UPD_FLAG NE SPACE.
        IF X_HEADER-FRM_RP_UPD NE SPACE.
          PERFORM (X_HEADER-FRM_RP_UPD) IN PROGRAM.
        ELSE.
          IF SY-SUBRC EQ 0.
PERFORM DB_UPD_/GDA/SDM_DS03_V.
          ENDIF.
        ENDIF.
        PERFORM AFTER_SAVING.
      ENDIF.
*....................................................................*

    WHEN RESET_LIST.     " Refresh all marked entries of EXTRACT from db
      PERFORM RESET_ENTRIES USING LIST_BILD.
*....................................................................*

    WHEN RESET_ENTRY.               " Refresh single entry from database
      PERFORM RESET_ENTRIES USING DETAIL_BILD.
*.......................................................................
  ENDCASE.
MOVE STATUS_/GDA/SDM_DS03_V-UPD_FLAG TO UPDATE_REQUIRED.
ENDFUNCTION.
