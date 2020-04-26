*&---------------------------------------------------------------------*
*& Report /GDA/SDM_EXCEPTIONS_DELETE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_exceptions_delete.

************************************************************************
*
*                            SCALAR DATA
*
************************************************************************
DATA: gv_exit          TYPE abap_bool,
      gv_period        TYPE /gda/sdm_excep-period,
      gv_error_message TYPE string.

************************************************************************
*
*                             REFERENCES
*
************************************************************************
DATA: go_brf_exc_util    TYPE REF TO /gda/sdm_cl_exceptions,
      gx_exc_report_util TYPE REF TO /gda/cx_sdm_exception_handl.

************************************************************************
*
*                           SELECTION-SCREEN
*
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK 101 WITH FRAME TITLE TEXT-001.     "General Selection
PARAMETERS:
  p_obj_t TYPE /gda/sdm_excep-object_type OBLIGATORY.

PARAMETERS: p_12mon RADIOBUTTON GROUP grp1,
            p_6mon  RADIOBUTTON GROUP grp1,
            p_3mon  RADIOBUTTON GROUP grp1,
            p_1mon  RADIOBUTTON GROUP grp1,
            p_all   RADIOBUTTON GROUP grp1.

SELECTION-SCREEN END OF BLOCK 101.

************************************************************************
*
*                         START-OF-SELECTION
*
************************************************************************
START-OF-SELECTION.
  PERFORM popup_to_confirm CHANGING gv_exit.
  IF gv_exit = abap_true.
    RETURN.
  ENDIF.

  PERFORM calc_period CHANGING gv_period.

  TRY.
*/ Create Utility 1Object
      CREATE OBJECT go_brf_exc_util
        EXPORTING
          iv_object_type = p_obj_t
          iv_period      = gv_period
          iv_del_sign    = 'LE'. "Less than or Equal

*/ Authorised?
      IF go_brf_exc_util->check_authorisation( iv_actvt = '41' ) = abap_false.
        MESSAGE 'Not authorised to Delete Statistics. Check SU53' TYPE 'E'.
      ENDIF.

*/ Delete Data
      go_brf_exc_util->delete_statistics( iv_commit = abap_true ).

    CATCH /gda/cx_sdm_exception_handl INTO gx_exc_report_util.
      gv_error_message = gx_exc_report_util->mv_text.
  ENDTRY.

  IF gv_error_message IS NOT INITIAL.
    IF sy-batch = abap_true.
      WRITE: gv_error_message.
    ELSE.
      MESSAGE gv_error_message TYPE 'I'.
    ENDIF.
  ENDIF.

************************************************************************
*
*                            SUBROUTINES
*
************************************************************************

FORM calc_period CHANGING cv_period TYPE /gda/sdm_excep-period.

  DATA: lv_datum TYPE datum.

  IF p_12mon = 'X'.
*/ One Year
    lv_datum = sy-datum - 365.
  ELSEIF p_6mon = 'X'.
*/ 6 Months
    lv_datum = sy-datum - 182.
  ELSEIF p_3mon = 'X'.
*/ 3 Months
    lv_datum = sy-datum - 91.
  ELSEIF p_1mon = 'X'.
    lv_datum = sy-datum - 31.
  ELSEIF p_all = 'X'.
    lv_datum = sy-datum.
  ELSE.
  ENDIF.

  cv_period = lv_datum.
ENDFORM.

FORM popup_to_confirm CHANGING cv_exit TYPE abap_bool.

  DATA: lv_answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
*     TITLEBAR       = ' '
*     DIAGNOSE_OBJECT             = ' '
      text_question  = 'Delete Stats for the Object and Period?'
*     TEXT_BUTTON_1  = 'Ja'(001)
*     ICON_BUTTON_1  = ' '
*     TEXT_BUTTON_2  = 'Nein'(002)
*     ICON_BUTTON_2  = ' '
*     DEFAULT_BUTTON = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN   = 25
*     START_ROW      = 6
*     POPUP_TYPE     =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      answer         = lv_answer
* TABLES
*     PARAMETER      =
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF lv_answer <> '1'.  "Yes
    cv_exit = abap_true.
  ENDIF.

ENDFORM.
