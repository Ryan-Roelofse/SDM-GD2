*&---------------------------------------------------------------------*
*& Report /GDA/SDM_EXCEPTION_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_exceptions_report.

DATA: go_alv_columns TYPE REF TO cl_salv_columns_table.

************************************************************************
*
*                            STRUCTURES
*
************************************************************************
**// Select-Options
DATA: BEGIN OF gs_so,
        object_type TYPE /gda/sdm_excep-object_type,
        period      TYPE /gda/sdm_excep-period,
        msg_id      TYPE /gda/sdm_excep-msg_id,
        msg_number  TYPE /gda/sdm_excep-msg_number,
      END OF gs_so.

DATA: BEGIN OF gs_output.
    INCLUDE STRUCTURE /gda/sdm_excep.
DATA: message TYPE /gda/sdm_de_message,
      END OF gs_output.

************************************************************************
*
*                           INTERNAL TABLES
*
************************************************************************
DATA: gt_output LIKE TABLE OF gs_output.

************************************************************************
*
*                           SELECTION-SCREEN
*
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK 101 WITH FRAME TITLE TEXT-001.     "General Selection
PARAMETERS: p_obj_t LIKE gs_so-object_type OBLIGATORY.

SELECT-OPTIONS:
  s_period FOR gs_so-period OBLIGATORY,
  s_id FOR gs_so-msg_id DEFAULT '/GDA/SDM1' MATCHCODE OBJECT t100af4,
  s_number FOR gs_so-msg_number.
SELECTION-SCREEN END OF BLOCK 101.

SELECTION-SCREEN BEGIN OF BLOCK 102 WITH FRAME TITLE TEXT-002.     "Report Options
PARAMETERS: p_sum_m RADIOBUTTON GROUP grp1, "Summarise by month
            p_sum_w RADIOBUTTON GROUP grp1, "Summarise by week
            p_sum_d RADIOBUTTON GROUP grp1. "Summarise by day
SELECTION-SCREEN END OF BLOCK 102.


************************************************************************
*
*                           INITIALIZATION
*
************************************************************************
INITIALIZATION.
  PERFORM init.

************************************************************************
*
*                         START-OF-SELECTION
*
************************************************************************
START-OF-SELECTION.
  PERFORM get_data.

************************************************************************
*
*                          END-OF-SELECTION
*
************************************************************************
END-OF-SELECTION.
  IF gt_output IS INITIAL.
    MESSAGE 'No data selected' TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM output_report.

************************************************************************
*
*                            SUBROUTINES
*
************************************************************************

FORM init.
  DATA: lv_first_day TYPE datum,
        lv_last_day  TYPE datum.

  lv_first_day = sy-datum.
  lv_first_day+6(2) = '01'.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = sy-datum
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  s_period-sign = 'I'.
  s_period-option = 'BT'.
  s_period-low = lv_first_day.
  s_period-high = lv_last_day.
  APPEND s_period.
ENDFORM.

FORM get_data.

  DATA: ls_output LIKE gs_output.
  DATA: gt_brf_exc TYPE TABLE OF /gda/sdm_excep.

  FIELD-SYMBOLS: <ls_brf_exc> TYPE /gda/sdm_excep.

  DATA: lv_msgno TYPE t100-msgnr.

  SELECT *
    INTO TABLE gt_brf_exc
    FROM /gda/sdm_excep
  WHERE object_type = p_obj_t
    AND period IN s_period.

  LOOP AT gt_brf_exc ASSIGNING <ls_brf_exc>.
    MOVE-CORRESPONDING <ls_brf_exc> TO ls_output.
*/ Get Message Long Text
    lv_msgno = ls_output-msg_number.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
*       LANGUAGE               = ' '
        msg_id                 = ls_output-msg_id
        msg_no                 = lv_msgno
*       MSG_VAR1               = ' '
*       MSG_VAR2               = ' '
*       MSG_VAR3               = ' '
*       MSG_VAR4               = ' '
      IMPORTING
        msg_text               = ls_output-message
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      ls_output-message = 'Error Determining message'.
    ENDIF.

    PERFORM summarise_by_period CHANGING ls_output-period.
    PERFORM convert_object USING ls_output-object_type
            CHANGING ls_output-object.

    COLLECT ls_output INTO gt_output.
    CLEAR ls_output.
  ENDLOOP.

ENDFORM.

FORM output_report.
  DATA: lv_error_message TYPE string.

  DATA: ls_layout_key TYPE salv_s_layout_key.

  DATA: lo_alv_grid      TYPE REF TO cl_salv_table,
        lo_alv_functions TYPE REF TO cl_salv_functions,
        lo_alv_display   TYPE REF TO cl_salv_display_settings,
        lo_alv_layout    TYPE REF TO cl_salv_layout,
        lo_alv_column    TYPE REF TO cl_salv_column_table.

  DATA: lx_salv_msg TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv_grid
        CHANGING t_table = gt_output ).

*/ Functions
      lo_alv_functions = lo_alv_grid->get_functions( ).
      lo_alv_functions->set_all( abap_true ).

*/ Display Settings
      lo_alv_display = lo_alv_grid->get_display_settings( ).
      lo_alv_display->set_striped_pattern( abap_true ).

*/ Layout Settings
      lo_alv_layout = lo_alv_grid->get_layout( ).
      ls_layout_key-report = sy-repid.
      lo_alv_layout->set_key( ls_layout_key ).
      lo_alv_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
      lo_alv_layout->set_default( abap_true ).

*/ Columns
      go_alv_columns = lo_alv_grid->get_columns( ).
      go_alv_columns->set_optimize( abap_true ).

      lo_alv_column ?= go_alv_columns->get_column( 'COUNTER' ).
      lo_alv_column->set_technical( 'X' ).

      PERFORM extra_fields_description.

*/ Disaply ALV
      lo_alv_grid->display( ).

    CATCH cx_salv_msg cx_salv_not_found..
      lv_error_message = 'Error displaing ALV'.
      MESSAGE lv_error_message TYPE 'E'.
  ENDTRY.

ENDFORM.

FORM summarise_by_period CHANGING cv_period TYPE datum.

  DATA: lv_period TYPE datum,
        lv_days   TYPE n LENGTH 2.

  lv_period = cv_period.
  lv_days = cv_period+6(2).

  IF p_sum_m = 'X'.
*/ Monthly
    lv_period+6(2) = '01'.
  ENDIF.

  IF p_sum_w = 'X'.
*/ Weekly
    IF lv_days >= '01' AND lv_days <= '07'.
      lv_period+6(2) = '01'.
    ELSEIF lv_days >= '08' AND lv_days <= '14'.
      lv_period+6(2) = '02'.
    ELSEIF lv_days >= '15' AND lv_days <= '21'.
      lv_period+6(2) = '03'.
    ELSE.
      lv_period+6(2) = '04'.
    ENDIF.
  ENDIF.

  IF p_sum_d = 'X'.
*/ Daily = Default
  ENDIF.

  cv_period = lv_period.

ENDFORM.

FORM extra_fields_description.

  DATA: ls_brf_exc_emap TYPE /gda/sdm_setup2.

  SELECT SINGLE * INTO ls_brf_exc_emap
    FROM /gda/sdm_setup2
  WHERE object_type = p_obj_t.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  PERFORM set_col_description USING:
    'EXTRA_V1' ls_brf_exc_emap-extra_v1,
    'EXTRA_V2' ls_brf_exc_emap-extra_v2,
    'EXTRA_V3' ls_brf_exc_emap-extra_v3,
    'EXTRA_V4' ls_brf_exc_emap-extra_v4,
    'EXTRA_V5' ls_brf_exc_emap-extra_v5,
    'EXTRA_V6' ls_brf_exc_emap-extra_v6.
ENDFORM.

FORM set_col_description USING iv_col_name TYPE c iv_description TYPE c.

  DATA: lo_alv_column TYPE REF TO cl_salv_column_table.

  DATA: lv_text_s TYPE scrtext_s,
        lv_text_m TYPE scrtext_m,
        lv_text_l TYPE scrtext_l.

  IF iv_description IS INITIAL.
    RETURN.
  ENDIF.

  lv_text_s = lv_text_m = lv_text_l = iv_description.

  TRY.
      lo_alv_column ?= go_alv_columns->get_column( iv_col_name ).
      lo_alv_column->set_short_text( lv_text_s ).
      lo_alv_column->set_medium_text( lv_text_m ).
      lo_alv_column->set_long_text( lv_text_l ).

    CATCH cx_salv_not_found.
      MESSAGE 'Extra Fields Mapping Error' TYPE 'E'.

  ENDTRY.

ENDFORM.

FORM convert_object USING iv_object_type TYPE /gda/sdm_de_object
                    CHANGING cv_object   TYPE /gda/sdm_de_object.

  IF iv_object_type = 'MATERIAL'.
    PERFORM conversion_alpha_output CHANGING cv_object.
  ENDIF.

ENDFORM.

FORM conversion_alpha_output CHANGING cv_output TYPE c.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = cv_output
    IMPORTING
      output = cv_output.

ENDFORM.
