*&---------------------------------------------------------------------*
*& Report /GDA/SDM_LICENSE_CHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_license_check  MESSAGE-ID /gda/sdm_core.

DATA:
  lt_sdm_lic     TYPE TABLE OF /gda/sdm_lic,
  lt_sdm_lic_alv TYPE STANDARD TABLE OF /gda/sdm_s_license_keys_alv,
  ls_sdm_lic_alv TYPE /gda/sdm_s_license_keys_alv,
  lt_modules     TYPE STANDARD TABLE OF dd07v.

FIELD-SYMBOLS:
  <sdm_lic> LIKE LINE OF lt_sdm_lic,
  <sdm_mod> LIKE LINE OF lt_modules.

AT SELECTION-SCREEN.
  AUTHORITY-CHECK OBJECT '/GDA/ADM'
           ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE TEXT-007 TYPE 'E'.
  ENDIF.


START-OF-SELECTION.
  PERFORM get_module_descriptions USING lt_modules.

  TRY.
      lt_sdm_lic = /gda/sdm_cl_brf_mapping=>get_function( ).
    CATCH /gda/cx_sdm_exception_handl .
      MESSAGE TEXT-008 TYPE 'E'.
  ENDTRY.

  IF lt_sdm_lic IS INITIAL.
    MESSAGE TEXT-006 TYPE 'E'.
  ENDIF.

  LOOP AT lt_sdm_lic ASSIGNING <sdm_lic>.
    MOVE-CORRESPONDING <sdm_lic> TO ls_sdm_lic_alv.
    SELECT SINGLE sdm_description FROM /gda/sdm_obj_tx INTO ls_sdm_lic_alv-sdm_object_desc
                            WHERE spras = sy-langu
                              AND sdm_object_id = <sdm_lic>-sdm_object_id.

    READ TABLE lt_modules ASSIGNING <sdm_mod> WITH KEY domvalue_l = <sdm_lic>-sdm_module.
    IF sy-subrc = 0.
      ls_sdm_lic_alv-sdm_module_desc = <sdm_mod>-ddtext.
    ENDIF.

* Check Dates
    IF sy-datum NOT BETWEEN ls_sdm_lic_alv-start_date AND ls_sdm_lic_alv-end_date.
      PERFORM highlight_cell USING 'START_DATE'
                             CHANGING ls_sdm_lic_alv.

      PERFORM highlight_cell USING 'END_DATE'
                             CHANGING ls_sdm_lic_alv.
    ENDIF.

    APPEND ls_sdm_lic_alv TO lt_sdm_lic_alv.
    CLEAR:
     ls_sdm_lic_alv.
  ENDLOOP.

  PERFORM alv_display.

*&---------------------------------------------------------------------*
*&      Form  GET_MODULE_DESCRIPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_module_descriptions USING xt_modules TYPE STANDARD TABLE.
  DATA:
    lt_taba TYPE STANDARD TABLE OF dd07v,
    lt_tabb TYPE STANDARD TABLE OF dd07v.

  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = '/GDA/SDM_D_MODULE_TYPE'
      langu         = sy-langu
      withtext      = 'X'
    TABLES
      dd07v_tab_a   = lt_taba
      dd07v_tab_n   = lt_tabb
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    xt_modules[] = lt_taba[].
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HIGHLIGHT_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_SDM_LIC_ALV  text
*----------------------------------------------------------------------*
FORM highlight_cell USING    p_field type fieldname
                    CHANGING p_sdm_lic_alv TYPE /gda/sdm_s_license_keys_alv.
  DATA:
     ls_color       TYPE lvc_s_scol.

  ls_color-fname     = p_field.
  ls_color-color-col = col_negative.
  ls_color-color-int = 0.
  ls_color-color-inv = 0.
  APPEND ls_color TO p_sdm_lic_alv-color.
  CLEAR  ls_color.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display .
  DATA:
    lo_table      TYPE REF TO cl_salv_table,
    lo_columns    TYPE REF TO cl_salv_columns_table,
    lo_column     TYPE REF TO cl_salv_column_table,
    lo_sorts      TYPE REF TO cl_salv_sorts,
    lv_short(10),
    lv_medium(20).

*...create an ALV table
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_table
        CHANGING
          t_table      = lt_sdm_lic_alv[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.


  lo_columns = lo_table->get_columns( ).
  lo_columns->set_optimize( abap_true ).

* Set color column
  TRY.
      CALL METHOD lo_columns->set_color_column
        EXPORTING
          value = 'COLOR'.
    CATCH cx_salv_data_error.
      MESSAGE TEXT-009 TYPE 'E'.
  ENDTRY.

* Hide Columns
  TRY.
      lo_column ?= lo_columns->get_column( columnname = 'SDM_OBJECT_ID' ).
    CATCH cx_salv_not_found .
      MESSAGE TEXT-009 TYPE 'E'.
  ENDTRY.

  lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).

  TRY.
      lo_column ?= lo_columns->get_column( columnname = 'SDM_MODULE' ).
    CATCH cx_salv_not_found .
      MESSAGE TEXT-009 TYPE 'E'.
  ENDTRY.

  lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).


* Column Headings
  TRY.
      lv_short = TEXT-001.
      lv_medium = TEXT-001.

      lo_column ?= lo_columns->get_column( 'SDM_OBJECT_DESC' ).
      lo_column->set_short_text( lv_short ).
      lo_column->set_medium_text( lv_medium ).
      lo_column->set_long_text( TEXT-001 ).

    CATCH cx_salv_not_found.
      MESSAGE TEXT-009 TYPE 'E'.
  ENDTRY.

  TRY.
      lv_short  = TEXT-002.
      lv_medium = TEXT-002.

      lo_column ?= lo_columns->get_column( 'SDM_MODULE_DESC' ).
      lo_column->set_short_text( lv_short ).
      lo_column->set_medium_text( lv_medium ).
      lo_column->set_long_text( TEXT-002 ).
    CATCH cx_salv_not_found.
      MESSAGE TEXT-009 TYPE 'E'.
  ENDTRY.

  TRY.
      lv_short  = TEXT-004.
      lv_medium = TEXT-004.

      lo_column ?= lo_columns->get_column( 'START_DATE' ).
      lo_column->set_short_text( lv_short ).
      lo_column->set_medium_text( lv_medium ).
      lo_column->set_long_text( TEXT-004 ).
    CATCH cx_salv_not_found.
      MESSAGE TEXT-009 TYPE 'E'.
  ENDTRY.

  TRY.
      lv_short  = TEXT-005.
      lv_medium = TEXT-005.

      lo_column ?= lo_columns->get_column( 'END_DATE' ).
      lo_column->set_short_text( lv_short ).
      lo_column->set_medium_text( lv_medium ).
      lo_column->set_long_text( TEXT-005 ).
    CATCH cx_salv_not_found.
      MESSAGE TEXT-009 TYPE 'E'.
  ENDTRY.

* Sort Output by CLient and SDM Object and  SDM Module
  lo_sorts = lo_table->get_sorts( ).
  TRY.

      lo_sorts->add_sort( 'MANDT' ).

      lo_sorts = lo_table->get_sorts( ).
      lo_sorts->add_sort( 'SDM_OBJECT_DESC' ).

      lo_sorts = lo_table->get_sorts( ).
      lo_sorts->add_sort( 'SDM_MODULE_DESC' ).
    CATCH cx_salv_not_found .
      MESSAGE TEXT-009 TYPE 'E'.
    CATCH cx_salv_existing .
      MESSAGE TEXT-009 TYPE 'E'.
    CATCH cx_salv_data_error .
      MESSAGE TEXT-009 TYPE 'E'.
  ENDTRY.

*... display the table
  lo_table->display( ).

ENDFORM.
