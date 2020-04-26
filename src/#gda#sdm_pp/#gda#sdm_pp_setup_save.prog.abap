*&---------------------------------------------------------------------*
*& Report /GDA/SDM_PP_SETUP_SAVE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_pp_setup_save.
INCLUDE lsvcmcod.
*PERFORM after_save.

FORM after_save.

  TYPES:
    source_line(72) TYPE c.

  CONSTANTS:
    lc_prg_name(30)         VALUE  '/GDA/SDM_INCLUDE_PP',
    lc_prg_name_mat(30)     VALUE  '/GDA/SDM_INCLUDE_MAT_SCR',
    lc_prg_name_bom(30)     VALUE  '/GDA/SDM_INCLUDE_BOM_SCR',
    lc_prg_name_scustom(30) VALUE  '/GDA/SDM_SCUST_DYN_SCR1'.

  DATA:
    lv_src          TYPE TABLE OF source_line,
    ls_src          TYPE source_line,
    lv_msg(120)     TYPE c,
    lv_line(10)     TYPE c,
    lv_word(10)     TYPE c,
    lv_off(3)       TYPE c,
    lt_main         TYPE STANDARD TABLE OF /gda/sdm_setup3,
    source          TYPE TABLE OF string,
    lv_object       TYPE vclstruc-object,
    lv_error_flag   TYPE vcl_flag_type,
    ls_stp3_v       TYPE /gda/sdm_stp3_v,
    lv_prg_name(30).

  FIELD-SYMBOLS:
    <main>    LIKE LINE OF lt_main.


  lv_object = '/GDA/SDM_STP3_V'.

  PERFORM vcl_set_table_access_for_obj
              USING
                  lv_object
              CHANGING
                 lv_error_flag.

  CHECK lv_error_flag IS INITIAL.

  READ TABLE <vcl_extract> INTO ls_stp3_v INDEX 1.
  CHECK sy-subrc = 0.



*FIELD-SYMBOLS:
*      <fs_lext> type any.
*      READ TABLE extract index 1
*           ASSIGNING <fs_lext>.

  SELECT * FROM /gda/sdm_setup3 INTO TABLE lt_main
    WHERE object_type = ls_stp3_v-object_type
      AND status      = abap_true
      ORDER BY ord.

*  APPEND 'SELECTION-SCREEN BEGIN OF BLOCK B11 WITH FRAME TITLE TEXT-002.' TO SRC.
  LOOP AT lt_main ASSIGNING <main>.
    IF <main>-object_view_o IS NOT INITIAL.
      CONCATENATE 'PARAMETERS:  ' <main>-object_view_o   ' TYPE C AS CHECKBOX DEFAULT ''X''. "Check Plant Extension' INTO ls_src.
      APPEND ls_src TO lv_src.
      CLEAR ls_src.
    ENDIF.
  ENDLOOP.

*APPEND 'SELECTION-SCREEN END OF BLOCK B11.' TO SRC.

*READ REPORT lc_prg_name INTO source.

  CASE ls_stp3_v-object_type.
    WHEN 'ARTICLE'..
      lv_prg_name = lc_prg_name.
    WHEN 'MATERIAL'.
      lv_prg_name = lc_prg_name_mat.
    WHEN 'BOM'.
      lv_prg_name = lc_prg_name_bom.
    WHEN 'SCUSTOM'.
      lv_prg_name = lc_prg_name_scustom.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  INSERT REPORT lv_prg_name FROM lv_src PROGRAM TYPE 'I'.
  COMMIT WORK.


*  GENERATE REPORT lc_prg_name
*    MESSAGE lv_msg LINE lv_line WORD lv_word OFFSET lv_off.


ENDFORM.

FORM check.
  DATA:
    lv_object     TYPE vclstruc-object,
    lv_error_flag TYPE vcl_flag_type,
    ls_obj_v      TYPE /gda/sdm_obj_v.

  BREAK rroelofse.
  lv_object = '/GDA/SDM_OBJ_V'.

  PERFORM vcl_set_table_access_for_obj
              USING
                  lv_object
              CHANGING
                 lv_error_flag.

  CHECK lv_error_flag IS INITIAL.

  READ TABLE <vcl_extract> INTO ls_obj_v INDEX 1.

ENDFORM.
*INCLUDE /gda/_pp_setup_save_savef01.
