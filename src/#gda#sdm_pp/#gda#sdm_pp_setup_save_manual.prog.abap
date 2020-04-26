*&---------------------------------------------------------------------*
*& Report /GDA/SDM_PP_SETUP_SAVE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_pp_setup_save_manual.
PARAMETERS:
 p_obj TYPE /gda/sdm_setup3-object_type DEFAULT 'MATERIAL'.

PERFORM after_save.

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
    lv_prg_name(30).

  FIELD-SYMBOLS:
    <main> LIKE LINE OF lt_main.

  CASE p_obj.
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



  SELECT * FROM /gda/sdm_setup3 INTO TABLE lt_main
    WHERE object_type = p_obj
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
  IF lv_prg_name IS NOT INITIAL.
    INSERT REPORT lv_prg_name FROM lv_src PROGRAM TYPE 'I'.
    COMMIT WORK.
  ENDIF.

*  GENERATE REPORT lc_prg_name
*    MESSAGE lv_msg LINE lv_line WORD lv_word OFFSET lv_off.


ENDFORM.
