*&---------------------------------------------------------------------*
*& Report /GDA/SDM_MM_ARTICLE_EXC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_mm_art_exc1 MESSAGE-ID /gda/sdm_pp1.

*INCLUDE /gda/sdm_mm_article_lcl.
PARAMETERS    :                 p_vari   TYPE slis_vari NO-DISPLAY.


INCLUDE /gda/sdm_mm_poi_art_data.
INCLUDE /gda/sdm_mm_art_obj_data_rep.
INCLUDE /gda/sdm_mm_art_data_decl.
INCLUDE /gda/sdm_mm_art_select_screen.
INCLUDE /gda/sdm_include_sdm_scr3.
INCLUDE /gda/sdm_mm_art_rf01.
INCLUDE /gda/sdm_common_core.

INITIALIZATION.
  PERFORM auth_check USING gc_auth_e.
  PERFORM get_sdm_types USING gc_article.

AT SELECTION-SCREEN.
  ok_code = sy-ucomm.
  IF sy-ucomm = space.
    gs_sscrfields-ucomm = 'ONLI'.
  ELSE.
    gs_sscrfields-ucomm = sy-ucomm.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_STAT1' OR screen-name = 'P_STAT2'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  PERFORM screen_output_art_exc.

START-OF-SELECTION.

  PERFORM get_data.

  PERFORM set_alv_data_new.

  IF p_struc = abap_true.
    PERFORM set_up_relations.
  ENDIF.

END-OF-SELECTION.
  CHECK gv_execute_report = abap_true.
  IF gt_sdm_articles[] IS NOT INITIAL.
    PERFORM display_results_basic USING gt_sdm_articles.

  ELSE.
    IF gv_config_err = abap_true.
      MESSAGE s005.
    ELSE.
      MESSAGE s006.
    ENDIF.
  ENDIF.

INCLUDE /gda/sdm_mm_art_exc1_screenf01.
