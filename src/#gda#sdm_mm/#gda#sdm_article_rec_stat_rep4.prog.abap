
*&---------------------------------------------------------------------*
*& Report /GDA/SDM_ARTICLE_REC_STAT_REP1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_article_rec_stat_rep4  MESSAGE-ID /gda/sdm_pp1.

*CLASS: lcl_event_rec DEFINITION DEFERRED.
*
*DATA:
*  go_handler_local TYPE REF TO lcl_event_rec,
*  go_handler_top   TYPE REF TO lcl_event_rec.
*
*INCLUDE /gda/fordata.
*INCLUDE /gda/sdm_mm_poi_art_data.
*INCLUDE /gda/sdm_mm_art_obj_data_rep.
*INCLUDE /gda/sdm_mm_art_data_decl.
*INCLUDE /gda/sdm_include_mm_scr1.
*INCLUDE /gda/sdm_mm_art_select_screen.
*INCLUDE /gda/sdm_include_sdm_scr2.
*INCLUDE /gda/sdm_include_sdm_scr3.
*INCLUDE /gda/sdm_mm_art_lcl_class.
*INCLUDE /gda/sdm_mm_rsr_art_rf01.
*INCLUDE /gda/sdm_mm_art_mod.
*INCLUDE /gda/sdm_common_core.
*INCLUDE /gda/sdm_common_core_rsr.
*
*INITIALIZATION.
*
*  perform auth_check using gc_auth_rsr.
*  PERFORM init USING gc_article.
*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM screen_output.
*
*AT SELECTION-SCREEN.
*  PERFORM at_selection_screen.
*  p_stat2 = abap_true.
*
*START-OF-SELECTION.
*
*  PERFORM build_structure USING gc_default
*                                gc_article
*                                p_struc.
*
*  PERFORM build_dynamic_itab USING gc_default
*                             CHANGING ro_data.
*
*  PERFORM get_data.
*
*  PERFORM set_alv_data_new.
*
*  IF p_struc = abap_true.
*    PERFORM set_up_relations.
*  ENDIF.
*
**  PERFORM format_final.
*
*END-OF-SELECTION.
*  IF <dyn_table>[] IS NOT INITIAL.
*    PERFORM display_results  USING gt_sdm_articles
*                                   gc_article.
*  ELSE.
*    IF gv_config_err = abap_true.
*      MESSAGE s005.
*    ELSE.
*      MESSAGE s006.
*    ENDIF.
*  ENDIF.
