*&---------------------------------------------------------------------*
*& Report /GDA/SDM_BOM_REC_STAT_REP1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_bom_rec_stat_rep1 MESSAGE-ID /gda/sdm_pp1.

*CLASS: lcl_event_rec DEFINITION DEFERRED.
*
*DATA:
*  go_handler_local            TYPE REF TO lcl_event_rec.
*
*INCLUDE /gda/fordata.
*INCLUDE /gda/sdm_mm_bom_lcl.
*INCLUDE /gda/sdm_mm_bom_data.
*INCLUDE /gda/sdm_mm_bom_obj_data_rep.
*
*INCLUDE /gda/sdm_include_bom_scr1.
*INCLUDE /gda/sdm_mm_bom_select_screen.
*INCLUDE /gda/sdm_include_sdm_scr2.
*INCLUDE /gda/sdm_include_sdm_scr3.
*
*INCLUDE /gda/sdm_mm_bom_lcl_class.
*INCLUDE /gda/sdm_mm_rsr_bom_f01.
*INCLUDE /gda/sdm_common_core.
*INCLUDE /gda/sdm_common_core_rsr.
*INCLUDE /gda/sdm_mod_core.
*
*INITIALIZATION.
*  PERFORM auth_check USING gc_auth_rsr.
*  PERFORM init USING gc_bom.
*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM screen_output.
*
*AT SELECTION-SCREEN.
*  PERFORM at_selection_screen.
*
*START-OF-SELECTION.
*
*  gv_type   = /gda/sdm_cl_core=>mc_validation.
*  gv_source = /gda/sdm_cl_core=>mc_rep.
*
*  IF go_selection IS INITIAL.
*    go_selection = lcl_mm_bom_exc=>factory( ).
*  ENDIF.
*
*  PERFORM selection_screen.
*  CHECK gv_execute_report = abap_true.
*  TRY.
*      go_selection->main( ).
*    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
*      gv_message = gx_sdm_root->mv_text.
*      IF sy-batch = abap_true.
*        WRITE: / gv_message.
*      ELSE.
*        MESSAGE gv_message TYPE 'I'.
*      ENDIF.
*      RETURN.
*  ENDTRY.
*
*  PERFORM set_alv_data_new.
*
*END-OF-SELECTION.
*  CHECK gv_execute_report = abap_true.
*  IF <dyn_table>[] IS NOT INITIAL.
*    PERFORM display_results USING gt_sdm_bom
*                                  gc_bom.
*  ELSE.
*    IF gv_config_err = abap_true.
*      MESSAGE s005.
*    ELSE.
*      MESSAGE s006.
*    ENDIF.
*  ENDIF.
