*&---------------------------------------------------------------------*
*& Report /GDA/SDM_MM_ARTICLE_EXC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_mat_mat_exc2 MESSAGE-ID /gda/sdm_pp1.

INCLUDE /gda/sdm_mm_material_lcl.
INCLUDE /gda/sdm_mm_mat_data.
INCLUDE /gda/sdm_mm_mat_obj_data.
INCLUDE /gda/sdm_mm_mat_select_screen.
INCLUDE /gda/sdm_include_sdm_scr3.
INCLUDE /gda/sdm_mm_mat_f01.
INCLUDE /gda/sdm_common_core.

INITIALIZATION.
  PERFORM auth_check USING gc_auth_e.
  PERFORM get_sdm_types USING gc_material.

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


START-OF-SELECTION.

  gv_type   = /gda/sdm_cl_core=>mc_validation.
  gv_source = /gda/sdm_cl_core=>mc_rep.

  IF go_selection IS INITIAL.
    go_selection = lcl_mm_material_exc=>factory( ).
  ENDIF.

  PERFORM selection_screen.
  CHECK gv_execute_report = abap_true.
  TRY.
      go_selection->main( ).
    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
      gv_message = gx_sdm_root->mv_text.
      IF sy-batch = abap_true.
        WRITE: / gv_message.
      ELSE.
        MESSAGE gv_message TYPE 'I'.
      ENDIF.
      RETURN.
  ENDTRY.

  PERFORM set_alv_data_new.

END-OF-SELECTION.
  CHECK gv_execute_report = abap_true.
  IF gt_sdm_material[] IS NOT INITIAL.
    PERFORM display_results_basic USING gt_sdm_material.
  ELSE.
    IF gv_config_err = abap_true.
      MESSAGE s005.
    ELSE.
      MESSAGE s006.
    ENDIF.
  ENDIF.
