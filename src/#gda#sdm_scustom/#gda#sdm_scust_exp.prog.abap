*&---------------------------------------------------------------------*
*& Report /GDA/SDM_SCUSTOM_EXP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_scust_exp  MESSAGE-ID /gda/sdm_scu.
TABLES:
  scustom.

* SDM Excel Download logic(RSR)
INCLUDE /gda/fordata.
* SDM SCUSTOM data declarations
INCLUDE /gda/sdm_scust_data.
* SDM SCUSTOM object declarations
INCLUDE /gda/sdm_scust_obj_data.
* SDM SCUST specific selections
INCLUDE /gda/sdm_scust_scr1.
* SDM common selections(common)
INCLUDE /gda/sdm_include_sdm_scr3.
* SDM commom logic(common)
INCLUDE /gda/sdm_common_core.
* SDM SCUSTOM logic
INCLUDE /gda/sdm_scust_logic.

INITIALIZATION.
* SDM Initialise(common)
  PERFORM sdm_init_common USING gc_scustom
                       CHANGING gv_object gv_type gv_source go_selection.
* SDM Authority Check(common)
  PERFORM auth_check    USING gc_auth_e.
* SDM Get SDM Types(common)
  PERFORM get_sdm_types USING gc_scustom.

AT SELECTION-SCREEN.
* SDM (common exception report)
  PERFORM at_sel_scrn_excep.

AT SELECTION-SCREEN OUTPUT.
* SDM (common exception report)
  PERFORM at_sel_scrn__output_excep.

START-OF-SELECTION.

  gs_selscreen-id       = s_id[].

* SDM Check Selection Screen(common)
  PERFORM check_selection_entries CHANGING gv_selection_fields_entered.
  PERFORM limit_max_entries       CHANGING p_max gv_execute_report.

  IF gv_execute_report = abap_false.
    RETURN.
  ENDIF.

  gs_selscreen-scustom  = p_scust.
  gs_selscreen-max_rows = p_max.
  go_selection->set_selscreen( xs_selscreen = gs_selscreen ).

  IF gv_execute_report = abap_false.
    RETURN.
  ENDIF.
* SDM Get data(common)
  PERFORM sdm_selection.

* SDM Core logic(common)
  PERFORM sdm_main_scustom.

END-OF-SELECTION.
* SDM Display Results(common exception report)
  PERFORM display_results_excep_report.
