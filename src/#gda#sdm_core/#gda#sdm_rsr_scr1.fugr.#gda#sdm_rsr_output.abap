FUNCTION /gda/sdm_rsr_output.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(XT_MAIN_SETUP) TYPE  /GDA/SDM_T_MAIN
*"  TABLES
*"      XT_SDM_OBJECTS TYPE  TABLE OPTIONAL
*"      XT_SDM_DYN_TABLE TYPE  TABLE OPTIONAL
*"----------------------------------------------------------------------

  gt_pp_main_setup[] = xt_main_setup[].

  GET REFERENCE OF xt_sdm_dyn_table[]  INTO po_sdm_dyn_table.
  GET REFERENCE OF xt_sdm_objects[]    INTO po_sdm_objects.
  ASSIGN po_sdm_dyn_table->* TO <dyn_table>.
  ASSIGN po_sdm_objects->*   TO <dyn_sdm_res>.

  READ TABLE <dyn_sdm_res> ASSIGNING <object> INDEX 1.
  ASSIGN COMPONENT 'SDM_INSTANCES' OF STRUCTURE <object> TO <instances>.
  READ TABLE <instances> ASSIGNING <instance> INDEX 1.
  ASSIGN COMPONENT 'OBJECT' OF STRUCTURE <instance> TO <sdm_object>.

  go_sdm_object = <sdm_object>.
  gv_title      = go_sdm_object->get_object_description( ).

  CALL SCREEN 100.

ENDFUNCTION.
