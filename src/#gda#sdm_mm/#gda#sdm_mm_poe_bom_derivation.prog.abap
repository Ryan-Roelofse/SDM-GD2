**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MM_POE_BOM_DERIVATION
**&---------------------------------------------------------------------*


*IF syst-tcode = 'CS02'.
*  LOOP AT SCREEN.
*    IF screen-name = 'RC29K-STLBE' OR
*       screen-name = 'RC29P-AUSCH' OR
*       screen-name = 'RC29P-LGORT' OR
*       screen-name = 'RC29P-SANKA'.
*      screen-input = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*    IF screen-name = 'RC29P-SANFE' OR
*       screen-name = 'RC29P-SANIN' OR
*       screen-name = 'RC29P-ERSKZ' OR
*       screen-name = 'RC29P-RVREL'.
*      screen-input = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*
*    IF screen-name = 'RC29P-BEIKZ' OR
*         screen-intensified = '1'.
*    ENDIF.
*  ENDLOOP.
*
** Screen Derivatiosn
*RC29P-AUSCH = '10.00'.
*RC29P-LGORT = '101D'.
*RC29P-MEINS = 'EA'.
*ENDIF.

*INCLUDE /gda/sdm_mm_poi_bom_data.
*INCLUDE /gda/sdm_mm_bom_obj_data_rep.
*
*DATA:
*  ls_screen_control TYPE /gda/sdm_s_screen_control,
*  lt_screen_control LIKE HASHED TABLE OF ls_screen_control
*                         WITH UNIQUE KEY screen_name.
*
*FIELD-SYMBOLS:
*  <field> TYPE any,
*  <table> TYPE any.
**  <results> TYPE STANDARD TABLE.
*
*
**FREE MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.
**CHECK p_t130m-aktyp <> gc_display.
*
** (SAPLCSIO)MASTB
**SAPLCSIO LCSIOFM1 MASTB
**SAPLCSDI LCSDIF07 stko stlsttab
*
**gs_syst_sdm = sy.
**
**MOVE-CORRESPONDING wmara TO gs_mara_sdm.
**MOVE-CORRESPONDING wmarc TO gs_marc_sdm.
**MOVE-CORRESPONDING wmard TO gs_mard_sdm.
**MOVE-CORRESPONDING wmbew TO gs_mbew_sdm.
**MOVE-CORRESPONDING wmvke TO gs_mvke_sdm.
**MOVE-CORRESPONDING wmlgn TO gs_mlgn_sdm.
**MOVE-CORRESPONDING wmaw1 TO gs_maw1_sdm.
**MOVE-CORRESPONDING wmakt TO gs_makt_sdm.
*
*** set to default SDM type
**gs_sdm_type-sign   =  'I'.
**gs_sdm_type-option =  'EQ'.
**gs_sdm_type-low    =  gc_der.
**APPEND gs_sdm_type TO gr_sdm_type.
**
**TRY.
**    GET BADI sdm_handle
**      FILTERS
**        sdm_type_main = gc_der.
**  CATCH cx_badi_not_implemented.
**    CLEAR sdm_handle.
**ENDTRY.
*
** Set default SDM Type and include any customr SDM Types
*gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gc_der
*                                                     x_sdm_source = gc_poe ).
*
**stlsttab
**MASTB
**BREAK-POINT.
**IF NOT sdm_handle IS INITIAL.
**  CALL BADI sdm_handle->add_sdm_type
**    EXPORTING
**      x_source          = gc_poe
**    CHANGING
**      xt_sdm_type       = gr_sdm_type
**    EXCEPTIONS
**      application_error = 1
**      OTHERS            = 2.
**  IF sy-subrc <> 0.
***          MESSAGE e() RAISING application_error.
**  ENDIF.
**ENDIF.
*** build a list of all the relevant SDM objects for Article - Validations
**SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
**  WHERE  sdm_object  = gc_bom
**   AND   type        IN gr_sdm_type
**   AND   active      = abap_true.
**
**CHECK gt_objects[] IS NOT INITIAL.
**
**LOOP AT gt_objects ASSIGNING <objects>.
**  TRY.
**      <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_bom
**                                                     iv_source      = gc_poe
**                                                     iv_type        = <objects>-type
**                                                     iv_stats       = space ).
**    CATCH cx_fdt_input INTO gx_fdt.
**
**      IF <objects>-object IS NOT INITIAL.
**        <objects>-object->display_messages( ).
**        EXIT.
**      ENDIF.
**  ENDTRY.
**
*
**  CHECK <objects> IS ASSIGNED.
**  CHECK <OBJECTS>-OBJECT IS BOUND.
**  CHECK <objects>-object->is_active( ) = abap_true
**    AND <objects>-object->mt_message[] IS INITIAL.
*
***  CHECK <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS INITIAL.
**
**  gt_attributes = <objects>-object->get_object_attributes(  iv_type = <objects>-type  ).
**
**  LOOP AT gt_attributes ASSIGNING <attribute>.
**    ASSIGN (<attribute>-abap_type) TO <set_data>.
**    <objects>-object->set_selection( iv_name = <attribute>-name
**                                     iv_data = <set_data>
**                                     iv_type = <attribute>-type  ).
**  ENDLOOP.
**
***  TRY.
***      <objects>-object->main( ).
***    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
***      gv_message = gx_sdm_root->mv_text.
***      IF sy-batch = abap_true.
***        WRITE: / gv_message.
***      ELSE.
***        MESSAGE gv_message TYPE 'I'.
***      ENDIF.
***      RETURN.
***  ENDTRY.
**
**  gr_data = <objects>-object->return_brf_result( ).
**  ASSIGN gr_data->* TO <results_der>.
**
***  IF <results_der> IS NOT ASSIGNED.
***    RETURN.
***  ENDIF.
**** collect all the results..
***  IF <results_der_all> IS NOT ASSIGNED.
***    IF <objects>-object IS BOUND.
***      gr_data_empty  = <objects>-object->return_brf_result_structure( ).
***      ASSIGN gr_data_empty->* TO <results_der_all>.
***    ENDIF.
***  ENDIF.
***
***  APPEND LINES OF <results_der> TO <results_der_all>.
**ENDLOOP.
***
***CHECK <results_der_all> IS ASSIGNED.
***LOOP AT <results_der_all> INTO gs_result_der.
***  IF gs_result_der-skip_derivation = space.
***    ASSIGN (gs_result_der-table) TO <table>.
***    IF sy-subrc <> 0.
***      gv_message = |BRF Assignment not set for { gs_result_der-table } { gs_result_der-field }|.
***      MESSAGE gv_message TYPE 'W'.
***      RETURN.
***    ENDIF.
***
***    ASSIGN COMPONENT gs_result_der-field OF STRUCTURE <table> TO <field>.
***    IF sy-subrc = 0.
***      <field> = gs_result_der-value.
***    ELSE.
***      gv_message = |BRF Assignment not set for { gs_result_der-table } { gs_result_der-field }|.
***      MESSAGE gv_message TYPE 'W'.
***    ENDIF.
***  ENDIF.
****
****// Fill the screen control internal table
***  MOVE-CORRESPONDING gs_result_der TO ls_screen_control.
***  IF ls_screen_control-screen_name IS NOT INITIAL.
***    INSERT ls_screen_control INTO TABLE lt_screen_control.
***  ENDIF.
***ENDLOOP.
***
***IF lt_screen_control IS NOT INITIAL.
***  EXPORT screen_control = lt_screen_control TO MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.    "Used in /GDA/SDM_MM_MATERIAL_SCR
***ENDIF.
***
