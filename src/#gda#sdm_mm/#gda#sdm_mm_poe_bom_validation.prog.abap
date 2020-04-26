**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MM_POE_BOM_VALIDATION
**&---------------------------------------------------------------------*
*
*INCLUDE /gda/sdm_mm_poe_bom_data.
*INCLUDE /gda/sdm_mm_bom_obj_data_rep.
*
*DATA:
*  ls_stko_sdm TYPE stko,
*  ls_stas_sdm TYPE stas,
*  ls_stpo_sdm TYPE stpo,
*  ls_mastb TYPE cs01_mastb,
*  ls_stasb TYPE cs01_stasb,
*  ls_stkob TYPE cs01_stkob,
*  ls_stpob TYPE cs01_stpob.
*
** Populate
**GS_MAST_SDM
**GT_STAS_SDM
**GT_STKO_SDM
**GT_STPO_SDM
*
*
*READ TABLE delta_mastb INTO ls_mastb  INDEX 1.
*MOVE-CORRESPONDING ls_mastb TO gs_mast_sdm.
*
*LOOP AT delta_stasb INTO ls_stasb.
*  MOVE-CORRESPONDING ls_stasb TO ls_stas_sdm.
*  APPEND ls_stas_sdm TO gt_stas_sdm.
*  CLEAR ls_stas_sdm.
*ENDLOOP.
*
*LOOP AT delta_stkob INTO ls_stkob.
*  MOVE-CORRESPONDING ls_stkob TO ls_stko_sdm.
*  APPEND ls_stko_sdm TO gt_stko_sdm.
*  CLEAR ls_stko_sdm.
*ENDLOOP.
*
*LOOP AT delta_stpob INTO ls_stpob.
*  MOVE-CORRESPONDING ls_stpob TO ls_stpo_sdm.
*  APPEND ls_stpo_sdm TO gt_stpo_sdm.
*  CLEAR ls_stpo_sdm.
*ENDLOOP.
*
** Set default SDM Type and include any customr SDM Types
*gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gc_val
*                                                     x_sdm_source = gc_poe ).
*
*** set to default SDM type
**gs_sdm_type-sign   =  'I'.
**gs_sdm_type-option =  'EQ'.
**gs_sdm_type-low    =  gc_val.
**APPEND gs_sdm_type TO gr_sdm_type.
**
**TRY.
**    GET BADI sdm_handle
**      FILTERS
**        sdm_type_main = gc_val.
**
**  CATCH cx_badi_not_implemented.
**    CLEAR sdm_handle.
**ENDTRY.
**
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
*
*
** build a list of all the relevant SDM objects for BOM - Validations
*SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
*  WHERE  sdm_object  = gc_bom
*   AND   type        IN gr_sdm_type
*   AND   active      = abap_true.
*
*CHECK gt_objects[] IS NOT INITIAL.
*BREAK rroelofse.
*LOOP AT gt_objects ASSIGNING <objects>.
*  TRY.
*      <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_bom
*                                                     iv_source      = gc_poe
*                                                     iv_type        = <objects>-type
*                                                     iv_stats       = abap_true ).
*    CATCH cx_fdt_input INTO gx_fdt.
*
*      IF <objects>-object IS NOT INITIAL.
*        <objects>-object->display_messages( ).
*        EXIT.
*      ENDIF.
*  ENDTRY.
*
*
**  CHECK <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS INITIAL.
*  CHECK <objects> IS ASSIGNED.
*  CHECK <OBJECTS>-OBJECT IS BOUND.
*  CHECK <objects>-object->is_active( ) = abap_true
*    AND <objects>-object->mt_message[] IS INITIAL.
*
*  gt_attributes = <objects>-object->get_object_attributes( iv_type = <objects>-type   ).
*
*  LOOP AT gt_attributes ASSIGNING <attribute>.
*    ASSIGN (<attribute>-abap_type) TO <set_data>.
*    <objects>-object->set_selection( iv_name = <attribute>-name
*                                     iv_data = <set_data>
*                                     iv_type = <attribute>-type ).
*  ENDLOOP.
*
*  TRY.
*      <objects>-object->main( ).
*    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
*      gv_message = gx_sdm_root->mv_text.
*      IF sy-batch = abap_true.
*        WRITE: / gv_message.
*      ELSE.
*        MESSAGE gv_message TYPE 'I'.
*      ENDIF.
*      RETURN.
*    CATCH cx_fdt_input INTO gx_fdt.
*      CALL METHOD gx_fdt->if_message~get_longtext
*        RECEIVING
*          result = gv_message.
*      IF sy-batch = abap_true.
*        WRITE: / gv_message.
*      ELSE.
*        MESSAGE gv_message TYPE 'I'.
*      ENDIF.
*      RETURN.
*
*  ENDTRY.
*
*  gr_data = <objects>-object->return_brf_result( ).
*  ASSIGN gr_data->* TO <results_val>.
*
*  IF <results_val> IS NOT ASSIGNED.
*    RETURN.
*  ENDIF.
*
** collect all the results..
*  IF <results_val_all> IS NOT ASSIGNED.
*    IF <objects>-object IS BOUND.
*      gr_data_empty  = <objects>-object->return_brf_result_structure( ).
*      ASSIGN gr_data_empty->* TO <results_val_all>.
*    ENDIF.
*  ENDIF.
*
*  APPEND LINES OF <results_val> TO <results_val_all>.
*ENDLOOP.
**
**IF <results_val_all> IS NOT ASSIGNED.
**  RETURN.
**ENDIF.
**
*LOOP AT <results_val_all> ASSIGNING <result_val>  WHERE type CA 'EAX'.
*  MESSAGE ID <result_val>-id TYPE <result_val>-type NUMBER <result_val>-number
*   WITH <result_val>-message_v1 <result_val>-message_v2
*        <result_val>-message_v3 <result_val>-message_v4
*   RAISING error_with_message.
*ENDLOOP.
