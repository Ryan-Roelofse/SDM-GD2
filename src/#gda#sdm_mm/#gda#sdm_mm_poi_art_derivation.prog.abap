*&---------------------------------------------------------------------*
*&  Include     : /GDA/SDM_MM_ART_DERIVATION                  [MWD-BRF+]
*&  Description : Call BRF+ MM Derivation Function, FN_DERIVE_ARTICLE,
*&                during execution of transactions MM42/MM41. Included
*&                in function module MATERIAL_REFERENCE_RT.  Parameter
*&                ID '/GDA/RMMG1' will contain original Sales Area selected
*&                and is populated in fnct MAIN_PARAMETER_SET_START_RET.
*&                Program will populate derived values back to screen.
*&---------------------------------------------------------------------*

INCLUDE /gda/sdm_mm_poi_art_data.
INCLUDE /gda/sdm_mm_art_object_data.

*data:
*  ls_derive_results TYPE /gda/sdm_cl_brf_mapping=>ty_der_results.

DATA:
*  ls_mara           TYPE /gda/sdm_s_mara,
*  ls_marc           TYPE /gda/sdm_s_marc,
*  ls_mard           TYPE /gda/sdm_s_mard,
*  ls_mbew           TYPE /gda/sdm_s_mbew,
*  ls_mvke           TYPE /gda/sdm_s_mvke,
*  ls_syst           TYPE syst,
*  ls_mlgn           TYPE mlgn,
*  ls_makt           TYPE makt,
*  ls_maw1           TYPE maw1,
*  ls_aktyp          TYPE if_fdt_types=>element_text,
  ls_screen_control TYPE /gda/sdm_s_screen_control,
  lt_screen_control LIKE HASHED TABLE OF ls_screen_control
                         WITH UNIQUE KEY screen_name.

FIELD-SYMBOLS:
  <field> TYPE any,
  <table> TYPE any.
*  <results> TYPE STANDARD TABLE.


FREE MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.
CHECK p_t130m-aktyp <> gc_display.

**WMARA Used
**WMAW1 Used
**WMAKT Used
**WMARC Used
**WMARD Used
**WMVKE Used
**WMLGN Used
**WMBEW Used
**WMLGT
**WMPOP
**WMPGD
**WMFHM
**WMYMS
**WEINA
**WEINE
**WWLK2

gs_syst_sdm = sy.

MOVE-CORRESPONDING wmara TO gs_mara_sdm.
MOVE-CORRESPONDING wmarc TO gs_marc_sdm.
MOVE-CORRESPONDING wmard TO gs_mard_sdm.
MOVE-CORRESPONDING wmbew TO gs_mbew_sdm.
MOVE-CORRESPONDING wmvke TO gs_mvke_sdm.
MOVE-CORRESPONDING wmlgn TO gs_mlgn_sdm.
MOVE-CORRESPONDING wmaw1 TO gs_maw1_sdm.
MOVE-CORRESPONDING wmakt TO gs_makt_sdm.

* Set default SDM Type and include any customr SDM Types
gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gc_der
                                                     x_sdm_source = gc_poe ).

** set to default SDM type
*gs_sdm_type-sign   =  'I'.
*gs_sdm_type-option =  'EQ'.
*gs_sdm_type-low    =  gc_der.
*APPEND gs_sdm_type TO gr_sdm_type.
*
*TRY.
*    GET BADI sdm_handle
*      FILTERS
*        sdm_type_main = gc_der.
*  CATCH cx_badi_not_implemented.
*    CLEAR sdm_handle.
*ENDTRY.
*
*IF NOT sdm_handle IS INITIAL.
*  CALL BADI sdm_handle->add_sdm_type
*    EXPORTING
*      x_source          = gc_poe
*    CHANGING
*      xt_sdm_type       = gr_sdm_type
*    EXCEPTIONS
*      application_error = 1
*      OTHERS            = 2.
*  IF sy-subrc <> 0.
**          MESSAGE e() RAISING application_error.
*  ENDIF.
*ENDIF.
* build a list of all the relevant SDM objects for Article - Validations
SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
  WHERE  sdm_object  = gc_article
   AND   type        IN gr_sdm_type
   AND   active      = abap_true.

CHECK gt_objects[] IS NOT INITIAL.

LOOP AT gt_objects ASSIGNING <objects>.
  TRY.
      <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_article
                                                     iv_source      = gc_poe
                                                     iv_type        = <objects>-type
                                                     iv_stats       = space ).
    CATCH cx_fdt_input INTO gx_fdt.

      IF <objects>-object IS NOT INITIAL.
        <objects>-object->display_messages( ).
        EXIT.
      ENDIF.
  ENDTRY.

  CHECK <objects> IS ASSIGNED.
  CHECK <OBJECTS>-OBJECT IS BOUND.
  CHECK <objects>-object->is_active( ) = abap_true
    AND <objects>-object->mt_message[] IS INITIAL.

  gt_attributes = <objects>-object->get_object_attributes(  iv_type = <objects>-type  ).

  LOOP AT gt_attributes ASSIGNING <attribute>.
    ASSIGN (<attribute>-abap_type) TO <set_data>.
    <objects>-object->set_selection( iv_name = <attribute>-name
                                     iv_data = <set_data>
                                     iv_type = <attribute>-type  ).
  ENDLOOP.

  TRY.
      <objects>-object->main( ).
    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
      gv_message = gx_sdm_root->mv_text.
      IF sy-batch = abap_true.
        WRITE: / gv_message.
      ELSE.
        MESSAGE gv_message TYPE 'I'.
      ENDIF.
      RETURN.
  ENDTRY.

  gr_data = <objects>-object->return_brf_result( ).
  ASSIGN gr_data->* TO <results_der>.

  IF <results_der> IS NOT ASSIGNED.
    RETURN.
  ENDIF.
* collect all the results..
  IF <results_der_all> IS NOT ASSIGNED.
    IF <objects>-object IS BOUND.
      gr_data_empty  = <objects>-object->return_brf_result_structure( ).
      ASSIGN gr_data_empty->* TO <results_der_all>.
    ENDIF.
  ENDIF.

  APPEND LINES OF <results_der> TO <results_der_all>.
ENDLOOP.

CHECK <results_der_all> IS ASSIGNED.
LOOP AT <results_der_all> INTO gs_result_der.
  IF gs_result_der-skip_derivation = space.
    ASSIGN (gs_result_der-table) TO <table>.
    IF sy-subrc <> 0.
      gv_message = |BRF Assignment not set for { gs_result_der-table } { gs_result_der-field }|.
      MESSAGE gv_message TYPE 'W'.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT gs_result_der-field OF STRUCTURE <table> TO <field>.
    IF sy-subrc = 0.
      <field> = gs_result_der-value.
    ELSE.
      gv_message = |BRF Assignment not set for { gs_result_der-table } { gs_result_der-field }|.
      MESSAGE gv_message TYPE 'W'.
    ENDIF.
  ENDIF.
*
*// Fill the screen control internal table
  MOVE-CORRESPONDING gs_result_der TO ls_screen_control.
  IF ls_screen_control-screen_name IS NOT INITIAL.
    INSERT ls_screen_control INTO TABLE lt_screen_control.
  ENDIF.
ENDLOOP.

IF lt_screen_control IS NOT INITIAL.
  EXPORT screen_control = lt_screen_control TO MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.    "Used in /GDA/SDM_MM_MATERIAL_SCR
ENDIF.
*!

*gs_result_der
****  TRANSLATE wmakt-maktx TO UPPER CASE.
****  RETURN.
*
*IF p_t130m-aktyp <> gc_display.
*  IF go_article IS INITIAL.
*    go_article ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_article
*                                             iv_source      = gc_poe
*                                             iv_type        = gc_der
*                                             iv_stats       = abap_false ).
*  ENDIF.
*
*  IF go_article IS INITIAL.
*    gv_is_active = abap_false.
*  ELSE.
*    gv_is_active = go_article->is_active( ).
*  ENDIF.
*
*  IF gv_is_active = abap_false.
*    EXIT.
*  ENDIF.
*
**WMARA Used
**WMAW1 Used
**WMAKT Used
**WMARC Used
**WMARD Used
**WMVKE Used
**WMLGN Used
**WMBEW Used
**WMLGT
**WMPOP
**WMPGD
**WMFHM
**WMYMS
**WEINA
**WEINE
**WWLK2
*
*  ls_syst = sy.
*
*  ls_mlgn = wmlgn.
*  ls_maw1 = wmaw1.
*  ls_makt = wmakt.
*  MOVE-CORRESPONDING wmara TO ls_mara.
*  MOVE-CORRESPONDING wmarc TO ls_marc.
*  MOVE-CORRESPONDING wmard TO ls_mard.
*  MOVE-CORRESPONDING wmbew TO ls_mbew.
*  MOVE-CORRESPONDING wmvke TO ls_mvke.
*
*  go_article->set_selection( iv_name = '/GDA/SDM_S_MARA' iv_data = ls_mara ).
*  go_article->set_selection( iv_name = '/GDA/SDM_S_MARC' iv_data = ls_marc ).
*  go_article->set_selection( iv_name = '/GDA/SDM_S_MARD' iv_data = ls_mard ).
*  go_article->set_selection( iv_name = '/GDA/SDM_S_MBEW' iv_data = ls_mbew ).
*  go_article->set_selection( iv_name = '/GDA/SDM_S_MVKE' iv_data = ls_mvke ).
*  go_article->set_selection( iv_name = 'SYST'            iv_data = ls_syst ).
*
*  TRY.
*      go_article->main( ).
*    CATCH /gda/cx_sdm_exception_handl INTO gx_article.
*      gv_message = gx_article->mv_text.
*      MESSAGE gv_message TYPE 'I'.
*      RETURN.
*    CATCH cx_fdt_input INTO gx_fdt.
*      gv_message = 'BRF Error'.
*      MESSAGE gv_message TYPE 'I'.
*      RETURN.
*  ENDTRY.
*
*  gr_data = go_article->return_brf_result( ).
*
*  ASSIGN gr_data->* TO <results>.
*
*  IF <results> IS ASSIGNED.
*    LOOP AT <results> INTO ls_derive_results.
*      IF ls_derive_results-skip_derivation = space.
*        ASSIGN (ls_derive_results-table) TO <table>.
*
*        IF sy-subrc <> 0.
*          gv_message = |BRF Assignment not set for { ls_derive_results-table } { ls_derive_results-field }|.
*          MESSAGE gv_message TYPE 'W'.
*          RETURN.
*        ENDIF.
*
*        ASSIGN COMPONENT ls_derive_results-field OF STRUCTURE <table> TO <field>.
*        IF sy-subrc = 0.
*          <field> = ls_derive_results-value.
*        ELSE.
*          gv_message = |BRF Assignment not set for { ls_derive_results-table } { ls_derive_results-field }|.
*          MESSAGE gv_message TYPE 'W'.
*        ENDIF.
*      ENDIF.
*
**// Fill the screen control internal table
*      MOVE-CORRESPONDING ls_derive_results TO ls_screen_control.
*      IF ls_screen_control-screen_name IS NOT INITIAL.
*        INSERT ls_screen_control INTO TABLE lt_screen_control.
*      ENDIF.
*    ENDLOOP.
*
*    IF lt_screen_control IS NOT INITIAL.
*      EXPORT screen_control = lt_screen_control TO MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.    "Used in /GDA/SDM_MM_MATERIAL_SCR
*    ENDIF.
*  ENDIF.
*ENDIF.
