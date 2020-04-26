**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MM_BOM_F01
**&---------------------------------------------------------------------*
*FORM selection_screen.
***// Populate Selection-Screen Structure
*  gs_selscreen-matnr = s_matnr[].
*  gs_selscreen-werks = s_werks[].
*  gs_selscreen-stlnr = s_stlnr[].
*  gs_selscreen-stlal = s_stlal[].
*  gs_selscreen-datuv = s_datuv[].
*  gs_selscreen-andat = s_andat[].
*  gs_selscreen-loekz = s_loekz[].
*
*  PERFORM check_selection_entries CHANGING gv_selection_fields_entered.
*  PERFORM limit_max_entries CHANGING p_max gv_execute_report.
*
*  IF gv_execute_report = abap_false.
*    RETURN.
*  ENDIF.
*
**  gs_selscreen-msgno    = s_msgnr[].
*  gs_selscreen-max_rows = p_max.
*  go_selection->set_selscreen( is_selscreen = gs_selscreen ).
*ENDFORM.
*
*FORM check_selection_entries CHANGING cv_selection_fields_entered TYPE abap_bool.
*
*  IF gs_selscreen IS INITIAL.
*    cv_selection_fields_entered = abap_false.
*  ELSE.
*    cv_selection_fields_entered = abap_true.
*  ENDIF.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  SET_ALV_DATA_NEW
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM set_alv_data_new .
*  DATA:
*    ro_data       TYPE REF TO data,
*    ro_data_empty TYPE REF TO data,
*    lv_field      TYPE fieldname,
*    lv_count      TYPE p,
*    lv_pur        TYPE p,
*    lv_text       TYPE string,
*    lv_pur_text   TYPE string,
*    lv_exit.
*
*  FIELD-SYMBOLS:
*    <results>         TYPE STANDARD TABLE,
*    <results_pp1>     TYPE STANDARD TABLE,
*    <result>          TYPE any,
*    <field>           TYPE any,
*    <field_alv>       TYPE any,
*    <general_default> LIKE LINE OF gt_default_fields,
*    <objects>         LIKE LINE OF gt_objects.
*
*  PERFORM progress_bar USING TEXT-018.
*
*  DESCRIBE TABLE go_selection->mt_mast LINES lv_count.
*  PERFORM progress_bar USING TEXT-018.
*
*  LOOP AT go_selection->mt_mast INTO go_selection->ms_mast_spec.
*    CLEAR:
*      lv_pur_text,
*      lv_pur,
*      lv_text.
*
*    lv_pur = ( sy-tabix / lv_count ) * 100.
*    lv_pur_text = lv_pur.
*    CONCATENATE 'BRF Rules '(917)  lv_pur_text ' %' INTO lv_text.
*    PERFORM progress_bar USING lv_text.
*
** BRF+ Logic
** Prepare the data for BRF functions - pass to temp tables
*    go_selection->refresh_spec( ).
**            me->mv_spec_stlnr = <fs_mast>-stlnr.
**            me->mv_spec_stlal = <fs_mast>-stlal.
*
*    go_selection->mv_spec_stlnr  = go_selection->ms_mast_spec-stlnr.
*    go_selection->mv_spec_stlal  = go_selection->ms_mast_spec-stlal.
*    go_selection->build_spec( ).
*
*    APPEND go_selection->ms_mast_spec TO gt_mast_sdm[].
*    gt_stko_sdm[] = go_selection->mt_stko_spec[].
*    gt_stas_sdm[] = go_selection->mt_stas_spec[].
*    gt_stpo_sdm[] = go_selection->mt_stpo_spec[].
*
** For each BOM Record process the BRF Functions
*    LOOP AT gt_objects ASSIGNING <objects>.
*      CLEAR:
*       <objects>-object.
*      PERFORM brf_logic  USING    <objects>-type
*                         CHANGING <objects>-object.
*
*      IF <objects>-object IS NOT BOUND OR  <objects>-object->mt_message IS NOT INITIAL.
*        gv_config_err = abap_true.
*        EXIT.
*      ENDIF.
*
*      IF <results> IS NOT ASSIGNED.
*        IF <objects>-object IS BOUND.
*          ro_data_empty  = <objects>-object->return_brf_result_structure( ).
*          ASSIGN ro_data_empty->* TO <results>.
*          REFRESH:
*           <results>.
*        ENDIF.
*      ENDIF.
*
*      IF <objects>-object IS BOUND.
*        ro_data  = <objects>-object->return_brf_result( ).
*        ASSIGN ro_data->* TO <results_pp1>.
*        IF sy-subrc = 0.
*          LOOP AT <results_pp1> ASSIGNING <result>.
**            ASSIGN COMPONENT 'MATNR' OF STRUCTURE <result> TO <key>.
**            <key> = go_selection->ms_mara_spec-matnr.
*
**            ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <result> TO <key_desc>.
**            <key_desc> = go_selection->ms_makt_spec-maktx.
*
*          ENDLOOP.
*
*          APPEND LINES OF <results_pp1> TO <results>.
*        ENDIF.
*      ENDIF.
*
*      gs_instance-type   = <objects>-type.
*      gs_instance-object = <objects>-object.
*      APPEND gs_instance TO gs_sdm_objects-sdm_instances.
*    ENDLOOP.
*
*    CHECK gv_config_err = abap_false.
*
**    IF <results> IS ASSIGNED.
**      SORT <results>.
**      DELETE ADJACENT DUPLICATES FROM <results>.
*
**      IF <results> IS NOT INITIAL.
**        LOOP AT <results> ASSIGNING <result>.
**
**          PERFORM message_filter USING    <result>
**                                 CHANGING lv_exit.
**
**          CHECK lv_exit = abap_false.
**
**          PERFORM message_context_link USING <result>
**                                             gc_bom.
**        ENDLOOP.
**      ENDIF.
**    ENDIF.
*
** Move MAST fields to general alv output
**    MOVE-CORRESPONDING go_selection->ms_mast_spec TO <dyn_wa>.
** Ensure Default/Key fields are populated..
*
**    LOOP AT gt_default_fields ASSIGNING <general_default>.
**      CLEAR:
**       lv_field.
**      CONCATENATE 'KEY_' <general_default>-field INTO lv_field.
**      ASSIGN COMPONENT lv_field OF STRUCTURE <dyn_wa> TO <field_alv>.
**      ASSIGN COMPONENT <general_default>-field OF STRUCTURE gs_mast_sdm TO <field>.
**      CHECK sy-subrc = 0.
**      <field_alv> = <field>.
**    ENDLOOP.
*
**    APPEND go_selection->ms_mast_spec TO gt_mast_sdm[].
**    gt_stko_sdm[] = go_selection->mt_stko_spec[].
**    gt_stas_sdm[] = go_selection->mt_stas_spec[].
**    gt_stpo_sdm[] = go_selection->mt_stpo_spec[].
*
*    gs_sdm_objects-matnr  = go_selection->ms_mast_spec-matnr.
*    gs_sdm_objects-werks  = go_selection->ms_mast_spec-werks.
*    gs_sdm_objects-stlnr  = go_selection->ms_mast_spec-stlnr.
*
**    gs_sdm_objects-mast[] = gt_mast_sdm[].
**    gs_sdm_objects-stko[] = gt_stko_sdm[].
**    gs_sdm_objects-stas[] = gt_stas_sdm[].
**    gs_sdm_objects-stpo[] = gt_stpo_sdm[].
*
**    PERFORM determine_output USING   gs_sdm_objects
**                             CHANGING  gt_sdm_bom.
*
*   append gs_sdm_objects TO gt_sdm_bom.
*    CLEAR gs_sdm_objects.
*
*    REFRESH:
*     gt_mast_sdm,
*     gt_stko_sdm,
*     gt_stas_sdm,
*     gt_stpo_sdm.
*
*    UNASSIGN:
*     <results>,
*     <results_pp1>.
*  ENDLOOP.
**
**  SORT <dyn_table>.
**
**  LOOP AT gt_sdm_articles ASSIGNING <sdm_articles>.
**    UNASSIGN <results>.
**
**    LOOP AT <sdm_articles>-sdm_instances ASSIGNING <instances>.
**      IF <instances>-object IS INITIAL.
**        CONTINUE.
**      ENDIF.
**      IF <results> IS NOT ASSIGNED.
**        ro_data_empty  = <instances>-object->return_brf_result_structure( ).
**        ASSIGN ro_data_empty->* TO <results>.
**        REFRESH:
**         <results>.
**      ENDIF.
**
**      ro_data        = <instances>-object->return_brf_result( ).
**      ASSIGN ro_data->* TO <results_temp>.
**
**      IF <results_temp> IS ASSIGNED.
**        APPEND LINES OF <results_temp> TO <results>.
**      ENDIF.
**    ENDLOOP.
**
**    SORT <results>.
**    DELETE ADJACENT DUPLICATES FROM <results>.
**
**    REFRESH lt_icons.
**
**    LOOP AT <results> ASSIGNING <result>.
**      ASSIGN COMPONENT 'TYPE'     OF STRUCTURE <result> TO <type>.
**      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
**      IF <field> IS ASSIGNED.
**        SPLIT <field> AT '-' INTO lv_table lv_field.
**        ls_icons-field = lv_field.
**      ENDIF.
*** Get key object...
**      ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE <result> TO <brf_key4>.
**      IF <brf_key4> IS ASSIGNED AND <brf_key4> IS NOT INITIAL.
**        ls_icons-brf_key = <brf_key4>.
**      ENDIF.
**
**      IF ls_icons-brf_key IS INITIAL.
**        ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE <result> TO <brf_key5>.
**        IF <brf_key5> IS ASSIGNED  AND <brf_key5> IS NOT INITIAL.
**          ls_icons-brf_key = <brf_key5>.
**        ENDIF.
**      ENDIF.
**
**      ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE <result> TO <brf_key6>.
**      IF <brf_key6> IS ASSIGNED  AND <brf_key6> IS NOT INITIAL.
**        CLEAR: ls_icons-brf_key.
**        CONCATENATE <brf_key5> '/' <brf_key6> INTO ls_icons-brf_key.
**      ENDIF.
**
**      IF ls_icons-brf_key IS INITIAL.
**        ls_icons-brf_key = <sdm_articles>-article.
**      ENDIF.
**
**      CASE <type>.
**        WHEN 'E'.
**          ls_icons-icon = icon_red_light.
**        WHEN 'W'.
**          ls_icons-icon = icon_yellow_light.
**        WHEN OTHERS.
**      ENDCASE.
**
**      APPEND ls_icons TO lt_icons.
**      CLEAR ls_icons.
**    ENDLOOP.
**    <sdm_articles>-icons[] = lt_icons[].
**    REFRESH:
**     lt_icons[].
**  ENDLOOP.
*
*  PERFORM progress_bar USING TEXT-019.
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  BRF_LOGIC
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_0342   text
**      -->P_0343   text
**      -->P_0344   text
**      <--P_<OBJECTS>_OBJECT  text
**----------------------------------------------------------------------*
*FORM brf_logic  USING x_type       TYPE /gda/sdm_de_type
*                CHANGING xo_object TYPE REF TO /gda/sdm_cl_bom.
*  .
*
*  DATA:
*    lx_fdt TYPE REF TO cx_fdt.
*
*  IF xo_object IS INITIAL.
*    TRY.
*        xo_object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_bom
*                                             iv_source         = gv_source
*                                             iv_type           = x_type
*                                             iv_stats          = space
*                                             iv_errors_only    = space ).
*      CATCH cx_fdt_input INTO lx_fdt.
*
*        IF xo_object IS NOT INITIAL.
*          xo_object->display_messages( ).
*          EXIT.
*        ENDIF.
*    ENDTRY.
*  ENDIF.
*
*  CHECK xo_object->mt_message[] IS INITIAL.
*
*  PERFORM set_data "USING x_type "gv_type
*                   CHANGING xo_object.
*
*  TRY.
*      xo_object->main( ).
*    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
*      gv_message = gx_sdm_root->mv_text.
*      IF sy-batch = abap_true.
*        WRITE: / gv_message.
*      ELSE.
*        MESSAGE gv_message TYPE 'W'.
*      ENDIF.
*    CATCH cx_fdt_input INTO gx_fdt.
*      CALL METHOD gx_fdt->if_message~get_longtext
*        RECEIVING
*          result = gv_message.
*      IF sy-batch = abap_true.
*        WRITE: / gv_message.
*      ELSE.
*        MESSAGE gv_message TYPE 'W'.
*      ENDIF.
*  ENDTRY.
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  SET_DATA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_X_TYPE  text
**      <--P_XO_OBJECT  text
**----------------------------------------------------------------------*
*FORM set_data "USING p_type
*              CHANGING xo_object TYPE REF TO /gda/sdm_cl_bom.
*  DATA:
**    ls_mara       TYPE /gda/sdm_s_mara,
*    lt_attributes TYPE STANDARD TABLE OF xo_object->ty_brf_attributes.
*
*  FIELD-SYMBOLS:
*    <attribute> LIKE LINE OF lt_attributes,
*    <data>      TYPE any.
*
*  lt_attributes = xo_object->get_object_attributes( ).
*
*  MOVE-CORRESPONDING go_selection->ms_mast_spec TO gs_mast_sdm.
*
*  LOOP AT lt_attributes ASSIGNING <attribute>.
*    ASSIGN (<attribute>-abap_type) TO <data>.
*    TRY.
*        xo_object->set_selection( iv_name = <attribute>-name  iv_data = <data> ).
*      CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
*        gv_message = gx_sdm_root->mv_text.
*        IF sy-batch = abap_true.
*          WRITE: / gv_message.
*        ELSE.
*          MESSAGE gv_message TYPE 'W'.
*        ENDIF.
*    ENDTRY.
*
*  ENDLOOP.
*
*  IF gv_message IS NOT INITIAL.
*    IF sy-batch = abap_true.
*      WRITE: / gv_message.
*    ELSE.
*      MESSAGE gv_message TYPE 'I'.
*    ENDIF.
*  ENDIF.
*ENDFORM.
