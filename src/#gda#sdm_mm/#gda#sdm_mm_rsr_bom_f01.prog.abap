**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MM_RSR_BOM_F01
**&---------------------------------------------------------------------*
*
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
*  gs_selscreen-msgno    = s_msgnr[].
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
*  PERFORM build_structure USING gc_default
*                                gc_bom
*                                space.
*
*
*  PERFORM build_dynamic_itab USING gc_default
*                             CHANGING ro_data.
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
** Set all Default Views to icon successful
*    PERFORM default_view_icons.
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
**    PERFORM prep_data USING go_selection->ms_mara_spec.
*
**    UNASSIGN <objects>.
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
*    IF <results> IS ASSIGNED.
*      SORT <results>.
*      DELETE ADJACENT DUPLICATES FROM <results>.
*
*      IF <results> IS NOT INITIAL.
*        LOOP AT <results> ASSIGNING <result>.
*
*          PERFORM message_filter USING    <result>
*                                 CHANGING lv_exit.
*
*          CHECK lv_exit = abap_false.
*
*          PERFORM message_context_link USING <result>
*                                             gc_bom.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
*
** Move MAST fields to general alv output
*    MOVE-CORRESPONDING go_selection->ms_mast_spec TO <dyn_wa>.
** Ensure Default/Key fields are populated..
*
*    LOOP AT gt_default_fields ASSIGNING <general_default>.
*      CLEAR:
*       lv_field.
*      CONCATENATE 'KEY_' <general_default>-field INTO lv_field.
*      ASSIGN COMPONENT lv_field OF STRUCTURE <dyn_wa> TO <field_alv>.
*      ASSIGN COMPONENT <general_default>-field OF STRUCTURE gs_mast_sdm TO <field>.
*      CHECK sy-subrc = 0.
*      <field_alv> = <field>.
*    ENDLOOP.
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
*    gs_sdm_objects-mast[] = gt_mast_sdm[].
*    gs_sdm_objects-stko[] = gt_stko_sdm[].
*    gs_sdm_objects-stas[] = gt_stas_sdm[].
*    gs_sdm_objects-stpo[] = gt_stpo_sdm[].
*
*    PERFORM determine_output USING   gs_sdm_objects
*                             CHANGING  gt_sdm_bom.
*
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
*                CHANGING xo_object TYPE REF TO /gda/sdm_cl_core. "/gda/sdm_cl_bom.
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
*  PERFORM set_data USING x_type "gv_type
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
*FORM set_data  USING p_type        TYPE /gda/sdm_de_type
*              CHANGING xo_object TYPE REF TO /gda/sdm_cl_core." /gda/sdm_cl_bom.
*  DATA:
**    ls_mara       TYPE /gda/sdm_s_mara,
*    lt_attributes TYPE STANDARD TABLE OF xo_object->ty_brf_attributes.
*
*  FIELD-SYMBOLS:
*    <attribute> LIKE LINE OF lt_attributes,
*    <data>      TYPE any.
*
*  lt_attributes = xo_object->get_object_attributes( iv_type = p_type ).
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
**&---------------------------------------------------------------------*
**&      Form  MASS_DOWNLOAD
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM mass_download.
*
*  DATA:
*    lv_template TYPE sy-repid.
*
*  FIELD-SYMBOLS:
*   <articles> LIKE LINE OF gt_sdm_bom.
*
*  PERFORM process_spreadsheet.
*
*  lv_template = '/GDA/SDM_BOM'.
*  EXPORT lv_template TO MEMORY ID 'TEMPLATE'.
**-----Create SAP Document
*  PERFORM create_sapdoc.
*
*ENDFORM.
*
*FORM pop_main.
**  DATA:
**   new,
**   tabix LIKE sy-tabix.
*
** Header
*  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
*  APPEND gs_tab TO gt_tab.
*
*  LOOP AT <dyn_table> ASSIGNING <dyn_wa>.
*    CONCATENATE '<dyn_wa>-' range_item-name INTO name.
*
*    ASSIGN (name) TO <cell>.
*
*    CHECK sy-subrc = 0.
*
*    WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
*
*    APPEND gs_tab TO gt_tab.
*  ENDLOOP.
*
*ENDFORM.
*
*FORM pop_main_details.
*  DATA:
**    new,
**    tabix         LIKE sy-tabix,
*    ro_data       TYPE REF TO data,
*    ro_data_empty TYPE REF TO data.
*
*  FIELD-SYMBOLS:
*    <bom>          LIKE LINE OF gt_sdm_bom,
*    <instances>    LIKE LINE OF <bom>-sdm_instances,
*    <results>      TYPE STANDARD TABLE,
*    <results_temp> TYPE STANDARD TABLE,
*    <result>       TYPE any.
*
** Header
*  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
*  APPEND gs_tab TO gt_tab.
*
*  LOOP AT gt_sdm_bom ASSIGNING <bom>.
*
** Test changes
*    UNASSIGN <results>.
*
*    LOOP AT <bom>-sdm_instances ASSIGNING <instances>.
*      IF <instances>-object IS INITIAL.
*        CONTINUE.
*      ENDIF.
*      IF <results> IS NOT ASSIGNED.
*        ro_data_empty  = <instances>-object->return_brf_result_structure( ).
*        ASSIGN ro_data_empty->* TO <results>.
*        REFRESH:
*         <results>.
*      ENDIF.
*
*      ro_data        = <instances>-object->return_brf_result( ).
*      ASSIGN ro_data->* TO <results_temp>.
*
*      IF <results_temp> IS ASSIGNED  AND <results_temp> IS NOT INITIAL.
*        APPEND LINES OF <results_temp> TO <results>.
*      ENDIF.
*    ENDLOOP.
*
*    SORT <results>.
*    DELETE ADJACENT DUPLICATES FROM <results>.
*
*    LOOP AT <results> ASSIGNING <result>.
*      IF range_item-name = 'MATNR'.
*        name = '<bom>-article'.
*      ELSE.
*        CONCATENATE '<result>-' range_item-name INTO name.
*      ENDIF.
*
*      ASSIGN (name) TO <cell>.
*
*      CHECK sy-subrc = 0.
*
*      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
*
*      APPEND gs_tab TO gt_tab.
*    ENDLOOP.
*
*  ENDLOOP.
*ENDFORM.
*FORM pop_context_details.
*  DATA:
*    ro_data       TYPE REF TO data,
*    ro_data_empty TYPE REF TO data.
*
*  FIELD-SYMBOLS:
*    <bom>          LIKE LINE OF gt_sdm_bom,
*    <instances>    LIKE LINE OF <bom>-sdm_instances,
*    <results>      TYPE STANDARD TABLE,
*    <results_temp> TYPE STANDARD TABLE,
*    <result>       TYPE any.
*
** Header
*  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
*  APPEND gs_tab TO gt_tab.
*
*  LOOP AT gt_sdm_bom ASSIGNING <bom>.
*
*    UNASSIGN <results>.
*
*    LOOP AT <bom>-sdm_instances ASSIGNING <instances>.
*      IF <instances>-object IS INITIAL.
*        CONTINUE.
*      ENDIF.
*      IF <results> IS NOT ASSIGNED.
*        ro_data_empty  = <instances>-object->return_brf_result_structure( ).
*        ASSIGN ro_data_empty->* TO <results>.
*        REFRESH:
*         <results>.
*      ENDIF.
*
*      ro_data        = <instances>-object->return_brf_result( ).
*      ASSIGN ro_data->* TO <results_temp>.
*
*      IF <results_temp> IS ASSIGNED  AND <results_temp> IS NOT INITIAL.
*        APPEND LINES OF <results_temp> TO <results>.
*      ENDIF.
*    ENDLOOP.
*
*    SORT <results>.
*    DELETE ADJACENT DUPLICATES FROM <results>.
** RROELOFSE
**    LOOP AT <results> ASSIGNING <result>.
**      IF range_item-name = 'MATNR'.
**        name = '<articles>-article'.
**      ELSE.
**        CONCATENATE '<result>-' range_item-name INTO name.
**      ENDIF.
**
**      ASSIGN (name) TO <cell>.
**
**      CHECK sy-subrc = 0.
**
**      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
**
**      APPEND gs_tab TO i_ztab.
**    ENDLOOP.
*
*  ENDLOOP.
*
*ENDFORM.
*
*FORM pop_calcs_details.
*  DATA:
*    ro_data           TYPE REF TO data,
*    ro_data_empty     TYPE REF TO data,
*    ro_download_table TYPE REF TO data,
*    ls_calcs1         TYPE /gda/sdm_s_calcs_message,
*    ls_calcs2         TYPE /gda/sdm_s_calcs_mtart,
*    ls_calcs3         TYPE /gda/sdm_s_calcs_mstae,
*    ls_calcs4         TYPE /gda/sdm_s_calcs_matkl,
*    ls_calcs5         TYPE /gda/sdm_s_calcs_attyp,
*    ls_wgbez60        TYPE wgbez60.
*
*
*  FIELD-SYMBOLS:
*    <bom>            LIKE LINE OF gt_sdm_bom,
*    <instances>      LIKE LINE OF <bom>-sdm_instances,
*    <results>        TYPE STANDARD TABLE,
*    <results_temp>   TYPE STANDARD TABLE,
*    <result>         TYPE any,
*    <type>           TYPE any,
*    <id>             TYPE any,
*    <number>         TYPE any,
*    <message>        TYPE any,
*    <calcs1>         LIKE LINE OF gt_calcs1,
*    <calcs2>         LIKE LINE OF gt_calcs2,
*    <calcs3>         LIKE LINE OF gt_calcs3,
*    <calcs4>         LIKE LINE OF gt_calcs4,
*    <calcs5>         LIKE LINE OF gt_calcs5,
*    <field>          TYPE any,
*    <table_download> TYPE ANY TABLE.
*
** Header
*  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
*  APPEND gs_tab TO gt_tab.
*
*  IF gt_calcs1 IS INITIAL.
*    LOOP AT gt_sdm_bom ASSIGNING <bom>.
*
*      UNASSIGN <results>.
*
*      LOOP AT <bom>-sdm_instances ASSIGNING <instances>.
*        IF <instances>-object IS INITIAL.
*          CONTINUE.
*        ENDIF.
*        IF <results> IS NOT ASSIGNED.
*          ro_data_empty  = <instances>-object->return_brf_result_structure( ).
*          ASSIGN ro_data_empty->* TO <results>.
*          REFRESH:
*           <results>.
*        ENDIF.
*
*        ro_data        = <instances>-object->return_brf_result( ).
*        ASSIGN ro_data->* TO <results_temp>.
*
*        IF <results_temp> IS ASSIGNED  AND <results_temp> IS NOT INITIAL.
*          APPEND LINES OF <results_temp> TO <results>.
*        ENDIF.
*      ENDLOOP.
*
*      SORT <results>.
*      DELETE ADJACENT DUPLICATES FROM <results>.
*
*      LOOP AT <results> ASSIGNING <result>.
*        ASSIGN COMPONENT 'ID'      OF STRUCTURE <result> TO <id>.
*        ASSIGN COMPONENT 'NUMBER'  OF STRUCTURE <result> TO <number>.
*
*        SELECT SINGLE text FROM t100
*                           INTO ls_calcs1-message
*                      WHERE sprsl = sy-langu
*                        AND arbgb = <id>
*                        AND msgnr = <number>.
*        IF sy-subrc = 0.
*          CONCATENATE <number> '-' ls_calcs1-message INTO ls_calcs1-message.
*          ls_calcs1-count1  = '1'.
*          COLLECT ls_calcs1 INTO gt_calcs1.
*          CLEAR ls_calcs1.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*
*    SORT gt_calcs1 BY count1 DESCENDING.
*  ENDIF.
*
*  CREATE DATA ro_download_table LIKE <dyn_table>.
*
*  ASSIGN ro_download_table->* TO <table_download>.
*  <table_download>[] = <dyn_table>[].
*
**  ASSIGN <dyn_table> TO <table_download>.
**  SORT <table_download> BY ('KEY_MTART').
*
*  IF gt_calcs2 IS INITIAL.
**    SORT <table_download> BY ('KEY_MTART').
*    LOOP AT <table_download> ASSIGNING <dyn_wa>.
*      ASSIGN COMPONENT 'KEY_MTART' OF STRUCTURE <dyn_wa> TO <field>.
*      ls_calcs2-count2  = '1'.
*      ls_calcs2-key_mtart  = <field>.
*      COLLECT ls_calcs2 INTO gt_calcs2.
*      CLEAR ls_calcs2.
*    ENDLOOP.
*    SORT gt_calcs2 BY count2 DESCENDING.
*  ENDIF.
*
*  IF gt_calcs3 IS INITIAL.
**    SORT <table_download> BY ('MSTAE').
*    LOOP AT <table_download> ASSIGNING <dyn_wa>.
*      ASSIGN COMPONENT 'MSTAE' OF STRUCTURE <dyn_wa> TO <field>.
*      ls_calcs3-count3  = '1'.
*      ls_calcs3-mstae  = <field>.
*      COLLECT ls_calcs3 INTO gt_calcs3.
*      CLEAR ls_calcs3.
*    ENDLOOP.
*    SORT gt_calcs3 BY count3 DESCENDING.
*  ENDIF.
*
*  IF gt_calcs4 IS INITIAL.
*    SORT <table_download> BY ('KEY_MATKL').
*    LOOP AT <table_download> ASSIGNING <dyn_wa>.
*      ASSIGN COMPONENT 'KEY_MATKL' OF STRUCTURE <dyn_wa> TO <field>.
*      ls_calcs4-count4  = '1'.
*      ls_calcs4-key_matkl  = <field>.
*      COLLECT ls_calcs4 INTO gt_calcs4.
*      CLEAR ls_calcs4.
*    ENDLOOP.
*
*    LOOP AT gt_calcs4 ASSIGNING <calcs4>.
*      SELECT SINGLE wgbez FROM t023t INTO ls_wgbez60
*           WHERE spras = sy-langu
*             AND matkl = <calcs4>-key_matkl.
*      CHECK sy-subrc = 0.
*      CONCATENATE <calcs4>-key_matkl '-' ls_wgbez60 INTO <calcs4>-key_matkl.
*    ENDLOOP.
*
*    SORT gt_calcs4 BY count4 DESCENDING.
*  ENDIF.
*
*  IF gt_calcs5 IS INITIAL.
**    SORT <table_download> BY ('KEY_ATTYP').
*    LOOP AT <table_download> ASSIGNING <dyn_wa>.
*      ASSIGN COMPONENT 'KEY_ATTYP' OF STRUCTURE <dyn_wa> TO <field>.
*      ls_calcs5-count5  = '1'.
*      ls_calcs5-key_attyp  = <field>.
*      COLLECT ls_calcs5 INTO gt_calcs5.
*      CLEAR ls_calcs5.
*    ENDLOOP.
*
*    SORT gt_calcs5 BY count5 DESCENDING.
*  ENDIF.
*
*
*  IF range_item-name = 'MESSAGE' OR range_item-name = 'COUNT1'.
*    LOOP AT gt_calcs1 ASSIGNING <calcs1>.
*      CONCATENATE '<calcs1>-' range_item-name INTO name.
*      ASSIGN (name) TO <cell>.
*      CHECK sy-subrc = 0.
*      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
*      IF range_item-name CS 'COUNT1'.
*        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
*        REPLACE ALL OCCURRENCES OF ',' IN gs_tab WITH '' .
*      ENDIF..
*      APPEND gs_tab TO gt_tab.
*    ENDLOOP.
*  ENDIF.
*
*  IF range_item-name = 'KEY_MTART' OR range_item-name = 'COUNT2'.
*    LOOP AT gt_calcs2 ASSIGNING <calcs2>.
*      CONCATENATE '<calcs2>-' range_item-name INTO name.
*      ASSIGN (name) TO <cell>.
*      CHECK sy-subrc = 0.
*      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
*      IF range_item-name CS 'COUNT2'.
*        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
*        REPLACE ALL OCCURRENCES OF ',' IN gs_tab WITH '' .
*      ENDIF..
*      APPEND gs_tab TO gt_tab.
*    ENDLOOP.
*  ENDIF.
*
*  IF range_item-name = 'MSTAE' OR range_item-name = 'COUNT3'.
*    LOOP AT gt_calcs3 ASSIGNING <calcs3>.
*      CONCATENATE '<calcs3>-' range_item-name INTO name.
*      ASSIGN (name) TO <cell>.
*      CHECK sy-subrc = 0.
*      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
*      IF range_item-name CS 'COUNT3'.
*        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
*        REPLACE ALL OCCURRENCES OF ',' IN gs_tab WITH '' .
*      ENDIF.
*      APPEND gs_tab TO gt_tab.
*    ENDLOOP.
*  ENDIF.
*
*  IF range_item-name = 'KEY_MATKL' OR range_item-name = 'COUNT4'.
*    LOOP AT gt_calcs4 ASSIGNING <calcs4>.
*      CONCATENATE '<calcs4>-' range_item-name INTO name.
*      ASSIGN (name) TO <cell>.
*      CHECK sy-subrc = 0.
*      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
*      IF range_item-name CS 'COUNT4'.
*        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
*        REPLACE ALL OCCURRENCES OF ',' IN gs_tab WITH '' .
*      ENDIF.
*      APPEND gs_tab TO gt_tab.
*    ENDLOOP.
*  ENDIF.
*
*  IF range_item-name = 'KEY_ATTYP' OR range_item-name = 'COUNT5'.
*    LOOP AT gt_calcs5 ASSIGNING <calcs5>.
*      CONCATENATE '<calcs5>-' range_item-name INTO name.
*      ASSIGN (name) TO <cell>.
*      CHECK sy-subrc = 0.
*      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.
*      IF range_item-name CS 'COUNT5'.
*        REPLACE ALL OCCURRENCES OF '.' IN gs_tab WITH '' .
*        REPLACE ALL OCCURRENCES OF ',' IN gs_tab WITH '' .
*      ENDIF..
*      APPEND gs_tab TO gt_tab.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  PROCESS_SPREADSHEET
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM process_spreadsheet .
**-----Populate main sheet
*  PERFORM pop_main_sheet.
**-----Populate main sheet
*  PERFORM pop_details_sheet.
**-----Populate context sheet
*  PERFORM pop_context_sheet.
**-----Populate Calcs sheet
*  PERFORM pop_calcs_sheet.
*
*ENDFORM.
*
*FORM pop_calcs_sheet.
*  DATA:
*   fldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
*
*  FIELD-SYMBOLS:
*   <fieldsymbol> LIKE LINE OF <main_setup>-tabstruc[].
*
**-----Context - 3nd sheet
*  MOVE 'CALCS' TO v_sheet.
*
**------Starting row
*  MOVE '1' TO v_row.
*
**-----Starting column
*  MOVE '0' TO v_col.
*
*  PERFORM build_partial_cat USING space
*                                  space
*                                  '/GDA/SDM_S_CALCS'
*                                  space.
*
*  fieldcat[] = gt_fldcat[].
*
*  PERFORM load_fieldcat.
*
*ENDFORM.
*
*FORM build_partial_cat USING prog_name
*                             tabname
*                             struct
*                             include.
*
*  REFRESH gt_fldcat.
*  CLEAR gt_fldcat.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name         = prog_name
*      i_internal_tabname     = tabname
*      i_structure_name       = struct
*      i_inclname             = include
*      i_client_never_display = 'X'
*    CHANGING
*      ct_fieldcat            = gt_fldcat[]
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " BUILD_PARTIAL_CAT
*
*FORM pop_context_sheet.
*  DATA:
*   fldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
*
*  FIELD-SYMBOLS:
*   <fieldsymbol> LIKE LINE OF <main_setup>-tabstruc[].
*
**-----Context - 3nd sheet
*  MOVE 'CONTEXT' TO v_sheet.
*
**------Starting row
*  MOVE '1' TO v_row.
*
**-----Starting column
*  MOVE '0' TO v_col.
*
*  LOOP AT <main_setup>-tabstruc ASSIGNING <fieldsymbol>.
*    MOVE-CORRESPONDING <fieldsymbol> TO fldcat.
*    APPEND fldcat.
*    CLEAR fldcat.
*  ENDLOOP.
*
*  fieldcat[] = fldcat[].
*
*  PERFORM load_fieldcat.
*
*ENDFORM.
*
*FORM pop_details_sheet .
*
*  PERFORM build_partial_cat USING space
*                                  space
*                                  '/GDA/SDM_S_VAL_RESULTS_KEY'
*                                  space.
*
*
**-----Main - 2nd sheet
*  MOVE 'DETAILS' TO v_sheet.
*
**------Starting row
*  MOVE '1' TO v_row.
*
**-----Starting column
*  MOVE '0' TO v_col.
*
*  fieldcat[] = gt_fldcat[].
*
*  PERFORM load_fieldcat.
*
*ENDFORM.
*
*FORM pop_main_sheet.
*  DATA:
*    fldcat   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
*    lt_views TYPE STANDARD TABLE OF /gda/sdm_setup3.
*
*  FIELD-SYMBOLS:
*    <fieldsymbol> LIKE LINE OF <main_setup>-tabstruc[],
*    <fieldname>   TYPE any,
*    <views>       LIKE LINE OF lt_views.
*
**-----Main - 1st sheet
*  MOVE 'DATA' TO v_sheet.
*
**------Starting row
*  MOVE '1' TO v_row.
*
**-----Starting column
*  MOVE '0' TO v_col.
*
*  LOOP AT <main_setup>-tabstruc ASSIGNING <fieldsymbol>.
*
*    MOVE-CORRESPONDING <fieldsymbol> TO fldcat.
*    APPEND fldcat.
*    CLEAR fldcat.
*    ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <fieldsymbol> TO <fieldname>.
*    IF <fieldname> = 'LINKAGE'.
** Display All Views in Spreedsheet and hide ones not populated.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*
*  SELECT * FROM /gda/sdm_setup3 INTO TABLE lt_views
*           WHERE object_type = <main_setup>-object_type
*            AND  object_view <> 'DEFAULT'
*            ORDER BY ord.
*
*
*  LOOP AT lt_views ASSIGNING <views>.
*    fldcat-fieldname = <views>-object_view.
*    APPEND fldcat.
*    CLEAR fldcat.
*  ENDLOOP.
*
*  LOOP AT lt_views ASSIGNING <views>.
*    READ TABLE <main_setup>-tabstruc WITH KEY fieldname = <views>-object_view TRANSPORTING NO FIELDS.
*    CHECK sy-subrc <> 0.
*    hide_columns-sheet = 'DATA'.
*    hide_columns-index = <views>-ord + 11.
*    hide_columns-view  = <views>-object_view.
*    APPEND hide_columns.
*    CLEAR hide_columns.
*  ENDLOOP.
*
*  fieldcat[] = fldcat[].
*
*  PERFORM load_fieldcat.
*
*ENDFORM.
*
**FORM pop_details_sheet .
**
**  PERFORM build_partial_cat USING space
**                                  space
**                                  '/GDA/SDM_S_VAL_RESULTS_KEY'
**                                  space.
**
**
***-----Main - 2nd sheet
**  MOVE 'DETAILS' TO v_sheet.
**
***------Starting row
**  MOVE '1' TO v_row.
**
***-----Starting column
**  MOVE '0' TO v_col.
**
**  fieldcat[] = gt_fldcat[].
**
**  PERFORM load_fieldcat.
**
**ENDFORM.
*
*
**FORM pop_calcs_sheet.
**  DATA:
**   fldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
**
**  FIELD-SYMBOLS:
**   <fieldsymbol> LIKE LINE OF <main_setup>-tabstruc[].
**
***-----Context - 3nd sheet
**  MOVE 'CALCS' TO v_sheet.
**
***------Starting row
**  MOVE '1' TO v_row.
**
***-----Starting column
**  MOVE '0' TO v_col.
**
**  PERFORM build_partial_cat USING space
**                                  space
**                                  '/GDA/SDM_S_CALCS'
**                                  space.
**
**  fieldcat[] = gt_fldcat[].
**
**  PERFORM load_fieldcat.
**
**ENDFORM.
