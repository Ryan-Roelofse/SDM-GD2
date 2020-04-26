
*&---------------------------------------------------------------------*
*& Report /GDA/SDM_ARTICLE_REC_STAT_REP1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_article_persist  MESSAGE-ID /gda/sdm_pp1.

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
*  PERFORM init USING gc_article.
*  s_arbgb-sign   = 'I'.
*  s_arbgb-option = 'CP'.
*  s_arbgb-low    = '/GDA/*'.
*  APPEND s_arbgb.
*
*  s_arbgb-sign   = 'I'.
*  s_arbgb-option = 'CP'.
*  s_arbgb-low    = 'Z*'.
*  APPEND s_arbgb.
*
*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM screen_output.
*
*AT SELECTION-SCREEN.
*  PERFORM at_selection_screen.
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
*  PERFORM persist_data.
*
**  IF p_struc = abap_true.
**    PERFORM set_up_relations.
**  ENDIF.
*
**  PERFORM format_final.
*
*END-OF-SELECTION.
**  IF <dyn_table>[] IS NOT INITIAL.
**    PERFORM display_results  USING gt_sdm_articles
**                                   gc_article.
**  ELSE.
**    IF gv_config_err = abap_true.
**      MESSAGE s005.
**    ELSE.
**      MESSAGE s006.
**    ENDIF.
**  ENDIF.
**&---------------------------------------------------------------------*
**&      Form  PERSIST_DATA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM persist_data .
*  DATA:
*    ro_data       TYPE REF TO data,
*    ro_data_empty TYPE REF TO data,
*    lv_field      TYPE /gda/sdm_de_fieldname, "field,
*    lv_table      TYPE /gda/sdm_de_tabname,
*    lt_general    TYPE STANDARD TABLE OF /gda/sdm_setup4,
*    ls_icons      TYPE /gda/sdm_s_icons,
*    lt_icons      TYPE STANDARD TABLE OF /gda/sdm_s_icons,
*    lv_count      TYPE p,
*    lv_pur        TYPE p,
*    lv_text       TYPE string,
*    lv_pur_text   TYPE string,
*    lv_exit.
*
*  FIELD-SYMBOLS:
*    <icon>            TYPE any,
*    <results>         TYPE STANDARD TABLE,
*    <results_temp>    TYPE STANDARD TABLE,
*    <results_pp1>     TYPE STANDARD TABLE,
*    <result>          TYPE any,
*    <field>           TYPE any,
*    <type>            TYPE any,
*    <number>          TYPE any,
*    <brf_key4>        TYPE any,
*    <brf_key5>        TYPE any,
*    <brf_key6>        TYPE any,
*    <field_alv>       TYPE any,
*    <general>         LIKE LINE OF lt_general,
*    <general_default> LIKE LINE OF gt_default_fields,
*    <view_table>      LIKE LINE OF gt_view_tables,
*    <maw1>            LIKE LINE OF gt_maw1,
*    <mean>            LIKE LINE OF gt_mean,
*    <eine>            LIKE LINE OF gt_eine,
*    <eina>            LIKE LINE OF gt_eina,
*    <sdm_articles>    LIKE LINE OF gt_sdm_articles,
*    <instances>       LIKE LINE OF <sdm_articles>-sdm_instances,
*    <objects>         LIKE LINE OF gt_objects.
*
*
*  SORT <dyn_table>.
*  SORT gt_sdm_articles.
*
** NEW PERSISTANCE LOGIC START
*  DATA: lo_data_model TYPE REF TO /gda/cl_sdm_data_model_main.
*
*  DATA:
*    i_sdm_object TYPE /gda/sdm_de_object,
*    i_sdm_guuid   TYPE /gda/sdm_de_uuid,
*    i_tabname     TYPE /gda/sdm_de_tabname,
**    i_uuid       TYPE /gda/sdm_de_uuid,
*    i_field       TYPE /gda/sdm_de_fieldname,
*    i_value       TYPE /gda/sdm_de_fieldvalue,
**    lv_hdr       TYPE /gda/sdm_exc_hdr,
*    lv_main      TYPE /gda/sdm_exc_mai.
*
*  LOOP AT gt_sdm_articles ASSIGNING <sdm_articles>.
*    UNASSIGN <results>.
*
*    LOOP AT <sdm_articles>-sdm_instances ASSIGNING <instances>.
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
*      IF <results_temp> IS ASSIGNED.
*        APPEND LINES OF <results_temp> TO <results>.
*      ENDIF.
*    ENDLOOP.
*
*    SORT <results>.
*    DELETE ADJACENT DUPLICATES FROM <results>.
*
*    REFRESH lt_icons.
*
*    TYPES: BEGIN OF s_tab_fields,
*             tabname16 TYPE tabname16,
*             field(10),
*           END OF s_tab_fields.
*
*    DATA: lt_key_fields     TYPE STANDARD TABLE OF s_tab_fields,
*          lt_key_fields_all TYPE STANDARD TABLE OF s_tab_fields,
*          lt_key_tables     TYPE STANDARD TABLE OF /gda/sdm_de_tabname.
*
*
*    FIELD-SYMBOLS:
*      <id>         TYPE any,
*      <sdm_key>    TYPE any,
*      <key_fields> LIKE LINE OF lt_key_fields,
*      <tables>     LIKE LINE OF lt_key_tables.
*
**    i_uuid = /gda/cl_sdm_data_model_excepts=>generate_uiid( ).
*
*    LOOP AT <results> ASSIGNING <result>.
*      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
*      IF <field> IS ASSIGNED.
*        SPLIT <field> AT '-' INTO lv_table lv_field.
*        ls_icons-field = lv_field.
*      ENDIF.
*
*      COLLECT lv_table INTO lt_key_tables.
*    ENDLOOP.
*
*    LOOP AT lt_key_tables ASSIGNING <tables>.
*      REFRESH:
*      lt_key_fields.
*
*      PERFORM get_key_fields USING <tables>
*                             CHANGING lt_key_fields.
*
*      LOOP AT lt_key_fields ASSIGNING <key_fields>.
*        COLLECT <key_fields> INTO lt_key_fields_all.
*      ENDLOOP.
*    ENDLOOP.
*
*    REFRESH:
*    lt_key_fields.
*
*    LOOP AT <results> ASSIGNING <result>.
*
*      ASSIGN COMPONENT 'SDM_TABKEY'    OF STRUCTURE <result> TO <sdm_key>.
*      CHECK <sdm_key> NE space.
*      ASSIGN COMPONENT 'ID'         OF STRUCTURE <result> TO <id>.
*      ASSIGN COMPONENT 'NUMBER'     OF STRUCTURE <result> TO <number>.
*
*      ASSIGN COMPONENT 'TYPE'     OF STRUCTURE <result> TO <type>.
*      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
*      IF <field> IS ASSIGNED.
*        SPLIT <field> AT '-' INTO lv_table lv_field.
*        ls_icons-field = lv_field.
*      ENDIF.
*
** Get key object...
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
*
** which table are we dealing with?
*      LOOP AT lt_key_fields_all ASSIGNING <key_fields> WHERE tabname16 = lv_table.
*        APPEND  <key_fields> TO lt_key_fields.
*      ENDLOOP.
*
** for this record... we need the key field values.
*
**      i_sdm_object = 'ARTICLE'.
**      i_tabname    = lv_table.
**      i_field      = lv_field.
*
*
*      CREATE OBJECT lo_data_model
*        EXPORTING
*          i_sdm_object_id = '01'
*          i_sdm_type      = '01'
*          i_sdm_tabkey    = <sdm_key>
*          i_tabname       = lv_table
*          i_field         = lv_field
*          i_msg_id        = <id>
*          i_msg_number    = <number>
*          i_begda         = sy-datum.
*
*
*      CALL METHOD lo_data_model->set_msg_type( i_msg_type = <type> ).
*
*      CALL METHOD lo_data_model->process( i_commit = abap_true ).
*
*    ENDLOOP.
*  ENDLOOP.
** endloop.
*
*  PERFORM progress_bar USING TEXT-019.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  GET_KEY_FIELDS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_LV_TABLE  text
**----------------------------------------------------------------------*
*FORM get_key_fields  USING    p_table
*                     CHANGING pt_fields TYPE STANDARD TABLE.
*
*  TYPES: BEGIN OF s_tab_fields,
*           tabname16 TYPE tabname16,
*           field(10),
*         END OF s_tab_fields.
*
*  DATA: go_strucdescr TYPE REF TO cl_abap_structdescr,
*        gt_tab_fields TYPE ddfields,
*        ls_fields     TYPE s_tab_fields.
*
*  FIELD-SYMBOLS: <gwa_tab_field> TYPE dfies.
*
*  TRY .
**   Get the details of the DDIC table
*      go_strucdescr ?= cl_abap_elemdescr=>describe_by_name( p_table ).
*    CATCH cx_sy_move_cast_error .
*      MESSAGE 'Error while casting' TYPE 'S'. RETURN.
*  ENDTRY.
*
** Check if input is a DDIC table
*  CHECK go_strucdescr->is_ddic_type( ) = 'X'.
*
** Get the details of the table fields
*  gt_tab_fields = go_strucdescr->get_ddic_field_list( ).
*
** Display the Key fields of the table
*  LOOP AT gt_tab_fields ASSIGNING <gwa_tab_field> WHERE keyflag = 'X'.
*    ls_fields-tabname16  = p_table.
*    ls_fields-field      = <gwa_tab_field>-fieldname.
*    APPEND ls_fields TO pt_fields.
*  ENDLOOP.
*ENDFORM.
