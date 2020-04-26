class /GDA/SDM_CL_REC_STATUS_EVENTS definition
  public
  create public .

public section.

  methods ON_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_COLUMN_ID
      !E_ROW_ID .
  methods HANDLE_CONTEXT_MENU
    for event CONTEXT_MENU_REQUEST of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !SENDER .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM
      !SENDER .
  methods TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods CONSTRUCTOR
    importing
      !XT_MAIN_SETUP type /GDA/SDM_T_MAIN optional
      !XT_DYN_TABLE type TABLE
      !XT_SDM_OBJECTS type TABLE
      !XO_PARENT type ref to CL_GUI_CONTAINER
      !XT_MSGNR type /GDA/SDM_T_RANGE_MSGNR optional
      !XT_ARBGB type /GDA/SDM_T_RANGE_ARBGB optional .
protected section.
private section.

  types:
    BEGIN OF struc1,
         table TYPE tabname16,
         key   TYPE boolean,
       END OF struc1 .
  types:
    BEGIN OF s_fields,
         field(10),
       END OF s_fields .

  data PV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT .
  data PO_TABLE type ref to DATA .
  data PO_SDM_DATA type ref to DATA .
  data PO_RESULTS type ref to DATA .
  data PO_RESULTS_EMPTY type ref to DATA .
  data PO_ALV_VIEW type ref to DATA .
  data PT_MAIN_SETUP type /GDA/SDM_T_MAIN .
  data PO_PARENT type ref to CL_GUI_CONTAINER .
  data PO_TREE type ref to CL_GUI_ALV_TREE .
  data PO_ALV type ref to CL_GUI_ALV_GRID .
  data PT_RESULT type /GDA/SDM_T_VAL_RESULTS .
  data PT_DEFAULT_FIELDS type /GDA/SDM_T_SETUP4 .
  constants PC_DEFAULT type LVC_S_COL value 'DEFAULT' ##NO_TEXT.
  data PO_OBJECT type ref to /GDA/SDM_CL_CORE .
  data GV_NO_RECORDS type I .
  data PT_MSGNR type /GDA/SDM_T_RANGE_MSGNR .
  data PT_ARBGB type /GDA/SDM_T_RANGE_ARBGB .

  methods MASS_DOWNLOAD .
  methods SET_VIEW_TREE
    importing
      !E_COLUMN_ID type LVC_S_COL
      !E_ROW_ID type LVC_S_ROW
      !E_OBJ_KEY type ANY .
  methods SET_VIEW_ALV
    importing
      !E_COLUMN_ID type LVC_S_COL
      !E_ROW_ID type LVC_S_ROW
      !E_OBJ_KEY type ANY .
  methods CREATE_TREE_HIERARCHY
    importing
      !E_COLUMN_ID type LVC_S_COL
      !E_ROW_ID type LVC_S_ROW
      !E_OBJ_KEY type ANY
    returning
      value(R_KEYS) type LVC_T_NKEY .
  methods ADD_FOLDER_TO_TREE
    importing
      !X_RELAT_KEY type LVC_NKEY optional
      !X_TEXT type LVC_VALUE optional
      !X_TYPE type CHAR1
    returning
      value(R_NODE_KEY) type LVC_NKEY .
  methods ADD_NODE_TO_FOLDER
    importing
      !X_RELAT_KEY type LVC_NKEY
      !X_RESULT type /GDA/SDM_S_VAL_RESULTS
    returning
      value(R_NODE_KEY) type LVC_NKEY .
  methods BUILD_STRUCTURE
    importing
      !X_VIEW type LVC_S_COL
      !X_OBJECT_TYPE type /GDA/SDM_DE_OBJECT
      !X_STRUC type CHAR1 optional
    returning
      value(R_VIEW_STRUC) type LVC_T_FCAT .
  methods BUILD_DYNAMIC_TABLE
    importing
      !X_VIEW_STRUCTURE type LVC_T_FCAT .
  methods DYNAMIC_TABLE_POPULATE
    importing
      !X_VIEW type LVC_S_COL
      !E_ROW_ID type LVC_S_ROW .
  methods BUILD_HIERARCHY_HEADER
    returning
      value(R_HEADER) type TREEV_HHDR .
  methods BUILD_RESULTS
    importing
      !E_ROW_ID type LVC_S_ROW .
  methods DETERMINE_ICON_CONTEXT_KEY
    importing
      !X_KEY type STRING
      !X_NODE_LEVEL type FIELD optional
      !X_MAIN type /GDA/SDM_S_MAIN
      !X_NODE_MAIN type STRING
      !X_RESULT type ANY
    exporting
      !Y_ICON1 type TV_IMAGE
      !Y_ICON2 type TV_IMAGE
    returning
      value(R_ICONS) type /GDA/SDM_S_ICONS .
  methods DETERMINE_ICON_CONTEXT_FIELD
    importing
      !X_FIELDNAME type FIELDNAME
      !X_TABNAME type TABNAME
      !X_CONTEXT_KEY type STRING
      !X_CONTEXT_FIELD type STRING
    returning
      value(R_ICONS) type /GDA/SDM_S_ICONS .
  methods ADD_CONTEXT_VALUE
    importing
      !X_NKEY_CONTEXT type LVC_NKEY
      !X_CONTEXT_KEY type STRING
      !X_CONTEXT_FIELD type STRING
      !X_FIELDNAME type LVC_FNAME
      !X_TABNAME type LVC_TNAME
    returning
      value(R_KEY_CONTEXT) type LVC_NKEY .
  methods ADD_CONTEXT_KEY
    importing
      !X_FOLDER type LVC_NKEY
      !X_KEY_FIELD_MAIN type STRING
      !X_NODE_LEVEL type FIELD
      !X_TABNAME type TABNAME16
      !X_ICONS type /GDA/SDM_S_ICONS
    returning
      value(R_KEY_CONTEXT) type LVC_NKEY .
  methods ADD_CONTEXT_ERRORS
    importing
      !X_FOLDER type LVC_NKEY
      !X_KEY type STRING
      !X_KEY_FIELD_MAIN type STRING
      !X_RESULT type ANY
      !X_MAIN_SETUP type /GDA/SDM_S_MAIN
    exporting
      !Y_MESS_ID type CHAR6
      !Y_ID_KEY type LVC_NKEY .
  methods ADD_ID
    importing
      !X_RELAT_KEY type LVC_NKEY
      !X_RESULT type /GDA/SDM_S_VAL_RESULTS
    returning
      value(R_KEY_NODE) type LVC_NKEY .
ENDCLASS.



CLASS /GDA/SDM_CL_REC_STATUS_EVENTS IMPLEMENTATION.


  METHOD add_context_errors.

    DATA:
      lv_field           TYPE fieldname,
      lv_table           TYPE struc1,
      ls_result          TYPE /gda/sdm_s_val_results,
      lv_mess_id_last(6) TYPE c,
      lv_key_node        TYPE lvc_nkey,
      lv_context_key(30).

    FIELD-SYMBOLS:
      <field>     TYPE any,
      <brf_key4>  TYPE any,
      <brf_key5>  TYPE any,
      <brf_key6>  TYPE any.

* EXTRA_V1 contains table-field
    ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE x_result TO <field>.
    IF <field> IS ASSIGNED.
      SPLIT <field> AT '-' INTO lv_table-table lv_field.
      READ TABLE x_main_setup-tabstruc WITH KEY fieldname = lv_field
                                                tabname   = lv_table-table TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDIF.
*
*    ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE x_result TO <brf_key>.
*    IF <brf_key> = space.
*      ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE x_result TO <brf_key>.
*    ENDIF.
*
*    ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE x_result TO <brf_key6>.

* Get key object...
    ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE x_result TO <brf_key4>.
    IF <brf_key4> IS ASSIGNED AND <brf_key4> IS NOT INITIAL.
      lv_context_key = <brf_key4>.
    ENDIF.

    IF lv_context_key IS INITIAL.
      ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE x_result TO <brf_key5>.
      IF <brf_key5> IS ASSIGNED  AND <brf_key5> IS NOT INITIAL.
        lv_context_key = <brf_key5>.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE x_result TO <brf_key6>.
    IF <brf_key6> IS ASSIGNED  AND <brf_key6> IS NOT INITIAL.
      CLEAR: lv_context_key.
      CONCATENATE <brf_key5> '/' <brf_key6> INTO lv_context_key.
    ENDIF.

    IF lv_context_key IS INITIAL.
* If BRF returns a blank then set to object key
*        r_icons-brf_key = <sdm_articles>-article.
    ENDIF.


    IF lv_context_key IS INITIAL.
      lv_context_key = x_key.
*    <brf_key> = p_matnr.
    ENDIF.

    IF x_key_field_main = lv_context_key. " AND x_attribute = <brf_key6>.
      MOVE-CORRESPONDING x_result TO ls_result.
      y_mess_id = ls_result-number.

      IF y_id_key = space.
        IF y_mess_id <> lv_mess_id_last.
          lv_mess_id_last = y_mess_id.
          lv_key_node = me->add_id( x_relat_key = x_folder x_result = ls_result ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD add_context_key.
    DATA:
      lv_node_text   TYPE lvc_value,
      lv_value       TYPE string,
      lv_ddtext      TYPE dd04t-ddtext,
      ls_node_layout TYPE lvc_s_layn,
      lt_item_layout TYPE lvc_t_layi,
      ls_item_layout TYPE lvc_s_layi,
      lv_rollname    TYPE dd03l-rollname.

    lv_value  = x_key_field_main.

* get rollname..
    SELECT SINGLE rollname FROM dd03l
                          INTO lv_rollname
                          WHERE tabname   = x_tabname
                            AND fieldname = x_node_level.
    IF sy-subrc <> 0.
      lv_rollname = x_node_level.
    ENDIF.

    SELECT SINGLE ddtext FROM dd04t
                          INTO lv_ddtext
                          WHERE rollname   = lv_rollname
                            AND ddlanguage = sy-langu.
    IF sy-subrc <> 0.
      lv_ddtext = x_node_level.
    ENDIF.

    CONCATENATE lv_ddtext ':' into lv_ddtext.
    CONCATENATE lv_ddtext lv_value INTO lv_node_text SEPARATED BY space.

    ls_node_layout-n_image   = x_icons-icon.
    ls_node_layout-exp_image = x_icons-icon2.

    IF x_icons-icon2 IS NOT INITIAL.
      ls_item_layout-t_image = x_icons-icon2.
      ls_item_layout-fieldname = po_tree->c_hierarchy_column_name.
      APPEND ls_item_layout TO lt_item_layout.
    ENDIF.

    CALL METHOD po_tree->add_node
      EXPORTING
        i_relat_node_key = x_folder
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = lv_node_text
        is_node_layout   = ls_node_layout
        it_item_layout   = lt_item_layout
      IMPORTING
        e_new_node_key   = r_key_context.

  ENDMETHOD.


  METHOD add_context_value.
    DATA:
      lv_node_text TYPE lvc_value,
      lv_value     TYPE char30, "string,
      lv_layout    TYPE lvc_s_layn,
      lv_ddtext    TYPE dd04t-ddtext,
      lv_line      TYPE /gda/sdm_s_val_results,
      lv_key_combo TYPE fieldname,
      ls_icons     TYPE /gda/sdm_s_icons.

    lv_value = x_context_field.

    SELECT SINGLE ddtext FROM dd03m
                          INTO lv_ddtext
                          WHERE tabname    = x_tabname
                            AND fieldname  = x_fieldname
                            AND ddlanguage = sy-langu.
    IF sy-subrc = 0.
      lv_node_text = lv_ddtext.
    ELSE.
      lv_node_text = x_fieldname.
    ENDIF.

    lv_line-extra_v5 = lv_value.

    ls_icons = me->determine_icon_context_field( x_fieldname = x_fieldname
                                                 x_tabname   = x_tabname
                                                 x_context_key = x_context_key
                                                 x_context_field = x_context_field ).
    lv_layout-n_image = ls_icons-icon.

    IF lv_layout-n_image IS INITIAL.
*      CONCATENATE x_context_field '/' x_attribute INTO lv_key_combo.
      lv_key_combo = x_context_key.
      ls_icons = me->determine_icon_context_field( x_fieldname     = lv_key_combo
                                                   x_tabname       = x_tabname
                                                   x_context_key   = x_context_key
                                                   x_context_field = x_context_field ).
      lv_layout-n_image = ls_icons-icon.
    ENDIF.

    CALL METHOD po_tree->add_node
      EXPORTING
        i_relat_node_key = x_nkey_context
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = lv_node_text
        is_node_layout   = lv_layout
*       it_item_layout   = lt_item_layout
        is_outtab_line   = lv_line
      IMPORTING
        e_new_node_key   = r_key_context.

  ENDMETHOD.


  method ADD_FOLDER_TO_TREE.
  DATA:
    lv_node_text TYPE lvc_value,
    ls_result    TYPE /gda/sdm_s_val_results,
    lv_rel       TYPE int4.

  lv_node_text = x_text.

  CASE x_type.
    WHEN '1'.
      lv_rel = cl_gui_column_tree=>relat_last_child.
    WHEN '2'.
      lv_rel = cl_gui_column_tree=>relat_last_sibling.
    WHEN OTHERS.
      lv_rel = cl_gui_column_tree=>relat_last_child.
  ENDCASE.


  CALL METHOD po_tree->add_node
    EXPORTING
      i_relat_node_key = x_relat_key
      i_relationship   = lv_rel "CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      i_node_text      = lv_node_text
      is_outtab_line   = ls_result
*     is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = r_node_key.


  endmethod.


  METHOD add_id.
    DATA:
     l_node_text TYPE lvc_value.

    l_node_text = x_result-number.

    CALL METHOD po_tree->add_node
      EXPORTING
        i_relat_node_key = x_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
        is_outtab_line   = x_result
      IMPORTING
        e_new_node_key   = r_key_node.

  ENDMETHOD.


  METHOD add_node_to_folder.
    DATA:
     l_node_text TYPE lvc_value.

    l_node_text = x_result-number.

    CALL METHOD po_tree->add_node
      EXPORTING
        i_relat_node_key = x_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
        is_outtab_line   = x_result
      IMPORTING
        e_new_node_key   = r_node_key.

  ENDMETHOD.


  method BUILD_DYNAMIC_TABLE.
*  DATA:
*    ro_table TYPE REF TO data,
*    ro_line  TYPE REF TO data.
*
*  FIELD-SYMBOLS:
*     <main_setup> LIKE LINE OF gt_pp_main_setup.
*
*  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = x_view.
*  IF sy-subrc <> 0 OR <main_setup>-ro_table IS INITIAL.
* Create dynamic internal table and assign to FS
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog  = X_VIEW_STRUCTURE
        i_length_in_byte = 'X'
      IMPORTING
        ep_table         = PO_ALV_VIEW.

*    IF x_view = gc_default.
*      ASSIGN PO_TABLE_VIEW->* TO <dyn_table>.
**      ASSIGN ro_table->* TO <dyn_table_final>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table>.
**       CREATE DATA ro_line LIKE LINE OF <dyn_table_final>.
*      ASSIGN ro_line->* TO <dyn_wa>.
*    ELSE.
*      ASSIGN PO_TABLE_VIEW->* TO <dyn_table_view>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table_view>.
*      ASSIGN ro_line->* TO <dyn_wa_view>.
*    ENDIF.
*
*    <main_setup>-tabname    = x_view.
*    <main_setup>-ro_table   = ro_table.
*    <main_setup>-tabstruc[] = gt_view_struc[].
*  ELSE.
*    IF x_view = gc_default.
*      ASSIGN <main_setup>-ro_table->* TO <dyn_table>.
**      ASSIGN <main_setup>-ro_table->* TO <dyn_table_final>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table>.
*      ASSIGN ro_line->* TO <dyn_wa>.
*    ELSE.
*      ASSIGN <main_setup>-ro_table->* TO <dyn_table_view>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table_view>.
*      ASSIGN ro_line->* TO <dyn_wa_view>.
**    ENDIF.
**  ENDIF.
*
*  xo_data = ro_table.
*
*  REFRESH:
*    gt_view_struc[].

  endmethod.


  method BUILD_HIERARCHY_HEADER.
  r_header-heading = TEXT-010.
  r_header-tooltip = TEXT-011.
  r_header-width = 75.
  r_header-width_pix = ' '.

  endmethod.


  METHOD build_results.
    DATA:
      ro_data       TYPE REF TO data,
      ro_data_empty TYPE REF TO data,
      lv_del.

    FIELD-SYMBOLS:
      <sdm_instance>  TYPE /gda/sdm_s_instances,
      <sdm_instances> TYPE /gda/sdm_t_instances,
      <sdm_data>      TYPE STANDARD TABLE,
      <sdm_object>    TYPE any,
      <results>       TYPE STANDARD TABLE,
      <results_temp>  TYPE STANDARD TABLE,
      <table>         TYPE STANDARD TABLE,
      <result>        TYPE any,
      <dyn_wa>        TYPE any,
      <key>           TYPE any.


    ASSIGN po_sdm_data->* TO <sdm_data>.
    ASSIGN po_table->*    TO <table>.

    CASE pv_object_type.
      WHEN 'ARTICLE'.
        READ TABLE  <table>[] ASSIGNING <dyn_wa>      INDEX e_row_id.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('ARTICLE') = <key>.
      WHEN 'MATERIAL'.
        READ TABLE  <table>[] ASSIGNING <dyn_wa>      INDEX e_row_id.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('MATERIAL') = <key>.
      WHEN 'BOM'.
        READ TABLE  <table>[] ASSIGNING <dyn_wa>      INDEX e_row_id.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('BOM') = <key>.

      WHEN OTHERS.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> INDEX e_row_id.
    ENDCASE.

*    READ TABLE <sdm_data> ASSIGNING <sdm_object> INDEX e_row_id.
    ASSIGN COMPONENT 'SDM_INSTANCES' OF STRUCTURE <sdm_object> TO <sdm_instances>.

    LOOP AT <sdm_instances> ASSIGNING <sdm_instance>.
      IF <sdm_instance>-object IS INITIAL.
        CONTINUE.
      ENDIF.
      IF <results> IS NOT ASSIGNED.
        ro_data_empty  = <sdm_instance>-object->return_brf_result_structure( ).
        ASSIGN ro_data_empty->* TO <results>.
        REFRESH:
         <results>.
      ENDIF.

      ro_data        = <sdm_instance>-object->return_brf_result( ).
      ASSIGN ro_data->* TO <results_temp>.
      IF <results_temp> IS ASSIGNED.
        APPEND LINES OF <results_temp> TO <results>.
      ENDIF.
*      IF po_results_empty IS INITIAL.
      po_results_empty =  <sdm_instance>-object->create_brf_result_structure( ).
*      ENDIF.
    ENDLOOP.

    SORT <results>.
    DELETE ADJACENT DUPLICATES FROM <results>.

* Filter results...
    IF pt_msgnr IS NOT INITIAL OR
       pt_arbgb IS NOT INITIAL.

      FIELD-SYMBOLS:
        <number> TYPE any,
        <id>     TYPE any.

      LOOP AT <results> ASSIGNING <result>.
        IF pt_arbgb IS NOT INITIAL.
          ASSIGN COMPONENT 'ID'     OF STRUCTURE <result> TO <id>.
        ENDIF.
        IF pt_msgnr IS NOT INITIAL.
          ASSIGN COMPONENT 'NUMBER' OF STRUCTURE <result> TO <number>.
        ENDIF.

        IF <id> IS ASSIGNED AND <number> IS NOT ASSIGNED.
          IF <id> IN pt_arbgb.
            lv_del =  abap_false.
          ELSE.
            lv_del =  abap_true.
          ENDIF.
        ELSEIF <number> IS ASSIGNED AND <id> IS NOT ASSIGNED.
          IF <number> IN pt_msgnr.
            lv_del =  abap_false.
          ELSE.
            lv_del =  abap_true.
          ENDIF.
        ELSEIF <number> IS ASSIGNED AND <id> IS ASSIGNED.
          IF <number> IN pt_msgnr AND <id> IN pt_arbgb.
            lv_del =  abap_false.
          ELSE.
            lv_del =  abap_true.
          ENDIF.
        ENDIF.
        IF lv_del = abap_true.
          DELETE <results>.
        ENDIF.
      ENDLOOP.
    ENDIF.

    GET REFERENCE OF <results> INTO po_results.
*    po_results_empty =  <sdm_instance>-object->return_brf_result_structure( ).
  ENDMETHOD.


  METHOD build_structure.
    DATA:
      lo_ref_table_des TYPE REF TO cl_abap_structdescr,
      lt_view_fields   TYPE STANDARD TABLE OF /gda/sdm_setup4,
      lt_details       TYPE abap_compdescr_tab,
      lv_lines         TYPE i,
      ls_view_struc    TYPE lvc_s_fcat,
      lt_view_tables   TYPE STANDARD TABLE OF tabname16,
      ls_table         TYPE tabname16,
      ls_field         TYPE s_fields,
      ls_def_fields    TYPE /gda/sdm_setup4.

    FIELD-SYMBOLS:
      <general>        LIKE LINE OF lt_view_fields,
      <default_fields> LIKE LINE OF lt_view_fields,
      <details>        LIKE LINE OF lt_details,
      <view_struc>     LIKE LINE OF r_view_struc,
      <parameter>      TYPE any,
      <main_setup>     LIKE LINE OF pt_main_setup.

* Retrieve fields for specific view
    SELECT * FROM /gda/sdm_setup4 INTO TABLE lt_view_fields
      WHERE object_type  = x_object_type
        AND object_view  = x_view
        AND outp         = abap_true.

* Ensure that default field are displayed in all views.
    LOOP AT pt_default_fields ASSIGNING <default_fields>.
      MOVE-CORRESPONDING <default_fields> TO ls_def_fields.
      READ TABLE lt_view_fields ASSIGNING <general>
      WITH KEY field   = <default_fields>-field
               tabname = <default_fields>-tabname.
      IF sy-subrc <> 0.
        CONCATENATE 'KEY_' <default_fields>-field INTO ls_def_fields-field. "<default_fields>-field.
        APPEND ls_def_fields TO lt_view_fields.
        CLEAR:
         ls_def_fields.
      ELSE.
        IF x_view <> pc_default.
          CONCATENATE 'KEY_' <default_fields>-field INTO ls_def_fields-field. "<default_fields>-field.
          ls_def_fields-object_view = x_view.
          APPEND ls_def_fields TO lt_view_fields.
          CLEAR:
           ls_def_fields.
        ELSE.
          CONCATENATE 'KEY_' <general>-field  INTO <general>-field.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT lt_view_fields BY default_all DESCENDING ord.
* Build a table of related tables for the view
    LOOP AT lt_view_fields ASSIGNING <general>.
      ls_table = <general>-tabname.
      APPEND ls_table TO lt_view_tables.
    ENDLOOP.

    SORT lt_view_tables.
    DELETE ADJACENT DUPLICATES FROM lt_view_tables.

    LOOP AT lt_view_tables INTO ls_table.
      lo_ref_table_des ?=
          cl_abap_typedescr=>describe_by_name( ls_table ).

      lt_details[] = lo_ref_table_des->components[].

      LOOP AT lt_view_fields ASSIGNING <general> WHERE tabname = ls_table.
        IF <general>-field CS 'KEY_'.
          ls_field = <general>-field+4(6).
          READ TABLE lt_details ASSIGNING <details> WITH KEY name = ls_field.
        ELSE.
          READ TABLE lt_details ASSIGNING <details> WITH KEY name = <general>-field.
        ENDIF.
        CHECK sy-subrc = 0.
        CLEAR: ls_view_struc.
        ls_view_struc-fieldname = <general>-field.
        ls_view_struc-tabname   = <general>-tabname.
        ls_view_struc-datatype  = <details>-type_kind.
        CASE <details>-type_kind.
          WHEN 'C'.
            ls_view_struc-datatype = 'CHAR'.
          WHEN 'N'.
            ls_view_struc-datatype = 'NUMC'.
          WHEN 'D'.
            ls_view_struc-datatype = 'DATE'.
          WHEN 'P'.
            ls_view_struc-datatype = 'PACK'.
          WHEN OTHERS.
            ls_view_struc-datatype = <details>-type_kind.
        ENDCASE.

        IF <general>-field CS 'KEY_'.
          ls_view_struc-key      = abap_true.
        ENDIF.

        ls_view_struc-inttype  = <details>-type_kind.
        ls_view_struc-intlen   = <details>-length.
        ls_view_struc-decimals = <details>-decimals.
        ls_view_struc-col_pos  = <general>-ord.

        SELECT SINGLE scrtext_s scrtext_m scrtext_l FROM dd03m
               INTO (ls_view_struc-scrtext_s, ls_view_struc-scrtext_m, ls_view_struc-scrtext_l)
              WHERE tabname    = <general>-tabname
                AND fieldname  = <general>-field
                AND ddlanguage = sy-langu.

        IF sy-subrc <> 0.
          SELECT SINGLE scrtext_s scrtext_m scrtext_l FROM dd03m
                 INTO (ls_view_struc-scrtext_s, ls_view_struc-scrtext_m, ls_view_struc-scrtext_l)
                WHERE tabname    = <general>-tabname
                  AND fieldname  = <general>-field+4
                  AND ddlanguage = sy-langu.
        ENDIF.

        APPEND ls_view_struc TO r_view_struc.
      ENDLOOP.
    ENDLOOP.

    CLEAR:
      ls_view_struc,
      lv_lines.

    SORT r_view_struc BY key DESCENDING col_pos.

    LOOP AT r_view_struc ASSIGNING <view_struc>.
      lv_lines = lv_lines + 1.
      <view_struc>-col_pos =  lv_lines.
    ENDLOOP.

    DESCRIBE TABLE r_view_struc LINES lv_lines.

    IF x_view  = 'DEFAULT'.
* Add an additional hidden linkage field to the end for DEFAULT View. But only if sructured oobject is selected
      lv_lines = lv_lines + 1.
      ls_view_struc-fieldname  = 'LINKAGE'.
      IF x_struc = abap_false.
        ls_view_struc-no_out = abap_true.
      ENDIF.
      ls_view_struc-datatype   = 'CHAR'.
      ls_view_struc-inttype    = 'C'.
      ls_view_struc-intlen     = 000018.
      ls_view_struc-outputlen  = 000018.
      ls_view_struc-col_pos    = 1.
      ls_view_struc-scrtext_l  = TEXT-012." 'Structured Material'.
      ls_view_struc-scrtext_m  = TEXT-012." 'Structured Material'.
      ls_view_struc-scrtext_s  = TEXT-002." 'Structured Material'.
      APPEND ls_view_struc TO r_view_struc.
      CLEAR:
       ls_view_struc.

      LOOP AT pt_main_setup ASSIGNING <main_setup>.
        IF <main_setup>-object_view NE pc_default.
          lv_lines = lv_lines + 1.
          ASSIGN (<main_setup>-object_view_o) TO <parameter>.
          IF <parameter> = abap_true.
            ls_view_struc-fieldname  = <main_setup>-object_view.
            ls_view_struc-datatype   = 'CHAR'.
            ls_view_struc-inttype    = 'C'.
            ls_view_struc-intlen     = 000004.
            ls_view_struc-outputlen  = 000004.
            ls_view_struc-icon       = abap_true.
            ls_view_struc-col_pos    = lv_lines.
            ls_view_struc-hotspot    =  abap_true.
            APPEND ls_view_struc TO r_view_struc.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ELSE.
* BRF Plus Fields..
      lv_lines = lv_lines + 1.
      ls_view_struc-fieldname  = 'MESSAGE'.
      ls_view_struc-datatype   = 'CHAR'.
      ls_view_struc-inttype    = 'C'.
      ls_view_struc-intlen     = 000220.
      ls_view_struc-outputlen  = 000100.
      ls_view_struc-col_pos    = lv_lines.
      APPEND ls_view_struc TO r_view_struc.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    FIELD-SYMBOLS:
      <sdm_objects>   TYPE any,
      <sdm_instances> TYPE /gda/sdm_t_instances,
      <sdm_instance>  TYPE /gda/sdm_s_instances.

*    CREATE DATA po_sdm_data TYPE TABLE OF ('/GDA/SDM_S_MATERIAL').
    GET REFERENCE OF xt_sdm_objects INTO po_sdm_data.
    GET REFERENCE OF xt_dyn_table   INTO po_table.

    DESCRIBE TABLE xt_dyn_table LINES gv_no_records.

* determine object type that the class is handling
    READ TABLE xt_sdm_objects ASSIGNING <sdm_objects> INDEX '1'.
    ASSIGN COMPONENT 'SDM_INSTANCES' OF STRUCTURE <sdm_objects> TO <sdm_instances>.
    READ TABLE <sdm_instances> ASSIGNING <sdm_instance> INDEX '1'.
    pv_object_type = <sdm_instance>-object->get_object_type( ).
    po_object  = <sdm_instance>-object.

*PV_OBJECT_TYPE

    IF xt_main_setup IS NOT INITIAL.
      pt_main_setup[] = xt_main_setup[].
    ELSE.
      SELECT * FROM /gda/sdm_setup3 INTO CORRESPONDING FIELDS OF TABLE pt_main_setup
        WHERE object_type = pv_object_type
          AND status      = abap_true
          ORDER BY ord.
    ENDIF.

* Set up Default Fields Heading
    SELECT * FROM /gda/sdm_setup4 INTO TABLE pt_default_fields
      WHERE object_type  = pv_object_type
        AND object_view  = pc_default
        AND outp         = abap_true
        AND default_all  = abap_true.

    po_parent = xo_parent.


    pt_msgnr = xt_msgnr.
    pt_arbgb = xt_arbgb.
  ENDMETHOD.


  METHOD create_tree_hierarchy.

    DATA:
      lv_nkey_folder_brf_all TYPE lvc_nkey,
      lv_nkey_folder_brf     TYPE lvc_nkey,
      lv_nkey_brf_err        TYPE lvc_nkey,
      lv_nkey_context        TYPE lvc_nkey,
      lv_nkey_context_value  TYPE lvc_nkey,
      lv_field               TYPE fieldname,
      lv_table               TYPE struc1,
      lv_tabname             TYPE tabname,
      lv_context_added       TYPE boolean,
      ls_result              TYPE /gda/sdm_s_val_results,
      lt_sequence            TYPE /gda/sdm_tt_output,
      lt_view_struc          TYPE lvc_t_fcat,
      ls_icons               TYPE /gda/sdm_s_icons,
      lv_object_key          TYPE string,
      lv_context_key         TYPE string,
      lv_context_field       TYPE string,
      lv_context_key_multi   TYPE string,
      ro_data                TYPE REF TO data,
      ro_data_empty          TYPE REF TO data.

    FIELD-SYMBOLS:
      <results>         TYPE STANDARD TABLE,
      <result>          TYPE any,
      <field>           TYPE any,
      <line_primary>    TYPE any,
      <line_02>         TYPE any,
      <context_key>     TYPE any,
      <node_level2>     TYPE any,
      <node_level3>     TYPE any,
      <context_field>   TYPE any,
      <key_field_attr>  TYPE any,
      <key_field>       TYPE any,
      <sdm_object>      TYPE any,
      <obj_key>         TYPE any,
      <table>           TYPE STANDARD TABLE,
      <line>            TYPE any,
      <icon>            TYPE any,
      <table_primary>   TYPE ANY TABLE,
      <table_secondary> TYPE ANY TABLE,
      <sdm_data>        TYPE STANDARD TABLE,
      <main_setup>      LIKE LINE OF pt_main_setup,
      <main_output>     LIKE LINE OF lt_sequence,
      <main_output_02>  LIKE LINE OF lt_sequence,
      <tabstruc>        LIKE LINE OF lt_view_struc,
      <key>             TYPE any.


    ASSIGN po_sdm_data->* TO <sdm_data>.

    ASSIGN po_table->* TO <table>.
    READ TABLE <table> ASSIGNING <line> INDEX e_row_id.
    ASSIGN COMPONENT e_column_id OF STRUCTURE <line> TO <icon>.
    CHECK <icon> IS ASSIGNED.
    CHECK <icon> NE icon_green_light.

    CASE pv_object_type.
      WHEN 'ARTICLE'.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <line> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('ARTICLE') = <key>.
      WHEN 'MATERIAL'.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <line> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('MATERIAL') = <key>.
      WHEN 'BOM'.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <line> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('MATERIAL') = <key>.
      WHEN 'SCUSTOM'.
        ASSIGN COMPONENT 'KEY_ID' OF STRUCTURE <line> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('CUSTOMER') = <key>.
      WHEN OTHERS.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> INDEX e_row_id.
    ENDCASE.

*    READ TABLE <sdm_data> ASSIGNING <sdm_object> INDEX e_row_id.
    ASSIGN COMPONENT 1 OF STRUCTURE <sdm_object> TO <obj_key>.

    me->build_results( e_row_id = e_row_id ).
    ASSIGN po_results->* TO <results>.

* Create Folder - BRF All
    lv_nkey_folder_brf_all = me->add_folder_to_tree( x_type = '1' x_text = TEXT-001 ).
* Set this folder to Expanded? No
*    APPEND lv_nkey_folder_brf_all TO r_keys.

* Add All BRF Errors to BRF All folder for the selected View
    READ TABLE pt_main_setup ASSIGNING <main_setup> WITH KEY object_view = e_column_id.

    IF <main_setup>-tabstruc IS INITIAL.
      <main_setup>-tabstruc = me->build_structure( x_view = e_column_id x_object_type = pv_object_type ).
    ENDIF.

*  Determine if error is related to the selected view
    LOOP AT <results> ASSIGNING <result>.
      CLEAR:
       lv_table,
       lv_field.

* EXTRA_V1 contains table-field
      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
      IF <field> IS ASSIGNED.
        SPLIT <field> AT '-' INTO lv_table-table lv_field.
        READ TABLE <main_setup>-tabstruc  WITH KEY fieldname = lv_field
                                                   tabname   = lv_table-table TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING <result> TO ls_result.
*      lv_mess_id = ls_result-number.
      lv_nkey_brf_err =  me->add_node_to_folder( x_relat_key = lv_nkey_folder_brf_all x_result = ls_result ).
    ENDLOOP.

* Create folder - 'Context'
    lv_nkey_folder_brf_all = me->add_folder_to_tree( x_type = '1' x_text = TEXT-003 ).
* Set this folder to Expanded? Yes
*    APPEND lv_nkey_folder_brf_all TO r_keys.

    LOOP AT <main_setup>-sequence ASSIGNING <main_output> WHERE seq = '1'.
      lv_tabname = '<SDM_OBJECT>-&&&&'.
      REPLACE ALL OCCURRENCES OF '&&&&' IN lv_tabname WITH <main_output>-tabname.

      ASSIGN (lv_tabname) TO <table_primary>.
* Loop through primary table
* Extract fields from primary table that are specified in config
      CHECK <table_primary> IS ASSIGNED.
      LOOP AT <table_primary> ASSIGNING <line_primary>.
* Get key for this table entry...
        ASSIGN COMPONENT <main_output>-node_level   OF STRUCTURE <line_primary> TO <context_key>.
        IF <main_output>-node_level2 IS NOT INITIAL.
          ASSIGN COMPONENT <main_output>-node_level2  OF STRUCTURE <line_primary> TO <node_level2>.
        ENDIF.
        IF <main_output>-node_level3 IS NOT INITIAL.
          ASSIGN COMPONENT <main_output>-node_level3  OF STRUCTURE <line_primary> TO <node_level3>.
        ENDIF.

        IF <node_level2> IS ASSIGNED AND <node_level2> IS NOT INITIAL.
          CONCATENATE <context_key> '/' <node_level2> INTO lv_context_key_multi.
        ENDIF.

        IF <node_level3> IS ASSIGNED AND  <node_level3> IS NOT INITIAL.
          CONCATENATE <context_key> '/' <node_level3> INTO lv_context_key_multi.
        ENDIF.

        IF lv_context_key_multi IS NOT INITIAL.
          ASSIGN lv_context_key_multi TO <context_key>.
        ENDIF.

* Special Case!
*        IF <main_output>-node_level = 'VKORG'.
*          ASSIGN COMPONENT 'VTWEG' OF STRUCTURE <line_primary> TO <key_field_attr>.
*          IF <key_field_attr> IS ASSIGNED.
*            lv_key_attribute = <key_field_attr>.
*          ENDIF.
*        ENDIF.

        IF <context_key> IS ASSIGNED AND sy-subrc = 0.
* Add entry to : Create folder - 'Context'
*          CLEAR:
*           lv_image,
*           lv_image2.

          lv_object_key   = <obj_key>.
          lv_context_key  = <context_key>.

* Determine if this context folder contains an error, if it does then mark with an error icon..
          LOOP AT <results> ASSIGNING <result>.
            ls_icons = me->determine_icon_context_key( x_key           = lv_object_key
                                                       x_node_level    = <main_output>-node_level
                                                       x_main          = <main_setup>
                                                       x_node_main     = lv_context_key
*                                                       x_key_attribute = lv_key_attribute
                                                       x_result        = <result> ).

* Exit as soon as an error is found
            IF ls_icons-icon = icon_failure.
              EXIT.
            ENDIF.
          ENDLOOP.

*          lv_key_field_main = <context_key>.
*          lv_context_key  = <context_key>.
          lv_nkey_context = me->add_context_key( x_folder        = lv_nkey_folder_brf_all
                                                x_key_field_main = lv_context_key "lv_key_field_main "lv_key_context "lv_context_key
*                                                x_attribute      = lv_key_attribute
                                                x_node_level     = <main_output>-node_level
                                                x_tabname        = <main_output>-tabname
                                                x_icons          = ls_icons ).

* Create Folder - 'BRF Errors'
          IF ls_icons-icon = icon_failure.
            lv_nkey_folder_brf = me->add_folder_to_tree( x_relat_key = lv_nkey_context x_type = '1' x_text = TEXT-004 ).
*            APPEND lv_nkey_folder_brf TO r_keys.

* Now add the relevant errors.
            LOOP AT <results> ASSIGNING <result>.
*              lv_key       = <obj_key>.
              me->add_context_errors( x_folder         = lv_nkey_folder_brf
                                      x_key            = lv_object_key
                                      x_key_field_main = lv_context_key "lv_key_field_main
*                                      x_attribute      = lv_key_attribute
                                      x_result         = <result>
                                      x_main_setup     = <main_setup> ).
            ENDLOOP.
          ENDIF.
        ENDIF.

* Include Context fields for main table on ALV Tree
        LOOP AT <main_setup>-tabstruc ASSIGNING <tabstruc>.
          IF <tabstruc>-fieldname = 'MESSAGE'.
            CONTINUE.
          ENDIF.

          UNASSIGN:
           <context_field>,
           <key_field>.

          IF <tabstruc>-fieldname CS 'KEY_'.
            CONTINUE.
          ELSE.
            ASSIGN COMPONENT <tabstruc>-fieldname  OF STRUCTURE <line_primary> TO <context_field>.
          ENDIF.

* is the context field found in the Primary table?
* Yes - then do logic
* No  - then find in seconday tables
          IF <context_field> IS ASSIGNED.

            lv_context_field  = <context_field>.
*            lv_key_field      = <context_key>.
*             lv_context_key     = <context_key>.

            lv_nkey_context_value = me->add_context_value( x_nkey_context   = lv_nkey_context    "lv_context_key     "lv_key_context
                                                           x_context_key    = lv_context_key
                                                           x_context_field  = lv_context_field
                                                           x_fieldname      = <tabstruc>-fieldname
                                                           x_tabname        = <tabstruc>-tabname ).
* Assume the value exists in a secondary table..
* No  - then find in seconday tables

          ELSE.
            lv_context_added = abap_false.

            LOOP AT <main_setup>-sequence ASSIGNING <main_output_02> WHERE seq = '2'.
              IF lv_context_added = abap_true.
                CONTINUE.
              ENDIF.

              UNASSIGN:
               <context_field>,
               <table_secondary>.

              IF <main_output_02> IS ASSIGNED.
                lv_tabname = '<SDM_OBJECT>-&&&&'.
                REPLACE ALL OCCURRENCES OF '&&&&' IN lv_tabname WITH <main_output_02>-tabname.
                ASSIGN (lv_tabname) TO <table_secondary>.

                CHECK <table_secondary> IS ASSIGNED.
                LOOP AT <table_secondary> ASSIGNING <line_02>.
                  UNASSIGN <context_field>.
                  ASSIGN COMPONENT <main_output>-node_level OF STRUCTURE <line_02> TO <context_field>.

                  CHECK <context_field> IS ASSIGNED.
* Get key for this table entry...
                  ASSIGN COMPONENT <main_output>-node_level   OF STRUCTURE <line_02> TO <key_field>.
                  IF <main_output>-node_level2 IS NOT INITIAL.
                    ASSIGN COMPONENT <main_output>-node_level2  OF STRUCTURE <line_02> TO <node_level2>.
                  ENDIF.
                  IF <main_output>-node_level3 IS NOT INITIAL.
                    ASSIGN COMPONENT <main_output>-node_level3  OF STRUCTURE <line_02> TO <node_level3>.
                  ENDIF.

                  IF <node_level2> IS ASSIGNED AND <node_level2> IS NOT INITIAL.
                    CONCATENATE <key_field> '/' <node_level2> INTO lv_context_key_multi.
                  ENDIF.

                  IF <node_level3> IS ASSIGNED AND  <node_level3> IS NOT INITIAL.
                    CONCATENATE <key_field> '/' <node_level3> INTO lv_context_key_multi.
                  ENDIF.

                  IF lv_context_key_multi IS NOT INITIAL.
                    ASSIGN lv_context_key_multi TO <key_field>.
                  ENDIF.

                  CHECK <key_field> EQ lv_context_key.

*                  ASSIGN COMPONENT <main_output>-node_level  OF STRUCTURE <line_02> TO <key_field>.
*                  IF <key_field> IS ASSIGNED AND <key_field> <> <context_key>.
*                    CONTINUE.
*                  ENDIF.

                  IF <tabstruc>-fieldname CS 'KEY_'.
**                    ASSIGN COMPONENT <TABSTRUC>-FIELDNAME+4  OF STRUCTURE <LINE_02> TO <VALUE_02>.
                  ELSE.
                    UNASSIGN <context_field>. "RR 17.01.2019
                    ASSIGN COMPONENT <tabstruc>-fieldname  OF STRUCTURE <line_02> TO <context_field>.
                  ENDIF.

                  IF <context_field> IS ASSIGNED.

                    lv_context_field  = <context_field>.
*                    lv_key_field      = <context_key>.
*                     lv_context_key = <context_key>.

                    lv_nkey_context_value = me->add_context_value(  x_nkey_context   = lv_nkey_context "lv_context_key      "lv_key_context
                                                                    x_context_key   = lv_context_key
                                                                    x_context_field = lv_context_field
                                                                    x_fieldname     = <tabstruc>-fieldname
                                                                    x_tabname       = <tabstruc>-tabname ).
*                                                                    x_field_main    = lv_context_key

                    lv_context_added = abap_true.
                    CONTINUE.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
    IF lv_context_added = abap_true.
      APPEND lv_nkey_folder_brf_all TO r_keys.
    ENDIF.
  ENDMETHOD.


  METHOD determine_icon_context_field.
    DATA:
      lv_field TYPE fieldname,
      lv_table TYPE tabname16.

    FIELD-SYMBOLS:
      <results>  TYPE STANDARD TABLE,
      <result>   TYPE any,
      <type>     TYPE any,
      <field>    TYPE any,
      <brf_key4> TYPE any,
      <brf_key5> TYPE any,
      <brf_key6> TYPE any.

    IF po_results IS INITIAL.
*    me->build_results( e_row_id = e_row_id ).
    ENDIF.
    ASSIGN po_results->* TO <results>.

    LOOP AT <results> ASSIGNING <result>.
      ASSIGN COMPONENT 'TYPE'     OF STRUCTURE <result> TO <type>.
      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
      IF <field> IS ASSIGNED.
        SPLIT <field> AT '-' INTO lv_table lv_field.
        r_icons-field = lv_field.
      ENDIF.

      CHECK x_fieldname = lv_field AND x_tabname = lv_table.
* Get key object...
      ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE <result> TO <brf_key4>.
      IF <brf_key4> IS ASSIGNED AND <brf_key4> IS NOT INITIAL.
        r_icons-brf_key = <brf_key4>.
      ENDIF.

      IF r_icons-brf_key IS INITIAL.
        ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE <result> TO <brf_key5>.
        IF <brf_key5> IS ASSIGNED  AND <brf_key5> IS NOT INITIAL.
          r_icons-brf_key = <brf_key5>.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE <result> TO <brf_key6>.
      IF <brf_key6> IS ASSIGNED  AND <brf_key6> IS NOT INITIAL.
        CLEAR: r_icons-brf_key.
        CONCATENATE <brf_key5> '/' <brf_key6> INTO r_icons-brf_key.
      ENDIF.

      IF r_icons-brf_key IS INITIAL.
* If BRF returns a blank then set to object key
        r_icons-brf_key = x_context_key.
      ENDIF.

      IF x_context_key NE r_icons-brf_key.
        CLEAR r_icons.
        CONTINUE.
      ENDIF.
      CASE <type>.
        WHEN 'E'.
          r_icons-icon = icon_red_light.
          EXIT.
        WHEN 'W'.
          r_icons-icon = icon_yellow_light.
          EXIT.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD determine_icon_context_key.
    DATA:
      lv_field TYPE fieldname,
      lv_table TYPE struc1,
      ls_t001w TYPE t001w.

    FIELD-SYMBOLS:
      <brf_key4> TYPE any,
      <brf_key5> TYPE any,
      <brf_key6> TYPE any,
      <type>     TYPE any,
      <field>    TYPE any.

* Get the object key..
    ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE x_result TO <brf_key4>.
    IF <brf_key4> IS ASSIGNED AND <brf_key4> IS NOT INITIAL.
      r_icons-brf_key = <brf_key4>.
    ENDIF.

    IF r_icons-brf_key IS INITIAL.
      ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE x_result TO <brf_key5>.
      IF <brf_key5> IS ASSIGNED  AND <brf_key5> IS NOT INITIAL.
        r_icons-brf_key = <brf_key5>.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE x_result TO <brf_key6>.
    IF <brf_key6> IS ASSIGNED  AND <brf_key6> IS NOT INITIAL.
      CLEAR: r_icons-brf_key.
      CONCATENATE <brf_key5> '/' <brf_key6> INTO r_icons-brf_key.
    ENDIF.

    IF r_icons-brf_key IS INITIAL.
* If BRF returns a blank then set to object key
      r_icons-brf_key = x_node_main.
    ENDIF.

*    IF x_node_main = r_icons-brf_key.
*      r_icons-icon = icon_failure.
*    ELSE.
*      r_icons-icon = icon_positive.
*    ENDIF.

* If the context key is the same as the key built from the BRF Message
* then check to see if the field and table in the BRF message is contained in the confif for the given View!
    IF x_node_main = r_icons-brf_key.
      ASSIGN COMPONENT 'TYPE'     OF STRUCTURE x_result TO <type>.
      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE x_result TO <field>.
      IF <field> IS ASSIGNED.
        SPLIT <field> AT '-' INTO lv_table lv_field.
        READ TABLE x_main-tabstruc WITH KEY fieldname  = lv_field
                                              tabname   = lv_table-table TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          r_icons-icon = icon_failure.
        ELSE.
          r_icons-icon = icon_positive.
        ENDIF.
      ENDIF.
    ELSE.
     r_icons-icon = icon_positive.
    ENDIF.

* Special condition! - Consider an exit for this..
    IF x_node_level = 'WERKS'.
      SELECT SINGLE * FROM t001w
                          INTO ls_t001w
                        WHERE werks = x_node_main. "x_key_attribute.

      IF ls_t001w-vlfkz = 'A'.
*        r_icons-icon2 = icon_store_location.
       r_icons-icon2 = ICON_RETAIL_STORE .
      ELSE.
        r_icons-icon2 = icon_distribution.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD dynamic_table_populate.
    DATA:
      lv_tabname     TYPE tabname,
      lv_field       TYPE fieldname,
      lv_table       TYPE struc1,
      lv_context_key TYPE string,
      ro_line        TYPE REF TO data.

    FIELD-SYMBOLS:
      <main_setup>      LIKE LINE OF pt_main_setup,
      <main_output>     TYPE /gda/sdm_setup5,
      <main_secondary>  TYPE /gda/sdm_setup5,
      <table_primary>   TYPE ANY TABLE,
      <table_secondary> TYPE ANY TABLE,
      <line_primary>    TYPE any,
      <line_secondary>  TYPE any,
      <view_table>      TYPE STANDARD TABLE,
      <view_wa>         TYPE any,
      <sdm_object>      TYPE any,
      <sdm_data>        TYPE STANDARD TABLE,
      <results>         TYPE STANDARD TABLE,
      <results_view>    TYPE STANDARD TABLE,
      <table>           TYPE STANDARD TABLE,
      <line>            TYPE any,
      <result>          TYPE any,
      <field>           TYPE any,
      <message1>        TYPE any,
      <message2>        TYPE any,
      <brf_key4>        TYPE any,
      <brf_key5>        TYPE any,
      <brf_key6>        TYPE any,
      <node_level1>     TYPE any,
      <node_level2>     TYPE any,
      <node_level3>     TYPE any,
      <key>             TYPE any.

    ASSIGN po_alv_view->* TO <view_table>.

    ASSIGN po_table->* TO <table>.
    READ TABLE <table> ASSIGNING <line> INDEX e_row_id.

    ASSIGN po_sdm_data->* TO <sdm_data>.

    CASE pv_object_type.
      WHEN 'ARTICLE'.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <line> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('ARTICLE') = <key>.
      WHEN 'MATERIAL'.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <line> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('MATERIAL') = <key>.
      WHEN 'BOM'.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <line> TO <key>.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> WITH KEY ('BOM') = <key>.

      WHEN OTHERS.
        READ TABLE <sdm_data> ASSIGNING <sdm_object> INDEX e_row_id.
    ENDCASE.

*    READ TABLE <sdm_data> ASSIGNING <sdm_object> INDEX e_row_id.

    CREATE DATA ro_line LIKE LINE OF <view_table>.
    ASSIGN ro_line->* TO <view_wa>.

    READ TABLE pt_main_setup ASSIGNING <main_setup>  WITH KEY object_view = x_view.
    CHECK <main_setup> IS ASSIGNED.

    me->build_results( e_row_id = e_row_id ).
*    ASSIGN po_results->* TO <results>.
*    ASSIGN po_results_empty->* TO <results_view>.
    ASSIGN po_results->* TO <results>.
    IF <results_view> IS NOT ASSIGNED.
      ASSIGN po_results_empty->* TO <results_view>.
    ELSE.
      REFRESH:
       <results_view>.
    ENDIF.
*      APPEND LINES OF <results> TO <results_view>.

*  Determine if error is related to the selected view
    LOOP AT <results> ASSIGNING <result>.
      CLEAR:
       lv_table,
       lv_field.

* EXTRA_V1 contains table-field
      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
      IF <field> IS ASSIGNED.
        SPLIT <field> AT '-' INTO lv_table-table lv_field.
        IF <main_setup>-tabstruc IS INITIAL.
          <main_setup>-tabstruc = me->build_structure( x_view = x_view x_object_type = pv_object_type ).
        ENDIF.

        READ TABLE <main_setup>-tabstruc  WITH KEY fieldname = lv_field
                                                   tabname   = lv_table-table TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          APPEND <result> TO <results_view>.
        ENDIF.
      ENDIF.
    ENDLOOP.

* we now need to include all these results in teh ALV
    LOOP AT <results_view> ASSIGNING <result>.
      CLEAR:
       lv_table,
       lv_field,
       lv_context_key.

*******TEST
      ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE <result> TO <brf_key4>.
      IF <brf_key4> IS ASSIGNED AND <brf_key4> IS NOT INITIAL.
        lv_context_key = <brf_key4>.
      ENDIF.

      IF lv_context_key IS INITIAL.
        ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE <result> TO <brf_key5>.
        IF <brf_key5> IS ASSIGNED  AND <brf_key5> IS NOT INITIAL.
          lv_context_key = <brf_key5>.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE <result> TO <brf_key6>.
      IF <brf_key6> IS ASSIGNED  AND <brf_key6> IS NOT INITIAL.
        CLEAR: lv_context_key.
        CONCATENATE <brf_key5> '/' <brf_key6> INTO lv_context_key.
      ENDIF.

      IF lv_context_key IS INITIAL.
* If BRF returns a blank then set to object key
*        r_icons-brf_key = <sdm_articles>-article.
      ENDIF.
*******TEST

* EXTRA_V1 contains table-field
      ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
      IF <field> IS ASSIGNED.
        SPLIT <field> AT '-' INTO lv_table-table lv_field.
        IF <main_setup>-tabstruc IS INITIAL.
          <main_setup>-tabstruc = me->build_structure( x_view = x_view x_object_type = pv_object_type ).
        ENDIF.

        READ TABLE <main_setup>-tabstruc  WITH KEY fieldname = lv_field
                                                   tabname   = lv_table-table TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
*          APPEND <result> TO <results_view>.
          LOOP AT <main_setup>-sequence ASSIGNING <main_output> WHERE seq = '1'.

            ASSIGN COMPONENT 'NODE_LEVEL' OF STRUCTURE <main_output> TO <node_level1>.
            ASSIGN COMPONENT 'NODE_LEVEL2' OF STRUCTURE <main_output> TO <node_level2>.
            ASSIGN COMPONENT 'NODE_LEVEL3' OF STRUCTURE <main_output> TO <node_level3>.

            lv_tabname = '<SDM_OBJECT>-&&&&'.
            REPLACE ALL OCCURRENCES OF '&&&&' IN lv_tabname WITH <main_output>-tabname.

            ASSIGN (lv_tabname) TO <table_primary>.
* Primary table
* Extract fields from primary table that are specified in config
            CHECK <table_primary> IS ASSIGNED.
            LOOP AT <table_primary> ASSIGNING <line_primary>.
*TEST
              FIELD-SYMBOLS:
                <key_primary1>   TYPE any,
                <key_primary2>   TYPE any,
                <key_primary3>   TYPE any,
                <key_secondary1> TYPE any,
                <key_secondary2> TYPE any,
                <key_secondary3> TYPE any.

              ASSIGN COMPONENT <node_level1> OF STRUCTURE <line_primary> TO <key_primary1>.
              IF <node_level2> IS NOT INITIAL.
                ASSIGN COMPONENT <node_level2> OF STRUCTURE <line_primary> TO <key_primary2>.
              ENDIF.
              IF <node_level3> IS NOT INITIAL.
                ASSIGN COMPONENT <node_level3> OF STRUCTURE <line_primary> TO <key_primary3>.
              ENDIF.

              IF <key_primary1> IS NOT INITIAL.
                IF <key_primary1> NE lv_context_key.
                  CONTINUE.
                ENDIF.
              ENDIF.

*TEST
              MOVE-CORRESPONDING <line_primary> TO <view_wa>.
* Seconday Table
* Extract fields from secondary table that are specified in config

*              FIELD-SYMBOLS:
*                <werks_secondary> TYPE any.

              LOOP AT <main_setup>-sequence ASSIGNING <main_secondary> WHERE seq = '2'.

                lv_tabname = '<SDM_OBJECT>-&&&&'.
                REPLACE ALL OCCURRENCES OF '&&&&' IN lv_tabname WITH <main_output>-tabname.

                ASSIGN (lv_tabname) TO <table_secondary>.

                CHECK <table_secondary> IS ASSIGNED.
                LOOP AT <table_secondary> ASSIGNING <line_secondary>.
*                  ASSIGN COMPONENT 'WERKS' OF STRUCTURE <line_secondary> TO <werks_secondary>.
                  ASSIGN COMPONENT <node_level1> OF STRUCTURE <line_secondary> TO <key_secondary1>.
                  IF <node_level2> IS NOT INITIAL.
                    ASSIGN COMPONENT <node_level2> OF STRUCTURE <line_secondary> TO <key_secondary2>.
                  ENDIF.
                  IF <node_level3> IS NOT INITIAL.
                    ASSIGN COMPONENT <node_level3> OF STRUCTURE <line_secondary> TO <key_secondary3>.
                  ENDIF.

                  IF <key_secondary1> IS ASSIGNED.
                    IF <key_secondary1> NE lv_context_key.
                      CONTINUE.
                    ENDIF.
                  ENDIF.

                  MOVE-CORRESPONDING <line_secondary> TO <view_wa>.
                ENDLOOP.
*                APPEND <view_wa> TO <view_table>.
              ENDLOOP.
            ENDLOOP.
          ENDLOOP.

          ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <result> TO <message1>.

          ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <view_wa> TO <message2>.

          <message2> = <message1>.

          APPEND <view_wa> TO <view_table>.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD handle_context_menu.

*    BREAK-POINT.
* https://archive.sap.com/discussions/thread/910750
*   define local data
    DATA:
      lt_fcodes     TYPE ui_funcattr,
      ls_fcode      TYPE uiattentry,
      ls_func       TYPE ui_func,
      lt_func       TYPE ui_functions,
      lt_actions    TYPE /gda/sdm_tt_obj_action,
      lt_key_fields TYPE /gda/sdm_t_key_fields,
      ls_row        TYPE lvc_s_row,
      ls_col        TYPE lvc_s_col.

    FIELD-SYMBOLS:
      <obj_key>     TYPE any,
      <dyn_wa>      TYPE any,
      <sdm_data_wa> TYPE any,
      <key_field>   TYPE any,
      <action>      LIKE LINE OF lt_actions,
      <table>       TYPE STANDARD TABLE,
      <sdm_data>    TYPE STANDARD TABLE.

    CALL METHOD sender->get_current_cell
      IMPORTING
        es_row_id = ls_row
        es_col_id = ls_col.


    ASSIGN po_table->* TO <table>.
    ASSIGN po_sdm_data->* TO <sdm_data>.

    lt_actions    = po_object->load_related_actions( ).
    lt_key_fields = po_object->get_object_keys( ).

* get the object related to this row..
    READ TABLE <table> ASSIGNING <dyn_wa> INDEX ls_row.
    CHECK sy-subrc = 0.
    LOOP AT lt_key_fields ASSIGNING <key_field>.
      ASSIGN COMPONENT <key_field> OF STRUCTURE <dyn_wa> TO <obj_key>.
    ENDLOOP.

    READ TABLE <sdm_data> ASSIGNING <sdm_data_wa> INDEX ls_row.

* Deactivate all standard functions
    CALL METHOD e_object->get_functions
      IMPORTING
        fcodes = lt_fcodes.

    LOOP AT lt_fcodes INTO ls_fcode.
      ls_func = ls_fcode-fcode.
      APPEND ls_func TO lt_func.
    ENDLOOP.

    e_object->hide_functions( lt_func ).
    e_object->add_separator( ).

* Now Add SDM specific functions
    LOOP AT lt_actions ASSIGNING <action>.

*      TRY.
*          GET BADI sdm_handle_context.
*        CATCH cx_badi_not_implemented.
*          CLEAR sdm_handle_context.
*      ENDTRY.
*
*      IF NOT sdm_handle_context IS INITIAL.
*
*        CALL BADI sdm_handle_context->context_menu_change
*          EXPORTING
*            xo_core           = <sdm_instance>-object
*          CHANGING
*            xo_action         = <action>
*          EXCEPTIONS
*            application_error = 1
*            OTHERS            = 2.
*        IF sy-subrc <> 0.
*          MESSAGE e() RAISING application_error.
*        ENDIF.
*      ENDIF.


      IF ls_col = <action>->get_view( ) OR <action>->get_view( ) = 'DEFAULT'.
        CALL METHOD <action>->add_to_context_menu
          EXPORTING
            xy_ctmenu = e_object.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_user_command.

    DATA:
      ls_row        TYPE lvc_s_row,
      ls_col        TYPE lvc_s_col,
      lt_actions    TYPE /gda/sdm_tt_obj_action,
      lt_key_fields TYPE /gda/sdm_t_key_fields,
      lv_fcode      TYPE /gda/sdm_cact-fcode.

    FIELD-SYMBOLS:
      <action>        LIKE LINE OF lt_actions,
      <key_field>     LIKE LINE OF lt_key_fields,
      <obj_key>       TYPE any,
      <material2>     TYPE any,
      <dyn_wa>        TYPE any,
      <sdm_data_wa>   TYPE any,
      <sdm_instance>  TYPE /gda/sdm_s_instances,
      <sdm_instances> TYPE /gda/sdm_t_instances,
      <table>         TYPE STANDARD TABLE,
      <sdm_data>      TYPE STANDARD TABLE.


    ASSIGN po_table->*    TO <table>.
    ASSIGN po_sdm_data->* TO <sdm_data>.

    CASE e_ucomm.
      WHEN 'MASS'.
        PERFORM mass_download IN PROGRAM /gda/sdm_scustom_rsr IF FOUND.
*        me->mass_download( ).
      WHEN OTHERS.
        CALL METHOD sender->get_current_cell
          IMPORTING
            es_row_id = ls_row
            es_col_id = ls_col.

        lt_key_fields = po_object->get_object_keys( ).

        READ TABLE <table>    ASSIGNING <dyn_wa>      INDEX ls_row.
        READ TABLE <sdm_data> ASSIGNING <sdm_data_wa> INDEX ls_row.
        CHECK sy-subrc = 0.
        LOOP AT lt_key_fields ASSIGNING <key_field>.
          ASSIGN COMPONENT <key_field> OF STRUCTURE <dyn_wa> TO <obj_key>.
        ENDLOOP.

        ASSIGN COMPONENT 'SDM_INSTANCES' OF STRUCTURE <sdm_data_wa> TO <sdm_instances>.

        READ TABLE <sdm_instances> ASSIGNING <sdm_instance> INDEX 1.

        lt_actions = <sdm_instance>-object->load_related_actions( ).

        LOOP AT lt_actions ASSIGNING <action>.
          IF ls_col = <action>->get_view( ) OR <action>->get_view( ) = 'DEFAULT'.
            CALL METHOD <action>->get_fcode
              RECEIVING
                r_fcode = lv_fcode.
            IF lv_fcode = e_ucomm.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

        CALL METHOD <sdm_instance>-object->/gda/sdm_if_action_processor~process_action
          EXPORTING
            x_action = <action>
            x_multi  = space
*  IMPORTING
*           y_refresh        =
*           y_action_handled =
*           y_not_authorised =
          .
    ENDCASE.
  ENDMETHOD.


  METHOD mass_download.
    DATA:
      lv_row_id TYPE lvc_s_row.

    FIELD-SYMBOLS:
      <results_all> TYPE STANDARD TABLE,
      <results>     TYPE STANDARD TABLE,
      <dyn_table>   TYPE STANDARD TABLE,
      <main_setup>  LIKE LINE OF pt_main_setup.

    DO gv_no_records TIMES.
      ADD 1 TO lv_row_id.
      me->build_results( e_row_id = lv_row_id ).
      ASSIGN po_results->* TO <results>.
      IF <results_all> IS NOT ASSIGNED.
        ASSIGN po_results_empty->* TO <results_all>.
      ENDIF.
      APPEND LINES OF <results> TO <results_all>.
    ENDDO.

    ASSIGN po_table->* TO  <dyn_table>.


    READ TABLE pt_main_setup assigning <main_setup> WITH KEY object_view = 'DEFAULT'.
    CHECK sy-subrc = 0.
    EXPORT  gt_view_struc = <main_setup>-tabstruc TO MEMORY ID 'TABSTRUC'.
*    EXPORT  gs_main_setup = ls_default_setup TO MEMORY ID 'DEFAULT_SETUP'.

    EXPORT <results>    = <results_all>         TO MEMORY ID 'RESULTS'.
    EXPORT <dyn_table>  = <dyn_table>           TO MEMORY ID 'TABLE'.

    SUBMIT /gda/sdm_record_status_dwnld AND RETURN.
  ENDMETHOD.


  METHOD on_hotspot_click.

    DATA:
      lv_row_no     TYPE i,
      lt_actions    TYPE /gda/sdm_tt_obj_action,
      lt_key_fields TYPE /gda/sdm_t_key_fields,
      lv_fcode      TYPE /gda/sdm_cact-fcode.

    FIELD-SYMBOLS:
      <obj_key>       TYPE any,
      <status>        TYPE any,
      <main_setup>    LIKE LINE OF pt_main_setup,
      <action>        LIKE LINE OF lt_actions,
      <table>         TYPE STANDARD TABLE,
      <dyn_wa>        TYPE any,
      <key_field>     TYPE any,
      <sdm_data>      TYPE STANDARD TABLE,
      <sdm_data_wa>   TYPE any,
      <sdm_instances> TYPE /gda/sdm_t_instances,
      <sdm_instance>  TYPE /gda/sdm_s_instances,
      <key>           TYPE any.

    lv_row_no = e_row_id.

    ASSIGN po_table->*    TO <table>.
    ASSIGN po_sdm_data->* TO <sdm_data>.

    lt_actions    = po_object->load_related_actions( ).
    lt_key_fields = po_object->get_object_keys( ).
* Set DEFAULT action as HOTSPOT
    LOOP AT lt_actions ASSIGNING <action>.
      IF <action>->get_view( ) = 'DEFAULT'.
        CALL METHOD <action>->get_fcode
          RECEIVING
            r_fcode = lv_fcode.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_row_no IS NOT INITIAL.
      READ TABLE  <table>[] ASSIGNING <dyn_wa>      INDEX lv_row_no.

      CASE pv_object_type.
        WHEN 'ARTICLE'.
          ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <key>.
          READ TABLE <sdm_data> ASSIGNING <sdm_data_wa> WITH KEY ('ARTICLE') = <key>.
        WHEN 'MATERIAL'.
          ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <key>.
          READ TABLE <sdm_data> ASSIGNING <sdm_data_wa> WITH KEY ('MATERIAL') = <key>.
        WHEN 'BOM'.
          ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <key>.
          READ TABLE <sdm_data> ASSIGNING <sdm_data_wa> WITH KEY ('BOM') = <key>.
        WHEN 'SCUSTOM'.
          ASSIGN COMPONENT 'KEY_ID' OF STRUCTURE <dyn_wa> TO <key>.
          READ TABLE <sdm_data> ASSIGNING <sdm_data_wa> WITH KEY ('CUSTOMER') = <key>.

        WHEN OTHERS.
          READ TABLE <sdm_data> ASSIGNING <sdm_data_wa> INDEX lv_row_no.
      ENDCASE.

      ASSIGN COMPONENT 'SDM_INSTANCES' OF STRUCTURE <sdm_data_wa> TO <sdm_instances>.

      LOOP AT <sdm_instances> ASSIGNING <sdm_instance>.
        EXIT.
      ENDLOOP.

      IF sy-subrc EQ 0.
        IF e_column_id CS 'KEY'.
          <sdm_instance>-object->/gda/sdm_if_action_processor~process_action( x_action = <action> x_multi = abap_false ).
        ELSE.

          LOOP AT lt_key_fields ASSIGNING <key_field>.
            ASSIGN COMPONENT <key_field> OF STRUCTURE <dyn_wa> TO <obj_key>.
          ENDLOOP.

*          IF po_tree IS BOUND.
*            po_tree->free( ).
*          ENDIF.
*          IF po_alv IS BOUND.
*            po_alv->free( ).
*          ENDIF.

          READ TABLE pt_main_setup ASSIGNING <main_setup>  WITH KEY object_view = e_column_id.
          IF <main_setup>-tree = abap_false.
            me->set_view_alv( e_column_id  = e_column_id
                               e_row_id     = e_row_id
                               e_obj_key    = <obj_key>  ).
          ELSE.
            me->set_view_tree( e_column_id  = e_column_id
                               e_row_id     = e_row_id
                               e_obj_key    = <obj_key> ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_view_alv.

    DATA:
      ls_layout     TYPE lvc_s_layo,
      ls_variant    TYPE disvariant,
      lt_view_struc TYPE lvc_t_fcat.

    FIELD-SYMBOLS:
      <view_alv> TYPE STANDARD TABLE.

    IF po_alv IS BOUND.
      po_alv->free( ).
      FREE po_alv.
    ENDIF.

    IF po_tree IS BOUND.
      po_tree->free( ).
      FREE po_tree.
    ENDIF.

* create an instance of alv control
    CREATE OBJECT po_alv
      EXPORTING
        i_parent = po_parent.

    ls_layout-cwidth_opt = abap_true.

    ls_variant-report    = sy-repid.
    CONCATENATE '/D_' pv_object_type+0(3) '_' e_column_id+0(5) INTO ls_variant-variant.

    lt_view_struc = me->build_structure( x_view = e_column_id x_object_type = pv_object_type ).
    me->build_dynamic_table( x_view_structure = lt_view_struc ).
    me->dynamic_table_populate( x_view = e_column_id e_row_id = e_row_id ).

    ASSIGN po_alv_view->* TO <view_alv>.

    CALL METHOD po_alv->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout
        is_variant      = ls_variant
        i_save          = 'A'
        i_default       = 'X'
      CHANGING
        it_fieldcatalog = lt_view_struc
        it_outtab       = <view_alv>.


  ENDMETHOD.


  METHOD set_view_tree.
    DATA:
      ls_hier_hdr TYPE treev_hhdr,
      ls_variant  TYPE disvariant,
      lt_keys     TYPE lvc_t_nkey.

*** Only if in Error..
*  IF x_status = icon_red_light OR x_status = icon_green_light  OR x_status = icon_yellow_light.
*    IF go_alv IS BOUND.
*      go_alv->free( ).
*      FREE go_alv.
*    ENDIF.

    IF po_alv IS BOUND.
      po_alv->free( ).
      FREE po_alv.
    ENDIF.

    IF po_tree IS BOUND.
      po_tree->free( ).
      FREE po_tree.
    ENDIF.

    IF po_tree IS INITIAL.
* create tree control
      CREATE OBJECT po_tree
        EXPORTING
          parent                      = po_parent
          node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
          item_selection              = abap_true
          no_html_header              = abap_true
          no_toolbar                  = abap_false
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          illegal_node_selection_mode = 5
          failed                      = 6
          illegal_column_name         = 7.
      IF sy-subrc <> 0.
        MESSAGE x208(00) WITH 'ERROR'.                      "#EC NOTEXT
      ENDIF.
    ENDIF.

    ls_hier_hdr = me->build_hierarchy_header( ).

    ls_variant-report = sy-repid.
    ls_variant-variant = '/DEFAULT'.

    CALL METHOD po_tree->set_table_for_first_display
      EXPORTING
        is_variant          = ls_variant
        i_save              = 'A'
        i_default           = 'X'
        i_structure_name    = '/GDA/SDM_S_VAL_RETURN_GUI'
        is_hierarchy_header = ls_hier_hdr
      CHANGING
        it_outtab           = pt_result.

*    IF x_status = icon_red_light OR x_status = icon_yellow_light.
* Create hierachy -
* Folders - BRF Errors All, Context
    lt_keys = me->create_tree_hierarchy( e_column_id = e_column_id
                                         e_row_id    = e_row_id
                                         e_obj_key   = e_obj_key ).
*    ENDIF.
* Send data to frontend.
    CALL METHOD po_tree->expand_nodes( it_node_key = lt_keys ).
    CALL METHOD po_tree->frontend_update.
*  ENDIF.

  ENDMETHOD.


  method TOOLBAR.

    DATA:
      ls_toolbar TYPE stb_button,
      lv_has_activex.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = lv_has_activex.

    CHECK lv_has_activex = abap_true.
*...Seperator
    ls_toolbar-function  = 'DUMMY'.
    ls_toolbar-butn_type = '3'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

*... Normal Button
*    IF sy-uname = 'BSCHREUDER' OR sy-uname = 'RROELOFSE'. "##USER_OK
      CLEAR ls_toolbar.
      ls_toolbar-function  = 'MASS'(931).                   "#EC NOTEXT
      ls_toolbar-icon      = icon_xxl.
      ls_toolbar-butn_type = '0'.
      ls_toolbar-disabled  = space.
      ls_toolbar-text      = 'Download Report'(932).          "#EC NOTEXT
      ls_toolbar-quickinfo = space.
      ls_toolbar-checked   = space.
      APPEND ls_toolbar TO e_object->mt_toolbar.
*    ENDIF.

  endmethod.
ENDCLASS.
