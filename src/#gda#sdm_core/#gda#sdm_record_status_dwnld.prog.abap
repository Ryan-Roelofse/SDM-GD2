*&---------------------------------------------------------------------*
*& Report /GDA/SDM_RECORD_STATUS_DWNLD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_record_status_dwnld.
*INCLUDE /gda/fordata.
*INCLUDE /gda/sdm_data_core.
*
*DATA:
*  lv_template TYPE sy-repid,
*  gt_tabstruc TYPE STANDARD TABLE OF lvc_s_fcat,
*  ro_results  TYPE REF TO data,
*  ro_table    TYPE REF TO data.
*
*FIELD-SYMBOLS:
*  <results>      TYPE STANDARD TABLE.
**   <articles> LIKE LINE OF gt_sdm_articles.
*
*START-OF-SELECTION.
*
*  SELECT * FROM /gda/sdm_setup3 INTO CORRESPONDING FIELDS OF TABLE gt_pp_main_setup
*    WHERE object_type = 'ARTICLE'
*      AND object_view = gc_default
*      AND status      = abap_true
*      ORDER BY ord.
*
*
*  CREATE DATA ro_results TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key.
*  ASSIGN ro_results->* TO <results>.
*
**  IMPORT gt_tabstruc FROM MEMORY ID 'TABSTRUC'.
*  IMPORT gt_view_struc FROM MEMORY ID 'TABSTRUC'.
*
*
**  PERFORM build_structure USING gc_default
**                                'ARTICLE'
**                                space.
*
*  PERFORM build_dynamic_itab USING gc_default
*                             CHANGING ro_table.
*
** Build a structure  based on teh top alv output...
**  CREATE DATA ro_table TYPE STANDARD TABLE OF "/gda/sdm_s_val_results_key.
*  ASSIGN ro_table->* TO <dyn_table>.
*
*  IMPORT <results>   FROM MEMORY ID 'RESULTS'.
*  IMPORT <dyn_table> FROM MEMORY ID 'TABLE'.
*
**  PERFORM process_spreadsheet.
*
*  lv_template = '/GDA/SDM_POSTPROC'.
*  EXPORT lv_template TO MEMORY ID 'TEMPLATE'.
**-----Create SAP Document
*  PERFORM create_sapdoc.
*
*
*FORM process_spreadsheet .
**-----Populate main sheet
*  PERFORM pop_main_sheet.
**-----Populate main sheet
**  PERFORM pop_details_sheet.
**-----Populate context sheet
**  PERFORM pop_context_sheet.
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  POP_MAIN_SHEET
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM pop_main_sheet .
*  DATA:
*    lt_fldcat   TYPE slis_t_fieldcat_alv WITH HEADER LINE.
*
*  FIELD-SYMBOLS:
*   <tabstruc> LIKE lvc_s_fcat. "<main_setup>-tabstruc[].
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
** import this from the class..<main_setup>-tabstruc
*  LOOP AT gt_view_struc ASSIGNING <tabstruc>.
*    MOVE-CORRESPONDING <tabstruc> TO lt_fldcat.
*    APPEND lt_fldcat.
*    CLEAR lt_fldcat.
*  ENDLOOP.
*
*  fieldcat[] = lt_fldcat[].
*
*  PERFORM load_fieldcat.
*
*ENDFORM.
*
*FORM pop_context_details.
*
*ENDFORM.
*
*FORM pop_main.
** Header
*  WRITE range_item-name TO r_ztab LEFT-JUSTIFIED.
*  APPEND r_ztab TO i_ztab.
*
*  LOOP AT <dyn_table> ASSIGNING <dyn_wa>.
*    CONCATENATE '<dyn_wa>-' range_item-name INTO name.
*
*    ASSIGN (name) TO <cell>.
*
*    CHECK sy-subrc = 0.
*
*    WRITE <cell> TO r_ztab LEFT-JUSTIFIED.
*
*    APPEND r_ztab TO i_ztab.
*  ENDLOOP.
*
*ENDFORM.
*
*FORM pop_main_details.
*  DATA:
***    new,
***    tabix         LIKE sy-tabix,
*    ro_data       TYPE REF TO data,
*    ro_data_empty TYPE REF TO data.
*
*  FIELD-SYMBOLS:
**    <articles>     LIKE LINE OF gt_sdm_articles,
**    <instances>    LIKE LINE OF <articles>-sdm_instances,
**    <results>      TYPE STANDARD TABLE,
*    <results_temp> TYPE STANDARD TABLE,
*    <result>       TYPE any.
*
** Header
*  WRITE range_item-name TO r_ztab LEFT-JUSTIFIED.
*  APPEND r_ztab TO i_ztab.
*
**  LOOP AT gt_sdm_articles ASSIGNING <articles>.
**
*** Test changes
**    UNASSIGN <results>.
**
**    LOOP AT <articles>-sdm_instances ASSIGNING <instances>.
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
*  LOOP AT <results> ASSIGNING <result>.
*    IF range_item-name = 'MATNR'.
*      name = '<articles>-article'.
*    ELSE.
*      CONCATENATE '<result>-' range_item-name INTO name.
*    ENDIF.
*
*    ASSIGN (name) TO <cell>.
*
*    CHECK sy-subrc = 0.
*
*    WRITE <cell> TO r_ztab LEFT-JUSTIFIED.
*
*    APPEND r_ztab TO i_ztab.
*  ENDLOOP.
*
**  ENDLOOP.
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
*  LOOP AT gt_view_struc ASSIGNING <fieldsymbol>.
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
*FORM build_structure USING x_view        TYPE lvc_s_col
*                           x_object_type TYPE /gda/sdm_de_object
*                           x_struc       TYPE char1.
*  DATA:
*    lo_ref_table_des TYPE REF TO cl_abap_structdescr,
*    lt_view_fields   TYPE STANDARD TABLE OF /gda/sdm_setup4,
*    lt_details       TYPE abap_compdescr_tab,
*    lv_lines         TYPE i.
*
*  FIELD-SYMBOLS:
*    <general>        LIKE LINE OF lt_view_fields,
*    <default_fields> LIKE LINE OF lt_view_fields,
*    <details>        LIKE LINE OF lt_details,
*    <view_struc>     LIKE LINE OF gt_view_struc,
*    <parameter>      TYPE any.
*
*  REFRESH:
*   gt_view_tables.
*
** Retrieve fields for specific view
*  SELECT * FROM /gda/sdm_setup4 INTO TABLE lt_view_fields
*    WHERE object_type  = x_object_type
*      AND object_view  = x_view
*      AND outp         = abap_true.
*
** Ensure that default field are displayed in all views.
*  LOOP AT gt_default_fields ASSIGNING <default_fields>.
*    READ TABLE lt_view_fields ASSIGNING <general>
*    WITH KEY field   = <default_fields>-field
*             tabname = <default_fields>-tabname.
*    IF sy-subrc <> 0.
*      CONCATENATE 'KEY_' <default_fields>-field INTO <default_fields>-field.
*      APPEND <default_fields> TO lt_view_fields.
*    ELSE.
*      IF x_view <> gc_default.
*        CONCATENATE 'KEY_' <default_fields>-field INTO <default_fields>-field.
*        <default_fields>-object_view = x_view.
*        APPEND <default_fields> TO lt_view_fields.
*      ELSE.
*        CONCATENATE 'KEY_' <general>-field  INTO <general>-field.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  SORT lt_view_fields BY default_all DESCENDING ord.
** Build a table of related tables for the view
*  LOOP AT lt_view_fields ASSIGNING <general>.
*    gs_table = <general>-tabname.
*    APPEND gs_table TO gt_view_tables.
*  ENDLOOP.
*
*  SORT gt_view_tables.
*  DELETE ADJACENT DUPLICATES FROM gt_view_tables.
*
*  LOOP AT gt_view_tables INTO gs_table.
*    lo_ref_table_des ?=
*        cl_abap_typedescr=>describe_by_name( gs_table ).
*
*    lt_details[] = lo_ref_table_des->components[].
*
*    LOOP AT lt_view_fields ASSIGNING <general> WHERE tabname = gs_table.
*      IF <general>-field CS 'KEY_'.
*        ls_field = <general>-field+4(6).
*        READ TABLE lt_details ASSIGNING <details> WITH KEY name = ls_field.
*      ELSE.
*        READ TABLE lt_details ASSIGNING <details> WITH KEY name = <general>-field.
*      ENDIF.
*      CHECK sy-subrc = 0.
*      CLEAR: gs_view_struc.
*      gs_view_struc-fieldname = <general>-field.
*      gs_view_struc-tabname   = <general>-tabname.
*      gs_view_struc-datatype  = <details>-type_kind.
*      CASE <details>-type_kind.
*        WHEN 'C'.
*          gs_view_struc-datatype = 'CHAR'.
*        WHEN 'N'.
*          gs_view_struc-datatype = 'NUMC'.
*        WHEN 'D'.
*          gs_view_struc-datatype = 'DATE'.
*        WHEN 'P'.
*          gs_view_struc-datatype = 'PACK'.
*        WHEN OTHERS.
*          gs_view_struc-datatype = <details>-type_kind.
*      ENDCASE.
*
*      IF <general>-field CS 'KEY_'.
*        gs_view_struc-key      = abap_true.
*      ENDIF.
*
*      gs_view_struc-inttype  = <details>-type_kind.
*      gs_view_struc-intlen   = <details>-length.
*      gs_view_struc-decimals = <details>-decimals.
*      gs_view_struc-col_pos  = <general>-ord.
*
*      SELECT SINGLE scrtext_s scrtext_m scrtext_l FROM dd03m
*             INTO (gs_view_struc-scrtext_s, gs_view_struc-scrtext_m, gs_view_struc-scrtext_l)
*            WHERE tabname    = <general>-tabname
*              AND fieldname  = <general>-field
*              AND ddlanguage = sy-langu.
*
*      IF sy-subrc <> 0.
*        SELECT SINGLE scrtext_s scrtext_m scrtext_l FROM dd03m
*               INTO (gs_view_struc-scrtext_s, gs_view_struc-scrtext_m, gs_view_struc-scrtext_l)
*              WHERE tabname    = <general>-tabname
*                AND fieldname  = <general>-field+4
*                AND ddlanguage = sy-langu.
*      ENDIF.
*
*      APPEND gs_view_struc TO gt_view_struc.
*    ENDLOOP.
*  ENDLOOP.
*
*  CLEAR:
*    gs_view_struc,
*    lv_lines.
*
*  SORT gt_view_struc BY key DESCENDING col_pos.
*
*  LOOP AT gt_view_struc ASSIGNING <view_struc>.
*    lv_lines = lv_lines + 1.
*    <view_struc>-col_pos =  lv_lines.
*  ENDLOOP.
*
*  DESCRIBE TABLE gt_view_struc LINES lv_lines.
*
*  IF x_view  = gc_default.
** Add an additional hidden linkage field to the end for DEFAULT View. But only if sructured oobject is selected
*    lv_lines = lv_lines + 1.
*    gs_view_struc-fieldname  = 'LINKAGE'.
*    IF x_struc = abap_false.
*      gs_view_struc-no_out = abap_true.
*    ENDIF.
*    gs_view_struc-datatype   = 'CHAR'.
*    gs_view_struc-inttype    = 'C'.
*    gs_view_struc-intlen     = 000018.
*    gs_view_struc-outputlen  = 000018.
*    gs_view_struc-col_pos    = 1.
*    gs_view_struc-scrtext_l  = TEXT-012." 'Structured Material'.
*    gs_view_struc-scrtext_m  = TEXT-012." 'Structured Material'.
*    gs_view_struc-scrtext_s  = TEXT-012." 'Structured Material'.
*    APPEND gs_view_struc TO gt_view_struc.
*    CLEAR:
*     gs_view_struc.
*
*    LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*      IF <main_setup>-object_view NE gc_default.
*        lv_lines = lv_lines + 1.
*        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*        IF <parameter> = abap_true.
*          gs_view_struc-fieldname  = <main_setup>-object_view.
*          gs_view_struc-datatype   = 'CHAR'.
*          gs_view_struc-inttype    = 'C'.
*          gs_view_struc-intlen     = 000004.
*          gs_view_struc-outputlen  = 000004.
*          gs_view_struc-icon       = abap_true.
*          gs_view_struc-col_pos    = lv_lines.
*          gs_view_struc-hotspot    =  abap_true.
*          APPEND gs_view_struc TO gt_view_struc.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*  ELSE.
** BRF Plus Fields..
*    lv_lines = lv_lines + 1.
*    gs_view_struc-fieldname  = 'MESSAGE'.
*    gs_view_struc-datatype   = 'CHAR'.
*    gs_view_struc-inttype    = 'C'.
*    gs_view_struc-intlen     = 000220.
*    gs_view_struc-outputlen  = 000100.
*    gs_view_struc-col_pos    = lv_lines.
*    APPEND gs_view_struc TO gt_view_struc.
*  ENDIF.
*ENDFORM.
*
*FORM build_dynamic_itab USING x_view TYPE lvc_s_col
*                        CHANGING xo_data TYPE REF TO data.
*
*
*
*  DATA:
*    ro_table TYPE REF TO data,
*    ro_line  TYPE REF TO data.
*
*  FIELD-SYMBOLS:
*     <main_setup> LIKE LINE OF gt_pp_main_setup.
*
*  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = x_view.
*  IF sy-subrc <> 0 OR <main_setup>-ro_table IS INITIAL.
** Create dynamic internal table and assign to FS
*    CALL METHOD cl_alv_table_create=>create_dynamic_table
*      EXPORTING
*        it_fieldcatalog  = gt_view_struc
*        i_length_in_byte = 'X'
*      IMPORTING
*        ep_table         = ro_table.
*
*    IF x_view = gc_default.
*      ASSIGN ro_table->* TO <dyn_table>.
**      ASSIGN ro_table->* TO <dyn_table_final>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table>.
**       CREATE DATA ro_line LIKE LINE OF <dyn_table_final>.
*      ASSIGN ro_line->* TO <dyn_wa>.
*    ELSE.
*      ASSIGN ro_table->* TO <dyn_table_view>.
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
*    ENDIF.
*  ENDIF.
*
*  xo_data = ro_table.
*
**  REFRESH:
**    gt_view_struc[].
*ENDFORM.                    " BUILD_DYNAMIC_ITAB
