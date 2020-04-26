*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_SCUSTOM_LOGIC
*&---------------------------------------------------------------------*

FORM   sdm_main_scustom.
  DATA:
    lo_data_empty TYPE REF TO data,
    lo_data       TYPE REF TO data,
    lv_count      TYPE p,
    lv_per        TYPE p,
    lv_text       TYPE string,
    lv_per_text   TYPE string.

  FIELD-SYMBOLS:
    <results_temp> LIKE gt_results.

  PERFORM progress_bar USING TEXT-018.

  DESCRIBE TABLE go_selection->mt_scustom LINES lv_count.

  LOOP AT go_selection->mt_scustom INTO go_selection->ms_scustom_spec.

    lv_per = ( sy-tabix / lv_count ) * 100.
    lv_per_text = lv_per.
    CONCATENATE 'BRF Rules '(917)  lv_per_text '%' INTO lv_text.
    PERFORM progress_bar USING lv_text.

* BRF+ Logic
* Prepare the data for BRF functions - pass to temp tables
    go_selection->refresh( ).
    go_selection->mv_spec_id = go_selection->ms_scustom_spec-id.
    go_selection->build_spec( ).

    gt_scustom_sdm[]   = go_selection->mt_scustom_spec[].

* For each SDM Object process the BRF Functions
    LOOP AT gt_objects ASSIGNING <objects>.
      CLEAR:
       <objects>-object.

      TRY.
          <objects>-object = /gda/sdm_cl_common_core=>sdm_initialise(  iv_object_type = gv_object
                                                                       iv_source       = gc_rep
                                                                       iv_type         = <objects>-type
                                                                       iv_stats        = abap_false ).
        CATCH /gda/cx_sdm_exception_handl.
          CONTINUE.
      ENDTRY.

      gt_attributes = <objects>-object->get_object_attributes( iv_type = <objects>-type   ).

      LOOP AT gt_attributes ASSIGNING <attribute>.
        ASSIGN (<attribute>-abap_type) TO <set_data>.

        TRY.
            <objects>-object->set_selection( iv_name = <attribute>-name
                                             iv_data = <set_data>
                                             iv_type = <attribute>-type ).
          CATCH /gda/cx_sdm_exception_handl ##NO_HANDLER.
        ENDTRY.
      ENDLOOP.

* Process data
      TRY.
          /gda/sdm_cl_common_core=>sdm_process_data( xo_object = <objects>-object ).
        CATCH /gda/cx_sdm_exception_handl.
          CONTINUE.
      ENDTRY.

* Process results
      lo_data = <objects>-object->return_brf_result( ).
      ASSIGN lo_data->* TO <results_temp>.

      IF <results> IS NOT ASSIGNED.
        lo_data_empty  = <objects>-object->return_brf_result_structure( ).
        ASSIGN lo_data_empty->* TO <results>.
        REFRESH:
         <results>.
      ENDIF.
      APPEND LINES OF <results_temp> TO <results>.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

FORM mass_download.
  DATA:
    lv_template TYPE sy-repid.

  PERFORM process_spreadsheet.

  lv_template = '/GDA/SDM_SCUSTOM'.
  EXPORT lv_template TO MEMORY ID 'TEMPLATE'.
*Create SAP Document
  PERFORM create_sapdoc.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_SPREADSHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_spreadsheet .
*-----Populate main sheet
  PERFORM pop_main_sheet.
*-----Populate main sheet
  PERFORM pop_details_sheet.
*-----Populate context sheet
*  PERFORM pop_context_sheet.
*-----Populate Calcs sheet
*  PERFORM pop_calcs_sheet.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POP_MAIN_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_main_sheet.
  DATA:
    fldcat   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
    lt_views TYPE STANDARD TABLE OF /gda/sdm_setup3.

  FIELD-SYMBOLS:
    <fieldsymbol> LIKE LINE OF <main_setup>-tabstruc[],
    <fieldname>   TYPE any,
    <views>       LIKE LINE OF lt_views.

*-----Main - 1st sheet
  MOVE 'DATA' TO v_sheet.

*------Starting row
  MOVE '1' TO v_row.

*-----Starting column
  MOVE '0' TO v_col.

  LOOP AT <main_setup>-tabstruc ASSIGNING <fieldsymbol>.

    MOVE-CORRESPONDING <fieldsymbol> TO fldcat.
    APPEND fldcat.
    CLEAR fldcat.
    ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <fieldsymbol> TO <fieldname>.
    IF <fieldname> = 'LINKAGE'.
* Display All Views in Spreedsheet and hide ones not populated.
      EXIT.
    ENDIF.
  ENDLOOP.

  SELECT * FROM /gda/sdm_setup3 INTO TABLE lt_views
           WHERE object_type = <main_setup>-object_type
            AND  object_view <> gc_default.
*            ORDER BY ord.
  SORT lt_views BY ord.

  LOOP AT lt_views ASSIGNING <views>.
    fldcat-fieldname = <views>-object_view.
    APPEND fldcat.
    CLEAR fldcat.
  ENDLOOP.

  LOOP AT lt_views ASSIGNING <views>.
    READ TABLE <main_setup>-tabstruc WITH KEY fieldname = <views>-object_view TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.
    hide_columns-sheet = 'DATA'.
    hide_columns-index = <views>-ord + 11 ##NUMBER_OK.
    hide_columns-view  = <views>-object_view.
    APPEND hide_columns.
    CLEAR hide_columns.
  ENDLOOP.

  fieldcat[] = fldcat[].

  PERFORM load_fieldcat.

ENDFORM.

FORM pop_details_sheet .

  PERFORM build_partial_cat USING space
                                  space
                                  '/GDA/SDM_S_VAL_RESULTS_KEY'
                                  space.


*-----Main - 2nd sheet
  MOVE 'DETAILS' TO v_sheet.

*------Starting row
  MOVE '1' TO v_row.

*-----Starting column
  MOVE '0' TO v_col.

  fieldcat[] = gt_fldcat[].

  PERFORM load_fieldcat.

ENDFORM.

FORM build_partial_cat USING prog_name
                             tabname
                             struct
                             include.

  REFRESH gt_fldcat.
  CLEAR gt_fldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = prog_name
      i_internal_tabname     = tabname
      i_structure_name       = struct
      i_inclname             = include
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = gt_fldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " BUILD_PARTIAL_CAT

FORM pop_calcs_details.

* Header
  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
  APPEND gs_tab TO gt_tab.

*  IF gt_calcs1 IS INITIAL.
*    LOOP AT gt_sdm_objects ASSIGNING <objects>.
*
*      UNASSIGN <results>.
*
*      LOOP AT <objects>-sdm_instances ASSIGNING <instances>.
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
ENDFORM.

FORM pop_context_details.
  DATA:
    ro_data       TYPE REF TO data,
    ro_data_empty TYPE REF TO data.

  FIELD-SYMBOLS:
    <objects>      LIKE LINE OF gt_sdm_objects,
    <instances>    LIKE LINE OF <objects>-sdm_instances,
    <results>      TYPE STANDARD TABLE,
    <results_temp> TYPE STANDARD TABLE.

* Header
  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
  APPEND gs_tab TO gt_tab.

  LOOP AT gt_sdm_objects ASSIGNING <objects>.

    UNASSIGN <results>.

    LOOP AT <objects>-sdm_instances ASSIGNING <instances>.
      IF <instances>-object IS INITIAL.
        CONTINUE.
      ENDIF.
      IF <results> IS NOT ASSIGNED.
        ro_data_empty  = <instances>-object->return_brf_result_structure( ).
        ASSIGN ro_data_empty->* TO <results>.
        REFRESH:
         <results>.
      ENDIF.

      ro_data        = <instances>-object->return_brf_result( ).
      ASSIGN ro_data->* TO <results_temp>.

      IF <results_temp> IS ASSIGNED  AND <results_temp> IS NOT INITIAL.
        APPEND LINES OF <results_temp> TO <results>.
      ENDIF.
    ENDLOOP.

    SORT <results>.
    DELETE ADJACENT DUPLICATES FROM <results>.
  ENDLOOP.

ENDFORM.

FORM pop_main.

* Header
  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
  APPEND gs_tab TO gt_tab.

  LOOP AT <dyn_table> ASSIGNING <dyn_wa>.
    CONCATENATE '<dyn_wa>-' range_item-name INTO name.

    ASSIGN (name) TO <cell>.

    CHECK sy-subrc = 0.

    WRITE <cell> TO gs_tab LEFT-JUSTIFIED.

    APPEND gs_tab TO gt_tab.
  ENDLOOP.

ENDFORM.

FORM pop_main_details.
  DATA:
    ro_data       TYPE REF TO data,
    ro_data_empty TYPE REF TO data.

  FIELD-SYMBOLS:
    <objects>      LIKE LINE OF gt_sdm_objects,
    <instances>    LIKE LINE OF <objects>-sdm_instances,
    <results>      TYPE STANDARD TABLE,
    <results_temp> TYPE STANDARD TABLE,
    <result>       TYPE any.

* Header
  WRITE range_item-name TO gs_tab LEFT-JUSTIFIED.
  APPEND gs_tab TO gt_tab.

  LOOP AT gt_sdm_objects ASSIGNING <objects>.

* Test changes
    UNASSIGN <results>.

    LOOP AT <objects>-sdm_instances ASSIGNING <instances>.
      IF <instances>-object IS INITIAL.
        CONTINUE.
      ENDIF.
      IF <results> IS NOT ASSIGNED.
        ro_data_empty  = <instances>-object->return_brf_result_structure( ).
        ASSIGN ro_data_empty->* TO <results>.
        REFRESH:
         <results>.
      ENDIF.

      ro_data        = <instances>-object->return_brf_result( ).
      ASSIGN ro_data->* TO <results_temp>.

      IF <results_temp> IS ASSIGNED  AND <results_temp> IS NOT INITIAL.
        APPEND LINES OF <results_temp> TO <results>.
      ENDIF.
    ENDLOOP.

    SORT <results>.
    DELETE ADJACENT DUPLICATES FROM <results>.

    LOOP AT <results> ASSIGNING <result>.
      IF range_item-name = 'ID'.
        name = '<objects>-id'.
      ELSE.
        CONCATENATE '<result>-' range_item-name INTO name.
      ENDIF.

      ASSIGN (name) TO <cell>.

      CHECK sy-subrc = 0.

      WRITE <cell> TO gs_tab LEFT-JUSTIFIED.

      APPEND gs_tab TO gt_tab.
    ENDLOOP.

  ENDLOOP.
ENDFORM.
