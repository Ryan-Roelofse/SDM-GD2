*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_COMMON_CORE
*&---------------------------------------------------------------------*

FORM progress_bar USING p_progress_message TYPE any.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE       = 0
      text = p_progress_message.

ENDFORM.

FORM auth_check USING x_auth_object TYPE xuobject.

****************************************************************
*
*          Authorization Check
*
****************************************************************
  AUTHORITY-CHECK OBJECT x_auth_object
           ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE e008(/gda/sdm_core).
*       No authorization to run this program
  ENDIF.

ENDFORM.

*FORM init USING x_object_type TYPE /gda/sdm_de_object.
*  DATA:
*    lt_view_key TYPE STANDARD TABLE OF struc2,
*    ls_view_key TYPE struc2,
*    lt_sequence TYPE STANDARD TABLE OF /gda/sdm_setup5.
*
*  FIELD-SYMBOLS:
*     <objects> LIKE LINE OF gt_objects.
*
*
*  CONCATENATE icon_select_all   TEXT-005 INTO sel_all  SEPARATED BY space.
*  CONCATENATE icon_deselect_all TEXT-006 INTO dsel_all SEPARATED BY space.
*
*  SELECT * FROM /gda/sdm_setup3 INTO CORRESPONDING FIELDS OF TABLE gt_pp_main_setup
*    WHERE object_type = x_object_type
*      AND status      = abap_true
*      ORDER BY ord.
*
*  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_pp_main_gen
*    WHERE object_type  = x_object_type
*      AND outp         = abap_true.
*
** Set up Default Fields Heading
*  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_default_fields
*    WHERE object_type  = x_object_type
*      AND object_view  = gc_default
*      AND outp         = abap_true
*      AND default_all  = abap_true.
*
** Set up Default Tables used.
*
*  LOOP AT gt_default_fields ASSIGNING <general_default>.
*    gs_default_tables = <general_default>-tabname.
*    COLLECT gs_default_tables INTO gt_default_tables.
*  ENDLOOP.
*
*  SELECT * FROM /gda/sdm_setup5 INTO TABLE gt_pp_output
*    WHERE object_type  = x_object_type.
*
*  LOOP AT gt_pp_main_gen ASSIGNING <main_gen>.
*    APPEND <main_gen>-tabname TO gt_view_tables_all.
*  ENDLOOP.
*
*  SORT gt_view_tables_all.
*
*  DELETE ADJACENT DUPLICATES FROM gt_view_tables_all.
*
** Build secondary key table for Views
*  LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*    LOOP AT gt_pp_main_gen ASSIGNING <main_gen> WHERE object_view = <main_setup>-object_view
*                                                  AND default_all = abap_true.
*
*      ls_view_key-tabname = <main_gen>-tabname.
*      ls_view_key-field   = <main_gen>-field.
*      APPEND ls_view_key TO lt_view_key.
*      CLEAR:
*        ls_view_key.
*    ENDLOOP.
*    IF lt_view_key[] IS INITIAL.
** get from default
*      LOOP AT gt_pp_main_gen ASSIGNING <main_gen> WHERE object_view = gc_default
*                                                    AND default_all = abap_true.
*
*        ls_view_key-tabname = <main_gen>-tabname.
*        CONCATENATE 'KEY_' <main_gen>-field INTO ls_view_key-field.
*        APPEND ls_view_key TO lt_view_key.
*        CLEAR:
*          ls_view_key.
*      ENDLOOP.
*    ENDIF.
*
*    LOOP AT gt_pp_output ASSIGNING <main_output> WHERE object_view = <main_setup>-object_view.
*      APPEND <main_output> TO lt_sequence.
*    ENDLOOP.
*
*    <main_setup>-sequence[]  = lt_sequence[].
*    <main_setup>-view_keys[] = lt_view_key[].
*    REFRESH:
*     lt_view_key[],
*     lt_sequence[].
*  ENDLOOP.
*
FORM get_sdm_types  USING x_object_type TYPE /gda/sdm_de_object.
  DATA:
    lv_type     TYPE /gda/sdm_de_type,
    lv_continue TYPE boolean,
    lo_mapping  TYPE REF TO /gda/sdm_cl_brf_mapping.

  FIELD-SYMBOLS:
   <objects> LIKE LINE OF gt_objects.

* Set default SDM Type and include any customer specific SDM Types
  gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gc_val
                                                       x_sdm_source = gc_rep ).

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_pir.
  APPEND gs_sdm_type TO gr_sdm_type.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_pri.
  APPEND gs_sdm_type TO gr_sdm_type.

  gs_sdm_type-sign   =  'I'.
  gs_sdm_type-option =  'EQ'.
  gs_sdm_type-low    =  gc_src.
  APPEND gs_sdm_type TO gr_sdm_type.

  SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
    WHERE  sdm_object  = x_object_type
     AND   active      = abap_true
     AND   type        IN gr_sdm_type.

  gv_source = /gda/sdm_cl_core=>mc_rep.

*  LOOP AT gt_objects ASSIGNING <objects>.
*    <objects>-mapping = /gda/sdm_cl_brf_mapping=>factory( iv_object  = x_object_type
*                                                          iv_include = <objects>-include
*                                                          iv_source  = gv_source
*                                                          iv_module  = gv_module_exp ).
*    IF <objects>-mapping->mv_message <> space.
*      MESSAGE e000(/gda/sdm_pp1) WITH <objects>-mapping->mv_message.
*    ENDIF.
*  ENDLOOP.

  LOOP AT gt_objects ASSIGNING <objects>.
    <objects>-mapping = /gda/sdm_cl_brf_mapping=>factory( iv_object  = x_object_type
                                                      iv_include     = <objects>-include
                                                      iv_source      = gv_source
                                                      iv_module      = gv_module_rsr ).

* at least one object needs to be active..
    IF <objects>-mapping->mv_active = abap_true.
      lv_continue = abap_true.
    ELSE.
      lv_type = <objects>-type.
      lo_mapping = <objects>-mapping.
      DELETE gt_objects WHERE type = lv_type.
    ENDIF.
  ENDLOOP.

  IF lv_continue = abap_false.
    IF gt_objects IS INITIAL AND lo_mapping->mv_message IS NOT INITIAL.
      MESSAGE e000(/gda/sdm_pp1) WITH lo_mapping->mv_message.
    ELSE.
      READ TABLE gt_objects ASSIGNING <objects> WITH KEY type = lv_type.
      IF sy-subrc = 0.
        MESSAGE e000(/gda/sdm_pp1) WITH lo_mapping->mv_message.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFORM.
*ENDFORM.
*
*FORM screen_output.
*  DATA:
*    lv_text(80).
*
*  FIELD-SYMBOLS:
*    <parameter> TYPE any.
*
*  LOOP AT SCREEN.
*    LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*      lv_text = '%_&&_%_APP_%-TEXT'.
*      REPLACE ALL OCCURRENCES OF '&&' IN lv_text WITH <main_setup>-object_view_o.
*      IF screen-name = lv_text.
*        ASSIGN (screen-name) TO <name>.
*        <name> = <main_setup>-object_view_d.
*      ENDIF.
*    ENDLOOP.
**    ENDIF.
*  ENDLOOP.
*
*  WRITE icon_red_light     AS ICON TO icon1.
*  WRITE icon_yellow_light  AS ICON TO icon2.
*  WRITE icon_green_light   AS ICON TO icon3.
*  WRITE icon_green_light   AS ICON TO icon6.
*  WRITE icon_complete      AS ICON TO icon4.
*  WRITE icon_message_error AS ICON TO icon5.
*
*  CASE ok_code.
*    WHEN 'COMP'.
*      IF p_com = abap_true. " OR p_fai = abap_true.
*        p_amb = abap_false.
*        p_suc = abap_false.
*        p_red = abap_false.
*        p_fai = abap_false.
*        p_gre = abap_false.
*      ENDIF.
*
*      IF p_com = abap_true.
** set to true..
*        LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*          ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*          CHECK <parameter> IS ASSIGNED.
*          <parameter> = abap_true.
*        ENDLOOP.
*
*        LOOP AT SCREEN.
*          IF screen-name  = 'SEL_ALL' OR screen-name  = 'DSEL_ALL'." or  screen-name CS 'P_'.
*            screen-input = 0.
*            MODIFY SCREEN.
*          ELSE.
*            READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view_o = screen-name.
*            IF sy-subrc = 0.
*              screen-input = 0.
*              MODIFY SCREEN.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*
*    WHEN 'FAIL'.
*      IF p_fai = abap_true.
*        p_amb = abap_false.
*        p_suc = abap_false.
*        p_red = abap_false.
*        p_com = abap_false.
*        p_gre = abap_false.
*      ENDIF.
*
*    WHEN 'SUCCESS'.
*      IF p_suc = abap_true.
*        p_amb = abap_false.
*        p_red = abap_false.
*        p_gre = abap_false.
*        p_com = abap_false.
*      ENDIF.
*
*    WHEN 'IND'.
*      IF p_amb = abap_true OR p_red = abap_true OR p_gre = abap_true.
*        p_com = abap_false.
*        p_fai = abap_false.
*        p_suc = abap_false.
*      ENDIF.
*
*    WHEN OTHERS.
*  ENDCASE.
*ENDFORM.
*
*FORM at_selection_screen .
*  FIELD-SYMBOLS:
*    <parameter>.
***// Select/Deselect All
*  CASE sy-ucomm.
*    WHEN 'SEL'.
*      LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*        CHECK <parameter> IS ASSIGNED.
*        <parameter> = abap_true.
*      ENDLOOP.
*
*    WHEN 'DSEL'.
*      LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*        CHECK <parameter> IS ASSIGNED.
*        <parameter> = abap_false.
*      ENDLOOP.
*  ENDCASE.
*
*  ok_code = sy-ucomm.
*  IF sy-ucomm = space.
*    gs_sscrfields-ucomm = 'ONLI'.
*  ELSE.
*    gs_sscrfields-ucomm = sy-ucomm.
*  ENDIF.
*
*ENDFORM.

FORM build_structure USING x_view        TYPE lvc_s_col
                           x_object_type TYPE /gda/sdm_de_object
                           x_struc       TYPE char1.
  DATA:
    lo_ref_table_des TYPE REF TO cl_abap_structdescr,
    lt_view_fields   TYPE STANDARD TABLE OF /gda/sdm_setup4,
    lt_details       TYPE abap_compdescr_tab,
    lv_lines         TYPE i.

  FIELD-SYMBOLS:
    <general>        LIKE LINE OF lt_view_fields,
    <default_fields> LIKE LINE OF lt_view_fields,
    <details>        LIKE LINE OF lt_details,
    <view_struc>     LIKE LINE OF gt_view_struc,
    <parameter>      TYPE any.

  REFRESH:
   gt_view_tables.

* Retrieve fields for specific view
  SELECT * FROM /gda/sdm_setup4 INTO TABLE lt_view_fields
    WHERE object_type  = x_object_type
      AND object_view  = x_view
      AND outp         = abap_true.

* Ensure that default field are displayed in all views.
  LOOP AT gt_default_fields ASSIGNING <default_fields>.
    READ TABLE lt_view_fields ASSIGNING <general>
    WITH KEY field   = <default_fields>-field
             tabname = <default_fields>-tabname.
    IF sy-subrc <> 0.
      CONCATENATE 'KEY_' <default_fields>-field INTO <default_fields>-field.
      APPEND <default_fields> TO lt_view_fields.
    ELSE.
      IF x_view <> gc_default.
        CONCATENATE 'KEY_' <default_fields>-field INTO <default_fields>-field.
        <default_fields>-object_view = x_view.
        APPEND <default_fields> TO lt_view_fields.
      ELSE.
        CONCATENATE 'KEY_' <general>-field  INTO <general>-field.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT lt_view_fields BY default_all DESCENDING ord.
* Build a table of related tables for the view
  LOOP AT lt_view_fields ASSIGNING <general>.
    gs_table = <general>-tabname.
    APPEND gs_table TO gt_view_tables.
  ENDLOOP.

  SORT gt_view_tables.
  DELETE ADJACENT DUPLICATES FROM gt_view_tables.

  LOOP AT gt_view_tables INTO gs_table.
    lo_ref_table_des ?=
        cl_abap_typedescr=>describe_by_name( gs_table ).

    lt_details[] = lo_ref_table_des->components[].

    LOOP AT lt_view_fields ASSIGNING <general> WHERE tabname = gs_table.
      IF <general>-field CS 'KEY_'.
        ls_field = <general>-field+4(6).
        READ TABLE lt_details ASSIGNING <details> WITH KEY name = ls_field.
      ELSE.
        READ TABLE lt_details ASSIGNING <details> WITH KEY name = <general>-field.
      ENDIF.
      CHECK sy-subrc = 0.
      CLEAR: gs_view_struc.
      gs_view_struc-fieldname = <general>-field.
      gs_view_struc-tabname   = <general>-tabname.
      gs_view_struc-datatype  = <details>-type_kind.
      CASE <details>-type_kind.
        WHEN 'C'.
          gs_view_struc-datatype = 'CHAR'.
        WHEN 'N'.
          gs_view_struc-datatype = 'NUMC'.
        WHEN 'D'.
          gs_view_struc-datatype = 'DATE'.
        WHEN 'P'.
          gs_view_struc-datatype = 'PACK'.
        WHEN OTHERS.
          gs_view_struc-datatype = <details>-type_kind.
      ENDCASE.

      IF <general>-field CS 'KEY_'.
        gs_view_struc-key      = abap_true.
      ENDIF.

      gs_view_struc-inttype  = <details>-type_kind.
      gs_view_struc-intlen   = <details>-length.
      gs_view_struc-decimals = <details>-decimals.
      gs_view_struc-col_pos  = <general>-ord.

      SELECT SINGLE scrtext_s scrtext_m scrtext_l FROM dd03m
             INTO (gs_view_struc-scrtext_s, gs_view_struc-scrtext_m, gs_view_struc-scrtext_l)
            WHERE tabname    = <general>-tabname
              AND fieldname  = <general>-field
              AND ddlanguage = sy-langu.

      IF sy-subrc <> 0.
        SELECT SINGLE scrtext_s scrtext_m scrtext_l FROM dd03m
               INTO (gs_view_struc-scrtext_s, gs_view_struc-scrtext_m, gs_view_struc-scrtext_l)
              WHERE tabname    = <general>-tabname
                AND fieldname  = <general>-field+4
                AND ddlanguage = sy-langu .

      ENDIF.

      APPEND gs_view_struc TO gt_view_struc.
    ENDLOOP.
  ENDLOOP.

  CLEAR:
    gs_view_struc,
    lv_lines.

  SORT gt_view_struc BY key DESCENDING col_pos.

  LOOP AT gt_view_struc ASSIGNING <view_struc>.
    lv_lines = lv_lines + 1.
    <view_struc>-col_pos =  lv_lines.
  ENDLOOP.

  DESCRIBE TABLE gt_view_struc LINES lv_lines.

  IF x_view  = gc_default.
* Add an additional hidden linkage field to the end for DEFAULT View. But only if sructured oobject is selected
    lv_lines = lv_lines + 1.
    gs_view_struc-fieldname  = 'LINKAGE'.
    IF x_struc = abap_false.
      gs_view_struc-no_out = abap_true.
    ENDIF.
    gs_view_struc-datatype   = 'CHAR'.
    gs_view_struc-inttype    = 'C'.
    gs_view_struc-intlen     = 000018.
    gs_view_struc-outputlen  = 000018.
    gs_view_struc-col_pos    = 1 .
    gs_view_struc-scrtext_l  = TEXT-012." 'Structured Material'.
    gs_view_struc-scrtext_m  = TEXT-012." 'Structured Material'.
    gs_view_struc-scrtext_s  = TEXT-012." 'Structured Material'.
    APPEND gs_view_struc TO gt_view_struc.
    CLEAR:
     gs_view_struc.

    LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
      IF <main_setup>-object_view NE gc_default.
        lv_lines = lv_lines + 1.
        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
        IF <parameter> = abap_true.
          gs_view_struc-fieldname  = <main_setup>-object_view.
          gs_view_struc-datatype   = 'CHAR'.
          gs_view_struc-inttype    = 'C'.
          gs_view_struc-intlen     = 000004.
          gs_view_struc-outputlen  = 000004.
          gs_view_struc-icon       = abap_true.
          gs_view_struc-col_pos    = lv_lines.
          gs_view_struc-hotspot    =  abap_true.
          APPEND gs_view_struc TO gt_view_struc.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
* BRF Plus Fields..
    lv_lines = lv_lines + 1.
    gs_view_struc-fieldname  = 'MESSAGE'.
    gs_view_struc-datatype   = 'CHAR'.
    gs_view_struc-inttype    = 'C'.
    gs_view_struc-intlen     = 000220.
    gs_view_struc-outputlen  = 000100.
    gs_view_struc-col_pos    = lv_lines.
    APPEND gs_view_struc TO gt_view_struc.
  ENDIF.
ENDFORM.

FORM build_dynamic_itab USING x_view TYPE lvc_s_col
                        CHANGING xo_data TYPE REF TO data.



  DATA:
    ro_table TYPE REF TO data,
    ro_line  TYPE REF TO data.

  FIELD-SYMBOLS:
     <main_setup> LIKE LINE OF gt_pp_main_setup.

  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = x_view .
  IF sy-subrc <> 0 OR <main_setup>-ro_table IS INITIAL.
* Create dynamic internal table and assign to FS
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog  = gt_view_struc
        i_length_in_byte = 'X'
      IMPORTING
        ep_table         = ro_table.

    IF x_view = gc_default.
      ASSIGN ro_table->* TO <dyn_table>.
*      ASSIGN ro_table->* TO <dyn_table2>.

*      ASSIGN ro_table->* TO <dyn_table_final>.
* Create dynamic work area and assign to FS
      CREATE DATA ro_line LIKE LINE OF <dyn_table>.
*       CREATE DATA ro_line LIKE LINE OF <dyn_table_final>.
      ASSIGN ro_line->* TO <dyn_wa>.
    ELSE.
      ASSIGN ro_table->* TO <dyn_table_view>.
* Create dynamic work area and assign to FS
      CREATE DATA ro_line LIKE LINE OF <dyn_table_view>.
      ASSIGN ro_line->* TO <dyn_wa_view>.
    ENDIF.

    <main_setup>-tabname    = x_view.
    <main_setup>-ro_table   = ro_table.
    <main_setup>-tabstruc[] = gt_view_struc[].
  ELSE.
    IF x_view = gc_default.
      ASSIGN <main_setup>-ro_table->* TO <dyn_table>.

*      ASSIGN <main_setup>-ro_table->* TO <dyn_table_final>.
* Create dynamic work area and assign to FS
      CREATE DATA ro_line LIKE LINE OF <dyn_table>.
      ASSIGN ro_line->* TO <dyn_wa>.
*      ASSIGN ro_line->* TO <dyn_wa2>.
    ELSE.
      ASSIGN <main_setup>-ro_table->* TO <dyn_table_view>.
* Create dynamic work area and assign to FS
      CREATE DATA ro_line LIKE LINE OF <dyn_table_view>.
      ASSIGN ro_line->* TO <dyn_wa_view>.
    ENDIF.
  ENDIF.

  xo_data = ro_table.

  REFRESH:
    gt_view_struc[].
ENDFORM.                    " BUILD_DYNAMIC_ITAB


FORM limit_max_entries CHANGING cv_max_no TYPE p_dbacc
                                cv_execute_report TYPE abap_bool.
  DATA: lv_question    TYPE c LENGTH 200,
        lv_answer      TYPE c LENGTH 1,
        lv_max_entries TYPE string.

*  cv_max_no = 0.
  cv_execute_report = abap_true.

  IF cv_max_no = '000000'.
    lv_max_entries = gc_maxdb_entries.
  ELSE.
    lv_max_entries = cv_max_no.
  ENDIF.

  SHIFT lv_max_entries LEFT DELETING LEADING '0'.

  lv_question = |No data entered into Selection-screen. The number of database entries will be restricted to |
                && lv_max_entries && |. Continue?|.

  IF sy-batch = space AND ( gs_sscrfields-ucomm = 'ONLI' OR gs_sscrfields-ucomm = 'PRIN' ) AND gv_selection_fields_entered = abap_false.
    PERFORM popup_to_confirm USING lv_question CHANGING lv_answer.
    IF lv_answer = '1'. "Yes
      cv_max_no = lv_max_entries. "gc_maxdb_entries.
    ELSE.
      cv_execute_report = abap_false.
    ENDIF.
  ENDIF.

ENDFORM.

FORM popup_to_confirm USING iv_question TYPE c
                      CHANGING cv_answer TYPE flag.

  DATA:
    lv_answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = iv_question
    IMPORTING
      answer         = lv_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  cv_answer = lv_answer.

ENDFORM.

*FORM message_filter USING    p_result
*                    CHANGING p_exit   TYPE char1.
*  FIELD-SYMBOLS:
*    <number> TYPE any,
*    <id>     TYPE any.
*
*  IF s_arbgb IS NOT INITIAL.
*    ASSIGN COMPONENT 'ID'     OF STRUCTURE p_result TO <id>.
*  ENDIF.
*  IF s_msgnr IS NOT INITIAL.
*    ASSIGN COMPONENT 'NUMBER' OF STRUCTURE p_result TO <number>.
*  ENDIF.
*
*  IF <id> IS ASSIGNED AND <number> IS NOT ASSIGNED.
*    IF <id> IN s_arbgb.
*      p_exit =  abap_false.
*    ELSE.
*      p_exit =  abap_true.
*    ENDIF.
*  ELSEIF <number> IS ASSIGNED AND <id> IS NOT ASSIGNED.
*    IF <number> IN s_msgnr.
*      p_exit =  abap_false.
*    ELSE.
*      p_exit =  abap_true.
*    ENDIF.
*  ELSEIF <number> IS ASSIGNED AND <id> IS ASSIGNED.
*    IF <number> IN s_msgnr AND <id> IN s_arbgb.
*      p_exit =  abap_false.
*    ELSE.
*      p_exit =  abap_true.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*
*FORM message_context_link USING x_result   TYPE any
*                                x_obj_type TYPE /gda/sdm_de_object.
*  DATA:
*    lt_general TYPE STANDARD TABLE OF /gda/sdm_setup4,
*    lv_field   TYPE field,
*    lv_table   TYPE tabname16.
*
*  FIELD-SYMBOLS:
*    <field>   TYPE any,
*    <type>    TYPE any,
*    <general> LIKE LINE OF lt_general,
*    <icon>    TYPE any.
*
*
** EXTRA_V1 contains table-field which is needs for mapping contect to messages
*  ASSIGN COMPONENT 'TYPE'     OF STRUCTURE x_result TO <type>.
*  ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE x_result TO <field>.
*
*  IF <field> IS ASSIGNED.
*
*    SPLIT <field> AT '-' INTO lv_table lv_field.
*
*    SELECT object_view FROM /gda/sdm_setup4
*      INTO CORRESPONDING FIELDS OF TABLE lt_general
*      WHERE  object_type = x_obj_type
*        AND  field       = lv_field
*        AND  tabname     = lv_table
*         AND outp        = abap_true.
*  ENDIF.
*
*  LOOP AT lt_general ASSIGNING <general>.
*    ASSIGN COMPONENT <general>-object_view OF STRUCTURE <dyn_wa> TO <icon>.
*    IF sy-subrc = 0.
*      CASE <type>.
*        WHEN 'E'.
*          <icon> = icon_red_light.
*        WHEN 'W'.
*          IF <icon> NE icon_red_light.
*            <icon> = icon_yellow_light.
*          ENDIF.
*        WHEN OTHERS.
*          IF <icon> NE icon_red_light.
*            <icon> = icon_green_light.
*          ENDIF.
*      ENDCASE.
*
*    ENDIF.
*  ENDLOOP.
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_SDM_MATERIAL  text
*----------------------------------------------------------------------*
*FORM determine_output  USING    x_sdm_object
*                       CHANGING  xt_sdm_object TYPE STANDARD TABLE.
*
*  DATA:
*    lv_append,
*    lv_append2.
*
*  FIELD-SYMBOLS:
*   <icon> TYPE any.
*
**  lv_append = abap_true.
*
** All Records
** determine if this record should be output
*  IF p_red = abap_true AND p_amb = abap_true AND p_gre = abap_true.
*    APPEND x_sdm_object TO xt_sdm_object.
*    APPEND <dyn_wa> TO <dyn_table>.
*  ENDIF.
*
** Errors Only - must contain at least one red icon
*  IF p_red = abap_true AND p_amb = abap_false AND p_gre = abap_false.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_red_light.
*        lv_append = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    IF lv_append = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*
*  ENDIF.
*
** Warning Only - must contain at least one amber icon
*  IF p_red = abap_false AND p_amb = abap_true AND p_gre = abap_false.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light.
*        lv_append = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*    IF lv_append = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
** Success Only - must contain at least one amber icon
*  IF p_red = abap_false AND p_amb = abap_false AND p_gre = abap_true.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_green_light.
*        lv_append = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*    IF lv_append = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
** Success Only - must contain at least one green icon
*  IF p_red = abap_false AND p_amb = abap_false AND p_gre = abap_true.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light OR <icon> = icon_red_light.
*        lv_append = abap_false.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_green_light.
*        lv_append = abap_true.
*      ENDIF.
*
*    ENDDO.
*    IF lv_append = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
*
** Success Only and Warning - must contain at least one green icon and ome yellow icon
*  IF p_red = abap_false AND p_amb = abap_true AND p_gre = abap_true.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_green_light.
*        lv_append = abap_false.
*        EXIT.
*      ENDIF.
*    ENDDO.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light.
*        lv_append2 = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    IF lv_append = abap_true AND lv_append2 = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
** Error and Success - must contain at least one green icon and ome yesllow icon
*  IF p_red = abap_true AND p_amb = abap_false AND p_gre = abap_true.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT. " no more components
*      ENDIF.
*
*      IF <icon> = icon_green_light.
*        lv_append = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_red_light.
*        lv_append2 = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    IF lv_append = abap_true AND lv_append2 = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
** Error and Warning - must contain at least one green icon and ome yesllow icon
*  IF p_red = abap_true AND p_amb = abap_true AND p_gre = abap_false.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_red_light. " OR <icon> = icon_yellow_light.
*        lv_append = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light.
*        lv_append2 = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    IF lv_append = abap_true AND lv_append2 = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
** Warning and Success - must contain at least one green icon and ome yesllow icon
*  IF p_red = abap_false AND p_amb = abap_true AND p_gre = abap_true.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_green_light. " OR <icon> = icon_yellow_light.
*        lv_append = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light.
*        lv_append2 = abap_true.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    IF lv_append = abap_true AND lv_append2 = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
** Success All -
** Only Green Items will appear on the line
*  IF p_com = abap_true OR p_suc = abap_true.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light OR <icon> = icon_red_light.
*        lv_append = abap_false.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_green_light.
*        lv_append = abap_true.
*      ENDIF.
*
*    ENDDO.
*    IF lv_append = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
** Errors All -
** Only Red Icons will appear on the entire line
*  IF p_fai = abap_true.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light OR <icon> = icon_green_light.
*        lv_append = abap_false.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_red_light.
*        lv_append = abap_true.
*      ENDIF.
*
*    ENDDO.
*    IF lv_append = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
*
** Success All and Errors All -
** Only Green Items should be appera on teh line
*  IF p_com = abap_true AND p_fai = abap_true.
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light OR <icon> = icon_red_light.
*        lv_append = abap_false.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_green_light.
*        lv_append = abap_true.
*      ENDIF.
*
*    ENDDO.
*    IF lv_append = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*
** Errors All
*    DO.
*      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_yellow_light OR <icon> = icon_green_light.
*        lv_append = abap_false.
*        EXIT.
*      ENDIF.
*
*      IF <icon> = icon_red_light.
*        lv_append = abap_true.
*      ENDIF.
*
*    ENDDO.
*    IF lv_append = abap_true.
*      APPEND x_sdm_object TO xt_sdm_object.
*      APPEND <dyn_wa> TO <dyn_table>.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*
*FORM set_display_top.
*  DATA:
**   lo_sort  TYPE REF TO cl_salv_sorts,
*    ls_txt_l  TYPE scrtext_l,
*    ls_txt_m  TYPE scrtext_m,
*    ls_txt_s  TYPE scrtext_s,
**    ls_col    TYPE lvc_fname,
*    ls_layout TYPE lvc_s_layo.
*
*
*  FIELD-SYMBOLS:
*    <parameter>  TYPE any,
*    <view_setup> LIKE LINE OF gt_pp_main_setup.
*
** Create Instance
*  TRY.
*      CREATE OBJECT go_alv_top
*        EXPORTING
*          i_parent = go_parent1.
*
*    CATCH cx_salv_msg INTO gx_root.
*      gv_message = gx_root->get_text( ).
*  ENDTRY.
*
*  IF gv_message IS NOT INITIAL.
*    MESSAGE e000 WITH gv_message.
*  ENDIF.
*
***  GO_LAYOUT->SET_KEY( GV_KEY ).
**  go_layout_top->set_save_restriction( if_salv_c_layout=>restrict_none ).
**  go_layout_top->set_default( if_salv_c_bool_sap=>true ).
**
*** Get Functions
**  go_func_top = go_table_top->get_functions( ).
**  go_func_top->set_all( ).
**  go_columns_top = go_table_top->get_columns( ).
**  go_columns_top->set_optimize( 'X' ).
*
*  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = 'DEFAULT'.
*
*  IF <main_setup> IS ASSIGNED.
*    LOOP AT <main_setup>-tabstruc ASSIGNING <tabstruc>.
*      IF <tabstruc>-fieldname = 'MESSAGE'.
*        CONTINUE.
*      ENDIF.
**      TRY.
**          go_column_top ?= go_columns_top->get_column( <tabstruc>-fieldname ).
**        CATCH cx_salv_not_found.
**      ENDTRY.
**
*      IF <tabstruc>-key = abap_true.
**        TRY.
**            go_column_top->set_key( value  = if_salv_c_bool_sap=>true ).
**          CATCH cx_salv_data_error .
**        ENDTRY.
*      ENDIF.
**
**      TRY.
**          go_column_top->set_alignment( value  = if_salv_c_alignment=>left ).
**        CATCH cx_salv_data_error .
**      ENDTRY.
**
**      go_column_top->set_long_text( <tabstruc>-scrtext_l ).
**      go_column_top->set_medium_text( <tabstruc>-scrtext_m ).
**      go_column_top->set_short_text( <tabstruc>-scrtext_s ).
**      go_column_top->set_icon( abap_true ).
**      go_column_top->set_visible( abap_true ).
**
*      READ TABLE gt_pp_main_setup ASSIGNING <view_setup> WITH KEY object_view = <tabstruc>-fieldname.
*      IF sy-subrc = 0.
*        ASSIGN (<view_setup>-object_view_o) TO <parameter>.
*        IF <parameter> = abap_true.
*          ls_txt_l  = <view_setup>-object_view_d.
*          ls_txt_m  = <view_setup>-object_view_d.
*          ls_txt_s  = <view_setup>-object_view_d.
*
*          <tabstruc>-scrtext_l = ls_txt_l.
*          <tabstruc>-scrtext_m = ls_txt_m.
*          <tabstruc>-scrtext_s = ls_txt_s.
*        ENDIF.
*      ENDIF.
*      IF <tabstruc>-fieldname CS 'MATNR'.
*        <tabstruc>-hotspot = abap_true.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  ls_layout-cwidth_opt = abap_true.
*
**  DATA: ro_table TYPE REF TO data.
**  DATA: ro_sdm_objects TYPE REF TO data.
**
***gt_sdm_material
**  FIELD-SYMBOLS:
**    <sdm_data> TYPE any,
**    <table>    TYPE any.
*
**  CREATE DATA ro_sdm_objects TYPE STANDARD TABLE OF sdm_objects.
**  ASSIGN ro_sdm_objects->* TO <sdm_data>. "gt_sdm_material[].
**  <sdm_data>  = gt_sdm_material[].
**
**    PERFORM build_dynamic_itab USING gc_default
**                               changing ro_table.
***  CREATE DATA ro_table TYPE STANDARD TABLE OF <dyn_table>.
**  ASSIGN ro_table->* TO <table>. "gt_sdm_material[].
**  <table>  =  <dyn_table>.
*
*
*  CREATE OBJECT go_handler_1
*    EXPORTING
*      xt_main_setup  = gt_pp_main_setup[]
*      xt_dyn_table   = <dyn_table>
*      xt_sdm_objects = <dyn_sdm_res>
*      xo_parent      = go_parent2.
*
*  CREATE OBJECT go_handler_local.
*
*  SET HANDLER go_handler_1->on_hotspot_click    FOR go_alv_top.
*  SET HANDLER go_handler_1->handle_context_menu FOR go_alv_top.
*  SET HANDLER go_handler_1->toolbar             FOR go_alv_top.
*
*  SET HANDLER go_handler_local->handle_user_command FOR go_alv_top.
*
** Display Table
*  CALL METHOD go_alv_top->set_table_for_first_display
*    EXPORTING
*      is_layout       = ls_layout
*    CHANGING
*      it_fieldcatalog = <main_setup>-tabstruc[]
*      it_outtab       = <dyn_table>[]. "<dyn_table_final>[].
*
*
*ENDFORM.
*
*FORM display_results USING xt_sdm_results TYPE table
*                           x_sdm_object   TYPE /gda/sdm_de_object.
*
*  CASE x_sdm_object.
*    WHEN 'ARTICLE'.
*      CREATE DATA go_sdm_results TYPE STANDARD TABLE OF /gda/sdm_s_article.
*    WHEN 'MATERIAL'..
*      CREATE DATA go_sdm_results TYPE STANDARD TABLE OF /gda/sdm_s_material.
*    WHEN 'BOM'.
*      CREATE DATA go_sdm_results TYPE STANDARD TABLE OF /gda/sdm_s_bom.
*    WHEN OTHERS.
*  ENDCASE.
*  ASSIGN go_sdm_results->* TO <dyn_sdm_res>. "gt_sdm_material[].
*  <dyn_sdm_res>  = xt_sdm_results[].
*  SORT <dyn_sdm_res>.
*  CALL SCREEN 0100.
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  DEFAULT_VIEW_ICONS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM default_view_icons.
*  FIELD-SYMBOLS:
*    <view>.
*
*  CLEAR <dyn_wa>.
*  LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*    ASSIGN COMPONENT <main_setup>-object_view OF STRUCTURE  <dyn_wa> TO <view>.
*    CHECK sy-subrc = 0.
*    IF <view> IS ASSIGNED.
*      <view> = icon_green_light.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_selection USING    iv_struc_name TYPE string
                                    iv_pref       TYPE string
                           CHANGING xt_field_list TYPE ANY TABLE.
  DATA:
    lo_type_descr   TYPE REF TO cl_abap_typedescr,
    lo_struct_descr TYPE REF TO cl_abap_structdescr,
    lt_field_list   TYPE TABLE OF char30,
    lv_last         TYPE i.

  FIELD-SYMBOLS:
    <ls_components> TYPE LINE OF abap_compdescr_tab,
    <ls_field_list> TYPE char30.


  cl_abap_typedescr=>describe_by_name(
  EXPORTING
    p_name         =  iv_struc_name
    RECEIVING
    p_descr_ref    =  lo_type_descr
  EXCEPTIONS
    type_not_found = 1
    OTHERS         = 2
    ).
  IF sy-subrc = 0.
    lo_struct_descr ?= lo_type_descr.
  ELSE.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            INTO me->mv_message
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

**// Build Dynamic Field Selection
  DESCRIBE TABLE lo_struct_descr->components LINES lv_last.
  LOOP AT lo_struct_descr->components ASSIGNING <ls_components>.
    APPEND INITIAL LINE TO lt_field_list ASSIGNING <ls_field_list>.
    IF sy-tabix = lv_last.
      CONCATENATE iv_pref <ls_components>-name INTO <ls_field_list>.
    ELSE.
      CONCATENATE iv_pref <ls_components>-name ',' INTO <ls_field_list>.
    ENDIF.
  ENDLOOP.

  xt_field_list[] =   lt_field_list[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_STRING_FROM_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3725   text
*      -->P_GS_MARA_SDM  text
*----------------------------------------------------------------------*
FORM build_string_from_key  USING p_tabname  TYPE tabname
                                  p_contents TYPE any
                            CHANGING c_tabkey TYPE cdtabkey.
* get key fields for table

  TYPES: BEGIN OF s_tab_fields,
           field TYPE fieldname,
         END OF s_tab_fields.

  DATA:
    go_strucdescr TYPE REF TO cl_abap_structdescr,
    gt_tab_fields TYPE ddfields.
*        lt_fields     TYPE STANDARD TABLE OF s_tab_fields,
*        ls_fields     TYPE s_tab_fields.

  FIELD-SYMBOLS:
    <gwa_tab_field> TYPE dfies,
    <contents>      TYPE any,
    <key_field>     TYPE any.

  REFRESH:
   gt_tab_fields.

  ASSIGN p_contents TO <contents>.

  TRY .
*   Get the details of the DDIC table
      go_strucdescr ?= cl_abap_elemdescr=>describe_by_name( p_tabname ).
    CATCH cx_sy_move_cast_error .
      MESSAGE 'Error while casting'  TYPE 'S' .
      RETURN.
  ENDTRY.

* Check if input is a DDIC table
  IF go_strucdescr->is_ddic_type( ) = abap_false.
    RETURN.
  ENDIF.

* Get the details of the table fields
  gt_tab_fields = go_strucdescr->get_ddic_field_list( ).

* Display the Key fields of the table
  LOOP AT gt_tab_fields ASSIGNING <gwa_tab_field> WHERE keyflag = 'X'.
    ASSIGN COMPONENT <gwa_tab_field>-fieldname OF STRUCTURE <contents> TO <key_field>.
    CHECK sy-subrc = 0.
    CONCATENATE c_tabkey <key_field> INTO c_tabkey.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULTS_BASIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SDM_MATERIAL  text
*      -->P_GC_MATERIAL  text
*----------------------------------------------------------------------*

FORM display_results_basic USING xt_sdm_results TYPE table.
*                           x_sdm_object   TYPE /gda/sdm_de_object.

  DATA:
    ro_salv       TYPE REF TO cl_salv_table,
    ro_salv_msg   TYPE REF TO cx_salv_msg,
    ro_data_empty TYPE REF TO data,
    lv_msg        TYPE string,
    functions     TYPE REF TO cl_salv_functions_list,
    columns       TYPE REF TO cl_salv_columns.

  FIELD-SYMBOLS:
    <sdm_objects>      TYPE any,
    <sdm_instances>    TYPE /gda/sdm_t_instances,
    <sdm_instance>     TYPE /gda/sdm_s_instances,
    <results_temp>     TYPE STANDARD TABLE,
    <results_collated> TYPE STANDARD TABLE.

  LOOP AT xt_sdm_results ASSIGNING <sdm_objects>.
    ASSIGN COMPONENT 'SDM_INSTANCES' OF STRUCTURE <sdm_objects> TO <sdm_instances>.
    LOOP AT <sdm_instances> ASSIGNING <sdm_instance>.
      IF <sdm_instance>-object IS INITIAL.
        CONTINUE.
      ENDIF.
      IF <results_collated> IS NOT ASSIGNED.
        ro_data_empty  = <sdm_instance>-object->return_brf_result_structure( ).
        ASSIGN ro_data_empty->* TO <results_collated>.
        REFRESH:
         <results_collated>.
      ENDIF.

      ro_data        = <sdm_instance>-object->return_brf_result( ).
      ASSIGN ro_data->* TO <results_temp>.

      IF <results_temp> IS ASSIGNED.
        APPEND LINES OF <results_temp> TO <results_collated>.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  TRY.
      cl_salv_table=>factory(
         IMPORTING
           r_salv_table = ro_salv
         CHANGING
           t_table      = <results_collated> ).
    CATCH cx_salv_msg INTO ro_salv_msg.
      lv_msg = ro_salv_msg->get_text( ).
      MESSAGE lv_msg TYPE 'E'.
  ENDTRY.

  columns = ro_salv->get_columns( ).
  columns->set_optimize( abap_true ).

  functions = ro_salv->get_functions( ).
  functions->set_all( ).

  ro_salv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTION_ENTRIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SELECTION_FIELDS_ENTERED  text
*----------------------------------------------------------------------*
FORM check_selection_entries CHANGING cv_selection_fields_entered TYPE abap_bool.

  IF gs_selscreen IS INITIAL.
    cv_selection_fields_entered = abap_false.
  ELSE.
    cv_selection_fields_entered = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_results_excep_report.
  DATA:
    lo_functions TYPE REF TO cl_salv_functions_list,
    lo_salv      TYPE REF TO cl_salv_table,
    lo_salv_msg  TYPE REF TO cx_salv_msg,
    lv_msg       TYPE string.

  CHECK gv_execute_report = abap_true.
  IF <results>[] IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory(
           IMPORTING
             r_salv_table = lo_salv
           CHANGING
             t_table      = <results> ).
      CATCH cx_salv_msg INTO lo_salv_msg.
        lv_msg = lo_salv_msg->get_text( ).
        MESSAGE lv_msg TYPE 'E'.
    ENDTRY.

    lo_functions = lo_salv->get_functions( ).
    lo_functions->set_all( ).

    lo_salv->display( ).

  ELSE.
    IF gv_config_err = abap_true.
*   SDM Setup Error
      MESSAGE e005(/gda/sdm_pp1).
    ELSE.
*   No data for specified selections
      MESSAGE e006(/gda/sdm_pp1).
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN_EXCEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM at_sel_scrn_excep.
  ok_code = sy-ucomm.
  IF sy-ucomm = space.
    gs_sscrfields-ucomm = 'ONLI'.
  ELSE.
    gs_sscrfields-ucomm = sy-ucomm.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCRN__OUTPUT_EXCEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM at_sel_scrn__output_excep .
  LOOP AT SCREEN.
    IF screen-name = 'P_STAT1' OR screen-name = 'P_STAT2'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SDM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_TYPE  text
*      -->P_GV_SOURCE  text
*      -->P_GV_OBJECT  text
*----------------------------------------------------------------------*
FORM sdm_init_common    USING x_object_type TYPE /gda/sdm_de_object
              CHANGING c_object_type TYPE /gda/sdm_de_object
                       c_type        TYPE /gda/sdm_de_type
                       c_source      TYPE /gda/sdm_de_source
                       co_selection  TYPE any.

  c_type        = /gda/sdm_cl_core=>mc_validation.
  c_source      = /gda/sdm_cl_core=>mc_rep.
  c_object_type = x_object_type.

  IF co_selection IS INITIAL.
    co_selection ?= /gda/sdm_cl_selections=>factory( iv_object_type = c_object_type ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SDM_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sdm_selection .
  TRY.
      go_selection->main( ).
    CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
      gv_message = gx_sdm_root->mv_text.
      IF sy-batch = abap_true.
        WRITE: / gv_message.
      ELSE.
        MESSAGE gv_message TYPE 'I'.
      ENDIF.
      RETURN.
  ENDTRY.
ENDFORM.
