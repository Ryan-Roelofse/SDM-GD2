*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_COMMON_CORE
*&---------------------------------------------------------------------*

*FORM progress_bar USING p_progress_message TYPE any.
*
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
**     PERCENTAGE       = 0
*      text = p_progress_message.
*
*ENDFORM.

FORM init USING x_object_type TYPE /gda/sdm_de_object.

  DATA:
    lt_view_key TYPE STANDARD TABLE OF struc2,
    ls_view_key TYPE struc2,
    lt_sequence TYPE STANDARD TABLE OF /gda/sdm_setup5,
    lv_type     TYPE /gda/sdm_de_type,
    lv_continue TYPE boolean,
    lo_mapping  TYPE REF TO /gda/sdm_cl_brf_mapping.

  FIELD-SYMBOLS:
     <objects> LIKE LINE OF gt_objects.


  CONCATENATE icon_select_all   TEXT-005 INTO sel_all  SEPARATED BY space.
  CONCATENATE icon_deselect_all TEXT-006 INTO dsel_all SEPARATED BY space.

  SELECT object_type object_view object_view_d object_view_o status tree
    FROM /gda/sdm_setup3 INTO CORRESPONDING FIELDS OF TABLE gt_pp_main_setup
    BYPASSING BUFFER
    WHERE object_type = x_object_type
      AND status      = abap_true
      ORDER BY ord.

  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_pp_main_gen
    WHERE object_type  = x_object_type
      AND outp         = abap_true.

* Set up Default Fields Heading
  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_default_fields
    WHERE object_type  = x_object_type
      AND object_view  = gc_default
      AND outp         = abap_true
      AND default_all  = abap_true.

* Set up default message classes
  SELECT * FROM /gda/sdm_setup8 INTO TABLE gt_default_msg_class
    WHERE sdm_object  = x_object_type.

  LOOP AT gt_default_msg_class ASSIGNING <default_msg_class>.
    s_arbgb-sign   = 'I'.
    s_arbgb-option = 'EQ'.
    s_arbgb-low    = <default_msg_class>-arbgb.
    APPEND s_arbgb.
  ENDLOOP.

  IF s_arbgb IS INITIAL.
    s_arbgb-sign   = 'I'.
    s_arbgb-option = 'CP'.
    s_arbgb-low    = '/GDA/*'.
    APPEND s_arbgb.

    s_arbgb-sign   = 'I'.
    s_arbgb-option = 'CP'.
    s_arbgb-low    = 'Z*'.
    APPEND s_arbgb.
  ENDIF.

* Set up Default Tables used.
  LOOP AT gt_default_fields ASSIGNING <general_default>.
    gs_default_tables = <general_default>-tabname.
    COLLECT gs_default_tables INTO gt_default_tables.
  ENDLOOP.

  SELECT * FROM /gda/sdm_setup5 INTO TABLE gt_pp_output
    WHERE object_type  = x_object_type.

  LOOP AT gt_pp_main_gen ASSIGNING <main_gen>.
    APPEND <main_gen>-tabname TO gt_view_tables_all.
  ENDLOOP.

  SORT gt_view_tables_all.

  DELETE ADJACENT DUPLICATES FROM gt_view_tables_all.

* Build secondary key table for Views
  LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
    LOOP AT gt_pp_main_gen ASSIGNING <main_gen> WHERE object_view = <main_setup>-object_view
                                                  AND default_all = abap_true.

      ls_view_key-tabname = <main_gen>-tabname.
      ls_view_key-field   = <main_gen>-field.
      APPEND ls_view_key TO lt_view_key.
      CLEAR:
        ls_view_key.
    ENDLOOP.
    IF lt_view_key[] IS INITIAL.
* get from default
      LOOP AT gt_pp_main_gen ASSIGNING <main_gen> WHERE object_view = gc_default
                                                    AND default_all = abap_true.

        ls_view_key-tabname = <main_gen>-tabname.
        CONCATENATE 'KEY_' <main_gen>-field INTO ls_view_key-field.
        APPEND ls_view_key TO lt_view_key.
        CLEAR:
          ls_view_key.
      ENDLOOP.
    ENDIF.

    LOOP AT gt_pp_output ASSIGNING <main_output> WHERE object_view = <main_setup>-object_view.
      APPEND <main_output> TO lt_sequence.
    ENDLOOP.

    <main_setup>-sequence[]  = lt_sequence[].
    <main_setup>-view_keys[] = lt_view_key[].
    REFRESH:
     lt_view_key[],
     lt_sequence[].
  ENDLOOP.


* Set default SDM Type and include any customr SDM Types
  gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gc_val
                                                       x_sdm_source = gc_rep ).

*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_val.
*  APPEND gs_sdm_type TO gr_sdm_type.

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

*  TRY.
*      GET BADI sdm_handle
*        FILTERS
*          sdm_type_main = gc_val.
*
*    CATCH cx_badi_not_implemented.
*      CLEAR sdm_handle.
*  ENDTRY.
*
*  IF NOT sdm_handle IS INITIAL.
*    CALL BADI sdm_handle->add_sdm_type
*      EXPORTING
*        x_source          = gc_rep
*      CHANGING
*        xt_sdm_type       = gr_sdm_type
*      EXCEPTIONS
*        application_error = 1
*        OTHERS            = 2.
*    IF sy-subrc <> 0.
**          MESSAGE e() RAISING application_error.
*    ENDIF.
*  ENDIF.

* Build a list of all the relevant SDM objects
  gt_objects = /gda/sdm_cl_common_core=>get_sdm_objects( x_sdm_obect = x_object_type
                                                         xt_sdm_types = gr_sdm_type ).


*  SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
*    WHERE  sdm_object  = x_object_type
*     AND   active      = abap_true
*     AND   type        IN gr_sdm_type.

  gv_source = /gda/sdm_cl_core=>mc_rep.

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

FORM screen_output.
  DATA:
    lv_text(80).

  FIELD-SYMBOLS:
    <parameter> TYPE any.

  LOOP AT SCREEN.
    IF screen-group1 = 'STA'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  LOOP AT SCREEN.
    LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
      lv_text = '%_&&_%_APP_%-TEXT'.
      REPLACE ALL OCCURRENCES OF '&&' IN lv_text WITH <main_setup>-object_view_o.
      IF screen-name = lv_text.
        ASSIGN (screen-name) TO <name>.
        <name> = <main_setup>-object_view_d.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  WRITE icon_red_light     AS ICON TO icon1.
  WRITE icon_yellow_light  AS ICON TO icon2.
  WRITE icon_green_light   AS ICON TO icon3.
  WRITE icon_green_light   AS ICON TO icon6.
  WRITE icon_complete      AS ICON TO icon4.
  WRITE icon_message_error AS ICON TO icon5.

  CASE ok_code.
    WHEN 'COMP'.
      IF p_com = abap_true. " OR p_fai = abap_true.
        p_amb = abap_false.
        p_suc = abap_false.
        p_red = abap_false.
        p_fai = abap_false.
        p_gre = abap_false.
      ENDIF.

      IF p_com = abap_true.
* set to true..
        LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
          ASSIGN (<main_setup>-object_view_o) TO <parameter>.
          CHECK <parameter> IS ASSIGNED.
          <parameter> = abap_true.
        ENDLOOP.

        LOOP AT SCREEN.
          IF screen-name  = 'SEL_ALL' OR screen-name  = 'DSEL_ALL'." or  screen-name CS 'P_'.
            screen-input = 0.
            MODIFY SCREEN.
          ELSE.
            READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view_o = screen-name.
            IF sy-subrc = 0.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    WHEN 'FAIL'.
      IF p_fai = abap_true.
        p_amb = abap_false.
        p_suc = abap_false.
        p_red = abap_false.
        p_com = abap_false.
        p_gre = abap_false.
      ENDIF.

    WHEN 'SUCCESS'.
      IF p_suc = abap_true.
        p_amb = abap_false.
        p_red = abap_false.
        p_gre = abap_false.
        p_com = abap_false.
      ENDIF.

    WHEN 'IND'.
      IF p_amb = abap_true OR p_red = abap_true OR p_gre = abap_true.
        p_com = abap_false.
        p_fai = abap_false.
        p_suc = abap_false.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.
ENDFORM.

FORM at_selection_screen .
  FIELD-SYMBOLS:
    <parameter>.
**// Select/Deselect All
  CASE sy-ucomm.
    WHEN 'SEL'.
      LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
        CHECK <parameter> IS ASSIGNED.
        <parameter> = abap_true.
      ENDLOOP.

    WHEN 'DSEL'.
      LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
        CHECK <parameter> IS ASSIGNED.
        <parameter> = abap_false.
      ENDLOOP.
  ENDCASE.

  ok_code = sy-ucomm.
  IF sy-ucomm = space.
    gs_sscrfields-ucomm = 'ONLI'.
  ELSE.
    gs_sscrfields-ucomm = sy-ucomm.
  ENDIF.

ENDFORM.

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
**      ASSIGN ro_table->* TO <dyn_table2>.
*
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
*
**      ASSIGN <main_setup>-ro_table->* TO <dyn_table_final>.
** Create dynamic work area and assign to FS
*      CREATE DATA ro_line LIKE LINE OF <dyn_table>.
*      ASSIGN ro_line->* TO <dyn_wa>.
**      ASSIGN ro_line->* TO <dyn_wa2>.
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
*  REFRESH:
*    gt_view_struc[].
*ENDFORM.                    " BUILD_DYNAMIC_ITAB


*FORM limit_max_entries CHANGING cv_max_no TYPE p_dbacc
*                                cv_execute_report TYPE abap_bool.
*  DATA: lv_question    TYPE c LENGTH 200,
*        lv_answer      TYPE c LENGTH 1,
*        lv_max_entries TYPE string.
*
**  cv_max_no = 0.
*  cv_execute_report = abap_true.
*
*  lv_max_entries = gc_maxdb_entries.
*  SHIFT lv_max_entries LEFT DELETING LEADING '0'.
*
*  lv_question = |No data entered into Selection-screen. The number of database entries will be restricted to |
*                && lv_max_entries && |. Continue?|.
*
*  IF sy-batch = space AND ( gs_sscrfields-ucomm = 'ONLI' OR gs_sscrfields-ucomm = 'PRIN' ) AND gv_selection_fields_entered = abap_false.
*    PERFORM popup_to_confirm USING lv_question CHANGING lv_answer.
*    IF lv_answer = '1'. "Yes
*      cv_max_no = gc_maxdb_entries.
*    ELSE.
*      cv_execute_report = abap_false.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.

*FORM popup_to_confirm USING iv_question TYPE c
*                      CHANGING cv_answer TYPE flag.
*
*  DATA:
*    lv_answer TYPE c LENGTH 1.
*
*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
*      text_question  = iv_question
*    IMPORTING
*      answer         = lv_answer
*    EXCEPTIONS
*      text_not_found = 1
*      OTHERS         = 2.
*
*  IF sy-subrc <> 0.
*    RETURN.
*  ENDIF.
*
*  cv_answer = lv_answer.
*
*ENDFORM.

FORM message_filter USING    p_result
                    CHANGING p_exit   TYPE char1.
  FIELD-SYMBOLS:
    <number> TYPE any,
    <id>     TYPE any.

  IF s_arbgb IS NOT INITIAL.
    ASSIGN COMPONENT 'ID'     OF STRUCTURE p_result TO <id>.
  ENDIF.
  IF s_msgnr IS NOT INITIAL.
    ASSIGN COMPONENT 'NUMBER' OF STRUCTURE p_result TO <number>.
  ENDIF.

  IF <id> IS ASSIGNED AND <number> IS NOT ASSIGNED.
    IF <id> IN s_arbgb.
      p_exit =  abap_false.
    ELSE.
      p_exit =  abap_true.
    ENDIF.
  ELSEIF <number> IS ASSIGNED AND <id> IS NOT ASSIGNED.
    IF <number> IN s_msgnr.
      p_exit =  abap_false.
    ELSE.
      p_exit =  abap_true.
    ENDIF.
  ELSEIF <number> IS ASSIGNED AND <id> IS ASSIGNED.
    IF <number> IN s_msgnr AND <id> IN s_arbgb.
      p_exit =  abap_false.
    ELSE.
      p_exit =  abap_true.
    ENDIF.
  ENDIF.

ENDFORM.

FORM message_context_link USING x_result   TYPE any
                                x_obj_type TYPE /gda/sdm_de_object.
  DATA:
    lt_general TYPE STANDARD TABLE OF /gda/sdm_setup4,
    lv_field   TYPE fieldname,
    lv_table   TYPE tabname16.

  FIELD-SYMBOLS:
    <field>   TYPE any,
    <type>    TYPE any,
    <icon>    TYPE any,
    <general> LIKE LINE OF lt_general.

* EXTRA_V1 contains table-field which is needs for mapping contect to messages
  ASSIGN COMPONENT 'TYPE'     OF STRUCTURE x_result TO <type>.
  ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE x_result TO <field>.

  IF <field> IS ASSIGNED.

    SPLIT <field> AT '-' INTO lv_table lv_field.

    SELECT object_view FROM /gda/sdm_setup4
      INTO CORRESPONDING FIELDS OF TABLE lt_general
      WHERE  object_type = x_obj_type
        AND  field       = lv_field
        AND  tabname     = lv_table
         AND outp        = abap_true.
  ENDIF.

  LOOP AT lt_general ASSIGNING <general>.
    ASSIGN COMPONENT <general>-object_view OF STRUCTURE <dyn_wa> TO <icon>.
    IF sy-subrc = 0.
      CASE <type>.
        WHEN 'E'.
          <icon> = icon_red_light.
        WHEN 'W'.
          IF <icon> NE icon_red_light.
            <icon> = icon_yellow_light.
          ENDIF.
        WHEN OTHERS.
          IF <icon> NE icon_red_light.
            <icon> = icon_green_light.
          ENDIF.
      ENDCASE.

    ENDIF.
  ENDLOOP.
ENDFORM.

*FORM brf_logic  USING x_type
*                      x_object_type
*                      x_source
*                CHANGING xo_object     TYPE REF TO /gda/sdm_cl_core.
*.
*
*  DATA:
*    lx_fdt TYPE REF TO cx_fdt.
*
*  IF xo_object IS INITIAL.
*    TRY.
*        xo_object ?= /gda/sdm_cl_core=>factory( iv_object_type = x_object_type
*                                             iv_source         = x_source
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
**  IF xo_article IS NOT INITIAL.
**    xo_article->display_messages( ).
***    EXIT.
**  ELSE.
**    EXIT.
**  ENDIF.
*
*  CHECK xo_object->mt_message[] IS INITIAL.
*
**  PERFORM set_data USING x_type "gv_type
**                   CHANGING xo_object.
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
*ENDFORM.                    " BRF_LOGIC
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_SDM_MATERIAL  text
*----------------------------------------------------------------------*
FORM determine_output  USING    x_sdm_object
                       CHANGING  xt_sdm_object TYPE STANDARD TABLE.

  DATA:
    lv_append,
    lv_append2.

  FIELD-SYMBOLS:
   <icon> TYPE any.

*  lv_append = abap_true.

* All Records
* determine if this record should be output
  IF p_red = abap_true AND p_amb = abap_true AND p_gre = abap_true.
    APPEND x_sdm_object TO xt_sdm_object.
    APPEND <dyn_wa> TO <dyn_table>.
  ENDIF.

* Errors Only - must contain at least one red icon
  IF p_red = abap_true AND p_amb = abap_false AND p_gre = abap_false.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_red_light.
        lv_append = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_append = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.

  ENDIF.

* Warning Only - must contain at least one amber icon
  IF p_red = abap_false AND p_amb = abap_true AND p_gre = abap_false.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light.
        lv_append = abap_true.
        EXIT.
      ENDIF.
    ENDDO.
    IF lv_append = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.

* Success Only - must contain at least one amber icon
  IF p_red = abap_false AND p_amb = abap_false AND p_gre = abap_true.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_green_light.
        lv_append = abap_true.
        EXIT.
      ENDIF.
    ENDDO.
    IF lv_append = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.

* Success Only - must contain at least one green icon
  IF p_red = abap_false AND p_amb = abap_false AND p_gre = abap_true.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light OR <icon> = icon_red_light.
        lv_append = abap_false.
        EXIT.
      ENDIF.

      IF <icon> = icon_green_light.
        lv_append = abap_true.
      ENDIF.

    ENDDO.
    IF lv_append = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.


* Success Only and Warning - must contain at least one green icon and ome yellow icon
  IF p_red = abap_false AND p_amb = abap_true AND p_gre = abap_true.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_green_light.
        lv_append = abap_false.
        EXIT.
      ENDIF.
    ENDDO.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light.
        lv_append2 = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_append = abap_true AND lv_append2 = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.

* Error and Success - must contain at least one green icon and ome yesllow icon
  IF p_red = abap_true AND p_amb = abap_false AND p_gre = abap_true.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT. " no more components
      ENDIF.

      IF <icon> = icon_green_light.
        lv_append = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_red_light.
        lv_append2 = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_append = abap_true AND lv_append2 = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.

* Error and Warning - must contain at least one green icon and ome yesllow icon
  IF p_red = abap_true AND p_amb = abap_true AND p_gre = abap_false.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_red_light. " OR <icon> = icon_yellow_light.
        lv_append = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light.
        lv_append2 = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_append = abap_true AND lv_append2 = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.

* Warning and Success - must contain at least one green icon and ome yesllow icon
  IF p_red = abap_false AND p_amb = abap_true AND p_gre = abap_true.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_green_light. " OR <icon> = icon_yellow_light.
        lv_append = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light.
        lv_append2 = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    IF lv_append = abap_true AND lv_append2 = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.

* Success All -
* Only Green Items will appear on the line
  IF p_com = abap_true OR p_suc = abap_true.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light OR <icon> = icon_red_light.
        lv_append = abap_false.
        EXIT.
      ENDIF.

      IF <icon> = icon_green_light.
        lv_append = abap_true.
      ENDIF.

    ENDDO.
    IF lv_append = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.

* Errors All -
* Only Red Icons will appear on the entire line
  IF p_fai = abap_true.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light OR <icon> = icon_green_light.
        lv_append = abap_false.
        EXIT.
      ENDIF.

      IF <icon> = icon_red_light.
        lv_append = abap_true.
      ENDIF.

    ENDDO.
    IF lv_append = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.


* Success All and Errors All -
* Only Green Items should be appera on teh line
  IF p_com = abap_true AND p_fai = abap_true.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light OR <icon> = icon_red_light.
        lv_append = abap_false.
        EXIT.
      ENDIF.

      IF <icon> = icon_green_light.
        lv_append = abap_true.
      ENDIF.

    ENDDO.
    IF lv_append = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.

* Errors All
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <dyn_wa> TO <icon>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <icon> = icon_yellow_light OR <icon> = icon_green_light.
        lv_append = abap_false.
        EXIT.
      ENDIF.

      IF <icon> = icon_red_light.
        lv_append = abap_true.
      ENDIF.

    ENDDO.
    IF lv_append = abap_true.
      APPEND x_sdm_object TO xt_sdm_object.
      APPEND <dyn_wa> TO <dyn_table>.
    ENDIF.
  ENDIF.

ENDFORM.

FORM set_display_top.
  DATA:
*   lo_sort  TYPE REF TO cl_salv_sorts,
    ls_txt_l  TYPE scrtext_l,
    ls_txt_m  TYPE scrtext_m,
    ls_txt_s  TYPE scrtext_s,
*    ls_col    TYPE lvc_fname,
    ls_layout TYPE lvc_s_layo.


  FIELD-SYMBOLS:
    <parameter>  TYPE any,
    <view_setup> LIKE LINE OF gt_pp_main_setup.

* Create Instance
  TRY.
      CREATE OBJECT go_alv_top
        EXPORTING
          i_parent = go_parent1.

    CATCH cx_salv_msg INTO gx_root.
      gv_message = gx_root->get_text( ).
  ENDTRY.

  IF gv_message IS NOT INITIAL.
    MESSAGE e000(/gda/sdm_pp1) WITH gv_message.
  ENDIF.

**  GO_LAYOUT->SET_KEY( GV_KEY ).
*  go_layout_top->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  go_layout_top->set_default( if_salv_c_bool_sap=>true ).
*
** Get Functions
*  go_func_top = go_table_top->get_functions( ).
*  go_func_top->set_all( ).
*  go_columns_top = go_table_top->get_columns( ).
*  go_columns_top->set_optimize( 'X' ).

  READ TABLE gt_pp_main_setup ASSIGNING <main_setup> WITH KEY object_view = 'DEFAULT'.

  IF <main_setup> IS ASSIGNED.
    LOOP AT <main_setup>-tabstruc ASSIGNING <tabstruc>.
      IF <tabstruc>-fieldname = 'MESSAGE'.
        CONTINUE.
      ENDIF.
*      TRY.
*          go_column_top ?= go_columns_top->get_column( <tabstruc>-fieldname ).
*        CATCH cx_salv_not_found.
*      ENDTRY.
*
      IF <tabstruc>-key = abap_true.
*        TRY.
*            go_column_top->set_key( value  = if_salv_c_bool_sap=>true ).
*          CATCH cx_salv_data_error .
*        ENDTRY.
      ENDIF.
*
*      TRY.
*          go_column_top->set_alignment( value  = if_salv_c_alignment=>left ).
*        CATCH cx_salv_data_error .
*      ENDTRY.
*
*      go_column_top->set_long_text( <tabstruc>-scrtext_l ).
*      go_column_top->set_medium_text( <tabstruc>-scrtext_m ).
*      go_column_top->set_short_text( <tabstruc>-scrtext_s ).
*      go_column_top->set_icon( abap_true ).
*      go_column_top->set_visible( abap_true ).
*
      READ TABLE gt_pp_main_setup ASSIGNING <view_setup> WITH KEY object_view = <tabstruc>-fieldname.
      IF sy-subrc = 0.
        ASSIGN (<view_setup>-object_view_o) TO <parameter>.
        IF <parameter> = abap_true.
          ls_txt_l  = <view_setup>-object_view_d.
          ls_txt_m  = <view_setup>-object_view_d.
          ls_txt_s  = <view_setup>-object_view_d.

          <tabstruc>-scrtext_l = ls_txt_l.
          <tabstruc>-scrtext_m = ls_txt_m.
          <tabstruc>-scrtext_s = ls_txt_s.
        ENDIF.
      ENDIF.
      IF <tabstruc>-fieldname CS 'MATNR'.
        <tabstruc>-hotspot = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.

  ls_layout-cwidth_opt = abap_true.

*  DATA: ro_table TYPE REF TO data.
*  DATA: ro_sdm_objects TYPE REF TO data.
*
**gt_sdm_material
*  FIELD-SYMBOLS:
*    <sdm_data> TYPE any,
*    <table>    TYPE any.

*  CREATE DATA ro_sdm_objects TYPE STANDARD TABLE OF sdm_objects.
*  ASSIGN ro_sdm_objects->* TO <sdm_data>. "gt_sdm_material[].
*  <sdm_data>  = gt_sdm_material[].
*
*    PERFORM build_dynamic_itab USING gc_default
*                               changing ro_table.
**  CREATE DATA ro_table TYPE STANDARD TABLE OF <dyn_table>.
*  ASSIGN ro_table->* TO <table>. "gt_sdm_material[].
*  <table>  =  <dyn_table>.


  CREATE OBJECT go_handler_1
    EXPORTING
      xt_main_setup  = gt_pp_main_setup[]
      xt_dyn_table   = <dyn_table>
      xt_sdm_objects = <dyn_sdm_res>
      xo_parent      = go_parent2
      xt_msgnr       = s_msgnr[]
      xt_arbgb       = s_arbgb[].

  CREATE OBJECT go_handler_local.

  SET HANDLER go_handler_1->on_hotspot_click    FOR go_alv_top.
  SET HANDLER go_handler_1->handle_context_menu FOR go_alv_top.
  SET HANDLER go_handler_1->toolbar             FOR go_alv_top.

  SET HANDLER go_handler_local->handle_user_command FOR go_alv_top.

* Display Table
  CALL METHOD go_alv_top->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = <main_setup>-tabstruc[]
      it_outtab       = <dyn_table>[]. "<dyn_table_final>[].


ENDFORM.

FORM display_results USING xt_sdm_results TYPE table
                           x_sdm_object   TYPE /gda/sdm_de_object.

  DATA:
     lv_struc TYPE string.

  CASE x_sdm_object.
    WHEN 'ARTICLE'.
      lv_struc = '/gda/sdm_s_article'.
      CREATE DATA go_sdm_results TYPE STANDARD TABLE OF (lv_struc).
    WHEN 'MATERIAL'.
      lv_struc = '/gda/sdm_s_material'.
      CREATE DATA go_sdm_results TYPE STANDARD TABLE OF (lv_struc).
    WHEN 'BOM'.
      lv_struc = '/gda/sdm_s_bom'.
      CREATE DATA go_sdm_results TYPE STANDARD TABLE OF (lv_struc).
    WHEN 'SCUSTOM'.
      lv_struc = '/gda/sdm_s_scustom_rsr'.
      CREATE DATA go_sdm_results TYPE STANDARD TABLE OF (lv_struc).
    WHEN OTHERS.
  ENDCASE.
  ASSIGN go_sdm_results->* TO <dyn_sdm_res>. "gt_sdm_material[].
  <dyn_sdm_res>  = xt_sdm_results[].
  SORT <dyn_sdm_res>.
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VIEW_ICONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_view_icons.
  FIELD-SYMBOLS:
    <view>.

  CLEAR <dyn_wa>.
  LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
    ASSIGN COMPONENT <main_setup>-object_view OF STRUCTURE  <dyn_wa> TO <view>.
    CHECK sy-subrc = 0.
    IF <view> IS ASSIGNED.
      <view> = icon_green_light.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM build_field_selection USING    iv_struc_name TYPE string
*                                    iv_pref       TYPE string
*                           CHANGING xt_field_list TYPE ANY TABLE.
*  DATA:
*    lo_type_descr   TYPE REF TO cl_abap_typedescr,
*    lo_struct_descr TYPE REF TO cl_abap_structdescr,
*    lt_field_list   TYPE TABLE OF char30,
*    lv_last         TYPE i.
*
*  FIELD-SYMBOLS:
*    <ls_components> TYPE LINE OF abap_compdescr_tab,
*    <ls_field_list> TYPE char30.
*
*
*  cl_abap_typedescr=>describe_by_name(
*  EXPORTING
*    p_name         =  iv_struc_name
*    RECEIVING
*    p_descr_ref    =  lo_type_descr
*  EXCEPTIONS
*    type_not_found = 1
*    OTHERS         = 2
*    ).
*  IF sy-subrc = 0.
*    lo_struct_descr ?= lo_type_descr.
*  ELSE.
**            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            INTO me->mv_message
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
***// Build Dynamic Field Selection
*  DESCRIBE TABLE lo_struct_descr->components LINES lv_last.
*  LOOP AT lo_struct_descr->components ASSIGNING <ls_components>.
*    APPEND INITIAL LINE TO lt_field_list ASSIGNING <ls_field_list>.
*    IF sy-tabix = lv_last.
*      CONCATENATE iv_pref <ls_components>-name INTO <ls_field_list>.
*    ELSE.
*      CONCATENATE iv_pref <ls_components>-name ',' INTO <ls_field_list>.
*    ENDIF.
*  ENDLOOP.
*
*  xt_field_list[] =   lt_field_list[].
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_STRING_FROM_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3725   text
*      -->P_GS_MARA_SDM  text
*----------------------------------------------------------------------*
*FORM build_string_from_key  USING p_tabname  TYPE tabname
*                                  p_contents TYPE any
*                            CHANGING c_tabkey TYPE cdtabkey.
** get key fields for table
*
*  TYPES: BEGIN OF s_tab_fields,
*           field TYPE fieldname,
*         END OF s_tab_fields.
*
*  DATA: go_strucdescr TYPE REF TO cl_abap_structdescr,
*        gt_tab_fields TYPE ddfields,
*        lt_fields     TYPE STANDARD TABLE OF s_tab_fields,
*        ls_fields     TYPE s_tab_fields.
*
*  FIELD-SYMBOLS:
*    <gwa_tab_field> TYPE dfies,
*    <contents>      TYPE any,
*    <key_field>     TYPE any.
*
*refresh:
* gt_tab_fields.
*
*  ASSIGN p_contents TO <contents>.
*
*  TRY .
**   Get the details of the DDIC table
*      go_strucdescr ?= cl_abap_elemdescr=>describe_by_name( p_tabname ).
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
*    ASSIGN COMPONENT <gwa_tab_field>-fieldname OF STRUCTURE <contents> TO <key_field>.
*    CHECK sy-subrc = 0.
*     CONCATENATE c_tabkey <key_field> into c_tabkey.
*  ENDLOOP.
*
*ENDFORM.

FORM sdm_init_rsr USING x_object_type TYPE /gda/sdm_de_object.

  DATA:
    lt_view_key TYPE STANDARD TABLE OF struc2,
    ls_view_key TYPE struc2,
    lt_sequence TYPE STANDARD TABLE OF /gda/sdm_setup5,
    lv_type     TYPE /gda/sdm_de_type,
    lv_continue TYPE boolean,
    lo_mapping  TYPE REF TO /gda/sdm_cl_brf_mapping.

  FIELD-SYMBOLS:
     <objects> LIKE LINE OF gt_objects.

* Set selection screen icons
  CONCATENATE icon_select_all   TEXT-005 INTO sel_all  SEPARATED BY space.
  CONCATENATE icon_deselect_all TEXT-006 INTO dsel_all SEPARATED BY space.

* get RSR main setup for object
  SELECT * FROM /gda/sdm_setup3 INTO CORRESPONDING FIELDS OF TABLE gt_pp_main_setup
    BYPASSING BUFFER
    WHERE object_type = x_object_type
      AND status      = abap_true
      ORDER BY ord.

* get RSR output fields for object
  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_pp_main_gen
    WHERE object_type  = x_object_type
      AND outp         = abap_true.

* get RSR DEFAULT output fields for object
  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_default_fields
    WHERE object_type  = x_object_type
      AND object_view  = gc_default
      AND outp         = abap_true
      AND default_all  = abap_true.

* Set up Default Tables used.
  LOOP AT gt_default_fields ASSIGNING <general_default>.
    gs_default_tables = <general_default>-tabname.
    COLLECT gs_default_tables INTO gt_default_tables.
  ENDLOOP.

* get RSR DEFAULT message classes
  SELECT * FROM /gda/sdm_setup8 INTO TABLE gt_default_msg_class
    WHERE sdm_object  = x_object_type.

  LOOP AT gt_default_msg_class ASSIGNING <default_msg_class>.
    s_arbgb-sign   = 'I'.
    s_arbgb-option = 'EQ'.
    s_arbgb-low    = <default_msg_class>-arbgb.
    APPEND s_arbgb.
  ENDLOOP.

* incase nothing is maintained
  IF s_arbgb IS INITIAL.
    s_arbgb-sign   = 'I'.
    s_arbgb-option = 'CP'.
    s_arbgb-low    = '/GDA/*'.
    APPEND s_arbgb.

    s_arbgb-sign   = 'I'.
    s_arbgb-option = 'CP'.
    s_arbgb-low    = 'Z*'.
    APPEND s_arbgb.
  ENDIF.

  SELECT * FROM /gda/sdm_setup5 INTO TABLE gt_pp_output
    WHERE object_type  = x_object_type.

  LOOP AT gt_pp_main_gen ASSIGNING <main_gen>.
    APPEND <main_gen>-tabname TO gt_view_tables_all.
  ENDLOOP.

  SORT gt_view_tables_all.

  DELETE ADJACENT DUPLICATES FROM gt_view_tables_all.

* Build secondary key table for Views
  LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
    LOOP AT gt_pp_main_gen ASSIGNING <main_gen> WHERE object_view = <main_setup>-object_view
                                                  AND default_all = abap_true.

      ls_view_key-tabname = <main_gen>-tabname.
      ls_view_key-field   = <main_gen>-field.
      APPEND ls_view_key TO lt_view_key.
      CLEAR:
        ls_view_key.
    ENDLOOP.
    IF lt_view_key[] IS INITIAL.
* get from default
      LOOP AT gt_pp_main_gen ASSIGNING <main_gen> WHERE object_view = gc_default
                                                    AND default_all = abap_true.

        ls_view_key-tabname = <main_gen>-tabname.
        CONCATENATE 'KEY_' <main_gen>-field INTO ls_view_key-field.
        APPEND ls_view_key TO lt_view_key.
        CLEAR:
          ls_view_key.
      ENDLOOP.
    ENDIF.

    LOOP AT gt_pp_output ASSIGNING <main_output> WHERE object_view = <main_setup>-object_view.
      APPEND <main_output> TO lt_sequence.
    ENDLOOP.

    <main_setup>-sequence[]  = lt_sequence[].
    <main_setup>-view_keys[] = lt_view_key[].
    REFRESH:
     lt_view_key[],
     lt_sequence[].
  ENDLOOP.


* Set default SDM Type and include any customr SDM Types
  gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gc_val
                                                       x_sdm_source = gc_rep ).

*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_val.
*  APPEND gs_sdm_type TO gr_sdm_type.

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

*  TRY.
*      GET BADI sdm_handle
*        FILTERS
*          sdm_type_main = gc_val.
*
*    CATCH cx_badi_not_implemented.
*      CLEAR sdm_handle.
*  ENDTRY.
*
*  IF NOT sdm_handle IS INITIAL.
*    CALL BADI sdm_handle->add_sdm_type
*      EXPORTING
*        x_source          = gc_rep
*      CHANGING
*        xt_sdm_type       = gr_sdm_type
*      EXCEPTIONS
*        application_error = 1
*        OTHERS            = 2.
*    IF sy-subrc <> 0.
**          MESSAGE e() RAISING application_error.
*    ENDIF.
*  ENDIF.

* Build a list of all the relevant SDM objects
  gt_objects = /gda/sdm_cl_common_core=>get_sdm_objects( x_sdm_obect = x_object_type
                                                         xt_sdm_types = gr_sdm_type ).


*  SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
*    WHERE  sdm_object  = x_object_type
*     AND   active      = abap_true
*     AND   type        IN gr_sdm_type.

  gv_source = /gda/sdm_cl_core=>mc_rep.

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
*&---------------------------------------------------------------------*
*&      Form  SDM_LOGIC_RSR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sdm_logic_rsr .
  DATA:
    ro_data       TYPE REF TO data,
    ro_data_empty TYPE REF TO data,
    lv_field      TYPE fieldname,
    lv_count      TYPE p,
    lv_pur        TYPE p,
    lv_text       TYPE string,
    lv_pur_text   TYPE string,
    lv_exit.

  FIELD-SYMBOLS:
    <results>     TYPE STANDARD TABLE,
    <results_pp1> TYPE STANDARD TABLE,
    <result>      TYPE any,
    <field_alv>   TYPE any,
*    <sdm_material> LIKE LINE OF gt_sdm_material,
    <objects>     LIKE LINE OF gt_objects.

  PERFORM build_structure USING gc_default
                                gv_object
                                space.


  PERFORM build_dynamic_itab USING gc_default
                             CHANGING ro_data.

  PERFORM progress_bar USING TEXT-018.

*  DESCRIBE TABLE go_selection->mt_mara LINES lv_count.
*
*  LOOP AT go_selection->mt_mara INTO go_selection->ms_mara_spec.
*    CLEAR:
*      lv_pur_text,
*      lv_pur,
*      lv_text.
*
*    lv_pur = ( sy-tabix / lv_count ) * 100.
*    lv_pur_text = lv_pur.
*    CONCATENATE 'BRF Rules '(917)  lv_pur_text '%' INTO lv_text.
*    PERFORM progress_bar USING lv_text.
*
** Set all Default Views to icon successful
*    PERFORM default_view_icons.
** BRF+ Logic
** Prepare the data for BRF functions - pass to temp tables
*    go_selection->refresh_spec( ).
*    go_selection->mv_spec_matnr = go_selection->ms_mara_spec-matnr.
*    go_selection->build_spec( ).
*
*    APPEND go_selection->ms_mara_spec TO gt_mara_sdm[].
*    APPEND go_selection->ms_makt_spec TO gt_makt_sdm[].
*    gt_marc_sdm[]   = go_selection->mt_marc_spec[].
*    gt_mard_sdm[]   = go_selection->mt_mard_spec[].
*    gt_mbew_sdm[]   = go_selection->mt_mbew_spec[].
*    gt_meinh_sdm[]  = go_selection->mt_meinh_spec[].
*    gt_mfhm_sdm[]   = go_selection->mt_mfhm_spec[].
*    gt_mlgn_sdm[]   = go_selection->mt_mlgn_spec[].
*    gt_mlgt_sdm[]   = go_selection->mt_mlgt_spec[].
*    gt_mvke_sdm[]   = go_selection->mt_mvke_spec[].
*    gt_mean_sdm[]   = go_selection->mt_mean_spec[].
*    gt_mpop_sdm[]   = go_selection->mt_mpop_spec[].
*    gt_marm_sdm[]   = go_selection->mt_marm_spec[].
*    gt_mlan_sdm[]   = go_selection->mt_mlan_spec[].
*    gt_steuer_sdm[] = go_selection->mt_steuertab[].
*    gt_steumm_sdm[] = go_selection->mt_steummtab[].
*
*
** For each Material process the BRF Functions
*    LOOP AT gt_objects ASSIGNING <objects>.
*      CLEAR:
*       <objects>-object.
*      PERFORM brf_logic  USING <objects>-type
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
*                                             gc_material.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
*
** Ensure Default/Key fields are populated..
** The key fields can can only equate to a key  equal to the object type..
** Table that can be used
** MARA
** MAKT
*    READ TABLE gt_makt_sdm INTO gs_makt_temp WITH KEY matnr = gs_mara_sdm-matnr.
*
*    LOOP AT gt_default_fields ASSIGNING <general_default>.
*      CLEAR:
*       lv_field.
*      CONCATENATE 'KEY_' <general_default>-field INTO lv_field.
*      ASSIGN COMPONENT lv_field OF STRUCTURE <dyn_wa> TO <field_alv>.
*      ASSIGN COMPONENT <general_default>-field OF STRUCTURE gs_mara_sdm TO <field>.
*      IF sy-subrc = 0.
*        <field_alv> = <field>.
*      ELSE.
*        ASSIGN COMPONENT <general_default>-field OF STRUCTURE gs_makt_temp TO <field>.
*        CHECK sy-subrc = 0.
*        <field_alv> = <field>.
*      ENDIF.
*    ENDLOOP.
*
*    gs_sdm_objects-material = gs_mara_sdm-matnr.
*    gs_sdm_objects-mara[]   = gt_mara_sdm[].
*    gs_sdm_objects-makt[]   = gt_makt_sdm[].
*    gs_sdm_objects-marc[]   = gt_marc_sdm[].
*    gs_sdm_objects-mard[]   = gt_mard_sdm[].
*    gs_sdm_objects-mbew[]   = gt_mbew_sdm[].
*    gs_sdm_objects-meinh[]  = gt_meinh_sdm[].
*    gs_sdm_objects-mfhm[]   = gt_mfhm_sdm[].
*    gs_sdm_objects-mlgn[]   = gt_mlgn_sdm[].
*    gs_sdm_objects-mlgt[]   = gt_mlgt_sdm[].
*    gs_sdm_objects-mvke[]   = gt_mvke_sdm[].
*    gs_sdm_objects-mean[]   = gt_mean_sdm[].
*    gs_sdm_objects-mpop[]   = gt_mpop_sdm[].
*    gs_sdm_objects-marm[]    = gt_marm_sdm[].
*    gs_sdm_objects-mlan[]    = gt_mlan_sdm[].
*    gs_sdm_objects-steuer[]  = gt_steuer_sdm[].
*    gs_sdm_objects-steumm[]  = gt_steumm_sdm[].
*    gs_syst_sdm              = syst.
*
**ENHANCEMENT-POINT /gda/sdm_mm_art_ep6 SPOTS /gda/sdm_mm_art_es6 .
*
*
*    PERFORM determine_output USING   gs_sdm_objects
*                             CHANGING  gt_sdm_material.
*
*    CLEAR gs_sdm_objects.
*
*    REFRESH:
*     gt_mara_sdm,
*     gt_makt_sdm,
*     gt_marc_sdm,
*     gt_mard_sdm,
*     gt_mbew_sdm,
*     gt_meinh_sdm,
*     gt_mfhm_sdm,
*     gt_mlgn_sdm,
*     gt_mlgt_sdm,
*     gt_mvke_sdm,
*     gt_mean_sdm,
*     gt_mpop_sdm,
*     gt_marm_sdm,
*     gt_mlan_sdm,
*     gt_steuer_sdm,
*     gt_steumm_sdm.
*
*
*
*    UNASSIGN:
*     <results>,
*     <results_pp1>.
*  ENDLOOP.

  PERFORM progress_bar USING TEXT-019.

ENDFORM.
