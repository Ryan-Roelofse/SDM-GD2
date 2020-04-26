*----------------------------------------------------------------------*
***INCLUDE /GDA/SDM_MAT_REC_STAT_REP1_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_SDM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM init.
*  DATA:
*    lt_view_key TYPE STANDARD TABLE OF struc2,
*    ls_view_key TYPE struc2,
*    lt_sequence TYPE STANDARD TABLE OF /gda/sdm_setup5.
*
*  IF go_selection IS INITIAL.
*    go_selection = lcl_mm_material_exc=>factory( ).
*  ENDIF.
*
*  CONCATENATE icon_select_all   TEXT-005 INTO sel_all  SEPARATED BY space.
*  CONCATENATE icon_deselect_all TEXT-006 INTO dsel_all SEPARATED BY space.
*
*  SELECT * FROM /gda/sdm_setup3 INTO CORRESPONDING FIELDS OF TABLE gt_pp_main_setup
*    WHERE object_type = gc_material
*      AND status      = abap_true
*      ORDER BY ord.
*
*  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_pp_main_gen
*    WHERE object_type  = gc_material
*      AND outp         = abap_true.
*
** Set up Default Fields Heading
*  SELECT * FROM /gda/sdm_setup4 INTO TABLE gt_default_fields
*    WHERE object_type  = gc_material
*      AND object_view  = gc_default
*      AND outp         = abap_true
*      AND default_all  = abap_true.
*
*
*  SELECT * FROM /gda/sdm_setup5 INTO TABLE gt_pp_output
*    WHERE object_type  = gc_material.
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
*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_val.
*  APPEND gs_sdm_type TO gr_sdm_type.
*
*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_pir.
*  APPEND gs_sdm_type TO gr_sdm_type.
*
*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_pri.
*  APPEND gs_sdm_type TO gr_sdm_type.
*
*  gs_sdm_type-sign   =  'I'.
*  gs_sdm_type-option =  'EQ'.
*  gs_sdm_type-low    =  gc_src.
*  APPEND gs_sdm_type TO gr_sdm_type.
*
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
*
*  SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
*    WHERE  sdm_object  = gc_material
*     AND   active      = abap_true
*     AND   type        IN gr_sdm_type.
*
*  gv_source = /gda/sdm_cl_core=>mc_rep.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM at_selection_screen .
*  FIELD-SYMBOLS:
*    <parameter>.
***// Select/Deselect All
*  CASE sy-ucomm.
*    WHEN 'SEL'.
*      LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*        <parameter> = abap_true.
*      ENDLOOP.
*
*    WHEN 'DSEL'.
*      LOOP AT gt_pp_main_setup ASSIGNING <main_setup>.
*        ASSIGN (<main_setup>-object_view_o) TO <parameter>.
*        <parameter> = abap_false.
*      ENDLOOP.
*  ENDCASE.
*
*  ok_code = sy-ucomm.
*
*ENDFORM.
