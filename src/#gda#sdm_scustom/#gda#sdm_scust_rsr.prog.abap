*&---------------------------------------------------------------------*
*& Report /GDA/SDM_SCUSTOM_EXP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_scust_rsr  MESSAGE-ID /gda/sdm_scu.
TABLES:
  scustom,
  t100.

CLASS: lcl_event_rec DEFINITION DEFERRED.

DATA:
  go_handler_local TYPE REF TO lcl_event_rec,
  go_handler_top   TYPE REF TO lcl_event_rec.

* SDM Excel Download logic(RSR)
INCLUDE /gda/fordata.
* SDM SCUSTOM data declarations
INCLUDE /gda/sdm_scust_data.
* SDM SCUSTOM object declarations
INCLUDE /gda/sdm_scust_obj_data.
*** Screens start
* SDM SCUST Config generated screen
INCLUDE /gda/sdm_scust_scr2.
* SDM SCUST specific selections
INCLUDE /gda/sdm_scust_scr1.
* SDM common selections(common RSR)
INCLUDE /gda/sdm_include_sdm_scr2.
* SDM common selections(common)
INCLUDE /gda/sdm_include_sdm_scr3.
* SDM local class(common RSR)
**** Screens end
INCLUDE /gda/sdm_scust_lcl_class.
* SDM commom logic(common)
INCLUDE /gda/sdm_common_core.
* SDM commom logic(common RSR)
INCLUDE /gda/sdm_common_core_rsr.
* SDM SCUSTOM logic
INCLUDE /gda/sdm_scust_logic.

INITIALIZATION.
* SDM Initialise(common)
  PERFORM sdm_init_common USING    gc_scustom
                   CHANGING gv_object gv_type gv_source go_selection.

* SDM Initialise(RSR)
  PERFORM sdm_init_rsr USING gv_object.

* SDM Authority Check(common)
  PERFORM auth_check    USING gc_auth_e.

AT SELECTION-SCREEN OUTPUT.
* SDM(RSR)
  PERFORM screen_output.

AT SELECTION-SCREEN.
* SDM(RSR)
  PERFORM at_selection_screen.

START-OF-SELECTION.

  gs_selscreen-id       = s_id[].

* SDM Check Selection Screen(common)
  PERFORM check_selection_entries CHANGING gv_selection_fields_entered.
  PERFORM limit_max_entries       CHANGING p_max gv_execute_report.

  IF gv_execute_report = abap_false.
    RETURN.
  ENDIF.

  gs_selscreen-scustom  = p_scust.
  gs_selscreen-max_rows = p_max.
  go_selection->set_selscreen( xs_selscreen = gs_selscreen ).

  IF gv_execute_report = abap_false.
    RETURN.
  ENDIF.
* SDM Get data(common)
  PERFORM sdm_selection.

* SDM lofig(SCUSTOM)
  PERFORM sdm_main_scustom_rsr.

END-OF-SELECTION.
  IF gv_execute_report = abap_false.
    RETURN.
  ENDIF.

  IF <dyn_table>[] IS NOT INITIAL.
    CALL FUNCTION '/GDA/SDM_RSR_OUTPUT'
      EXPORTING
        xt_main_setup    = gt_pp_main_setup
      TABLES
        xt_sdm_objects   = gt_sdm_objects[] "<dyn_sdm_res>
        xt_sdm_dyn_table = <dyn_table>.
  ELSE.
    IF gv_config_err = abap_true.
*   SDM Setup Error
      MESSAGE s005(/gda/sdm_pp1).
    ELSE.
*   No data for specified selections
      MESSAGE e006(/gda/sdm_pp1).
    ENDIF.
  ENDIF.


FORM   sdm_main_scustom_rsr.
  DATA:
    lo_data_empty TYPE REF TO data,
    lo_data       TYPE REF TO data,
    lv_count      TYPE p,
    lv_per        TYPE p,
    lv_text       TYPE string,
    lv_per_text   TYPE string,
    lv_field      TYPE fieldname,
    lv_exit.

  FIELD-SYMBOLS:
    <results_temp> LIKE gt_results,
    <result>       TYPE any,
    <field>        TYPE any,
    <field_alv>    TYPE any.

  PERFORM build_structure USING gc_default
                                gv_object
                                space.


  PERFORM build_dynamic_itab USING gc_default
                           CHANGING ro_data.

  PERFORM progress_bar USING TEXT-018.

  DESCRIBE TABLE go_selection->mt_scustom LINES lv_count.
  PERFORM progress_bar USING TEXT-018.

  LOOP AT go_selection->mt_scustom INTO go_selection->ms_scustom_spec.

    lv_per = ( sy-tabix / lv_count ) * 100.
    lv_per_text = lv_per.
    CONCATENATE 'BRF Rules'(917)  lv_per_text '%' INTO lv_text.
    PERFORM progress_bar USING lv_text.

* Set all Default Views to icon successful
    PERFORM default_view_icons.

* BRF+ Logic
* Prepare the data for BRF functions - pass to temp tables
    go_selection->refresh( ).
    go_selection->mv_spec_id = go_selection->ms_scustom_spec-id.
    go_selection->build_spec( ).

    gt_scustom_sdm[]   = go_selection->mt_scustom_spec[].

* Populate additional structures
* gt_Z****_sdm[]   = go_selection->mt_Z****_spec[].
ENHANCEMENT-POINT /GDA/SDM_SCUST_EP_RSR SPOTS /GDA/SDM_SCUST_ES_RSR1 .



* For each SDM Object process the BRF Functions
    LOOP AT gt_objects ASSIGNING <objects>.
      CLEAR:
       <objects>-object.

      TRY.
          <objects>-object ?= /gda/sdm_cl_common_core=>sdm_initialise(  iv_object_type = gv_object
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
          CATCH /gda/cx_sdm_exception_handl .
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

      gs_instance-type   = <objects>-type.
      gs_instance-object = <objects>-object.
      APPEND gs_instance TO gs_sdm_objects-sdm_instances.

    ENDLOOP.

    CHECK gv_config_err = abap_false.

    IF <results> IS ASSIGNED.
      SORT <results>.
      DELETE ADJACENT DUPLICATES FROM <results>.

      IF <results> IS NOT INITIAL.
        LOOP AT <results> ASSIGNING <result>.

          PERFORM message_filter USING    <result>
                                 CHANGING lv_exit.

          CHECK lv_exit = abap_false.

          PERFORM message_context_link USING <result>
                                             gv_object.
        ENDLOOP.
      ENDIF.
    ENDIF.

    LOOP AT gt_default_fields ASSIGNING <general_default>.
      CLEAR:
       lv_field.
      CONCATENATE 'KEY_' <general_default>-field INTO lv_field.
      ASSIGN COMPONENT lv_field OF STRUCTURE <dyn_wa> TO <field_alv>.
      ASSIGN COMPONENT <general_default>-field OF STRUCTURE go_selection->ms_scustom_spec TO <field>.
      IF sy-subrc = 0.
        <field_alv> = <field>.
      ENDIF.
    ENDLOOP.

    gs_sdm_objects-customer = go_selection->ms_scustom_spec-id.
    gs_sdm_objects-scustom[] = gt_scustom_sdm[].

* Populate additional structures
* gs_sdm_objects-Z****[] = gt_Z****_sdm[].
ENHANCEMENT-POINT /GDA/SDM_SCUST_EP_RSR2 SPOTS /GDA/SDM_SCUST_ES_RSR2 .


    PERFORM determine_output USING   gs_sdm_objects
                             CHANGING  gt_sdm_objects.

    CLEAR gs_sdm_objects.

    REFRESH:
     gt_scustom_sdm.

    UNASSIGN:
     <results>.
  ENDLOOP.
ENDFORM.
