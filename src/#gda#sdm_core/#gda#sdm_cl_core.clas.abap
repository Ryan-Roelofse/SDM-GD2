class /GDA/SDM_CL_CORE definition
  public
  create protected .

public section.

  interfaces /GDA/SDM_IF_ACTION_PROCESSOR .

  types:
    BEGIN OF ty_brf_attributes,
        type        TYPE /gda/sdm_de_type,
        name        TYPE abap_parmname,
        data_object TYPE string,
        ref_table   TYPE char100,
        ref_type    TYPE char1,
        lr_data     TYPE REF TO data,
        abap_type   TYPE typename,
      END OF ty_brf_attributes .
  types:
    ty_it_brf_attributes TYPE SORTED TABLE OF ty_brf_attributes WITH UNIQUE KEY type name .

  constants MC_POE type /GDA/SDM_DE_SOURCE value 2 ##NO_TEXT.
  constants MC_REP type /GDA/SDM_DE_SOURCE value 1 ##NO_TEXT.
  constants MC_VALIDATION type /GDA/SDM_DE_TYPE value 1 ##NO_TEXT.
  constants MC_DERIVATION type /GDA/SDM_DE_TYPE value 2 ##NO_TEXT.
  data MV_NO_BRF type BOOLEAN .
  data MT_MESSAGE type IF_FDT_TYPES=>T_MESSAGE .

  class-methods FACTORY
    importing
      !IV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT
      !IV_SOURCE type /GDA/SDM_DE_SOURCE
      !IV_TYPE type /GDA/SDM_DE_TYPE
      !IV_STATS type /GDA/SDM_DE_STATS
      !IV_STATS_BRF type /GDA/SDM_DE_STATS optional
      !IV_ERRORS_ONLY type /GDA/SDM_DE_OUTPUT optional
      !IV_MAPPING type ref to /GDA/SDM_CL_BRF_MAPPING optional
    returning
      value(RO_OBJECT) type ref to OBJECT
    raising
      CX_FDT_INPUT .
  methods CONSTRUCTOR
    importing
      !IV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT
      !IV_OBJECT_TYPE_ID type /GDA/SDM_DE_OBJECT_ID optional
      !IV_SOURCE type /GDA/SDM_DE_SOURCE
      !IV_TYPE type /GDA/SDM_DE_TYPE
      !IV_STATS type BOOLEAN
      !IV_STATS_BRF type BOOLEAN optional
      value(IV_ERRORS_ONLY) type BOOLEAN
      !IV_MAPPING type ref to /GDA/SDM_CL_BRF_MAPPING optional
    raising
      CX_FDT_INPUT .
  class-methods CLASS_CONSTRUCTOR .
  methods MAIN
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL
      CX_FDT_INPUT .
  methods SET_SELECTION
    importing
      !IV_NAME type TYPENAME
      !IV_DATA type ANY
      !IV_TYPE type /GDA/SDM_DE_TYPE optional
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods RETURN_MESSAGE
    returning
      value(RV_MESSAGE) type STRING .
  methods RETURN_BRF_RESULT
    returning
      value(R_RESULT_STRUCTURE) type ref to DATA .
  methods IS_ACTIVE
    returning
      value(R_ACTIVE) type BOOLEAN .
  methods RETURN_BRF_RESULT_STRUCTURE
    returning
      value(R_RESULT_STRUCTURE) type ref to DATA .
  methods CREATE_BRF_RESULT_STRUCTURE
    returning
      value(R_RESULT_STRUCTURE) type ref to DATA .
  methods DISPLAY_MESSAGES .
  methods DISPLAY_MESSAGE .
  methods GET_RELATED_ACTIONS
    returning
      value(R_ACTIONS) type /GDA/SDM_TT_OBJ_ACTION .
  methods LOAD_RELATED_ACTIONS
    returning
      value(R_ACTIONS) type /GDA/SDM_TT_OBJ_ACTION .
  methods GET_OBJECT_ATTRIBUTES
    importing
      !IV_TYPE type /GDA/SDM_DE_TYPE optional
    returning
      value(RO_ATTRIBUTES) type TY_IT_BRF_ATTRIBUTES .
  methods GET_OBJECT_TYPE
    returning
      value(R_OBJECT_TYPE) type /GDA/SDM_DE_OBJECT .
  methods GET_OBJECT_KEYS
    returning
      value(R_KEY_FIELDS) type /GDA/SDM_T_KEY_FIELDS .
  methods GET_PRIMARY_OBJECT
    returning
      value(R_OBJECT_MAIN) type CHAR30 .
  class-methods SET_LICENSE
    importing
      !LICENSE type STRING
    exporting
      !MSGV3 type CHAR50
      !MSGV4 type CHAR50
      !RESULT type BOOLEAN
      !MESSAGE type NUMC3
      !MSGV1 type CHAR50
      !MSGV2 type CHAR50 .
  methods GET_OBJECT_DESCRIPTION
    returning
      value(R_DESC) type STRING .
protected section.

  data MV_SOURCE type /GDA/SDM_DE_SOURCE .
  data MV_MODULE type /GDA/SDM_DE_MOD_TYPE .
  data MV_TYPE type /GDA/SDM_DE_TYPE .
  data MV_MESSAGE type STRING value '|BRF MAPPING INACTIVE FOR OBJECT &1, INCLUDE &2|' ##NO_TEXT.
  data MV_TIMESTAMP type IF_FDT_TYPES=>TIMESTAMP .
  data MV_GUID type IF_FDT_TYPES=>ID .
  data MV_ACTIVE type XFELD .
  data MV_INCLUDE type PROGNAME .
  data MV_STATS type BOOLEAN .
  data MV_STATS_BRF type BOOLEAN .
  data MV_ERRORS_ONLY type BOOLEAN .
  data MV_OBJECT_ID type /GDA/SDM_DE_OBJECT_ID .
  data MV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT .
  data MV_OBJECT_ID_DESC type STRING .
  data MT_DERIVATION_RESULTS type /GDA/SDM_T_DER_RESULTS .
  data MT_VALIDATION_RESULTS type /GDA/SDM_T_VAL_RESULTS .
  data MO_BRF_RESULT type ref to DATA .
  class-data MT_BRF_ATTRIBUTES type TY_IT_BRF_ATTRIBUTES .
  data MS_FUNCTION_ATTRIBUTES type /GDA/SDM_SETUP6 .
  data MT_BRF_ATTRIBUTES_INSTANCE type TY_IT_BRF_ATTRIBUTES .
  data MO_SDM_EXCEPTIONS type ref to /GDA/SDM_CL_EXCEPTIONS .
  data MT_SDM_EXCEPTIONS type /GDA/SDM_T_PERSIST_MODEL .
  data MO_BRF_RESULT_EMPTY type ref to DATA .
  class-data MV_VERSION type CHAR4 .
  data MO_MAPPING type ref to /GDA/SDM_CL_BRF_MAPPING .
  class-data MT_FUNCTION_ATTRIBUTES type /GDA/SDM_T_FUNCTION .
  class-data MV_ID type FDT_UUID .
  class-data MV_CLIENT type MANDT .

  methods CALL_BRF
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL
      CX_FDT_INPUT .
  methods CALL_CLASS
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL
      CX_FDT_INPUT .
  methods RECORD_STATISTICS
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods CHECK_AUTHORISATION
    importing
      !IV_ACTVT type ACTIV_AUTH
    returning
      value(RV_AUTHORISED) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_CORE IMPLEMENTATION.


  METHOD /gda/sdm_if_action_processor~process_action.
    DATA:
      lv_fcode      TYPE          sy-ucomm.

*BREAK-POINT.
* Handle common actions to SDM
    lv_fcode = x_action->get_fcode( ).

* Check for common actions
    CASE lv_fcode.


      WHEN 'ARTICLE_DISPLAY'.


      WHEN OTHERS.


    ENDCASE.


  ENDMETHOD.


  METHOD call_brf.

    DATA:
      ls_name_value         TYPE abap_parmbind,
      ls_validation_results TYPE /gda/sdm_s_val_results,
      lt_name_value         TYPE abap_parmbind_tab,
      lt_message            TYPE if_fdt_types=>t_message,
      lr_data               TYPE REF TO data,
      lr_data_empty         TYPE REF TO data,
      lx_fdt                TYPE REF TO cx_fdt.

    FIELD-SYMBOLS:
      <lr_t_validation_results> TYPE STANDARD TABLE, "ZCA_BRF_GT_VAL_RETURN,
      <lr_s_validation_results> TYPE any, "LIKE LS_VALIDATION_RESULTS.
      <brf_attributes>          LIKE LINE OF mt_brf_attributes.

    IF me->mv_stats_brf = abap_false.
      TRY.

          LOOP AT mt_brf_attributes ASSIGNING <brf_attributes> WHERE type = mv_type.
            CHECK <brf_attributes>-data_object IS NOT INITIAL.
            ls_name_value-name = <brf_attributes>-name.

            cl_fdt_function_process=>move_data_to_data_object( EXPORTING ir_data             = <brf_attributes>-lr_data " LR_DATA
                                                                         iv_function_id      = me->mv_guid
                                                                         iv_data_object      = <brf_attributes>-data_object "ZGD_MM_MARA
                                                                         iv_timestamp        = me->mv_timestamp
                                                                         iv_trace_generation = abap_false
                                                                         iv_has_ddic_binding = abap_true
                                                               IMPORTING er_data             = ls_name_value-value ).
            INSERT ls_name_value INTO TABLE lt_name_value.
          ENDLOOP.

          cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = me->mv_guid
                                                                        iv_data_object      = '_V_RESULT'
                                                                        iv_timestamp        = me->mv_timestamp
                                                                        iv_trace_generation = abap_false
                                                              IMPORTING er_data             = lr_data ).
          ASSIGN lr_data->* TO <lr_t_validation_results>.
          mo_brf_result = lr_data.


          cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = me->mv_guid
                                                                        iv_data_object      = '_V_RESULT'
                                                                        iv_timestamp        = me->mv_timestamp
                                                                        iv_trace_generation = abap_false
                                                              IMPORTING er_data             = lr_data_empty ).

          mo_brf_result_empty = lr_data_empty.

          cl_fdt_function_process=>process( EXPORTING iv_function_id = me->mv_guid      "lv_function_id
                                                      iv_timestamp   = me->mv_timestamp
                                            IMPORTING ea_result      = <lr_t_validation_results>
                                            CHANGING  ct_name_value  = lt_name_value ).
        CATCH cx_fdt INTO lx_fdt.
*        me->mv_message = lx_fdt->get_text( ).
*        me->mv_message = |BRF Error:| && me->mv_message.
*        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handling
*          EXPORTING
*            mv_text = mv_message.

          RAISE EXCEPTION TYPE cx_fdt_input
            EXPORTING
              previous   = lx_fdt
              mt_message = lx_fdt->mt_message.

      ENDTRY.
    ELSE.
*      BREAK rroelofse.
* For each table or structure build a key and extract any resulys from main BRF table
*      LOOP AT mt_brf_attributes ASSIGNING <brf_attributes> WHERE type = mv_type.

*      ENDLOOP.

      DATA:
        lt_sdm_exc_mai        TYPE STANDARD TABLE OF /gda/sdm_exc_mai,
        lv_primary_object(30),
        lv_sdm_tabkey         TYPE cdtabkey,
        lv_extra_v1           TYPE symsgv.

      FIELD-SYMBOLS:
        <sdm_exc_mai> LIKE LINE OF lt_sdm_exc_mai.

      cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = me->mv_guid
                                                                    iv_data_object      = '_V_RESULT'
                                                                    iv_timestamp        = me->mv_timestamp
                                                                    iv_trace_generation = abap_false
                                                          IMPORTING er_data             = lr_data ).
      ASSIGN lr_data->* TO <lr_t_validation_results>.

      lv_primary_object =  me->get_primary_object( ).

      CONCATENATE lv_primary_object  '%' INTO lv_sdm_tabkey.
      SELECT * FROM /gda/sdm_exc_mai INTO TABLE lt_sdm_exc_mai
                                     WHERE sdm_object_id = me->mv_object_id
                                       AND sdm_type      = me->mv_type
                                       AND sdm_tabkey    LIKE lv_sdm_tabkey.


      LOOP AT lt_sdm_exc_mai ASSIGNING <sdm_exc_mai>.
        ls_validation_results-sdm_tabkey = <sdm_exc_mai>-sdm_tabkey.
        ls_validation_results-id         = <sdm_exc_mai>-msg_id.
        ls_validation_results-number     = <sdm_exc_mai>-msg_number.
        ls_validation_results-type       = <sdm_exc_mai>-msg_type.
        ls_validation_results-extra_v2   = <sdm_exc_mai>-extra_v2.
        ls_validation_results-extra_v3   = <sdm_exc_mai>-extra_v3.
        ls_validation_results-extra_v4   = <sdm_exc_mai>-extra_v4.
        ls_validation_results-extra_v5   = <sdm_exc_mai>-extra_v5.
        ls_validation_results-extra_v6   = <sdm_exc_mai>-extra_v6.
        CONCATENATE <sdm_exc_mai>-tabname '-' <sdm_exc_mai>-field INTO ls_validation_results-extra_v1.
        APPEND ls_validation_results TO <lr_t_validation_results>.
      ENDLOOP.


      mo_brf_result = lr_data.

    ENDIF.


  ENDMETHOD.


  METHOD call_class.

    DATA:
*      ls_name_value         TYPE abap_parmbind,
*      ls_validation_results TYPE /gda/sdm_s_val_results,
*      lt_name_value         TYPE abap_parmbind_tab,
*      lt_message            TYPE if_fdt_types=>t_message,
      lr_data               TYPE REF TO data,
      lr_data_empty         TYPE REF TO data.
*      lx_fdt                TYPE REF TO cx_fdt.
*
*    FIELD-SYMBOLS:
*      <lr_t_validation_results> TYPE ANY TABLE, "ZCA_BRF_GT_VAL_RETURN,
*      <lr_s_validation_results> TYPE any, "LIKE LS_VALIDATION_RESULTS.
*      <brf_attributes>          LIKE LINE OF mt_brf_attributes.
*
*    TRY.
*
*        LOOP AT mt_brf_attributes ASSIGNING <brf_attributes>.
*          ls_name_value-name = <brf_attributes>-name.
*
*          cl_fdt_function_process=>move_data_to_data_object( EXPORTING ir_data             = <brf_attributes>-lr_data " LR_DATA
*                                                                       iv_function_id      = me->mv_guid
*                                                                       iv_data_object      = <brf_attributes>-data_object "ZGD_MM_MARA
*                                                                       iv_timestamp        = me->mv_timestamp
*                                                                       iv_trace_generation = abap_false
*                                                                       iv_has_ddic_binding = abap_true
*                                                             IMPORTING er_data             = ls_name_value-value ).
*          INSERT ls_name_value INTO TABLE lt_name_value.
*        ENDLOOP.
*
*        cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = me->mv_guid
*                                                                      iv_data_object      = '_V_RESULT'
*                                                                      iv_timestamp        = me->mv_timestamp
*                                                                      iv_trace_generation = abap_false
*                                                            IMPORTING er_data             = lr_data ).
*        ASSIGN lr_data->* TO <lr_t_validation_results>. "<la_any>.
*        mo_brf_result = lr_data.
*
*        cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = me->mv_guid
*                                                                      iv_data_object      = '_V_RESULT'
*                                                                      iv_timestamp        = me->mv_timestamp
*                                                                      iv_trace_generation = abap_false
*                                                            IMPORTING er_data             = lr_data_empty ).

    DATA:
      wf_ref       TYPE REF TO data,
      wf_ref_dummy TYPE REF TO data.


    FIELD-SYMBOLS:
      <ref> TYPE any.

    CREATE DATA wf_ref TYPE ('/GDA/SDM_T_VAL_RESULTS'). "Dynamic Structure
    ASSIGN wf_ref->* TO <ref>.
    GET REFERENCE OF <ref> INTO lr_data_empty.

    mo_brf_result_empty = lr_data_empty.


* Add some dummy data for demo....
    CREATE DATA wf_ref_dummy TYPE ('/GDA/SDM_T_VAL_RESULTS'). "Dynamic Structure
    ASSIGN wf_ref_dummy->* TO <ref>.
    GET REFERENCE OF <ref> INTO lr_data_empty.


  ENDMETHOD.


  method CHECK_AUTHORISATION.
    rv_authorised = mo_sdm_exceptions->check_authorisation( iv_actvt = iv_actvt  ).
  endmethod.


  METHOD class_constructor.

*data:
*lv_result  type boolean,
*lv_message type string.

    SELECT SINGLE client FROM /gda/sdm_client
                    INTO mv_client
                    WHERE client = sy-mandt
                      AND active = abap_true.

  ENDMETHOD.


  METHOD constructor.

    DATA:
      lx_fdt     TYPE REF TO cx_fdt,
      lx_root    TYPE REF TO cx_root,
      ls_message TYPE if_fdt_types=>s_message,
      lv_message TYPE string,
      lv_method  TYPE string,
      lv_result  TYPE boolean.

    FIELD-SYMBOLS:
     <brf_attributes> LIKE LINE OF mt_brf_attributes.


    me->mv_object_type  = iv_object_type.
    me->mv_object_id    = iv_object_type_id.
    me->mv_type         = iv_type.
    me->mv_source       = iv_source.
    me->mv_stats        = iv_stats.
    me->mv_stats_brf    = iv_stats_brf.
    me->mv_errors_only  = iv_errors_only.
    me->mv_version      = 'V2.0'.
    me->mo_mapping      = iv_mapping.

    CASE me->mv_source.
      WHEN '1'.
        me->mv_module = '01'.
      WHEN '2'.
        me->mv_module = '02'.
    ENDCASE.

    IF mt_function_attributes IS INITIAL.

      TRY.
          SELECT SINGLE * FROM /gda/sdm_setup6
                          INTO ms_function_attributes
                          WHERE sdm_object = me->mv_object_type
                            AND type       = me->mv_type
                            AND active     = abap_true.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_fdt_input
              EXPORTING
                previous   = lx_fdt
                mt_message = lx_fdt->mt_message.
          ENDIF.

        CATCH cx_fdt INTO lx_fdt.
          CLEAR:
           ls_message.
          ls_message-msgid  = '/GDA/SDM1'.
          ls_message-msgty  = 'E'.
          ls_message-msgno  = '601'.
          ls_message-msgv1  = TEXT-001.
          ls_message-msgv2  = TEXT-002.
          ls_message-text   = TEXT-003.
          ls_message-source = TEXT-004.
          APPEND ls_message TO lx_fdt->mt_message[].
      ENDTRY.

    ELSE.
      READ TABLE mt_function_attributes INTO ms_function_attributes WITH KEY type = me->mv_type.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_fdt_input
          EXPORTING
            previous   = lx_fdt
            mt_message = lx_fdt->mt_message.
      ENDIF.

    ENDIF.
* Get application ID
    IF mv_id IS INITIAL.
      SELECT SINGLE id FROM fdt_admn_0000
                      INTO mv_id "lv_app_id
                        WHERE object_type = 'AP'
                        AND   name        = ms_function_attributes-application
                        AND   deleted     = space.

    ENDIF.
    me->mv_include = ms_function_attributes-include.

*/ Create BRF Exception Object Utility Object
*    IF mo_sdm_exceptions IS NOT BOUND AND me->mv_stats IS NOT INITIAL AND me->mv_no_brf = abap_false.
*      CREATE OBJECT mo_sdm_exceptions
*        EXPORTING
*          iv_object_type = me->mv_object_type
*          iv_period      = sy-datum
*          iv_del_sign    = 'EQ'. "Equal
*    ENDIF.

**// First check if BRF Function is active
    IF me->mv_no_brf = abap_true.
      me->mv_active = abap_true.
    ELSE.
      IF ms_function_attributes-source = '1'.
        lv_method = 'CHECK_FUNCTION_BRF'.
      ELSEIF ms_function_attributes-source = '2'.
        lv_method = 'CHECK_FUNCTION_CLASS'.
      ENDIF.
      IF me->mo_mapping IS INITIAL.

        IF me->mv_object_id IS INITIAL.
          SELECT SINGLE sdm_object_id FROM /gda/sdm_obj
            INTO mv_object_id
            WHERE sdm_object = mv_object_type.
        ENDIF.
        TRY.
            CALL METHOD /gda/sdm_cl_brf_mapping=>check_function
              EXPORTING
                iv_object  = me->mv_object_id
                iv_module  = me->mv_module
              IMPORTING
                ev_result  = lv_result
                ev_message = lv_message.
          CATCH /gda/cx_sdm_exception_handl .

            CLEAR:
             ls_message.
            ls_message-msgid  = '/GDA/SDM1'.
            ls_message-msgty  = 'E'.
            ls_message-msgno  = '601'.
            ls_message-msgv1  = 'Class:Method'.
            ls_message-msgv2  = '/GDA/SDM_CL_CORE->CONTRUCTOR'.
            ls_message-text   = lv_message.
            ls_message-source = ''.
            APPEND ls_message TO me->mt_message[].
        ENDTRY.


        TRY .
            CALL METHOD /gda/sdm_cl_brf_mapping=>(lv_method)
              EXPORTING
                iv_object  = me->mv_object_type
                iv_include = me->mv_include
                iv_source  = me->mv_source
              IMPORTING
                ev_active  = me->mv_active
                ev_guid    = me->mv_guid.

          CATCH cx_fdt_input  INTO lx_fdt.
            lv_message = lx_fdt->get_text( ).

            CLEAR:
             ls_message.
            ls_message-msgid  = '/GDA/SDM1'.
            ls_message-msgty  = 'E'.
            ls_message-msgno  = '601'.
            ls_message-msgv1  = 'Class:Method'.
            ls_message-msgv2  = '/GDA/SDM_CL_CORE->CONTRUCTOR'.
            ls_message-text   = lv_message.
            ls_message-source = ''.
            APPEND ls_message TO me->mt_message[].

          CATCH cx_fdt INTO lx_fdt.
*        RAISE EXCEPTION TYPE cx_fdt_input
*          EXPORTING
*            previous   = lx_fdt
*            mt_message = lx_fdt->mt_message.
          CATCH cx_sy_no_handler INTO lx_root.
            lv_message = lx_root->get_text( ).

            CLEAR:
             ls_message.
            ls_message-msgid  = '/GDA/SDM1'.
            ls_message-msgty  = 'E'.
            ls_message-msgno  = '601'.
            ls_message-msgv1  = 'Class:Method'.
            ls_message-msgv2  = '/GDA/SDM_CL_CORE->CONTRUCTOR'.
            ls_message-text   = lv_message.
            ls_message-source = ''.
            APPEND ls_message TO me->mt_message[].

        ENDTRY.
      ELSE.
        me->mv_active = me->mo_mapping->mv_active.
        me->mv_guid   = me->mo_mapping->mv_guid.
      ENDIF.
    ENDIF.
    TRY.
        IF ( me->mv_guid IS INITIAL OR me->mv_active = abap_false ) AND me->mv_active = abap_false.
          IF lx_fdt IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_fdt_input
              EXPORTING
                previous   = lx_fdt
                mt_message = lx_fdt->mt_message.
          ELSE.
            RAISE EXCEPTION TYPE cx_fdt_input.
          ENDIF.
        ENDIF.
      CATCH cx_fdt INTO lx_fdt.
        REPLACE '&1' WITH iv_object_type INTO me->mv_message.
        REPLACE '&2' WITH me->mv_include INTO me->mv_message.

        CLEAR:
         ls_message.
        ls_message-msgid = '/GDA/SDM1'.
        ls_message-msgty = 'E'.
        ls_message-msgno = '600'.
        ls_message-msgv1 = 'Class:Method'.
        ls_message-msgv2 = '/GDA/SDM_CL_CORE->CONTRUCTOR'.
        ls_message-text   = me->mv_message.
        ls_message-source = '/GDA/SDM_CL_CORE->CONTRUCTOR'.
        APPEND ls_message TO lx_fdt->mt_message[].

    ENDTRY.

    IF me->mv_timestamp IS INITIAL.
      GET TIME STAMP FIELD me->mv_timestamp.
    ENDIF.

    IF mt_brf_attributes IS INITIAL.
      TRY.
          SELECT * FROM /gda/sdm_setup1 INTO CORRESPONDING FIELDS OF TABLE mt_brf_attributes
            WHERE sdm_object = iv_object_type
             AND  active     = abap_true
             AND  type       = iv_type.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_fdt_input.
*            EXPORTING
*              previous   = lx_fdt
*              mt_message = lx_fdt->mt_message.

          ENDIF.

* Retrieve GUIDs...
          LOOP AT mt_brf_attributes ASSIGNING <brf_attributes>.
            CLEAR:
             <brf_attributes>-data_object.

            SELECT SINGLE id FROM fdt_admn_0000
                            INTO <brf_attributes>-data_object
                              WHERE object_type    = 'DO'
                              AND   name           = <brf_attributes>-name
                              AND   application_id = mv_id
                              AND   deleted        = space.
          ENDLOOP.


        CATCH cx_fdt INTO lx_fdt.
          CLEAR:
           ls_message.
          ls_message-msgid  = '/GDA/SDM1'.
          ls_message-msgty  = 'E'.
          ls_message-msgno  = '601'.
          ls_message-msgv1  = 'Config Table'.
          ls_message-msgv2  = '/GDA/SDM_SETUP'.
          ls_message-text   = 'SDM Config table not maintained correctly'.
          ls_message-source = '/GDA/SDM_SETUP'.
          APPEND ls_message TO me->mt_message[].
      ENDTRY.
    ENDIF.

    mv_source = iv_source.

*    IF lx_fdt IS NOT INITIAL.
*      me->mt_message[] = lx_fdt->mt_message[].
*    ENDIF.
  ENDMETHOD.


  METHOD create_brf_result_structure.
    DATA:
     lr_data_empty TYPE REF TO data.
    cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = me->mv_guid
                                                                  iv_data_object      = '_V_RESULT'
                                                                  iv_timestamp        = me->mv_timestamp
                                                                  iv_trace_generation = abap_false
                                                        IMPORTING er_data             = lr_data_empty ).


    r_result_structure = lr_data_empty.

  ENDMETHOD.


  METHOD display_message.

    FIELD-SYMBOLS:
      <message> LIKE LINE OF mt_message.

    IF me->mt_message[] IS INITIAL.
      EXIT.
    ENDIF.

    READ TABLE me->mt_message[] ASSIGNING <message> INDEX 1.
    CHECK sy-subrc = 0.
    MESSAGE w007(/gda/sdm_core) WITH <message>-msgv1.

  ENDMETHOD.


  METHOD display_messages.

    DATA:
      lo_alv       TYPE REF TO cl_salv_table,
      lr_functions TYPE REF TO cl_salv_functions_list,
      lr_layout    TYPE REF TO cl_salv_layout,
      key          TYPE salv_s_layout_key,
      lv_variant   TYPE slis_vari.

    IF me->mt_message[] IS INITIAL.
      EXIT.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = me->mt_message[] ).

      CATCH cx_salv_msg.
    ENDTRY.


    lr_layout = lo_alv->get_layout( ).
    key-report = sy-repid.
    lr_layout->set_key( key ).

    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   set initial Layout
    lv_variant = '/DEFAULT'.
    lr_layout->set_initial_layout( lv_variant ).

    lr_functions = lo_alv->get_functions( ).
    lr_functions->set_all( 'X' ).

    IF lo_alv IS BOUND.
*    IF i_popup = 'X'.
      lo_alv->set_screen_popup(
        start_column = '5' "i_start_column
        end_column  = '175' "i_end_column
        start_line  = '6' "i_start_line
        end_line    = '20'). "i_end_line ).
*    ENDIF.

      lo_alv->display( ).
    ENDIF.
  ENDMETHOD.


  METHOD factory.
    DATA:
      lo_object TYPE REF TO object,
      lx_fdt    TYPE REF TO cx_fdt,
      lx_root   TYPE REF TO cx_root,
      lv_class  TYPE classname.

    CHECK mv_client = sy-mandt.
    SELECT SINGLE class FROM /gda/sdm_setup6
                        INTO lv_class
                       WHERE sdm_object = iv_object_type
                        AND  type       = iv_type
                        AND active      = abap_true.

    TRY.
        CREATE OBJECT lo_object TYPE (lv_class)
              EXPORTING
            iv_object_type = iv_object_type
            iv_source      = iv_source
            iv_type        = iv_type
            iv_stats       = iv_stats
            IV_STATS_BRF   = IV_STATS_BRF
            iv_mapping     = iv_mapping.
      CATCH cx_fdt_input INTO lx_fdt.

        RAISE EXCEPTION TYPE cx_fdt_input
          EXPORTING
            previous   = lx_fdt
            mt_message = lx_fdt->mt_message.

      CATCH cx_root INTO lx_root.
    ENDTRY.

    ro_object = lo_object.
  ENDMETHOD.


  METHOD get_object_attributes.
    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF me->mt_brf_attributes.

    IF iv_type = space.
      ro_attributes =  me->mt_brf_attributes_instance.
    ELSE.
      LOOP AT me->mt_brf_attributes_instance ASSIGNING <brf_attributes> WHERE type = iv_type.
        APPEND <brf_attributes> TO ro_attributes.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  method GET_OBJECT_DESCRIPTION.
    r_desc = me->mv_object_id_desc.
  endmethod.


  method GET_OBJECT_KEYS.
  endmethod.


  method GET_OBJECT_TYPE.
    r_object_type = mv_object_type.
  endmethod.


  method GET_PRIMARY_OBJECT.
  endmethod.


  METHOD get_related_actions.

    DATA:
      lt_exact  TYPE STANDARD TABLE OF /gda/sdm_cact,
      lo_action TYPE REF TO /gda/sdm_cl_actions.

    FIELD-SYMBOLS:
       <exact> LIKE LINE OF lt_exact.

* Load up each action based upon the configuration
    SELECT *
      FROM /gda/sdm_cact
      INTO TABLE lt_exact
      WHERE OBJECT_TYPE = MV_OBJECT_TYPE.
*      ORDER BY sequence.
*
** Loop through each action to try to find if it matches
    LOOP AT lt_exact ASSIGNING <exact>.
*
*    CLEAR :
*          lv_actid.
*
*    IF pv_raw_data-class_id CP <exact>-class AND
*       pv_raw_data-subclass_id CP <exact>-subclass AND
*       pv_raw_data-status_id   CP <exact>-status.
*
*      LOOP AT  lt_profex INTO ls_profex WHERE     ( class CP pv_raw_data-class_id OR class EQ '*' )
*                                             AND  (  subclass CP  pv_raw_data-subclass_id  OR subclass EQ '*' )
*                                             AND  ( status  CP  pv_raw_data-status_id OR status  EQ '*' )
*                                             AND  (  actionid    CP <exact>-actionid OR actionid EQ '*' ).
*      ENDLOOP.
*      IF sy-subrc = 0.
*        CONTINUE.
*      ENDIF.
*
      CALL METHOD /gda/sdm_cl_actions=>get_object
        EXPORTING
          x_sdm_item = me
          x_actionid = <exact>-actionid
        RECEIVING
          r_object   = lo_action.
*
**     Add to returned list
*      IF NOT lo_action IS INITIAL.
*        lv_actid = lo_action->get_actionid( ).
*        IF NOT lv_actid IS INITIAL.
** Is this Alert item activated for this user?
*          select single profile into lv_profile
*                   from YCAIR_PROFILE
*                   where AI_USER = sy-uname
*                   and ACTIONID = lv_actid.
*
*          CALL METHOD lo_action->set_active_flag( lv_profile ) .
*
          APPEND lo_action TO r_actions.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
    ENDLOOP.
*
*  READ TABLE r_actions ASSIGNING <action> INDEX 1.
*  IF sy-subrc = 0.
*    <action>->set_group( 'REL' ).
*  ENDIF.


  ENDMETHOD.


  METHOD is_active.
    IF me->mv_guid EQ space OR me->mv_active = abap_false.
      r_active = abap_false.
    ELSE.
      r_active = abap_true.
    ENDIF.

  ENDMETHOD.


  method LOAD_RELATED_ACTIONS.

   DATA:
    lt_actions TYPE /GDA/SDM_TT_OBJ_ACTION.

*  IF pv_raw_data-t_related_actions[] IS INITIAL.
*    IF get_wr_hist_flag( ) IS INITIAL.
      lt_actions = me->get_related_actions( ).
*      APPEND LINES OF lt_actions TO pv_raw_data-t_related_actions[].
      APPEND LINES OF lt_actions TO r_actions[].
      CLEAR lt_actions[].
*      lt_actions = me->get_dynamic_actions( ).
*      APPEND LINES OF lt_actions TO pv_raw_data-t_related_actions[].
*      CLEAR lt_actions[].
*      lt_actions = me->get_static_actions( ).
*      APPEND LINES OF lt_actions TO pv_raw_data-t_related_actions[].
*      CLEAR lt_actions[].
*    ELSE.
*      lt_actions = me->get_historic_actions( ).
*      APPEND LINES OF lt_actions TO pv_raw_data-t_related_actions[].
*    ENDIF.
*  ENDIF.
*  r_actions[] = pv_raw_data-t_related_actions[].

  endmethod.


  METHOD main.
    DATA:
      lx_exc_report_util TYPE REF TO /gda/cx_sdm_exception_handl,
      lx_root            TYPE REF TO cx_root,
      lx_fdt             TYPE REF TO cx_fdt,
      lv_method          TYPE string.

    CASE me->ms_function_attributes-source.
      WHEN '1'.
        lv_method = 'CALL_BRF'.
      WHEN '2'.
        lv_method = 'CALL_CLASS'.
      WHEN OTHERS.
* raise exception
    ENDCASE.
    TRY.
        CALL METHOD me->(lv_method).

      CATCH /gda/cx_sdm_exception_handl INTO lx_exc_report_util.
*        gv_message = lx_exc_report_util->mv_text.
      CATCH cx_sy_no_handler INTO lx_root.

      CATCH cx_fdt_input INTO lx_fdt.
        RAISE EXCEPTION TYPE cx_fdt_input
          EXPORTING
            previous   = lx_fdt
            mt_message = lx_fdt->mt_message.
    ENDTRY.

    IF me->mv_stats = abap_true.
      TRY.
          me->record_statistics( ).
        CATCH /gda/cx_sdm_exception_handl INTO lx_exc_report_util.
*        gv_message = lx_exc_report_util->mv_text.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD record_statistics.


    DATA:
      lo_data_model TYPE REF TO /gda/cl_sdm_data_model_main,
      lv_id         TYPE symsgid,
      lv_number     TYPE symsgno,
      lv_type       TYPE symsgty,
      lv_sdm_key    TYPE cdtabkey,
      lv_field      TYPE /gda/sdm_de_fieldname, "field,
      lv_table      TYPE /gda/sdm_de_tabname,
      lv_extra_v2   TYPE symsgv,
      lv_extra_v3   TYPE symsgv,
      lv_extra_v4   TYPE symsgv,
      lv_extra_v5   TYPE symsgv,
      lv_extra_v6   TYPE symsgv.

    FIELD-SYMBOLS:
      <results>  TYPE STANDARD TABLE,
      <result>   TYPE any,
      <id>       TYPE any,
      <number>   TYPE any,
      <type>     TYPE any,
      <field>    TYPE any,
      <extra_v2> TYPE any,
      <extra_v3> TYPE any,
      <extra_v4> TYPE any,
      <extra_v5> TYPE any,
      <extra_v6> TYPE any,
      <sdm_key>  TYPE any.

*      <key_fields> LIKE LINE OF lt_key_fields,
*      <tables>     LIKE LINE OF lt_key_tables.

*    DATA: lv_method TYPE string.
*
*    IF me->mv_version = 'V2.0'.
*      lv_method = 'record_statistics_v2'.
*    ELSE.
*      lv_method = 'record_statistics'.
*    ENDIF.
*
*    TRY.
*        CALL METHOD me->mo_sdm_exceptions->(lv_method)
*          EXPORTING
*            iv_commit = abap_true.
*      CATCH /gda/cx_sdm_exception_handl .
*    ENDTRY.

*    BREAK rroelofse.

    IF me->mv_stats IS NOT INITIAL AND me->mo_brf_result IS NOT INITIAL. "me->mv_no_brf = abap_false.

      ASSIGN me->mo_brf_result->* TO <results>.
*   BREAK rroelofse.
      SORT <results>.
      DELETE ADJACENT DUPLICATES FROM <results>.

      LOOP AT <results> ASSIGNING <result>.

        ASSIGN COMPONENT 'SDM_TABKEY'    OF STRUCTURE <result> TO <sdm_key>.
        CHECK <sdm_key> NE space.
        ASSIGN COMPONENT 'ID'         OF STRUCTURE <result> TO <id>.
        ASSIGN COMPONENT 'NUMBER'     OF STRUCTURE <result> TO <number>.

        ASSIGN COMPONENT 'TYPE'     OF STRUCTURE <result> TO <type>.
        ASSIGN COMPONENT 'EXTRA_V1' OF STRUCTURE <result> TO <field>.
        IF <field> IS ASSIGNED.
          SPLIT <field> AT '-' INTO lv_table lv_field.
        ENDIF.

        ASSIGN COMPONENT 'EXTRA_V2' OF STRUCTURE <result> TO <extra_v2>.
        ASSIGN COMPONENT 'EXTRA_V3' OF STRUCTURE <result> TO <extra_v3>.
        ASSIGN COMPONENT 'EXTRA_V4' OF STRUCTURE <result> TO <extra_v4>.
        ASSIGN COMPONENT 'EXTRA_V5' OF STRUCTURE <result> TO <extra_v5>.
        ASSIGN COMPONENT 'EXTRA_V6' OF STRUCTURE <result> TO <extra_v6>.


        lv_sdm_key  = <sdm_key>.
        lv_id       = <id>.
        lv_number   = <number>.
        lv_type     = <type>.
        lv_extra_v2 = <extra_v2>.
        lv_extra_v3 = <extra_v3>.
        lv_extra_v4 = <extra_v4>.
        lv_extra_v5 = <extra_v5>.
        lv_extra_v6 = <extra_v6>.


        CREATE OBJECT lo_data_model
          EXPORTING
            i_sdm_object_id = me->mv_object_id
            i_sdm_type      = me->mv_type
            i_sdm_tabkey    = lv_sdm_key
            i_tabname       = lv_table
            i_field         = lv_field
            i_msg_id        = lv_id
            i_msg_number    = lv_number
            i_begda         = sy-datum.


        CALL METHOD lo_data_model->set_msg_type( i_msg_type = lv_type ).

        CALL METHOD lo_data_model->set_extra_v2( i_extra_v2 = lv_extra_v2 ).
        CALL METHOD lo_data_model->set_extra_v3( i_extra_v3 = lv_extra_v3 ).
        CALL METHOD lo_data_model->set_extra_v4( i_extra_v4 = lv_extra_v4 ).
        CALL METHOD lo_data_model->set_extra_v5( i_extra_v5 = lv_extra_v5 ).
        CALL METHOD lo_data_model->set_extra_v6( i_extra_v6 = lv_extra_v6 ).

        CALL METHOD lo_data_model->process( i_commit = abap_false ).
      ENDLOOP.

      COMMIT WORK.

    ENDIF.

  ENDMETHOD.


  method RETURN_BRF_RESULT.
    R_RESULT_STRUCTURE = MO_BRF_RESULT.
  endmethod.


  METHOD return_brf_result_structure.
    r_result_structure = mo_brf_result_empty.
  ENDMETHOD.


  method RETURN_MESSAGE.
       rv_message = me->mv_message.
  endmethod.


  METHOD set_license.


************************************************************************
*  This method updates the license table and is called by the
*  /GDA/SDM_LICENSE PROGRAM.
************************************************************************
*  ABAPer: Charles de Jager
************************************************************************
*  VERSION 1.0
*     Initial release of the license update module
************************************************************************


************************************************************************
*
*                            SCALAR DATA
*
************************************************************************

    DATA:
      it_license_tab TYPE likey_license_tab,
      lv_license     LIKE LINE OF it_license_tab,
      lv_system_no   TYPE likey_system_no,
      lv_sys_no_str  TYPE string,
      lv_string      TYPE string,
      lv_xstring     TYPE xstring,
      it_data_tab    TYPE TABLE OF string,
      it_lic_tab     TYPE TABLE OF string,
      lv_lic         TYPE string,
      lv_dat         TYPE dats,
      lv_dat2        TYPE dats,
      lv_result      TYPE boolean,
      it_sdm_client  TYPE TABLE OF /gda/sdm_client,
      lv_sdm_client  LIKE LINE OF it_sdm_client,
      it_sdm_lic     TYPE TABLE OF /gda/sdm_lic,
      lv_sdm_lic     LIKE LINE OF it_sdm_lic.

************************************************************************
*
*                             REFERENCES
*
************************************************************************
    DATA:
          clhwe          TYPE REF TO cl_hard_wired_encryptor.


    CREATE OBJECT clhwe.


***********************************************************
*  GET THE CURRENT SYSTEM NO FROM THE LICENSE KEYS TABLE
***********************************************************
    CALL FUNCTION 'SLIC_LIKEY_GET_SYSTEM_NO'
      IMPORTING
        systemno = lv_system_no
*       ERROR_MESSAGES             =
* EXCEPTIONS
*       NO_SYSTEMNO_ASSIGNED       = 1
*       ERROR    = 2
*       OTHERS   = 3
      .



*    CALL FUNCTION 'SLIC_LIKEY_GET_ALL_LICENSES'
*      IMPORTING
*        licenses = it_license_tab.
*
*    LOOP AT it_license_tab INTO lv_license.
*      IF lv_license-validity = 4 OR lv_license-begin_date >= sy-datum AND lv_license-end_date <= 99991231.
*        lv_system_no = lv_license-system_no.
*      ENDIF.
*    ENDLOOP.
*
    lv_sys_no_str = lv_system_no.

    IF lv_sys_no_str = ''.
      message = '009'.
      msgv1 = sy-subrc.
      result = '-'.
      RETURN.
    ENDIF.


***********************************************************
*  DECRYPT THE LICENSE KEY
***********************************************************
    TRY.
        CALL METHOD clhwe->base64_decode
          EXPORTING
            the_string = license
          RECEIVING
            result     = lv_xstring.

      CATCH cx_encrypt_error.

    ENDTRY.

    CALL METHOD cl_abap_conv_in_ce=>create
      EXPORTING
        input = lv_xstring
      RECEIVING
        conv  = DATA(lv_conv).

    CALL FUNCTION 'CRM_IC_XML_XSTRING2STRING'
      EXPORTING
        inxstring = lv_xstring
      IMPORTING
        outstring = lv_string.

    REPLACE '<?xml version="1.0"?>' WITH '' INTO lv_string.

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE it_data_tab.

    LOOP AT it_data_tab INTO lv_string.
      CONDENSE lv_string.

      IF sy-tabix = 1.  " GLUEDATA SDM LICENSE
        IF lv_string <> 'GLUEDATA SDM LICENSE'.
          message = '001'.
          result = '-'.
          RETURN.
        ENDIF.
      ENDIF.

      IF sy-tabix = 2.  " system number
        IF lv_string <> lv_sys_no_str.
          message = '002'.
          msgv1 = lv_sys_no_str.
          msgv2 = lv_string.
          result = '-'.
          RETURN.
        ENDIF.
      ENDIF.

      IF sy-tabix = 3.  " license date has expired
        lv_dat = lv_string.
        lv_dat2 = sy-datum.
        lv_dat2 = lv_dat2 - 14.
        IF lv_dat < lv_dat2.
          message = '005'.
          msgv1 = lv_dat2.
          result = '-'.
          RETURN.
        ENDIF.
      ENDIF.

      IF sy-tabix >= 4.

        SPLIT lv_string AT '|' INTO TABLE it_lic_tab.

        LOOP AT it_lic_tab INTO lv_lic.
          IF sy-tabix = 1.
            lv_sdm_lic-sdm_object_id = lv_lic.
          ENDIF.

          IF sy-tabix = 2.
            lv_sdm_lic-sdm_module = lv_lic.
          ENDIF.

          IF sy-tabix = 3.
            lv_sdm_lic-start_date = lv_lic.
          ENDIF.

          IF sy-tabix = 4.
            lv_sdm_lic-end_date = lv_lic.
          ENDIF.
        ENDLOOP.

        lv_sdm_lic-mandt = sy-mandt.
*        lv_sdm_lic-status = 'X'.

*        UPDATE /gda/sdm_lic USING CLIENT @lv_sdm_client-client FROM @lv_sdm_lic.
*        MODIFY /gda/sdm_lic USING CLIENT @lv_sdm_client-client FROM @lv_sdm_lic.


        MODIFY /gda/sdm_lic FROM lv_sdm_lic.
        IF sy-subrc <> 0.
          message = '006'.
          msgv1 = sy-subrc.
          result = '-'.
          RETURN.
        ENDIF.

      ENDIF.

    ENDLOOP.

    result = 'X'.
    message = 'The license was updated successfully'.
    RETURN.

  ENDMETHOD.


  METHOD set_selection.
    DATA:
     lv_message TYPE string.

    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF mt_brf_attributes_instance,
      <ref>            TYPE any.

    TRY.
        IF iv_type IS INITIAL.
          READ TABLE me->mt_brf_attributes_instance ASSIGNING <brf_attributes> WITH KEY name = iv_name.
        ELSE.
          READ TABLE me->mt_brf_attributes_instance ASSIGNING <brf_attributes> WITH KEY name = iv_name
                                                                               type = iv_type.
        ENDIF.
        IF sy-subrc <> 0.
*          RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
*            EXPORTING
*              mv_text = lv_message.
        ELSE.
          ASSIGN <brf_attributes>-lr_data->* TO <ref>.
          <ref> = iv_data.

        ENDIF.
      CATCH /gda/cx_sdm_exception_handl.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
