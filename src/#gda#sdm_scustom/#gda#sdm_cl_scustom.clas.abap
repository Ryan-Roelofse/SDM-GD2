class /GDA/SDM_CL_SCUSTOM definition
  public
  inheriting from /GDA/SDM_CL_CORE
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !IV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT
      !IV_SOURCE type /GDA/SDM_DE_SOURCE
      !IV_TYPE type /GDA/SDM_DE_TYPE
      !IV_STATS type BOOLEAN
      !IV_STATS_BRF type BOOLEAN optional
      !IV_ERRORS_ONLY type BOOLEAN optional
      !IV_MAPPING type ref to /GDA/SDM_CL_BRF_MAPPING optional
    raising
      CX_FDT_INPUT .

  methods /GDA/SDM_IF_ACTION_PROCESSOR~PROCESS_ACTION
    redefinition .
  methods GET_OBJECT_KEYS
    redefinition .
  methods GET_PRIMARY_OBJECT
    redefinition .
protected section.

  methods CALL_BRF
    redefinition .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_SCUSTOM IMPLEMENTATION.


  METHOD /gda/sdm_if_action_processor~process_action.
    DATA:
      lv_fcode      TYPE          sy-ucomm.

    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF me->mt_brf_attributes,
      <ref>            TYPE STANDARD TABLE,
      <line>           TYPE any,
      <id>             TYPE any.

    CALL METHOD super->/gda/sdm_if_action_processor~process_action
      EXPORTING
        x_action = x_action
        x_multi  = x_multi
*  IMPORTING
*       y_refresh        =
*       y_action_handled =
*       y_not_authorised =
      .

* Handle common actions to SDM
    lv_fcode = x_action->get_fcode( ).

* Check for common actions
    CASE lv_fcode.
      WHEN 'DISP'.
        READ TABLE me->mt_brf_attributes_instance ASSIGNING <brf_attributes> WITH KEY name = '/GDA/SDM_T_SCUSTOM'
                                                                             type = me->mv_type.
        ASSIGN <brf_attributes>-lr_data->* TO <ref>.
        read table <ref> ASSIGNING <line> index 1.
        ASSIGN COMPONENT 'ID' OF STRUCTURE <line> TO <id>.
        SET PARAMETER ID: 'CSM' FIELD <id>.
        CALL TRANSACTION 'BC_GLOBAL_SCUST_DISP' AND SKIP FIRST SCREEN.
      WHEN OTHERS.


    ENDCASE.

  ENDMETHOD.


  METHOD call_brf.

    DATA:
      lt_sdm_gui_val TYPE STANDARD TABLE OF /gda/sdm_s_scust_val_res_gui,
      ls_sdm_gui_val TYPE /gda/sdm_s_scust_val_res_gui,
      lt_sdm_gui_der TYPE STANDARD TABLE OF /gda/sdm_s_scust_der_res_gui,
      ls_sdm_gui_der TYPE /gda/sdm_s_scust_der_res_gui,
      lo_data        TYPE REF TO data,
      lo_data_empty  TYPE REF TO data,
      lv_message     TYPE string.

    FIELD-SYMBOLS:
      <table>           TYPE STANDARD TABLE,
      <results>         TYPE STANDARD TABLE,
      <result>          TYPE any,
      <customers>       TYPE /gda/sdm_t_scustom,
      <customer>        TYPE /gda/sdm_s_scustom,
      <data_attributes> LIKE LINE OF me->mt_brf_attributes_instance,
      <val_results>     LIKE LINE OF me->mt_validation_results.

    TRY.
        CALL METHOD super->call_brf.
      CATCH /gda/cx_sdm_exception_handl.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
      CATCH cx_fdt_input.
        RAISE EXCEPTION TYPE cx_fdt_input.
    ENDTRY.

    IF me->mv_source = me->mc_rep or me->mv_source = me->mc_poe.

      IF me->mv_type = me->mc_validation.
        CREATE DATA lo_data       LIKE lt_sdm_gui_val.
        CREATE DATA lo_data_empty LIKE lt_sdm_gui_val.
      ELSEIF me->mv_type = me->mc_derivation.
        CREATE DATA lo_data       LIKE lt_sdm_gui_der.
        CREATE DATA lo_data_empty LIKE lt_sdm_gui_der.
      ELSE.
        CREATE DATA lo_data       LIKE lt_sdm_gui_val.
        CREATE DATA lo_data_empty LIKE lt_sdm_gui_val.
      ENDIF.

      ASSIGN lo_data->*           TO <table>.

* Derivation
      IF me->mv_type = me->mc_derivation.
* For now there is no need to populate the key field and description for derivations
        ASSIGN me->mo_brf_result->* TO <results>.

        LOOP AT <results> ASSIGNING <result>.
          MOVE-CORRESPONDING <result> TO ls_sdm_gui_der.
          COLLECT ls_sdm_gui_der INTO <table>.
          CLEAR:
           ls_sdm_gui_der.
        ENDLOOP.
* Validations and others
      ELSE.

        READ TABLE me->mt_brf_attributes_instance ASSIGNING <data_attributes> WITH KEY name = '/GDA/SDM_T_SCUSTOM'
                                                                                       type = me->mv_type.
        IF sy-subrc = 0.
          ASSIGN <data_attributes>-lr_data->* TO <customers>.
          IF <customers> IS ASSIGNED.
            READ TABLE <customers> ASSIGNING <customer> INDEX 1.
          ENDIF.
        ENDIF.

        ASSIGN me->mo_brf_result->* TO <results>.

        SORT <results>.
        LOOP AT <results> ASSIGNING <val_results>.
          IF <val_results>-id = space.
            CONTINUE.
          ENDIF.
          IF me->mv_errors_only = abap_true.
            IF <val_results>-type NA 'EAX'.
              CONTINUE.
            ENDIF.
          ENDIF.

          MESSAGE ID <val_results>-id
                TYPE <val_results>-type
              NUMBER <val_results>-number
                WITH <val_results>-message_v1 <val_results>-message_v2
                     <val_results>-message_v3 <val_results>-message_v4
                INTO lv_message.

* Populate key additional for SCUSTOM...
*CUSTOMER
*NAME
          IF <customer> IS ASSIGNED.
            ls_sdm_gui_val-customer = <customer>-id.
            ls_sdm_gui_val-name     = <customer>-name.
          ENDIF.

          ls_sdm_gui_val-message        = lv_message.
          ls_sdm_gui_val-id             = <val_results>-id.
          ls_sdm_gui_val-number         = <val_results>-number.
          ls_sdm_gui_val-type           = <val_results>-type.
          ls_sdm_gui_val-extra_v1       = <val_results>-extra_v1.
          ls_sdm_gui_val-extra_v2       = <val_results>-extra_v2.
          ls_sdm_gui_val-extra_v3       = <val_results>-extra_v3.
          ls_sdm_gui_val-extra_v4       = <val_results>-extra_v4.
          ls_sdm_gui_val-extra_v5       = <val_results>-extra_v5.
          ls_sdm_gui_val-extra_v6       = <val_results>-extra_v6.
          ls_sdm_gui_val-sdm_tabkey     = <val_results>-sdm_tabkey.

          COLLECT ls_sdm_gui_val INTO <table>.
          CLEAR
            ls_sdm_gui_val.

        ENDLOOP.
      ENDIF.

      mo_brf_result       = lo_data.
      mo_brf_result_empty = lo_data_empty.

    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
    DATA:
      ms_function_attributes TYPE /gda/sdm_setup6.

    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF mt_brf_attributes.

    SELECT * FROM /gda/sdm_setup6
                    INTO TABLE mt_function_attributes
                    WHERE sdm_object = 'SCUSTOM'
                      AND active     = abap_true.

*    TRY.
    SELECT * FROM /gda/sdm_setup1 INTO CORRESPONDING FIELDS OF TABLE mt_brf_attributes
      WHERE sdm_object = 'SCUSTOM'
       AND  active     = abap_true.
*           AND  type       = iv_type.

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

      READ TABLE mt_function_attributes INTO ms_function_attributes WITH KEY sdm_object = 'SCUSTOM'
                                                                             type       = <brf_attributes>-type.
      CHECK sy-subrc = 0.

      SELECT SINGLE id FROM fdt_admn_0000
                      INTO mv_id
                        WHERE object_type = 'AP'
                        AND   name        = ms_function_attributes-application
                        AND   deleted     = space.

      SELECT SINGLE id FROM fdt_admn_0000
                      INTO <brf_attributes>-data_object
                        WHERE object_type    = 'DO'
                        AND   name           = <brf_attributes>-name
                        AND   application_id = mv_id
                        AND   deleted        = space.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    DATA:
      wf_ref     TYPE REF TO data,
      lx_root    TYPE REF TO cx_root,
      lv_message TYPE string.

    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF mt_brf_attributes,
      <ref>            TYPE any.

    TRY.
        super->constructor( iv_object_type    = iv_object_type
                            iv_source         = iv_source
                            iv_type           = iv_type
                            iv_stats          = iv_stats
                            iv_stats_brf      = iv_stats_brf
                            iv_errors_only    = iv_errors_only
                            iv_mapping        = iv_mapping ).
      CLEANUP INTO lx_root.

        lv_message = lx_root->get_text( ).

    ENDTRY.

    LOOP AT mt_brf_attributes ASSIGNING <brf_attributes> WHERE type = me->mv_type.
      IF <brf_attributes>-ref_type IS INITIAL.
        <brf_attributes>-ref_type = '1'.
      ENDIF.
      IF <brf_attributes>-ref_type = '2' .
        CREATE DATA wf_ref TYPE STANDARD TABLE OF (<brf_attributes>-name). "Dynamic Structure
        ASSIGN wf_ref->* TO <ref>.
        GET REFERENCE OF <ref> INTO <brf_attributes>-lr_data.
      ELSEIF <brf_attributes>-ref_type = '1'.
        CREATE DATA wf_ref TYPE (<brf_attributes>-name). "Dynamic Structure
        ASSIGN wf_ref->* TO <ref>.
        GET REFERENCE OF <ref> INTO <brf_attributes>-lr_data.
      ENDIF.
      APPEND <brf_attributes> TO mt_brf_attributes_instance.
    ENDLOOP.

    me->mv_object_id_desc = 'SCUSTOM'.
  ENDMETHOD.


  METHOD get_object_keys.
*CALL METHOD SUPER->GET_OBJECT_KEYS
*  RECEIVING
*    R_KEY_FIELDS =
*    .

    DATA:
      ls_key_field TYPE fieldname.

    r_key_fields = super->get_object_keys( ).

    ls_key_field = 'KEY_ID'.
    APPEND ls_key_field TO r_key_fields.

  ENDMETHOD.


  METHOD get_primary_object.
    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF mt_brf_attributes,
      <key_structure>  TYPE any,
      <object_key>     TYPE any.

    READ TABLE mt_brf_attributes ASSIGNING <brf_attributes> WITH KEY type = mv_type
                                                                    name = '/GDA/SDM_T_SCUSTOM'.
    CHECK sy-subrc = 0.

    ASSIGN <brf_attributes>-lr_data->* TO <key_structure>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'ID' OF STRUCTURE <key_structure> TO <object_key>.
    CHECK sy-subrc = 0.
    r_object_main = <object_key>.

  ENDMETHOD.
ENDCLASS.
