class /GDA/SDM_CL_ARTICLE definition
  public
  inheriting from /GDA/SDM_CL_CORE
  final
  create public .

public section.

  types:
    BEGIN OF TY_REPORT_OUTPUT,
          MATNR          TYPE MATNR,
          MAKTX          TYPE MAKTX,
          MTART          TYPE MTART,
          MATKL          TYPE MATKL,
          MSTAE          TYPE MSTAE,
          MESSAGE        TYPE /GDA/SDM_DE_MESSAGE,
          MESSAGE_ID     TYPE SYMSGID,
          MESSAGE_NUMBER TYPE SYMSGNO,
          MESSAGE_TYPE   TYPE SYMSGTY,
          COUNT          TYPE /GDA/SDM_DE_COUNTER,
          EXTRA_V1       TYPE /GDA/SDM_EXTRA_INFORMATION,
          EXTRA_V2       TYPE /GDA/SDM_EXTRA_INFORMATION,
          EXTRA_V3       TYPE /GDA/SDM_EXTRA_INFORMATION,
          EXTRA_V4       TYPE /GDA/SDM_EXTRA_INFORMATION,
          EXTRA_V5       TYPE /GDA/SDM_EXTRA_INFORMATION,
          EXTRA_V6       TYPE /GDA/SDM_EXTRA_INFORMATION,
      END OF TY_REPORT_OUTPUT .
  types:
    TY_IT_REPORT_OUTPUT type table of TY_REPORT_OUTPUT .
  types:
    BEGIN OF TY_REPORT_OUTPUT_DER,
          MATNR           TYPE MATNR,
          TABLE           Type  TABLE_NAME,
          FIELD           Type  FIELD_NAME,
          VALUE           Type  TEXT200,
          SKIP_DERIVATION Type  XFELD,
          SCREEN_NAME     Type  TEXT132,
          GREY_OUT        Type  /GDA/SDM_DE_SC_GREY,
          HIDE            Type  /GDA/SDM_DE_SC_HIDE,
          REQUIRED        Type  /GDA/SDM_DE_SC_REQUIRED,
          BOLD            Type  /GDA/SDM_DE_SC_BOLD,
          GROUP1          Type  /GDA/SDM_DE_SC_GRP1,
          GROUP2          Type  /GDA/SDM_DE_SC_GRP2,
          GROUP3          Type  /GDA/SDM_DE_SC_GRP3,
          GROUP4          Type  /GDA/SDM_DE_SC_GRP4 ,
      END OF TY_REPORT_OUTPUT_DER .
  types:
    TY_IT_REPORT_OUTPUT_DER type table of TY_REPORT_OUTPUT_DER .

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
  class-methods CLASS_CONSTRUCTOR .

  methods /GDA/SDM_IF_ACTION_PROCESSOR~PROCESS_ACTION
    redefinition .
  methods CREATE_BRF_RESULT_STRUCTURE
    redefinition .
  methods GET_OBJECT_KEYS
    redefinition .
  methods GET_PRIMARY_OBJECT
    redefinition .
protected section.

  methods CALL_BRF
    redefinition .
  methods CALL_CLASS
    redefinition .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_ARTICLE IMPLEMENTATION.


  METHOD /gda/sdm_if_action_processor~process_action.
    CALL METHOD super->/gda/sdm_if_action_processor~process_action
      EXPORTING
        x_action = x_action
        x_multi  = x_multi
*  IMPORTING
*       y_refresh        =
*       y_action_handled =
*       y_not_authorised =
      .

    DATA:
      lv_fcode      TYPE          sy-ucomm.

    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF me->mt_brf_attributes,
      <ref>            TYPE any,
      <matnr>          TYPE any,
      <mstae>          TYPE any.

* Handle common actions to SDM
    lv_fcode = x_action->get_fcode( ).

* Check for common actions
    CASE lv_fcode.


      WHEN 'MM43'.
        READ TABLE me->mt_brf_attributes_instance ASSIGNING <brf_attributes> WITH KEY name = '/GDA/SDM_S_MARA'
                                                                             type = me->mv_type.
        ASSIGN <brf_attributes>-lr_data->* TO <ref>.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ref> TO <matnr>.

        SET PARAMETER ID: 'MAT' FIELD <matnr>.
*        SET PARAMETER ID 'MXX' FIELD 'K' .
        CALL TRANSACTION 'MM43' AND SKIP FIRST SCREEN.

      WHEN 'MM42'.
        READ TABLE me->mt_brf_attributes_instance ASSIGNING <brf_attributes> WITH KEY name = '/GDA/SDM_S_MARA'
                                                                             type = me->mv_type.
        ASSIGN <brf_attributes>-lr_data->* TO <ref>.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ref> TO <matnr>.

        SET PARAMETER ID: 'MAT' FIELD <matnr>.
        CALL TRANSACTION 'MM42' AND SKIP FIRST SCREEN.

      WHEN 'VKP5'.
        READ TABLE me->mt_brf_attributes_instance ASSIGNING <brf_attributes> WITH KEY name = '/GDA/SDM_S_MARA'
                                                                             type = me->mv_type.
        ASSIGN <brf_attributes>-lr_data->* TO <ref>.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ref> TO <matnr>.

        SET PARAMETER ID: 'MAT' FIELD <matnr>.
        CALL TRANSACTION 'VKP5'.
      WHEN '/GDA/SDM_STAT_CLEAR'.
        READ TABLE me->mt_brf_attributes_instance ASSIGNING <brf_attributes> WITH KEY name = '/GDA/SDM_S_MARA'
                                                                             type = me->mv_type.
        ASSIGN <brf_attributes>-lr_data->* TO <ref>.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ref> TO <matnr>.
        SET PARAMETER ID: 'MAT' FIELD <matnr>.

        ASSIGN COMPONENT 'MSTAE' OF STRUCTURE <ref> TO <mstae>.
        SET PARAMETER ID: 'MST' FIELD <mstae>.

        CALL TRANSACTION '/GDA/SDM_STAT_CLEAR' AND SKIP FIRST SCREEN.


      WHEN OTHERS.


    ENDCASE.

  ENDMETHOD.


  METHOD call_brf.

    DATA:
      lt_sdm_gui_out       TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key,
      ls_sdm_gui_out       TYPE /gda/sdm_s_val_results_key,
      lt_report_output_der TYPE STANDARD TABLE OF ty_report_output_der,
      ls_report_output_der TYPE ty_report_output_der,
      lv_message           TYPE string,
      ls_brf_exc_rep_line  TYPE /gda/sdm_exceptions_alv,
      er_data              TYPE REF TO data,
      er_data_empty        TYPE REF TO data.

    FIELD-SYMBOLS:
      <results>             TYPE STANDARD TABLE,
      <val_results>         LIKE LINE OF me->mt_validation_results,
      <table>               TYPE ANY TABLE,
      <any>                 TYPE any,
      <attributes_instance> LIKE LINE OF me->mt_brf_attributes_instance,
      <matnr>               TYPE any,
      <mara>                TYPE /gda/sdm_s_mara,
      <makt>                TYPE /gda/sdm_s_makt,
      <makt_tab>            TYPE /gda/sdm_t_makt.

    TRY.
        CALL METHOD super->call_brf.
      CATCH /gda/cx_sdm_exception_handl.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
      CATCH cx_fdt_input.
        RAISE EXCEPTION TYPE cx_fdt_input.
    ENDTRY.

* For Reporting - Return BRF results with Master Data
    IF me->mv_source = me->mc_rep.

      IF me->mv_type = me->mc_validation.
        CREATE DATA er_data       LIKE lt_sdm_gui_out.
        CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
      ELSEIF me->mv_type = me->mc_derivation.
        CREATE DATA er_data       LIKE lt_report_output_der.
        CREATE DATA er_data_empty LIKE lt_report_output_der.
      ELSE.
        CREATE DATA er_data       LIKE lt_sdm_gui_out.
        CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
      ENDIF.

      ASSIGN er_data->*           TO <table>.

* Derivation
      IF me->mv_type = me->mc_derivation.
        ASSIGN me->mo_brf_result->* TO <results>.

        LOOP AT <results> ASSIGNING <any>.
          MOVE-CORRESPONDING <any> TO ls_report_output_der.
          COLLECT ls_report_output_der INTO <table>.
          CLEAR ls_report_output_der.
        ENDLOOP.

* Validations and others
      ELSE.

        READ TABLE me->mt_brf_attributes_instance ASSIGNING <attributes_instance> WITH KEY name = '/GDA/SDM_S_MARA'
                                                                                            type = me->mv_type.
        IF sy-subrc = 0.
          ASSIGN <attributes_instance>-lr_data->* TO <mara>.
        ENDIF.

        READ TABLE me->mt_brf_attributes_instance ASSIGNING <attributes_instance> WITH KEY name = '/GDA/SDM_S_MAKT'
                                                                                            type = me->mv_type.
        IF sy-subrc = 0.
          ASSIGN <attributes_instance>-lr_data->* TO <makt>.
        ELSE.
          READ TABLE me->mt_brf_attributes_instance ASSIGNING <attributes_instance> WITH KEY name = '/GDA/SDM_T_MAKT'
                                                                                              type = me->mv_type.

          IF sy-subrc = 0.
            ASSIGN <attributes_instance>-lr_data->* TO  <makt_tab>.
            IF <makt_tab> IS ASSIGNED.
              READ TABLE <makt_tab> ASSIGNING <makt> WITH KEY spras = sy-langu.
            ENDIF.
          ENDIF.
        ENDIF.

        ASSIGN me->mo_brf_result->* TO <results>.

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

* Populate key fields for ARTICLE...
*MATNR
*MAKTX
*MTART
*MATKL
*MSTAE

          IF <mara> IS ASSIGNED.
            ls_sdm_gui_out-matnr = <mara>-matnr.
            ls_sdm_gui_out-mtart = <mara>-mtart.
            ls_sdm_gui_out-matkl = <mara>-matkl.
            ls_sdm_gui_out-mstae = <mara>-mstae.
          ENDIF.

          IF <makt> IS ASSIGNED.
            ls_sdm_gui_out-maktx = <makt>-maktx.
          ENDIF.

          ls_sdm_gui_out-message        = lv_message.
          ls_sdm_gui_out-id             = <val_results>-id.
          ls_sdm_gui_out-number         = <val_results>-number.
          ls_sdm_gui_out-type           = <val_results>-type.
          ls_sdm_gui_out-extra_v1       = <val_results>-extra_v1.
          ls_sdm_gui_out-extra_v2       = <val_results>-extra_v2.
          ls_sdm_gui_out-extra_v3       = <val_results>-extra_v3.
          ls_sdm_gui_out-extra_v4       = <val_results>-extra_v4.
          ls_sdm_gui_out-extra_v5       = <val_results>-extra_v5.
          ls_sdm_gui_out-extra_v6       = <val_results>-extra_v6.
          ls_sdm_gui_out-sdm_tabkey     = <val_results>-sdm_tabkey.

          COLLECT ls_sdm_gui_out INTO <table>.
          CLEAR ls_sdm_gui_out.

        ENDLOOP.
      ENDIF.

      mo_brf_result       = er_data.
      mo_brf_result_empty = er_data_empty.

    ENDIF.
  ENDMETHOD.


  METHOD call_class.

    DATA:
      lt_sdm_gui_out       TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key,
      ls_sdm_gui_out       TYPE /gda/sdm_s_val_results_key,
      lt_report_output_der TYPE STANDARD TABLE OF ty_report_output_der,
      ls_report_output_der TYPE ty_report_output_der,
      lv_message           TYPE string,
      ls_brf_exc_rep_line  TYPE /gda/sdm_exceptions_alv,
      er_data              TYPE REF TO data,
      er_data_empty        TYPE REF TO data.

    FIELD-SYMBOLS:
      <results>     TYPE STANDARD TABLE,
      <val_results> LIKE LINE OF me->mt_validation_results,
      <table>       TYPE ANY TABLE,
      <any>         TYPE any,
      <REF>         type any,
      <KEY>         type any,
      <BRF_ATTRIBUTES> like line of ME->MT_BRF_ATTRIBUTES.


   READ TABLE ME->MT_BRF_ATTRIBUTES ASSIGNING <BRF_ATTRIBUTES> WITH KEY NAME = '/GDA/SDM_S_MARA'.
    CHECK SY-SUBRC = 0.
    ASSIGN <BRF_ATTRIBUTES>-LR_DATA->* TO <REF>.
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <REF> to <KEY>.
    check sy-subrc = 0.

*    check <key> = 'R100000'.
    TRY.
        CALL METHOD super->call_class.
      CATCH /gda/cx_sdm_exception_handl.
      CATCH cx_fdt_input .
    ENDTRY.

* For Reporting - Return BRF results with Master Data
    IF me->mv_source = me->mc_rep.

      IF me->mv_type = me->mc_validation.
        CREATE DATA er_data       LIKE lt_sdm_gui_out.
        CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
      ELSEIF me->mv_type = me->mc_derivation.
        CREATE DATA er_data       LIKE lt_report_output_der.
        CREATE DATA er_data_empty LIKE lt_report_output_der.
      ELSE.
        CREATE DATA er_data       LIKE lt_sdm_gui_out.
        CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
      ENDIF.

      ASSIGN er_data->*           TO <table>.

* INDEX TYPE ID NUMBER EXTRA_V1 EXTRA_V2 EXTRA_V4 EXTRA_V5
*3  E ZGD_BRF 532 EINA-RUECK  Return Agreement    5300007316
*4  E ZGD_BRF 533 EINE-APLFZ  Planned Delivery Time in Days   5300007316
*5  E ZGD_BRF 512 MARC-RDPRF  Rnding Profile  RFST
*6  E ZGD_BRF 512 MARC-RDPRF  Rnding Profile  RFST
*7  E ZGD_BRF 514 MARC-RDPRF  Rnding Profile  RFDC
*8  E ZGD_BRF 531 MARC-DISPO  MRP controller  RFDC

      lv_message = 'Demo:BRF: X-site status may not be blank.'.

      ls_sdm_gui_out-message        = lv_message.
      ls_sdm_gui_out-id             = '/GDA/SDM1'.
      ls_sdm_gui_out-number         = '510'.
      ls_sdm_gui_out-type           = 'E'.
      ls_sdm_gui_out-extra_v1       = 'MARA-MSTAE'.
      ls_sdm_gui_out-extra_v2       = 'X-plant status'.
      COLLECT ls_sdm_gui_out INTO <table>.
      CLEAR ls_sdm_gui_out.

      lv_message = 'Demo:BRF: Valid from date may not be blank if X-site status is maintained.'.
      ls_sdm_gui_out-message        = lv_message.
      ls_sdm_gui_out-id             = '/GDA/SDM1'.
      ls_sdm_gui_out-number         = '511'.
      ls_sdm_gui_out-type           = 'E'.
      ls_sdm_gui_out-extra_v1       = 'MARA-MSTDE'.
      ls_sdm_gui_out-extra_v2       = 'Valid from'.
      COLLECT ls_sdm_gui_out INTO <table>.
      CLEAR ls_sdm_gui_out.

      lv_message = 'Demo:BRF: Return Agreement may not be blank EINA'.
      ls_sdm_gui_out-message        = lv_message.
      ls_sdm_gui_out-id             = '/GDA/SDM1'.
      ls_sdm_gui_out-number         = '532'.
      ls_sdm_gui_out-type           = 'E'.
      ls_sdm_gui_out-extra_v1       = 'EINA-RUECK'.
      ls_sdm_gui_out-extra_v2       = 'Return Agreement'.
      ls_sdm_gui_out-extra_v5       = '5300007316'.

      COLLECT ls_sdm_gui_out INTO <table>.
      CLEAR ls_sdm_gui_out.

      lv_message = 'Demo:BRF: Planned Delivery Time in Days may not be blank EINE'.
      ls_sdm_gui_out-message        = lv_message.
      ls_sdm_gui_out-id             = '/GDA/SDM1'.
      ls_sdm_gui_out-number         = '533'.
      ls_sdm_gui_out-type           = 'E'.
      ls_sdm_gui_out-extra_v1       = 'EINE-APLFZ'.
      ls_sdm_gui_out-extra_v2       = 'Planned Delivery Time in Days'.
      ls_sdm_gui_out-extra_v5       = '5300007316'.
      COLLECT ls_sdm_gui_out INTO <table>.
      CLEAR ls_sdm_gui_out.

      lv_message = 'Demo:BRF:BRF: Rounding Profile may not be blank if MRP Type is ND. ST'.
      ls_sdm_gui_out-message        = lv_message.
      ls_sdm_gui_out-id             = '/GDA/SDM1'.
      ls_sdm_gui_out-number         = '512'.
      ls_sdm_gui_out-type           = 'E'.
      ls_sdm_gui_out-extra_v1       = 'MARC-RDPRF'.
      ls_sdm_gui_out-extra_v2       = 'Rnding Profile'.
      ls_sdm_gui_out-extra_v2       = 'RFST'.
      COLLECT ls_sdm_gui_out INTO <table>.
      CLEAR ls_sdm_gui_out.

      lv_message = 'Demo:BRF: Rounding Profile may not be blank if MRP Type is ND. DC'.
      ls_sdm_gui_out-message        = lv_message.
      ls_sdm_gui_out-id             = '/GDA/SDM1'.
      ls_sdm_gui_out-number         = '514'.
      ls_sdm_gui_out-type           = 'E'.
      ls_sdm_gui_out-extra_v1       = 'MARC-RDPRF'.
      ls_sdm_gui_out-extra_v2       = 'Rnding Profile'.
      ls_sdm_gui_out-extra_v4       = 'RFDC'.
      COLLECT ls_sdm_gui_out INTO <table>.
      CLEAR ls_sdm_gui_out.

    ENDIF.

    mo_brf_result       = er_data.
    mo_brf_result_empty = er_data_empty.

  ENDMETHOD.


  METHOD class_constructor.
    DATA:
      ms_function_attributes TYPE /gda/sdm_setup6.

    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF mt_brf_attributes.

    SELECT * FROM /gda/sdm_setup6
                    INTO TABLE mt_function_attributes
                    WHERE sdm_object = 'ARTICLE'
                      AND active     = abap_true.

*    TRY.
    SELECT * FROM /gda/sdm_setup1 INTO CORRESPONDING FIELDS OF TABLE mt_brf_attributes
      WHERE sdm_object = 'ARTICLE'
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

      READ TABLE mt_function_attributes INTO ms_function_attributes WITH KEY sdm_object = 'ARTICLE'
                                                                             type       = <brf_attributes>-type.
      CHECK sy-subrc = 0.

      SELECT SINGLE id FROM fdt_admn_0000
                      INTO mv_id "lv_app_id
                        WHERE object_type = 'AP'
                        AND   name        = ms_function_attributes-application
                        AND   deleted     = space.

      SELECT SINGLE id FROM fdt_admn_0000
                      INTO <brf_attributes>-data_object
                        WHERE object_type    = 'DO'
                        AND   name           = <brf_attributes>-name
                        AND   application_id = mv_id "lv_app_id
                        AND   deleted        = space.
    ENDLOOP.


*      CATCH cx_fdt INTO lx_fdt.
*        CLEAR:
*         ls_message.
*
*        ls_message-msgid  = '/GDA/SDM1'.
*        ls_message-msgty  = 'E'.
*        ls_message-msgno  = '601'.
*        ls_message-msgv1  = 'Config Table'.
*        ls_message-msgv2  = '/GDA/SDM_SETUP'.
*        ls_message-text   = 'SDM Config table not maintained correctly'.
*        ls_message-source = '/GDA/SDM_SETUP'.
*        APPEND ls_message TO lx_fdt->mt_message[].
*
**        RAISE EXCEPTION TYPE cx_fdt_input
**          EXPORTING
**            previous   = lx_fdt
**            mt_message = lx_fdt->mt_message.
*
*    ENDTRY.


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
*                            iv_object_type_id = '01'
                            iv_source         = iv_source
                            iv_type           = iv_type
                            iv_stats          = iv_stats
                            iv_stats_brf      = iv_stats_brf
                            iv_errors_only    = iv_errors_only
                            iv_mapping        = iv_mapping ).
      CLEANUP INTO lx_root.
        lv_message = lx_root->get_text( ).

*        lv_exit = abap_true.
*        RAISE EXCEPTION TYPE cx_fdt_input
*          EXPORTING
*            previous = lx_fdt.
    ENDTRY.


* SDM_OBJECT
*if me->mt_message[] IS NOT INITIAL.
*  exit.
*endif.

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

  ENDMETHOD.


  METHOD create_brf_result_structure.

    DATA:
      er_data_empty        TYPE REF TO data,
      lt_sdm_gui_out       TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key,
      lt_report_output_der TYPE STANDARD TABLE OF ty_report_output_der.


*CALL METHOD SUPER->CREATE_BRF_RESULT_STRUCTURE
*  RECEIVING
*    R_RESULT_STRUCTURE =
*    .

    IF me->mv_type = me->mc_validation.
      CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
    ELSEIF me->mv_type = me->mc_derivation.
      CREATE DATA er_data_empty LIKE lt_report_output_der.
    ELSE.
      CREATE DATA er_data_empty LIKE lt_sdm_gui_out.
    ENDIF.

    r_result_structure = er_data_empty.

  ENDMETHOD.


  method GET_OBJECT_KEYS.
    DATA:
      ls_key_field TYPE fieldname.

    r_key_fields = super->get_object_keys( ).

    ls_key_field = 'KEY_MATNR'.
    APPEND ls_key_field TO r_key_fields.

    ls_key_field = 'KEY_MAKTX'.
    APPEND ls_key_field TO r_key_fields.

    ls_key_field = 'KEY_ATTYP'.
    APPEND ls_key_field TO r_key_fields.

    ls_key_field = 'KEY_MATKL'.
    APPEND ls_key_field TO r_key_fields.

    ls_key_field = 'KEY_MTART'.
    APPEND ls_key_field TO r_key_fields.

  endmethod.


  METHOD get_primary_object.
*CALL METHOD SUPER->GET_PRIMARY_OBJECT
*  RECEIVING
*    R_OBJECT_MAIN =
*    .
    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF mt_brf_attributes,
      <key_structure>  TYPE any,
      <object_key>     TYPE any.

    READ TABLE mt_brf_attributes ASSIGNING <brf_attributes> WITH KEY type = mv_type
                                                                    name = '/GDA/SDM_S_MARA'.
    CHECK sy-subrc = 0.

    ASSIGN <brf_attributes>-lr_data->* TO <key_structure>.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <key_structure> TO <object_key>.
    CHECK sy-subrc = 0.
    r_object_main = <object_key>.
  ENDMETHOD.
ENDCLASS.
