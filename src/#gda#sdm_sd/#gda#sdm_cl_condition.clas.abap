class /GDA/SDM_CL_CONDITION definition
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
      !IV_ERRORS_ONLY type BOOLEAN optional
    raising
      CX_FDT_INPUT .

  methods /GDA/SDM_IF_ACTION_PROCESSOR~PROCESS_ACTION
    redefinition .
protected section.

  methods CALL_BRF
    redefinition .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_CONDITION IMPLEMENTATION.


  METHOD /GDA/SDM_IF_ACTION_PROCESSOR~PROCESS_ACTION.
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
      <matnr>          type any.

* Handle common actions to SDM
    lv_fcode = x_action->get_fcode( ).

* Check for common actions
    CASE lv_fcode.


      WHEN 'MM43'.
        READ TABLE me->mt_brf_attributes ASSIGNING <brf_attributes> WITH KEY name = '/GDA/SDM_S_MARA'.
        ASSIGN <brf_attributes>-lr_data->* TO <ref>.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ref> TO <matnr>.

        SET PARAMETER ID: 'MAT' FIELD <matnr>.
*        SET PARAMETER ID 'MXX' FIELD 'K' .
        CALL TRANSACTION 'MM43' AND SKIP FIRST SCREEN.

      WHEN 'MM42'.
        READ TABLE me->mt_brf_attributes ASSIGNING <brf_attributes> WITH KEY name = '/GDA/SDM_S_MARA'.
        ASSIGN <brf_attributes>-lr_data->* TO <ref>.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ref> TO <matnr>.

        SET PARAMETER ID: 'MAT' FIELD <matnr>.
        CALL TRANSACTION 'MM42' AND SKIP FIRST SCREEN.

      WHEN 'VKP5'.
        READ TABLE me->mt_brf_attributes ASSIGNING <brf_attributes> WITH KEY name = '/GDA/SDM_S_MARA'.
        ASSIGN <brf_attributes>-lr_data->* TO <ref>.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ref> TO <matnr>.

        SET PARAMETER ID: 'MAT' FIELD <matnr>.
        CALL TRANSACTION 'VKP5'.

      WHEN OTHERS.


    ENDCASE.

  ENDMETHOD.


  METHOD CALL_BRF.

    DATA:
      LT_SDM_GUI_OUT       TYPE STANDARD TABLE OF /GDA/SDM_S_VAL_RESULTS_KEY,
      LS_SDM_GUI_OUT       TYPE                   /GDA/SDM_S_VAL_RESULTS_KEY,
      LT_REPORT_OUTPUT_DER TYPE STANDARD TABLE OF TY_REPORT_OUTPUT_DER,
      LS_REPORT_OUTPUT_DER TYPE                   TY_REPORT_OUTPUT_DER,
      LV_MESSAGE           TYPE                   STRING,
      LS_BRF_EXC_REP_LINE  TYPE                   /GDA/SDM_EXCEPTIONS_ALV,
      ER_DATA              TYPE REF TO            DATA,
      ER_DATA_EMPTY        TYPE REF TO            DATA.

    FIELD-SYMBOLS:
      <RESULTS>            TYPE STANDARD TABLE,
      <VAL_RESULTS>        LIKE LINE OF ME->MT_VALIDATION_RESULTS,
      <TABLE>              TYPE ANY TABLE,
      <ANY>                TYPE ANY.

    TRY.
        CALL METHOD super->call_brf.
      CATCH /gda/cx_sdm_exception_handl.
    ENDTRY.

* For Reporting - Return BRF results with Master Data
    IF ME->MV_SOURCE = ME->MC_REP.

      IF ME->MV_TYPE = ME->MC_VALIDATION.
        CREATE DATA ER_DATA       LIKE LT_SDM_GUI_OUT.
        CREATE DATA ER_DATA_EMPTY LIKE LT_SDM_GUI_OUT.
      ELSEIF ME->MV_TYPE = ME->MC_DERIVATION.
        CREATE DATA ER_DATA       LIKE LT_REPORT_OUTPUT_DER.
        CREATE DATA ER_DATA_EMPTY LIKE LT_REPORT_OUTPUT_DER.
      ELSE.
        CREATE DATA ER_DATA       LIKE LT_SDM_GUI_OUT.
        CREATE DATA ER_DATA_EMPTY LIKE LT_SDM_GUI_OUT.
      ENDIF.

      ASSIGN ER_DATA->*           TO <TABLE>.

* Derivation
      IF ME->MV_TYPE = ME->MC_DERIVATION.
        ASSIGN ME->MO_BRF_RESULT->* TO <RESULTS>.

        LOOP AT <RESULTS> ASSIGNING <ANY>.
          MOVE-CORRESPONDING <ANY> TO LS_REPORT_OUTPUT_DER.
*          LS_REPORT_OUTPUT_DER-MATNR = ME->MS_ZMARA-MATNR.
          COLLECT LS_REPORT_OUTPUT_DER INTO <TABLE>.
          CLEAR LS_REPORT_OUTPUT_DER.
        ENDLOOP.

* Validations and others
      ELSE.
        ASSIGN ME->MO_BRF_RESULT->* TO <RESULTS>.

        LOOP AT <RESULTS> ASSIGNING <VAL_RESULTS>.

*      IF <lr_s_validation_results>-number NOT IN me->ms_selscreen-msgno.
*        CONTINUE.
*      ENDIF.

          IF <VAL_RESULTS>-ID = SPACE.
            CONTINUE.
          ENDIF.

          IF ME->MV_ERRORS_ONLY = ABAP_TRUE.
            IF <VAL_RESULTS>-TYPE NA 'EAX'.
              CONTINUE.
            ENDIF.
          ENDIF.

          MESSAGE ID <VAL_RESULTS>-ID
                TYPE <VAL_RESULTS>-TYPE
              NUMBER <VAL_RESULTS>-NUMBER
                WITH <VAL_RESULTS>-MESSAGE_V1 <VAL_RESULTS>-MESSAGE_V2
                     <VAL_RESULTS>-MESSAGE_V3 <VAL_RESULTS>-MESSAGE_V4
                INTO LV_MESSAGE.

          LS_SDM_GUI_OUT-MESSAGE        = LV_MESSAGE.
          LS_SDM_GUI_OUT-ID             = <VAL_RESULTS>-ID.
          LS_SDM_GUI_OUT-NUMBER         = <VAL_RESULTS>-NUMBER.
          LS_SDM_GUI_OUT-TYPE           = <VAL_RESULTS>-TYPE.
          LS_SDM_GUI_OUT-EXTRA_V1       = <VAL_RESULTS>-EXTRA_V1.
          LS_SDM_GUI_OUT-EXTRA_V2       = <VAL_RESULTS>-EXTRA_V2.
          LS_SDM_GUI_OUT-EXTRA_V3       = <VAL_RESULTS>-EXTRA_V3.
          LS_SDM_GUI_OUT-EXTRA_V4       = <VAL_RESULTS>-EXTRA_V4.
          LS_SDM_GUI_OUT-EXTRA_V5       = <VAL_RESULTS>-EXTRA_V5.
          LS_SDM_GUI_OUT-EXTRA_V6       = <VAL_RESULTS>-EXTRA_V6.

          COLLECT LS_SDM_GUI_OUT INTO <TABLE>.
          CLEAR LS_SDM_GUI_OUT.

*          IF ME->MV_STATS = ABAP_TRUE.
*            MOVE-CORRESPONDING <VAL_RESULTS> TO LS_BRF_EXC_REP_LINE.
*
*            LS_BRF_EXC_REP_LINE-MSG_COUNT  = 1.
*            LS_BRF_EXC_REP_LINE-MSG_ID     = <VAL_RESULTS>-ID.
*            LS_BRF_EXC_REP_LINE-MSG_NUMBER = <VAL_RESULTS>-NUMBER.
*            LS_BRF_EXC_REP_LINE-MSG_TYPE   = <VAL_RESULTS>-TYPE.
** Add item to queue
*            ME->MO_SDM_EXCEPTIONS->ADD_LINE( LS_BRF_EXC_REP_LINE ).
*          ENDIF.

        ENDLOOP.
** Derivation
*      ELSEIF ME->MV_TYPE = ME->MC_DERIVATION.
*        ASSIGN ME->MO_BRF_RESULT->* TO <RESULTS>.
*
*        LOOP AT <RESULTS> ASSIGNING <ANY>.
*          MOVE-CORRESPONDING <ANY> TO LS_REPORT_OUTPUT_DER.
**          LS_REPORT_OUTPUT_DER-MATNR = ME->MS_ZMARA-MATNR.
*          COLLECT LS_REPORT_OUTPUT_DER INTO <TABLE>.
*          CLEAR LS_REPORT_OUTPUT_DER.
*        ENDLOOP.
      ENDIF.

      MO_BRF_RESULT       = ER_DATA.
      MO_BRF_RESULT_EMPTY = ER_DATA_EMPTY.

    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    DATA:
      wf_ref     TYPE REF TO data,
      wf_line    TYPE REF TO data,
      lx_root    TYPE REF TO cx_root,
      lv_message TYPE string,
      lv_exit.

    FIELD-SYMBOLS:
      <brf_attributes> LIKE LINE OF mt_brf_attributes,
      <ref>            TYPE any,
      <fs_tab>         TYPE any.

    TRY.
        super->constructor( iv_object_type = iv_object_type iv_source = iv_source iv_type = iv_type iv_stats = iv_stats iv_errors_only = iv_errors_only ).
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

    LOOP AT mt_brf_attributes ASSIGNING <brf_attributes>.
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

      ELSEIF <brf_attributes>-ref_type = '3'.
*        CREATE DATA WF_REF TYPE SORTED TABLE OF (<BRF_ATTRIBUTES>-NAME). "Dynamic Structure
*        ASSIGN WF_REF->* TO <REF>.
*        GET REFERENCE OF <REF> INTO <BRF_ATTRIBUTES>-LR_DATA.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
