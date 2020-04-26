class /GDA/SDM_CL_BRF_MAPPING definition
  public
  final
  create private .

public section.

  types:
    BEGIN OF ty_der_results,
      table TYPE if_fdt_types=>element_text,
      field TYPE if_fdt_types=>element_text,
      value TYPE if_fdt_types=>element_text,
      skip_derivation TYPE xfeld,
      screen_name TYPE if_fdt_types=>element_text,
      grey_out TYPE xfeld,
      hide TYPE xfeld,
      required TYPE xfeld,
      bold TYPE xfeld,
      group1 TYPE if_fdt_types=>element_text,
      group2 TYPE if_fdt_types=>element_text,
      group3 TYPE if_fdt_types=>element_text,
      group4 TYPE if_fdt_types=>element_text,
    END OF ty_der_results .
  types:
    BEGIN OF ty_val_results ,
        type TYPE if_fdt_types=>element_text,
        id TYPE if_fdt_types=>element_text,
        number TYPE if_fdt_types=>element_text,
        message TYPE if_fdt_types=>element_text,
        log_no TYPE if_fdt_types=>element_text,
        log_msg_no TYPE if_fdt_types=>element_text,
        message_v1 TYPE if_fdt_types=>element_text,
        message_v2 TYPE if_fdt_types=>element_text,
        message_v3 TYPE if_fdt_types=>element_text,
        message_v4 TYPE if_fdt_types=>element_text,
        parameter TYPE if_fdt_types=>element_text,
        row TYPE if_fdt_types=>element_number,
        field TYPE if_fdt_types=>element_text,
        system TYPE if_fdt_types=>element_text,
      END OF ty_val_results .

  data MV_ACTIVE type XFELD .
  data MV_GUID type FDT_UUID .
  data MV_RESULT type BOOLEAN .
  data MV_MESSAGE type STRING .

  class-methods CHECK_FUNCTION_BRF
    importing
      !IV_OBJECT type /GDA/SDM_DE_OBJECT
      !IV_INCLUDE type PROGNAME
      !IV_SOURCE type /GDA/SDM_DE_SOURCE
    exporting
      !EV_ACTIVE type XFELD
      !EV_GUID type FDT_UUID
    raising
      CX_FDT_INPUT
      /GDA/CX_SDM_EXCEPTION_HANDL .
  class-methods CHECK_FUNCTION_CLASS
    importing
      !IV_OBJECT type /GDA/SDM_DE_OBJECT
      !IV_INCLUDE type PROGNAME
    exporting
      !EV_ACTIVE type XFELD
      !EV_GUID type FDT_UUID
    raising
      CX_FDT_INPUT .
  class-methods FACTORY
    importing
      !IV_OBJECT type /GDA/SDM_DE_OBJECT
      !IV_INCLUDE type PROGNAME
      !IV_SOURCE type /GDA/SDM_DE_SOURCE
      !IV_MODULE type /GDA/SDM_DE_MOD_TYPE
    returning
      value(RO_BRF_MAPPING) type ref to /GDA/SDM_CL_BRF_MAPPING .
  class-methods CHECK_FUNCTION
    importing
      !IV_OBJECT type /GDA/SDM_DE_OBJECT_ID optional
      !IV_MODULE type /GDA/SDM_DE_MOD_TYPE optional
    exporting
      value(EV_RESULT) type BOOLEAN
      !EV_MESSAGE type STRING
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_FUNCTION
    returning
      value(RT_RESULT) type /GDA/SDM_T_LICENSE_KEYS
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
protected section.

  constants MC_OBJECT type /GDA/SDM_DE_OBJECT value 'DEFAULT' ##NO_TEXT.
  constants MC_TYPE type /GDA/SDM_DE_TYPE value '09' ##NO_TEXT.
private section.

  data MV_OBJECT_ID type /GDA/SDM_DE_OBJECT_ID .
  class-data MV_CHECK type CHAR16 .
  class-data MV_CLIENT type MANDT .

  class-methods CHECK_CLIENT
    returning
      value(R_MESSAGE) type STRING
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods CONSTRUCTOR
    importing
      !IV_OBJECT type /GDA/SDM_DE_OBJECT
      !IV_INCLUDE type PROGNAME
      !IV_SOURCE type /GDA/SDM_DE_SOURCE
      !IV_MODULE type /GDA/SDM_DE_MOD_TYPE .
ENDCLASS.



CLASS /GDA/SDM_CL_BRF_MAPPING IMPLEMENTATION.


  METHOD check_client.
    IF mv_client <> sy-mandt.
      r_message = TEXT-008.
      REPLACE '&1' WITH sy-mandt INTO r_message.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_FUNCTION. _verify iv_object iv_module. ENDMETHOD.


  METHOD check_function_brf.

    DATA:
      lv_timestamp   TYPE timestamp,
      lt_name_value  TYPE abap_parmbind_tab,
      ls_name_value  TYPE abap_parmbind,
      lt_message     TYPE if_fdt_types=>t_message,
      ls_message     TYPE if_fdt_types=>s_message,
      lv_brf_mapping TYPE /gda/sdm_brf_mapping,
      lt_messages    TYPE if_fdt_types=>t_message,
      lv_guid        TYPE if_fdt_types=>id,
      lv_app_id      TYPE if_fdt_types=>id,
      lv_func_id     TYPE if_fdt_types=>id,
      lv_app_name    TYPE fdt_name,
      lv_func_name   TYPE fdt_name,
      lv_message     TYPE string,
      lx_fdt         TYPE REF TO cx_fdt,
      lr_data        TYPE REF TO data.

    FIELD-SYMBOLS:
      <poe>    TYPE any,
      <rep>    TYPE any,
      <guid>   TYPE any,
      <result> TYPE any.
*  <ls_brf_mapping_out> TYPE /gda/sdm_brf_mapping.

    IF iv_object IS INITIAL OR iv_include IS INITIAL.
      RETURN.
    ENDIF.

* Get application details
    SELECT SINGLE application class FROM /gda/sdm_setup6
                              INTO ( lv_app_name, lv_func_name )
                             WHERE sdm_object = mc_object
                               AND  type      = mc_type.

* Get application ID
    SELECT SINGLE id FROM fdt_admn_0000
                    INTO lv_app_id
                      WHERE object_type = 'AP'
                      AND   name        = lv_app_name
                      AND   deleted     = space.

* Get function ID
    SELECT SINGLE id FROM fdt_admn_0000
                    INTO lv_func_id
                      WHERE object_type    = 'FU'
                      AND   name           = lv_func_name
                      AND   application_id = lv_app_id
                      AND   deleted        = space.

****************************************************************************************************
* All method calls within one processing cycle calling the same function must use the same timestamp.
* For subsequent calls of the same function, we recommend to use the same timestamp for all calls.
* This is to improve the system performance.
****************************************************************************************************
* If you are using structures or tables without DDIC binding, you have to declare the respective types
* by yourself. Insert the according data type at the respective source code line.
****************************************************************************************************
    GET TIME STAMP FIELD lv_timestamp.
****************************************************************************************************
* Process a function without recording trace data, passing context data objects via a name/value table.
****************************************************************************************************
* Prepare function processing:
****************************************************************************************************
* Let BRFplus convert your data into the type BRFplus requires:
* Data object is bound to a DDIC type, so you can improve performance by passing a variable of that type.
* If you pass a variable of this type, you should indicate this by passing "abap_true" for parameter "iv_has_ddic_binding".
****************************************************************************************************
    SELECT SINGLE name FROM /gda/sdm_setup1 INTO ls_name_value-name
      WHERE sdm_object = mc_object
      AND   active    =  abap_true
      AND   name      = 'SYST'.

*    ls_name_value-name = 'SYST'.

    SELECT SINGLE id FROM fdt_admn_0000
                    INTO lv_guid
                      WHERE object_type    = 'DO'
                      AND   name           = ls_name_value-name
                      AND   application_id = lv_app_id.


    GET REFERENCE OF sy INTO lr_data.
    TRY.
        cl_fdt_function_process=>move_data_to_data_object( EXPORTING ir_data             = lr_data
                                                                     iv_function_id      = lv_func_id "lv_function_id
                                                                     iv_data_object      = lv_guid "'0019BBC7F1241ED785A88A530C0E20FF' "SYST
                                                                     iv_timestamp        = lv_timestamp
                                                                     iv_trace_generation = abap_false
                                                                     iv_has_ddic_binding = abap_true
                                                           IMPORTING er_data             = ls_name_value-value ).
        INSERT ls_name_value INTO TABLE lt_name_value.
      CATCH cx_fdt INTO lx_fdt.
*        RAISE EXCEPTION TYPE cx_fdt_input
*          EXPORTING
*            previous   = lx_fdt
*            mt_message = lx_fdt->mt_message.
        APPEND LINES OF lx_fdt->mt_message[] TO lt_messages[].
    ENDTRY.

    CLEAR:
     lv_guid.

    SELECT SINGLE name FROM /gda/sdm_setup1 INTO ls_name_value-name
      WHERE sdm_object = mc_object
      AND   active    =  abap_true
      AND   name      = '/GDA/SDM_BRF_MAPPING'.

    IF sy-subrc <> 0.
      SELECT SINGLE name FROM /gda/sdm_setup1 INTO ls_name_value-name
        WHERE sdm_object = mc_object
        AND   active    =  abap_true
        AND   name      LIKE '%BRF_MAPPING'.
    ENDIF.
*    ls_name_value-name = '/GDA/SDM_BRF_MAPPING'.

    SELECT SINGLE id FROM fdt_admn_0000
                    INTO lv_guid
                      WHERE object_type    = 'DO'
                      AND   name           = ls_name_value-name
                      AND   application_id = lv_app_id.

    lv_brf_mapping-data_object  = iv_object.
    lv_brf_mapping-include_name = iv_include.

    TRY.
        GET REFERENCE OF lv_brf_mapping INTO lr_data.
        cl_fdt_function_process=>move_data_to_data_object( EXPORTING ir_data             = lr_data
                                                                     iv_function_id      = lv_func_id "lv_function_id
                                                                     iv_data_object      = lv_guid "'0019BBC7F1261ED59DE3ECD8DB7D00F1' "ZCA_BRF_MAPPING
                                                                     iv_timestamp        = lv_timestamp
                                                                     iv_trace_generation = abap_false
                                                                     iv_has_ddic_binding = abap_true
                                                           IMPORTING er_data             = ls_name_value-value ).
        INSERT ls_name_value INTO TABLE lt_name_value.
      CATCH cx_fdt INTO lx_fdt.
        APPEND LINES OF lx_fdt->mt_message[] TO lt_messages[].
    ENDTRY.

****************************************************************************************************
* Create the data to store the result value after processing the function


* You can skip the following call, if you already have
* a variable for the result. Please replace also the parameter
* EA_RESULT in the method call CL_FDT_FUNCTION_PROCESS=>PROCESS
* with the desired variable.
****************************************************************************************************
    TRY.

        cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_func_id"lv_function_id
                                                                      iv_data_object      = '_V_RESULT'
                                                                      iv_timestamp        = lv_timestamp
                                                                      iv_trace_generation = abap_false
                                                            IMPORTING er_data             = lr_data ).
        ASSIGN lr_data->* TO <result>.
      CATCH cx_fdt INTO lx_fdt.
        APPEND LINES OF lx_fdt->mt_message[] TO lt_messages[].
    ENDTRY.

    TRY.
        IF <result> IS ASSIGNED.
          cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_func_id
                                                      iv_timestamp   = lv_timestamp
                                            IMPORTING ea_result      = <result>
                                            CHANGING  ct_name_value  = lt_name_value ).

          IF <result> IS ASSIGNED.
            ASSIGN COMPONENT 'BRF_GUID' OF STRUCTURE <result> TO <guid>.
            IF sy-subrc = 0.
              ev_guid = <guid>.
            ENDIF.

            ASSIGN COMPONENT 'ACTIVE_REP' OF STRUCTURE <result> TO <rep>.

            ASSIGN COMPONENT 'ACTIVE_POE' OF STRUCTURE <result> TO <poe>.

            CASE iv_source.
              WHEN '1'.
                IF <rep> IS ASSIGNED.
                  IF <rep> = abap_true.
                    ev_active = abap_true.
                  ENDIF.
                ENDIF.
              WHEN '2'.
                IF <poe> IS ASSIGNED.
                  IF <poe> = abap_true.
                    ev_active = abap_true.
                  ENDIF.
                ENDIF.

              WHEN OTHERS.

            ENDCASE.

          ENDIF.
        ENDIF.
      CATCH cx_fdt INTO lx_fdt .
        APPEND LINES OF lx_fdt->mt_message[] TO lt_messages[].

****************************************************************************************************
* You can check CX_FDT->MT_MESSAGE for error handling.
****************************************************************************************************
    ENDTRY.

    IF lt_messages[] IS NOT INITIAL.
      REFRESH:
       lx_fdt->mt_message[].

      APPEND LINES OF lt_messages[] TO lx_fdt->mt_message[].

      RAISE EXCEPTION TYPE cx_fdt_input
        EXPORTING
          previous   = lx_fdt
          mt_message = lx_fdt->mt_message[].
    ENDIF.

    IF ev_guid IS INITIAL.
      lv_message = TEXT-005.

      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = lv_message.

    ENDIF.

    IF ev_active IS INITIAL.
      lv_message = TEXT-006.

      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = lv_message.

    ENDIF.

  ENDMETHOD.


  METHOD CHECK_FUNCTION_CLASS.

*    CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '0019BBC7F1261ED59CCE05BC8B6FC0F1'.
*
*    DATA:lv_timestamp       TYPE timestamp,
*         lt_name_value      TYPE abap_parmbind_tab,
*         ls_name_value      TYPE abap_parmbind,
*         lr_data            TYPE REF TO data,
*         lt_message         TYPE if_fdt_types=>t_message,
*         lx_fdt             TYPE REF TO cx_fdt,
*         la_zca_brf_mapping TYPE /gda/sdm_brf_mapping,
*         lt_messages        TYPE if_fdt_types=>t_message.
*
*    FIELD-SYMBOLS: <la_any>             TYPE any,
*                   <lv_field>           TYPE any,
*                   <ls_brf_mapping_out> TYPE /gda/sdm_brf_mapping.
*
*    IF iv_object IS INITIAL OR iv_include IS INITIAL.
*      RETURN.
*    ENDIF.
*
*****************************************************************************************************
** All method calls within one processing cycle calling the same function must use the same timestamp.
** For subsequent calls of the same function, we recommend to use the same timestamp for all calls.
** This is to improve the system performance.
*****************************************************************************************************
** If you are using structures or tables without DDIC binding, you have to declare the respective types
** by yourself. Insert the according data type at the respective source code line.
*****************************************************************************************************
*    GET TIME STAMP FIELD lv_timestamp.
*****************************************************************************************************
** Process a function without recording trace data, passing context data objects via a name/value table.
*****************************************************************************************************
** Prepare function processing:
*****************************************************************************************************
** Let BRFplus convert your data into the type BRFplus requires:
** Data object is bound to a DDIC type, so you can improve performance by passing a variable of that type.
** If you pass a variable of this type, you should indicate this by passing "abap_true" for parameter "iv_has_ddic_binding".
*****************************************************************************************************
*
*    ls_name_value-name = 'SYST'.
*    GET REFERENCE OF sy INTO lr_data.
*    TRY.
*        cl_fdt_function_process=>move_data_to_data_object( EXPORTING ir_data             = lr_data
*                                                                     iv_function_id      = lv_function_id
*                                                                     iv_data_object      = '0019BBC7F1241ED785A88A530C0E20FF' "SYST
*                                                                     iv_timestamp        = lv_timestamp
*                                                                     iv_trace_generation = abap_false
*                                                                     iv_has_ddic_binding = abap_true
*                                                           IMPORTING er_data             = ls_name_value-value ).
*        INSERT ls_name_value INTO TABLE lt_name_value.
*      CATCH cx_fdt INTO lx_fdt.
**        RAISE EXCEPTION TYPE cx_fdt_input
**          EXPORTING
**            previous   = lx_fdt
**            mt_message = lx_fdt->mt_message.
*        APPEND LINES OF lx_fdt->mt_message[] TO lt_messages[].
*    ENDTRY.
*
*    ls_name_value-name = '/GDA/SDM_BRF_MAPPING'.
*    la_zca_brf_mapping-data_object = iv_object.
*    la_zca_brf_mapping-include_name = iv_include.
*
*    TRY.
*        GET REFERENCE OF la_zca_brf_mapping INTO lr_data.
*        cl_fdt_function_process=>move_data_to_data_object( EXPORTING ir_data             = lr_data
*                                                                     iv_function_id      = lv_function_id
*                                                                     iv_data_object      = '0019BBC7F1261ED59DE3ECD8DB7D00F1' "ZCA_BRF_MAPPING
*                                                                     iv_timestamp        = lv_timestamp
*                                                                     iv_trace_generation = abap_false
*                                                                     iv_has_ddic_binding = abap_true
*                                                           IMPORTING er_data             = ls_name_value-value ).
*        INSERT ls_name_value INTO TABLE lt_name_value.
*      CATCH cx_fdt INTO lx_fdt.
**        RAISE EXCEPTION TYPE cx_fdt_input
**          EXPORTING
**            previous   = lx_fdt
**            mt_message = lx_fdt->mt_message.
*        APPEND LINES OF lx_fdt->mt_message[] TO lt_messages[].
*
*    ENDTRY.
*
*****************************************************************************************************
** Create the data to store the result value after processing the function
*
*
** You can skip the following call, if you already have
** a variable for the result. Please replace also the parameter
** EA_RESULT in the method call CL_FDT_FUNCTION_PROCESS=>PROCESS
** with the desired variable.
*****************************************************************************************************
*    TRY.
*
*        cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_function_id
*                                                                      iv_data_object      = '_V_RESULT'
*                                                                      iv_timestamp        = lv_timestamp
*                                                                      iv_trace_generation = abap_false
*                                                            IMPORTING er_data             = lr_data ).
*        ASSIGN lr_data->* TO <la_any>.
*      CATCH cx_fdt INTO lx_fdt.
*
**        RAISE EXCEPTION TYPE cx_fdt_input
**          EXPORTING
**            previous   = lx_fdt
**            mt_message = lx_fdt->mt_message.
*        APPEND LINES OF lx_fdt->mt_message[] TO lt_messages[].
*    ENDTRY.
*
*    TRY.
*        IF <la_any> IS ASSIGNED.
*          cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_function_id
*                                                      iv_timestamp   = lv_timestamp
*                                            IMPORTING ea_result      = <la_any>
*                                            CHANGING  ct_name_value  = lt_name_value ).
*
*          IF <la_any> IS ASSIGNED.
*            ASSIGN COMPONENT 'BRF_GUID' OF STRUCTURE <la_any> TO <lv_field>.
*            IF sy-subrc = 0.
*              ev_guid = <lv_field>.
*            ENDIF.
*
*            ASSIGN COMPONENT 'ACTIVE_FLAG' OF STRUCTURE <la_any> TO <lv_field>.
*            IF sy-subrc = 0.
*              ev_active = <lv_field>.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      CATCH cx_fdt INTO lx_fdt .
*
**        RAISE EXCEPTION TYPE cx_fdt_input
**          EXPORTING
**            previous   = lx_fdt
**            mt_message = lx_fdt->mt_message.
*        APPEND LINES OF lx_fdt->mt_message[] TO lt_messages[].
*
*****************************************************************************************************
** You can check CX_FDT->MT_MESSAGE for error handling.
*****************************************************************************************************
*    ENDTRY.
*
*    IF lt_messages[] IS NOT INITIAL.
*      REFRESH:
*       lx_fdt->mt_message[].
*
*      APPEND LINES OF lt_messages[] TO lx_fdt->mt_message[].
*
*      RAISE EXCEPTION TYPE cx_fdt_input
*        EXPORTING
*          previous   = lx_fdt
*          mt_message = lx_fdt->mt_message[].
*    ENDIF.

EV_ACTIVE = ABAP_TRUE.
EV_GUID   = 'DEMO'.

  ENDMETHOD.


  method CLASS_CONSTRUCTOR.
    MV_CHECK = '/GDA/SDM_LIC'.

    SELECT SINGLE client FROM /gda/sdm_client
                    INTO mv_client
                    WHERE client = sy-mandt
                      AND active = abap_true.
  endmethod.


  METHOD constructor.

    DATA:
      lv_result    TYPE boolean,
      lv_message   TYPE string,
      lx_fdt_input TYPE REF TO cx_fdt_input,
      lx_sdm_excep TYPE REF TO /gda/cx_sdm_exception_handl.

* Check if SDM has been activated for client
    TRY.
        me->mv_message = /gda/sdm_cl_brf_mapping=>check_client( ).
      CATCH /gda/cx_sdm_exception_handl .
    ENDTRY.

    CHECK me->mv_message = space.

* get the object ID
* for now BRFPlus uses SDM OBJECT everything else uses SDM Object ID
    SELECT SINGLE sdm_object_id INTO me->mv_object_id
             FROM /gda/sdm_obj
             WHERE sdm_object = iv_object.

    TRY.
        CALL METHOD /gda/sdm_cl_brf_mapping=>check_function
          EXPORTING
            iv_object  = me->mv_object_id
            iv_module  = iv_module
          IMPORTING
            ev_result  = me->mv_result
            ev_message = me->mv_message.


      CATCH /gda/cx_sdm_exception_handl .
    ENDTRY.

    CHECK me->mv_message = space.
    TRY.
        CALL METHOD /gda/sdm_cl_brf_mapping=>check_function_brf
          EXPORTING
            iv_object  = iv_object
            iv_include = iv_include
            iv_source  = iv_source
          IMPORTING
            ev_active  = me->mv_active
            ev_guid    = me->mv_guid.

      CATCH cx_fdt_input INTO lx_fdt_input.
        me->mv_message = lx_fdt_input->get_text( ).
      CATCH /gda/cx_sdm_exception_handl INTO lx_sdm_excep.
        me->mv_message = lx_sdm_excep->mv_text.
    ENDTRY.
  ENDMETHOD.


  METHOD factory.
    CREATE OBJECT ro_brf_mapping
      EXPORTING
        iv_object  = iv_object
        iv_include = iv_include
        iv_source  = iv_source
        iv_module  = iv_module.

  ENDMETHOD.


  METHOD GET_FUNCTION. _retrieve. ENDMETHOD.
ENDCLASS.
