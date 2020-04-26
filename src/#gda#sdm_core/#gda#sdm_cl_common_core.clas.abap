class /GDA/SDM_CL_COMMON_CORE definition
  public
  final
  create public .

public section.

  class-methods SDM_RETRIEVE_DATA
    importing
      !XO_OBJECT type ref to /GDA/SDM_CL_CORE
    returning
      value(RT_RESULTS) type ref to DATA
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  class-methods SDM_PROCESS_DATA
    importing
      !XO_OBJECT type ref to /GDA/SDM_CL_CORE
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  class-methods SDM_INITIALISE
    importing
      !IV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT
      !IV_SOURCE type /GDA/SDM_DE_SOURCE
      !IV_TYPE type /GDA/SDM_DE_TYPE
      !IV_STATS type /GDA/SDM_DE_STATS
      !IV_STATS_BRF type /GDA/SDM_DE_STATS optional
      !IV_ERRORS_ONLY type /GDA/SDM_DE_OUTPUT optional
      !IV_MAPPING type ref to /GDA/SDM_CL_BRF_MAPPING optional
    returning
      value(RO_OBJECT) type ref to /GDA/SDM_CL_CORE
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  class-methods GET_SDM_TYPE
    importing
      !X_SDM_TYPE type /GDA/SDM_DE_TYPE
      !X_SDM_SOURCE type /GDA/SDM_DE_SOURCE
    returning
      value(R_SDM_TYPES) type /GDA/SDM_R_SDM_TYPE .
  class-methods GET_SDM_OBJECTS
    importing
      !X_SDM_OBECT type /GDA/SDM_DE_OBJECT
      !XT_SDM_TYPES type /GDA/SDM_R_SDM_TYPE
    returning
      value(R_SDM_OBJECTS) type /GDA/SDM_T_OBJECTS .
protected section.
private section.

  class-data PX_FDT_INPUT type ref to CX_FDT_INPUT .
  class-data PX_SDM_ROOT type ref to /GDA/CX_SDM_EXCEPTION_HANDL .
  class-data PV_MESSAGE type STRING .
ENDCLASS.



CLASS /GDA/SDM_CL_COMMON_CORE IMPLEMENTATION.


  METHOD get_sdm_objects.
    SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE r_sdm_objects
      WHERE  sdm_object  = x_sdm_obect
       AND   type        IN xt_sdm_types
       AND   active      = abap_true.
  ENDMETHOD.


  METHOD get_sdm_type.
    DATA:
      ls_sdm_type TYPE /gda/sdm_s_sdm_type,
      sdm_handle  TYPE REF TO /gda/sdm_badi1.

* set to default SDM type
    ls_sdm_type-sign   =  'I'.
    ls_sdm_type-option =  'EQ'.
    ls_sdm_type-low    =  x_sdm_type.
    APPEND ls_sdm_type TO r_sdm_types.

    TRY.
        GET BADI sdm_handle
          FILTERS
            sdm_type_main = x_sdm_type.

      CATCH cx_badi_not_implemented.
        CLEAR sdm_handle.
    ENDTRY.

IF NOT sdm_handle IS INITIAL.
  CALL BADI sdm_handle->add_sdm_type
    EXPORTING
      x_source          = X_SDM_SOURCE
    CHANGING
      xt_sdm_type       = r_sdm_types
    EXCEPTIONS
      application_error = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
*          MESSAGE e() RAISING application_error.
  ENDIF.
ENDIF.

  ENDMETHOD.


  METHOD sdm_initialise.

    TRY.
        ro_object ?= /gda/sdm_cl_core=>factory( iv_object_type = iv_object_type
                                                       iv_source      = iv_source
                                                       iv_type        = iv_type
                                                       iv_stats       = abap_true ).
      CATCH cx_fdt_input INTO px_fdt_input.
        IF ro_object IS NOT INITIAL.
          ro_object->display_messages( ).
          EXIT.
        ENDIF.
    ENDTRY.


    IF NOT ro_object IS BOUND.
      RAISE EXCEPTION TYPE  /gda/cx_sdm_exception_handl.
    ENDIF.

    IF ro_object->is_active( ) = abap_false.
      RAISE EXCEPTION TYPE  /gda/cx_sdm_exception_handl.
    ENDIF.

    IF NOT ro_object->mt_message[] IS INITIAL.
      RAISE EXCEPTION TYPE  /gda/cx_sdm_exception_handl.
    ENDIF.

  ENDMETHOD.


  METHOD sdm_process_data.
    TRY.
        xo_object->main( ).
      CATCH /gda/cx_sdm_exception_handl INTO px_sdm_root.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
*      gv_message = gx_sdm_root->mv_text.
*      IF sy-batch = abap_true.
*        WRITE: / gv_message.
*      ELSE.
*        MESSAGE gv_message TYPE 'I'.
*      ENDIF.
*      RETURN.
      CATCH cx_fdt_input INTO px_fdt_input.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
*        CALL METHOD px_fdt_input->if_message~get_longtext
*          RECEIVING
*            result = pv_message.
*      IF sy-batch = abap_true.
*        WRITE: / gv_message.
*      ELSE.
*        MESSAGE pv_message TYPE 'I'.
*      ENDIF.
*        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD sdm_retrieve_data.
*    DATA:
*      lr_data       TYPE REF TO data.
**      lr_data_empty TYPE REF TO data.
*
*    FIELD-SYMBOLS:
*      <results>     TYPE standard table."/gda/sdm_t_val_results.
**      <results_val_all> TYPE /gda/sdm_t_val_results.
*
*    lr_data = xo_object->return_brf_result( ).
*    ASSIGN lr_data->* TO <results>.
*
*    IF <results> IS NOT ASSIGNED.
*      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
*    ENDIF.
*
*** Collate results.
**    IF <results_val_all> IS NOT ASSIGNED.
**      IF xo_object IS BOUND.
**        lr_data_empty  = xo_object->return_brf_result_structure( ).
**        ASSIGN lr_data_empty->* TO <results_val_all>.
**      ENDIF.
**    ENDIF.
**
**    APPEND LINES OF <results_val> TO <results_val_all>.
  ENDMETHOD.
ENDCLASS.
