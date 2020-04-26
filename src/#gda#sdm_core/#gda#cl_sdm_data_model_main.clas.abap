class /GDA/CL_SDM_DATA_MODEL_MAIN definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !I_SDM_OBJECT_ID type /GDA/SDM_DE_OBJECT_ID
      !I_SDM_TYPE type /GDA/SDM_DE_TYPE
      !I_SDM_TABKEY type CDTABKEY
      !I_TABNAME type /GDA/SDM_DE_TABNAME
      !I_FIELD type /GDA/SDM_DE_FIELDNAME
      !I_MSG_ID type SYMSGID
      !I_MSG_NUMBER type SYMSGNO
      !I_BEGDA type DATUM .
  methods SET_ENDDA
    importing
      !I_ENDDA type DATUM .
  methods SET_MSG_TYPE
    importing
      !I_MSG_TYPE type SYMSGTY .
  methods SET_SPR_STATUS
    importing
      !I_SPR_STATUS type /GDA/SDM_DE_SPRINT_STATUS .
  methods PROCESS
    importing
      !I_COMMIT type BOOLEAN .
  methods SET_EXTRA_V2
    importing
      !I_EXTRA_V2 type SYMSGV .
  methods SET_EXTRA_V3
    importing
      !I_EXTRA_V3 type SYMSGV .
  methods SET_EXTRA_V4
    importing
      !I_EXTRA_V4 type SYMSGV .
  methods SET_EXTRA_V5
    importing
      !I_EXTRA_V5 type SYMSGV .
  methods SET_EXTRA_V6
    importing
      !I_EXTRA_V6 type SYMSGV .
  class-methods BUILD_STRING_FROM_KEY
    importing
      !I_TABNAME type TABNAME
      !I_CONTENTS type ANY
    returning
      value(R_TABKEY) type CDTABKEY .
protected section.
private section.

  data PT_SDM_PERSIST_MAIN type /GDA/SDM_T_PERSIST_MAIN .
  data PS_SDM_PERSIST_MAIN type /GDA/SDM_S_PERSIST_MAIN .
ENDCLASS.



CLASS /GDA/CL_SDM_DATA_MODEL_MAIN IMPLEMENTATION.


  METHOD build_string_from_key.

* get key fields for table

    TYPES: BEGIN OF s_tab_fields,
             field TYPE fieldname,
           END OF s_tab_fields.

    DATA: lo_strucdescr TYPE REF TO cl_abap_structdescr,
          lt_tab_fields TYPE ddfields,
          lt_fields     TYPE STANDARD TABLE OF s_tab_fields,
          ls_fields     TYPE s_tab_fields.

    FIELD-SYMBOLS:
      <tab_field> TYPE dfies,
      <contents>  TYPE any,
      <key_field> TYPE any.

    ASSIGN i_contents TO <contents>.

    TRY .
*   Get the details of the DDIC table
        lo_strucdescr ?= cl_abap_elemdescr=>describe_by_name( i_tabname ).
      CATCH cx_sy_move_cast_error .
        MESSAGE 'Error while casting' TYPE 'S'. RETURN.
    ENDTRY.

* Check if input is a DDIC table
    CHECK lo_strucdescr->is_ddic_type( ) = 'X'.

* Get the details of the table fields
    lt_tab_fields = lo_strucdescr->get_ddic_field_list( ).

* Display the Key fields of the table
    LOOP AT lt_tab_fields ASSIGNING <tab_field> WHERE keyflag = 'X'.
      ASSIGN COMPONENT <tab_field>-fieldname OF STRUCTURE <contents> TO <key_field>.
      CHECK sy-subrc = 0.
      CONCATENATE r_tabkey <key_field> INTO r_tabkey.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    DATA:
      sdm_agent      TYPE REF TO /gda/ca_sdm_persistent_main,
      ls_sdm_exc_mai TYPE /gda/sdm_exc_mai,
      lv_method      TYPE string,
      lv_datum       TYPE datum.

*  i_sdm_tabkey     = 'A10001'.
*  i_sdm_object_id  = '01'.
*  i_tabname        = 'MARA'.
*  i_field          = 'MATNR'.
*  i_msg_id         = '/GDA/SDM'.
*  i_msg_number     = '001'.

    sdm_agent = /gda/ca_sdm_persistent_main=>agent.

* Check to see if an entry already exists...ignore begda!
    SELECT SINGLE * FROM /gda/sdm_exc_mai INTO ls_sdm_exc_mai
                    WHERE sdm_object_id = i_sdm_object_id
                      AND sdm_type      = i_sdm_type
                      AND sdm_tabkey    = i_sdm_tabkey
                      AND tabname       = i_tabname
                      AND field         = i_field
                      AND msg_id        = i_msg_id
                      AND msg_number    = i_msg_number.

    IF sy-subrc = '0'.
      ps_sdm_persist_main-type = 'U'.
      lv_method = 'GET_PERSISTENT'.
      lv_datum  = ls_sdm_exc_mai-begda.
    ELSE.
      ps_sdm_persist_main-type = 'C'.
      lv_method = 'CREATE_PERSISTENT'.
      lv_datum  = sy-datum.
    ENDIF.

    TRY.
        CALL METHOD sdm_agent->(lv_method)
          EXPORTING
            i_sdm_object_id = i_sdm_object_id
            i_sdm_type      = i_sdm_type
            i_sdm_tabkey    = i_sdm_tabkey
            i_tabname       = i_tabname
            i_field         = i_field
            i_msg_id        = i_msg_id
            i_msg_number    = i_msg_number
            i_begda         = lv_datum
          RECEIVING
            result          = ps_sdm_persist_main-main.

      CATCH cx_os_object_not_found .
    ENDTRY.

    APPEND ps_sdm_persist_main TO pt_sdm_persist_main.

  ENDMETHOD.


  METHOD process.
    IF ps_sdm_persist_main-type = 'C'.
      TRY.
          CALL METHOD ps_sdm_persist_main-main->set_endda( i_endda = '99991231' ).

          CALL METHOD ps_sdm_persist_main-main->set_spr_status( i_spr_status = '1' ). "Initial always set to Backlog
        CATCH cx_os_object_not_found .
      ENDTRY.
    ELSEIF ps_sdm_persist_main-type = 'U'.

    ENDIF.

    IF i_commit = abap_true.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD set_endda.

    TRY.
        CALL METHOD ps_sdm_persist_main-main->set_endda
          EXPORTING
            i_endda = i_endda.
      CATCH cx_os_object_not_found .
    ENDTRY.

  ENDMETHOD.


  METHOD set_extra_v2.
    TRY.
        ps_sdm_persist_main-main->set_extra_v2( i_extra_v2 = i_extra_v2 ).
      CATCH cx_os_object_not_found .
    ENDTRY.

  ENDMETHOD.


  METHOD SET_EXTRA_V3.
    TRY.
        ps_sdm_persist_main-main->set_extra_v3( i_extra_v3 = i_extra_v3 ).
      CATCH cx_os_object_not_found .
    ENDTRY.

  ENDMETHOD.


  METHOD SET_EXTRA_V4.
    TRY.
        ps_sdm_persist_main-main->set_extra_v4( i_extra_v4 = i_extra_v4 ).
      CATCH cx_os_object_not_found .
    ENDTRY.

  ENDMETHOD.


  METHOD SET_EXTRA_V5.
    TRY.
        ps_sdm_persist_main-main->set_extra_v5( i_extra_v5 = i_extra_v5 ).
      CATCH cx_os_object_not_found .
    ENDTRY.

  ENDMETHOD.


  METHOD SET_EXTRA_V6.
    TRY.
        ps_sdm_persist_main-main->set_extra_v6( i_extra_v6 = i_extra_v6 ).
      CATCH cx_os_object_not_found .
    ENDTRY.

  ENDMETHOD.


  METHOD set_msg_type.
    TRY.
        ps_sdm_persist_main-main->set_msg_type( i_msg_type = i_msg_type ).
      CATCH cx_os_object_not_found .
    ENDTRY.

  ENDMETHOD.


  method SET_SPR_STATUS.
  endmethod.
ENDCLASS.
