*----------------------------------------------------------------------*
***INCLUDE /GDA/LSDM_ABAP_NEWF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0006   text
*      -->P_0007   text
*      <--P_LT_FIELDS  text
*----------------------------------------------------------------------*
FORM build_field_selection USING    iv_struc_name TYPE string
                                    iv_pref       TYPE string
                           CHANGING xt_field_list TYPE ANY TABLE.
  DATA:
    lo_type_descr   TYPE REF TO cl_abap_typedescr,
    lo_struct_descr TYPE REF TO cl_abap_structdescr,
    lt_field_list   TYPE TABLE OF char30,
    lv_last         TYPE i.

  FIELD-SYMBOLS:
    <ls_components> TYPE LINE OF abap_compdescr_tab,
    <ls_field_list> TYPE char30.


  cl_abap_typedescr=>describe_by_name(
  EXPORTING
    p_name         =  iv_struc_name
    RECEIVING
    p_descr_ref    =  lo_type_descr
  EXCEPTIONS
    type_not_found = 1
    OTHERS         = 2
    ).
  IF sy-subrc = 0.
    lo_struct_descr ?= lo_type_descr.
  ELSE.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            INTO me->mv_message
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

**// Build Dynamic Field Selection
  DESCRIBE TABLE lo_struct_descr->components LINES lv_last.
  LOOP AT lo_struct_descr->components ASSIGNING <ls_components>.
    APPEND INITIAL LINE TO lt_field_list ASSIGNING <ls_field_list>.
    IF sy-tabix = lv_last.
      CONCATENATE iv_pref <ls_components>-name INTO <ls_field_list>.
    ELSE.
      CONCATENATE iv_pref <ls_components>-name ',' INTO <ls_field_list>.
    ENDIF.
  ENDLOOP.

  xt_field_list[] =   lt_field_list[].
ENDFORM.
