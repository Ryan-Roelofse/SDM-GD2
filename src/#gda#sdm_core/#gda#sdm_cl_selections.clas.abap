class /GDA/SDM_CL_SELECTIONS definition
  public
  create protected .

public section.

  types:
    BEGIN OF ty_selscreen_base,
* SDM Common
      msgno             TYPE RANGE OF bapiret2-number,
      max_rows          TYPE p_dbacc,
      record_statistics TYPE c LENGTH 1,
      errors_only       TYPE c LENGTH 1,
    END OF ty_selscreen_base .

  class-methods FACTORY
    importing
      !IV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT
    returning
      value(RO_OBJECT) type ref to /GDA/SDM_CL_SELECTIONS .
  methods CONSTRUCTOR .
  methods SET_SELSCREEN_CORE
    importing
      !XS_SELSCREEN_BASE type TY_SELSCREEN_BASE optional .
  methods MAIN
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods BUILD_SPEC .
  methods REFRESH .
  methods LIMIT_MAX_ENTRIES
    exporting
      !EV_MAXNO type P_DBACC
      !EV_EXECUTE type ABAP_BOOL .
protected section.

  data MS_SELSCREEN_BASE type TY_SELSCREEN_BASE .
  data MV_MESSAGE type STRING .
  data:
    mt_field_list TYPE TABLE OF char30 .
  methods BUILD_FIELD_SELECTION
    importing
      !IV_STRUCT_NAME type STRING
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_SELECTIONS IMPLEMENTATION.


  method BUILD_FIELD_SELECTION.
    FIELD-SYMBOLS:
      <ls_components> TYPE LINE OF abap_compdescr_tab,
      <ls_field_list> TYPE char30.

    DATA:
      lo_type_descr   TYPE REF TO cl_abap_typedescr,
      lo_struct_descr TYPE REF TO cl_abap_structdescr.

    FREE me->mt_field_list.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         =  iv_struct_name
      RECEIVING
        p_descr_ref    =  lo_type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc = 0.
      lo_struct_descr ?= lo_type_descr.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  INTO me->mv_message
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

**// Build Dynamic Field Selection
    LOOP AT lo_struct_descr->components ASSIGNING <ls_components>.
      IF <ls_components>-name = 'SDM_TABKEY'.
        CONTINUE.
      ENDIF.
      APPEND INITIAL LINE TO me->mt_field_list ASSIGNING <ls_field_list>.
      <ls_field_list> = <ls_components>-name.
    ENDLOOP.

  endmethod.


  METHOD BUILD_SPEC.

  ENDMETHOD.


  method CONSTRUCTOR.
  endmethod.


  METHOD factory.
    DATA:
     lv_class  TYPE classname.

    SELECT SINGLE class_select
      INTO lv_class
      FROM /gda/sdm_setup6
      WHERE sdm_object = iv_object_type.

    CREATE OBJECT ro_object TYPE (lv_class).
  ENDMETHOD.


  method LIMIT_MAX_ENTRIES.

  DATA: lv_question    TYPE c LENGTH 200,
        lv_answer      TYPE c LENGTH 1,
        lv_max_entries TYPE string.

*  cv_max_no = 0.
  EV_EXECUTE = abap_true.

*  IF EV_MAXNO = '000000'.
*    lv_max_entries = gc_maxdb_entries.
*  ELSE.
*    lv_max_entries = cv_max_no.
*  ENDIF.
*
*  SHIFT lv_max_entries LEFT DELETING LEADING '0'.
*
*  lv_question = |No data entered into Selection-screen. The number of database entries will be restricted to |
*                && lv_max_entries && |. Continue?|.
*
**  IF sy-batch = space AND ( gs_sscrfields-ucomm = 'ONLI' OR gs_sscrfields-ucomm = 'PRIN' ) AND gv_selection_fields_entered = abap_false.
*  IF sy-batch = space AND  gv_selection_fields_entered = abap_false.
*    PERFORM popup_to_confirm USING lv_question CHANGING lv_answer.
*    IF lv_answer = '1'. "Yes
*      cv_max_no = lv_max_entries. "gc_maxdb_entries.
*    ELSE.
*      cv_execute_report = abap_false.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.

*FORM popup_to_confirm USING iv_question TYPE c
*                      CHANGING cv_answer TYPE flag.
*
*  DATA:
*    lv_answer TYPE c LENGTH 1.
*
*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
*      text_question  = iv_question
*    IMPORTING
*      answer         = lv_answer
*    EXCEPTIONS
*      text_not_found = 1
*      OTHERS         = 2.
*
*  IF sy-subrc <> 0.
*    RETURN.
*  ENDIF.
*
*  cv_answer = lv_answer.

  endmethod.


  method MAIN.

  endmethod.


  METHOD REFRESH.

  ENDMETHOD.


  METHOD SET_SELSCREEN_CORE.
    me->ms_selscreen_base = xs_selscreen_base.
  ENDMETHOD.
ENDCLASS.
