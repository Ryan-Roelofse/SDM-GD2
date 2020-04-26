*&---------------------------------------------------------------------*
*& Report /GDA/U_XLS_TEMPLATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/u_xls_template MESSAGE-ID /gda/u1.

CALL SCREEN 100.

TYPE-POOLS:
  soi,
  sbdst,
  swww.

CLASS c_oi_errors DEFINITION LOAD.

DATA:
  fcode             LIKE sy-ucomm,
  v_dis,
  v_change          TYPE i,
  v_ucomm           LIKE sy-ucomm,
  oi_spreadsheet    TYPE REF TO i_oi_spreadsheet,
  control           TYPE REF TO i_oi_container_control,
  container         TYPE REF TO cl_gui_custom_container,
  retcode           TYPE soi_ret_string,
  document_type     TYPE soi_document_type,
  report(40),
  document_name(40),
  lengh             TYPE int4,
  v_client,
  izdoch            TYPE /gda/doch OCCURS 0 WITH HEADER LINE,
  izdoci            TYPE /gda/doci OCCURS 0 WITH HEADER LINE,
  i_sheets          TYPE soi_sheets_table,
  wa_sheets         TYPE LINE OF soi_sheets_table.

DATA: BEGIN OF lines OCCURS 0,
        line1 LIKE /gda/doci-line1,
        line2 LIKE /gda/doci-line2,
        line3 LIKE /gda/doci-line3,
        line4 LIKE /gda/doci-line4,
        line5 LIKE /gda/doci-line5,
      END OF lines.

DATA: BEGIN OF tab OCCURS 0,
        ucomm LIKE sy-ucomm,
      END OF tab.


CLASS c_office_document DEFINITION FINAL.

  PUBLIC SECTION.
    DATA:
      proxy                 TYPE REF TO i_oi_document_proxy,
      document_type         TYPE soi_document_type,
      document_format       TYPE soi_document_type,
      has_changed_at_reopen TYPE i,
      data_table            TYPE sbdst_content,
      data_size             TYPE i.

    METHODS: constructor
      IMPORTING control         TYPE REF TO i_oi_container_control
                document_type   TYPE c
                document_format TYPE c.

*    METHODS: has_changed
**              FOR EVENT HAS_CHANGED OF i_oi_document_proxy
*      EXPORTING retcode   TYPE soi_ret_string
*                error     TYPE REF TO i_oi_error
*                ret_value TYPE i.

    METHODS: create_document
      IMPORTING open_inplace TYPE c DEFAULT ' '
      EXPORTING retcode      TYPE soi_ret_string.

    METHODS: open_document
      IMPORTING open_inplace  TYPE c DEFAULT ' '
                open_readonly TYPE c DEFAULT ' '
                protect       TYPE c DEFAULT ' '
      EXPORTING retcode       TYPE soi_ret_string.

*    METHODS: view_document
*      EXPORTING retcode TYPE soi_ret_string.

    METHODS: close_document
      IMPORTING do_save TYPE c DEFAULT ' '
      EXPORTING retcode TYPE soi_ret_string
                error   TYPE REF TO i_oi_error.

  PRIVATE SECTION.
    DATA: control  TYPE REF TO i_oi_container_control.
ENDCLASS.                    "c_office_document DEFINITION

************************************************************************
*  CLASS c_office_document IMPLEMENTATION.
************************************************************************
CLASS c_office_document IMPLEMENTATION.
  METHOD: constructor.
    me->control = control.
    me->document_type = document_type.
    me->document_format = document_format.
  ENDMETHOD.                    "constructor

  METHOD create_document.
    IF NOT proxy IS INITIAL.
      CALL METHOD me->close_document.
    ENDIF.
    CALL METHOD control->get_document_proxy
      EXPORTING
        document_type   = document_type
        document_format = document_format
      IMPORTING
        document_proxy  = proxy
        retcode         = retcode.
    IF retcode NE c_oi_errors=>ret_ok.
      EXIT.
    ENDIF.

    CALL METHOD proxy->create_document
      EXPORTING
        create_view_data = 'X'
        open_inplace     = open_inplace
      IMPORTING
        retcode          = retcode.
    IF retcode NE c_oi_errors=>ret_ok.
      EXIT.
    ENDIF.

*    SET HANDLER me->on_close_document FOR proxy.
  ENDMETHOD.                    "create_document

  METHOD open_document.
    IF NOT proxy IS INITIAL.
      CALL METHOD me->close_document.
    ENDIF.
    CALL METHOD control->get_document_proxy
      EXPORTING
        document_type   = document_type
        document_format = document_format
      IMPORTING
        document_proxy  = proxy
        retcode         = retcode.
    IF retcode NE c_oi_errors=>ret_ok.
      EXIT.
    ENDIF.

    CALL METHOD proxy->open_document_from_table
      EXPORTING
        document_table   = data_table
        document_size    = data_size
        open_inplace     = open_inplace
        open_readonly    = open_readonly
        document_title   = izdoch-name
        protect_document = protect
        no_flush         = 'X'
      IMPORTING
        retcode          = retcode.
    IF retcode NE c_oi_errors=>ret_ok.
      EXIT.
    ENDIF.

    CALL METHOD proxy->get_spreadsheet_interface
      IMPORTING
        sheet_interface = oi_spreadsheet.

    CHECK v_ucomm = 'DISPLAY'.

    CALL METHOD proxy->delete_menu_item
      EXPORTING
        menu_popup_name = 'Protection'(121)
        item_name       = 'Save Copy As...'(119)
        no_flush        = 'X'.

    CALL METHOD proxy->delete_menu_item
      EXPORTING
        menu_popup_name = 'Tools'(120)
        item_name       = 'Protection'(121)
        no_flush        = 'X'.

  ENDMETHOD.                    "open_document

*  METHOD view_document.
*
*    IF NOT proxy IS INITIAL.
*      CALL METHOD me->close_document.
*    ENDIF.
*    CALL METHOD control->get_document_proxy
*      EXPORTING
*        document_type   = document_type
*        document_format = document_format
*      IMPORTING
*        document_proxy  = proxy
*        retcode         = retcode.
*    IF retcode NE c_oi_errors=>ret_ok.
*      EXIT.
*    ENDIF.
*
*    CALL METHOD proxy->view_document_from_table
*      EXPORTING
*        document_table = data_table
*        document_size  = data_size
*        open_inplace   = 'X'
*      IMPORTING
*        retcode        = retcode.
*    IF retcode NE c_oi_errors=>ret_ok.
*      EXIT.
*    ENDIF.
*
*  ENDMETHOD.                    "view_document

  METHOD close_document.
    DATA: has_changed TYPE i, is_closed TYPE i.
    DATA: save_error TYPE REF TO i_oi_error.

    IF NOT proxy IS INITIAL.
      CALL METHOD proxy->is_destroyed
        IMPORTING
          ret_value = is_closed.

      IF is_closed IS INITIAL.
        CALL METHOD proxy->close_document
          EXPORTING
            do_save     = do_save
          IMPORTING
            has_changed = has_changed
            retcode     = retcode
            error       = error.
        IF retcode NE c_oi_errors=>ret_ok.
          EXIT.
        ENDIF.
      ENDIF.

      IF NOT has_changed IS INITIAL OR
       ( NOT has_changed_at_reopen IS INITIAL
       AND NOT do_save IS INITIAL ).
        CALL METHOD proxy->save_document_to_table
          EXPORTING
            no_flush       = 'X'
          IMPORTING
            error          = save_error
          CHANGING
            document_table = data_table
            document_size  = data_size.
      ENDIF.
      CALL METHOD proxy->release_document
        IMPORTING
          retcode = retcode
          error   = error.
      IF NOT save_error IS INITIAL.
        IF save_error->error_code NE c_oi_errors=>ret_ok.
          CALL METHOD save_error->flush_error.
          retcode = save_error->error_code.
          error = save_error.
        ENDIF.
      ENDIF.

      CLEAR: has_changed_at_reopen.
    ELSE.
      retcode = c_oi_errors=>ret_document_not_open.
    ENDIF.
  ENDMETHOD.                    "close_document

*  METHOD has_changed.
*    DATA: is_closed TYPE i.
*    DATA: save_error TYPE REF TO i_oi_error.
*
*    CHECK v_change = 0.
*
*    IF NOT proxy IS INITIAL.
*      CALL METHOD proxy->is_destroyed
*        IMPORTING
*          ret_value = is_closed.
*
**      CALL METHOD proxy->has_changed
**        IMPORTING
**          ret_value = v_change
**          retcode   = retcode
**          error     = error.
*
*      CALL METHOD proxy->close_document
*        EXPORTING
*          do_save     = 'X'
**         no_flush    = 'X'
*        IMPORTING
*          has_changed = v_change
*          retcode     = retcode
*          error       = error.
*      IF retcode NE c_oi_errors=>ret_ok.
*        EXIT.
*      ENDIF.
*
*      CHECK v_change NE 0.
*
*      CALL METHOD proxy->reopen_document
*        EXPORTING
*          open_inplace = 'X'
*          no_flush     = 'X'
*        IMPORTING
*          retcode      = retcode.
*      IF retcode NE c_oi_errors=>ret_ok.
*        EXIT.
*      ENDIF.
*
**    CALL METHOD proxy->has_changed
**      IMPORTING
**        ret_value = v_change
**        retcode   = retcode
**        error     = error.
*
*    ENDIF.
*
*  ENDMETHOD.                    "has_changed.

ENDCLASS.                    "c_office_document IMPLEMENTATION


DATA: document TYPE REF TO c_office_document.
*DATA: bds_instance TYPE REF TO cl_bds_document_set.

INCLUDE /gda/u_xls_f01.
