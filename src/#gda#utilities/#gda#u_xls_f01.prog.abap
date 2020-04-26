*----------------------------------------------------------------------*
***INCLUDE /GDA/U_XLS_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF v_client = space.
    IF v_dis = '1'.
      REFRESH tab.
      CLEAR tab.
    ELSE.
*      REFRESH tab.
*      CLEAR tab.
*      MOVE 'SAVE' TO tab-ucomm.
*      APPEND tab.
    ENDIF.
  ENDIF.

  PERFORM check_client.

  SET PF-STATUS 'MAIN' EXCLUDING tab.
  SET TITLEBAR '100' WITH document_name.

*------Excel Control
  PERFORM load_excel_control.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  LOAD_EXCEL_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_excel_control .
  retcode = c_oi_errors=>ret_ok.
  IF control IS INITIAL.
    DATA: b_has_activex.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = b_has_activex.
    IF b_has_activex IS INITIAL.
      MESSAGE e007.
    ENDIF.

    CALL METHOD c_oi_container_control_creator=>get_container_control
      IMPORTING
        control = control
        retcode = retcode.
    CALL METHOD c_oi_errors=>raise_message
      EXPORTING
        type = 'E'.

    CREATE OBJECT container
      EXPORTING
        container_name = 'CONTAINER'.

    CALL METHOD control->init_control
      EXPORTING
        r3_application_name      = 'Document'(000)
        inplace_enabled          = 'X'
        inplace_scroll_documents = 'X'
        parent                   = container
        register_on_close_event  = 'X'
        register_on_custom_event = 'X'
      IMPORTING
        retcode                  = retcode.
    CALL METHOD c_oi_errors=>raise_message
      EXPORTING
        type = 'E'.

  ENDIF.

*  IF bds_instance IS INITIAL.
*    CREATE OBJECT bds_instance.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CLIENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_client .
*  CHECK v_client IS INITIAL.
*
**  CHECK sy-mandt(1) NE '2'.
*
*  MOVE 'SAVE' TO tab-ucomm.
*  COLLECT tab.
*  MOVE 'CREATE' TO tab-ucomm.
*  COLLECT tab.
*  MOVE 'CHANGE' TO tab-ucomm.
*  COLLECT tab.
*  MOVE 'DELETE' TO tab-ucomm.
*  COLLECT tab.
*  MOVE 'COPY' TO tab-ucomm.
*  COLLECT tab.

  v_client = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: l_fcode LIKE fcode.
  l_fcode = sy-ucomm.
  CLEAR fcode.

  MOVE sy-ucomm TO v_ucomm.

  CALL METHOD cl_gui_cfw=>dispatch.

  CASE l_fcode.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.

*      IF NOT document IS INITIAL.
*        CALL METHOD document->has_changed.
*          EXPORTING
*            document_proxy = data_proxy
*            has_changed    = v_change.
*      ENDIF.

      PERFORM check_changes.

    WHEN 'DELETE'.
      PERFORM get_data.
      PERFORM delete_data.
      v_dis = '1'.

    WHEN 'CREATE'.

      IF NOT control IS INITIAL.

        IF NOT document IS INITIAL.
          CALL METHOD document->close_document.
        ENDIF.

        CLEAR: report, document_name.

*------You can determine here what type of document you want to create
        document_type = 'Excel.Sheet'.          " XLS
*        document_type = 'Word.Document'.       " DOC
*        document_type = 'AcroExch.Document'.   " PDF
*        document_type = 'PowerPoint.Show'.      " Power Point

        PERFORM get_document_name.

        CHECK NOT document_name IS INITIAL
        AND   NOT report IS INITIAL.

        v_dis = '1'.

        CREATE OBJECT document
          EXPORTING
            control         = control
            document_type   = document_type
            document_format = soi_docformat_compound.

        CALL METHOD document->create_document
          EXPORTING
            open_inplace = 'X'
          IMPORTING
            retcode      = retcode.

        CALL METHOD c_oi_errors=>raise_message
          EXPORTING
            type = 'E'.
      ENDIF.

    WHEN 'CHANGE'.
      PERFORM get_data.
      IF NOT document IS INITIAL AND document->data_size NE 0.
        IF NOT control IS INITIAL.
          v_dis = '1'.
          CALL METHOD document->open_document
            EXPORTING
              open_inplace  = 'X'
              open_readonly = ' '
            IMPORTING
              retcode       = retcode.
          CALL METHOD c_oi_errors=>raise_message
            EXPORTING
              type = 'E'.
        ENDIF.
      ELSE.
*        MESSAGE e005.
      ENDIF.

    WHEN 'DISPLAY'.
      PERFORM get_data.
      IF NOT document IS INITIAL AND document->data_size NE 0.
        IF NOT control IS INITIAL.
          v_dis = '0'.
          CALL METHOD document->open_document
            EXPORTING
              open_inplace  = 'X'
              open_readonly = 'X'
              protect       = 'X'
            IMPORTING
              retcode       = retcode.
          CALL METHOD c_oi_errors=>raise_message
            EXPORTING
              type = 'E'.
        ENDIF.
      ELSE.
*        MESSAGE e005.
      ENDIF.

    WHEN 'COPY'.
      PERFORM get_data.
      CHECK NOT izdoch IS INITIAL.
      PERFORM get_document_name.
      CHECK NOT document_name IS INITIAL
      AND   NOT report IS INITIAL.
      PERFORM copy_template.

      IF NOT document IS INITIAL AND document->data_size NE 0.
        IF NOT control IS INITIAL.
          v_dis = '1'.
          CALL METHOD document->open_document
            EXPORTING
              open_inplace  = 'X'
              open_readonly = ' '
            IMPORTING
              retcode       = retcode.
          CALL METHOD c_oi_errors=>raise_message
            EXPORTING
              type = 'E'.
        ENDIF.
      ENDIF.

    WHEN 'PRINT'.
      PERFORM print_document.

    WHEN 'SAVE'.

      PERFORM save_document.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data .
  DATA: answer.
  DATA: text(100).

  CHECK NOT izdoch-repid IS INITIAL
  AND   NOT izdoch-name  IS INITIAL.

  CONCATENATE 'Delete'(101) izdoch-name '?' INTO
  text SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Message'(102)
      diagnose_object       = ' '
      text_question         = text
      text_button_1         = 'Yes'(103)
      icon_button_1         = ' '
      text_button_2         = 'No'(104)
      icon_button_2         = ' '
      default_button        = '1'
      display_cancel_button = 'X'
      userdefined_f1_help   = ' '
      start_column          = 25
      start_row             = 6
*     POPUP_TYPE            =
    IMPORTING
      answer                = answer
*     TABLES
*     PARAMETER             =
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc = 0.
    CASE answer.

      WHEN '1'.  " Yes
        CLEAR: document_name.

        DELETE FROM /gda/doch
        WHERE repid = izdoch-repid
        AND   name = izdoch-name.

        DELETE FROM /gda/doci
        WHERE repid = izdoch-repid
        AND   name = izdoch-name.

        COMMIT WORK.
        MESSAGE s000 WITH izdoch-name 'has been deleted'(105).

        CLEAR: izdoch, izdoci, lines, document->data_size.
        REFRESH: izdoch, izdoci, lines, document->data_table.

      WHEN '2'.  " No
        CLEAR: document_name.

      WHEN 'A'.  " Cancel
        CLEAR: document_name.

    ENDCASE.
  ENDIF.
ENDFORM.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  print_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_document .
  DATA: descr_list TYPE soi_document_type_descr_list
            WITH HEADER LINE.
  DATA: field_desc TYPE TABLE OF rsvbfidesc.
  DATA: wa_field_desc TYPE rsvbfidesc.
*  DATA: descr TYPE soi_document_type_descr.
  DATA: l_nr LIKE sy-tabix.

* Get sheet names in the spreadsheet
  IF NOT oi_spreadsheet IS INITIAL.
    CALL METHOD oi_spreadsheet->get_sheets
      IMPORTING
        sheets = i_sheets.
  ENDIF.

  CLEAR: field_desc, wa_field_desc.
  wa_field_desc-fieldnum = 4.
  wa_field_desc-display = 'X'.
  wa_field_desc-fieldname = 'DOCUMENT_TYPE'.
  wa_field_desc-col_head = 'Print Sheet'(106).
  APPEND wa_field_desc TO field_desc.

  LOOP AT i_sheets INTO wa_sheets.

    MOVE wa_sheets-sheet_name TO descr_list-type_full_name.
    MOVE wa_sheets-sheet_name TO descr_list-application_name.
    APPEND descr_list.

  ENDLOOP.

  l_nr = 0.
  CALL FUNCTION 'RS_VALUES_BOX'
    EXPORTING
      column_heading = 'X'
      left_upper_col = 5
      left_upper_row = 5
      pagesize       = 10
      title          = 'Spreadsheet'(107)
    IMPORTING
      linenumber     = l_nr
    TABLES
      field_desc     = field_desc
      value_tab      = descr_list[]
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc EQ 0 AND l_nr NE 0.
    READ TABLE i_sheets INTO wa_sheets INDEX l_nr.
  ELSE.
    CLEAR wa_sheets.
  ENDIF.

  CHECK NOT wa_sheets-sheet_name IS INITIAL.

*------Print Sheet
  CALL METHOD oi_spreadsheet->print
    EXPORTING
      name = wa_sheets-sheet_name.

ENDFORM.                    " print_document
*&---------------------------------------------------------------------*
*&      Form  check_changes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_changes .
  DATA:
    answer,
    text(100).

  IF v_change NE 0.

    MOVE 'Save changes before exiting ?'(108) TO text.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Message'(102)
        diagnose_object       = ' '
        text_question         = text
        text_button_1         = 'Yes'
        icon_button_1         = ' '
        text_button_2         = 'No'
        icon_button_2         = ' '
        default_button        = '1'
        display_cancel_button = 'X'
        userdefined_f1_help   = ' '
        start_column          = 25
        start_row             = 6
*       POPUP_TYPE            =
      IMPORTING
        answer                = answer
*     TABLES
*       PARAMETER             =
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc = 0.
      CASE answer.

        WHEN '1'.  " Yes

          PERFORM save_document.
          LEAVE PROGRAM.

        WHEN '2'.  " No

          IF NOT document IS INITIAL.
            CALL METHOD document->close_document.
            FREE document.
          ENDIF.
          IF NOT control IS INITIAL.
            CALL METHOD control->destroy_control
              IMPORTING
                retcode = retcode.
            FREE control.
          ENDIF.

*        IF NOT bds_instance IS INITIAL.
*          FREE bds_instance.
*        ENDIF.

          LEAVE PROGRAM.

        WHEN 'A'.  " Cancel

      ENDCASE.
    ENDIF.
  ELSE.

    IF NOT document IS INITIAL.
      CALL METHOD document->close_document.
      FREE document.
    ENDIF.
    IF NOT control IS INITIAL.
      CALL METHOD control->destroy_control
        IMPORTING
          retcode = retcode.
      FREE control.
    ENDIF.

*    IF NOT bds_instance IS INITIAL.
*      FREE bds_instance.
*    ENDIF.

    LEAVE PROGRAM.

  ENDIF.

ENDFORM.                    " check_changes
*&---------------------------------------------------------------------*
*&      Form  load_excel_control
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM load_excel_control .
*
*  retcode = c_oi_errors=>ret_ok.
*  IF control IS INITIAL.
*    DATA: b_has_activex.
*
*    CALL FUNCTION 'GUI_HAS_ACTIVEX'
*      IMPORTING
*        return = b_has_activex.
**    IF b_has_activex IS INITIAL. MESSAGE e007. ENDIF.
*
*    CALL METHOD c_oi_container_control_creator=>get_container_control
*      IMPORTING
*        control = control
*        retcode = retcode.
*    CALL METHOD c_oi_errors=>raise_message
*      EXPORTING
*        type = 'E'.
*
*    CREATE OBJECT container
*              EXPORTING container_name = 'CONTAINER'.
*
*    CALL METHOD control->init_control
*      EXPORTING
*        r3_application_name      = 'Document'(000)
*        inplace_enabled          = 'X'
*        inplace_scroll_documents = 'X'
*        parent                   = container
*        register_on_close_event  = 'X'
*        register_on_custom_event = 'X'
*      IMPORTING
*        retcode                  = retcode.
*    CALL METHOD c_oi_errors=>raise_message
*      EXPORTING
*        type = 'E'.
*
*  ENDIF.
*
*  IF bds_instance IS INITIAL.
*    CREATE OBJECT bds_instance.
*  ENDIF.
*
*ENDFORM.                    " load_excel_control

FORM copy_template .

  READ TABLE izdoch INDEX 1.
  CHECK sy-subrc = 0.
  MOVE report TO izdoch-repid.
  MOVE document_name TO izdoch-name.
  MODIFY izdoch INDEX sy-tabix.

  LOOP AT izdoci.

    MOVE report TO izdoci-repid.
    MOVE document_name TO izdoci-name.
    MODIFY izdoci.

  ENDLOOP.

ENDFORM.

FORM get_data.

*-----Header table
  SELECT *
  FROM /gda/doch
  INTO TABLE izdoch.

  PERFORM get_list.

  CHECK NOT izdoch IS INITIAL.

  document_type = 'Excel.Sheet'.
  document_name = izdoch-name.
  report = izdoch-repid.

  LOOP AT izdoch.
    IF izdoch-repid = report
    AND izdoch-name = document_name.
    ELSE.
      DELETE izdoch.
    ENDIF.
  ENDLOOP.
  READ TABLE izdoch WITH KEY repid = report
                             name = document_name.
  CHECK sy-subrc = 0.

  REFRESH lines.

*-----Item table
  SELECT *
  FROM /gda/doci
  INTO TABLE izdoci
  FOR ALL ENTRIES IN izdoch
  WHERE repid = izdoch-repid
  AND   name = izdoch-name.

  LOOP AT izdoci
  WHERE repid = report
  AND   name = document_name.
    MOVE-CORRESPONDING izdoci TO lines.
    APPEND lines.
  ENDLOOP.

*-----Close document if open
  IF NOT document IS INITIAL.
    CALL METHOD document->close_document.
  ENDIF.

*-----Create document
  CREATE OBJECT document
    EXPORTING
      control         = control
      document_type   = document_type
      document_format = soi_docformat_compound.

  MOVE izdoch-length TO document->data_size.

  IF document->data_table[] NE lines[].
    document->data_table[] = lines[].
  ENDIF.

ENDFORM.                    " get_data

FORM get_document_name.

  CALL SCREEN 101 STARTING AT 5 5 ENDING AT 95 7.

ENDFORM.                    " get_document_name

FORM get_list.

  DATA: descr_list TYPE soi_document_type_descr_list
        WITH HEADER LINE.
  DATA: field_desc TYPE TABLE OF rsvbfidesc.
  DATA: wa_field_desc TYPE rsvbfidesc.
*  DATA: descr TYPE soi_document_type_descr.
  DATA: l_nr LIKE sy-tabix.

  CLEAR: field_desc, wa_field_desc.
  wa_field_desc-fieldnum = 4.
  wa_field_desc-display = 'X'.
  wa_field_desc-fieldname = 'DOCUMENT_TYPE'.
  wa_field_desc-col_head = 'Template'(109).
  APPEND wa_field_desc TO field_desc.
  CLEAR wa_field_desc.
  wa_field_desc-fieldnum = 3.
  wa_field_desc-display = 'X'.
  wa_field_desc-fieldname = 'DOCUMENT_TYPE'.
  wa_field_desc-col_head = 'Report'(110).
  APPEND wa_field_desc TO field_desc.

  LOOP AT izdoch.

    MOVE izdoch-repid TO descr_list-document_type.
    MOVE izdoch-name TO descr_list-type_full_name.
    MOVE izdoch-repid TO descr_list-type_short_name.
    MOVE izdoch-name TO descr_list-application_name.
    APPEND descr_list.

  ENDLOOP.

  l_nr = 0.
  CALL FUNCTION 'RS_VALUES_BOX'
    EXPORTING
      column_heading = 'X'
*     CURSOR_FIELD   = 1
*     CURSOR_LINE    = 1
      left_upper_col = 5
      left_upper_row = 5
      pagesize       = 10
      title          = 'Documents'(117)
    IMPORTING
      linenumber     = l_nr
    TABLES
      field_desc     = field_desc
      value_tab      = descr_list[]
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc EQ 0 AND l_nr NE 0.
    READ TABLE izdoch INDEX l_nr.
  ELSE.
    CLEAR izdoch.
  ENDIF.

ENDFORM.                    " get_list

FORM save_document.
  DATA: error TYPE REF TO i_oi_error.

  CHECK NOT document IS INITIAL.

  CALL METHOD document->close_document
    EXPORTING
      do_save = 'X'
    IMPORTING
      error   = error.

  CALL METHOD error->raise_message
    EXPORTING
      type = 'W'.

  v_dis = '1'.

  REFRESH: izdoci, lines.

  lengh = document->data_size.
  lines[] = document->data_table.

  IF lengh IS INITIAL.
    MESSAGE s000 WITH 'No changes made'(111).
    EXIT.
  ENDIF.

  DELETE FROM /gda/doch
  WHERE repid = izdoch-repid
  AND   name = izdoch-name.

  READ TABLE izdoch WITH KEY repid = report
                             name = document_name.
  IF sy-subrc = 0.
    MOVE document->data_size TO izdoch-length.
    MODIFY izdoch INDEX sy-tabix.
  ELSE.
    MOVE sy-mandt TO izdoch-mandt.
    MOVE report TO izdoch-repid.
    MOVE document_name TO izdoch-name.
    MOVE document->data_size TO izdoch-length.
    APPEND izdoch.
  ENDIF.

  DELETE FROM /gda/doci
  WHERE repid = izdoch-repid
  AND   name = izdoch-name.

  LOOP AT lines.
    MOVE sy-mandt TO izdoci-mandt.
    MOVE izdoch-repid TO izdoci-repid.
    MOVE izdoch-name TO izdoci-name.
    MOVE sy-tabix TO izdoci-seqno.
    MOVE-CORRESPONDING lines TO izdoci.
    APPEND izdoci.
  ENDLOOP.

  INSERT /gda/doch FROM TABLE izdoch.
  INSERT /gda/doci FROM TABLE izdoci.

  COMMIT WORK.

  MESSAGE s000 WITH 'Template saved'(112).

  CLEAR: lengh, izdoch, izdoci, report, document_name, v_change.
  REFRESH: izdoch, izdoci.

ENDFORM.                    " save_document
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS '101'.
  SET TITLEBAR '101'.
ENDMODULE.

MODULE user_command_0101 INPUT.

  CASE sy-ucomm.

*    WHEN 'OK' OR 'ENTE'.
    WHEN 'ENTE'.
      IF report IS INITIAL
      OR document_name IS INITIAL.
        MESSAGE i008.

      ELSE.

        SELECT SINGLE *
        FROM /gda/doch
        INTO izdoch
        WHERE repid = report
        AND   name  = document_name.

        IF sy-subrc = 0.
          MESSAGE e000 WITH 'Template for'(114) document_name 'Already Exists'(115).
        ENDIF.
      CLEAR izdoch.
      SET SCREEN 0.

      ENDIF.
*      CLEAR izdoch.
*      SET SCREEN 0.

    WHEN 'EXIT' OR 'OK'.
      CLEAR: report, document_name.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT
