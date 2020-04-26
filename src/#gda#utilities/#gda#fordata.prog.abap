*&---------------------------------------------------------------------*
*&  Include           /GDA/FORDATA
*&---------------------------------------------------------------------*
 TABLES:
   /gda/doch.

 TYPE-POOLS:
  soi,
  slis,
  sbdst,
  swww,
  vrm.

 CLASS c_oi_errors DEFINITION LOAD.

 TYPES: BEGIN OF document_descr,
          document_name(40), document_id(40),
        END OF document_descr.

 TYPES: document_list TYPE TABLE OF document_descr.

 DATA: BEGIN OF lines OCCURS 0,
         line1 LIKE /gda/doci-line1,
         line2 LIKE /gda/doci-line2,
         line3 LIKE /gda/doci-line3,
         line4 LIKE /gda/doci-line4,
         line5 LIKE /gda/doci-line5,
       END OF lines.

 DATA: BEGIN OF range_item OCCURS 0,
         name(30)   TYPE c,
         top        TYPE i,
         left       TYPE i,
         sheet(40),
         value(128),
       END OF range_item.

 DATA: BEGIN OF icolumn OCCURS 0,
         sheet(40),
         field(30),
         name(30),
       END OF icolumn.

 DATA: BEGIN OF hide_columns OCCURS 0,
         sheet(40),
         index     TYPE i,
         view      TYPE /gda/sdm_setup3-object_view,
       END OF hide_columns.

 DATA:
   go_control     TYPE REF TO i_oi_container_control,
   go_container   TYPE REF TO cl_gui_custom_container,
   go_spreadsheet TYPE REF TO i_oi_spreadsheet,
   gv_retcode     TYPE soi_ret_string,
   gv_doc_type    TYPE soi_document_type VALUE 'Excel.Sheet',
   gv_col_nr      TYPE i,
   lt_sheets      TYPE soi_sheets_table,
   ls_sheets      TYPE LINE OF soi_sheets_table,
   lv_flush(1)    VALUE 'X',
   v_item(10)     TYPE n,
   column(30),
   object_id(30)  VALUE 'Z',
   object_url(30) VALUE 'Z.GIF',
   vrm_name       TYPE vrm_id VALUE 'COLUMN',
   vrm_list       TYPE vrm_values,
   vrm_value      LIKE LINE OF vrm_list,
   fieldcat       TYPE slis_t_fieldcat_alv WITH HEADER LINE,
   izdoci         TYPE /gda/doci OCCURS 0  WITH HEADER LINE,
   idel_column    LIKE icolumn OCCURS 0 WITH HEADER LINE,
   gt_tab         TYPE STANDARD TABLE OF /gda/tab,
   gs_tab         TYPE /gda/tab,
   gv_tabix       LIKE sy-tabix,
   v_repid        LIKE sy-repid,
   v_row          TYPE i,
   v_col          TYPE i,
   name(30)       TYPE c,
   v_sheet(40),
   v_cell(6),
   v_sum(255),
   v_sum1(255),
   v_gtsum(255).

 RANGES:
  s_rname FOR /gda/doch-name.

 FIELD-SYMBOLS:
  <cell> TYPE any.

 CLASS lcl_event_handler DEFINITION.

   PUBLIC SECTION.

     METHODS: on_sapevent
                   FOR EVENT sapevent OF cl_gui_html_viewer
       IMPORTING action.
*    METHODS: on_sapevent_help
*                  FOR EVENT sapevent OF cl_gui_html_viewer
*      IMPORTING action.

 ENDCLASS.               "LCL_EVENT_HANDLER
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
 CLASS lcl_event_handler IMPLEMENTATION.

* this handles all the even related to the hyperlink click on the index
* side
   METHOD on_sapevent.

     IF NOT action IS INITIAL.
       ls_sheets-sheet_name = action.
     ENDIF.

   ENDMETHOD.                    "lcl_event_handler

* this handles all the even related to the hyperlink click on the
* help side.
*  METHOD on_sapevent_help.
*
*    IF NOT action IS INITIAL.
*      wa_sheets-sheet_name = action.
*    ENDIF.
*
*  ENDMETHOD.                    "lcl_event_handler

 ENDCLASS.               "lcl_event_handler

 DATA: gv_html_index_container TYPE REF TO cl_gui_custom_container.
 DATA: gv_html_index TYPE REF TO cl_gui_html_viewer.
 DATA: gv_evt_receiver  TYPE REF TO lcl_event_handler.
*DATA: gv_html_help TYPE REF TO cl_gui_html_viewer.

* variables for the event processing and registering.
 DATA: events    TYPE cntl_simple_events,
       wa_events TYPE cntl_simple_event.
*DATA: document     TYPE REF TO c_office_document.
* DATA: bds_instance TYPE REF TO cl_bds_document_set.


*---------------------------------------------------------------------*
*       CLASS c_office_document DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
 CLASS c_office_document DEFINITION.

   PUBLIC SECTION.
     DATA:
       proxy                 TYPE REF TO i_oi_document_proxy,
       document_type         TYPE soi_document_type ,
       document_format       TYPE soi_document_type,
       data_table            TYPE sbdst_content,
       has_changed_at_reopen TYPE i,
       data_size             TYPE i.

     METHODS: constructor
       IMPORTING control         TYPE REF TO i_oi_container_control
                 document_type   TYPE c
                 document_format TYPE c.

     METHODS: open_document
       IMPORTING open_inplace  TYPE c DEFAULT ' '
                 open_readonly TYPE c DEFAULT ' '
       EXPORTING retcode       TYPE soi_ret_string .

     METHODS: close_document
       IMPORTING do_save TYPE c DEFAULT ' '
       EXPORTING retcode TYPE soi_ret_string
                 error   TYPE REF TO i_oi_error.

   PRIVATE SECTION.
     DATA:
       control  TYPE REF TO i_oi_container_control.
 ENDCLASS.                    "c_office_document DEFINITION

 DATA: document     TYPE REF TO c_office_document.

************************************************************************
*  CLASS c_office_document IMPLEMENTATION.
************************************************************************
 CLASS c_office_document IMPLEMENTATION.
   METHOD: constructor.
     me->control = control.
     me->document_type = gv_doc_type.
     me->document_format = document_format.
   ENDMETHOD.                    "constructor

   METHOD open_document.
     IF NOT proxy IS INITIAL.
       CALL METHOD me->close_document.
     ENDIF.
     CALL METHOD control->get_document_proxy
       EXPORTING
         document_type   = gv_doc_type
         document_format = document_format
       IMPORTING
         document_proxy  = proxy
         retcode         = gv_retcode.
     IF gv_retcode NE c_oi_errors=>ret_ok.
       RETURN.
     ENDIF.

     CALL METHOD proxy->open_document_from_table
       EXPORTING
         document_table   = data_table
         document_size    = data_size
         open_inplace     = open_inplace
         open_readonly    = open_readonly
         document_title   = /gda/doch-name
         startup_macro    = 'X'
         protect_document = 'X'
       IMPORTING
         retcode          = gv_retcode.
     IF gv_retcode NE c_oi_errors=>ret_ok.
       RETURN.
*       EXIT.
     ENDIF.

     CALL METHOD proxy->get_spreadsheet_interface
       IMPORTING
         sheet_interface = go_spreadsheet.

* This has been removed to allow users to save the spreadsheets
* locally and to then to subsequently edit them
*    CALL METHOD proxy->delete_menu_item
*      EXPORTING
*        menu_popup_name = 'File'
*        item_name       = 'Save Copy As...'
*        no_flush        = 'X'.

*    CALL METHOD proxy->delete_menu_item
*      EXPORTING
*        menu_popup_name = 'File'
*        item_name       = 'Save as Web Page...'
*        no_flush        = 'X'.

     CALL METHOD proxy->delete_menu_item
       EXPORTING
         menu_popup_name = 'Tools'(900)
         item_name       = 'Protection'(901)
         no_flush        = lv_flush.

*    CALL METHOD PROXY->DELETE_MENU_ITEM
*      EXPORTING
*        MENU_POPUP_NAME = 'Tools'
*        ITEM_NAME       = 'Options...'
*        NO_FLUSH        = 'X'.

   ENDMETHOD.                    "open_document

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
             retcode     = gv_retcode
             error       = error.
         IF gv_retcode NE c_oi_errors=>ret_ok.
           RETURN.
*           EXIT.
         ENDIF.
       ENDIF.

       IF NOT has_changed IS INITIAL OR
                        ( NOT has_changed_at_reopen IS INITIAL AND
                          NOT do_save IS INITIAL ).
         CALL METHOD proxy->save_document_to_table
           EXPORTING
             no_flush       = lv_flush
           IMPORTING
             error          = save_error
           CHANGING
             document_table = data_table
             document_size  = data_size.
       ENDIF.
       CALL METHOD proxy->release_document
         IMPORTING
           retcode = gv_retcode
           error   = error.
       IF NOT save_error IS INITIAL.
         IF save_error->error_code NE c_oi_errors=>ret_ok.
           CALL METHOD save_error->flush_error.
           gv_retcode = save_error->error_code.
           error = save_error.
         ENDIF.
       ENDIF.

       CLEAR: has_changed_at_reopen.

     ELSE.
       gv_retcode = c_oi_errors=>ret_document_not_open.
     ENDIF.
   ENDMETHOD.                    "close_document

 ENDCLASS.                    "c_office_document IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  create_sapdoc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM create_sapdoc.
   DATA:
     lv_has_activex.

   gv_retcode = c_oi_errors=>ret_ok.
   CLEAR go_control.
   IF go_control IS INITIAL.

     CALL FUNCTION 'GUI_HAS_ACTIVEX'
       IMPORTING
         return = lv_has_activex.

     IF lv_has_activex = abap_false.
       RETURN.
     ENDIF.

     CALL METHOD c_oi_container_control_creator=>get_container_control
       IMPORTING
         control = go_control
         retcode = gv_retcode.
     CALL METHOD c_oi_errors=>raise_message
       EXPORTING
         type = 'E'.

     CREATE OBJECT go_container
       EXPORTING
         container_name = 'CONTAINER'.

     CALL METHOD go_control->init_control
       EXPORTING
         r3_application_name      = sy-title
         inplace_enabled          = 'X'
         inplace_scroll_documents = 'X'
         parent                   = go_container
         register_on_close_event  = 'X'
         register_on_custom_event = 'X'
         no_flush                 = 'X'
       IMPORTING
         retcode                  = gv_retcode.

     CALL METHOD c_oi_errors=>raise_message
       EXPORTING
         type = 'E'.

*     IF bds_instance IS INITIAL.
*       CREATE OBJECT bds_instance.
*     ENDIF.

*------Open Document
     PERFORM open_sapdoc.

*------Populate Spreadsheet
     PERFORM pop_sheet.

   ENDIF.

*-----Select Sheet
   IF NOT go_spreadsheet IS INITIAL
   AND NOT ls_sheets-sheet_name IS INITIAL.

     CALL METHOD go_spreadsheet->select_sheet
       EXPORTING
         name     = ls_sheets-sheet_name
         no_flush = lv_flush.

   ENDIF.

 ENDFORM.                    " create_sapdoc
*&---------------------------------------------------------------------*
*&      Form  open_sapdoc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM open_sapdoc.

   DATA: lv_template TYPE sy-repid.

*-----Close document if open
   IF NOT document IS INITIAL.
     CALL METHOD document->close_document.
   ENDIF.

*-----Create document
   CREATE OBJECT document
     EXPORTING
       control         = go_control
       document_type   = gv_doc_type
       document_format = soi_docformat_compound.

* RROEL
* Check to see if sending program has a template defined
* differantly ie.. the template name is not equal to the program name
   IMPORT lv_template FROM MEMORY ID 'TEMPLATE'.

   IF lv_template IS INITIAL.
     lv_template = sy-repid.
   ENDIF.
* RROEL

*------Get spreadsheet
   SELECT SINGLE *
   FROM /gda/doch
   WHERE repid EQ lv_template
   AND   name IN s_rname.

   IF sy-subrc NE 0.
*    MESSAGE s000 WITH 'No template found for'(902) sy-title.
*     EXIT.
     RETURN.
   ELSE.
     MOVE /gda/doch-repid TO v_repid.
   ENDIF.

*-----Item table
   SELECT *
   FROM /gda/doci
   INTO TABLE izdoci
   WHERE repid = /gda/doch-repid
   AND   name = /gda/doch-name.

   LOOP AT izdoci.
     MOVE-CORRESPONDING izdoci TO lines.
     APPEND lines.
   ENDLOOP.

   MOVE /gda/doch-length TO document->data_size.

   IF document->data_table[] NE lines[].
     document->data_table[] = lines[].
   ENDIF.

   IF NOT document IS INITIAL AND document->data_size NE 0.
     IF NOT go_control IS INITIAL.
       CALL METHOD document->open_document
         EXPORTING
           open_inplace  = ''
           open_readonly = ''
         IMPORTING
           retcode       = gv_retcode.

       CALL METHOD c_oi_errors=>raise_message
         EXPORTING
           type = 'E'.
     ENDIF.
   ELSE.
   ENDIF.

* Get sheet names in the spreadsheet
   CALL METHOD go_spreadsheet->get_sheets
     EXPORTING
       no_flush = 'X'
     IMPORTING
       sheets   = lt_sheets.
 ENDFORM.                    " open_sapdoc
*&---------------------------------------------------------------------*
*&      Form  pop_sheet
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM pop_sheet.
   DATA:
     text(100),
     cnt       TYPE i,
     cell.

   CLEAR gv_tabix.

*-----Loop Through Spreadseets
   LOOP AT lt_sheets INTO ls_sheets WHERE sheet_name = 'DATA'.

     cnt = cnt + 1.

     CONCATENATE 'Creating spreadsheet'(913) '-' ls_sheets-sheet_name
     INTO text SEPARATED BY space.

     PERFORM progress_indicator USING '' text.

*-----Select Sheet
     CALL METHOD go_spreadsheet->select_sheet
       EXPORTING
         name     = ls_sheets-sheet_name
         no_flush = lv_flush.

     PERFORM get_cell USING cnt '1' cell.

*-----Loop through ranges
     LOOP AT range_item
     WHERE sheet = ls_sheets-sheet_name.

*------Populate full column
       PERFORM pop_ztab.

       CONCATENATE cell range_item-name INTO range_item-name.

*-----Set Cell
       CALL METHOD go_spreadsheet->insert_range_dim
         EXPORTING
           name      = range_item-name
           top       = range_item-top
           left      = range_item-left
           sheetname = ls_sheets-sheet_name
           rows      = 1
           columns   = 1
           no_flush  = lv_flush.

*------Populate Cell
       CALL METHOD go_spreadsheet->insert_one_table
         EXPORTING
           rangename  = range_item-name
           ddic_name  = '/GDA/TAB'
           data_table = gt_tab
           wholetable = lv_flush
           no_flush   = lv_flush.

     ENDLOOP.
   ENDLOOP.

*-----Select Sheet
   CALL METHOD go_spreadsheet->select_sheet
     EXPORTING
       name     = ls_sheets-sheet_name
       no_flush = lv_flush.


*   gv_col_nr = 1.

*------Hide Column
   LOOP AT hide_columns WHERE sheet = 'DATA'.
     CALL METHOD go_spreadsheet->hide_columns
       EXPORTING
         name     = ls_sheets-sheet_name
         no_flush = lv_flush
         first    = hide_columns-index
         last     = hide_columns-index
       IMPORTING
*        error    =
         retcode  = gv_retcode.
   ENDLOOP.


*----- BRF Details
   LOOP AT lt_sheets INTO ls_sheets WHERE sheet_name = 'DETAILS'.

     cnt = cnt + 1.

     CONCATENATE 'Creating spreadsheet'(913) '-' ls_sheets-sheet_name
     INTO text SEPARATED BY space.

     PERFORM progress_indicator USING '' text.

*-----Select Sheet
     CALL METHOD go_spreadsheet->select_sheet
       EXPORTING
         name     = ls_sheets-sheet_name
         no_flush = lv_flush.

*-----Load summary tables per sheet
*    PERFORM load_tables.

     PERFORM get_cell USING cnt '1' cell.

*-----Loop through ranges
     LOOP AT range_item
     WHERE sheet = ls_sheets-sheet_name.

*------Populate full column
       PERFORM pop_ztab.

       CONCATENATE cell range_item-name INTO range_item-name.

*-----Set Cell
       CALL METHOD go_spreadsheet->insert_range_dim
         EXPORTING
           name      = range_item-name
           top       = range_item-top
           left      = range_item-left
           sheetname = ls_sheets-sheet_name
           rows      = 1
           columns   = 1
           no_flush  = lv_flush.

*------Populate Cell
       CALL METHOD go_spreadsheet->insert_one_table
         EXPORTING
           rangename  = range_item-name
           ddic_name  = '/GDA/TAB'
           data_table = gt_tab
           wholetable = lv_flush
           no_flush   = lv_flush.

     ENDLOOP.
   ENDLOOP.

*----- Context Details
   LOOP AT lt_sheets INTO ls_sheets WHERE sheet_name = 'CONTEXT'.

     cnt = cnt + 1.

     CONCATENATE 'Creating spreadsheet'(913) '-' ls_sheets-sheet_name
     INTO text SEPARATED BY space.

     PERFORM progress_indicator USING '' text.

*-----Select Sheet
     CALL METHOD go_spreadsheet->select_sheet
       EXPORTING
         name     = ls_sheets-sheet_name
         no_flush = lv_flush.

     PERFORM get_cell USING cnt '1' cell.

*-----Loop through ranges
     LOOP AT range_item
     WHERE sheet = ls_sheets-sheet_name.

*------Populate full column
       PERFORM pop_ztab.

       CONCATENATE cell range_item-name INTO range_item-name.
*-----Set Cell
       CALL METHOD go_spreadsheet->insert_range_dim
         EXPORTING
           name      = range_item-name
           top       = range_item-top
           left      = range_item-left
           sheetname = ls_sheets-sheet_name
           rows      = 1
           columns   = 1
           no_flush  = lv_flush.

*------Populate Cell
       CALL METHOD go_spreadsheet->insert_one_table
         EXPORTING
           rangename  = range_item-name
           ddic_name  = '/GDA/TAB'
           data_table = gt_tab
           wholetable = lv_flush
           no_flush   = lv_flush.

     ENDLOOP.
   ENDLOOP.

   MOVE 'Preparing output...'(903) TO text.
   PERFORM progress_indicator USING space text.

   LOOP AT lt_sheets INTO ls_sheets WHERE sheet_name = 'CALCS'.

     cnt = cnt + 1.

     CONCATENATE 'Creating Spreedsheet'(913) '-' ls_sheets-sheet_name
     INTO text SEPARATED BY space.

     PERFORM progress_indicator USING space text.

*-----Select Sheet
     CALL METHOD go_spreadsheet->select_sheet
       EXPORTING
         name     = ls_sheets-sheet_name
         no_flush = lv_flush.

*-----Load summary tables per sheet
*    PERFORM load_tables.

     PERFORM get_cell USING cnt '1' cell.

*-----Loop through ranges
     LOOP AT range_item
     WHERE sheet = ls_sheets-sheet_name.

*------Populate full column
       PERFORM pop_ztab.

       CONCATENATE cell range_item-name INTO range_item-name.

*-----Set Cell
       CALL METHOD go_spreadsheet->insert_range_dim
         EXPORTING
           name      = range_item-name
           top       = range_item-top
           left      = range_item-left
           sheetname = ls_sheets-sheet_name
           rows      = 1
           columns   = 1
           no_flush  = lv_flush.

*------Populate Cell
       CALL METHOD go_spreadsheet->insert_one_table
         EXPORTING
           rangename  = range_item-name
           ddic_name  = '/GDA/TAB'
           data_table = gt_tab
           wholetable = lv_flush
           no_flush   = lv_flush.

     ENDLOOP.
   ENDLOOP.

   text = 'Excel Calculations Begin'(933).
   PERFORM progress_indicator USING space text.

 ENDFORM.                    " pop_sheet
*&---------------------------------------------------------------------*
*&      Form  progress_indicator
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM progress_indicator USING percent text.

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       percentage = percent
       text       = text.

 ENDFORM.                    " progress_indicator

******************************EXCEL SUBROUTINES************************


*&---------------------------------------------------------------------*
*&      Form  excel_worksheet_cell
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 FORM excel_worksheet_cell USING p_sheet p_column p_row p_type p_value.
*                           CHANGING  p_value.

   v_item = v_item + 1.
   CONCATENATE p_type v_item INTO range_item-name.
   WRITE p_value TO range_item-value.
   MOVE p_row TO range_item-top.
   MOVE p_column TO range_item-left.
   MOVE p_sheet TO range_item-sheet.
   APPEND range_item.

 ENDFORM.                    " excel_worksheet_cell

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_REPID  text
*      -->P_1199   text
*      -->P_SPACE  text
*      -->P_V_STRTOP  text
*----------------------------------------------------------------------*
*FORM build_fieldcat USING prog_name
*                             tabname
*                             include.
*  REFRESH fieldcat.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name         = prog_name
*      i_internal_tabname     = tabname
*      i_inclname             = include
*      i_client_never_display = 'X'
*    CHANGING
*      ct_fieldcat            = fieldcat[]
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " BUILD_FIELDCAT

* Parameters definition
 DEFINE xls_par.
   v_row = v_row + 1.
   v_col = v_col + 1.
   PERFORM pop_parmaters USING &2 'PARAMETERS'. " Description
   v_col = v_col + 1.
   PERFORM pop_parmaters USING &1 'PARAMETERS'. " Value
   v_col = v_col - 2.
 END-OF-DEFINITION.

* Select Options definition
 DEFINE xls_sel.
   SORT &1 BY sign DESCENDING option ASCENDING low ASCENDING.
   CLEAR text_long.
   LOOP AT &1.
     CLEAR txt.
     CASE &1-option.
       WHEN 'EQ'.
       WHEN 'BT'.
       WHEN OTHERS.

         READ TABLE itddd07t WITH KEY domvalue_l = &1-option.
         IF sy-subrc = 0.
           MOVE itddd07t-ddtext TO txt.
         ENDIF.

     ENDCASE.
     CASE &1-sign.
       WHEN 'I'.
       WHEN 'E'.
         CONCATENATE ' Exclude'(962) txt
         INTO txt SEPARATED BY space.
     ENDCASE.

     WRITE &1-low TO text_low.
     SHIFT text_low LEFT DELETING LEADING space.
     WRITE &1-high TO text_high.
     SHIFT text_high LEFT DELETING LEADING space.

     IF NOT &1-high IS INITIAL.
       CONCATENATE txt text_low 'to' text_high INTO txt
         SEPARATED BY space.
     ELSE.
       CONCATENATE txt text_low INTO txt SEPARATED BY space.
     ENDIF.

     IF text_long IS INITIAL.
       MOVE txt TO text_long.
     ELSE.
       CONCATENATE text_long txt INTO text_long SEPARATED BY ', '.
     ENDIF.

   ENDLOOP.
   SHIFT text_long LEFT DELETING LEADING space.
   v_row = v_row + 1.
   v_col = v_col + 1.
   PERFORM pop_parmaters USING &2 'PARAMETERS'. " Description
   v_col = v_col + 1.
*  if v_sheet = 'Import Detail Drawdown'
*  or v_sheet = 'Export Detail Drawdown'
*  or v_sheet = 'Outstanding Bank Contracts'
*  or v_sheet = 'Audit of Transactions'.
*    v_col = v_col + 2.
*  endif.
   PERFORM pop_parmaters USING text_long 'PARAMETERS'. " Value
   v_col = v_col - 2.
*  if v_sheet = 'Import Detail Drawdown'
*  or v_sheet = 'Export Detail Drawdown'
*  or v_sheet = 'Outstanding Bank Contracts'
*  or v_sheet = 'Audit of Transactions'.
*    v_col = v_col - 2.
*  endif.
 END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      Form  pop_parmaters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM pop_parmaters USING result TYPE.
*
**------Fill cell
*  PERFORM excel_worksheet_cell USING v_sheet
*                                     v_col
*                                     v_row
*                                     type
*                            CHANGING result.
*
*ENDFORM.                    " pop_parmaters
*&---------------------------------------------------------------------*
*&      Form  pop_ztab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM pop_ztab .

   REFRESH gt_tab.
   CLEAR   gs_tab.

*------Parameters
   IF range_item-sheet = 'DATA'.
*-----Main Data processing
     PERFORM pop_main.
   ELSEIF range_item-sheet = 'DETAILS'.
*-----Details processing
     PERFORM pop_main_details.
   ELSEIF range_item-sheet = 'CONTEXT'.
*-----Context processing
     PERFORM pop_context_details.
   ELSEIF range_item-sheet = 'CALCS'.
*-----Context processing
     PERFORM pop_calcs_details.
   ENDIF.
 ENDFORM.                    " pop_ztab

*&---------------------------------------------------------------------*
*&      Form  get_cell
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_COL  text
*      -->P_V_ROW  text
*----------------------------------------------------------------------*
 FORM get_cell USING col row cell.
   DATA: c_col(2).
   DATA: c_row(4).

   MOVE row TO c_row.
   SHIFT c_row LEFT DELETING LEADING space.

   CASE col.
     WHEN 1 . c_col = 'A'.
     WHEN 2 . c_col = 'B'.
     WHEN 3 . c_col = 'C'.
     WHEN 4 . c_col = 'D'.
     WHEN 5 . c_col = 'E'.
     WHEN 6 . c_col = 'F'.
     WHEN 7 . c_col = 'G'.
     WHEN 8 . c_col = 'H'.
     WHEN 9 . c_col = 'I'.
     WHEN 10. c_col = 'J'.
     WHEN 11. c_col = 'K'.
     WHEN 12. c_col = 'L'.
     WHEN 13. c_col = 'M'.
     WHEN 14. c_col = 'N'.
     WHEN 15. c_col = 'O'.
     WHEN 16. c_col = 'P'.
     WHEN 17. c_col = 'Q'.
     WHEN 18. c_col = 'R'.
     WHEN 19. c_col = 'S'.
     WHEN 20. c_col = 'T'.
     WHEN 21. c_col = 'U'.
     WHEN 22. c_col = 'V'.
     WHEN 23. c_col = 'W'.
     WHEN 24. c_col = 'X'.
     WHEN 25. c_col = 'Y'.
     WHEN 26. c_col = 'Z'.
     WHEN 27. c_col = 'AA'.
     WHEN 28. c_col = 'AB'.
     WHEN 29. c_col = 'AC'.
     WHEN 30. c_col = 'AD'.
     WHEN 31. c_col = 'AE'.
     WHEN 32. c_col = 'AF'.
     WHEN 33. c_col = 'AG'.
     WHEN 34. c_col = 'AH'.
     WHEN 35. c_col = 'AI'.
     WHEN 36. c_col = 'AJ'.
     WHEN 37. c_col = 'AK'.
     WHEN 38. c_col = 'AL'.
     WHEN 39. c_col = 'AM'.
     WHEN 40. c_col = 'AN'.
     WHEN 41. c_col = 'AO'.
     WHEN 42. c_col = 'AP'.
     WHEN 43. c_col = 'AQ'.
     WHEN 44. c_col = 'AR'.
     WHEN 45. c_col = 'AS'.
     WHEN 46. c_col = 'AT'.
     WHEN 47. c_col = 'AU'.
     WHEN 48. c_col = 'AV'.
     WHEN 49. c_col = 'AW'.
     WHEN 50. c_col = 'AX'.
     WHEN 51. c_col = 'AY'.
     WHEN 52. c_col = 'AZ'.
   ENDCASE.

   CONCATENATE c_col c_row INTO cell.

 ENDFORM.                    " get_cell
*&---------------------------------------------------------------------*
*&      Form  do_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM do_sum .
   DATA: sum(6).

*-----Subtotal
   sum = ( v_row + gv_tabix ) - 1.
   SHIFT sum LEFT DELETING LEADING space.
   CONCATENATE v_cell(1) sum INTO sum.
   CONCATENATE '=SUM(' v_cell ':' sum ')'
   INTO gs_tab.

*-----Grand Total
   sum = v_row + gv_tabix.
   SHIFT sum LEFT DELETING LEADING space.
   CONCATENATE v_cell(1) sum INTO sum.

   IF v_sum IS INITIAL.
     MOVE sum TO v_sum.
   ELSE.
     CONCATENATE v_sum '+' sum INTO v_sum.
   ENDIF.

 ENDFORM.                    " do_sum
*&---------------------------------------------------------------------*
*&      Form  add_gtot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM add_gtot .
   DATA: sum(6).
   DATA: line TYPE i.

   CONCATENATE '=SUM(' v_sum ')' INTO gs_tab.

*-----Grand Total
   DESCRIBE TABLE gt_tab LINES line.
   sum = range_item-top + line.
   SHIFT sum LEFT DELETING LEADING space.
   CONCATENATE v_cell(1) sum INTO sum.

   IF v_gtsum IS INITIAL.
     MOVE sum TO v_gtsum.
   ELSE.
     CONCATENATE v_gtsum '+' sum INTO v_gtsum.
   ENDIF.

 ENDFORM.                    " add_gtot
*&---------------------------------------------------------------------*
*&      Form  delete_sheet
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM delete_sheet .

   DELETE lt_sheets INDEX sy-tabix.

*------Delete sheet
   CALL METHOD go_spreadsheet->delete_sheet
     EXPORTING
       name     = ls_sheets-sheet_name
       no_flush = 'X'.

 ENDFORM.                    " delete_sheet
*&---------------------------------------------------------------------*
*&      Form  add_tot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM add_tot .
   DATA: sum(6).
   DATA: line TYPE i.

*-----Grand Total
   DESCRIBE TABLE gt_tab LINES line.
   sum = range_item-top + line.
   SHIFT sum LEFT DELETING LEADING space.
   CONCATENATE v_cell(1) sum INTO sum.

   IF v_sum1 IS INITIAL.
     MOVE sum TO v_sum1.
   ELSE.
     CONCATENATE v_sum1 '+' sum INTO v_sum1.
   ENDIF.

 ENDFORM.                    " add_tot
*&---------------------------------------------------------------------*
*&      Form  load_html
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM load_html .


   DATA: doc_url(200) TYPE c.
*        table        TYPE swww_t_merge_table.

   IF gv_html_index_container IS INITIAL.
     CREATE OBJECT gv_html_index_container
       EXPORTING
         container_name = 'HTML'
       EXCEPTIONS
         OTHERS         = 1.
     CASE sy-subrc.
       WHEN '0'.
*      everything is fine here.
       WHEN OTHERS.
         RAISE cntl_error .
     ENDCASE.
   ENDIF.

   IF gv_html_index IS INITIAL.
     CREATE OBJECT gv_html_index
       EXPORTING
         parent = gv_html_index_container.
*    IF sy-subrc NE 0.
*      RAISE cntl_error.
*    ENDIF.

*  Registering for the events.
     wa_events-eventid = gv_html_index->m_id_sapevent.
     wa_events-appl_event = 'X'.
     APPEND wa_events TO events.

     CALL METHOD gv_html_index->set_registered_events
       EXPORTING
         events = events.

     CREATE OBJECT gv_evt_receiver.

     SET HANDLER gv_evt_receiver->on_sapevent
                 FOR gv_html_index.

     CALL METHOD gv_html_index->load_mime_object
       EXPORTING
         object_id  = object_id
         object_url = object_url
       EXCEPTIONS
         OTHERS     = 1.
     IF sy-subrc = 1.
       RAISE cntl_error.
     ENDIF.


     CALL METHOD gv_html_index->load_html_document
       EXPORTING
         document_id          = v_repid
         document_url         = v_repid
       IMPORTING
         assigned_url         = doc_url
       EXCEPTIONS
         document_not_found   = 1
         dp_error_general     = 2
         dp_invalid_parameter = 3.

     IF sy-subrc = 0.
       CALL METHOD gv_html_index->show_data
         EXPORTING
           url        = doc_url
         EXCEPTIONS
           cntl_error = 1.
       IF sy-subrc = 1.
         RAISE cntl_error.
       ENDIF.
     ENDIF.

   ENDIF.

 ENDFORM.                    " load_html
*&---------------------------------------------------------------------*
*&      Form  load_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM load_fieldcat .

*-----Loop through fields
   LOOP AT fieldcat.

*-----Increment column
     v_col = v_col + 1.

     MOVE fieldcat-fieldname TO range_item-name.
     MOVE v_row   TO range_item-top.
     MOVE v_col   TO range_item-left.
     MOVE v_sheet TO range_item-sheet.
     APPEND range_item.
     CLEAR range_item.

     MOVE v_sheet TO icolumn-sheet.
     MOVE fieldcat-fieldname TO icolumn-field.
     MOVE fieldcat-seltext_l TO icolumn-name.
     APPEND icolumn.
     CLEAR icolumn.

   ENDLOOP.

 ENDFORM.                    " load_fieldcat
*&---------------------------------------------------------------------*
*&      Form  hide_column
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM hide_column .
   DATA: cell.

   REFRESH vrm_list.
   CLEAR gv_tabix.

   LOOP AT icolumn
   WHERE sheet = ls_sheets-sheet_name.
     gv_tabix = gv_tabix + 1.

     READ TABLE idel_column WITH KEY sheet = icolumn-sheet
                                     name = icolumn-name.
     CHECK sy-subrc NE 0.

     vrm_value-text = icolumn-name.
     WRITE gv_tabix TO vrm_value-key LEFT-JUSTIFIED.
     APPEND vrm_value TO vrm_list.

   ENDLOOP.

   CALL FUNCTION 'VRM_SET_VALUES'
     EXPORTING
       id     = vrm_name
       values = vrm_list.

   CLEAR column.
   CALL SCREEN 110 STARTING AT 5 10 ENDING AT 50 12.

   IF column IS INITIAL.
     RETURN.
   ENDIF.

   CLEAR gv_tabix.
   LOOP AT icolumn
   WHERE sheet = ls_sheets-sheet_name.
     gv_tabix = gv_tabix + 1.
     IF gv_tabix = column.
       MOVE-CORRESPONDING icolumn TO idel_column.
       APPEND idel_column.
       EXIT.
     ENDIF.
   ENDLOOP.

   IF sy-subrc = 0.

     READ TABLE lt_sheets INTO ls_sheets
     WITH KEY sheet_name = ls_sheets-sheet_name.

     IF sy-subrc = 0.

       PERFORM get_cell USING sy-tabix '1' cell.

       CONCATENATE cell icolumn-field INTO icolumn-field.

*------Hide Column
       CALL METHOD go_spreadsheet->hide_columns
         EXPORTING
           name     = icolumn-field
           no_flush = lv_flush.
     ENDIF.
   ENDIF.
 ENDFORM.                    " hide_column
*&---------------------------------------------------------------------*
*&      Module  status_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE status_0110 OUTPUT.
*  SET PF-STATUS '110'.
*  IF sy-ucomm = 'HIDE'.
*    SET TITLEBAR '110' WITH 'Hide'(917).
*  ELSE.
*    SET TITLEBAR '110' WITH 'Show'(918).
*  ENDIF.

*ENDMODULE.                 " status_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
 MODULE user_command_0110 INPUT.

   CASE sy-ucomm.

     WHEN 'OK' OR 'ENTE'.
       IF column IS INITIAL.
*        MESSAGE e000 WITH 'Make entry to all required fields'(910).
       ENDIF.

       SET SCREEN 0.

     WHEN 'EXIT'.
       CLEAR column.
       SET SCREEN 0.
   ENDCASE.

 ENDMODULE.                 " user_command_0110  INPUT
*&---------------------------------------------------------------------*
*&      Form  show_column
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM show_column .
   DATA: cell.

   REFRESH vrm_list.
   CLEAR gv_tabix.

   LOOP AT idel_column
   WHERE sheet = ls_sheets-sheet_name.
     gv_tabix = gv_tabix + 1.

     vrm_value-text = idel_column-name.
     WRITE gv_tabix TO vrm_value-key LEFT-JUSTIFIED.
     APPEND vrm_value TO vrm_list.

   ENDLOOP.

   CALL FUNCTION 'VRM_SET_VALUES'
     EXPORTING
       id     = vrm_name
       values = vrm_list.

   CLEAR column.
   CALL SCREEN 110 STARTING AT 5 10 ENDING AT 50 12.

   IF column IS INITIAL.
     RETURN.
   ENDIF.

   CLEAR gv_tabix.
   LOOP AT idel_column
   WHERE sheet = ls_sheets-sheet_name.
     gv_tabix = gv_tabix + 1.
     IF gv_tabix = column.
       DELETE idel_column.
       EXIT.
     ENDIF.
   ENDLOOP.

   IF sy-subrc = 0.

     READ TABLE lt_sheets INTO ls_sheets
     WITH KEY sheet_name = ls_sheets-sheet_name.

     IF sy-subrc = 0.

       PERFORM get_cell USING sy-tabix '1' cell.

       CONCATENATE cell idel_column-field INTO idel_column-field.

*------Show Column
       CALL METHOD go_spreadsheet->show_columns
         EXPORTING
           name     = idel_column-field
           no_flush = lv_flush.
     ENDIF.
   ENDIF.
 ENDFORM.                    " show_column
*&---------------------------------------------------------------------*
*&      Form  FIT_WIDEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM fit_widest .
   DATA: cell.

   REFRESH vrm_list.
   CLEAR gv_tabix.

   LOOP AT icolumn
   WHERE sheet = ls_sheets-sheet_name.
     gv_tabix = gv_tabix + 1.

     READ TABLE idel_column WITH KEY sheet = icolumn-sheet
                                     name = icolumn-name.
     CHECK sy-subrc NE 0.

     vrm_value-text = icolumn-name.
     WRITE gv_tabix TO vrm_value-key LEFT-JUSTIFIED.
     APPEND vrm_value TO vrm_list.

   ENDLOOP.

   CALL FUNCTION 'VRM_SET_VALUES'
     EXPORTING
       id     = vrm_name
       values = vrm_list.

   CLEAR column.
   CALL SCREEN 110 STARTING AT 5 10 ENDING AT 50 12.

   IF column IS INITIAL.
     RETURN.
   ENDIF.

   CLEAR gv_tabix.
   LOOP AT icolumn
   WHERE sheet = ls_sheets-sheet_name.
     gv_tabix = gv_tabix + 1.
     IF gv_tabix = column.
       EXIT.
     ENDIF.
   ENDLOOP.

   IF sy-subrc = 0.
     READ TABLE lt_sheets INTO ls_sheets
     WITH KEY sheet_name = ls_sheets-sheet_name.
     IF sy-subrc = 0.
       PERFORM get_cell USING sy-tabix '1' cell.
       CONCATENATE cell icolumn-field INTO icolumn-field.
*------Fit widest
       CALL METHOD go_spreadsheet->fit_widest
         EXPORTING
           name     = icolumn-field
           no_flush = 'X'
         IMPORTING
           retcode  = gv_retcode.
     ENDIF.
   ENDIF.
 ENDFORM.                    " FIT_WIDEST
*&---------------------------------------------------------------------*
*&      Form  sum_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM sum_amount USING amount.

   CLEAR gs_tab.

   IF amount < 0.
     amount = amount * -1.
     WRITE amount TO gs_tab LEFT-JUSTIFIED.
     CONCATENATE '-' gs_tab INTO gs_tab.
   ELSE.
     WRITE amount TO gs_tab LEFT-JUSTIFIED.
   ENDIF.

 ENDFORM.                    " sum_amount
