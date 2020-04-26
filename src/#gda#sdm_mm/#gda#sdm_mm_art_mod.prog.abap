*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_ARTICLE_REC_STAT_MOD
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
***INCLUDE ZRMDG_MM_EXCEPTION_REPORT2_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK_CODE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_ok_code OUTPUT.
  CLEAR ok_code.
ENDMODULE.                 " CLEAR_OK_CODE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'DYN'.
  SET TITLEBAR 'T1'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'ALV'.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_objects OUTPUT.

  CREATE OBJECT go_container_1
    EXPORTING
*     parent                      = g_grid_main
      repid                       = sy-repid
      dynnr                       = sy-dynnr
      side                        = cl_gui_docking_container=>dock_at_bottom
      extension                   = cl_gui_docking_container=>ws_maximizebox
*     style                       =
*     lifetime                    = lifetime_default
*     caption                     =
*     metric                      = 0
*     ratio                       = 70
*     no_autodef_progid_dynnr     =
*     name                        =
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*   create splitter container in which we’ll place the alv table
  CREATE OBJECT go_splitter
    EXPORTING
      parent  = go_container_1
      rows    = 2
      columns = 1
      align   = 15.

*   get part of splitter container for 1st table
  CALL METHOD go_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = go_parent1.

*   get part of splitter container for 2nd table
  CALL METHOD go_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = go_parent2.

  CALL METHOD go_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 55.
*
  CALL METHOD cl_gui_cfw=>flush.

***  Display first ALV
*  IF sy-uname = 'RROELOFSE'.
  IF sy-repid = '/GDA/SDM_ARTICLE_REC_STAT_REP2'.
    PERFORM set_display_top.
  ELSE.
    PERFORM set_display_top2.
  ENDIF.
*  ELSE.
*    PERFORM set_display_top.
*  ENDIF.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT
