*&---------------------------------------------------------------------*
*& Report /GDA/SDM_LICENSE
*&---------------------------------------------------------------------*

************************************************************************
*  This report allows the customer to upload a new SDM license file
*  supplied by GlueData.
************************************************************************
*  ABAPer: Charles de Jager
************************************************************************
*  VERSION 1.0
*     Initial release of the license upload program
************************************************************************

REPORT /gda/sdm_license MESSAGE-ID /gda/sdm_core.


************************************************************************
*
* CONSTANTS, TYPES, DATA
*
************************************************************************
DATA:
  lv_enc type string.

************************************************************************
*
*                           SELECTION-SCREEN
*
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK 101 WITH FRAME.
PARAMETERS:
  p_file TYPE string OBLIGATORY.

SELECTION-SCREEN END OF BLOCK 101.

AT SELECTION-SCREEN.

****************************************************************
*
*          Authorization Check
*
****************************************************************
  AUTHORITY-CHECK OBJECT '/GDA/ADM'
           ID 'ACTVT' FIELD '16'.
  IF sy-subrc <> 0.
    MESSAGE 'No authorization' TYPE 'E'.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'GUI_FILE_LOAD_DIALOG'
    EXPORTING
      window_title      = 'SDM License File'
      default_extension = 'key'
      default_file_name = 'SDM License'
      WITH_ENCODING     = 'X'
      file_filter       = 'SDM License (*.key)|key'
*     INITIAL_DIRECTORY = '\'
    IMPORTING
*     FILENAME          = p_file.
      fullpath          = p_file.
*   USER_ACTION             =
*   FILE_ENCODING           =



************************************************************************
*
*                         START-OF-SELECTION
*
************************************************************************
END-OF-SELECTION.

  PERFORM get_file USING p_file CHANGING lv_enc.
  IF lv_enc <> '!'.
    PERFORM process_license USING lv_enc.
  ENDIF.



************************************************************************
*
*                            SUBROUTINES
*
************************************************************************


****************************************************************
*
*          FORM: get_file
*
*          Subroutine to open the license file and return
*          the license string contained in it.
*
****************************************************************

FORM get_file USING cv_file
              CHANGING cv_enc TYPE string.

  DATA: it_data_tab TYPE TABLE OF string.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = p_file
    TABLES
      data_tab                = it_data_tab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE it_data_tab INDEX 1 INTO cv_enc.
ENDFORM.

****************************************************************
*
*          FORM: process_license
*
*          Subroutine to process the license key
*
****************************************************************
FORM process_license USING cv_license.

  DATA:
    lv_result  TYPE boolean,
    lv_message TYPE numc3,
    lv_msgv1   TYPE char50,
    lv_msgv2   TYPE char50,
    lv_msgv3   TYPE char50,
    lv_msgv4   TYPE char50.

  CALL METHOD /gda/sdm_cl_core=>set_license
    EXPORTING
      license = cv_license
    IMPORTING
      result  = lv_result
      message = lv_message
      msgv1   = lv_msgv1
      msgv2   = lv_msgv2
      msgv3   = lv_msgv3
      msgv4   = lv_msgv4.

  IF lv_result = 'X'.
    MESSAGE i000.
  ELSE.
    MESSAGE ID '/GDA/SDM_CORE' TYPE 'A' NUMBER lv_message WITH lv_msgv1 lv_msgv2.

*    CASE lv_message.
*      WHEN '001'.
*        MESSAGE e001 WITH
*        lv_msgv1 lv_msgv2.
*      WHEN '002'.
*        MESSAGE e002 WITH
*        lv_msgv1 lv_msgv2.
*      WHEN '004'.
*        MESSAGE e003 WITH
*        lv_msgv1 lv_msgv2.
*      WHEN '004'.
*        MESSAGE e004 WITH
*        lv_msgv1 lv_msgv2.
*      WHEN '005'.
*        MESSAGE e005 WITH
*        lv_msgv1 lv_msgv2.
*    ENDCASE.

  ENDIF.


ENDFORM.
