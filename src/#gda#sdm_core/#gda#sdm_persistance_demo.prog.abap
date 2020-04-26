*&---------------------------------------------------------------------*
*& Report /GDA/SDM_PERSISTANCE_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_persistance_demo.

*DATA sdm_agent TYPE REF TO /gda/ca_sdm_persistent_main.
*DATA sdm       TYPE REF TO /gda/cl_sdm_persistent_main.
**DATA:lo_uiid  TYPE REF TO cl_system_uuid.
*
*DATA:
*  i_sdm_object_id TYPE /gda/sdm_de_object_id,
*  i_sdm_tabkey    TYPE cdtabkey,
*  i_tabname        TYPE /gda/sdm_de_tabname,
*  i_field          TYPE /gda/sdm_de_fieldname,
*  i_msg_id        TYPE symsgid,
*  i_msg_number    TYPE symsgno.
*
*START-OF-SELECTION.
*
*  i_sdm_tabkey     = 'A10001'.
*  i_sdm_object_id  = '01'.
*  i_tabname        = 'MARA'.
*  i_field          = 'MATNR'.
*  i_msg_id         = '/GDA/SDM'.
*  i_msg_number     = '001'.
*
*  sdm_agent = /gda/ca_sdm_persistent_main=>agent.
*
*  TRY.
*      CALL METHOD sdm_agent->create_persistent
*        EXPORTING
*          i_sdm_object_id = i_sdm_object_id
*          i_sdm_tabkey    = i_sdm_tabkey
*          i_tabname       = i_tabname
*          i_field         = i_field
*          i_msg_id        = i_msg_id
*          i_msg_number    = i_msg_number
*          i_begda         = sy-datum
*        RECEIVING
*          result          = sdm.
*    CATCH cx_os_object_existing .
*  ENDTRY.
*
*  DATA:
*   lv_endda TYPE sy-datum.
*
*lv_endda = '99991231'.
*
*  TRY.
*      CALL METHOD sdm->set_endda
*        EXPORTING
*          i_endda = lv_endda.
*
*      CALL METHOD sdm->set_msg_type
*        EXPORTING
*          i_msg_type = 'E'.
*
*      CALL METHOD sdm->set_spr_status
*        EXPORTING
*          i_spr_status = '1'.
*    CATCH cx_os_object_not_found .
*  ENDTRY.
*
*
*  COMMIT WORK.
