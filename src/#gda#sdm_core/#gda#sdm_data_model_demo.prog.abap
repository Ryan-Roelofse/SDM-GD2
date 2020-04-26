*&---------------------------------------------------------------------*
*& Report /GDA/SDM_PERSISTANCE_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_data_model_demo.


*DATA: lo_data_model TYPE REF TO /gda/cl_sdm_data_model_excepts.
*
*DATA:
*  i_sdm_object TYPE /gda/sdm_de_object,
*  i_sdm_guuid   TYPE /gda/sdm_de_uuid,
*  i_tabname     TYPE /gda/sdm_de_tabname,
*  i_field       TYPE /gda/sdm_de_fieldname,
*  i_value       TYPE /gda/sdm_de_fieldvalue,
*  lv_hdr       TYPE /gda/sdm_exc_hdr,
*  lv_itm       TYPE /gda/sdm_exc_itm.
*
*
*
*START-OF-SELECTION.
*
*
*  i_sdm_object = 'ARTICLE'.
*  i_tabname    = 'MARA'.
*  i_field      = 'MATNR'.
*
*  CREATE OBJECT lo_data_model
*    EXPORTING
*      i_sdm_object = i_sdm_object
*      i_tabname    = i_tabname
*      i_field      = i_field.
*
*  lv_hdr-value = '1234'.
*  lo_data_model->set_header( x_hdr = lv_hdr ).
*
** Item
*
**  lv_itm-SDM_OBJECT
**  lv_itm-SDM_GUUID
*  lv_itm-MSG_ID          = '/GDA/ART'.
*  lv_itm-MSG_NUMBER      = '001'.
*  lv_itm-tabname         = 'MARA'.
*  lv_itm-field           = 'MATNR'.
*  lv_itm-value           = '10001'.
*  lv_itm-value_correct   = 'ABCDE'.
*  lv_itm-msg_count       = '1'.
*  lv_itm-msg_type        = 'E'.
*
*  lo_data_model->set_item( x_itm = lv_itm ).
*
*
*  COMMIT WORK.
