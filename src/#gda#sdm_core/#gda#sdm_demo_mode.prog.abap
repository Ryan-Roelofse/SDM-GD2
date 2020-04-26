*&---------------------------------------------------------------------*
*& Report /GDA/SDM_DEMO_MODE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_demo_mode.
DATA:
  lv_no_brf.

PARAMETERS: p_set AS CHECKBOX.


START-OF-SELECTION.

*    IMPORT lv_no_brf = lv_no_brf FROM MEMORY ID 'NO_BRF'.

  lv_no_brf = p_set.

*  EXPORT lv_no_brf = lv_no_brf TO MEMORY ID 'NO_BRF'.
*  EXPORT lv_no_brf TO MEMORY ID 'NO_BRF'.
  export lv_no_brf from lv_no_brf TO MEMORY ID 'NO_BRF'.
