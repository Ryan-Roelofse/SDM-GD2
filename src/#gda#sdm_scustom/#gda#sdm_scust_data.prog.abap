*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_SCUSTOM_DATA
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_data_core.

CONSTANTS:
  gc_scustom  TYPE /gda/sdm_de_object VALUE 'SCUSTOM',
  gc_auth_rsr TYPE xuobject           VALUE '/GDA/SCU_R',
  gc_auth_e   TYPE xuobject           VALUE '/GDA/SCU_E'.

DATA:
  go_selection   TYPE REF TO /gda/sdm_cl_scustom_selections,
  gs_selscreen   TYPE /gda/sdm_cl_scustom_selections=>ty_selscreen,
  gt_results     TYPE STANDARD TABLE OF /gda/sdm_s_scust_val_res_gui,
  gs_sdm_objects TYPE /gda/sdm_s_scustom_rsr,
  gt_sdm_objects TYPE STANDARD TABLE OF /gda/sdm_s_scustom_rsr.

FIELD-SYMBOLS:
  <results>      LIKE gt_results,
  <result>       LIKE LINE OF gt_results ##NEEDED.
