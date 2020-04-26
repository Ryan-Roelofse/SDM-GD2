*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POI_ART_DATA
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_data_core.

CONSTANTS:
  gc_article  TYPE /gda/sdm_de_object VALUE 'ARTICLE',
  gc_auth_rsr TYPE xuobject           VALUE '/GDA/ART_R',
  gc_auth_e   TYPE xuobject           VALUE '/GDA/MAT_E'.

DATA:
  go_article   TYPE REF TO /gda/sdm_cl_article,
  go_selection TYPE REF TO /gda/sdm_cl_selections,
  gs_selscreen,
  gt_results   TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key.

FIELD-SYMBOLS:
  <results> LIKE gt_results,
  <result>  LIKE LINE OF gt_results.

FIELD-SYMBOLS:
  <results_der>     TYPE STANDARD TABLE,
  <results_der_all> TYPE STANDARD TABLE.
