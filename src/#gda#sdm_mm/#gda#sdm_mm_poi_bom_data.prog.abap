**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MM_POI_BOM_DATA
**&---------------------------------------------------------------------*
*INCLUDE /gda/sdm_data_core.
*
*TYPES BEGIN OF objects.
*TYPES: type   TYPE /gda/sdm_de_type.
*TYPES: class  TYPE classname.
*TYPEs: INCLUDE type include.
*TYPES: object TYPE REF TO /gda/sdm_cl_bom.
*types: mapping TYPE ref to /gda/sdm_cl_brf_mapping.
*TYPES END OF objects.
*
*CONSTANTS:
*  gc_bom TYPE /gda/sdm_de_object VALUE 'BOM'.
*
*DATA:
*  go_article    TYPE REF TO /gda/sdm_cl_bom, "/gda/sdm_cl_core,
*  gt_objects    TYPE STANDARD TABLE OF objects,
*  gt_attributes TYPE STANDARD TABLE OF go_article->ty_brf_attributes.
*
*FIELD-SYMBOLS:
*  <objects>   LIKE LINE OF gt_objects,
*  <attribute> LIKE LINE OF gt_attributes,
**  <set_data>  TYPE any,
**  <result_val>      TYPE /gda/sdm_s_val_results,
**  <results_val>     TYPE /gda/sdm_t_val_results,
**  <results_val_all> TYPE /gda/sdm_t_val_results,
*  <results_der>     TYPE STANDARD TABLE,
*  <results_der_all> TYPE STANDARD TABLE.
