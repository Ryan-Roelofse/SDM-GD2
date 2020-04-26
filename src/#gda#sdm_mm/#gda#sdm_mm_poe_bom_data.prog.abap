**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MM_POE_BOM_DATA
**&---------------------------------------------------------------------*
*INCLUDE /gda/sdm_data_core.
*
*CONSTANTS:
*  gc_bom TYPE /gda/sdm_de_object VALUE 'BOM'.
*
*TYPES BEGIN OF objects.
*TYPES: type   TYPE /gda/sdm_de_type.
*TYPES: class  TYPE classname.
*TYPES: include TYPE include.
*TYPES: object TYPE REF TO /gda/sdm_cl_bom.
*TYPES: mapping TYPE REF TO /gda/sdm_cl_brf_mapping.
*TYPES END OF objects.
*
*DATA:
*  gt_sdm_bom     TYPE STANDARD TABLE OF /gda/sdm_s_bom,
*  gt_objects     TYPE STANDARD TABLE OF objects,
*  gs_sdm_objects TYPE /gda/sdm_s_bom,
*  gt_attributes  TYPE STANDARD TABLE OF /gda/sdm_cl_core=>ty_brf_attributes.
*
*FIELD-SYMBOLS:
* <objects>   LIKE LINE OF gt_objects,
* <attribute> like line of gt_attributes.
