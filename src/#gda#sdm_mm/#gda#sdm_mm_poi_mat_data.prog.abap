*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POI_MAT_DATA
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_data_core.

TYPES BEGIN OF objects.
TYPES: type    TYPE /gda/sdm_de_type.
TYPES: class   TYPE classname.
TYPES: include TYPE include.
TYPES: object  TYPE REF TO /gda/sdm_cl_material.
TYPES: mapping TYPE REF TO /gda/sdm_cl_brf_mapping.
TYPES END OF objects.

CONSTANTS:
  gc_material TYPE /gda/sdm_de_object VALUE 'MATERIAL'.

DATA:
  go_material   TYPE REF TO /gda/sdm_cl_material. "/gda/sdm_cl_core,
*  gt_objects    TYPE STANDARD TABLE OF /gda/sdm_s_objects, "objects,
*  gt_attributes TYPE STANDARD TABLE OF go_material->ty_brf_attributes.

FIELD-SYMBOLS:
*  <objects>         LIKE LINE OF gt_objects,
*  <attribute>       LIKE LINE OF gt_attributes,
  <results_der>     TYPE STANDARD TABLE,
  <results_der_all> TYPE STANDARD TABLE.
