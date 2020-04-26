**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MM_BOM_DATA
**&---------------------------------------------------------------------*
*INCLUDE /gda/sdm_data_core.
*
*TABLES:
**  mast,
**  stko,
**  stas,
**  stpo,
*  t100.
*
*CONSTANTS:
*  gc_bom      TYPE /gda/sdm_de_object VALUE 'BOM',
*  gc_auth_rsr TYPE xuobject VALUE '/GDA/BOM_R',
*  gc_auth_e   TYPE xuobject VALUE '/GDA/BOM_E'.
*
*
**TYPES BEGIN OF sdm_objects.
**TYPES:  bom             TYPE matnr.
**TYPES:  werks           TYPE werks_d.
**TYPES:  sdm_instances   TYPE /gda/sdm_t_instances.
**TYPES:  mast            TYPE /gda/sdm_t_mast.
**TYPES:  stko            TYPE stko_tab.
**TYPES:  stas            TYPE /gda/sdm_t_stas.
**TYPES:  stpo            TYPE stpo_tab.
**TYPES END OF sdm_objects.
*
*TYPES BEGIN OF objects.
*TYPES: type   TYPE /gda/sdm_de_type.
*TYPES: class  TYPE classname.
*TYPES: include TYPE include.
*TYPES: object TYPE REF TO /gda/sdm_cl_bom.
*TYPES: mapping TYPE REF TO /gda/sdm_cl_brf_mapping.
*TYPES END OF objects.
*
***// Select-Options
*DATA: BEGIN OF gs_so,
*        matnr TYPE mast-matnr,
*        werks TYPE mast-werks,
*        stlnr TYPE stko-stlnr,
*        stlal TYPE stko-stlal,
*        datuv TYPE stko-datuv,
*        andat TYPE stko-andat,
*        loekz TYPE stko-loekz,
*        msgno TYPE t100-msgnr,
*      END OF gs_so.
*
*DATA:
**  go_bom         TYPE REF TO /gda/sdm_cl_bom,
*  gt_sdm_bom     TYPE STANDARD TABLE OF /gda/sdm_s_bom,
*  gt_objects     TYPE STANDARD TABLE OF /gda/sdm_s_objects, "objects,
**  gt_attributes  TYPE STANDARD TABLE OF /gda/sdm_cl_bom=>ty_brf_attributes,
**  gv_mvke_first,
*  gs_sdm_objects TYPE /gda/sdm_s_bom,
*  gs_selscreen   TYPE lcl_mm_bom_exc=>ty_selscreen,
*  go_selection   TYPE REF TO lcl_mm_bom_exc.
*
*data:
*  gt_calcs1     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_message,
*  gt_calcs2     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_mtart,
*  gt_calcs3     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_mstae,
*  gt_calcs4     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_matkl,
*  gt_calcs5     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_attyp.
