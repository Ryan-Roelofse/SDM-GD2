*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ARTICLE_DATA
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_data_core.

TABLES:
  mara,
  marc,
  mard,
  mvke,
  eord,
  t100.

CONSTANTS:
  gc_material TYPE /gda/sdm_de_object VALUE 'MATERIAL',
  gc_auth_rsr TYPE xuobject           VALUE '/GDA/MAT_R',
  gc_auth_e   TYPE xuobject           VALUE '/GDA/MAT_E'.



TYPES BEGIN OF objects.
TYPES: type   TYPE /gda/sdm_de_type.
TYPES: class  TYPE classname.
TYPES: include TYPE include.
TYPES: object TYPE REF TO /gda/sdm_cl_material.
TYPES: mapping TYPE REF TO /gda/sdm_cl_brf_mapping.
TYPES END OF objects.

**// Select-Options
DATA: BEGIN OF gs_so,
        matnr TYPE mara-matnr,
        ersda TYPE mara-ersda,
        mtart TYPE mara-mtart,
        mstae TYPE mara-mstae,
        matkl TYPE mara-matkl,
        lvorm TYPE mara-lvorm,
        werks TYPE marc-werks,
        mmsta TYPE marc-mmsta,
        lgort TYPE mard-lgort,
        vkorg TYPE mvke-vkorg,
        vtweg TYPE mvke-vtweg,
        bwkey TYPE mbew-bwkey,
        bwtar TYPE mbew-bwtar,
        lgnum TYPE mlgn-lgnum,
        lgtyp TYPE mlgt-lgtyp,
        msgno TYPE t100-msgnr,
      END OF gs_so.

DATA:
  go_material     TYPE REF TO /gda/sdm_cl_material ##NEEDED,
  gt_sdm_material TYPE STANDARD TABLE OF /gda/sdm_s_material,
*  gt_objects      TYPE STANDARD TABLE OF /gda/sdm_s_objects, "objects,
*  gt_attributes   TYPE STANDARD TABLE OF go_material->ty_brf_attributes,
  gv_mvke_first,
  gs_sdm_objects  TYPE /gda/sdm_s_material,
  gs_selscreen    TYPE lcl_mm_material_exc=>ty_selscreen,
  go_selection    TYPE REF TO lcl_mm_material_exc.

DATA:
  gt_mara       TYPE STANDARD TABLE OF /gda/sdm_s_mara,
  gt_marc       TYPE STANDARD TABLE OF /gda/sdm_s_marc,
  gt_marc_temp  TYPE STANDARD TABLE OF /gda/sdm_s_marc,
  gt_mard       TYPE STANDARD TABLE OF /gda/sdm_s_mard,
  gt_mard_temp  TYPE STANDARD TABLE OF /gda/sdm_s_mard,
  gt_mvke       TYPE STANDARD TABLE OF /gda/sdm_s_mvke,
  gt_mvke_temp  TYPE STANDARD TABLE OF /gda/sdm_s_mvke,
  gt_mbew       TYPE STANDARD TABLE OF /gda/sdm_s_mbew,
  gt_mbew_temp  TYPE STANDARD TABLE OF /gda/sdm_s_mbew,
  gt_mlgn       TYPE /gda/sdm_t_mlgn,
  gt_mlgn_temp  TYPE /gda/sdm_t_mlgn,
  gt_mlgt       TYPE /gda/sdm_t_mlgt,
  gt_mlgt_temp  TYPE /gda/sdm_t_mlgt,
  gt_marm       TYPE STANDARD TABLE OF marm,
  gt_marm_temp  TYPE STANDARD TABLE OF marm,
  gt_meinh      TYPE mat_meinh,
  gt_meinh_temp TYPE mat_meinh,
  gt_makt       TYPE STANDARD TABLE OF makt,
  gt_makt_temp  TYPE STANDARD TABLE OF makt,
  gs_makt_temp  TYPE /gda/sdm_s_makt, "makt,
  gt_ktex       TYPE mat_ktext,
  gt_mamt       TYPE STANDARD TABLE OF mamt,
  gt_mamt_temp  TYPE STANDARD TABLE OF mamt,
  gt_malg       TYPE STANDARD TABLE OF malg,
  gt_malg_temp  TYPE STANDARD TABLE OF malg,
  gt_basic_text TYPE lvc_t_tlin,
  gt_mpop       TYPE /gda/sdm_t_mpop,
  gs_mara       TYPE /gda/sdm_s_mara,
  gs_makt       TYPE makt.

DATA:
  gt_mapr      TYPE SORTED TABLE OF mapr   WITH UNIQUE KEY matnr werks,
  gt_crvm      TYPE SORTED TABLE OF crvm_b WITH UNIQUE KEY matnr werks objty objid,
  gt_mean      TYPE SORTED TABLE OF mean   WITH UNIQUE KEY mandt matnr meinh lfnum,
  gt_mean_temp TYPE SORTED TABLE OF mean   WITH UNIQUE KEY mandt matnr meinh lfnum.

DATA:
  gt_calcs1 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_message,
  gt_calcs2 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_mtart,
  gt_calcs3 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_mstae,
  gt_calcs4 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_matkl,
  gt_calcs5 TYPE STANDARD TABLE OF /gda/sdm_s_calcs_attyp.


DATA:
  ls_meinh  TYPE smeinh,
  ls_mpop   TYPE mpop,     "Forecasting
  ls_mfhm   TYPE mfhm,     "PRT
  ls_smeinh TYPE smeinh,   "UoM
  ls_mwli   TYPE mwli.     "Listing (Retail)

DATA:
*  go_selection TYPE REF TO /gda/sdm_cl_selections,
*  gs_selscreen,
  gt_results   TYPE STANDARD TABLE OF /gda/sdm_s_val_results_key.

FIELD-SYMBOLS:
  <results> LIKE gt_results,
  <result>  LIKE LINE OF gt_results.

FIELD-SYMBOLS:
  <results_collated> TYPE STANDARD TABLE,
  <field>            TYPE any,
  <any>              TYPE any,
  <mara>             LIKE LINE OF gt_mara,
  <marc>             LIKE LINE OF gt_marc,
  <makt>             LIKE LINE OF gt_makt,
  <mard>             LIKE LINE OF gt_mard,
  <mvke>             LIKE LINE OF gt_mvke,
  <mbew>             LIKE LINE OF gt_mbew,
  <mlgn>             LIKE LINE OF gt_mlgn,
  <mlgt>             LIKE LINE OF gt_mlgt,
  <marm>             LIKE LINE OF gt_marm,
  <mamt>             LIKE LINE OF gt_mamt,
  <malg>             LIKE LINE OF gt_malg,
  <ttext>            TYPE sktext,
  <mapr>             LIKE LINE OF gt_mapr,
  <mean>             LIKE LINE OF gt_mean.
