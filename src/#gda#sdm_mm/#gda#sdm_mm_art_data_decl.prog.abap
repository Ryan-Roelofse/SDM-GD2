*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_DATA_DECL
*&---------------------------------------------------------------------*

TABLES:
  mara,
  marc,
  mard,
  mvke,
  eord,
  eina,
  eine,
  t100.

TYPE-POOLS:
  abap,
  icon.


*CLASS: lcl_event_rec DEFINITION DEFERRED.
*
*DATA:
*  go_handler_local            TYPE REF TO lcl_event_rec.


TYPES BEGIN OF struc_rel.
TYPES: matnr     TYPE mara-matnr.
TYPES: matnr_rel TYPE mara-matnr.
TYPES END OF struc_rel.

TYPES BEGIN OF struc_tax.
TYPES: matnr     TYPE mara-matnr.
TYPES: mg03steuer TYPE mg03steuer.
TYPES END OF struc_tax.

DATA:
  BEGIN OF gs_mard,
    matnr TYPE marc-matnr,
    werks TYPE mard-werks,
    lgort TYPE mard-lgort,
    lgpbe TYPE mard-lgpbe,
  END OF gs_mard,

  BEGIN OF gs_stpo,
    idnrk TYPE stpo-idnrk,
    stlty TYPE stpo-stlty,
    stlnr TYPE stpo-stlnr,
  END OF gs_stpo,

  BEGIN OF gs_tpst,
    stlnr TYPE tpst-stlnr,
    stlal TYPE tpst-stlal,
    tplnr TYPE tpst-tplnr,
    werks TYPE tpst-werks,
  END OF gs_tpst,

*  BEGIN OF gs_iflo,
*    tplnr TYPE iflo-tplnr,
*    pltxt TYPE iflo-pltxt,
*  END OF gs_iflo,

  BEGIN OF gs_stko,
    stlnr TYPE stko-stlnr,
    stlal TYPE stko-stlal,
    stlst TYPE stko-stlst,
  END OF gs_stko,

  BEGIN OF gs_eqst,
    stlnr TYPE eqst-stlnr,
    stlal TYPE eqst-stlal,
    equnr TYPE eqst-equnr,
    werks TYPE eqst-werks,
  END OF gs_eqst,

  gs_mg03steuer TYPE mg03steuer.

TYPES BEGIN OF cond_header.
TYPES: matnr TYPE mara-matnr.
TYPES: knumh TYPE konh-knumh.
TYPES END OF cond_header.

DATA:
  gv_eina_first,
  gv_eord_first,
  gv_mvke_first,
  gt_sdm_articles TYPE STANDARD TABLE OF /gda/sdm_s_article,
  gs_sdm_objects  TYPE /gda/sdm_s_article,
*  go_handler_top  TYPE REF TO lcl_event_rec,

  gt_mara         TYPE STANDARD TABLE OF /gda/sdm_s_mara,
  gt_mara_struc   TYPE STANDARD TABLE OF /gda/sdm_s_mara,
  gt_relations    TYPE STANDARD TABLE OF struc_rel,
  gs_relations    TYPE struc_rel,
  gs_mara         TYPE /gda/sdm_s_mara,
  gt_makt         TYPE STANDARD TABLE OF makt,
  gs_makt         TYPE makt,
*  gs_marc        TYPE marc,
  gt_marc         TYPE STANDARD TABLE OF marc,
  gt_mard         LIKE TABLE OF gs_mard,
  gt_mbew         TYPE STANDARD TABLE OF /gda/sdm_s_mbew,
  gt_mvke         TYPE STANDARD TABLE OF mvke,
  gt_wlk1         TYPE STANDARD TABLE OF wlk1,
  gt_wlk2         TYPE STANDARD TABLE OF wlk2,
  gt_mean         TYPE STANDARD TABLE OF mean,
*  gs_mean        TYPE mean,
  gt_maw1         TYPE STANDARD TABLE OF maw1,
  gt_eina         LIKE STANDARD TABLE OF eina,
  gt_eine         TYPE STANDARD TABLE OF eine,
  gt_eord         TYPE STANDARD TABLE OF eord,
  gt_stpo         LIKE TABLE OF gs_stpo,
*  gt_tpst        LIKE TABLE OF gs_tpst,
  gt_mg03         TYPE STANDARD TABLE OF struc_tax,
  gs_mg03         TYPE struc_tax,
*  gt_iflo        LIKE TABLE OF gs_iflo,
  gt_stko         LIKE TABLE OF gs_stko,
  gt_eqst         LIKE TABLE OF gs_eqst,
  gt_mast         TYPE STANDARD TABLE OF mast,
  gt_steuer       LIKE TABLE OF gs_mg03steuer,
  gt_konh         TYPE STANDARD TABLE OF konh,
*  gs_cond_header TYPE cond_header,
  gt_cond_header  TYPE SORTED TABLE OF cond_header WITH NON-UNIQUE KEY matnr knumh,
  gt_pricing      TYPE /gda/sdm_t_pricing,
  gt_mlgn         TYPE /gda/sdm_t_mlgn,
  gt_mlgt         TYPE /gda/sdm_t_mlgt,
  gt_myms         TYPE /gda/sdm_t_myms,
  gt_mwli         TYPE /gda/sdm_t_mwli,
  gt_marm         TYPE STANDARD TABLE OF marm,
  gt_mamt         TYPE STANDARD TABLE OF mamt,
  gt_malg         TYPE STANDARD TABLE OF malg.

* FOR BRFPlus
DATA:
*  gt_mara_sdm     TYPE STANDARD TABLE OF mara,
*  gt_makt_sdm     TYPE STANDARD TABLE OF makt,
*  gs_makt_sdm     TYPE makt,
  gt_ktex       TYPE mat_ktext,
*  gt_marc_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_marc,
*  gt_mard_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_mard,
*  gt_mvke_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_mvke,
*  gt_mbew_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_mbew,
*  gt_mlgn_sdm     TYPE /gda/sdm_t_mlgn,
*  gt_mlgt_sdm     TYPE /gda/sdm_t_mlgt,
*  gt_rmmw1_sdm    TYPE STANDARD TABLE OF rmmw1,
*  gt_eord_sdm     TYPE STANDARD TABLE OF eord,
*  gt_mast_sdm     TYPE STANDARD TABLE OF mast,
  gs_rmmw1      TYPE rmmw1,
*  gt_meinh_sdm    TYPE mat_meinh,
*  gt_mamt_sdm     TYPE STANDARD TABLE OF mamt,
*  gt_malg_sdm     TYPE STANDARD TABLE OF malg,
  gt_basic_text TYPE lvc_t_tlin,
*  gt_myms_sdm     TYPE STANDARD TABLE OF myms,
*  gt_maw1_sdm     TYPE SORTED TABLE OF maw1 WITH UNIQUE KEY mandt matnr,
*  gt_mwli_sdm     TYPE /gda/sdm_t_mwli,
*  gt_eina_sdm     TYPE mmpr_eina,
*  gt_eine_sdm     TYPE mmpr_eine,
*  gt_mean_sdm     TYPE SORTED TABLE OF mean WITH UNIQUE KEY mandt matnr meinh lfnum,
*  gt_konh_sdm     TYPE STANDARD TABLE OF konh,
*  gt_wlk2_sdm     TYPE STANDARD TABLE OF wlk2,
*  gt_wlk1_sdm     TYPE STANDARD TABLE OF wlk1,
*  gt_mg03_sdm     TYPE /gda/sdm_t_mlan,
*  gs_mg03_sdm     TYPE /gda/sdm_mlan,
*  gt_mg03_sdm_brf TYPE mat_steuer,
*  gs_mg03_sdm_brf TYPE mg03steuer.
  gt_calcs1     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_message,
  gt_calcs2     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_mtart,
  gt_calcs3     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_mstae,
  gt_calcs4     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_matkl,
  gt_calcs5     TYPE STANDARD TABLE OF /gda/sdm_s_calcs_attyp.


FIELD-SYMBOLS:
*  <dyn_table>      TYPE STANDARD TABLE,
**  <dyn_table_final> TYPE STANDARD TABLE,
*  <dyn_table_view> TYPE STANDARD TABLE,
*  <dyn_wa>,
*  <dyn_wa_view>,
*  <main_setup>     LIKE LINE OF gt_pp_main_setup,
*  <main_gen>       LIKE LINE OF gt_pp_main_gen,
*  <main_output>    LIKE LINE OF gt_pp_output,
*  <tabstruc>       LIKE LINE OF gt_view_struc,
*  <name>           TYPE any,
  <mara_struc>     LIKE LINE OF gt_mara_struc.

* Enhance2-here
* define new table and temp table
