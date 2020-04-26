*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_OBJECT_DATA
*&---------------------------------------------------------------------*
DATA:
  gt_mara_sdm     TYPE STANDARD TABLE OF mara,
  gt_makt_sdm     TYPE STANDARD TABLE OF makt,
  gt_marc_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_marc,
  gt_mard_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_mard,
  gt_mvke_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_mvke,
  gt_mbew_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_mbew,

  gs_mara_sdm     TYPE /gda/sdm_s_mara,
  gs_marc_sdm     TYPE /gda/sdm_s_marc,
  gs_mard_sdm     TYPE /gda/sdm_s_mard,
  gs_mvke_sdm     TYPE /gda/sdm_s_mvke,
  gs_mbew_sdm     TYPE /gda/sdm_s_mbew,

  gt_mlgn_sdm     TYPE /gda/sdm_t_mlgn,
  gt_mlgt_sdm     TYPE /gda/sdm_t_mlgt,
  gs_makt_sdm     TYPE makt,
  gt_rmmw1_sdm    TYPE STANDARD TABLE OF rmmw1,
  gt_eord_sdm     TYPE STANDARD TABLE OF eord,
  gt_mast_sdm     TYPE STANDARD TABLE OF mast,
*  gs_rmmw1         TYPE rmmw1,
  gt_meinh_sdm    TYPE /gda/sdm_t_meinh,
  gs_meinh_sdm    TYPE /gda/sdm_s_meinh,
  gt_mlea_sdm     TYPE /gda/sdm_t_mlea,
  gs_mlea_sdm     TYPE /gda/sdm_s_mlea,
  gt_mamt_sdm     TYPE STANDARD TABLE OF mamt,
  gt_malg_sdm     TYPE STANDARD TABLE OF malg,
*  gt_basic_text    TYPE lvc_t_tlin,
  gt_myms_sdm     TYPE STANDARD TABLE OF myms,
  gt_maw1_sdm     TYPE SORTED TABLE OF maw1 WITH UNIQUE KEY mandt matnr,
  gt_mwli_sdm     TYPE /gda/sdm_t_mwli,
*  gt_meinh_sdm    TYPE /gda/sdm_t_meinh,
  gt_eina_sdm     TYPE mmpr_eina,
  gt_eine_sdm     TYPE mmpr_eine,
  gt_mean_sdm     TYPE SORTED TABLE OF mean WITH UNIQUE KEY mandt matnr meinh lfnum,
  gt_konh_sdm     TYPE STANDARD TABLE OF konh,
  gt_wlk2_sdm     TYPE STANDARD TABLE OF wlk2,
  gt_wlk1_sdm     TYPE STANDARD TABLE OF wlk1,
  gt_mg03_sdm     TYPE /gda/sdm_t_mlan,
  gs_mg03_sdm     TYPE /gda/sdm_mlan,
  gt_mg03_sdm_brf TYPE mat_steuer,
  gs_mg03_sdm_brf TYPE mg03steuer,
  gs_mlgn_sdm     TYPE mlgn,
  gs_maw1_sdm     TYPE maw1,
  gs_syst_sdm     TYPE syst.

* Enhancement Point: Declare non standard SDM tables and structures( Validations and Derivations)
ENHANCEMENT-POINT /gda/sdm_mm_art_ep1 SPOTS /gda/sdm_mm_art_es1 INCLUDE BOUND .
