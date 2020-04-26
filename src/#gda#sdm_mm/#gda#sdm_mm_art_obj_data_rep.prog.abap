*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_OBJ_DATA_REP
*&---------------------------------------------------------------------*

DATA:
  gt_mara_sdm     TYPE STANDARD TABLE OF /GDA/SDM_S_MARA,
  gt_makt_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_makt,
  gt_marc_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_marc,
  gt_mard_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_mard,
  gt_mbew_sdm     TYPE STANDARD TABLE OF /gda/sdm_s_mbew,

  gs_mara_sdm     TYPE /gda/sdm_s_mara,
  gs_marc_sdm     TYPE /gda/sdm_s_marc,
  gs_mard_sdm     TYPE /gda/sdm_s_mard,
  gs_mvke_sdm     TYPE /gda/sdm_s_mvke,
  gs_mbew_sdm     TYPE /gda/sdm_s_mbew,

  gt_mlgn_sdm     TYPE /gda/sdm_t_mlgn,
  gt_mlgt_sdm     TYPE /gda/sdm_t_mlgt,
  gt_mvke_sdm     TYPE /gda/sdm_t_mvke,
  gt_mlea_sdm     TYPE /gda/sdm_t_mlea,
  gs_makt_sdm     TYPE makt,
  gt_rmmw1_sdm    TYPE STANDARD TABLE OF rmmw1,
  gt_eord_sdm     TYPE STANDARD TABLE OF eord,
  gt_mast_sdm     TYPE STANDARD TABLE OF mast,
*  gs_rmmw1         TYPE rmmw1,
  gt_meinh_sdm    TYPE /gda/sdm_t_meinh,
  gt_mamt_sdm     TYPE STANDARD TABLE OF mamt,
  gt_malg_sdm     TYPE STANDARD TABLE OF malg,
*  gt_basic_text    TYPE lvc_t_tlin,
  gt_myms_sdm     TYPE STANDARD TABLE OF myms,
  gt_maw1_sdm     TYPE STANDARD TABLE OF maw1,
  gt_mwli_sdm     TYPE /gda/sdm_t_mwli,
  gt_eina_sdm     TYPE mmpr_eina,
  gt_eine_sdm     TYPE mmpr_eine,
  gt_mean_sdm     TYPE STANDARD TABLE OF mean,
  gt_konh_sdm     TYPE STANDARD TABLE OF konh,
  gt_wlk2_sdm     TYPE STANDARD TABLE OF wlk2,
  gt_wlk1_sdm     TYPE STANDARD TABLE OF wlk1,
  gt_mg03_sdm     TYPE /gda/sdm_t_mlan,
  gs_mg03_sdm     TYPE /gda/sdm_mlan,
  gt_maritc_sdm   TYPE /gda/sdm_t_maritc,
*  gs_maritc_sdm   TYPE /gda/sdm_maritc,
  gt_tariff_sdm   TYPE /gda/sdm_tt_tariffs,
  gt_src_list_sdm TYPE /gda/sdm_tt_srclist,
  gt_pricing_sdm  type /GDA/SDM_T_PRICING,
  gt_mg03_sdm_brf TYPE mat_steuer,
  gs_mg03_sdm_brf TYPE mg03steuer,
  gs_mlgn_sdm     TYPE mlgn,
  gs_maw1_sdm     TYPE maw1,
  gs_syst_sdm     TYPE syst.

*ENHANCEMENT-POINT /gda/sdm_mm_art_ep3 SPOTS /gda/sdm_mm_art_es3 STATIC .

ENHANCEMENT-POINT /GDA/SDM_MM_ART_EP3 SPOTS /GDA/SDM_MM_ART_ES3 STATIC INCLUDE BOUND .
