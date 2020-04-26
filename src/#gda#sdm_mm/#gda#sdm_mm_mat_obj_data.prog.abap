*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_MAT_OBJ_DATA_REP
*&---------------------------------------------------------------------*
DATA:
  gt_mara_sdm        TYPE STANDARD TABLE OF /gda/sdm_s_mara,
  gt_makt_sdm        TYPE STANDARD TABLE OF /gda/sdm_s_makt,
  gt_marc_sdm        TYPE STANDARD TABLE OF /gda/sdm_s_marc,
  gt_mard_sdm        TYPE STANDARD TABLE OF /gda/sdm_s_mard,
  gt_mbew_sdm        TYPE STANDARD TABLE OF /gda/sdm_s_mbew,
  gt_mlgn_sdm        TYPE /gda/sdm_t_mlgn,
  gt_mlgt_sdm        TYPE /gda/sdm_t_mlgt,
  gt_mapr_sdm        TYPE /gda/sdm_t_mapr,
  gt_crvm_sdm        TYPE /gda/sdm_t_crvm,
  gt_mlan_sdm        TYPE /gda/sdm_t_mlan,
  gt_marm_sdm        TYPE /gda/sdm_t_marm,
  gt_mean_sdm        TYPE /gda/sdm_t_mean,
  gt_stat_sdm        TYPE mgstat,            "still to do
  gt_mfhm_sdm        TYPE /gda/sdm_t_mfhm,   "still to do
  gt_mpop_sdm        TYPE /gda/sdm_t_mpop,
  gt_mg03steuer_sdm  TYPE mgint_mg03steuer,  "still to do
  gt_mg03steumm_sdm  TYPE mgint_mg03steumm,  "still to do
  gt_meinh_sdm       TYPE /gda/sdm_t_meinh, "mgint_smeinh,      "still to do
  gt_me_mean_tab_sdm TYPE mgint_me_mean_tab, "still to do
  gt_steuer_sdm      TYPE mat_steuer,
  gt_steumm_sdm      TYPE mat_steumm,
  gs_mara_sdm        TYPE /gda/sdm_s_mara,
  gs_marc_sdm        TYPE /gda/sdm_s_marc,
  gs_mard_sdm        TYPE /gda/sdm_s_mard,
  gs_mvke_sdm        TYPE /gda/sdm_s_mvke,
  gs_mbew_sdm        TYPE /gda/sdm_s_mbew,
  gs_mlgn_sdm        TYPE /gda/sdm_s_mlgn,
  gt_mvke_sdm        TYPE /gda/sdm_t_mvke,
  gs_makt_sdm        TYPE /gda/sdm_s_makt,
  gs_syst_sdm        TYPE syst.
