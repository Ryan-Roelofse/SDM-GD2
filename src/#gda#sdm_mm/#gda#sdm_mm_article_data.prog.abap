*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ARTICLE_DATA
*&---------------------------------------------------------------------*
DATA: gv_message TYPE string.

TYPES BEGIN OF sdm_objects.
TYPES:  article    TYPE matnr.
TYPES:  maktx	     TYPE	maktx.
TYPES:  mtart	     TYPE	mtart.
TYPES:  matkl	     TYPE	matkl.
TYPES:  mstae	     TYPE	mstae.
TYPES:  sdm_object TYPE REF TO /gda/sdm_cl_core.
TYPES END OF sdm_objects.

CONSTANTS:
  gc_article TYPE /gda/sdm_de_object VALUE 'ARTICLE'.

DATA:
  go_article      TYPE REF TO /gda/sdm_cl_article,
  gx_article      TYPE REF TO /gda/cx_sdm_exception_handl,
  gt_sdm_articles TYPE STANDARD TABLE OF sdm_objects,
  gs_sdm_objects  TYPE sdm_objects,
  gv_type         TYPE /gda/sdm_de_type,
  gv_source,
  gv_table        TYPE dd02t-tabname,
  ro_data         TYPE REF TO data,
  ro_collated     TYPE REF TO data,
  ro_salv         TYPE REF TO cl_salv_table.

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
  gs_makt_temp  TYPE makt,
  gt_ktex       TYPE mat_ktext,
  gt_mamt       TYPE STANDARD TABLE OF mamt,
  gt_mamt_temp  TYPE STANDARD TABLE OF mamt,
  gt_malg       TYPE STANDARD TABLE OF malg,
  gt_malg_temp  TYPE STANDARD TABLE OF malg,
  gt_basic_text TYPE lvc_t_tlin,
  gt_mpop       TYPE /gda/sdm_t_mpop.


DATA:
  gt_mapr      TYPE SORTED TABLE OF mapr   WITH UNIQUE KEY matnr werks,
  gt_crvm      TYPE SORTED TABLE OF crvm_b WITH UNIQUE KEY matnr werks objty objid,
  gt_mean      TYPE SORTED TABLE OF mean   WITH UNIQUE KEY mandt matnr meinh lfnum,
  gt_mean_temp TYPE SORTED TABLE OF mean   WITH UNIQUE KEY mandt matnr meinh lfnum,
  gt_eina      TYPE SORTED TABLE OF eina   WITH NON-UNIQUE KEY mandt matnr,
  gt_eine      TYPE SORTED TABLE OF eine   WITH NON-UNIQUE KEY mandt infnr ekorg esokz werks,
  gt_eina_temp TYPE mmpr_eina,
  gt_eine_temp TYPE mmpr_eine,
  gt_wlk2      TYPE SORTED TABLE OF wlk2 WITH NON-UNIQUE KEY mandt matnr,
  gt_maw1      TYPE SORTED TABLE OF maw1 WITH UNIQUE KEY mandt matnr,
  gt_maw1_temp TYPE SORTED TABLE OF maw1 WITH UNIQUE KEY mandt matnr,
  gt_mwli      TYPE SORTED TABLE OF mwli WITH UNIQUE KEY mandt matnr, "Populated from MAW1
  gt_mwli_temp TYPE SORTED TABLE OF mwli WITH UNIQUE KEY mandt matnr, "Populated from MAW1
  gt_mlea      TYPE SORTED TABLE OF mlea WITH NON-UNIQUE KEY mandt matnr,
  gt_myms      TYPE SORTED TABLE OF myms WITH NON-UNIQUE KEY mandt matnr,
  gt_myms_temp TYPE SORTED TABLE OF myms WITH NON-UNIQUE KEY mandt matnr.

DATA:
  ls_meinh   TYPE smeinh.

DATA: ls_mpop   TYPE /gda/sdm_s_mpop,"mpop,       "Forecasting
      ls_mfhm   TYPE mfhm,       "PRT
      ls_smeinh TYPE smeinh,   "UoM
      ls_mwli   TYPE mwli.       "Listing (Retail)

FIELD-SYMBOLS:
  <results>          TYPE ANY TABLE,
  <result>           TYPE any,
  <article>          LIKE LINE OF gt_sdm_articles,
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
  <mean>             LIKE LINE OF gt_mean,
  <eina>             LIKE LINE OF gt_eina,
  <eine>             LIKE LINE OF gt_eine,
  <maw1>             LIKE LINE OF gt_maw1,
  <myms>             LIKE LINE OF gt_myms.
