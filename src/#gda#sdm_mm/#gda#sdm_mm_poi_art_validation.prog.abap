*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_VALIDATION
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_mm_poi_art_data.
INCLUDE /gda/sdm_mm_art_object_data.

DATA: ls_mara         TYPE /gda/sdm_s_mara,
      ls_marc         TYPE /gda/sdm_s_marc,
      ls_mard         TYPE /gda/sdm_s_mard,
      ls_mbew         TYPE /gda/sdm_s_mbew,
      ls_mvke         TYPE /gda/sdm_s_mvke,

*      lt_marc       TYPE /gda/sdm_t_marc,
      lt_mpgd         TYPE /gda/sdm_t_mpgd,
      lt_mard         TYPE /gda/sdm_t_mard,
      lt_mbew         TYPE /gda/sdm_t_mbew,
      lt_mfhm         TYPE /gda/sdm_t_mfhm,
      lt_mlgn         TYPE /gda/sdm_t_mlgn,
      lt_mlgt         TYPE /gda/sdm_t_mlgt,
      lt_mpop         TYPE /gda/sdm_t_mpop,
      lt_mvke         TYPE /gda/sdm_t_mvke,
      lt_myms         TYPE /gda/sdm_t_myms,
      lt_maw1         TYPE /gda/sdm_t_maw1,
      lt_wlk2         TYPE /gda/sdm_t_wlk2,
      lt_mwli         TYPE /gda/sdm_t_mwli,
      lt_rmmw1        TYPE /gda/sdm_t_rmmw1,
      ls_mpgd         TYPE mpgd,
      ls_mfhm         TYPE /gda/sdm_s_mfhm, "mfhm,
      ls_mlgn         TYPE /gda/sdm_s_mlgn,
      ls_mlgt         TYPE /gda/sdm_s_mlgt,
      ls_mpop         TYPE /gda/sdm_s_mpop, " mpop,
      ls_myms         TYPE myms,
      ls_maw1         TYPE maw1,
      ls_wlk2         TYPE wlk2,
      ls_mwli         TYPE mwli,
      lt_basic_text   TYPE swdtline,
      ls_t130m        TYPE t130m,
      lt_malg         TYPE malg_tty,
      lt_mlea         TYPE mlea_tty,
      lt_mamt         TYPE mamt_tty,
      lt_mat_ktext    TYPE mat_ktext,
*      lt_mat_meinh  TYPE mat_meinh,
      lt_mat_steuer   TYPE mat_steuer,
      lt_mat_steumm   TYPE mat_steumm,
      lt_mean         TYPE mean_tab,
      ls_syst         TYPE syst,
      ls_mg03_sdm_brf TYPE mg03steuer.

DATA: ls_eina      TYPE eina,
      ls_eine      TYPE eine,
      ls_merrdat   TYPE merrdat,
      ls_rmmw1     TYPE rmmw1,
      lt_eina      TYPE mmpr_eina,
      lt_eine      TYPE mmpr_eine,
      lv_text_name TYPE thead-tdname,
      lv_count     TYPE i.

FIELD-SYMBOLS:
  <sdm_results> TYPE  /gda/sdm_t_val_results,
  <sdm_result>  TYPE  /gda/sdm_s_val_results,
  <marc>        LIKE  LINE OF gt_marc_sdm,
  <mlea>        LIKE LINE OF tmlea,
  <meinh>       LIKE LINE OF tmeinh,
  <tsteuertab>  LIKE LINE OF tsteuertab.


* Set default SDM Type and include any customr SDM Types
gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gc_val
                                                     x_sdm_source = gc_poe ).

* Build a list of all the relevant SDM objects
gt_objects = /gda/sdm_cl_common_core=>get_sdm_objects( x_sdm_obect = gc_article
                                                       xt_sdm_types = gr_sdm_type ).


** set to default SDM type
*gs_sdm_type-sign   =  'I'.
*gs_sdm_type-option =  'EQ'.
*gs_sdm_type-low    =  gc_val.
*APPEND gs_sdm_type TO gr_sdm_type.
*
*TRY.
*    GET BADI sdm_handle
*      FILTERS
*        sdm_type_main = gc_val.
*
*  CATCH cx_badi_not_implemented.
*    CLEAR sdm_handle.
*ENDTRY.
*
*IF NOT sdm_handle IS INITIAL.
*  CALL BADI sdm_handle->add_sdm_type
*    EXPORTING
*      x_source          = gc_poe
*    CHANGING
*      xt_sdm_type       = gr_sdm_type
*    EXCEPTIONS
*      application_error = 1
*      OTHERS            = 2.
*  IF sy-subrc <> 0.
**          MESSAGE e() RAISING application_error.
*  ENDIF.
*ENDIF.

* build a list of all the relevant SDM objects for Article - Validations
*SELECT * FROM /gda/sdm_setup6 INTO CORRESPONDING FIELDS OF TABLE gt_objects
*  WHERE  sdm_object  = gc_article
*   AND   type        IN gr_sdm_type
*   AND   active      = abap_true.

CHECK gt_objects[] IS NOT INITIAL.

IMPORT rmmw1 TO ls_rmmw1           FROM MEMORY ID 'GD_MM_ARTICLE_VAL_RMMW1'.
APPEND ls_rmmw1 TO gt_rmmw1_sdm.

IMPORT basic_text TO lt_basic_text FROM MEMORY ID 'GD_MM_ARTICLE_VAL_TEXT'.

IMPORT eina TO ls_eina eine TO ls_eine FROM MEMORY ID 'GD_MM_ARTICLE_VAL_EINA_EINE'.
APPEND ls_eina TO gt_eina_sdm.
APPEND ls_eine TO gt_eine_sdm.

MOVE-CORRESPONDING wmara TO gs_mara_sdm.
MOVE-CORRESPONDING wmarc TO ls_marc.


gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).

ls_marc-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                         i_contents = ls_marc ).


MOVE-CORRESPONDING wmard TO ls_mard.
APPEND ls_mard TO gt_mard_sdm.

ls_mpgd = wmpgd.
APPEND ls_mpgd TO lt_mpgd.

MOVE-CORRESPONDING wmbew TO ls_mbew.
APPEND ls_mbew TO gt_mbew_sdm.

MOVE-CORRESPONDING wmfhm TO ls_mfhm.
APPEND ls_mfhm TO lt_mfhm.

MOVE-CORRESPONDING wmlgn TO ls_mlgn.
APPEND ls_mlgn TO gt_mlgn_sdm.

*ls_mlgt = wmlgt.
MOVE-CORRESPONDING wmlgt TO ls_mlgt.
APPEND ls_mlgt TO gt_mlgt_sdm.

MOVE-CORRESPONDING wmpop TO ls_mpop.
APPEND ls_mpop TO lt_mpop.

MOVE-CORRESPONDING wmvke TO ls_mvke.
APPEND ls_mvke TO gt_mvke_sdm.

ls_myms = wmyms.
*APPEND ls_myms TO lt_myms.
APPEND ls_myms TO gt_myms_sdm.

ls_maw1 = wmaw1.
APPEND ls_maw1 TO gt_maw1_sdm.

ls_wlk2 = wwlk2.
APPEND ls_wlk2 TO gt_wlk2_sdm.

ls_mwli = wmwli.
APPEND ls_mwli TO gt_mwli_sdm.

ls_t130m = wstat.

lt_malg = tmalg[].

LOOP AT tmlea ASSIGNING <mlea>.
  MOVE-CORRESPONDING <mlea> TO gs_mlea_sdm.
  APPEND gs_mlea_sdm TO gt_mlea_sdm.
  CLEAR:
   gs_mlea_sdm.
ENDLOOP.

*lt_mlea = tmlea[].

lt_mamt = tmamt[].

DELETE ttext WHERE spras <> sy-langu.  "Delete other languages
lt_mat_ktext = ttext[].


LOOP AT tmeinh ASSIGNING <meinh>.
  MOVE-CORRESPONDING <meinh> TO gs_meinh_sdm.
  APPEND gs_meinh_sdm TO gt_meinh_sdm.
  CLEAR:
   gs_meinh_sdm.
ENDLOOP.

lt_mat_steuer = tsteuertab[].


LOOP AT tsteuertab ASSIGNING <tsteuertab>.
  MOVE-CORRESPONDING <tsteuertab> TO ls_mg03_sdm_brf.
*  gs_mg03_sdm-matnr = gs_mara_sdm-matnr.
  APPEND ls_mg03_sdm_brf TO gt_mg03_sdm_brf.
ENDLOOP.

*  LOOP AT gt_mg03 ASSIGNING <mg03> WHERE matnr = x_mara-matnr.
*    MOVE-CORRESPONDING <mg03>-mg03steuer TO gs_mg03_sdm.
*    gs_mg03_sdm-matnr = <mg03>-matnr.
*    APPEND gs_mg03_sdm TO gt_mg03_sdm.
*
*    MOVE-CORRESPONDING <mg03>-mg03steuer TO gs_mg03_sdm_brf.
*    APPEND gs_mg03_sdm_brf TO gt_mg03_sdm_brf.
*
*    CLEAR:
*      gs_mg03_sdm,
*      gs_mg03_sdm_brf.
*  ENDLOOP.

lt_mat_steumm = tsteummtab[].

lt_mean       = tmean_me_tab[].

ls_syst = sy.

* Enhancement Point: Populate non standard SDM tables and structures
ENHANCEMENT-POINT /gda/sdm_mm_art_ep2 SPOTS /gda/sdm_mm_art_ep2 .


* Only process SDM once!
IF ls_rmmw1-fiwrk NE space.
  ADD 1 TO lv_count.
ENDIF.

IF ls_rmmw1-vzwrk NE space.
  ADD 1 TO lv_count.
ENDIF.

* Store
IF ( ls_marc-werks = ls_rmmw1-fiwrk ) AND
   ( ls_marc-werks = wrmmg1-werks )    AND
     ls_marc-werks IS NOT INITIAL.
  IMPORT gt_marc_sdm = gt_marc_sdm  FROM MEMORY ID ls_marc-matnr.
  READ TABLE gt_marc_sdm ASSIGNING <marc> WITH KEY matnr = ls_marc-matnr
                                               werks = ls_marc-werks.
  IF sy-subrc <> 0.
    APPEND ls_marc TO gt_marc_sdm.
    EXPORT gt_marc_sdm = gt_marc_sdm  TO MEMORY ID ls_marc-matnr.
  ENDIF.
ENDIF.

* DC
IF ( ls_marc-werks = ls_rmmw1-vzwrk  ) AND
   ( ls_marc-werks = wrmmg1-werks )     AND
     ls_marc-werks IS NOT INITIAL.
  IMPORT gt_marc_sdm = gt_marc_sdm  FROM MEMORY ID ls_marc-matnr.
  READ TABLE gt_marc_sdm ASSIGNING <marc> WITH KEY matnr = ls_marc-matnr
                                               werks = ls_marc-werks.
  IF sy-subrc <> 0.
    APPEND ls_marc TO gt_marc_sdm.
    EXPORT gt_marc_sdm = gt_marc_sdm  TO MEMORY ID ls_marc-matnr.
  ENDIF.
ENDIF.

IMPORT gt_marc_sdm = gt_marc_sdm  FROM MEMORY ID ls_marc-matnr.
DESCRIBE TABLE gt_marc_sdm.
IF sy-tfill = lv_count.
  LOOP AT gt_objects ASSIGNING <objects>.
    TRY.
        <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_article
                                                       iv_source      = gc_poe
                                                       iv_type        = <objects>-type
                                                       iv_stats       = abap_true ).
      CATCH cx_fdt_input INTO gx_fdt.

        IF <objects>-object IS NOT INITIAL.
          <objects>-object->display_messages( ).
          EXIT.
        ENDIF.
    ENDTRY.


*    IF <objects>-object IS NOT BOUND.
*      MESSAGE w005(/gda/sdm_art2).
*      RETURN.
*    ENDIF.

  CHECK <objects> IS ASSIGNED.
  CHECK <OBJECTS>-OBJECT IS BOUND.
  CHECK <objects>-object->is_active( ) = abap_true
    AND <objects>-object->mt_message[] IS INITIAL.

*    CHECK <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS INITIAL.
*    IF <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS NOT INITIAL.
*      MESSAGE w005(/gda/sdm_art2).
*      RETURN.
*    ENDIF.
    gt_attributes = <objects>-object->get_object_attributes( iv_type = <objects>-type   ).

    LOOP AT gt_attributes ASSIGNING <attribute>.
      ASSIGN (<attribute>-abap_type) TO <set_data>.
      <objects>-object->set_selection( iv_name = <attribute>-name
                                       iv_data = <set_data>
                                       iv_type = <attribute>-type ).
    ENDLOOP.

    TRY.
        <objects>-object->main( ).
      CATCH /gda/cx_sdm_exception_handl INTO gx_sdm_root.
        gv_message = gx_sdm_root->mv_text.
        IF sy-batch = abap_true.
          WRITE: / gv_message.
        ELSE.
          MESSAGE gv_message TYPE 'I'.
        ENDIF.
        RETURN.
      CATCH cx_fdt_input INTO gx_fdt.
        CALL METHOD gx_fdt->if_message~get_longtext
          RECEIVING
            result = gv_message.
        IF sy-batch = abap_true.
          WRITE: / gv_message.
        ELSE.
          MESSAGE gv_message TYPE 'I'.
        ENDIF.
        RETURN.

    ENDTRY.

    gr_data = <objects>-object->return_brf_result( ).
    ASSIGN gr_data->* TO <results_val>.

    IF <results_val> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

* collect all the results..
    IF <results_val_all> IS NOT ASSIGNED.
      IF <objects>-object IS BOUND.
        gr_data_empty  = <objects>-object->return_brf_result_structure( ).
        ASSIGN gr_data_empty->* TO <results_val_all>.
      ENDIF.
    ENDIF.

    APPEND LINES OF <results_val> TO <results_val_all>.
  ENDLOOP.

  REFRESH:
   gt_marc_sdm.
  EXPORT gt_marc_sdm = gt_marc_sdm  TO MEMORY ID ls_marc-matnr.
ENDIF.

CHECK <results_val_all> IS ASSIGNED.
SORT <results_val_all> BY id number.
DELETE ADJACENT DUPLICATES FROM <results_val_all>.

****// Process Message
LOOP AT <results_val_all> ASSIGNING <result_val> WHERE type CA 'EAX'.
  IF <result_val>-id IS INITIAL.
    <result_val>-id = '/GDA/SDM1'.
  ENDIF.

  IF <result_val>-number IS INITIAL.
    <result_val>-number = '002'.
  ENDIF.

  IF <result_val>-message IS NOT INITIAL.
*/ Output only message
    ls_merrdat-tranc = ls_merrdat-tranc + 1.
    ls_merrdat-matnr = wmara-matnr.
    ls_merrdat-msgid = <result_val>-id.
    ls_merrdat-msgty = <result_val>-type.
    ls_merrdat-msgno = <result_val>-number.
    ls_merrdat-msgv1 = <result_val>-message(50).
    ls_merrdat-msgv2 = <result_val>-message+50(50).
    ls_merrdat-msgv3 = <result_val>-message+100(50).
    ls_merrdat-msgv4 = <result_val>-message+150(50).
    APPEND ls_merrdat TO rt_errdat.
  ELSE.

*/ Output Variable parts
    ls_merrdat-tranc = ls_merrdat-tranc + 1.
    ls_merrdat-matnr = wmara-matnr.
    ls_merrdat-msgid = <result_val>-id.
    ls_merrdat-msgty = <result_val>-type.
    ls_merrdat-msgno = <result_val>-number.
    ls_merrdat-msgv1 = <result_val>-message_v1.
    ls_merrdat-msgv2 = <result_val>-message_v2.
    ls_merrdat-msgv3 = <result_val>-message_v3.
    ls_merrdat-msgv4 = <result_val>-message_v4.
    APPEND ls_merrdat TO rt_errdat.
  ENDIF.
ENDLOOP.
*!
