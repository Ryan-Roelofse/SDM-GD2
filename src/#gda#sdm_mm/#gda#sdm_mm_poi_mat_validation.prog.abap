*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_ART_VALIDATION
*&---------------------------------------------------------------------*
INCLUDE /gda/sdm_mm_poi_mat_data.
INCLUDE /gda/sdm_mm_mat_obj_data.
*INCLUDE /gda/sdm_common_core1.

DATA: ls_mara   TYPE /gda/sdm_s_mara,
      ls_stext  TYPE short_desc, "MGINT_SHORT_DESC,
      ls_marc   TYPE /gda/sdm_s_marc,
      ls_mard   TYPE /gda/sdm_s_mard,
      ls_mbew   TYPE /gda/sdm_s_mbew,
      ls_mvke   TYPE /gda/sdm_s_mvke,
      ls_mlgn   TYPE /gda/sdm_s_mlgn,
      ls_mlgt   TYPE /gda/sdm_s_mlgt,
      ls_mpop   TYPE /gda/sdm_s_mpop,
      ls_meinh  TYPE /gda/sdm_s_meinh,
      ls_stat   TYPE mgstat,
      ls_mean   TYPE /gda/sdm_s_mean,
      ls_mfhm   TYPE /gda/sdm_s_mfhm,
      ls_steuer TYPE mg03steuer,
      ls_steumm TYPE mg03steumm.

FIELD-SYMBOLS:
  <wmeinh> LIKE LINE OF wmeinh,
  <stext>  LIKE LINE OF stext,
  <mean>   LIKE LINE OF smean_me_tab,
  <steuer> LIKE LINE OF ssteuertab,
  <steumm> LIKE LINE OF ssteummtab.

* Set default SDM Type and include any customr SDM Types
gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gc_val
                                                     x_sdm_source = gc_poe ).

* Build a list of all the relevant SDM objects
gt_objects = /gda/sdm_cl_common_core=>get_sdm_objects( x_sdm_obect  = gc_material
                                                       xt_sdm_types = gr_sdm_type ).

CHECK gt_objects[] IS NOT INITIAL.

*WMARA - GS_MARA_SDM
*WMARC - GS_MARC_SDM
*WMARD - GS_MARD_SDM
*WMBEW - GT_MBEW_SDM
*WMLGN - GT_MLGN_SDM
*WMLGT - GT_MLGT_SDM
*WMVKE - GT_MVKE_SDM
*WSTAT        - available in signature - still to do
*WMFHM  - GT_MFHM_SDM
*WMPOP  - GT_MPOP_SDM
*SSTEUERTAB   - GT_STEUER_SDM - Still to do -- Create /GDA/Structure
*SSTEUMMTAB   - GT_STEUMM_SDM - Still to do -- Create /GDA/Structure
*WMEINH - GT_MEINH_SDM
*SMEAN_ME_TAB - GT_MEAN_SDM


MOVE-CORRESPONDING wmara TO gs_mara_sdm.
gs_mara_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARA'
                                                                         i_contents = gs_mara_sdm ).

LOOP AT stext ASSIGNING <stext>.
  MOVE-CORRESPONDING <stext> TO gs_makt_sdm.
  gs_makt_sdm-matnr = gs_mara_sdm-matnr.
  gs_makt_sdm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MAKT'
                                                                           i_contents = gs_makt_sdm ).
  APPEND gs_makt_sdm TO gt_makt_sdm.
  CLEAR:
   gs_makt_sdm.
ENDLOOP.

IF NOT wmarc IS INITIAL.
  MOVE-CORRESPONDING wmarc TO ls_marc.
  ls_marc-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARC'
                                                                           i_contents = ls_marc ).
  APPEND ls_marc TO gt_marc_sdm.
ENDIF.

IF NOT wmard IS INITIAL.
  MOVE-CORRESPONDING wmard TO ls_mard.
  ls_mard-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MARD'
                                                                           i_contents = ls_mard ).
  APPEND ls_mard TO gt_mard_sdm.
ENDIF.

IF NOT wmbew IS INITIAL.
  MOVE-CORRESPONDING wmbew TO ls_mbew.
  ls_mbew-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MBEW'
                                                                           i_contents = ls_mbew ).
  APPEND ls_mbew TO gt_mbew_sdm.
ENDIF.

IF NOT wmlgn IS INITIAL.
  MOVE-CORRESPONDING wmlgn TO ls_mlgn.
  ls_mlgn-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLGN'
                                                                           i_contents = ls_mlgn ).

  APPEND ls_mlgn TO gt_mlgn_sdm.
ENDIF.

IF NOT wmlgt IS INITIAL.
  MOVE-CORRESPONDING wmlgt TO ls_mlgt.
  ls_mlgt-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MLGT'
                                                                           i_contents = ls_mlgt ).

  APPEND ls_mlgt TO gt_mlgt_sdm.
ENDIF.

IF NOT wmvke IS INITIAL.
  MOVE-CORRESPONDING wmvke TO ls_mvke.
  ls_mvke-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MVKE'
                                                                           i_contents = ls_mvke ).
  APPEND ls_mvke TO gt_mvke_sdm.
ENDIF.

IF NOT wmeinh[] IS INITIAL.
  LOOP AT wmeinh ASSIGNING <wmeinh>.
    MOVE-CORRESPONDING <wmeinh> TO ls_meinh.
    ls_meinh-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MEINH'
                                                                             i_contents = ls_meinh ).
    APPEND ls_meinh TO gt_meinh_sdm.
  ENDLOOP.
ENDIF.

*IF NOT wstat IS INITIAL.
*  MOVE-CORRESPONDING wstat TO ls_stat.
*  ls_stat-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MVKE'
*                                                                           i_contents = ls_mvke ).
*  APPEND ls_stat TO gt_stat_sdm.
*ENDIF.

IF NOT wmpop IS INITIAL.
  MOVE-CORRESPONDING wmpop TO ls_mpop.
  ls_mpop-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MPOP'
                                                                           i_contents = ls_mpop ).
  APPEND ls_mpop TO gt_mpop_sdm.
ENDIF.

IF NOT smean_me_tab IS INITIAL.
  LOOP AT smean_me_tab ASSIGNING <mean>.
    MOVE-CORRESPONDING <mean> TO ls_mean.
    ls_mean-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MEAN'
                                                                             i_contents = ls_mean ).
    APPEND ls_mean TO gt_mean_sdm.
  ENDLOOP.
ENDIF.

IF NOT wmfhm IS INITIAL.
  MOVE-CORRESPONDING wmfhm TO ls_mfhm.
  ls_mfhm-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = 'MFHM'
                                                                           i_contents = ls_mfhm ).
ENDIF.

IF NOT ssteuertab IS INITIAL.
  LOOP AT ssteuertab ASSIGNING <steuer>.
    MOVE-CORRESPONDING <steuer> TO ls_steuer.
*    ls_mean-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = ''
*                                                                             i_contents = ls_mean ).
    APPEND ls_steuer TO gt_steuer_sdm.
  ENDLOOP.
ENDIF.

IF NOT ssteummtab IS INITIAL.
  LOOP AT ssteummtab ASSIGNING <steumm>.
    MOVE-CORRESPONDING <steumm> TO ls_steumm.
*    ls_mean-sdm_tabkey = /gda/cl_sdm_data_model_main=>build_string_from_key( i_tabname  = ''
*                                                                             i_contents = ls_mean ).
    APPEND ls_steumm TO gt_steumm_sdm.
  ENDLOOP.
ENDIF.

LOOP AT gt_objects ASSIGNING <objects>.
  TRY.
      <objects>-object ?= /gda/sdm_cl_core=>factory( iv_object_type = gc_material
                                                     iv_source      = gc_poe
                                                     iv_type        = <objects>-type
                                                     iv_stats       = abap_true ).
    CATCH cx_fdt_input INTO gx_fdt.

      IF <objects>-object IS NOT INITIAL.
        <objects>-object->display_messages( ).
        EXIT.
      ENDIF.
  ENDTRY.


  CHECK <objects> IS ASSIGNED.
  CHECK <objects>-object IS BOUND.
  CHECK <objects>-object->is_active( ) = abap_true
    AND <objects>-object->mt_message[] IS INITIAL.
*  CHECK <objects>-object->is_active( ) = abap_true AND <objects>-object->mt_message[] IS INITIAL.

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

* Collect all the results..
  IF <results_val_all> IS NOT ASSIGNED.
    IF <objects>-object IS BOUND.
      gr_data_empty  = <objects>-object->return_brf_result_structure( ).
      ASSIGN gr_data_empty->* TO <results_val_all>.
    ENDIF.
  ENDIF.

  APPEND LINES OF <results_val> TO <results_val_all>.
ENDLOOP.

CHECK <results_val_all> IS ASSIGNED.
* Process Message#
LOOP AT <results_val_all> ASSIGNING <result_val> WHERE type CA 'EAX'.
  IF <result_val>-id IS INITIAL.
    <result_val>-id = '/GDA/SDM1'.
  ENDIF.

  IF <result_val>-number IS INITIAL.
    <result_val>-number = '002'.
  ENDIF.

  IF <result_val>-message IS NOT INITIAL.
* Output only message
    MESSAGE ID <result_val>-id TYPE 'E' NUMBER <result_val>-number
     WITH <result_val>-message
     RAISING application_error.

  ELSE.
* Output Variable parts
    MESSAGE ID <result_val>-id TYPE 'E' NUMBER <result_val>-number
     WITH <result_val>-message_v1 <result_val>-message_v2
          <result_val>-message_v3 <result_val>-message_v4
       RAISING application_error.

  ENDIF.
ENDLOOP.
