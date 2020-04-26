**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_SD_COND_VALIDATION
**&---------------------------------------------------------------------*
*
*DATA: lo_condition      TYPE REF TO /gda/sdm_cl_condition,
*      lx_condition      TYPE REF TO /gda/cx_sdm_exception_handl,
*      lv_message        TYPE string,
*      lr_data           TYPE REF TO data,
*      la_cond_konhdb_t  TYPE cond_konhdb_t,
*      la_cond_konpdb_t  TYPE cond_konpdb_t,
*      la_cond_vkondat_t TYPE cond_vkondat_t,
*      la_cond_vakevb_t  TYPE cond_vakevb_t,
*      ls_cond_konpdb    TYPE /gda/sdm_s_cond_konpdb,
*      lt_cond_konpdb    TYPE /gda/sdm_t_cond_konpdb,
*      la_syst           TYPE syst.
*
*FIELD-SYMBOLS:
*  <sdm_results> TYPE  /gda/sdm_t_val_results,
*  <sdm_result>  TYPE  /gda/sdm_s_val_results.
*
*
*IF lo_condition IS INITIAL.
*  lo_condition ?= /gda/sdm_cl_core=>factory( iv_object_type = 'CONDITION' iv_source = '2' iv_type = '1' iv_stats = space ).
*ENDIF.
*
*IF lo_condition->is_active( ) = abap_false.
*  RETURN.
*ENDIF.
*
*la_cond_konhdb_t = xkonh[].
*
*LOOP AT xkonp.
*  MOVE-CORRESPONDING xkonp TO ls_cond_konpdb.
*  APPEND ls_cond_konpdb TO lt_cond_konpdb.
*ENDLOOP.
*
*la_cond_vkondat_t = xkondat[].
*la_cond_vakevb_t = xvake[].
*la_syst = sy.
*
*lo_condition->set_selection( iv_name = 'COND_KONHDB_T'          iv_data = la_cond_konhdb_t ).
*lo_condition->set_selection( iv_name = '/GDA/SDM_T_COND_KONPDB' iv_data = lt_cond_konpdb ).
*lo_condition->set_selection( iv_name = 'COND_VKONDAT_T'         iv_data = la_cond_vkondat_t ).
*lo_condition->set_selection( iv_name = 'COND_VAKEVB_T'          iv_data = la_cond_vakevb_t ).
*lo_condition->set_selection( iv_name = 'SYST'                   iv_data = la_syst ).
*
*lr_data = lo_condition->return_brf_result( ).
*ASSIGN lr_data->* TO <sdm_results>.
*
*IF <sdm_results> IS NOT ASSIGNED.
*  RETURN.
*ENDIF.
*
*SORT <sdm_results> BY number.
*DELETE ADJACENT DUPLICATES FROM <sdm_results>.
*
****// Process Message
*LOOP AT <sdm_results> ASSIGNING <sdm_result> WHERE type CA 'EAX'.
*  IF <sdm_result>-id IS INITIAL.
*    <sdm_result>-id = '/GDA/SDM1'.
*  ENDIF.
*
*  IF <sdm_result>-number IS INITIAL.
*    <sdm_result>-number = '002'.
*  ENDIF.
*
*  MESSAGE ID <sdm_result>-id TYPE
*                <sdm_result>-type NUMBER
*                <sdm_result>-number
*    WITH <sdm_result>-message_v1
*         <sdm_result>-message_v2
*         <sdm_result>-message_v3
*         <sdm_result>-message_v4.
*
*ENDLOOP.
