*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
.























































































































DEFINE _verify.
  DATA:
    it_sdm_lic TYPE TABLE OF /gda/sdm_lic,
    lv_sdm_lic LIKE LINE OF it_sdm_lic,
    lv_dat     TYPE dats.

  IF &1 = space OR &2 = space.
    ev_result = abap_false.
    ev_message = TEXT-001.
    RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
  ENDIF.

  SELECT *
        INTO TABLE it_sdm_lic
        FROM (mv_check)
        WHERE sdm_object_id = &1
        AND   sdm_module    = &2.

  IF sy-subrc <> 0.
    ev_result = abap_false.
    ev_message = TEXT-002.
    RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
  ENDIF.

  READ TABLE it_sdm_lic INDEX 1 INTO lv_sdm_lic.
  lv_dat = sy-datum.
  lv_dat = lv_dat - 90.     " we should consider to parameterise this grace period.

*  IF lv_sdm_lic-status <> abap_true.
*    ev_result = abap_false.
*    ev_message = TEXT-003.
*    RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
*  ENDIF.

  IF lv_sdm_lic-start_date > sy-datum.
*    lv_sdm_lic-status = space.
    UPDATE (mv_check) FROM lv_sdm_lic.

    ev_result = abap_false.
    ev_message = TEXT-004.
    RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
  ENDIF.

  IF lv_sdm_lic-end_date < lv_dat.
*    lv_sdm_lic-status = space.
    UPDATE (mv_check) FROM lv_sdm_lic.

    ev_result = abap_false.
    ev_message = TEXT-004.
    RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
  ENDIF.

  ev_result = abap_true.
  RETURN.

END-OF-DEFINITION.

DEFINE _retrieve.
  SELECT *
        INTO TABLE rt_result
        FROM (mv_check).

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl.
  ENDIF.
END-OF-DEFINITION.
