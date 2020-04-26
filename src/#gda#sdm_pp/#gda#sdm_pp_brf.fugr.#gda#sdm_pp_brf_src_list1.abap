FUNCTION /gda/sdm_pp_brf_src_list1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(X_MATNR) TYPE  MATNR
*"  EXPORTING
*"     REFERENCE(Y_RESULT) TYPE  /GDA/SDM_DE_RESULT
*"----------------------------------------------------------------------

  DATA:
    lv_bdatu TYPE eord-bdatu, "Valid To
    lv_vdatu TYPE eord-vdatu. "Valid From

  SELECT SINGLE bdatu vdatu
         FROM eord
         INTO ( lv_bdatu,lv_vdatu )
         WHERE matnr = x_matnr.

  IF sy-subrc = 0.
    IF ( lv_vdatu <= sy-datum AND lv_bdatu >= sy-datum ).
* Valid Record
      y_result = '0'.
    ELSE.
      y_result = 'W'.
    ENDIF.
  ELSE.
* No Record
    y_result = 'E'.
  ENDIF.

ENDFUNCTION.
