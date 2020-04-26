FUNCTION /gda/sdm_pp_brf_src_list2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(X_MATNR) TYPE  MATNR
*"  EXPORTING
*"     REFERENCE(Y_RESULT) TYPE  /GDA/SDM_TT_SRCLIST
*"  TABLES
*"      XT_EORD STRUCTURE  EORD OPTIONAL
*"      XT_RMMW1 STRUCTURE  RMMW1 OPTIONAL
*"----------------------------------------------------------------------
  CONSTANTS:
    lc_store TYPE t001w-vlfkz VALUE 'A',
    lc_dc    TYPE t001w-vlfkz VALUE 'B'.

  DATA:
    lv_bdatu TYPE eord-bdatu, "Valid To
    lv_vdatu TYPE eord-vdatu, "Valid From
    ls_src   TYPE /gda/sdm_st_srclist,
    lt_eord  TYPE STANDARD TABLE OF eord,
    lt_t001w TYPE STANDARD TABLE OF t001w,
    lv_subrc TYPE sy-subrc,
    lv_dc.

  FIELD-SYMBOLS:
    <eord> LIKE LINE OF lt_eord.

* Populate field ID
*1) Nothing is found in EORD - Return E
*2) Valid record ( DC or Store ) - dates need to be valid - Return 0
*3) Valid record ( DC or Store ) - dates need to be valid - Return W

* Populate field CHECK
*1) If no DC is found CHECK = 'X'
*2) if DC is found    CHECK = SPACE
  IF xt_eord IS SUPPLIED.
    lt_eord[] = xt_eord[].
    IF lt_eord[] IS INITIAL.
      lv_subrc = 4.
    ENDIF.
  ELSE.
    SELECT bdatu vdatu zeord werks
           FROM eord
           INTO CORRESPONDING FIELDS OF TABLE lt_eord
           WHERE matnr = x_matnr.
    lv_subrc = sy-subrc.
  ENDIF.
  IF lv_subrc <> 0.
* No Record
    ls_src-id    = 'E'.
    ls_src-check = abap_true.
  ELSE.
* Check validity for DC or Store? Field ID
    LOOP AT lt_eord ASSIGNING <eord>.
* Is there at least one valid plant?
      IF ( <eord>-vdatu <= sy-datum AND <eord>-bdatu >= sy-datum ).
* Valid Record
        ls_src-id    = '0'.
        ls_src-zeord = <eord>-zeord.
        ls_src-werks = <eord>-werks.
        EXIT.
      ELSE.
* Invalid Record
        ls_src-id = 'W'.
        ls_src-zeord = <eord>-zeord.
        ls_src-werks = <eord>-werks.
      ENDIF.
    ENDLOOP.

    IF xt_rmmw1 IS SUPPLIED.
* Is there at least one DC?
      LOOP AT lt_eord ASSIGNING <eord>.
        READ TABLE xt_rmmw1 WITH KEY vzwrk = <eord>-werks TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
* DC Found
          lv_dc =  abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_dc = abap_true.
        lv_subrc = 0.
      ELSE.
        lv_subrc = 4.
      ENDIF.
    ELSE.
* Is there at least one DC?
      SELECT * FROM t001w INTO TABLE lt_t001w
                          FOR ALL ENTRIES IN lt_eord
                        WHERE werks = lt_eord-werks
                          AND vlfkz = lc_dc.
      lv_subrc = sy-subrc.
    ENDIF.

    IF lv_subrc <> 0.
      ls_src-check = abap_true.
      ls_src-vlfkz = lc_store.
    ELSE.
      ls_src-check = abap_false.
      ls_src-vlfkz = lc_dc.
    ENDIF.
  ENDIF.
  APPEND ls_src TO y_result.

ENDFUNCTION.
