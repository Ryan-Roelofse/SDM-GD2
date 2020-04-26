FUNCTION /gda/sdm_pp_brf_tariff1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(X_MATNR) TYPE  MATNR
*"     REFERENCE(XT_MVKE) TYPE  /GDA/SDM_T_MVKE OPTIONAL
*"  EXPORTING
*"     REFERENCE(Y_RESULT) TYPE  /GDA/SDM_TT_TARIFFS
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_vkorg.
  TYPES: vkorg TYPE vkorg.
  TYPES: END OF ty_vkorg.

  TYPES: BEGIN OF ty_bukrs.
  TYPES: bukrs TYPE bukrs.
  TYPES: END OF ty_bukrs.

  TYPES: BEGIN OF ty_land1.
  TYPES: land1 TYPE land1.
  TYPES: END OF ty_land1.

  DATA:
    lv_tabname1  TYPE tabname1,
    lt_mvke      TYPE /gda/sdm_t_mvke,
    lt_sales_org TYPE STANDARD TABLE OF ty_vkorg,
    ls_sales_org TYPE ty_vkorg,
    ls_results   TYPE /gda/sdm_st_tariffs,
    lt_comp_code TYPE STANDARD TABLE OF ty_bukrs,
    lt_country   TYPE STANDARD TABLE OF ty_land1,
    lo_ref       TYPE REF TO data,
    lx_root      TYPE REF TO cx_root,
    lv_message   TYPE string,
    lv_text(255),
    lv_found.

  FIELD-SYMBOLS:
    <mvke>         LIKE LINE OF xt_mvke,
    <country>      LIKE LINE OF lt_country,
    <tariff_table> TYPE STANDARD TABLE,
    <tariff>       TYPE any,
    <stcts>        TYPE any,
    <ccngn>        TYPE any,
    <datbi>        TYPE any,
    <datab>        TYPE any,
    <matnr>        TYPE any.
*1)
* Get all sales ordes in MVKE for a material
  IF xt_mvke IS INITIAL.
    SELECT vkorg FROM mvke INTO CORRESPONDING FIELDS OF TABLE lt_mvke
             WHERE matnr = x_matnr.
  ELSE.
    lt_mvke[] = xt_mvke[].
  ENDIF.

  LOOP AT lt_mvke ASSIGNING <mvke>.
    ls_sales_org-vkorg = <mvke>-vkorg.
    COLLECT ls_sales_org INTO lt_sales_org.
    CLEAR:
      ls_sales_org.
  ENDLOOP.

  SORT lt_sales_org BY vkorg.
  DELETE ADJACENT DUPLICATES FROM lt_sales_org.

*2)
* Get company codes for these sales orders..TVKO
  SELECT bukrs FROM tvko INTO CORRESPONDING FIELDS OF TABLE lt_comp_code
       FOR ALL ENTRIES IN lt_sales_org
      WHERE vkorg  = lt_sales_org-vkorg.

*3)
* Get country for the company code.. T001
  SELECT land1 FROM t001 INTO CORRESPONDING FIELDS OF TABLE lt_country
       FOR ALL ENTRIES IN lt_comp_code
      WHERE  bukrs  = lt_comp_code-bukrs.

*4)
* /SAPSLL/MARITC -MATNR
* /SAPSLL/MARITC-STCTS(ZA01 for country ZA)

  IF sy-dbsys = 'HDB'.
    lv_tabname1 = '/SAPSLL/MARITC'.
  ELSE.
    lv_tabname1 = '/GDA/SDM_MARITC'.
  ENDIF.

  CREATE DATA lo_ref TYPE TABLE OF (lv_tabname1).
  ASSIGN lo_ref->* TO <tariff_table>.

  TRY.
      SELECT matnr
             stcts
             ccngn
             datab
             datbi FROM (lv_tabname1)
               INTO CORRESPONDING FIELDS OF TABLE <tariff_table>
               WHERE matnr = x_matnr.
    CATCH cx_sy_dynamic_osql_syntax    INTO lx_root.
    CATCH cx_sy_dynamic_osql_semantics INTO lx_root.
  ENDTRY.

  IF lx_root IS NOT INITIAL.
    lv_message = lx_root->get_text( ).
    MESSAGE lv_message TYPE 'I'.
  ENDIF.

  LOOP AT lt_country ASSIGNING <country>.
    ls_results-land1 = <country>-land1.
    LOOP AT <tariff_table> ASSIGNING <tariff>.
      ASSIGN COMPONENT 'STCTS' OF STRUCTURE <tariff> TO <stcts>.
      IF <country> = <stcts>+0(2).
        ASSIGN COMPONENT 'CCNGN' OF STRUCTURE <tariff> TO <ccngn>.
        ASSIGN COMPONENT 'DATBI' OF STRUCTURE <tariff> TO <datbi>.
        ASSIGN COMPONENT 'DATAB' OF STRUCTURE <tariff> TO <datab>.
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <tariff> TO <matnr>.

        IF <datbi> = '99991231'.
          ls_results-id    = '0'.
        ELSE.
          ls_results-id    = 'W'.
        ENDIF.
        lv_found         = abap_true.
        ls_results-ccngn = <ccngn>.
        ls_results-datbi = <datbi>.
        ls_results-datab = <datab>.
        ls_results-matnr = <matnr>.

* Get text..
        IF sy-dbsys = 'HDB'.
          lv_tabname1 = '/SAPSLL/CLSNRT'.
        ELSE.
          lv_tabname1 = '/GDA/SDM_CLSNRT'.
        ENDIF.

        TRY.
            SELECT SINGLE text FROM (lv_tabname1)
                     INTO lv_text
                     WHERE nosct  = <stcts>
                       AND ccngn  = <ccngn>
                       AND datab <= <datab>
                       AND datbi >= <datbi>
                       AND langu  = sy-langu.

            IF sy-subrc = 0.
              ls_results-text = lv_text.
            ENDIF.
          CATCH cx_sy_dynamic_osql_syntax    INTO lx_root.
          CATCH cx_sy_dynamic_osql_semantics INTO lx_root.
        ENDTRY.

        IF lx_root IS NOT INITIAL.
          lv_message = lx_root->get_text( ).
          MESSAGE lv_message TYPE 'I'.
        ENDIF.
      ELSE.
        lv_found         = abap_false.
      ENDIF.
    ENDLOOP.
    IF lv_found = abap_false AND ls_results-id = space.
      ls_results-id    = 'E'.
      ls_results-matnr = x_matnr.
    ENDIF.
    APPEND ls_results TO y_result.
    CLEAR:
     ls_results,
     lv_found,
     lv_text.
  ENDLOOP.
ENDFUNCTION.
