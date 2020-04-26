FUNCTION /gda/sdm_pp_brf_pricing1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(X_MATNR) TYPE  MATNR
*"  EXPORTING
*"     REFERENCE(Y_RESULT) TYPE  /GDA/SDM_ST_PRICING
*"----------------------------------------------------------------------
*https://blogs.sap.com/2018/08/28/reading-price-conditions-dynamically-from-condition-tables/


  DATA:
    ob_rec    TYPE REF TO data,
    lt_result TYPE /gda/sdm_t_pricing,
    ls_struc  TYPE struc1.
*    lt_struc                 TYPE STANDARD TABLE OF struc1.
*    lv_app_id                TYPE fdt_admn_0000-id,
*    lv_id2                   TYPE fdt_admn_0000-id,
*    lt_table_data_converted1 TYPE if_fdt_decision_table=>ts_table_data,
*    lt_table_data_converted2 TYPE if_fdt_decision_table=>ts_table_data.


  FIELD-SYMBOLS:
    <table>          TYPE any,
    <condition>      TYPE any,
    <kschl>          TYPE any,
    <ls_rec>         TYPE any,
    <datbi>          TYPE any,
    <datab>          TYPE any,
    <knumh>          TYPE any,
    <table_data>     LIKE LINE OF gt_table_data_converted1,
    <condition_data> LIKE LINE OF gt_table_data_converted2.


  IF gv_init = abap_false.
    SELECT SINGLE active FROM /gda/sdm_setup6 INTO gv_is_active
        WHERE sdm_object  = 'ARTICLE'
          AND ( type        = '04' OR type = '41')
          AND  active     = abap_true.

*    IF sy-subrc <> 0.
*      SELECT SINGLE active FROM /gda/sdm_setup6 INTO gv_is_active
*          WHERE sdm_object  = 'ARTICLE'
*            AND type        = '41'
*            AND  active     = abap_true.
*
*    ENDIF.
    gv_init = abap_true.
  ENDIF.


  IF gv_app_id IS INITIAL AND gv_is_active = abap_true.
    SELECT SINGLE id FROM fdt_admn_0000 INTO gv_app_id
                     WHERE name        = '/GDA/SDM_MM_ARTICLE_MASTER'
                       AND object_type = 'AP'.
    SELECT SINGLE id FROM fdt_admn_0000 INTO gv_id2
                     WHERE name           = 'DT_POST_PRICING_CONDITION_TYPE'
                       AND object_type    = 'EX'
                       AND application_id = gv_app_id.
  ENDIF.
  CHECK sy-subrc = 0 AND gv_id2 IS NOT INITIAL.

  IF gt_struc IS INITIAL.
    TRY.
        CALL METHOD cl_fdt_simplifier=>simplify_decision_table
          EXPORTING
            iv_decision_table_id     = gv_id2
          IMPORTING
            ets_table_data_converted = gt_table_data_converted2.
      CATCH cx_fdt_input .
    ENDTRY.

    gt_table_data_converted1[] = gt_table_data_converted2[].

    LOOP AT gt_table_data_converted2 ASSIGNING <condition_data> WHERE col_no = '1'.
      ASSIGN COMPONENT 'R_VALUE' OF STRUCTURE <condition_data> TO <kschl>.
      ASSIGN <kschl>->* TO <kschl>.
      ls_struc-kschl = <kschl>.
      LOOP AT gt_table_data_converted1 ASSIGNING <table_data> WHERE col_no = '2'
                                                               AND  row_no = <condition_data>-row_no.

        ASSIGN COMPONENT 'R_VALUE' OF STRUCTURE <table_data> TO <table>.
        ASSIGN <table>->* TO <table>.
        ls_struc-tabname = <table>.
        APPEND ls_struc TO gt_struc.
        CLEAR ls_struc.

      ENDLOOP.
    ENDLOOP.
  ENDIF.
* Build details from column 1 - Condition Type


* First check for any SUCCESS Records
  LOOP AT gt_struc INTO ls_struc.

    CREATE DATA ob_rec TYPE (ls_struc-tabname).
    ASSIGN ob_rec->* TO <ls_rec>.

    SELECT SINGLE * INTO <ls_rec>
             FROM (ls_struc-tabname)
             WHERE matnr = x_matnr
               AND kschl = ls_struc-kschl
               AND datbi >= sy-datum
               AND datab <= sy-datum.

    IF sy-subrc = 0.
*      ASSIGN COMPONENT 'DATBI' OF STRUCTURE <ls_rec> TO <datbi>.
*      ASSIGN COMPONENT 'DATAB' OF STRUCTURE <ls_rec> TO <datab>.
      ASSIGN COMPONENT 'KNUMH' OF STRUCTURE <ls_rec> TO <knumh>.
*      IF ( <datab> <= sy-datum AND <datbi> >= sy-datum ).
      y_result-id     = '0'.
      y_result-knumh  =  <knumh>.
      EXIT.
*      ELSE.
*        y_result-id     = 'W'.
*        y_result-knumh  =  <knumh>.

*      ENDIF.
*    ELSE.
*      y_result-id     = 'E'.
*      y_result-knumh  =  ''.
    ENDIF.
  ENDLOOP.

  CHECK y_result-id     = space.
* If no SUCCESS then check for WARNING
  LOOP AT gt_struc INTO ls_struc.

    CREATE DATA ob_rec TYPE (ls_struc-tabname).
    ASSIGN ob_rec->* TO <ls_rec>.

    SELECT SINGLE * INTO <ls_rec>
             FROM (ls_struc-tabname)
             WHERE matnr = x_matnr
               AND kschl = ls_struc-kschl.

    IF sy-subrc = 0.
*      ASSIGN COMPONENT 'DATBI' OF STRUCTURE <ls_rec> TO <datbi>.
*      ASSIGN COMPONENT 'DATAB' OF STRUCTURE <ls_rec> TO <datab>.
      ASSIGN COMPONENT 'KNUMH' OF STRUCTURE <ls_rec> TO <knumh>.
*      IF ( <datab> <= sy-datum AND <datbi> >= sy-datum ).
*        y_result-id     = '0'.
*        y_result-knumh  =  <knumh>.
*        EXIT.
*      ELSE.
        y_result-id     = 'W'.
        y_result-knumh  =  <knumh>.
        EXIT.
*      ENDIF.
*    ELSE.
*      y_result-id     = 'E'.
*      y_result-knumh  =  ''.
    ENDIF.
  ENDLOOP.

  CHECK y_result-id     = space.
* No SUCCESS or WARNING therefore set to ERROR
  y_result-id = 'E'.
ENDFUNCTION.
