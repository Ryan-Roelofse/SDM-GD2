FUNCTION /GDA/SDM_MM_MARA_GET_OLD .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(X_MAX_ROWS) TYPE  P_DBACC
*"     REFERENCE(XT_MATERIALS) TYPE  MATNR_TTY OPTIONAL
*"  EXPORTING
*"     REFERENCE(XT_MARA) TYPE  /GDA/SDM_T_MARA
*"  TABLES
*"      XT_MATNR STRUCTURE  MAT_RANGE OPTIONAL
*"      XT_ERSDA STRUCTURE  RANGE_S_DATS OPTIONAL
*"      XT_ERNAM STRUCTURE  ERNAM_RAN OPTIONAL
*"      XT_LAEDA STRUCTURE  RANGE_S_DATS OPTIONAL
*"      XT_AENAM STRUCTURE  ERNAM_RAN OPTIONAL
*"      XT_MTART STRUCTURE  WART_SR_MTART OPTIONAL
*"      XT_MATKL STRUCTURE  RDM_SR_MATKL OPTIONAL
*"      XT_MSTAE STRUCTURE  /GDA/SDM_S_MSTAE OPTIONAL
*"      XT_BWSCL STRUCTURE  /GDA/SDM_S_BWSCL OPTIONAL
*"      XT_ATTYP STRUCTURE  WART_SR_ATTYP OPTIONAL
*"      XT_WERKS STRUCTURE  WERKS_RANG OPTIONAL
*"--------------------------------------------------------------------

*  PERFORM build_field_selection USING '/GDA/SDM_S_MARA'
*                                      'MARA~'
*                            CHANGING gt_fields.
*
*
*  IF xt_materials IS INITIAL.
*    SELECT DISTINCT (gt_fields) INTO TABLE @xt_mara
*             FROM mara
*             INNER JOIN marc ON ( mara~matnr = marc~matnr )
*               UP TO @x_max_rows ROWS
*             WHERE mara~matnr IN @xt_matnr
*               AND mara~ersda IN @xt_ersda
*               AND mara~ernam IN @xt_ernam
*               AND mara~laeda IN @xt_laeda
*               AND mara~aenam IN @xt_aenam
*               AND mara~mtart IN @xt_mtart
*               AND mara~matkl IN @xt_matkl
*               AND mara~mstae IN @xt_mstae
*               AND mara~bwscl IN @xt_bwscl
*               AND mara~attyp IN @xt_attyp
*               AND mara~attyp <> ''  "Retail Materials only!
*               AND marc~werks IN @xt_werks.
*
*  ELSE.
*
*    gt_materials[] = xt_materials[].
*
*    SELECT (gt_fields) FROM mara
*             INNER JOIN marc ON mara~matnr = marc~matnr
*             INTO TABLE @xt_mara
*             FOR ALL ENTRIES IN @gt_materials
*             WHERE mara~matnr = @gt_materials-matnr
*               AND mara~matnr IN @xt_matnr
*               AND mara~ersda IN @xt_ersda
*               AND mara~ernam IN @xt_ernam
*               AND mara~laeda IN @xt_laeda
*               AND mara~aenam IN @xt_aenam
*               AND mara~mtart IN @xt_mtart
*               AND mara~matkl IN @xt_matkl
*               AND mara~mstae IN @xt_mstae
*               AND mara~bwscl IN @xt_bwscl
*               AND mara~attyp IN @xt_attyp
*               AND mara~attyp <> ''  "Retail Materials only!
*               AND marc~werks IN @xt_werks.
*  ENDIF.
ENDFUNCTION.
