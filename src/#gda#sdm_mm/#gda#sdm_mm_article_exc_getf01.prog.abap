*----------------------------------------------------------------------*
***INCLUDE /GDA/SDM_MM_ARTICLE_EXC_GETF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FOR_BRF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_for_brf .
  SELECT *
*          UP TO me->ms_selscreen-max_rows ROWS
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE gt_mara
  WHERE matnr IN s_matnr
    AND ersda IN s_ersda
    AND mtart IN s_mtart
    AND matkl IN s_matkl
    AND lvorm IN s_lvorm
    AND attyp IN s_attyp
    AND attyp <> ''.  "Retail Materials only!


  IF p_makt = abap_true.
*/ MAKT
    SELECT *
      FROM makt
      INTO TABLE gt_makt
      FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
      AND spras = sy-langu.

*/ MAMT
    SELECT *
      FROM mamt
      INTO TABLE gt_mamt
      FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
      AND spras = sy-langu.
  ENDIF.

  IF p_marc_d = abap_true AND p_marc_s = abap_true.
* MARC
    SELECT *
      FROM marc
      INTO CORRESPONDING FIELDS OF TABLE gt_marc
      FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
    AND ( werks IN s_werksd
     OR   werks IN s_werkss )
      AND lvorm IN s_lvorm.
  ENDIF.

  IF p_marc_d = abap_true AND p_marc_s = abap_true.   "MARD goes together with MARC for Retail
* MARD
    IF gt_marc IS NOT INITIAL.
      SELECT *
        FROM mard
        INTO CORRESPONDING FIELDS OF TABLE gt_mard
       FOR ALL ENTRIES IN gt_mara
      WHERE matnr = gt_mara-matnr
        AND werks IN s_werksd
        AND lgort IN s_lvowk
        AND lvorm IN s_lvorm.
    ENDIF.
  ENDIF.

  IF p_mvke = abap_true.
*MVKE
    SELECT *
      FROM mvke
      INTO CORRESPONDING FIELDS OF TABLE gt_mvke
     FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
      AND vkorg IN s_vkorg
      AND vtweg IN s_vtweg
      AND lvorm IN s_lvorm.
  ENDIF.

  IF p_mbew = abap_true.
*MBEW
    SELECT *
      FROM mbew
      INTO CORRESPONDING FIELDS OF TABLE gt_mbew
     FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
*          AND BWKEY IN S_BWKEY
*          AND BWTAR IN S_BWTAR
      AND lvorm IN s_lvorm.
  ENDIF.

  IF p_mlgn = abap_true.
* MLGN
    SELECT *
      FROM mlgn
      INTO CORRESPONDING FIELDS OF TABLE gt_mlgn
     FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
*    AND LGNUM IN S_LGNUM
      AND lvorm IN s_lvorm.
  ENDIF.

  IF p_mlgt = abap_true.
* MLGT
    SELECT *
      FROM mlgt
      INTO CORRESPONDING FIELDS OF TABLE gt_mlgt
     FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
*      AND lgnum IN me->ms_selscreen-lgnum
*      AND lgtyp IN me->ms_selscreen-lgtyp
      AND lvorm IN s_lvorm.
  ENDIF.

  IF p_mapr = abap_true.
*/ MAPR (Forecasting) Data
    SELECT *
      FROM mapr
      INTO TABLE gt_mapr
      FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
      AND ( werks IN s_werksd
       OR werks IN s_werksd ).
  ENDIF.

  IF p_crvm = abap_true.
*/ PRT (Production Resource Tool) Data
    SELECT *
      FROM crvm_b
      INTO TABLE gt_crvm
      FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr
      AND ( werks IN s_werksd
       OR werks IN s_werksd ).

  ENDIF.

  IF p_marm = abap_true.
* MARM
    SELECT *
        FROM marm
        INTO CORRESPONDING FIELDS OF TABLE gt_marm
        FOR ALL ENTRIES IN gt_mara
      WHERE matnr = gt_mara-matnr.
  ENDIF.

  IF p_mean = abap_true.

*/ MEAN
    SELECT *
      FROM mean
      INTO TABLE gt_mean
      FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr.

  ENDIF.

  IF p_eina = abap_true.

*EINE and EINA
    SELECT *
     FROM eina
     INTO TABLE gt_eina
     FOR ALL ENTRIES IN gt_mara
   WHERE matnr = gt_mara-matnr
     AND lifnr IN s_lifnr
     AND ltsnr IN s_ltsnr
     AND loekz = space.

    IF NOT gt_eina IS INITIAL.
      SELECT *
        FROM eine
        INTO TABLE gt_eine
        FOR ALL ENTRIES IN gt_eina
       WHERE infnr = gt_eina-infnr
         AND ekorg IN s_ekorg
         AND loekz = space.
    ENDIF.
  ENDIF.

  IF p_wlk2 = abap_true.
* WLK2
    SELECT *
     FROM wlk2
     INTO TABLE gt_wlk2
     FOR ALL ENTRIES IN gt_mara
   WHERE matnr = gt_mara-matnr
     AND vkorg IN s_vkorg
     AND vtweg IN s_vtweg.

  ENDIF.

  IF p_maw1 = abap_true.
* MAW1
    SELECT *
     FROM maw1
     INTO TABLE gt_maw1
     FOR ALL ENTRIES IN gt_mara
   WHERE matnr = gt_mara-matnr.
  ENDIF.

  IF p_malg = abap_true.
* MALG
    SELECT *
     FROM malg
     INTO TABLE gt_malg
     FOR ALL ENTRIES IN gt_mara
    WHERE matnr = gt_mara-matnr.
  ENDIF.

  IF p_mlea = abap_true.
    SELECT *
     FROM mlea
     INTO TABLE gt_mlea
     FOR ALL ENTRIES IN gt_mara
   WHERE matnr = gt_mara-matnr.
  ENDIF.

  IF p_myms = abap_true.
    SELECT *
     FROM myms
     INTO TABLE gt_myms
     FOR ALL ENTRIES IN gt_mara
   WHERE matnr = gt_mara-matnr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prep_data .
* MAKT
  LOOP AT gt_makt ASSIGNING <makt> WHERE matnr = <mara>-matnr.
    APPEND <makt> TO gt_makt_temp.
  ENDLOOP.

  READ TABLE gt_makt
    INTO gs_makt_temp
    WITH KEY mandt = sy-mandt
             matnr = <mara>-matnr
             spras = sy-langu.
  IF sy-subrc = 0.
    APPEND INITIAL LINE TO gt_ktex ASSIGNING <ttext>.
    <ttext>-maktx = gs_makt_temp-maktx.
    <ttext>-spras = sy-langu.
  ENDIF.


  DATA:
    lv_id     TYPE thead-tdid,
    lv_name   TYPE thead-tdname,
    lv_object TYPE thead-tdobject.

  lv_id     = 'GRUN'.
  lv_name   = |{ <mara>-matnr }|.
  lv_object = 'MATERIAL'.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      id                      = lv_id
      language                = sy-langu
      name                    = lv_name
      object                  = lv_object
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*          IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = gt_basic_text
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.


* MARC
  LOOP AT gt_marc ASSIGNING <marc> WHERE matnr = <mara>-matnr.
    APPEND <marc> TO gt_marc_temp.
  ENDLOOP.

* MARD
  LOOP AT gt_mard ASSIGNING <mard> WHERE matnr = <mara>-matnr.
    APPEND <mard> TO gt_mard_temp.
  ENDLOOP.

* MVKE
  LOOP AT gt_mvke ASSIGNING <mvke> WHERE matnr = <mara>-matnr.
    APPEND <mvke> TO gt_mvke_temp.
  ENDLOOP.

* MBEW
  LOOP AT gt_mbew ASSIGNING <mbew> WHERE matnr = <mara>-matnr.
    APPEND <mbew> TO gt_mbew_temp.
  ENDLOOP.

* MLGN
  LOOP AT gt_mlgn ASSIGNING <mlgn> WHERE matnr = <mara>-matnr.
    APPEND <mlgn> TO gt_mlgn_temp.
  ENDLOOP.

* MLGT
  LOOP AT gt_mlgt ASSIGNING <mlgt> WHERE matnr = <mara>-matnr.
    APPEND <mlgt> TO gt_mlgt_temp.
  ENDLOOP.

* MARM
  LOOP AT gt_marm ASSIGNING <marm> WHERE matnr = <mara>-matnr.
    MOVE-CORRESPONDING <marm> TO ls_meinh.
    APPEND ls_meinh TO gt_meinh_temp.
  ENDLOOP.

* MAMT
  LOOP AT gt_mamt ASSIGNING <mamt> WHERE matnr = <mara>-matnr.
    APPEND <mamt> TO gt_mamt_temp.
  ENDLOOP.

* MALG
  LOOP AT gt_malg ASSIGNING <malg> WHERE matnr = <mara>-matnr.
    APPEND <malg> TO gt_malg_temp.
  ENDLOOP.



*/ MAPR (Forecasting)
  IF p_mapr = abap_true.
    READ TABLE gt_mapr TRANSPORTING NO FIELDS
      WITH KEY matnr = <mara>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_mapr ASSIGNING <mapr> FROM sy-tabix.
        IF <mapr>-matnr <> <mara>-matnr.
          EXIT.
        ELSE.
*/ Read with FM


          CALL FUNCTION 'MPOP_SINGLE_READ'
            EXPORTING
*             kzrfb      = SPACE    " Ind.: Refresh buffer entry for material no.
              matnr      = <mapr>-matnr
*             maxtz      =     " Max. No. of Entries in Buffer
              werks      = <mapr>-werks
            IMPORTING
*             wmpop      =     " Work area for MPOP
              o_mpop     = ls_mpop
*        TABLES
*             prowf_tab  =     " Table of forecast values (w/o key)
            EXCEPTIONS
              not_found  = 1
              wrong_call = 2
              OTHERS     = 3.
          IF sy-subrc = 0.
            INSERT ls_mpop INTO TABLE gt_mpop.
            CLEAR ls_mpop.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*/ MEAN (Additional EANs)
  IF p_mean = abap_true.
    READ TABLE gt_mean TRANSPORTING NO FIELDS
     WITH KEY mandt = sy-mandt matnr = <mara>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_mean ASSIGNING <mean> FROM sy-tabix.
        IF <mean>-matnr <> <mara>-matnr.
          EXIT.
        ELSE.
          APPEND <mean> TO gt_mean_temp.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*/ EINA (Purchase Inforecords)
  IF p_eina = abap_true.
    READ TABLE gt_eina TRANSPORTING NO FIELDS
     WITH KEY mandt = sy-mandt matnr = <mara>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_eina ASSIGNING <eina> FROM sy-tabix.
        IF <eina>-matnr <> <mara>-matnr.
          EXIT.
        ELSE.
          APPEND <eina> TO gt_eina_temp.
*/ EINE
          LOOP AT gt_eine ASSIGNING <eine>
            WHERE mandt = sy-mandt AND infnr = <eina>-infnr.
            APPEND <eine> TO gt_eine_temp.
          ENDLOOP.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*/ MAW1 (Material Master: Default Fields and Special Retail Fields, Listing)
  IF p_maw1 = abap_true.
    READ TABLE gt_maw1 TRANSPORTING NO FIELDS
     WITH KEY mandt = sy-mandt matnr = <mara>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_maw1 ASSIGNING <maw1> FROM sy-tabix.
        IF <maw1>-matnr <> <mara>-matnr.
          EXIT.
        ELSE.
          APPEND <maw1> TO gt_maw1_temp.
*/ Build MWLI from MAW1
          MOVE-CORRESPONDING <maw1> TO ls_mwli.
          APPEND ls_mwli TO gt_mwli_temp.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*/ MYMS (LIFO-relevant materials)
  IF p_myms = abap_true.
    READ TABLE gt_myms TRANSPORTING NO FIELDS
     WITH KEY mandt = sy-mandt matnr = <mara>-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_myms ASSIGNING <myms> FROM sy-tabix.
        IF <myms>-matnr <> <mara>-matnr.
          EXIT.
        ELSE.
          APPEND <myms> TO gt_myms_temp.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.

FORM set_data .
  go_article->set_selection( iv_name = '/GDA/SDM_S_MARA' iv_data = <mara> ).
  go_article->set_selection( iv_name = '/GDA/SDM_T_MARC' iv_data = gt_marc_temp ).
  go_article->set_selection( iv_name = '/GDA/SDM_T_MARD' iv_data = gt_mard_temp ).
  go_article->set_selection( iv_name = '/GDA/SDM_T_MBEW' iv_data = gt_mbew_temp ).
  go_article->set_selection( iv_name = '/GDA/SDM_T_MLGN' iv_data = gt_mlgn_temp ).
  go_article->set_selection( iv_name = '/GDA/SDM_T_MLGT' iv_data = gt_mlgt_temp ).
  go_article->set_selection( iv_name = '/GDA/SDM_S_MVKE' iv_data = gt_mvke_temp ).
  go_article->set_selection( iv_name = '/GDA/SDM_T_MYMS' iv_data = gt_myms_temp ).
  go_article->set_selection( iv_name = '/GDA/SDM_T_MAW1' iv_data = gt_maw1_temp ).
  go_article->set_selection( iv_name = '/GDA/SDM_T_MWLI' iv_data = gt_mwli_temp ).
  go_article->set_selection( iv_name = 'MAT_MEINH'       iv_data = gt_meinh_temp ).
  go_article->set_selection( iv_name = 'MAMT_TTY'        iv_data = gt_mamt_temp ).
  go_article->set_selection( iv_name = 'MALG_TTY'        iv_data = gt_malg_temp ).
  go_article->set_selection( iv_name = 'MAT_KTEXT'       iv_data = gt_ktex ).
  go_article->set_selection( iv_name = 'SWDTLINET'       iv_data = gt_basic_text ).
  go_article->set_selection( iv_name = 'MEAN_TAB'        iv_data = gt_mean_temp ).
  go_article->set_selection( iv_name = 'MMPR_EINA'       iv_data = gt_eina_temp ).
  go_article->set_selection( iv_name = 'MMPR_EINE'       iv_data = gt_eine_temp ).
  go_article->set_selection( iv_name = 'MYMS'            iv_data = gt_myms_temp ).
ENDFORM.                    " SET_DATA
