
*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_MATERIAL_LCL
*&---------------------------------------------------------------------*
CLASS lcl_mm_material_exc DEFINITION FINAL." CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_selscreen,
             matnr             TYPE RANGE OF mara-matnr,
             ersda             TYPE RANGE OF mara-ersda,
             mtart             TYPE RANGE OF mara-mtart,
             prdha             TYPE RANGE OF mara-prdha,
             mstae             TYPE RANGE OF mara-mstae,
             matkl             TYPE RANGE OF mara-matkl,
             werks             TYPE RANGE OF marc-werks,
             mmsta             TYPE RANGE OF marc-mmsta,
             lgort             TYPE RANGE OF mard-lgort,
             vkorg             TYPE RANGE OF mvke-vkorg,
             vtweg             TYPE RANGE OF mvke-vtweg,
             bwkey             TYPE RANGE OF mbew-bwkey,
             bwtar             TYPE RANGE OF mbew-bwtar,
             lgnum             TYPE RANGE OF mlgn-lgnum,
             lgtyp             TYPE RANGE OF mlgt-lgtyp,

             mara_lvorm        TYPE RANGE OF mara-lvorm,
             marc_lvorm        TYPE RANGE OF marc-lvorm,
             mard_lvorm        TYPE RANGE OF mard-lvorm,
             mvke_lvorm        TYPE RANGE OF mvke-lvorm,
             mbew_lvorm        TYPE RANGE OF mbew-lvorm,
             mlgn_lvorm        TYPE RANGE OF mlgn-lvorm,
             mlgt_lvorm        TYPE RANGE OF mlgt-lvorm,

             makt              TYPE c LENGTH 1,
             marc              TYPE c LENGTH 1,
             mard              TYPE c LENGTH 1,
             mvke              TYPE c LENGTH 1,
             mbew              TYPE c LENGTH 1,
             mlgn              TYPE c LENGTH 1,
             mlgt              TYPE c LENGTH 1,

             mapr              TYPE c LENGTH 1,
             crvm              TYPE c LENGTH 1,
             mlan              TYPE c LENGTH 1,
             marm              TYPE c LENGTH 1,
             mean              TYPE c LENGTH 1,

             eord              TYPE c LENGTH 1,

             msgno             TYPE RANGE OF bapiret2-number,
             max_rows          TYPE p_dbacc,
             record_statistics TYPE c LENGTH 1,
             errors_only       TYPE c LENGTH 1,
           END OF ty_selscreen.

    TYPES: BEGIN OF ty_report_output,
             matnr          TYPE matnr,
             maktx          TYPE maktx,
             mtart          TYPE mtart,
             matkl          TYPE matkl,
             mstae          TYPE mstae,
             message        TYPE bapi_msg,
             message_id     TYPE symsgid,
             message_number TYPE symsgno,
             message_type   TYPE symsgty,
             count          TYPE i,
             extra_v1       TYPE symsgv,
             extra_v2       TYPE symsgv,
             extra_v3       TYPE symsgv,
             extra_v4       TYPE symsgv,
             extra_v5       TYPE symsgv,
             extra_v6       TYPE symsgv,
           END OF ty_report_output.

    TYPES: ty_it_report_output TYPE STANDARD TABLE OF ty_report_output.

    DATA: ms_mara_spec TYPE /gda/sdm_s_mara,
          ms_makt_spec TYPE /gda/sdm_s_makt.

    DATA: mt_mara          TYPE STANDARD TABLE OF /gda/sdm_s_mara,
          mt_marc_spec     TYPE /gda/sdm_t_marc,
          mt_mard_spec     TYPE /gda/sdm_t_mard,
          mt_mvke_spec     TYPE /gda/sdm_t_mvke,
          mt_mbew_spec     TYPE /gda/sdm_t_mbew,
          mt_mlgn_spec     TYPE /gda/sdm_t_mlgn,
          mt_mlgt_spec     TYPE /gda/sdm_t_mlgt,
          mt_mpop_spec     TYPE /gda/sdm_t_mpop,##NEEDED     "Forecasting - populated from MAPR
          mt_mfhm_spec     TYPE /gda/sdm_t_mfhm,##NEEDED     "PRT Data - populated from CRVM
          mt_meinh_spec    TYPE /gda/sdm_t_meinh,##NEEDED
          mt_steuertab     TYPE mat_steuer,         "Table for Taxes
          mt_steummtab     TYPE mat_steumm,         "Table for Taxes (Purchasing)
          mt_marm_spec     TYPE /gda/sdm_t_marm, "mat_meinh ##NEEDED,          "Units of Measure
          mt_mean_spec     TYPE /gda/sdm_t_mean, "mean_tab,           "EANs
          mt_mlan_spec     TYPE /gda/sdm_t_mlan, "mean_tab,           "EANs


*          mt_eord_spec     TYPE /gda/sdm_t_eord,

          mt_report_output TYPE TABLE OF ty_report_output,
          mv_spec_matnr    TYPE matnr. "Specific Material


    CLASS-METHODS: factory RETURNING VALUE(ro_material) TYPE REF TO lcl_mm_material_exc.

    METHODS constructor.
    METHODS main RAISING /gda/cx_sdm_exception_handl.
*    METHODS check_authorisation IMPORTING iv_actvt             TYPE activ_auth
*                                RETURNING VALUE(rv_authorised) TYPE abap_bool.
    METHODS set_selscreen IMPORTING is_selscreen TYPE ty_selscreen.
*    METHODS return_message RETURNING VALUE(rv_message) TYPE string.
    METHODS get_output_table EXPORTING et_output_table TYPE ty_it_report_output.
*    METHODS record_statistics RAISING zcx_gd_brf_exc_report_util.
    METHODS refresh_spec.
    METHODS build_spec.


  PRIVATE SECTION.
*    CONSTANTS: gc_object_material TYPE /gda/sdm_de_object VALUE 'MATERIAL'.

*    DATA: mo_brf_exc_util TYPE REF TO zgd_brf_exc_report_util.

    DATA: mv_message   TYPE string.
*          mv_spec_matnr TYPE matnr, "Specific Material
*          mv_timestamp TYPE timestamp.

    DATA: ms_selscreen TYPE ty_selscreen.

    DATA:
      mt_field_list TYPE TABLE OF char30,
      mt_makt       TYPE HASHED TABLE OF /gda/sdm_s_makt
                    WITH UNIQUE KEY matnr spras,
      mt_marc       TYPE /gda/sdm_t_marc,
      mt_mard       TYPE /gda/sdm_t_mard,
      mt_mvke       TYPE /gda/sdm_t_mvke,
      mt_mbew       TYPE /gda/sdm_t_mbew,
      mt_mlgn       TYPE /gda/sdm_t_mlgn,
      mt_mlgt       TYPE /gda/sdm_t_mlgt,
      mt_meinh      TYPE /gda/sdm_t_meinh,
      mt_mapr       TYPE SORTED TABLE OF mapr
                    WITH UNIQUE KEY matnr werks,
      mt_crvm       TYPE SORTED TABLE OF crvm_b
                    WITH UNIQUE KEY matnr werks objty objid,
      mt_marm       TYPE SORTED TABLE OF marm
                    WITH UNIQUE KEY mandt matnr meinh,
      mt_mean       TYPE SORTED TABLE OF mean
                    WITH UNIQUE KEY mandt matnr meinh lfnum,
      mt_eord       TYPE SORTED TABLE OF eord
                    WITH UNIQUE KEY mandt matnr werks zeord,
      mt_mpop       TYPE SORTED TABLE OF mpop
                    WITH UNIQUE KEY mandt matnr werks,
      mt_mlan       TYPE SORTED TABLE OF mlan
                    WITH UNIQUE KEY mandt matnr aland.


    METHODS build_field_selection
      IMPORTING
        iv_struct_name TYPE string.

    METHODS build_mara RAISING /gda/cx_sdm_exception_handl.
    METHODS build_makt RAISING /gda/cx_sdm_exception_handl.
    METHODS build_marc RAISING /gda/cx_sdm_exception_handl.
    METHODS build_mard RAISING /gda/cx_sdm_exception_handl.
    METHODS build_mvke RAISING /gda/cx_sdm_exception_handl.
    METHODS build_mbew RAISING /gda/cx_sdm_exception_handl.
    METHODS build_mlgn RAISING /gda/cx_sdm_exception_handl.
    METHODS build_mlgt RAISING /gda/cx_sdm_exception_handl.

    METHODS build_mapr RAISING /gda/cx_sdm_exception_handl.
    METHODS build_crvm RAISING /gda/cx_sdm_exception_handl.
    METHODS build_marm RAISING /gda/cx_sdm_exception_handl.
    METHODS build_mlan RAISING /gda/cx_sdm_exception_handl.
    METHODS build_mean RAISING /gda/cx_sdm_exception_handl.
    METHODS build_eord RAISING /gda/cx_sdm_exception_handl.
    METHODS build_mpop RAISING /gda/cx_sdm_exception_handl.

ENDCLASS.                    "lcl_EXRATE DEFINITION

CLASS lcl_alv_grid DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS display CHANGING it_table  TYPE lcl_mm_material_exc=>ty_it_report_output
                                   iv_layout TYPE slis_vari
                          RAISING  cx_salv_error.

    METHODS handle_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.

  PRIVATE SECTION.
    CLASS-DATA: mt_alv_table         TYPE lcl_mm_material_exc=>ty_it_report_output,
                mo_alv_event_handler TYPE REF TO lcl_alv_grid,
                so_alv_columns       TYPE REF TO cl_salv_columns_table.

    CLASS-METHODS: extra_fields_description,
      set_col_description IMPORTING iv_col_name TYPE char30 iv_description TYPE clike.

ENDCLASS.

************************************************************************
*
*                        CLASS IMPLEMENTATIONS
*
************************************************************************

CLASS lcl_mm_material_exc IMPLEMENTATION.
  METHOD factory.
    CREATE OBJECT ro_material.
  ENDMETHOD.

  METHOD main.
    me->build_mara( ).
    me->build_makt( ).
    me->build_marc( ).
    me->build_mard( ).

    me->build_mvke( ).
    me->build_mpop( ).
    me->build_mbew( ).
    me->build_mlgn( ).
    me->build_mlgt( ).

    me->build_mapr( ).
    me->build_crvm( ).
    me->build_marm( ).
    me->build_mean( ).
    me->build_mlan( ).

  ENDMETHOD.

  METHOD set_selscreen.
    me->ms_selscreen = is_selscreen.
  ENDMETHOD.                    "SET_SELSCREEN

*  METHOD return_message.
*    rv_message = me->mv_message.
*  ENDMETHOD.


  METHOD build_field_selection.

    FIELD-SYMBOLS:
      <ls_components> TYPE LINE OF abap_compdescr_tab,
      <ls_field_list> TYPE char30.

    DATA:
      lo_type_descr   TYPE REF TO cl_abap_typedescr,
      lo_struct_descr TYPE REF TO cl_abap_structdescr.

    FREE me->mt_field_list.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         =  iv_struct_name
      RECEIVING
        p_descr_ref    =  lo_type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2
    ).
    IF sy-subrc = 0.
      lo_struct_descr ?= lo_type_descr.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  INTO me->mv_message
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

**// Build Dynamic Field Selection
    LOOP AT lo_struct_descr->components ASSIGNING <ls_components>.
      IF <ls_components>-name = 'SDM_TABKEY'.
        CONTINUE.
      ENDIF.
      APPEND INITIAL LINE TO me->mt_field_list ASSIGNING <ls_field_list>.
      <ls_field_list> = <ls_components>-name.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_mara.

    DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MARA' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          UP TO me->ms_selscreen-max_rows ROWS
          FROM mara
          INTO CORRESPONDING FIELDS OF TABLE me->mt_mara
        WHERE matnr IN me->ms_selscreen-matnr
          AND prdha IN me->ms_selscreen-prdha
          AND ersda IN me->ms_selscreen-ersda
          AND mtart IN me->ms_selscreen-mtart
          AND matkl IN me->ms_selscreen-matkl
          AND lvorm IN me->ms_selscreen-mara_lvorm
          AND mstae IN me->ms_selscreen-mstae.
      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MARA:| && me->mv_message.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
    ENDTRY.

    IF lines( me->mt_mara ) = 0.
      me->mv_message = TEXT-901. "'No data selected'.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = mv_message.
    ENDIF.

  ENDMETHOD.


  METHOD build_makt.

    IF me->ms_selscreen-makt = abap_false.
      RETURN.
    ENDIF.

*/ MAKT
    SELECT *
      FROM makt
      INTO CORRESPONDING FIELDS OF TABLE me->mt_makt
      FOR ALL ENTRIES IN me->mt_mara
    WHERE matnr = me->mt_mara-matnr
      AND spras = sy-langu.

  ENDMETHOD.


  METHOD build_marc.

    DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    IF me->ms_selscreen-marc = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MARC' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          FROM marc
          INTO TABLE me->mt_marc
          FOR ALL ENTRIES IN me->mt_mara
        WHERE matnr = me->mt_mara-matnr
          AND werks IN me->ms_selscreen-werks
          AND lvorm IN me->ms_selscreen-marc_lvorm
          AND mmsta IN me->ms_selscreen-mmsta.
      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MARC:| && me->mv_message.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
    ENDTRY.

  ENDMETHOD.

  METHOD build_mard.

    DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    IF me->ms_selscreen-mard = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MARD' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          FROM mard
          INTO TABLE me->mt_mard
         FOR ALL ENTRIES IN me->mt_mara
        WHERE matnr = me->mt_mara-matnr
          AND werks IN me->ms_selscreen-werks
          AND lgort IN me->ms_selscreen-lgort
          AND lvorm IN me->ms_selscreen-mard_lvorm.
      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MARD:| && me->mv_message.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
    ENDTRY.

  ENDMETHOD.

  METHOD build_mvke.

    DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    IF me->ms_selscreen-mvke = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MVKE' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          FROM mvke
          INTO TABLE me->mt_mvke
         FOR ALL ENTRIES IN me->mt_mara
        WHERE matnr = me->mt_mara-matnr
          AND vkorg IN me->ms_selscreen-vkorg
          AND vtweg IN me->ms_selscreen-vtweg
          AND lvorm IN me->ms_selscreen-mvke_lvorm.
      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MVKE:| && me->mv_message.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
    ENDTRY.

  ENDMETHOD.

  METHOD build_mbew.

    DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    IF me->ms_selscreen-mbew = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MBEW' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          FROM mbew
          INTO TABLE me->mt_mbew
         FOR ALL ENTRIES IN me->mt_mara
        WHERE matnr = me->mt_mara-matnr
          AND bwkey IN me->ms_selscreen-bwkey
          AND bwtar IN me->ms_selscreen-bwtar
          AND lvorm IN me->ms_selscreen-mbew_lvorm.
      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MBEW:| && me->mv_message.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
    ENDTRY.

  ENDMETHOD.

  METHOD build_mlgn.

    IF me->ms_selscreen-mlgn = abap_false.
      RETURN.
    ENDIF.

*/ Select Data
    SELECT *
      FROM mlgn
      INTO CORRESPONDING FIELDS OF TABLE me->mt_mlgn
     FOR ALL ENTRIES IN me->mt_mara
    WHERE matnr = me->mt_mara-matnr
      AND lgnum IN me->ms_selscreen-lgnum
      AND lvorm IN me->ms_selscreen-mlgn_lvorm.

  ENDMETHOD.

  METHOD build_mlgt.

    IF me->ms_selscreen-mlgt = abap_false.
      RETURN.
    ENDIF.

*/ Select Data
    SELECT *
      FROM mlgt
      INTO CORRESPONDING FIELDS OF TABLE me->mt_mlgt
     FOR ALL ENTRIES IN me->mt_mara
    WHERE matnr = me->mt_mara-matnr
      AND lgnum IN me->ms_selscreen-lgnum
      AND lgtyp IN me->ms_selscreen-lgtyp
      AND lvorm IN me->ms_selscreen-mlgt_lvorm.

  ENDMETHOD.

  METHOD build_crvm.

    IF me->ms_selscreen-crvm = abap_false.
      RETURN.
    ENDIF.

*/ PRT (Production Resource Tool) Data
    SELECT *
      FROM crvm_b
      INTO TABLE me->mt_crvm
      FOR ALL ENTRIES IN me->mt_mara
    WHERE matnr = me->mt_mara-matnr
      AND werks IN me->ms_selscreen-werks.

  ENDMETHOD.

  METHOD build_mapr.

    IF me->ms_selscreen-mapr = abap_false.
      RETURN.
    ENDIF.

*/ MAPR (Forecasting) Data
    SELECT *
      FROM mapr
      INTO TABLE me->mt_mapr
      FOR ALL ENTRIES IN me->mt_mara
    WHERE matnr = me->mt_mara-matnr
      AND werks IN me->ms_selscreen-werks.

  ENDMETHOD.

  METHOD build_mean.

    IF me->ms_selscreen-mean = abap_false.
      RETURN.
    ENDIF.

*/ MEAN
    SELECT *
      FROM mean
      INTO TABLE me->mt_mean
      FOR ALL ENTRIES IN me->mt_mara
    WHERE matnr = me->mt_mara-matnr.
  ENDMETHOD.

  METHOD build_marm.

    IF me->ms_selscreen-marm = abap_false.
      RETURN.
    ENDIF.

*/ MEAN
    SELECT *
      FROM marm
      INTO TABLE me->mt_marm
      FOR ALL ENTRIES IN me->mt_mara
    WHERE matnr = me->mt_mara-matnr.

  ENDMETHOD.

  METHOD build_mlan.

    IF me->ms_selscreen-mlan = abap_false.
      RETURN.
    ENDIF.

*/ MLAN
    SELECT *
      FROM mlan
      INTO TABLE me->mt_mlan
      FOR ALL ENTRIES IN me->mt_mara
    WHERE matnr = me->mt_mara-matnr.

  ENDMETHOD.


  METHOD build_eord.

    DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    IF me->ms_selscreen-eord = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_EORD' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          FROM eord
          INTO TABLE me->mt_eord
         FOR ALL ENTRIES IN me->mt_mara
        WHERE matnr = me->mt_mara-matnr
         AND werks      IN me->ms_selscreen-werks.
*         AND lifnr      IN me->ms_selscreen-lifnr
*         AND vdatu      IN me->ms_selscreen-vdatu
*         AND bdatu      IN me->ms_selscreen-bdatu.
      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_EORD:| && me->mv_message.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
    ENDTRY.

  ENDMETHOD.

  METHOD build_mpop.

    DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    IF me->ms_selscreen-mvke = abap_false.
      RETURN.
    ENDIF.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MPOP' ).

    TRY.
*/ Select Data
*        SELECT (me->mt_field_list)
*          FROM mpop
*          INTO TABLE me->mt_mpop
*         FOR ALL ENTRIES IN me->mt_mara
*        WHERE matnr = me->mt_mara-matnr
*          AND werks IN me->ms_selscreen-werks.
      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_MPOP:| && me->mv_message.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
    ENDTRY.

  ENDMETHOD.

*  METHOD build_meinh.
*
*    DATA:
*      lx_open_sql_error TYPE REF TO cx_sy_open_sql_error,
*      ls_meinh          TYPE /gda/sdm_s_meinh.
*    FIELD-SYMBOLS:
*      <marm> LIKE LINE OF me->mt_marm,
*      <mara> LIKE LINE OF me->mt_mara.
*
*    IF me->ms_selscreen-marm = abap_false.
*      RETURN.
*    ENDIF.
*
*    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_MEINH' ).
*
*    TRY.
**/ Select Data
*        LOOP AT me->mt_marm ASSIGNING <marm>.
*          MOVE-CORRESPONDING <marm> TO ls_meinh.
*          READ TABLE me->mt_mara WITH KEY matnr = <marm>-matnr ASSIGNING <mara>.
*          IF sy-subrc = 0.
*            ls_meinh-ntgew = <mara>-ntgew.
*          ENDIF.
*          APPEND ls_meinh TO me->mt_meinh.
*          CLEAR:
*           ls_meinh.
*        ENDLOOP.
*
*      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
*        me->mv_message = lx_open_sql_error->get_text( ).
*        me->mv_message = |Error /GDA/SDM_S_MEINH:| && me->mv_message.
*        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
*          EXPORTING
*            mv_text = mv_message.
*    ENDTRY.
*
*  ENDMETHOD.

  METHOD refresh_spec.
    CLEAR: me->mv_spec_matnr.
*    CLEAR: me->ms_makt_spec.
    FREE: me->mt_marc_spec, me->mt_mard_spec,
          me->mt_mvke_spec, me->mt_mbew_spec,
          me->mt_mlgn_spec, me->mt_mlgt_spec,
          me->mt_mpop_spec, me->mt_mfhm_spec,
          me->mt_steuertab, me->mt_steummtab,
          me->mt_marm_spec, me->mt_mean_spec,
          me->mt_mlan_spec.
  ENDMETHOD.


  METHOD build_spec.

    FIELD-SYMBOLS:
      <ls_marc> TYPE /gda/sdm_s_marc,
      <ls_mard> TYPE /gda/sdm_s_mard,
      <ls_mvke> TYPE /gda/sdm_s_mvke,
      <ls_mbew> TYPE /gda/sdm_s_mbew,
      <ls_mlgn> TYPE /gda/sdm_s_mlgn,
      <ls_mlgt> TYPE /gda/sdm_s_mlgt,
      <ls_mapr> TYPE mapr,
      <ls_crvm> TYPE crvm_b,
      <ls_mean> TYPE mean,
      <ls_marm> TYPE marm,
      <ls_mlan> TYPE mlan.

    DATA:
      ls_mpop_sdm   TYPE /gda/sdm_s_mpop, "Forecasting
      ls_mpop       TYPE mpop,            "Forecasting
      ls_mfhm       TYPE mfhm,            "PRT
      ls_mfhm_sdm   TYPE /gda/sdm_s_mfhm, "PRT
      ls_smeinh     TYPE smeinh,          "UoM
      ls_smeinh_sdm TYPE /gda/sdm_s_meinh, "UoM
      ls_mean_sdm   TYPE /gda/sdm_s_mean,
      ls_mlan_sdm   TYPE /gda/sdm_mlan.


*/ MAKT
    IF me->ms_selscreen-makt = abap_true.
      READ TABLE me->mt_makt INTO me->ms_makt_spec
        WITH KEY matnr = me->mv_spec_matnr
                 spras = sy-langu.
    ENDIF.

*/ MARC
    IF me->ms_selscreen-marc = abap_true.
      READ TABLE mt_marc TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT me->mt_marc ASSIGNING <ls_marc> FROM sy-tabix.
          IF <ls_marc>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            INSERT <ls_marc> INTO TABLE me->mt_marc_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MARD
    IF me->ms_selscreen-mard = abap_true.
      READ TABLE mt_mard TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mard ASSIGNING <ls_mard> FROM sy-tabix.
          IF <ls_mard>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            INSERT <ls_mard> INTO TABLE me->mt_mard_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MVKE
    IF me->ms_selscreen-mvke = abap_true.
      READ TABLE mt_mvke TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mvke ASSIGNING <ls_mvke> FROM sy-tabix.
          IF <ls_mvke>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            INSERT <ls_mvke> INTO TABLE me->mt_mvke_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MBEW
    IF me->ms_selscreen-mbew = abap_true.
      READ TABLE mt_mbew TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mbew ASSIGNING <ls_mbew> FROM sy-tabix.
          IF <ls_mbew>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            INSERT <ls_mbew> INTO TABLE me->mt_mbew_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MLGN
    IF me->ms_selscreen-mlgn = abap_true.
      READ TABLE mt_mlgn TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mlgn ASSIGNING <ls_mlgn> FROM sy-tabix.
          IF <ls_mlgn>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            INSERT <ls_mlgn> INTO TABLE me->mt_mlgn_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MLGT
    IF me->ms_selscreen-mlgt = abap_true.
      READ TABLE mt_mlgt TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mlgt ASSIGNING <ls_mlgt> FROM sy-tabix.
          IF <ls_mlgt>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            INSERT <ls_mlgt> INTO TABLE me->mt_mlgt_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MAPR (Forecasting)
    IF me->ms_selscreen-mapr = abap_true.
      READ TABLE mt_mapr TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mapr ASSIGNING <ls_mapr> FROM sy-tabix.
          IF <ls_mapr>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
*/ Read with FM
            CALL FUNCTION 'MPOP_SINGLE_READ'
              EXPORTING
*               kzrfb      = SPACE    " Ind.: Refresh buffer entry for material no.
                matnr      = <ls_mapr>-matnr
*               maxtz      =     " Max. No. of Entries in Buffer
                werks      = <ls_mapr>-werks
              IMPORTING
*               wmpop      =     " Work area for MPOP
                o_mpop     = ls_mpop
*        TABLES
*               prowf_tab  =     " Table of forecast values (w/o key)
              EXCEPTIONS
                not_found  = 1
                wrong_call = 2
                OTHERS     = 3.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING ls_mpop TO ls_mpop_sdm.
              INSERT ls_mpop_sdm INTO TABLE me->mt_mpop_spec.
              CLEAR:
               ls_mpop,
               ls_mpop_sdm.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ CRVM (PRT)
    IF me->ms_selscreen-crvm = abap_true.
      READ TABLE mt_crvm TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_crvm ASSIGNING <ls_crvm> FROM sy-tabix.
          IF <ls_crvm>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
*/ Read with FM
            CALL FUNCTION 'MFHM_SINGLE_READ'
              EXPORTING
*               kzrfb      = ' '    " Ind.: Refresh buffer entry for material no.
                matnr      = <ls_crvm>-matnr
*               maxtz      =     " Max. No. of Entries in Buffer
                werks      = <ls_crvm>-werks
              IMPORTING
                wmfhm      = ls_mfhm
*               o_mfhm     =
              EXCEPTIONS
                not_found  = 1
                wrong_call = 2
                OTHERS     = 3.

            IF sy-subrc = 0.
              MOVE-CORRESPONDING ls_mfhm TO ls_mfhm_sdm.
              INSERT ls_mfhm_sdm INTO TABLE me->mt_mfhm_spec.
              CLEAR:
               ls_mfhm,
               ls_mfhm_sdm.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MLAN (Taxes)
    IF me->ms_selscreen-mlan = abap_true.
      CALL FUNCTION 'STEUERTAB_READ'
        EXPORTING
*         kzrfb           = ' '
          matnr           = me->mv_spec_matnr
        TABLES
          steuertab       = me->mt_steuertab
        EXCEPTIONS
          wrong_call      = 1
          steuertab_empty = 2
          OTHERS          = 3. "#EC *

      CALL FUNCTION 'STEUMMTAB_READ'
        EXPORTING
*         kzrfb           = ' '
          matnr           = me->mv_spec_matnr
        TABLES
          steummtab       = me->mt_steummtab
        EXCEPTIONS
          wrong_call      = 1
          steummtab_empty = 2
          OTHERS          = 3. "#EC *

      READ TABLE mt_mlan TRANSPORTING NO FIELDS
        WITH KEY matnr = mv_spec_matnr.
      IF sy-subrc = 0.
        LOOP AT mt_mlan ASSIGNING <ls_mlan> FROM sy-tabix.
          IF <ls_mlan>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            MOVE-CORRESPONDING <ls_mlan> TO ls_mlan_sdm.
            APPEND ls_mlan_sdm TO  me->mt_mlan_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MARM (Units of Measure)
    IF me->ms_selscreen-marm = abap_true.
      READ TABLE mt_marm TRANSPORTING NO FIELDS
       WITH KEY mandt = sy-mandt
                matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_marm ASSIGNING <ls_marm> FROM sy-tabix.
          IF <ls_marm>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            MOVE-CORRESPONDING <ls_marm> TO ls_smeinh_sdm.
            APPEND <ls_marm> TO me->mt_marm_spec.
            APPEND ls_smeinh_sdm TO me->mt_meinh_spec.
            CLEAR ls_smeinh_sdm.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*/ MEAN (Additional EANs)
    IF me->ms_selscreen-mean = abap_true.
      READ TABLE mt_mean TRANSPORTING NO FIELDS
       WITH KEY mandt = sy-mandt matnr = mv_spec_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT mt_mean ASSIGNING <ls_mean> FROM sy-tabix.
          IF <ls_mean>-matnr <> mv_spec_matnr.
            EXIT.
          ELSE.
            MOVE-CORRESPONDING <ls_mean> TO ls_mean_sdm.
            APPEND ls_mean_sdm TO me->mt_mean_spec.
            CLEAR:
             ls_mean_sdm.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD constructor ##NEEDED.
*/ Create BRF Exception Object Utility Object
*    IF mo_brf_exc_util IS NOT BOUND.
*      CREATE OBJECT mo_brf_exc_util
*        EXPORTING
*          iv_object_type = gc_object_material
*          iv_period      = sy-datum
*          iv_del_sign    = 'EQ'. "Equal
*    ENDIF.

  ENDMETHOD.

  METHOD get_output_table.
    et_output_table = me->mt_report_output.
  ENDMETHOD.


*  METHOD record_statistics.
*    me->mo_brf_exc_util->record_statistics( iv_commit = abap_true ).
*  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_grid IMPLEMENTATION.

  METHOD display.
    DATA: ls_layout_key TYPE salv_s_layout_key.

    DATA:
      lo_alv_grid      TYPE REF TO cl_salv_table,
      lo_alv_functions TYPE REF TO cl_salv_functions,
      lo_alv_display   TYPE REF TO cl_salv_display_settings,
      lo_alv_layout    TYPE REF TO cl_salv_layout,
*  lo_alv_sorts        TYPE REF TO cl_salv_sorts,
*   lo_alv_aggregations TYPE REF TO cl_salv_aggregations,
      lo_alv_events    TYPE REF TO cl_salv_events_table.
*   lx_salv_error       TYPE REF TO cx_salv_error.

    mt_alv_table = it_table.

    cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv_grid
                            CHANGING t_table = mt_alv_table ).
*/ Functions
    lo_alv_functions = lo_alv_grid->get_functions( ).
    lo_alv_functions->set_all( abap_true ).

*/ Display Settings
    lo_alv_display = lo_alv_grid->get_display_settings( ).
    lo_alv_display->set_striped_pattern( abap_true ).

*/ Layout Settings
    lo_alv_layout = lo_alv_grid->get_layout( ).
    ls_layout_key-report = sy-repid.
    lo_alv_layout->set_key( ls_layout_key ).
    lo_alv_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
    lo_alv_layout->set_default( abap_true ).
    IF iv_layout IS NOT INITIAL.
      lo_alv_layout->set_initial_layout( iv_layout ).
    ENDIF.

*/ Columns
    so_alv_columns = lo_alv_grid->get_columns( ).
    so_alv_columns->set_optimize( 'X' ).
    extra_fields_description( ).

*/ Events
    lo_alv_events = lo_alv_grid->get_event( ).
    CREATE OBJECT mo_alv_event_handler.
    SET HANDLER mo_alv_event_handler->handle_double_click FOR lo_alv_events.

*/ Disaply ALV
    lo_alv_grid->display( ).

  ENDMETHOD.

  METHOD handle_double_click.
    FIELD-SYMBOLS: <ls_alv_table> TYPE lcl_mm_material_exc=>ty_report_output.

    IF row <= 0 OR column IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE mt_alv_table ASSIGNING <ls_alv_table> INDEX row.
    SET PARAMETER ID 'MAT' FIELD <ls_alv_table>-matnr.
    CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
  ENDMETHOD.

  METHOD extra_fields_description ##NEEDED.

*    DATA: ls_brf_exc_emap TYPE zgd_brf_exc_emap.
*
*    SELECT SINGLE * INTO ls_brf_exc_emap
*      FROM zgd_brf_exc_emap
*    WHERE object_type = gc_object_material.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    set_col_description( EXPORTING iv_col_name = 'EXTRA_V1' iv_description = ls_brf_exc_emap-extra_v1 ).
*    set_col_description( EXPORTING iv_col_name = 'EXTRA_V2' iv_description = ls_brf_exc_emap-extra_v2 ).
*    set_col_description( EXPORTING iv_col_name = 'EXTRA_V3' iv_description = ls_brf_exc_emap-extra_v3 ).
*    set_col_description( EXPORTING iv_col_name = 'EXTRA_V4' iv_description = ls_brf_exc_emap-extra_v4 ).
*    set_col_description( EXPORTING iv_col_name = 'EXTRA_V5' iv_description = ls_brf_exc_emap-extra_v5 ).
*    set_col_description( EXPORTING iv_col_name = 'EXTRA_V6' iv_description = ls_brf_exc_emap-extra_v6 ).

  ENDMETHOD.

  METHOD set_col_description.
    DATA: lo_alv_column TYPE REF TO cl_salv_column_table.

    DATA: lv_text_s TYPE scrtext_s,
          lv_text_m TYPE scrtext_m,
          lv_text_l TYPE scrtext_l.

    IF iv_description IS INITIAL.
      RETURN.
    ENDIF.

    lv_text_s = lv_text_m = lv_text_l = iv_description.

    TRY.
        lo_alv_column ?= so_alv_columns->get_column( iv_col_name ).
        lo_alv_column->set_short_text( lv_text_s ).
        lo_alv_column->set_medium_text( lv_text_m ).
        lo_alv_column->set_long_text( lv_text_l ).

      CATCH cx_salv_not_found.
        MESSAGE TEXT-902 TYPE 'E'. "Extra Fields Mapping Error

    ENDTRY.
  ENDMETHOD.

ENDCLASS.
