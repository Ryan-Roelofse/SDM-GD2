***&---------------------------------------------------------------------*
***&  Include           /GDA/SDM_MM_BOM_LCL
***&---------------------------------------------------------------------*
**
***&---------------------------------------------------------------------*
***&  Include           /GDA/SDM_MM_MATERIAL_LCL
***&---------------------------------------------------------------------*
*
*************************************************************************
**
**                         CLASS DEFINITIONS
**
*************************************************************************
*
*      CLASS lcl_mm_bom_exc DEFINITION FINAL CREATE PRIVATE.
*        PUBLIC SECTION.
*          TYPES: BEGIN OF ty_selscreen,
*                   stlnr             TYPE RANGE OF stko-stlnr,
*                   stlal             TYPE RANGE OF stko-stlal,
*                   matnr             TYPE RANGE OF mast-matnr,
*                   werks             TYPE RANGE OF mast-werks,
*                   datuv             TYPE RANGE OF stko-datuv,
*                   andat             TYPE RANGE OF stko-andat,
*                   loekz             TYPE RANGE OF stko-loekz,
*                   msgno             TYPE RANGE OF bapiret2-number,
*                   max_rows          TYPE p_dbacc,
*                   record_statistics TYPE c LENGTH 1,
*                   errors_only       TYPE c LENGTH 1,
*                 END OF ty_selscreen.
*
**          TYPES: BEGIN OF ty_report_output,
**                   stlty          TYPE stko-stlty,
**                   stlnr          TYPE stko-stlnr,
**                   matnr          TYPE matnr,
**                   message_id     TYPE symsgid,
**                   message_number TYPE symsgno,
**                   message_type   TYPE symsgty,
**                 END OF ty_report_output.
*
**          TYPES: ty_it_report_output TYPE TABLE OF ty_report_output.
*          DATA:
*            mt_stko TYPE SORTED TABLE OF stko WITH UNIQUE KEY stlty stlnr stlal stkoz,
*            mt_stas TYPE SORTED TABLE OF stas WITH UNIQUE KEY stlty stlnr stlal stlkn stasz,
*            mt_stpo TYPE SORTED TABLE OF stpo WITH UNIQUE KEY stlty stlnr stlkn stpoz,
*            mt_mast TYPE TABLE OF mast.
*
*          DATA:
**            mt_mast      TYPE TABLE OF mast,
*            ms_mast_spec  TYPE /gda/sdm_s_mast,
*            mt_mast_spec  TYPE /gda/sdm_t_mast, "LIKE mt_mast,
*            mt_stko_spec  TYPE /gda/sdm_t_stko, "mt_stko,
*            mt_stas_spec  LIKE mt_stas,
*            mt_stpo_spec  LIKE mt_stpo,
*
*            mv_spec_stlnr TYPE stko-stlnr, "Specific BOM
*            mv_spec_stlal TYPE stko-stlal. "Specific Alternative
*
*          CLASS-METHODS: factory RETURNING VALUE(ro_bom) TYPE REF TO lcl_mm_bom_exc.
*
*          METHODS constructor.
*          METHODS main RAISING /gda/cx_sdm_exception_handl.
**          METHODS check_authorisation IMPORTING iv_actvt             TYPE activ_auth
**                                      RETURNING VALUE(rv_authorised) TYPE abap_bool.
*          METHODS set_selscreen IMPORTING is_selscreen TYPE ty_selscreen.
**          METHODS return_message RETURNING VALUE(rv_message) TYPE string.
**          METHODS get_output_table EXPORTING et_output_table TYPE ty_it_report_output.
**          METHODS record_statistics RAISING zcx_gd_brf_exc_report_util.
*          METHODS refresh_spec.
*          METHODS build_spec.
*
*        PRIVATE SECTION.
**          CONSTANTS: gc_object_bom TYPE /gda/sdm_de_object VALUE 'BOM'.
*
**          DATA: mo_brf_exc_util TYPE REF TO zgd_brf_exc_report_util.
*
*          DATA: mv_message    TYPE string.
**                mv_spec_stlnr TYPE stko-stlnr, "Specific BOM
**                mv_spec_stlal TYPE stko-stlal, "Specific Alternative
**                mv_timestamp  TYPE timestamp.
*
*          DATA: ms_selscreen TYPE ty_selscreen.
*
*          DATA: mt_field_list TYPE TABLE OF char30.
*          DATA: ms_stko_spec TYPE stko,
*                ms_stpo_spec TYPE stpo.
**       DATA:    mt_report_output TYPE TABLE OF ty_report_output.
*
*          METHODS build_field_selection
*            IMPORTING
*              iv_struct_name TYPE string.
*
*
*          METHODS build_mast RAISING /gda/cx_sdm_exception_handl.
*          METHODS build_stko RAISING /gda/cx_sdm_exception_handl.
*          METHODS build_stpo RAISING /gda/cx_sdm_exception_handl.
*
**          METHODS refresh_spec.
**          METHODS build_spec.
**          METHODS call_brf RAISING /gda/cx_sdm_exception_handl.
*      ENDCLASS.                    "lcl_EXRATE DEFINITION
*
*
*************************************************************************
**
**                        CLASS IMPLEMENTATIONS
**
*************************************************************************
*
*      CLASS lcl_mm_bom_exc IMPLEMENTATION.
*        METHOD factory.
*          CREATE OBJECT ro_bom.
*        ENDMETHOD.
*
*        METHOD main.
** FIELD-SYMBOLS:  " <ls_mara> TYPE /gda/sdm_s_mara,
**   <ls_makt> TYPE makt.
*
*
*          me->build_mast( ).
*          me->build_stko( ).
*          me->build_stpo( ).
*
**          LOOP AT me->mt_mast ASSIGNING FIELD-SYMBOL(<fs_mast>).
**            me->refresh_spec( ).
**            me->mv_spec_stlnr = <fs_mast>-stlnr.
**            me->mv_spec_stlal = <fs_mast>-stlal.
**            me->build_spec( ).
**            APPEND <fs_mast> TO me->mt_mast_spec.
**
**            me->call_brf( ).
**          ENDLOOP.
*
**          IF lines( me->mt_report_output ) IS INITIAL.
**            me->mv_message = |NO DATA selected FOR OUTPUT|.
**            RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
**              EXPORTING
**                mv_text = mv_message.
**          ENDIF.
*
*        ENDMETHOD.
*
*        METHOD set_selscreen.
*          me->ms_selscreen = is_selscreen.
*        ENDMETHOD.                    "SET_SELSCREEN
*
**        METHOD return_message.
**          rv_message = me->mv_message.
**        ENDMETHOD.
*
*
*        METHOD build_field_selection.
*
*          FIELD-SYMBOLS: <ls_components> TYPE LINE OF abap_compdescr_tab,
*                         <ls_field_list> TYPE char30.
*
*          DATA: lo_type_descr   TYPE REF TO cl_abap_typedescr,
*                lo_struct_descr TYPE REF TO cl_abap_structdescr.
*
*          FREE me->mt_field_list.
*
*          cl_abap_typedescr=>describe_by_name(
*          EXPORTING
*            p_name         =  iv_struct_name
*            RECEIVING
*            p_descr_ref    =  lo_type_descr
*          EXCEPTIONS
*            type_not_found = 1
*            OTHERS         = 2
*            ).
*          IF sy-subrc = 0.
*            lo_struct_descr ?= lo_type_descr.
*          ELSE.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            INTO me->mv_message
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          ENDIF.
*
***// Build Dynamic Field Selection
*          LOOP AT lo_struct_descr->components ASSIGNING <ls_components>.
*            APPEND INITIAL LINE TO me->mt_field_list ASSIGNING <ls_field_list>.
*            <ls_field_list> = <ls_components>-name.
*          ENDLOOP.
*
*        ENDMETHOD.
*
*        METHOD build_mast.
*
*          DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.
*
*          me->build_field_selection( iv_struct_name = 'MAST' ).
*
*          TRY.
**/ Select Data
*              SELECT (me->mt_field_list)
*              UP TO me->ms_selscreen-max_rows ROWS
*              FROM mast
*              INTO TABLE me->mt_mast
*              WHERE matnr IN me->ms_selscreen-matnr
*              AND werks IN me->ms_selscreen-werks
*              AND stlnr IN me->ms_selscreen-stlnr
*              AND stlal IN me->ms_selscreen-stlal.
*            CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
*              me->mv_message = lx_open_sql_error->get_text( ).
*              me->mv_message = |Error MAST:| && me->mv_message.
*              RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
*                EXPORTING
*                  mv_text = mv_message.
*          ENDTRY.
*
*          IF lines( me->mt_mast ) = 0.
*            me->mv_message = TEXT-020. "'No data selected'.
*            RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
*              EXPORTING
*                mv_text = mv_message.
*          ENDIF.
*
*        ENDMETHOD.
*
*        METHOD build_stko.
*          DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.
*
*
*          TRY.
*              SELECT * FROM
*              stko INTO TABLE me->mt_stko
*              FOR ALL ENTRIES IN me->mt_mast
*              WHERE stlnr = me->mt_mast-stlnr
*              AND stlal = me->mt_mast-stlal
*              AND stlty = 'M'
*              AND datuv IN me->ms_selscreen-datuv
*              AND andat IN me->ms_selscreen-andat.
*
*            CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
*              me->mv_message = lx_open_sql_error->get_text( ).
*              me->mv_message = |Error STPO:| && me->mv_message.
*              RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
*                EXPORTING
*                  mv_text = mv_message.
*          ENDTRY.
*
*        ENDMETHOD.
*
*
*        METHOD build_stpo.
*
*          DATA: lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.
*
*          TRY.
*
*              SELECT * FROM
*              stas INTO TABLE me->mt_stas
*              FOR ALL ENTRIES IN me->mt_stko
*              WHERE stlnr = me->mt_stko-stlnr
*              AND stlty = me->mt_stko-stlty
*              AND stlal = me->mt_stko-stlal.
*
*              SELECT * FROM
*              stpo INTO TABLE me->mt_stpo
*              FOR ALL ENTRIES IN me->mt_stas
*              WHERE stlnr = me->mt_stas-stlnr
*              AND stlty = me->mt_stas-stlty
*              AND stlkn = me->mt_stas-stlkn  .
*            CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
*              me->mv_message = lx_open_sql_error->get_text( ).
*              me->mv_message = |Error STPO:| && me->mv_message.
*              RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
*                EXPORTING
*                  mv_text = mv_message.
*          ENDTRY.
*
*
*        ENDMETHOD.
*
*
*        METHOD refresh_spec.
*          CLEAR: me->mv_spec_stlnr,me->ms_stko_spec.
*
*          FREE: me->mt_mast_spec, me->mt_stko_spec,me->mt_stpo_spec,me->mt_stas_spec.
*        ENDMETHOD.
*
*
*        METHOD build_spec.
*          READ TABLE me->mt_stko TRANSPORTING NO FIELDS
*          WITH KEY  stlty = 'M' stlnr = mv_spec_stlnr stlal = mv_spec_stlal  BINARY SEARCH.
*          IF sy-subrc = 0.
*            LOOP AT me->mt_stko ASSIGNING FIELD-SYMBOL(<fs_stko>) FROM sy-tabix.
*              IF <fs_stko>-stlnr <> mv_spec_stlnr OR <fs_stko>-stlal <> mv_spec_stlal.
*                EXIT.
*              ELSE.
*                INSERT <fs_stko> INTO TABLE me->mt_stko_spec.
*                MOVE-CORRESPONDING <fs_stko> TO me->ms_stko_spec.
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
*
*
*          READ TABLE me->mt_stas TRANSPORTING NO FIELDS
*          WITH KEY stlty = 'M' stlnr = mv_spec_stlnr stlal = mv_spec_stlal  BINARY SEARCH.
*          IF sy-subrc = 0.
*            LOOP AT me->mt_stas ASSIGNING FIELD-SYMBOL(<fs_stas>) FROM sy-tabix.
*              IF <fs_stas>-stlnr <> mv_spec_stlnr OR <fs_stas>-stlal <> mv_spec_stlal.
*                EXIT.
*              ELSE.
*                INSERT <fs_stas> INTO TABLE me->mt_stas_spec.
*                READ TABLE me->mt_stpo ASSIGNING FIELD-SYMBOL(<fs_stpo>)
*                WITH KEY stlty = 'M' stlnr = mv_spec_stlnr stlkn = <fs_stas>-stlkn BINARY SEARCH.
*                IF sy-subrc = 0.
*                  INSERT <fs_stpo> INTO TABLE me->mt_stpo_spec.
*                ENDIF.
*
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
*        ENDMETHOD.
*
*        METHOD constructor.
*
*        ENDMETHOD.
*      ENDCLASS.
