class /GDA/SDM_CL_SCUSTOM_SELECTIONS definition
  public
  inheriting from /GDA/SDM_CL_SELECTIONS
  create public .

public section.

  types:
    BEGIN OF ty_selscreen,
* SCUSTOM specific fields
      id                TYPE RANGE OF scustom-id,
* SCUSTOM specific tables
      scustom           TYPE c LENGTH 1,
* SDM Common
      msgno             TYPE RANGE OF bapiret2-number,
      max_rows          TYPE p_dbacc,
      record_statistics TYPE c LENGTH 1,
      errors_only       TYPE c LENGTH 1,
    END OF ty_selscreen .

  data MV_SPEC_ID type SCUSTOM-ID .
  data MT_SCUSTOM type /GDA/SDM_T_SCUSTOM_SORTED .
  data MT_SCUSTOM_SPEC type /GDA/SDM_T_SCUSTOM .
  data MS_SCUSTOM_SPEC type /GDA/SDM_S_SCUSTOM .

  methods SET_SELSCREEN
    importing
      !XS_SELSCREEN type TY_SELSCREEN optional .

  methods BUILD_SPEC
    redefinition .
  methods MAIN
    redefinition .
  methods REFRESH
    redefinition .
protected section.

  data MS_SELSCREEN type TY_SELSCREEN .

  methods BUILD_SCUSTOM .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_SCUSTOM_SELECTIONS IMPLEMENTATION.


  method BUILD_SCUSTOM.

    DATA:
     lx_open_sql_error TYPE REF TO cx_sy_open_sql_error.

    me->build_field_selection( iv_struct_name = '/GDA/SDM_S_SCUSTOM' ).

    TRY.
*/ Select Data
        SELECT (me->mt_field_list)
          UP TO me->ms_selscreen-max_rows ROWS
          FROM scustom
          INTO CORRESPONDING FIELDS OF TABLE me->mt_scustom
          WHERE id IN me->ms_selscreen-id.

      CATCH cx_sy_open_sql_error INTO lx_open_sql_error.
        me->mv_message = lx_open_sql_error->get_text( ).
        me->mv_message = |Error /GDA/SDM_S_SCUSTOM:| && me->mv_message.
        RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
          EXPORTING
            mv_text = mv_message.
    ENDTRY.

    IF lines( me->mt_scustom ) = 0.
      me->mv_message = TEXT-901. "'No data selected'.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = mv_message.
    ENDIF.

  endmethod.


  method BUILD_SPEC.
*CALL METHOD SUPER->BUILD_SPEC
*    .
   FIELD-SYMBOLS:
    <scustom> like line of me->mt_scustom.

*/ SCUSTOM
    IF me->ms_selscreen-scustom = abap_true.
      READ TABLE mt_scustom TRANSPORTING NO FIELDS
        WITH KEY id = mv_spec_id BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT me->mt_scustom ASSIGNING <scustom> FROM sy-tabix.
          IF <scustom>-id <> mv_spec_id.
            EXIT.
          ELSE.
            INSERT <scustom> INTO TABLE me->mt_scustom_spec.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  endmethod.


  METHOD MAIN.
**TRY.
*CALL METHOD SUPER->MAIN
*    .
** CATCH /gda/cx_sdm_exception_handl .
**ENDTRY.

    TRY.
        me->build_scustom( ).
      CATCH /gda/cx_sdm_exception_handl .
      me->mv_message = TEXT-901.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = mv_message.

    ENDTRY.

  ENDMETHOD.


  method REFRESH.
*CALL METHOD SUPER->REFRESH
*    .

    CLEAR:
      me->mv_spec_id.
    FREE:
      me->mt_scustom_spec.

  endmethod.


  METHOD SET_SELSCREEN.
    me->ms_selscreen = xs_selscreen.
  ENDMETHOD.
ENDCLASS.
