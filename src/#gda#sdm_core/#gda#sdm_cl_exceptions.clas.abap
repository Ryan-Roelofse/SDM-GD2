class /GDA/SDM_CL_EXCEPTIONS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT
      !IV_PERIOD type DATUM
      !IV_DEL_SIGN type CHAR2 .
  methods CHECK_AUTHORISATION
    importing
      !IV_ACTVT type ACTIV_AUTH
    returning
      value(RV_AUTHORISED) type ABAP_BOOL .
  methods ADD_LINE
    importing
      !IS_EXC_REP_LINE type /GDA/SDM_EXCEPTIONS_ALV .
  methods RECORD_STATISTICS
    importing
      !IV_COMMIT type ABAP_BOOL
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods DELETE_STATISTICS
    importing
      !IV_COMMIT type ABAP_BOOL
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods RECORD_STATISTICS_V2
    importing
      !IV_COMMIT type ABAP_BOOL
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
protected section.
private section.

  data MV_OBJECT_TYPE type /GDA/SDM_DE_OBJECT .
  data MV_PERIOD type DATUM .
  data MV_DELETE_SIGN type CHAR2 .
  data MV_LOCKING_ERROR_TEXT type STRING .
  data MV_DELETE_SUCCESSFUL type ABAP_BOOL .
  data MV_INSERT_SUCCESSFUL type ABAP_BOOL .
  data:
    mt_SDM_EXCEPTIONS_ALV TYPE STANDARD TABLE OF /GDA/SDM_EXCEPTIONS_ALV
            WITH DEFAULT KEY .
  data:
    mt_SDM_EXCEPTIONS TYPE STANDARD TABLE OF /GDA/SDM_EXCEP .

  methods PREPARE_STATS .
  methods DELETE_STATS .
  methods INSERT_STATS .
  methods LOCK_STATS
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods UNLOCK_STATS .
  methods EXISTENCE_CHECK
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods PREPARE_STATS_V2 .
  methods LOCK_STATS_V2
    raising
      /GDA/CX_SDM_EXCEPTION_HANDL .
  methods EXISTENCE_CHECK_V2
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods DELETE_STATS_V2 .
  methods UNLOCK_STATS_V2 .
  methods INSERT_STATS_V2 .
ENDCLASS.



CLASS /GDA/SDM_CL_EXCEPTIONS IMPLEMENTATION.


  METHOD add_line.
    COLLECT is_exc_rep_line INTO me->mt_sdm_exceptions_alv.
  ENDMETHOD.


  method CHECK_AUTHORISATION.
    AUTHORITY-CHECK OBJECT '/GDA/SDM1'
           ID 'ACTVT'      FIELD iv_actvt
           ID '/GDA/SDM_O' FIELD me->mv_object_type.
    IF sy-subrc = 0.
      rv_authorised = abap_true.
    ELSE.
      rv_authorised = abap_false.
    ENDIF.

  endmethod.


  method CONSTRUCTOR.
    mv_object_type = iv_object_type.
    mv_period      = iv_period.  "Stats are created once per month
    mv_delete_sign = iv_del_sign.
*    mv_version     = iv_version.
  endmethod.


  method DELETE_STATISTICS.
*    DATA: lv_exist TYPE abap_bool.
*
*    me->lock_stats( ).
*
*    IF me->mv_locking_error_text IS NOT INITIAL.
*      RAISE EXCEPTION TYPE zcx_gd_brf_exc_report_util
*        EXPORTING
*          mv_text = me->mv_locking_error_text.
*    ENDIF.
*
*    lv_exist = existence_check( ).
*
*    IF lv_exist = abap_true.
*      me->delete_stats( ).
*    ENDIF.
*
*    me->unlock_stats( ).
*
*    IF mv_delete_successful = abap_true.
*      IF iv_commit = abap_true.
*        COMMIT WORK.
*      ENDIF.
*
*      RAISE EXCEPTION TYPE zcx_gd_brf_exc_report_util
*        EXPORTING
*          mv_text = 'Statistics table deleted successfully'.
*
*    ELSEIF lv_exist = abap_false.
*      RAISE EXCEPTION TYPE zcx_gd_brf_exc_report_util
*        EXPORTING
*          mv_text = 'Statistics Table does not contain any data'.
*
*    ELSE.
*      RAISE EXCEPTION TYPE zcx_gd_brf_exc_report_util
*        EXPORTING
*          mv_text = 'Error deleting table ZGD_BRF_EXC_STAT'.
*    ENDIF.


  endmethod.


  METHOD delete_stats.
*/ Deletion
    IF me->mv_delete_sign = 'LE'.
      DELETE FROM /gda/sdm_excep
        WHERE object_type = me->mv_object_type
          AND period <= me->mv_period.
    ELSEIF me->mv_delete_sign = 'EQ'.
      DELETE FROM /gda/sdm_excep
        WHERE object_type = me->mv_object_type
          AND period = me->mv_period.
    ELSE.
    ENDIF.

    IF sy-subrc = 0.
      me->mv_delete_successful = abap_true.
    ELSE.
      me->mv_delete_successful = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD DELETE_STATS_V2.
*/ Deletion
    IF me->mv_delete_sign = 'LE'.
      DELETE FROM /gda/sdm_excep
        WHERE object_type = me->mv_object_type
          AND period <= me->mv_period.
    ELSEIF me->mv_delete_sign = 'EQ'.
      DELETE FROM /gda/sdm_excep
        WHERE object_type = me->mv_object_type
          AND period = me->mv_period.
    ELSE.
    ENDIF.

    IF sy-subrc = 0.
      me->mv_delete_successful = abap_true.
    ELSE.
      me->mv_delete_successful = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD existence_check.

    DATA:
     lv_object TYPE /gda/sdm_de_object.

*/ Existence Check
    IF me->mv_delete_sign = 'LE'.
      SELECT SINGLE object_type
        INTO lv_object
        FROM /gda/sdm_excep
      WHERE object_type = me->mv_object_type
        AND period <= me->mv_period.
    ELSEIF me->mv_delete_sign = 'EQ'.
      SELECT SINGLE object_type
        INTO lv_object
        FROM /gda/sdm_excep
      WHERE object_type = me->mv_object_type
        AND period = me->mv_period.
    ELSE.
    ENDIF.

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD EXISTENCE_CHECK_V2.

    DATA:
     lv_object TYPE /gda/sdm_de_object.

*/ Existence Check
    IF me->mv_delete_sign = 'LE'.
      SELECT SINGLE object_type
        INTO lv_object
        FROM /gda/sdm_excep
      WHERE object_type = me->mv_object_type
        AND period <= me->mv_period.
    ELSEIF me->mv_delete_sign = 'EQ'.
      SELECT SINGLE object_type
        INTO lv_object
        FROM /gda/sdm_excep
      WHERE object_type = me->mv_object_type
        AND period = me->mv_period.
    ELSE.
    ENDIF.

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD insert_stats.
    INSERT /gda/sdm_excep FROM TABLE me->mt_sdm_exceptions.

    IF sy-subrc = 0.
      me->mv_insert_successful = abap_true.
    ELSE.
      me->mv_insert_successful = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD INSERT_STATS_V2.
    INSERT /gda/sdm_excep FROM TABLE me->mt_sdm_exceptions.

    IF sy-subrc = 0.
      me->mv_insert_successful = abap_true.
    ELSE.
      me->mv_insert_successful = abap_false.
    ENDIF.

  ENDMETHOD.


  method LOCK_STATS.
    CALL FUNCTION 'ENQUEUE_/GDA/SDM_EXCEP'
      EXPORTING
        mode_zgd_brf_exc_stat = 'E'
        mandt                 = sy-mandt
        object_type           = me->mv_object_type
        period                = me->mv_period
*       X_OBJECT              = ' '
*       X_PERIOD              = ' '
*       _SCOPE                = '2'
*       _WAIT                 = ' '
*       _COLLECT              = ' '
      EXCEPTIONS
        foreign_lock          = 1
        system_failure        = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                               INTO me->mv_locking_error_text
                               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      me->mv_locking_error_text = |Locking Error: | && me->mv_locking_error_text.
    ENDIF.

  endmethod.


  method LOCK_STATS_V2.
    CALL FUNCTION 'ENQUEUE_/GDA/SDM_EXCEP'
      EXPORTING
        mode_zgd_brf_exc_stat = 'E'
        mandt                 = sy-mandt
        object_type           = me->mv_object_type
        period                = me->mv_period
*       X_OBJECT              = ' '
*       X_PERIOD              = ' '
*       _SCOPE                = '2'
*       _WAIT                 = ' '
*       _COLLECT              = ' '
      EXCEPTIONS
        foreign_lock          = 1
        system_failure        = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                               INTO me->mv_locking_error_text
                               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      me->mv_locking_error_text = |Locking Error: | && me->mv_locking_error_text.
    ENDIF.

  endmethod.


  METHOD prepare_stats.
    DATA:
     ls_sdm_excep TYPE /gda/sdm_excep.

    FIELD-SYMBOLS:
     <exceptions_alv> TYPE /gda/sdm_exceptions_alv.

    LOOP AT me->mt_sdm_exceptions_alv ASSIGNING <exceptions_alv>.
      MOVE-CORRESPONDING <exceptions_alv> TO ls_sdm_excep.

      ls_sdm_excep-object_type = me->mv_object_type.
      ls_sdm_excep-period      = me->mv_period.
      ls_sdm_excep-counter     = ls_sdm_excep-counter + 1.

      APPEND ls_sdm_excep TO me->mt_sdm_exceptions.
    ENDLOOP.
  ENDMETHOD.


  METHOD PREPARE_STATS_V2.
    DATA:
     ls_sdm_excep TYPE /gda/sdm_excep.

    FIELD-SYMBOLS:
     <exceptions_alv> TYPE /gda/sdm_exceptions_alv.

    LOOP AT me->mt_sdm_exceptions_alv ASSIGNING <exceptions_alv>.
      MOVE-CORRESPONDING <exceptions_alv> TO ls_sdm_excep.

      ls_sdm_excep-object_type = me->mv_object_type.
      ls_sdm_excep-period      = me->mv_period.
      ls_sdm_excep-counter     = ls_sdm_excep-counter + 1.

      APPEND ls_sdm_excep TO me->mt_sdm_exceptions.
    ENDLOOP.
  ENDMETHOD.


  METHOD record_statistics.
    DATA: lv_exist TYPE abap_bool.

    IF me->mt_sdm_exceptions_alv IS INITIAL.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = 'No Statistical data found'.
    ENDIF.

    me->prepare_stats( ).
    me->lock_stats( ).

    IF me->mv_locking_error_text IS NOT INITIAL.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = me->mv_locking_error_text.
    ENDIF.

    lv_exist = me->existence_check_v2( ).
    IF lv_exist = abap_true.
      me->delete_stats( ).
    ENDIF.

    me->insert_stats( ).
    me->unlock_stats( ).

    IF ( mv_delete_successful = abap_true OR lv_exist = abap_false )
    AND mv_insert_successful = abap_true.
      IF iv_commit = abap_true.
        COMMIT WORK.
      ENDIF.

      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = 'Statistics table updated successfully'.

    ELSE.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = 'Error updating table /GDA/SDM_EXCEP'.
    ENDIF.


  ENDMETHOD.


  METHOD RECORD_STATISTICS_V2.
    DATA: lv_exist TYPE abap_bool.

    IF me->mt_sdm_exceptions_alv IS INITIAL.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = 'No Statistical data found'.
    ENDIF.

    me->prepare_stats_v2( ).
    me->lock_stats_v2( ).

    IF me->mv_locking_error_text IS NOT INITIAL.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = me->mv_locking_error_text.
    ENDIF.

    lv_exist = me->existence_check_v2( ).
    IF lv_exist = abap_true.
      me->delete_stats_v2( ).
    ENDIF.

    me->insert_stats_v2( ).
    me->unlock_stats_v2( ).

    IF ( mv_delete_successful = abap_true OR lv_exist = abap_false )
    AND mv_insert_successful = abap_true.
      IF iv_commit = abap_true.
        COMMIT WORK.
      ENDIF.

      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = 'Statistics table updated successfully'.

    ELSE.
      RAISE EXCEPTION TYPE /gda/cx_sdm_exception_handl
        EXPORTING
          mv_text = 'Error updating table /GDA/SDM_EXCEP'.
    ENDIF.


  ENDMETHOD.


  method UNLOCK_STATS.


   CALL FUNCTION 'DEQUEUE_/GDA/SDM_EXCEP'
      EXPORTING
        mode_zgd_brf_exc_stat = 'E'
        mandt                 = sy-mandt
        object_type           = me->mv_object_type
        period                = me->mv_period
*       X_OBJECT              = ' '
*       X_PERIOD              = ' '
*       _SCOPE                = '3'
*       _SYNCHRON             = ' '
*       _COLLECT              = ' '
      .
  endmethod.


  method UNLOCK_STATS_V2.


   CALL FUNCTION 'DEQUEUE_/GDA/SDM_EXCEP'
      EXPORTING
        mode_zgd_brf_exc_stat = 'E'
        mandt                 = sy-mandt
        object_type           = me->mv_object_type
        period                = me->mv_period
*       X_OBJECT              = ' '
*       X_PERIOD              = ' '
*       _SCOPE                = '3'
*       _SYNCHRON             = ' '
*       _COLLECT              = ' '
      .
  endmethod.
ENDCLASS.
