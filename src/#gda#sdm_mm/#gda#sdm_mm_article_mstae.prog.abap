*&---------------------------------------------------------------------*
*& Report /GDA/SDM_MM_ARTICLE_MSTAE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_mm_article_mstae.

TABLES:
  mara.

DATA:
  ls_headdata    TYPE bapie1mathead,
  ls_clientdata  TYPE bapie1marart,
  ls_clientdatax TYPE bapie1marartx,
  lt_clientdata  TYPE STANDARD TABLE OF bapie1marart,
  lt_clientdatax TYPE STANDARD TABLE OF bapie1marartx,
  ls_return      TYPE bapireturn1,
  lv_mstae       TYPE mara-mstae,
  lv_mstde       TYPE mara-MSTDE.

PARAMETERS:
  p_matnr TYPE mara-matnr MEMORY ID mat,
  p_mstae TYPE mara-mstae MEMORY ID mst.

INITIALIZATION.
  GET PARAMETER ID 'MAT' FIELD p_matnr.
  GET PARAMETER ID 'MST' FIELD p_mstae.

START-OF-SELECTION.

  ls_headdata-material      = p_matnr.
  ls_headdata-basic_view    = abap_true.
*  ls_headdata-logst_view    = abap_true.

  ls_clientdata-material    = p_matnr.
  ls_clientdata-pur_status  = lv_mstae.
  ls_clientdata-pvalidfrom  = lv_mstde.

  ls_clientdatax-material   = p_matnr.
  ls_clientdatax-pur_status = abap_true.
  ls_clientdatax-pvalidfrom = abap_true. "sy-datum.

  APPEND ls_clientdata  TO lt_clientdata.
  APPEND ls_clientdatax TO lt_clientdatax.


  CALL FUNCTION 'BAPI_MATERIAL_MAINTAINDATA_RT'
    EXPORTING
      headdata    = ls_headdata
    IMPORTING
      return      = ls_return
    TABLES
      clientdata  = lt_clientdata
      clientdatax = lt_clientdatax.

  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.

  IF ls_return IS NOT INITIAL.
    CLEAR:
     ls_return-message.

    IF ls_return-type = 'E'.
      ls_return-number = '538'.
      ls_return-type   = 'I'.
    ELSEIF ls_return-type = 'S'.
      ls_return-number = '537'.
    ENDIF.

    MESSAGE ID   '/GDA/SDM_ART2'
          TYPE   ls_return-type
          NUMBER ls_return-number
          WITH   ls_return-message_v1
                 ls_return-message_v2
                 ls_return-message_v3
                 ls_return-message_v4.
  ENDIF.
