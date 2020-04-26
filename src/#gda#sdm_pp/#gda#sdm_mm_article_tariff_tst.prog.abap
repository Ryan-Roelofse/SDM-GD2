*&---------------------------------------------------------------------*
*& Report /GDA/SDM_MM_ARTICLE_TARIFF_TST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_mm_article_tariff_tst.

DATA:
  lt_mvke   TYPE /gda/sdm_t_mvke,
  lt_result TYPE /gda/sdm_tt_tariffs.

FIELD-SYMBOLS:
  <result> LIKE LINE OF lt_result.

PARAMETERS:
 p_matnr TYPE mara-matnr.


START-OF-SELECTION.

  SELECT vkorg FROM mvke INTO CORRESPONDING FIELDS OF TABLE lt_mvke
           WHERE matnr = p_matnr.

  CALL FUNCTION '/GDA/SDM_PP_BRF_TARIFF1'
    EXPORTING
      x_matnr  = p_matnr
      xt_mvke  = lt_mvke
    IMPORTING
      y_result = lt_result.

  LOOP AT lt_result ASSIGNING <result>.
    WRITE:/ <result>.
  ENDLOOP.
