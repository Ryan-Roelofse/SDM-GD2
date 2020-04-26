*----------------------------------------------------------------------*
***INCLUDE /GDA/LSDM_MAIN_SEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SORT_EXTRACT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE sort_extract OUTPUT.
  DATA:
    itab3 LIKE /gda/sdm_stp3_v OCCURS 1 WITH HEADER LINE,
    itab4 LIKE /gda/sdm_stp4_v OCCURS 1 WITH HEADER LINE,
    itab5 LIKE /gda/sdm_stp5_v OCCURS 1 WITH HEADER LINE.

  IF NOT sy-ucomm = 'NEWL'.
    CASE view_name.
      WHEN '/GDA/SDM_STP3_V'.
        itab3[] = extract[].
        SORT itab3 BY ord.
        extract[] = itab3[].
      WHEN '/GDA/SDM_STP4_V'.
        itab4[] = extract[].
        SORT itab4 BY ord.
        extract[] = itab4[].
      WHEN '/GDA/SDM_STP5_V'.
        itab5[] = extract[].
        SORT itab5 BY seq.
        extract[] = itab5[].
    ENDCASE.
  ENDIF.
ENDMODULE.
