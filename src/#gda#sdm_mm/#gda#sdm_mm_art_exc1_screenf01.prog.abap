*----------------------------------------------------------------------*
***INCLUDE /GDA/SDM_MM_ART_EXC1_SCREENF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT_ART_EXC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_output_art_exc.
  DATA:
    lv_active.


  CASE ok_code.
    WHEN 'STRUC'.
      IF p_struc = abap_true.
        lv_active = 1.
      ELSE.
        lv_active = 0.
      ENDIF.
      LOOP AT SCREEN.
        IF screen-group1 = 'SC1'.
          screen-active = lv_active.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
      IF p_struc = abap_true.
        lv_active = 1.
      ELSE.
        lv_active = 0.
      ENDIF.
      LOOP AT SCREEN.
        IF screen-group1 = 'SC1'.
          screen-active = lv_active.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFORM.
