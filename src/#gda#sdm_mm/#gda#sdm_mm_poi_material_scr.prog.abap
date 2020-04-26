*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_MATERIAL_SCR
*&---------------------------------------------------------------------*

 DATA:
   ls_brf_screen_control TYPE /GDA/SDM_S_SCREEN_CONTROL,
   lt_screen_control     LIKE HASHED TABLE OF ls_brf_screen_control
                         WITH UNIQUE KEY screen_name.

IMPORT screen_control = lt_screen_control FROM MEMORY ID 'GD_MM_BRF_SCREEN_CONTROL'.  "Populated in /GDA/SDM_MM_ART_DERIVATION

READ TABLE lt_screen_control INTO ls_brf_screen_control
                                 WITH KEY screen_name = fauswtab-fname.
IF sy-subrc = 0.
  IF ls_brf_screen_control-grey_out IS NOT INITIAL.
    fauswtab-kzinp = '0'.
    fauswtab-kzout = '1'.
  ENDIF.

  IF ls_brf_screen_control-hide IS NOT INITIAL.
    fauswtab-kzact = '0'.
    fauswtab-kzinv = '1'.
  ENDIF.

  IF ls_brf_screen_control-required IS NOT INITIAL.
    fauswtab-kzreq = '1'.
  ENDIF.

  IF ls_brf_screen_control-bold IS NOT INITIAL.
    fauswtab-kzint = '1'.
  ENDIF.
ENDIF.
