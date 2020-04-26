**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MM_BOM_SELECT_SCREEN
**&---------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&  Include           /GDA/SDM_MAT_SELECTIONS
**&---------------------------------------------------------------------*
**SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE TEXT-002.
**SELECTION-SCREEN PUSHBUTTON /1(20) sel_all USER-COMMAND sel.
**SELECTION-SCREEN PUSHBUTTON /1(20) dsel_all USER-COMMAND dsel.
**INCLUDE /gda/sdm_include_bom_scr.
**SELECTION-SCREEN END OF BLOCK b11.
*
**SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME TITLE TEXT-003.
*SELECTION-SCREEN BEGIN OF BLOCK b14 WITH FRAME TITLE TEXT-020.
*SELECT-OPTIONS:
*  s_matnr FOR gs_so-matnr,
*  s_werks FOR gs_so-werks,
*  s_stlnr FOR gs_so-stlnr,
*  s_stlal FOR gs_so-stlal,
*  s_datuv FOR gs_so-datuv,
*  s_andat FOR gs_so-andat,
*  s_loekz FOR gs_so-loekz.
*SELECTION-SCREEN SKIP.
*
*SELECTION-SCREEN END OF BLOCK b14.
**SELECTION-SCREEN SKIP.
**SELECT-OPTIONS:
**            s_arbgb FOR t100-arbgb DEFAULT '/GDA/SDM1' TO '/GDA/SDM_PP1',
**            s_msgnr FOR t100-msgnr.
**
**
***PARAMETERS: p_struc TYPE char1 AS CHECKBOX.
**SELECTION-SCREEN END OF BLOCK b12.
*
**SELECTION-SCREEN BEGIN OF BLOCK b13 WITH FRAME TITLE TEXT-004.
**PARAMETERS    :                 p_red  AS CHECKBOX TYPE /gda/sdm_de_traffic DEFAULT 'X' USER-COMMAND ind.
**SELECTION-SCREEN COMMENT 40(30) icon1.
**PARAMETERS    :                 p_amb  AS CHECKBOX TYPE /gda/sdm_de_traffic  DEFAULT 'X' USER-COMMAND ind.
**SELECTION-SCREEN COMMENT 40(30) icon2.
**PARAMETERS    :                 p_gre  AS CHECKBOX TYPE /gda/sdm_de_traffic  DEFAULT 'X'   USER-COMMAND ind.
**SELECTION-SCREEN COMMENT 40(30) icon6.
**PARAMETERS    :                 p_suc  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND success.
**SELECTION-SCREEN COMMENT 40(30) icon3.
**PARAMETERS    :                 p_fai  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND fail.
**SELECTION-SCREEN COMMENT 40(30) icon5.
**SELECTION-SCREEN SKIP.
**PARAMETERS    :                 p_com  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND comp.
**SELECTION-SCREEN COMMENT 40(30) icon4.
**PARAMETERS    :                 p_vari   TYPE slis_vari NO-DISPLAY.
**SELECTION-SCREEN END OF BLOCK b13.
*
**PARAMETERS: p_max TYPE p_dbacc DEFAULT 5000 OBLIGATORY.
