*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_SELECTIONS
*&---------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE TEXT-002.
*SELECTION-SCREEN PUSHBUTTON /1(20) sel_all USER-COMMAND sel.
*SELECTION-SCREEN PUSHBUTTON /1(20) dsel_all USER-COMMAND dsel.
*INCLUDE /gda/sdm_include_pp.
*SELECTION-SCREEN END OF BLOCK b11.

SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF BLOCK b14 WITH FRAME TITLE TEXT-020.

SELECT-OPTIONS: s_matnr  FOR mara-matnr,
                s_mtart  FOR mara-mtart,
                s_attyp  FOR mara-attyp,
                s_matkl  FOR mara-matkl,
                s_bwscl  FOR mara-bwscl,
                s_ersda  FOR mara-ersda,
                s_ernam  FOR mara-ernam,
                s_laeda  FOR mara-laeda,
                s_aenam  FOR mara-aenam.
SELECTION-SCREEN SKIP.

SELECT-OPTIONS: s_mstae  FOR mara-mstae,
                s_mmsta  FOR marc-mmsta.
SELECTION-SCREEN SKIP.
PARAMETERS:     p_struc   TYPE char1 AS CHECKBOX USER-COMMAND STRUC.
SELECT-OPTIONS: s_attyps  FOR mara-attyp MODIF ID sc1.
SELECTION-SCREEN END OF BLOCK b14.

SELECTION-SCREEN BEGIN OF BLOCK b15 WITH FRAME TITLE TEXT-021.
SELECT-OPTIONS: s_werks  FOR marc-werks,
                s_lgort  FOR mard-lgort.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b16 WITH FRAME TITLE TEXT-022.
SELECT-OPTIONS: s_vkorg  FOR mvke-vkorg,
                s_vtweg  FOR mvke-vtweg.
SELECTION-SCREEN END OF BLOCK b16.


SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b17 WITH FRAME TITLE TEXT-023.
SELECT-OPTIONS: s_infnr FOR eina-infnr,
                s_lifnr  FOR eina-lifnr,
                s_ekorg  FOR eine-ekorg.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN SKIP.
SELECT-OPTIONS:
            s_vdatu FOR eord-vdatu NO INTERVALS,
            s_bdatu FOR eord-bdatu NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b17.
SELECTION-SCREEN END OF BLOCK b15.

*SELECTION-SCREEN SKIP.
*SELECT-OPTIONS:
*            s_arbgb FOR t100-arbgb,
*            s_msgnr FOR t100-msgnr.


*PARAMETERS: p_struc TYPE char1 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b12.

*SELECTION-SCREEN BEGIN OF BLOCK b13 WITH FRAME TITLE TEXT-004.
*PARAMETERS    :                 p_red  AS CHECKBOX TYPE /gda/sdm_de_traffic DEFAULT 'X' USER-COMMAND ind.
*SELECTION-SCREEN COMMENT 40(30) icon1.
*
*PARAMETERS    :                 p_amb  AS CHECKBOX TYPE /gda/sdm_de_traffic  DEFAULT 'X' USER-COMMAND ind.
*SELECTION-SCREEN COMMENT 40(30) icon2.
*
*PARAMETERS    :                 p_gre  AS CHECKBOX TYPE /gda/sdm_de_traffic  DEFAULT 'X'   USER-COMMAND ind.
*SELECTION-SCREEN COMMENT 40(30) icon6.
*
*PARAMETERS    :                 p_suc  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND success.
*SELECTION-SCREEN COMMENT 40(30) icon3.
*
*PARAMETERS    :                 p_fai  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND fail.
*SELECTION-SCREEN COMMENT 40(30) icon5.
*
*SELECTION-SCREEN SKIP.
*
*PARAMETERS    :                 p_com  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND comp.
*SELECTION-SCREEN COMMENT 40(30) icon4.
*
*PARAMETERS    :                 p_vari   TYPE slis_vari NO-DISPLAY.
*SELECTION-SCREEN END OF BLOCK b13.

*PARAMETERS: p_max TYPE p_dbacc DEFAULT 5000 OBLIGATORY.
*
*PARAMETERS: p_stat1 AS CHECKBOX.
*PARAMETERS: p_stat2 AS CHECKBOX.
