*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_INCLUDE_SDM_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b13 WITH FRAME TITLE TEXT-004.
PARAMETERS    :                 p_red  AS CHECKBOX TYPE /gda/sdm_de_traffic DEFAULT 'X' USER-COMMAND ind.
SELECTION-SCREEN COMMENT 40(30) icon1.

PARAMETERS    :                 p_amb  AS CHECKBOX TYPE /gda/sdm_de_traffic  DEFAULT 'X' USER-COMMAND ind.
SELECTION-SCREEN COMMENT 40(30) icon2.

PARAMETERS    :                 p_gre  AS CHECKBOX TYPE /gda/sdm_de_traffic  DEFAULT 'X'   USER-COMMAND ind.
SELECTION-SCREEN COMMENT 40(30) icon6.

PARAMETERS    :                 p_suc  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND success.
SELECTION-SCREEN COMMENT 40(30) icon3.

PARAMETERS    :                 p_fai  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND fail.
SELECTION-SCREEN COMMENT 40(30) icon5.

SELECTION-SCREEN SKIP.

PARAMETERS    :                 p_com  AS CHECKBOX TYPE /gda/sdm_de_traffic  USER-COMMAND comp.
SELECTION-SCREEN COMMENT 40(30) icon4.

PARAMETERS    :                 p_vari   TYPE slis_vari NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b13.

PARAMETERS: p_max TYPE p_dbacc DEFAULT 5000 OBLIGATORY.
