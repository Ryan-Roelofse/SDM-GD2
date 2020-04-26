*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_INCLUDE_SDM_SCR3
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b19 WITH FRAME TITLE TEXT-024.
PARAMETERS: p_max TYPE p_dbacc DEFAULT 5000 OBLIGATORY.

PARAMETERS: p_stat1 AS CHECKBOX MODIF ID sta.
PARAMETERS: p_stat2 AS CHECKBOX MODIF ID sta.
SELECTION-SCREEN END OF BLOCK b19.
