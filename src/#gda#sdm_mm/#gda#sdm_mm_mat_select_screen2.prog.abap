*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_MAT_SELECT_SCREEN2
*&---------------------------------------------------------------------*

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
*SELECT-OPTIONS: s_infnr FOR eina-infnr,
*                s_lifnr  FOR eina-lifnr,
*                s_ekorg  FOR eine-ekorg.
*
*SELECTION-SCREEN SKIP.

*SELECTION-SCREEN SKIP.
*SELECT-OPTIONS:
*            s_vdatu FOR eord-vdatu NO INTERVALS,
*            s_bdatu FOR eord-bdatu NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b17.
SELECTION-SCREEN END OF BLOCK b15.
