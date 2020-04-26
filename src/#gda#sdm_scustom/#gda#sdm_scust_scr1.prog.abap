*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_SCUSTOM_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF BLOCK b14 WITH FRAME TITLE TEXT-020.

SELECT-OPTIONS: s_id  FOR scustom-id.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b14.
SELECTION-SCREEN END OF BLOCK b12.

PARAMETERS: p_scust DEFAULT 'X' NO-DISPLAY. "SFLIGHT Customers
