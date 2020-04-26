*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MAT_SELECTIONS
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF BLOCK b14 WITH FRAME TITLE TEXT-020.

SELECT-OPTIONS: s_matnr  FOR mara-matnr,
                s_mtart  FOR mara-mtart,
                s_attyp  FOR mara-attyp no-DISPLAY,
*                s_prdha  FOR mara-prdha,
                s_matkl  FOR mara-matkl,
*                s_bwscl  FOR mara-bwscl,
                s_ersda  FOR mara-ersda,
                s_ernam  FOR mara-ernam,
                s_laeda  FOR mara-laeda,
                s_aenam  FOR mara-aenam,
                s_mstae  FOR mara-mstae.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b14.
SELECTION-SCREEN END OF BLOCK b12.

PARAMETERS: p_makt DEFAULT 'X' NO-DISPLAY, "Descriptions
            p_marc DEFAULT 'X' NO-DISPLAY, "Plant
            p_mard DEFAULT 'X' NO-DISPLAY, "Storage Location
            p_mvke DEFAULT 'X' NO-DISPLAY, "Sales
            p_mbew DEFAULT 'X' NO-DISPLAY, "Valuation
            p_mlgn DEFAULT 'X' NO-DISPLAY, "Warehousing
            p_mlgt DEFAULT 'X' NO-DISPLAY, "Storage Types
            p_mapr DEFAULT 'X' NO-DISPLAY, "Forecasting
            p_crvm DEFAULT 'X' NO-DISPLAY, "PRT
            p_mlan DEFAULT 'X' NO-DISPLAY, "Tax Data
            p_marm DEFAULT 'X' NO-DISPLAY, "Units of Measure
            p_mean DEFAULT 'X' NO-DISPLAY, "Additional EANs
            p_eord DEFAULT 'X' NO-DISPLAY. "Purchasing Source List
