*&---------------------------------------------------------------------*
*& Report  /GDA/U_TABLE_EXPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT /GDA/U_TABLE_EXPORT.
TABLES:
  DD02T,
  DD03T.

FIELD-SYMBOLS: <DYN_TABLE> TYPE STANDARD TABLE,
               <DYN_WA>,
               <DYN_FIELD>.

DATA:
  FILENAME    TYPE STRING,
  P_TABLE(30) TYPE C.

DATA: BEGIN OF  IT_DETAILS_TXT OCCURS 0,
        LINE(20000),
      END OF  IT_DETAILS_TXT.

DATA: DY_TABLE TYPE REF TO DATA,
      DY_LINE  TYPE REF TO DATA,
      XFC      TYPE LVC_S_FCAT,
      IFC      TYPE LVC_T_FCAT.

SELECT-OPTIONS TAB FOR DD02T-TABNAME OBLIGATORY.
PARAMETERS: FPATH TYPE STRING OBLIGATORY.
PARAMETERS: DELIM TYPE CHAR01 OBLIGATORY DEFAULT ','.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FPATH.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE
*   EXPORTING
*     WINDOW_TITLE         =
*     INITIAL_FOLDER       =
    CHANGING
      SELECTED_FOLDER = FPATH
*   EXCEPTIONS
*     CNTL_ERROR      = 1
*     ERROR_NO_GUI    = 2
*     NOT_SUPPORTED_BY_GUI = 3
*     others          = 4
    .
  IF SY-SUBRC <> 0.
*  Implement suitable error handling here
  ENDIF.



START-OF-SELECTION.
  LOOP AT TAB.
  p_table = TAB-LOW.
*  p_table = TAB.
BREAK-POINT.

    PERFORM GET_STRUCTURE.
    PERFORM CREATE_DYNAMIC_ITAB.

*******Creates a dynamic internal table*********
    PERFORM GET_DATA.
    PERFORM WRITE_OUT.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  get_structure
*&---------------------------------------------------------------------*
FORM GET_STRUCTURE.
  DATA : IDETAILS TYPE ABAP_COMPDESCR_TAB,
         XDETAILS TYPE ABAP_COMPDESCR.
  DATA : REF_TABLE_DES TYPE REF TO CL_ABAP_STRUCTDESCR.
* Get the structure of the table.
  REF_TABLE_DES ?=
      CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( P_TABLE ).
  IDETAILS[] = REF_TABLE_DES->COMPONENTS[].
  LOOP AT IDETAILS INTO XDETAILS.
    CLEAR XFC.
    XFC-FIELDNAME = XDETAILS-NAME .
* Correction by Paul Robert Oct 28, 2009 17:04
*    xfc-datatype = xdetails-type_kind.
    CASE XDETAILS-TYPE_KIND.
      WHEN 'C'.
        XFC-DATATYPE = 'CHAR'.
      WHEN 'N'.
        XFC-DATATYPE = 'NUMC'.
      WHEN 'D'.
        XFC-DATATYPE = 'DATE'.
      WHEN 'P'.
        XFC-DATATYPE = 'PACK'.
      WHEN OTHERS.
        XFC-DATATYPE = XDETAILS-TYPE_KIND.
    ENDCASE.
    XFC-INTTYPE = XDETAILS-TYPE_KIND.
    XFC-INTLEN = XDETAILS-LENGTH.
    XFC-DECIMALS = XDETAILS-DECIMALS.
    APPEND XFC TO IFC.
  ENDLOOP.
ENDFORM.                    "get_structure
*&---------------------------------------------------------------------*
*&      Form  create_dynamic_itab
*&---------------------------------------------------------------------*
FORM CREATE_DYNAMIC_ITAB.
* Create dynamic internal table and assign to FS
  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = IFC
*     i_length_in_byte = 'X' "added by Paul Robert Oct 28, 2009 17:04
    IMPORTING
      EP_TABLE        = DY_TABLE.
  ASSIGN DY_TABLE->* TO <DYN_TABLE>.
* Create dynamic work area and assign to FS
  CREATE DATA DY_LINE LIKE LINE OF <DYN_TABLE>.
  ASSIGN DY_LINE->* TO <DYN_WA>.
ENDFORM.                    "create_dynamic_itab

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM GET_DATA.
* Select Data from table.
  SELECT * INTO TABLE <DYN_TABLE>
             FROM (P_TABLE).
ENDFORM.                    "get_data

*&---------------------------------------------------------------------*
*&      Form  write_out
*&---------------------------------------------------------------------*
FORM WRITE_OUT.
  LOOP AT <DYN_TABLE> INTO <DYN_WA>.
    DO.
      ASSIGN COMPONENT  SY-INDEX
         OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.
      IF SY-INDEX = 1.
        WRITE:/ <DYN_FIELD>.
      ELSE.
        WRITE: <DYN_FIELD>.
      ENDIF.
    ENDDO.
  ENDLOOP.
ENDFORM.                    "write_out
