*----------------------------------------------------------------------*
***INCLUDE /GDA/LSDM_MAIN_DSI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALUE_REQUEST INPUT.

data lt_dynpfield type table of DYNPREAD.
data ls_dynpfield type DYNPREAD.


CALL FUNCTION 'DYNP_VALUES_READ'
  EXPORTING
    dyname                               = sy-repid
    dynumb                               = sy-dynnr
   START_SEARCH_IN_CURRENT_SCREEN       = abap_true
   START_SEARCH_IN_MAIN_SCREEN          = abap_true
   START_SEARCH_IN_STACKED_SCREEN       = abap_true
   START_SEARCH_ON_SCR_STACKPOS         = abap_true
   SEARCH_OWN_SUBSCREENS_FIRST          = abap_true
   SEARCHPATH_OF_SUBSCREEN_AREAS        = abap_true
  tables
    dynpfields                           = lt_dynpfield
 EXCEPTIONS
   OTHERS                               = 11.
*DATA:
*  l_index TYPE sytabix, " Index to note the lines found
*  l_tabix TYPE sytabix, " Index of total table
*  lv_desc1(30),
*  lv_desc2(30).
*
*FIELD-SYMBOLS:
*  <record>          TYPE ANY, " Work area for table
*  <DS_TYPE_ID>      type any,
*  <DS_DESIG_ID>     type any,
*  <DS_LINK_ID>      type any,
*  <DS_LINK_ID_DESC> type any.

* Modify changed by and changed on fields for any updates
LOOP AT total.
*l_tabix = sy-tabix.
** Assign the table header line to the work area
*ASSIGN total TO <record>.
*IF sy-subrc = 0. " If assigning is successful.
** Check if data was changed and get the corresponding line index
*IF <action> = 'U' " Updated entry
*OR <action> = 'N'. " New entry
*
*READ TABLE extract WITH KEY <vim_xtotal_key>. " Key field
*
*IF sy-subrc = 0.
** Clears the variables after updation.
*l_index = sy-tabix.
*
*
*ASSIGN COMPONENT 'DS_TYPE_ID'  of STRUCTURE <record> to <DS_TYPE_ID>.
*ASSIGN COMPONENT 'DS_DESIG_ID' of STRUCTURE <record> to <DS_DESIG_ID>.
*ASSIGN COMPONENT 'DS_LINK_ID'  of STRUCTURE <record> to <DS_LINK_ID>.
*
*ASSIGN COMPONENT 'DS_LINK_ID'      of STRUCTURE <record> to <DS_LINK_ID>.
*ASSIGN COMPONENT 'DS_LINK_ID_DESC' of STRUCTURE <record> to <DS_LINK_ID_DESC>.
*
** Get desriptions
*SELECT SINGLE DS_TYPE_ID_DESC
* FROM /GDA/SDM_DS01T
* INTO lv_desc1
* WHERE spras      = sy-langu
*   AND DS_TYPE_ID = <DS_TYPE_ID>.
*
*SELECT SINGLE DS_DESIG_ID_DESC
* FROM /GDA/SDM_DS02T
* INTO lv_desc2
* WHERE spras      = sy-langu
*   AND DS_DESIG_ID = <DS_DESIG_ID>.
*
*concatenate lv_desc1 ':' lv_desc2      into <DS_LINK_ID_DESC>.
*concatenate <DS_TYPE_ID> <DS_DESIG_ID> into <DS_LINK_ID>.
** Modify total table
*MODIFY total FROM <record> INDEX l_tabix.
*
*CHECK l_index > 0.
*extract = <record>.
*
** Modify extract table
*MODIFY extract INDEX l_index.
*ENDIF.
*ENDIF.
*ENDIF.
ENDLOOP.

data:
  lv_tabname   type tabname,
  lv_fieldname type fieldname,
  lt_return    type STANDARD TABLE OF DDSHRETVAL.
BREAK-POINT.
lv_tabname = 'MARC'.
lv_fieldname = 'WERKS'.

data:
  ls_entries type /GDA/SDM_DS05_V.

ls_entries =  <VIM_TOTAL_KEY>.


CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
  EXPORTING
    tabname                   = ls_entries-tabname
    fieldname                 = ls_entries-fieldname
 TABLES
   RETURN_TAB                = lt_return
 EXCEPTIONS
   FIELD_NOT_FOUND           = 1
   NO_HELP_FOR_FIELD         = 2
   INCONSISTENT_HELP         = 3
   NO_VALUES_FOUND           = 4
   OTHERS                    = 5.

ENDMODULE.                 " VALUE_REQUEST  INPUT
