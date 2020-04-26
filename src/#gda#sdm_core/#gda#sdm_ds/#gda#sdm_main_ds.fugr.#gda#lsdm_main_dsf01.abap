*----------------------------------------------------------------------*
***INCLUDE /GDA/LSDM_MAIN_DSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MODIFY_RECORD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_RECORD INPUT.
DATA:
  l_index TYPE sytabix, " Index to note the lines found
  l_tabix TYPE sytabix, " Index of total table
  lv_desc1(30),
  lv_desc2(30).

FIELD-SYMBOLS:
  <record>          TYPE ANY, " Work area for table
  <DS_TYPE_ID>      type any,
  <DS_DESIG_ID>     type any,
  <DS_LINK_ID>      type any,
  <DS_LINK_ID_DESC> type any.

* Modify changed by and changed on fields for any updates
LOOP AT total.
l_tabix = sy-tabix.
* Assign the table header line to the work area
ASSIGN total TO <record>.
IF sy-subrc = 0. " If assigning is successful.
* Check if data was changed and get the corresponding line index
IF <action> = 'U' " Updated entry
OR <action> = 'N'. " New entry

READ TABLE extract WITH KEY <vim_xtotal_key>. " Key field

IF sy-subrc = 0.
* Clears the variables after updation.
l_index = sy-tabix.


ASSIGN COMPONENT 'DS_TYPE_ID'  of STRUCTURE <record> to <DS_TYPE_ID>.
ASSIGN COMPONENT 'DS_DESIG_ID' of STRUCTURE <record> to <DS_DESIG_ID>.
ASSIGN COMPONENT 'DS_LINK_ID'  of STRUCTURE <record> to <DS_LINK_ID>.

ASSIGN COMPONENT 'DS_LINK_ID'      of STRUCTURE <record> to <DS_LINK_ID>.
ASSIGN COMPONENT 'DS_LINK_ID_DESC' of STRUCTURE <record> to <DS_LINK_ID_DESC>.

* Get desriptions
SELECT SINGLE DS_TYPE_ID_DESC
 FROM /GDA/SDM_DS01T
 INTO lv_desc1
 WHERE spras      = sy-langu
   AND DS_TYPE_ID = <DS_TYPE_ID>.

SELECT SINGLE DS_DESIG_ID_DESC
 FROM /GDA/SDM_DS02T
 INTO lv_desc2
 WHERE spras      = sy-langu
   AND DS_DESIG_ID = <DS_DESIG_ID>.

concatenate lv_desc1 ':' lv_desc2      into <DS_LINK_ID_DESC>.
concatenate <DS_TYPE_ID> <DS_DESIG_ID> into <DS_LINK_ID>.
* Modify total table
MODIFY total FROM <record> INDEX l_tabix.

CHECK l_index > 0.
extract = <record>.

* Modify extract table
MODIFY extract INDEX l_index.
ENDIF.
ENDIF.
ENDIF.
ENDLOOP.
ENDMODULE.                 " MODIFY_RECORD  INPUT
