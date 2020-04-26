*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 05.07.2019 at 09:53:52
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: /GDA/SDM_CACT_V.................................*
TABLES: /GDA/SDM_CACT_V, */GDA/SDM_CACT_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_CACT_V
TYPE TABLEVIEW USING SCREEN '0008'.
DATA: BEGIN OF STATUS_/GDA/SDM_CACT_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_CACT_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_CACT_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_CACT_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_CACT_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_CACT_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_CACT_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_CACT_V_TOTAL.

*...processing: /GDA/SDM_CLSNRT.................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_CLSNRT               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_CLSNRT               .
CONTROLS: TCTRL_/GDA/SDM_CLSNRT
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: /GDA/SDM_MARITC.................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_MARITC               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_MARITC               .
CONTROLS: TCTRL_/GDA/SDM_MARITC
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: /GDA/SDM_OBJ_V..................................*
TABLES: /GDA/SDM_OBJ_V, */GDA/SDM_OBJ_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_OBJ_V
TYPE TABLEVIEW USING SCREEN '0007'.
DATA: BEGIN OF STATUS_/GDA/SDM_OBJ_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_OBJ_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_OBJ_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_OBJ_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_OBJ_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_OBJ_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_OBJ_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_OBJ_V_TOTAL.

*...processing: /GDA/SDM_STP3_V.................................*
TABLES: /GDA/SDM_STP3_V, */GDA/SDM_STP3_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_STP3_V
TYPE TABLEVIEW USING SCREEN '0005'.
DATA: BEGIN OF STATUS_/GDA/SDM_STP3_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_STP3_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_STP3_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_STP3_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_STP3_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_STP3_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_STP3_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_STP3_V_TOTAL.

*...processing: /GDA/SDM_STP4_V.................................*
TABLES: /GDA/SDM_STP4_V, */GDA/SDM_STP4_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_STP4_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/GDA/SDM_STP4_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_STP4_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_STP4_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_STP4_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_STP4_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_STP4_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_STP4_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_STP4_V_TOTAL.

*...processing: /GDA/SDM_STP5_V.................................*
TABLES: /GDA/SDM_STP5_V, */GDA/SDM_STP5_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_STP5_V
TYPE TABLEVIEW USING SCREEN '0003'.
DATA: BEGIN OF STATUS_/GDA/SDM_STP5_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_STP5_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_STP5_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_STP5_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_STP5_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_STP5_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_STP5_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_STP5_V_TOTAL.

*...processing: /GDA/SDM_STP8_V.................................*
TABLES: /GDA/SDM_STP8_V, */GDA/SDM_STP8_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_STP8_V
TYPE TABLEVIEW USING SCREEN '0009'.
DATA: BEGIN OF STATUS_/GDA/SDM_STP8_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_STP8_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_STP8_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_STP8_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_STP8_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_STP8_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_STP8_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_STP8_V_TOTAL.

*...processing: /GDA/SDM_TYPE...................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_TYPE                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_TYPE                 .
CONTROLS: TCTRL_/GDA/SDM_TYPE
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: */GDA/SDM_CLSNRT               .
TABLES: */GDA/SDM_MARITC               .
TABLES: */GDA/SDM_TYPE                 .
TABLES: */GDA/SDM_TYPE_TX              .
TABLES: /GDA/SDM_CACT                  .
TABLES: /GDA/SDM_CACTT                 .
TABLES: /GDA/SDM_CLSNRT                .
TABLES: /GDA/SDM_MARITC                .
TABLES: /GDA/SDM_OBJ                   .
TABLES: /GDA/SDM_SETUP3                .
TABLES: /GDA/SDM_SETUP4                .
TABLES: /GDA/SDM_SETUP5                .
TABLES: /GDA/SDM_SETUP8                .
TABLES: /GDA/SDM_TYPE                  .
TABLES: /GDA/SDM_TYPE_TX               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
