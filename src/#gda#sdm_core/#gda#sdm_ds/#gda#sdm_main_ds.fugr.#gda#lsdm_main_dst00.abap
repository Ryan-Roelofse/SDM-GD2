*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 05.09.2019 at 10:42:43 by user RROELOFSE
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: /GDA/SDM_DS01_V.................................*
TABLES: /GDA/SDM_DS01_V, */GDA/SDM_DS01_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_DS01_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/GDA/SDM_DS01_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_DS01_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_DS01_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS01_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS01_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_DS01_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS01_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS01_V_TOTAL.

*...processing: /GDA/SDM_DS02_V.................................*
TABLES: /GDA/SDM_DS02_V, */GDA/SDM_DS02_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_DS02_V
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_/GDA/SDM_DS02_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_DS02_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_DS02_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS02_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS02_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_DS02_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS02_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS02_V_TOTAL.

*...processing: /GDA/SDM_DS03_V.................................*
TABLES: /GDA/SDM_DS03_V, */GDA/SDM_DS03_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_DS03_V
TYPE TABLEVIEW USING SCREEN '0003'.
DATA: BEGIN OF STATUS_/GDA/SDM_DS03_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_DS03_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_DS03_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS03_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS03_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_DS03_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS03_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS03_V_TOTAL.

*...processing: /GDA/SDM_DS03_V2................................*
TABLES: /GDA/SDM_DS03_V2, */GDA/SDM_DS03_V2. "view work areas
CONTROLS: TCTRL_/GDA/SDM_DS03_V2
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_/GDA/SDM_DS03_V2. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_DS03_V2.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_DS03_V2_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS03_V2.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS03_V2_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_DS03_V2_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS03_V2.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS03_V2_TOTAL.

*...processing: /GDA/SDM_DS04_V.................................*
TABLES: /GDA/SDM_DS04_V, */GDA/SDM_DS04_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_DS04_V
TYPE TABLEVIEW USING SCREEN '0004'.
DATA: BEGIN OF STATUS_/GDA/SDM_DS04_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_DS04_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_DS04_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS04_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS04_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_DS04_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS04_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS04_V_TOTAL.

*...processing: /GDA/SDM_DS05_V.................................*
TABLES: /GDA/SDM_DS05_V, */GDA/SDM_DS05_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_DS05_V
TYPE TABLEVIEW USING SCREEN '0005'.
DATA: BEGIN OF STATUS_/GDA/SDM_DS05_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_DS05_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_DS05_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS05_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS05_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_DS05_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS05_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS05_V_TOTAL.

*...processing: /GDA/SDM_DS06_V.................................*
TABLES: /GDA/SDM_DS06_V, */GDA/SDM_DS06_V. "view work areas
CONTROLS: TCTRL_/GDA/SDM_DS06_V
TYPE TABLEVIEW USING SCREEN '0007'.
DATA: BEGIN OF STATUS_/GDA/SDM_DS06_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/GDA/SDM_DS06_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /GDA/SDM_DS06_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS06_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS06_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /GDA/SDM_DS06_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /GDA/SDM_DS06_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /GDA/SDM_DS06_V_TOTAL.

*.........table declarations:.................................*
TABLES: /GDA/SDM_DS01                  .
TABLES: /GDA/SDM_DS01T                 .
TABLES: /GDA/SDM_DS02                  .
TABLES: /GDA/SDM_DS02T                 .
TABLES: /GDA/SDM_DS03                  .
TABLES: /GDA/SDM_DS03T                 .
TABLES: /GDA/SDM_DS04                  .
TABLES: /GDA/SDM_DS05                  .
TABLES: /GDA/SDM_DS06                  .
