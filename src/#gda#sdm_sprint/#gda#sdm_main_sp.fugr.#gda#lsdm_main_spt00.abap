*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 30.01.2019 at 09:59:22
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: /GDA/SDM_SETUP7.................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_SETUP7               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_SETUP7               .
CONTROLS: TCTRL_/GDA/SDM_SETUP7
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */GDA/SDM_SETUP7               .
TABLES: /GDA/SDM_SETUP7                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
