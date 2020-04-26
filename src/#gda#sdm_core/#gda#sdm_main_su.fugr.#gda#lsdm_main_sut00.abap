*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01.07.2019 at 10:20:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: /GDA/SDM_CACT...................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_CACT                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_CACT                 .
CONTROLS: TCTRL_/GDA/SDM_CACT
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: /GDA/SDM_CLIENT.................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_CLIENT               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_CLIENT               .
CONTROLS: TCTRL_/GDA/SDM_CLIENT
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: /GDA/SDM_FDT....................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_FDT                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_FDT                  .
CONTROLS: TCTRL_/GDA/SDM_FDT
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: /GDA/SDM_OBJ....................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_OBJ                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_OBJ                  .
CONTROLS: TCTRL_/GDA/SDM_OBJ
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: /GDA/SDM_SETUP1.................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_SETUP1               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_SETUP1               .
CONTROLS: TCTRL_/GDA/SDM_SETUP1
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: /GDA/SDM_SETUP6.................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_SETUP6               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_SETUP6               .
CONTROLS: TCTRL_/GDA/SDM_SETUP6
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: /GDA/SDM_STAT...................................*
DATA:  BEGIN OF STATUS_/GDA/SDM_STAT                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/GDA/SDM_STAT                 .
CONTROLS: TCTRL_/GDA/SDM_STAT
            TYPE TABLEVIEW USING SCREEN '0007'.
*.........table declarations:.................................*
TABLES: */GDA/SDM_CACT                 .
TABLES: */GDA/SDM_CACTT                .
TABLES: */GDA/SDM_CLIENT               .
TABLES: */GDA/SDM_FDT                  .
TABLES: */GDA/SDM_FDT_TX               .
TABLES: */GDA/SDM_OBJ                  .
TABLES: */GDA/SDM_OBJ_TX               .
TABLES: */GDA/SDM_SETUP1               .
TABLES: */GDA/SDM_SETUP6               .
TABLES: */GDA/SDM_STAT                 .
TABLES: */GDA/SDM_STATT                .
TABLES: /GDA/SDM_CACT                  .
TABLES: /GDA/SDM_CACTT                 .
TABLES: /GDA/SDM_CLIENT                .
TABLES: /GDA/SDM_FDT                   .
TABLES: /GDA/SDM_FDT_TX                .
TABLES: /GDA/SDM_OBJ                   .
TABLES: /GDA/SDM_OBJ_TX                .
TABLES: /GDA/SDM_SETUP1                .
TABLES: /GDA/SDM_SETUP6                .
TABLES: /GDA/SDM_STAT                  .
TABLES: /GDA/SDM_STATT                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
