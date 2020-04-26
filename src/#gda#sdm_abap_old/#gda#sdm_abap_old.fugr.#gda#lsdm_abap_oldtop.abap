FUNCTION-POOL /GDA/SDM_ABAP_OLD.            "MESSAGE-ID ..

* INCLUDE /GDA/LSDM_ABAP_OLDD...             " Local class definition

TYPES: BEGIN OF s_matnr,
         matnr TYPE mara-matnr,
       END OF s_matnr.

DATA gt_fields TYPE STANDARD TABLE OF char30.
* INCLUDE /GDA/LSDM_ABAP_NEWD...             " Local class definition

DATA:
 gt_materials    TYPE STANDARD TABLE OF s_matnr.
