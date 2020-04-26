
*DATA:
* gs_selscreen TYPE lcl_mm_material_exc=>ty_selscreen,
* go_selection TYPE REF TO lcl_mm_material_exc.
*
*
***// Select-Options
*DATA: BEGIN OF gs_so,
*        matnr TYPE mara-matnr,
*        ersda TYPE mara-ersda,
*        mtart TYPE mara-mtart,
*        mstae TYPE mara-mstae,
*        matkl TYPE mara-matkl,
*        lvorm TYPE mara-lvorm,
*        werks TYPE marc-werks,
*        mmsta TYPE marc-mmsta,
*        lgort TYPE mard-lgort,
*        vkorg TYPE mvke-vkorg,
*        vtweg TYPE mvke-vtweg,
*        bwkey TYPE mbew-bwkey,
*        bwtar TYPE mbew-bwtar,
*        lgnum TYPE mlgn-lgnum,
*        lgtyp TYPE mlgt-lgtyp,
*        msgno TYPE t100-msgnr,
*      END OF gs_so.
