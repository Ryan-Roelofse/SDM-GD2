FUNCTION-POOL /GDA/SDM_PP_BRF.              "MESSAGE-ID ..

* INCLUDE /GDA/LSDM_PP_BRFD...               " Local class definition

  TYPES BEGIN OF struc1.
  TYPES: kschl TYPE kschl.
  TYPES: tabname TYPE tabname.
  TYPES END OF struc1.

data:
    gv_app_id                TYPE fdt_admn_0000-id,
    gv_id2                   TYPE fdt_admn_0000-id,
    gt_table_data_converted1 TYPE if_fdt_decision_table=>ts_table_data,
    gt_table_data_converted2 TYPE if_fdt_decision_table=>ts_table_data,
    gt_struc                 TYPE STANDARD TABLE OF struc1,
    gv_is_active             TYPE BOOLEAN,
    gv_init.
