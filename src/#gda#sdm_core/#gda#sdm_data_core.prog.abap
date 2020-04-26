*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_DATA_CORE
*&---------------------------------------------------------------------*
* This include is to contain only objects and declarations relevant to all SDM objects

TYPE-POOLS: icon.

CONSTANTS:
  gc_default       TYPE lvc_s_col          VALUE 'DEFAULT', " look into changing this..
  gc_display       TYPE aktyp              VALUE 'A',
  gc_poe           TYPE /gda/sdm_de_source VALUE '2',
  gc_rep           TYPE /gda/sdm_de_source VALUE '1',
  gc_der           TYPE /gda/sdm_de_type   VALUE '02',
  gc_val           TYPE /gda/sdm_de_type   VALUE '01',
  gc_pir           TYPE /gda/sdm_de_type   VALUE '03',
  gc_pri           TYPE /gda/sdm_de_type   VALUE '04',
  gc_src           TYPE /gda/sdm_de_type   VALUE '05',
  gc_maxdb_entries TYPE p_dbacc            VALUE '5000'.


TYPES: BEGIN OF s_fields,
         field(10),
       END OF s_fields.

TYPES: BEGIN OF struc1,
         table TYPE tabname16,
         key   TYPE boolean,
       END OF struc1.

TYPES: BEGIN OF struc2,
         tabname TYPE tabname16,
         field   TYPE fieldname,
       END OF struc2.

DATA:
  ok_code                     LIKE sy-ucomm,
  gx_sdm_root                 TYPE REF TO /gda/cx_sdm_exception_handl,
  gx_root                     TYPE REF TO cx_root,
  gx_fdt                      TYPE REF TO cx_fdt_input,
  gr_data                     TYPE REF TO data,
  gr_data_empty               TYPE REF TO data,
  gv_message                  TYPE string,
  gv_config_err,
  gv_is_active                TYPE boolean,
  gs_result_der               TYPE /gda/sdm_cl_brf_mapping=>ty_der_results,
  gr_sdm_type                 TYPE /gda/sdm_r_sdm_type,
  gs_sdm_type                 TYPE /gda/sdm_s_sdm_type,
  gt_result                   TYPE STANDARD TABLE OF /gda/sdm_s_val_results,
  gs_instance                 TYPE /gda/sdm_s_instances,
  gt_default_fields           TYPE STANDARD TABLE OF /gda/sdm_setup4,
  gt_default_msg_class        TYPE STANDARD TABLE OF /gda/sdm_setup8,
  gt_default_tables           TYPE STANDARD TABLE OF tabname,
  gs_default_tables           TYPE tabname,
  gv_type                     TYPE /gda/sdm_de_type,
  gv_module_rsr               TYPE /gda/sdm_de_mod_type VALUE '03',
  gv_module_exp               TYPE /gda/sdm_de_mod_type VALUE '01',
  gv_source,
  gv_table                    TYPE dd02t-tabname,
  ro_data                     TYPE REF TO data,
  ro_collated                 TYPE REF TO data,
  gv_selection_fields_entered TYPE abap_bool,
  gv_execute_report           TYPE abap_bool,
  gs_sscrfields               TYPE sscrfields,
  gt_view_tables              TYPE STANDARD TABLE OF tabname16,
  gt_view_tables_all          TYPE STANDARD TABLE OF tabname16,
  gt_pp_main_setup            TYPE STANDARD TABLE OF /gda/sdm_s_main,
  gt_pp_main_gen              TYPE STANDARD TABLE OF /gda/sdm_setup4,
  gt_pp_output                TYPE STANDARD TABLE OF /gda/sdm_setup5,
  gs_table                    TYPE tabname16,
  ls_field                    TYPE s_fields,
  gt_view_struc               TYPE lvc_t_fcat,
  gs_view_struc               TYPE lvc_s_fcat,
  ro_salv                     TYPE REF TO cl_salv_table,
  go_sdm_results              TYPE REF TO data,
  go_handler_1                TYPE REF TO /gda/sdm_cl_rec_status_events,
  go_container_1              TYPE REF TO cl_gui_docking_container,
  go_splitter                 TYPE REF TO cl_gui_splitter_container,
  go_parent1                  TYPE REF TO cl_gui_container,
  go_parent2                  TYPE REF TO cl_gui_container,
  go_table_top                TYPE REF TO cl_salv_table,
  go_alv_top                  TYPE REF TO cl_gui_alv_grid,
  go_alv                      TYPE REF TO cl_gui_alv_grid,
  go_tree                     TYPE REF TO cl_gui_alv_tree,
  go_columns_top              TYPE REF TO cl_salv_columns_table,
  go_column_top               TYPE REF TO cl_salv_column_table,
  go_func_top                 TYPE REF TO cl_salv_functions_list,
  go_layout_top               TYPE REF TO cl_salv_layout,
  gt_fldcat                   TYPE slis_t_fieldcat_alv,
  gv_object                   TYPE /gda/sdm_de_object.


DATA:
  sdm_handle         TYPE REF TO /gda/sdm_badi1,
  sdm_handle_context TYPE REF TO /gda/sdm_badi12.

FIELD-SYMBOLS:
  <dyn_table>         TYPE STANDARD TABLE,
  <dyn_sdm_res>       TYPE STANDARD TABLE,
  <dyn_table_view>    TYPE STANDARD TABLE,
  <name>              TYPE any,
  <dyn_wa>            TYPE any,
  <dyn_wa_view>       TYPE any,
  <set_data>          TYPE any,
  <main_setup>        LIKE LINE OF gt_pp_main_setup,
  <main_gen>          LIKE LINE OF gt_pp_main_gen,
  <main_output>       LIKE LINE OF gt_pp_output,
  <tabstruc>          LIKE LINE OF gt_view_struc,
  <general_default>   LIKE LINE OF gt_default_fields,
  <default_msg_class> LIKE LINE OF gt_default_msg_class ,
  <result_val>        TYPE /gda/sdm_s_val_results,
  <results_val>       TYPE /gda/sdm_t_val_results,
  <results_val_all>   TYPE /gda/sdm_t_val_results.

DATA:
  gt_objects    TYPE STANDARD TABLE OF /gda/sdm_s_objects,
  gt_attributes TYPE /gda/sdm_cl_core=>ty_it_brf_attributes.

FIELD-SYMBOLS:
  <objects>   LIKE LINE OF gt_objects,
  <attribute> LIKE LINE OF gt_attributes.
