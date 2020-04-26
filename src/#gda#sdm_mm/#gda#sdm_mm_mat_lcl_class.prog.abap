*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_MAT_LCL_CLASS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_CLASS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_event_rec DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_rec DEFINITION.
  PUBLIC SECTION.
    METHODS :
      on_hotspot_click
*                link_click OF cl_salv_events_table IMPORTING row column,
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                    e_column_id,

      handle_context_menu
                    FOR EVENT context_menu_request OF cl_gui_alv_grid
        IMPORTING e_object
                    sender,

      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm
                    sender,

      toolbar              FOR EVENT toolbar
                    OF cl_gui_alv_grid
        IMPORTING e_object.
*                    e_interactive.

ENDCLASS.                    "lcl_event_rec DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_rec IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_rec IMPLEMENTATION.

  METHOD on_hotspot_click.
    DATA:
     lv_row_no TYPE i.

    FIELD-SYMBOLS:
      <material> TYPE any,
      <status>   TYPE any.

*    CLEAR:  LV_ROW_NO.

    lv_row_no = e_row_id.

    IF lv_row_no IS NOT INITIAL.
*      READ TABLE  <dyn_table_final>[] ASSIGNING <dyn_wa> INDEX lv_row_no.
      READ TABLE  <dyn_table>[] ASSIGNING <dyn_wa> INDEX lv_row_no.
      IF sy-subrc EQ 0.
        IF e_column_id CS 'MATNR'.
          ASSIGN COMPONENT e_column_id OF STRUCTURE <dyn_wa> TO <material>.
          SET PARAMETER ID: 'MAT' FIELD <material>.
          CALL TRANSACTION 'MM42' AND SKIP FIRST SCREEN.
*        ELSEIF e_column_id CS 'PRICE'.
*          ASSIGN COMPONENT e_column_id OF STRUCTURE <dyn_wa> TO <material>.
*          SET PARAMETER ID: 'MAT' FIELD <material>.
*          CALL TRANSACTION 'VKP5' AND SKIP FIRST SCREEN.
        ELSE.
          ASSIGN COMPONENT e_column_id OF STRUCTURE <dyn_wa> TO <status>.

          READ TABLE gt_pp_main_setup ASSIGNING <main_setup>  WITH KEY object_view = e_column_id.
          IF <main_setup>-tree = abap_false.
            PERFORM set_view_output_new USING e_column_id <status>.
          ELSE.
            PERFORM set_view_output_tree USING e_column_id <status>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "on_hotspot_click


  METHOD handle_context_menu.
*    BREAK-POINT.
* https://archive.sap.com/discussions/thread/910750
*   define local data
    DATA:
      lt_fcodes  TYPE ui_funcattr,
      ls_fcode   TYPE uiattentry,
      ls_func    TYPE ui_func,
      lt_func    TYPE ui_functions,
      lt_actions TYPE /gda/sdm_tt_obj_action,
      ls_row     TYPE lvc_s_row,
      ls_col     TYPE lvc_s_col.

    FIELD-SYMBOLS:
      <article>      TYPE any,
      <articles>     LIKE LINE OF gt_sdm_material,
      <sdm_instance> TYPE /gda/sdm_s_instances,
      <action>       LIKE LINE OF lt_actions.

    CALL METHOD sender->get_current_cell
      IMPORTING
        es_row_id = ls_row
        es_col_id = ls_col.

* get the object related to this row..
    READ TABLE <dyn_table> ASSIGNING <dyn_wa> INDEX ls_row.
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <article>.
    READ TABLE gt_sdm_material ASSIGNING <articles> WITH KEY material = <article>.
    READ TABLE <articles>-sdm_instances ASSIGNING <sdm_instance> INDEX 1.

    lt_actions = <sdm_instance>-object->load_related_actions( ).

* Deactivate all standard functions
    CALL METHOD e_object->get_functions
      IMPORTING
        fcodes = lt_fcodes.

    LOOP AT lt_fcodes INTO ls_fcode.
      ls_func = ls_fcode-fcode.
      APPEND ls_func TO lt_func.
    ENDLOOP.

    e_object->hide_functions( lt_func ).
    e_object->add_separator( ).

* Now Add SDM specific functions
    LOOP AT lt_actions ASSIGNING <action>.

      TRY.
          GET BADI sdm_handle_context.
        CATCH cx_badi_not_implemented.
          CLEAR sdm_handle_context.
      ENDTRY.

      IF NOT sdm_handle_context IS INITIAL.

        CALL BADI sdm_handle_context->context_menu_change
          EXPORTING
            xo_core           = <sdm_instance>-object
          CHANGING
            xo_action         = <action>
          EXCEPTIONS
            application_error = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
*          MESSAGE e() RAISING application_error.
        ENDIF.
      ENDIF.

      IF ls_col = <action>->get_view( ) OR <action>->get_view( ) = 'DEFAULT'.
        CALL METHOD <action>->add_to_context_menu
          EXPORTING
            xy_ctmenu = e_object.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_user_command.
    DATA:
      ls_row     TYPE lvc_s_row,
      ls_col     TYPE lvc_s_col,
      lt_actions TYPE /gda/sdm_tt_obj_action,
      lv_fcode   TYPE /gda/sdm_cact-fcode.

    FIELD-SYMBOLS:
      <article>      TYPE any,
      <articles>     LIKE LINE OF gt_sdm_material,
      <sdm_instance> TYPE /gda/sdm_s_instances,
      <action>       LIKE LINE OF lt_actions.

    CASE e_ucomm.
      WHEN 'MASS'.
        PERFORM mass_download.
      WHEN OTHERS.
        CALL METHOD sender->get_current_cell
          IMPORTING
            es_row_id = ls_row
            es_col_id = ls_col.

* get the object related to this row..
        READ TABLE <dyn_table> ASSIGNING <dyn_wa> INDEX ls_row.
        CHECK sy-subrc = 0.
        ASSIGN COMPONENT 'KEY_MATNR' OF STRUCTURE <dyn_wa> TO <article>.
        READ TABLE gt_sdm_material ASSIGNING <articles> WITH KEY material = <article>.
        READ TABLE <articles>-sdm_instances ASSIGNING <sdm_instance> INDEX 1.


        lt_actions = <sdm_instance>-object->load_related_actions( ).


        LOOP AT lt_actions ASSIGNING <action>.
          IF ls_col = <action>->get_view( ) OR <action>->get_view( ) = 'DEFAULT'.

            CALL METHOD <action>->get_fcode
              RECEIVING
                r_fcode = lv_fcode.

            IF lv_fcode = e_ucomm.
              EXIT.
            ENDIF.
          ENDIF.

*    CALL METHOD <action>->add_to_context_menu
*      EXPORTING
*        xy_ctmenu = e_object.
        ENDLOOP.

        CALL METHOD <sdm_instance>-object->/gda/sdm_if_action_processor~process_action
          EXPORTING
            x_action = <action>
            x_multi  = space
*  IMPORTING
*           y_refresh        =
*           y_action_handled =
*           y_not_authorised =
          .

    ENDCASE.
  ENDMETHOD.

  METHOD toolbar.
    DATA:
      ls_toolbar TYPE stb_button,
      lv_has_activex.

    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = lv_has_activex.

    CHECK lv_has_activex = abap_true.

*...Seperator
    ls_toolbar-function  = 'DUMMY'.
    ls_toolbar-butn_type = '3'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

*... Normal Button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'MASS'(931).                     "#EC NOTEXT
    ls_toolbar-icon      = icon_xxl.
    ls_toolbar-butn_type = '0'.
    ls_toolbar-disabled  = space.
    ls_toolbar-text      = 'Download Report'(932).          "#EC NOTEXT
    ls_toolbar-quickinfo = space.
    ls_toolbar-checked   = space.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.                    "toolbar
ENDCLASS.                    "lcl_event_rec IMPLEMENTATION
