class /GDA/SDM_CL_ACTIONS definition
  public
  create public .

public section.

  class-methods GET_OBJECT
    importing
      !X_SDM_ITEM type ref to /GDA/SDM_CL_CORE
      !X_ACTIONID type CHAR10
      !X_ACT type /GDA/SDM_CACT optional
      !X_ACTT type /GDA/SDM_CACTT optional
    returning
      value(R_OBJECT) type ref to /GDA/SDM_CL_ACTIONS .
  methods GET_ACTION_START
    exporting
      !EV_START_DATE type DATUM
      !EV_START_TIME type UZEIT .
  methods GET_FCODE
    returning
      value(R_FCODE) type /GDA/SDM_CACT-FCODE .
  methods IS_ACTION_HANDLED
    returning
      value(R_HANDLED_FLAG) type BOOLE-BOOLE .
  methods CONSTRUCTOR
    importing
      !X_SDM_ITEM type ref to /GDA/SDM_CL_CORE
      !X_ACTIONID type /GDA/SDM_CACT-ACTIONID
      !X_ACTION_PROCESSOR type ref to /GDA/SDM_IF_ACTION_PROCESSOR optional
      !X_ACT type /GDA/SDM_CACT optional
      !X_ACTT type /GDA/SDM_CACTT optional .
  methods GET_ACTIONID
    returning
      value(R_ACTIONID) type CHAR10 .
  methods IS_REFRESH_REQUIRED
    returning
      value(R_REFRESH_REQUIRED) type CHAR1 .
  methods SET_REFRESH_FLAG
    importing
      !X_REFRESH type CHAR1 optional .
  methods SET_ACTION_PROCESSOR
    importing
      !X_ACTION_PROCESSOR type ref to /GDA/SDM_IF_ACTION_PROCESSOR .
  methods SET_GROUP
    importing
      !X_GROUP type STRING .
  methods GET_ACTION_TEXT
    returning
      value(R_ACTION_TEXT) type STRING .
  methods GET_ICON_NAME
    returning
      value(R_ICON_NAME) type ICONNAME .
  methods SET_ACTION_HANDLED .
  methods CLEAR_ACTION_HANDLED .
  methods EXECUTE
    importing
      !X_NO_LOG type BOOLEAN optional
      !X_MULTI type FLAG optional .
  methods IS_NOT_SUPPORTED
    returning
      value(R_IS_NOT_SUPPORTED) type BOOLE-BOOLE .
  methods HAS_USER_MESSAGES
    returning
      value(R_HAS_MESSAGES) type BOOLE-BOOLE .
  methods OUTPUT_USER_MESSAGES .
  methods ADD_TO_CONTEXT_MENU
    importing
      !XY_CTMENU type ref to CL_CTMENU
      !X_DISABLED type FLAG default ' ' .
  methods GET_ACTION_PROCESSOR .
  methods SET_ACTIVE_FLAG
    importing
      !X_PROFILE type CHAR1 .
  methods IS_ACTIVE_FOR_USER
    returning
      value(R_ACTIVE) type CHAR1 .
  methods GET_VIEW
    returning
      value(R_VIEW) type /GDA/SDM_CACT-OBJECT_VIEW .
  methods SET_TO_GRAY .
protected section.

  data PV_ACTION_HANDLED type BOOLE-BOOLE .
  data PV_C_ACT type /GDA/SDM_CACT .
  data PV_REFRESH type CHAR1 .
  data PV_C_ACTT type /GDA/SDM_CACTT .
  data PO_MESSAGE_LOG type ref to /GDA/SDM_CL_MESSAGE_LOG .
  data PO_ACTION_PROCESSOR type ref to /GDA/SDM_IF_ACTION_PROCESSOR .
  data PV_GROUP type STRING .
  data PV_START_DATE type DATUM .
  data PV_START_TIME type UZEIT .
  data PV_NOT_SUPPORTED type BOOLE-BOOLE .
  data PV_ACTIVE type CHAR1 .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_ACTIONS IMPLEMENTATION.


  METHOD ADD_TO_CONTEXT_MENU.
    DATA:
      lv_fcode       TYPE ui_func,
      lv_text        TYPE gui_text,
      lv_icon        TYPE icon_d,
      lv_ftype       TYPE cua_ftyp,
      lv_disabled    TYPE cua_active,
      lv_hidden      TYPE cua_active,
      lv_checked     TYPE cua_active,
      lv_accelerator TYPE cua_path.

* Type cast to expected parameters
    lv_fcode       = pv_c_act-fcode.
    lv_text        = pv_c_actt-menutext.
    lv_ftype       = space.
    lv_disabled    = x_disabled.
    lv_hidden      = space.
    lv_checked     = space.
    lv_accelerator = space.

* Get the icon
*    lv_icon  = ycl_db_support=>get_icon( pv_c_act-iconname ).

* Check if this menu has its own group
    IF NOT pv_group IS INITIAL.
*   Add a separator first...
      CALL METHOD xy_ctmenu->add_separator( ).
    ENDIF.


    CASE me->pv_active.
      WHEN 0.
        lv_disabled = space.
        lv_hidden   = space.
      WHEN 1.
        lv_disabled = 'X'.
        lv_hidden   = space.
      WHEN 2.
        lv_disabled = space.
        lv_hidden   = 'X'.
    ENDCASE.

* Now add to the context menu
    CALL METHOD xy_ctmenu->add_function
      EXPORTING
        fcode       = lv_fcode
        text        = lv_text
        icon        = lv_icon
        ftype       = lv_ftype
        disabled    = lv_disabled
        hidden      = lv_hidden
        checked     = lv_checked
        accelerator = lv_accelerator.

  ENDMETHOD.


  method CLEAR_ACTION_HANDLED.
      clear pv_action_handled.
  endmethod.


  METHOD constructor.
*    CONSTANTS:
*      lc_tab TYPE tabname   VALUE '/GDA/SDM_CACT',
*      lc_fld TYPE fieldname VALUE 'ACTIONID'.

    IF x_act IS INITIAL.
* Select details of the action if not provided
      SELECT SINGLE *
        FROM /gda/sdm_cact
        INTO pv_c_act
       WHERE actionid = x_actionid
         AND inactive = space.

      CHECK sy-subrc = 0.
    ELSE.
      pv_c_act = x_act.
    ENDIF.

    IF x_actt IS INITIAL.
      SELECT SINGLE menutext FROM /gda/sdm_cactt
       INTO pv_c_actt-menutext
      WHERE actionid = pv_c_act-actionid
       AND  spras     = sy-langu.
    ELSE.
      pv_c_actt = x_actt.
    ENDIF.

* Set the action processor if it is passed through
    CREATE OBJECT po_message_log.

    IF NOT x_action_processor IS INITIAL.
      CALL METHOD set_action_processor( x_action_processor ).
    ENDIF.

  ENDMETHOD.


  method EXECUTE.
    DATA:
    lv_refresh        TYPE boole-boole,
    lv_action_handled TYPE boole-boole,
    lv_not_authorised TYPE boole-boole.

  CHECK NOT po_action_processor IS INITIAL.
  CALL METHOD me->clear_action_handled.
  po_message_log->refresh( ).


** TODO - logging
*  pv_start_date = sy-datum.
*  pv_start_time = sy-uzeit.

* Ask the action processor to execute this action
  CALL METHOD po_action_processor->process_action
    EXPORTING
      x_action         = me
      x_multi          = x_multi
    IMPORTING
      y_refresh        = lv_refresh
      y_action_handled = lv_action_handled
      y_not_authorised = lv_not_authorised.

* Handle exporting parameters

* Handle refresh
* Handle action handled
* Handle not supported flag

* If the action has user messages - output them... (need to work out
* a good way of displaying more than 1!)
  IF me->has_user_messages( ) EQ 'X'.

    CALL METHOD me->output_user_messages( ).

  ENDIF.

  IF NOT me->is_action_handled( ) IS INITIAL AND x_no_log IS INITIAL.

    CALL METHOD po_action_processor->save_log( me ).

  ENDIF.

*  CALL METHOD me->set_refresh_flag.

  endmethod.


  method GET_ACTIONID.
    r_actionid = pv_c_act-actionid.
  endmethod.


  method GET_ACTION_PROCESSOR.
  endmethod.


  method GET_ACTION_START.
  ev_start_date = pv_start_date.
  ev_start_time = pv_start_time.
  endmethod.


  method GET_ACTION_TEXT.
    r_action_text = pv_c_actt-menutext.
  endmethod.


  method GET_FCODE.
    r_fcode = pv_c_act-fcode.
  endmethod.


  method GET_ICON_NAME.
*    r_icon_name = pv_c_act-iconname.
  endmethod.


  method GET_OBJECT.

    CREATE OBJECT r_object
    EXPORTING
      X_SDM_ITEM         = X_SDM_ITEM
      x_actionid         = x_actionid
      x_action_processor = X_SDM_ITEM
      x_act              = x_act
      x_actt             = x_actt.

  endmethod.


  METHOD get_view.
    r_view = pv_c_act-object_view.
  ENDMETHOD.


  method HAS_USER_MESSAGES.
    R_has_messages = po_message_log->has_user_messages( ).
  endmethod.


  method IS_ACTION_HANDLED.
    r_handled_flag = pv_action_handled.
  endmethod.


  METHOD IS_ACTIVE_FOR_USER.
    r_active = pv_active.
  ENDMETHOD.


  method IS_NOT_SUPPORTED.
    r_is_not_supported = pv_not_supported.

  endmethod.


  METHOD IS_REFRESH_REQUIRED.
    r_refresh_required = pv_c_act-refresh.
    r_refresh_required = pv_refresh.
  ENDMETHOD.


  method OUTPUT_USER_MESSAGES.
* Get the message log to output them...
  CALL METHOD po_message_log->output_user_messages( ).

  endmethod.


  method SET_ACTION_HANDLED.
      pv_action_handled = 'X'.
  endmethod.


  method SET_ACTION_PROCESSOR.
   po_action_processor = x_action_processor.
  endmethod.


  METHOD SET_ACTIVE_FLAG.
    pv_active = x_profile.
  ENDMETHOD.


  method SET_GROUP.
    pv_group = x_group.
  endmethod.


  METHOD SET_REFRESH_FLAG.
    pv_refresh = pv_c_act-refresh.
    pv_refresh = x_refresh.
  ENDMETHOD.


  METHOD SET_TO_GRAY.
    pv_active = 1.
  ENDMETHOD.
ENDCLASS.
