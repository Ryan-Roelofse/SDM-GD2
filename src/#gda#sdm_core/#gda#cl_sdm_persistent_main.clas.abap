class /GDA/CL_SDM_PERSISTENT_MAIN definition
  public
  final
  create protected

  global friends /GDA/CB_SDM_PERSISTENT_MAIN .

public section.

  interfaces IF_OS_STATE .

  methods SET_SPR_STATUS
    importing
      !I_SPR_STATUS type /GDA/SDM_DE_SPRINT_STATUS
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_MSG_TYPE
    importing
      !I_MSG_TYPE type SYMSGTY
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_EXTRA_V6
    importing
      !I_EXTRA_V6 type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_EXTRA_V5
    importing
      !I_EXTRA_V5 type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_EXTRA_V4
    importing
      !I_EXTRA_V4 type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_EXTRA_V3
    importing
      !I_EXTRA_V3 type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_EXTRA_V2
    importing
      !I_EXTRA_V2 type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods SET_ENDDA
    importing
      !I_ENDDA type DATUM
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_TABNAME
    returning
      value(RESULT) type /GDA/SDM_DE_TABNAME
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_SPR_STATUS
    returning
      value(RESULT) type /GDA/SDM_DE_SPRINT_STATUS
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_SDM_TYPE
    returning
      value(RESULT) type /GDA/SDM_DE_TYPE
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_SDM_TABKEY
    returning
      value(RESULT) type CDTABKEY
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_SDM_OBJECT_ID
    returning
      value(RESULT) type /GDA/SDM_DE_OBJECT_ID
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_MSG_TYPE
    returning
      value(RESULT) type SYMSGTY
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_MSG_NUMBER
    returning
      value(RESULT) type SYMSGNO
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_MSG_ID
    returning
      value(RESULT) type SYMSGID
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_FIELD
    returning
      value(RESULT) type /GDA/SDM_DE_FIELDNAME
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_EXTRA_V6
    returning
      value(RESULT) type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_EXTRA_V5
    returning
      value(RESULT) type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_EXTRA_V4
    returning
      value(RESULT) type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_EXTRA_V3
    returning
      value(RESULT) type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_EXTRA_V2
    returning
      value(RESULT) type SYMSGV
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_ENDDA
    returning
      value(RESULT) type DATUM
    raising
      CX_OS_OBJECT_NOT_FOUND .
  methods GET_BEGDA
    returning
      value(RESULT) type DATUM
    raising
      CX_OS_OBJECT_NOT_FOUND .
protected section.

  data SDM_OBJECT_ID type /GDA/SDM_DE_OBJECT_ID .
  data SDM_TABKEY type CDTABKEY .
  data TABNAME type /GDA/SDM_DE_TABNAME .
  data FIELD type /GDA/SDM_DE_FIELDNAME .
  data MSG_ID type SYMSGID .
  data MSG_NUMBER type SYMSGNO .
  data BEGDA type DATUM .
  data SDM_TYPE type /GDA/SDM_DE_TYPE .
  data ENDDA type DATUM .
  data MSG_TYPE type SYMSGTY .
  data SPR_STATUS type /GDA/SDM_DE_SPRINT_STATUS .
  data EXTRA_V2 type SYMSGV .
  data EXTRA_V3 type SYMSGV .
  data EXTRA_V4 type SYMSGV .
  data EXTRA_V5 type SYMSGV .
  data EXTRA_V6 type SYMSGV .
private section.
ENDCLASS.



CLASS /GDA/CL_SDM_PERSISTENT_MAIN IMPLEMENTATION.


  method GET_BEGDA.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute BEGDA
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = BEGDA.

           " GET_BEGDA
  endmethod.


  method GET_ENDDA.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute ENDDA
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = ENDDA.

           " GET_ENDDA
  endmethod.


  method GET_EXTRA_V2.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute EXTRA_V2
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = EXTRA_V2.

           " GET_EXTRA_V2
  endmethod.


  method GET_EXTRA_V3.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute EXTRA_V3
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = EXTRA_V3.

           " GET_EXTRA_V3
  endmethod.


  method GET_EXTRA_V4.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute EXTRA_V4
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = EXTRA_V4.

           " GET_EXTRA_V4
  endmethod.


  method GET_EXTRA_V5.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute EXTRA_V5
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = EXTRA_V5.

           " GET_EXTRA_V5
  endmethod.


  method GET_EXTRA_V6.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute EXTRA_V6
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = EXTRA_V6.

           " GET_EXTRA_V6
  endmethod.


  method GET_FIELD.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute FIELD
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = FIELD.

           " GET_FIELD
  endmethod.


  method GET_MSG_ID.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute MSG_ID
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = MSG_ID.

           " GET_MSG_ID
  endmethod.


  method GET_MSG_NUMBER.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute MSG_NUMBER
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = MSG_NUMBER.

           " GET_MSG_NUMBER
  endmethod.


  method GET_MSG_TYPE.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute MSG_TYPE
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = MSG_TYPE.

           " GET_MSG_TYPE
  endmethod.


  method GET_SDM_OBJECT_ID.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute SDM_OBJECT_ID
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = SDM_OBJECT_ID.

           " GET_SDM_OBJECT_ID
  endmethod.


  method GET_SDM_TABKEY.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute SDM_TABKEY
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = SDM_TABKEY.

           " GET_SDM_TABKEY
  endmethod.


  method GET_SDM_TYPE.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute SDM_TYPE
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = SDM_TYPE.

           " GET_SDM_TYPE
  endmethod.


  method GET_SPR_STATUS.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute SPR_STATUS
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = SPR_STATUS.

           " GET_SPR_STATUS
  endmethod.


  method GET_TABNAME.
***BUILD 090501
     " returning RESULT
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Get Attribute TABNAME
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, result is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
************************************************************************

* * Inform class agent and handle exceptions
  state_read_access.

  result = TABNAME.

           " GET_TABNAME
  endmethod.


  method IF_OS_STATE~GET.
***BUILD 090501
     " returning result type ref to object
************************************************************************
* Purpose        : Get state.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* GENERATED: Do not modify
************************************************************************

  data: STATE_OBJECT type ref to CL_OS_STATE.

  create object STATE_OBJECT.
  call method STATE_OBJECT->SET_STATE_FROM_OBJECT( ME ).
  result = STATE_OBJECT.

  endmethod.


  method IF_OS_STATE~HANDLE_EXCEPTION.
***BUILD 090501
     " importing I_EXCEPTION type ref to IF_OS_EXCEPTION_INFO optional
     " importing I_EX_OS type ref to CX_OS_OBJECT_NOT_FOUND optional
************************************************************************
* Purpose        : Handles exceptions during attribute access.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : If an exception is raised during attribut access,
*                  this method is called and the exception is passed
*                  as a paramater. The default is to raise the exception
*                  again, so that the caller can handle the exception.
*                  But it is also possible to handle the exception
*                  here in the callee.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
* - 2000-08-02   : (SB)  OO Exceptions
************************************************************************
* Modify if you like
************************************************************************

  if i_ex_os is not initial.
    raise exception i_ex_os.
  endif.

  endmethod.


  method IF_OS_STATE~INIT.
***BUILD 090501
"#EC NEEDED
************************************************************************
* Purpose        : Initialisation of the transient state partition.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : Transient state is initial.
*
* OO Exceptions  : -
*
* Implementation : Caution!: Avoid Throwing ACCESS Events.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* Modify if you like
************************************************************************

  endmethod.


  method IF_OS_STATE~INVALIDATE.
***BUILD 090501
"#EC NEEDED
************************************************************************
* Purpose        : Do something before all persistent attributes are
*                  cleared.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : Whatever you like to do.
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* Modify if you like
************************************************************************

  endmethod.


  method IF_OS_STATE~SET.
***BUILD 090501
     " importing I_STATE type ref to object
************************************************************************
* Purpose        : Set state.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : -
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-07   : (BGR) Initial Version 2.0
************************************************************************
* GENERATED: Do not modify
************************************************************************

  data: STATE_OBJECT type ref to CL_OS_STATE.

  STATE_OBJECT ?= I_STATE.
  call method STATE_OBJECT->SET_OBJECT_FROM_STATE( ME ).

  endmethod.


  method SET_ENDDA.
***BUILD 090501
     " importing I_ENDDA
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute ENDDA
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_ENDDA <> ENDDA ).

    ENDDA = I_ENDDA.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_ENDDA <> ENDDA )

           " GET_ENDDA
  endmethod.


  method SET_EXTRA_V2.
***BUILD 090501
     " importing I_EXTRA_V2
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute EXTRA_V2
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_EXTRA_V2 <> EXTRA_V2 ).

    EXTRA_V2 = I_EXTRA_V2.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_EXTRA_V2 <> EXTRA_V2 )

           " GET_EXTRA_V2
  endmethod.


  method SET_EXTRA_V3.
***BUILD 090501
     " importing I_EXTRA_V3
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute EXTRA_V3
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_EXTRA_V3 <> EXTRA_V3 ).

    EXTRA_V3 = I_EXTRA_V3.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_EXTRA_V3 <> EXTRA_V3 )

           " GET_EXTRA_V3
  endmethod.


  method SET_EXTRA_V4.
***BUILD 090501
     " importing I_EXTRA_V4
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute EXTRA_V4
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_EXTRA_V4 <> EXTRA_V4 ).

    EXTRA_V4 = I_EXTRA_V4.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_EXTRA_V4 <> EXTRA_V4 )

           " GET_EXTRA_V4
  endmethod.


  method SET_EXTRA_V5.
***BUILD 090501
     " importing I_EXTRA_V5
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute EXTRA_V5
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_EXTRA_V5 <> EXTRA_V5 ).

    EXTRA_V5 = I_EXTRA_V5.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_EXTRA_V5 <> EXTRA_V5 )

           " GET_EXTRA_V5
  endmethod.


  method SET_EXTRA_V6.
***BUILD 090501
     " importing I_EXTRA_V6
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute EXTRA_V6
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_EXTRA_V6 <> EXTRA_V6 ).

    EXTRA_V6 = I_EXTRA_V6.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_EXTRA_V6 <> EXTRA_V6 )

           " GET_EXTRA_V6
  endmethod.


  method SET_MSG_TYPE.
***BUILD 090501
     " importing I_MSG_TYPE
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute MSG_TYPE
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_MSG_TYPE <> MSG_TYPE ).

    MSG_TYPE = I_MSG_TYPE.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_MSG_TYPE <> MSG_TYPE )

           " GET_MSG_TYPE
  endmethod.


  method SET_SPR_STATUS.
***BUILD 090501
     " importing I_SPR_STATUS
     " raising CX_OS_OBJECT_NOT_FOUND
************************************************************************
* Purpose        : Set attribute SPR_STATUS
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : The object state is loaded, attribute is set
*
* OO Exceptions  : CX_OS_OBJECT_NOT_FOUND
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 2000-03-14   : (BGR) Version 2.0
* - 2000-07-28   : (SB)  OO Exceptions
* - 2000-10-04   : (SB)  Namespaces
************************************************************************

* * Inform class agent and handle exceptions
  state_write_access.

  if ( I_SPR_STATUS <> SPR_STATUS ).

    SPR_STATUS = I_SPR_STATUS.

*   * Inform class agent and handle exceptions
    state_changed.

  endif. "( I_SPR_STATUS <> SPR_STATUS )

           " GET_SPR_STATUS
  endmethod.
ENDCLASS.
