class /GDA/SDM_CL_MESSAGE_LOG definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !X_OUTPUT_MEDIUM type CHAR10 optional
      !X_PERSISTENT type CHAR1 optional .
  methods ADD_MESSAGE
    importing
      !X_PRIORITY type BALPROBCL
      !X_TYPE type SYMSGTY
      !X_CLASS type SYMSGID
      !X_NUMBER type SYMSGNO
      !X_MSGV1 type C
      !X_MSGV2 type C
      !X_MSGV3 type C
      !X_MSGV4 type C
      !X_WRKEY type CHAR10 .
  methods GET_MESSAGES
    returning
      value(RT_MESSAGES) type /GDA/T_OBJ_MESSAGE .
  methods INITIALISE .
  methods OUTPUT_TO_SPOOL .
  methods USER_MESSAGE_EXISTS .
  methods OUTPUT_USER_MESSAGES .
  methods REFRESH .
  methods GET_MESSAGE_AS_STRING .
  methods ADD_MESSAGE_OBJECT .
  methods HAS_USER_MESSAGES
    returning
      value(R_HAS_MESSAGES) type BOOLE-BOOLE .
  methods OUTPUT_MESSAGES_ALV .
  methods ADD_MESSAGES .
protected section.
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_MESSAGE_LOG IMPLEMENTATION.


  method ADD_MESSAGE.
  endmethod.


  method ADD_MESSAGES.
  endmethod.


  method ADD_MESSAGE_OBJECT.
  endmethod.


  method CONSTRUCTOR.
  endmethod.


  method GET_MESSAGES.
  endmethod.


  method GET_MESSAGE_AS_STRING.
  endmethod.


  method HAS_USER_MESSAGES.
  endmethod.


  method INITIALISE.
  endmethod.


  method OUTPUT_MESSAGES_ALV.
  endmethod.


  method OUTPUT_TO_SPOOL.
  endmethod.


  method OUTPUT_USER_MESSAGES.
  endmethod.


  method REFRESH.
  endmethod.


  method USER_MESSAGE_EXISTS.
  endmethod.
ENDCLASS.
