class /GDA/SDM_CL_MESSAGE definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !X_MSGID type SY-MSGID
      !X_MSGTY type SY-MSGTY
      !X_MSGNO type SY-MSGNO
      !X_MSGV1 type SY-MSGV1
      !X_MSGV2 type SY-MSGV2
      !X_MSGV3 type SY-MSGV3
      !X_MSGV4 type SY-MSGV4
      !X_WRKEY type CHAR10 .
  methods GET_DATA
    exporting
      !Y_MSGID type SY-MSGID
      !Y_MSGTY type SY-MSGTY
      !Y_MSGNO type SY-MSGNO
      !Y_MSGV1 type SY-MSGV1
      !Y_MSGV2 type SY-MSGV2
      !Y_MSGV3 type SY-MSGV3
      !Y_MSGV4 type SY-MSGV4
      !Y_WRKEY type CHAR10 .
  methods OUTPUT .
protected section.

  data PV_MSGID type SY-MSGID .
  data PV_MSGTY type SY-MSGTY .
  data PV_MSGNO type SY-MSGNO .
  data PV_MSGV1 type SY-MSGV1 .
  data PV_MSGV2 type SY-MSGV2 .
  data PV_MSGV3 type SY-MSGV3 .
  data PV_MSGV4 type SY-MSGV4 .
  data PV_WRKEY type CHAR10 .
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_MESSAGE IMPLEMENTATION.


  method CONSTRUCTOR.
   pv_msgid = x_msgid.
  pv_msgty = x_msgty.
  pv_msgno = x_msgno.
  pv_msgv1 = x_msgv1.
  pv_msgv2 = x_msgv2.
  pv_msgv3 = x_msgv3.
  pv_msgv4 = x_msgv4.
  pv_wrkey = x_wrkey.
  endmethod.


  method GET_DATA.
  y_msgid = pv_msgid.
  y_msgty = pv_msgty.
  y_msgno = pv_msgno.
  y_msgv1 = pv_msgv1.
  y_msgv2 = pv_msgv2.
  y_msgv3 = pv_msgv3.
  y_msgv4 = pv_msgv4.
  y_wrkey = pv_wrkey.
  endmethod.


  method OUTPUT.

* Output the message
  MESSAGE ID pv_msgid
     TYPE 'S'
   NUMBER pv_msgno
     WITH pv_msgv1
          pv_msgv2
          pv_msgv3
          pv_msgv4.
  endmethod.
ENDCLASS.
