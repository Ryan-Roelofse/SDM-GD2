class /GDA/CX_SDM_EXCEPTION_HANDL definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  data MV_TEXT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MV_TEXT type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS /GDA/CX_SDM_EXCEPTION_HANDL IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->MV_TEXT = MV_TEXT .
  endmethod.
ENDCLASS.
