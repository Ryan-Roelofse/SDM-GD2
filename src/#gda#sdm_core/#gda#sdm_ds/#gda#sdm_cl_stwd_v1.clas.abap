class /GDA/SDM_CL_STWD_V1 definition
  public
  inheriting from /GDA/SDM_CL_STWD
  final
  create public .

public section.

  methods WRITE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_STWD_V1 IMPLEMENTATION.


  method WRITE.
*CALL METHOD SUPER->WRITE
*    .

write: 'SDM Stewardship Version 001'.
  endmethod.
ENDCLASS.
