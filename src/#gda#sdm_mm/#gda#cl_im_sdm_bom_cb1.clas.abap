class /GDA/CL_IM_SDM_BOM_CB1 definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BOM_UPDATE .
protected section.
private section.
ENDCLASS.



CLASS /GDA/CL_IM_SDM_BOM_CB1 IMPLEMENTATION.


  method IF_EX_BOM_UPDATE~CHANGE_AT_SAVE.
*    include /GDA/SDM_MM_POE_BOM_VALIDATION.
  endmethod.


  method IF_EX_BOM_UPDATE~CHANGE_BEFORE_UPDATE.
  endmethod.


  method IF_EX_BOM_UPDATE~CHANGE_IN_UPDATE.
  endmethod.


  method IF_EX_BOM_UPDATE~CREATE_TREX_CPOINTER.
  endmethod.
ENDCLASS.
