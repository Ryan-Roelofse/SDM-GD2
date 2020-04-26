class /GDA/SDM_CL_MAT_F_SPEC_SEL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_BADI_MAT_F_SPEC_SEL .
protected section.
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_MAT_F_SPEC_SEL IMPLEMENTATION.


  method IF_EX_BADI_MAT_F_SPEC_SEL~FIELD_SELECTION.
    include /GDA/SDM_MM_POI_MATERIAL_SCR.
  endmethod.
ENDCLASS.
