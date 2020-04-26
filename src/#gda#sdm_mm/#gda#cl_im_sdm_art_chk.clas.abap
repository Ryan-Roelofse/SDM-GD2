class /GDA/CL_IM_SDM_ART_CHK definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_MATERIAL_CHECK .
protected section.
private section.
ENDCLASS.



CLASS /GDA/CL_IM_SDM_ART_CHK IMPLEMENTATION.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_MARA_MEINS.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_PMATA.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_DATA.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_DATA_RETAIL.
    include /GDA/SDM_MM_ART_VALIDATION.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_MASS_MARC_DATA.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~FRE_SUPPRESS_MARC_CHECK.
  endmethod.
ENDCLASS.
