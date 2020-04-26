class /GDA/SDM_CL_IM_ART_CHK definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_MATERIAL_CHECK .
protected section.
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_IM_ART_CHK IMPLEMENTATION.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_MARA_MEINS.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_CHANGE_PMATA.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_DATA.
 ##NEEDED.
    include /GDA/SDM_MM_POI_MAT_VALIDATION.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_DATA_RETAIL.
    include /GDA/SDM_MM_POI_ART_VALIDATION.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~CHECK_MASS_MARC_DATA.
  endmethod.


  method IF_EX_BADI_MATERIAL_CHECK~FRE_SUPPRESS_MARC_CHECK.
  endmethod.
ENDCLASS.
