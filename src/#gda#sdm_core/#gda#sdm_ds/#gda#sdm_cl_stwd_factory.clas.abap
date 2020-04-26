class /GDA/SDM_CL_STWD_FACTORY definition
  public
  final
  create public .

public section.

  interfaces /GDA/SDM_IF_STWD_FACTORY .
protected section.
private section.
ENDCLASS.



CLASS /GDA/SDM_CL_STWD_FACTORY IMPLEMENTATION.


  method /GDA/SDM_IF_STWD_FACTORY~CREATE.
    DATA:
     lo_stwd TYPE REF TO /gda/sdm_cl_stwd.
    CASE IV_VERSION.
      WHEN '001'.
        CREATE OBJECT lo_stwd TYPE /GDA/SDM_CL_STWD_V1.
      WHEN '002'.
*        CREATE OBJECT lo_stwd TYPE zcl_order.
      WHEN OTHERS.
        " default, create order
*        CREATE OBJECT lo_doc TYPE zcl_order.
    ENDCASE.

    ro_object = lo_stwd.
  endmethod.
ENDCLASS.
