class /GDA/CA_SDM_PERSISTENT_MAIN definition
  public
  inheriting from /GDA/CB_SDM_PERSISTENT_MAIN
  final
  create private .

public section.

  class-data AGENT type ref to /GDA/CA_SDM_PERSISTENT_MAIN read-only .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS /GDA/CA_SDM_PERSISTENT_MAIN IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
***BUILD 090501
************************************************************************
* Purpose        : Initialize the 'class'.
*
* Version        : 2.0
*
* Precondition   : -
*
* Postcondition  : Singleton is created.
*
* OO Exceptions  : -
*
* Implementation : -
*
************************************************************************
* Changelog:
* - 1999-09-20   : (OS) Initial Version
* - 2000-03-06   : (BGR) 2.0 modified REGISTER_CLASS_AGENT
************************************************************************
* GENERATED: Do not modify
************************************************************************

  create object AGENT.

  call method AGENT->REGISTER_CLASS_AGENT
    exporting
      I_CLASS_NAME          = '/GDA/CL_SDM_PERSISTENT_MAIN'
      I_CLASS_AGENT_NAME    = '/GDA/CA_SDM_PERSISTENT_MAIN'
      I_CLASS_GUID          = '00155D0A0C061EE99DC43FA00FDEE5B0'
      I_CLASS_AGENT_GUID    = '00155D0A0C061EE99DC43FA00FDFA5B0'
      I_AGENT               = AGENT
      I_STORAGE_LOCATION    = '/GDA/SDM_EXC_MAI'
      I_CLASS_AGENT_VERSION = '2.0'.

           "CLASS_CONSTRUCTOR
  endmethod.
ENDCLASS.
