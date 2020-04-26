interface /GDA/SDM_IF_BADI_CORE2
  public .


  interfaces IF_BADI_INTERFACE .

  methods CONTEXT_MENU_CHANGE
    importing
      !XO_CORE type ref to /GDA/SDM_CL_CORE
    changing
      !XO_ACTION type ref to /GDA/SDM_CL_ACTIONS
    exceptions
      APPLICATION_ERROR .
endinterface.
