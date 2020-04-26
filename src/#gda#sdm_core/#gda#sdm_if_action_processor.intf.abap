interface /GDA/SDM_IF_ACTION_PROCESSOR
  public .


  methods PROCESS_ACTION
    importing
      !X_ACTION type ref to /GDA/SDM_CL_ACTIONS
      !X_MULTI type XFLAG
    exporting
      !Y_REFRESH type BOOLE-BOOLE
      !Y_ACTION_HANDLED type BOOLE-BOOLE
      !Y_NOT_AUTHORISED type BOOLE-BOOLE .
  methods SAVE_LOG
    importing
      !X_ACTION type ref to /GDA/SDM_CL_ACTIONS
      !X_NO_LOG type BOOLE optional .
endinterface.
