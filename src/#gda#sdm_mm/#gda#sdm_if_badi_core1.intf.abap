interface /GDA/SDM_IF_BADI_CORE1
  public .


  interfaces IF_BADI_INTERFACE .

  methods ADD_SDM_TYPE
    importing
      !X_SOURCE type /GDA/SDM_DE_SOURCE
    changing
      !XT_SDM_TYPE type /GDA/SDM_R_SDM_TYPE
    exceptions
      APPLICATION_ERROR .
endinterface.
