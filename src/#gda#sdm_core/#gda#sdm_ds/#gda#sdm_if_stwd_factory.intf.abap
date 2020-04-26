interface /GDA/SDM_IF_STWD_FACTORY
  public .


  methods CREATE
    importing
      !IV_VERSION type /GDA/SDM_DE_STWD_VERSION
    returning
      value(RO_OBJECT) type ref to /GDA/SDM_CL_STWD .
endinterface.
