*&---------------------------------------------------------------------*
*& Report  /GDA/SDM_STEWARDSHIP_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT /GDA/SDM_STEWARDSHIP_TEST.

DATA:
  go_stwd         TYPE REF TO /gda/sdm_cl_stwd,
  go_stwd_factory TYPE REF TO /gda/sdm_if_stwd_factory.

PARAMETERS:
    pa_vers TYPE /GDA/SDM_DE_STWD_VERSION DEFAULT '001'.

START-OF-SELECTION.
  " create factory
  CREATE OBJECT go_stwd_factory TYPE /gda/sdm_cl_stwd_factory.

  " create stweardship Object at runtime.
  go_stwd = go_stwd_factory->create( pa_vers ).
  go_stwd->write( ).
