*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_POE_VALIDATION_CORE
*&---------------------------------------------------------------------*
 DATA:
   lo_data_empty TYPE REF TO data,
   lo_data       TYPE REF TO data.

 FIELD-SYMBOLS:
   <results_temp> LIKE gt_results.

* Get default SDM Type and include Custom SDM Types, if any
 gr_sdm_type = /gda/sdm_cl_common_core=>get_sdm_type( x_sdm_type   = gv_type
                                                      x_sdm_source = gc_poe ).

* Get SDM objects for given object
 gt_objects = /gda/sdm_cl_common_core=>get_sdm_objects( x_sdm_obect  = gv_object
                                                        xt_sdm_types = gr_sdm_type ).

*CHECK gt_objects[] IS NOT INITIAL.

 LOOP AT gt_objects ASSIGNING <objects>.
   TRY.
       <objects>-object ?= /gda/sdm_cl_common_core=>sdm_initialise(  iv_object_type = gv_object
                                                                    iv_source       = gc_poe
                                                                    iv_type         = <objects>-type
                                                                    iv_stats        = abap_false ).
     CATCH /gda/cx_sdm_exception_handl.
       CONTINUE.
   ENDTRY.

   gt_attributes = <objects>-object->get_object_attributes( iv_type = <objects>-type   ).

   LOOP AT gt_attributes ASSIGNING <attribute>.
     ASSIGN (<attribute>-abap_type) TO <set_data>.
     <objects>-object->set_selection( iv_name = <attribute>-name
                                      iv_data = <set_data>
                                      iv_type = <attribute>-type ).
   ENDLOOP.

* Process data
   TRY.
       /gda/sdm_cl_common_core=>sdm_process_data( xo_object = <objects>-object ).
     CATCH /gda/cx_sdm_exception_handl.
       CONTINUE.
   ENDTRY.

* Process results
   lo_data = <objects>-object->return_brf_result( ).
   ASSIGN lo_data->* TO <results_temp>.

   IF <results> IS NOT ASSIGNED.
     lo_data_empty  = <objects>-object->return_brf_result_structure( ).
     ASSIGN lo_data_empty->* TO <results>.
     REFRESH:
      <results>.
   ENDIF.
   APPEND LINES OF <results_temp> TO <results>.

* Retrieve results
*  TRY.
*     ASSIGN gt_results to <results>.
*      <results> = /gda/sdm_cl_common_core=>sdm_retrieve_data( xo_object = <objects>-object  ).
*    CATCH /gda/cx_sdm_exception_handl.
*      CONTINUE.
*  ENDTRY.

 ENDLOOP.
