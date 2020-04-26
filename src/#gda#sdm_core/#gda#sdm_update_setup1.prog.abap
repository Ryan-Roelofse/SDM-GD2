*&---------------------------------------------------------------------*
*& Report /GDA/SDM_UPDATE_SETUP1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /gda/sdm_update_setup1.

CONSTANTS:
*  lv_function_id TYPE if_fdt_types=>id VALUE '0019BBC7F1261ED59CCE05BC8B6FC0F1',
  lc_app_name  TYPE fdt_name         VALUE '/GDA/SDM_MM_ARTICLE_MASTER',
  lc_func_name TYPE fdt_name         VALUE 'FN_VALIDATE_ARTICLE'.

DATA:
  gt_brf_attributes TYPE STANDARD TABLE OF /gda/sdm_setup1,
  lv_guid           TYPE if_fdt_types=>id,
  lv_app_id         TYPE if_fdt_types=>id,
  lv_do_id          TYPE if_fdt_types=>id.


FIELD-SYMBOLS:
  <setup> LIKE LINE OF gt_brf_attributes.



START-OF-SELECTION.

* Get application ID
  SELECT SINGLE id FROM fdt_admn_0000
                  INTO lv_app_id
                    WHERE object_type = 'AP'
                    AND   name        = lc_app_name
                    AND   deleted     = space.

  SELECT * FROM /gda/sdm_setup1 INTO CORRESPONDING FIELDS OF TABLE gt_brf_attributes
    WHERE sdm_object = 'ARTICLE' "iv_object_type
     AND  active     = abap_true
     AND  type       =  '5'.  "'3'. "'1'.       "iv_type.

* update GUUID
  LOOP AT gt_brf_attributes ASSIGNING <setup>.

    SELECT SINGLE id FROM fdt_admn_0000
                    INTO lv_do_id
                      WHERE object_type    = 'DO'
                      AND   name           = <setup>-name
                      AND   application_id = lv_app_id
                      AND   deleted        = space.


*    <setup>-data_object = lv_do_id.

  ENDLOOP.


  MODIFY /gda/sdm_setup1 FROM TABLE gt_brf_attributes.

  if sy-subrc = 0.
    write:/ 'All ok'.
  endif.
