*&---------------------------------------------------------------------*
*&  Include  /GDA/SDM_POE_SCUST_VALIDATION
*&---------------------------------------------------------------------*

* Prerequisites
*1) Class Creation
* Create a class for the the object with /GDA/SDM_CL_CORE as the super class
* Naming convention /GDA/SDM_CL_(OBJECT_NAME)
* Description GlueData:SDM (Object Name)
* /GDA/SDM_CL_SCUSTOM
*2) Config Setup(Liase with a functional consultant)
*   SPRO->GlueData:Simple Data Management
*                 ->Client Setup
*                 ->Object Setup
*                 ->Type Setup
*                 ->Linkage Setup
*                 ->Structure Setup

*3) License Key

INCLUDE /gda/sdm_scust_data.
INCLUDE /gda/sdm_scust_obj_data.

* Move required structures from SAP standard to corresponding SDM structures
MOVE-CORRESPONDING scustom TO gs_scustom_sdm.
APPEND gs_scustom_sdm TO gt_scustom_sdm.
CLEAR gs_scustom_sdm.

gv_type   =  gc_der.
gv_object =  gc_scustom.

INCLUDE /gda/sdm_poe_core.
