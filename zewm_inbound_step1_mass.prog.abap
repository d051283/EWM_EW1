*&--------------------------------------------------------------------*
*& Report ZEWM_INBOUND_STEP1
*&---------------------------------------------------------------------*
*&   Inbound Process Step 1
*&    - Inbound Pack  (Create HU and pack partial quantity)
*&---------------------------------------------------------------------*
REPORT zewm_inbound_step1_mass.

DATA lo_message_box  TYPE REF TO /scdl/cl_sp_message_box       ##NEEDED.
DATA lv_error        TYPE abap_bool                            ##NEEDED.
DATA lo_prd_mgmt     TYPE REF TO /scwm/cl_dlv_management_prd   ##NEEDED.
DATA lt_pdi_itm      TYPE /scwm/dlv_item_out_prd_tab           ##NEEDED.
DATA lo_message_gm   TYPE REF TO /scdl/cl_dm_message           ##NEEDED.
DATA lv_huident      TYPE /scwm/huident                        ##NEEDED.
DATA lv_index        TYPE sy-index.

PARAMETERS:
  p_lgnum  TYPE /scwm/lgnum       MEMORY ID /scwm/lgn OBLIGATORY,
  p_docno  TYPE /scdl/dl_docno_int                    OBLIGATORY,
  p_docitm TYPE /scdl/dl_itemno   NO-DISPLAY,
  p_pmat   TYPE /scwm/de_pmat     DEFAULT 'Z901-PACK-DUMMY',
  p_qty    TYPE /scwm/de_ui_quan  NO-DISPLAY,
  p_uom    TYPE /scwm/de_unit     NO-DISPLAY.


START-OF-SELECTION.
*--------------------------------------------------------------------------
* Set Warehouse Number
*--------------------------------------------------------------------------
  /scwm/cl_tm=>set_lgnum( iv_lgnum = p_lgnum ).

*--------------------------------------------------------------------------
* Query Inbound Delivery
*--------------------------------------------------------------------------
* Service Provider (Instantiation)
  DATA(lo_pdi_sp) = NEW /scdl/cl_sp_prd_out(
                            io_message_box = lo_message_box
                            iv_mode        = /scdl/cl_sp=>sc_mode_classic
                            iv_doccat      = /scdl/if_dl_doc_c=>sc_doccat_inb_prd ) ##NEEDED.

* Read Inbound Delivery item which should be adjusted & lock the item.
  PERFORM query_pdi  USING p_lgnum
                           p_docno
                           p_docitm
                  CHANGING lt_pdi_itm
                           lo_message_box
                           lo_prd_mgmt
                           lv_error.

*--------------------------------------------------------------------------
* Pack to Dummy HU for the whole document (all items)
*--------------------------------------------------------------------------
  PERFORM pack_process USING lt_pdi_itm
                             p_lgnum
                             p_pmat
                             p_qty
                             p_uom
                    CHANGING lv_error
                             lv_index.


  IF lv_error = abap_false.
    MESSAGE s100(/scwm/delivery) WITH 'ダミー荷役への梱包が完了しました。荷役数：' lv_index.
    lo_prd_mgmt->save( EXPORTING iv_do_commit_work = abap_true
                                 iv_wait           = abap_true ).
    /scwm/cl_tm=>cleanup( iv_reason = /scdl/cl_sp=>/scdl/if_sp1_transaction~sc_cleanup_end ).
  ELSE.
    ROLLBACK WORK.
    /scwm/cl_tm=>cleanup( iv_reason = /scdl/cl_sp=>/scdl/if_sp1_transaction~sc_cleanup_rollback ).
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  CHECK_MESSAGES
*&---------------------------------------------------------------------*
FORM check_messages
  USING    it_messages TYPE /scdl/dm_message_tab
  CHANGING cv_error    TYPE abap_bool.

  LOOP AT it_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
    IF <ls_messages>-msgty = wmegc_severity_err.
      cv_error = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_MATID_BY_NO
*&---------------------------------------------------------------------*
FORM get_matid_by_no
  USING    iv_mat   TYPE /scwm/de_pmat
  CHANGING cv_matid TYPE /scwm/de_matid.

  DATA ls_matnr TYPE /scmb/mdl_matnr_str.
  DATA ls_matid TYPE /scmb/mdl_matid_str.

  CLEAR cv_matid.
  CHECK iv_mat IS NOT INITIAL.

  ls_matnr-matnr = iv_mat.
  TRY.
      CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
        EXPORTING
          is_key  = ls_matnr
        IMPORTING
          es_data = ls_matid.

      cv_matid = ls_matid-matid.

    CATCH /scmb/cx_mdl ##NO_HANDLER.
      "No handling
  ENDTRY.

ENDFORM.


FORM pack_process  USING it_item    TYPE /scwm/dlv_item_out_prd_tab
                         iv_lgnum   TYPE /scwm/lgnum
                         iv_mat     TYPE /scwm/de_pmat
                         iv_qty     TYPE /scwm/de_ui_quan
                         iv_uom     TYPE /scwm/de_unit
                CHANGING cv_error   TYPE boole_d
                         cv_index   TYPE sy-index.
  DATA lt_docid        TYPE /scwm/tt_docid.
  DATA lv_matid        TYPE /scwm/de_matid.
  DATA ls_huhdr        TYPE /scwm/s_huhdr_int.
  DATA ls_material     TYPE /scwm/s_pack_stock.
  DATA ls_quantity     TYPE /scwm/s_quan.
*--------------------------------------------------------------------------
* Instantiate Packing (/SCWM/CL_DLV_PACK_IBDL)
* the /SCWM/CL_DLV_PACK_IBDL should only be used for stocks that are not
* GR yet (‘planned stock’).
* After the GR has been posted, the class /SCWM/CL_WM_PACKING has to be used.
*--------------------------------------------------------------------------
  DATA lv_datum TYPE dats.
  lv_datum = sy-datum + 60.
*--------------------------------------------------------------------------
* Get Material ID from Material Number
*--------------------------------------------------------------------------
  PERFORM get_matid_by_no USING iv_mat
                       CHANGING lv_matid.


  READ TABLE it_item ASSIGNING FIELD-SYMBOL(<ls_item>) INDEX 1.
  lt_docid = VALUE #( ( docid = <ls_item>-docid ) ) .
  DATA(lo_ibdl_pack) = NEW /scwm/cl_dlv_pack_ibdl( ) ##NEEDED.
  lo_ibdl_pack->init( EXPORTING iv_lgnum = iv_lgnum
                                iv_doccat = /scdl/if_dl_doc_c=>sc_doccat_inb_prd
                                it_docid  = lt_docid ).

  LOOP AT it_item ASSIGNING <ls_item>.

*--------------------------------------------------------------------------
* Create HU
*--------------------------------------------------------------------------
    ls_huhdr = lo_ibdl_pack->/scwm/if_pack_bas~create_hu(
                                EXPORTING iv_pmat = lv_matid ).
*--------------------------------------------------------------------------
* Pack to HU (Repack to Dummy HU)
*--------------------------------------------------------------------------
    ls_material-qdoccat = <ls_item>-doccat.
    ls_material-qdocid  = <ls_item>-docid.
    ls_material-qitmid  = <ls_item>-itemid.

    IF iv_qty IS NOT INITIAL  AND iv_uom IS NOT INITIAL.
      ls_quantity-quan   = iv_qty.
      ls_quantity-unit   = iv_uom.
    ELSE.
      ls_quantity-quan   = <ls_item>-qty-qty."iv_qty.
      ls_quantity-unit   = <ls_item>-qty-uom."iv_uom.
    ENDIF.


    lo_ibdl_pack->/scwm/if_pack_bas~pack_stock(
                  EXPORTING iv_dest_hu  = ls_huhdr-guid_hu
                            is_material = ls_material
                            is_quantity = ls_quantity
                  EXCEPTIONS error  = 1
                             OTHERS = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE wmegc_severity_suc NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE wmegc_severity_err.
      cv_error = abap_true.
      RETURN.
    ENDIF.


    lo_ibdl_pack->get_hu_item( EXPORTING iv_guid_hu = ls_huhdr-guid_hu
                               IMPORTING es_huitm   = DATA(ls_huitm)
                                         et_huitm   = DATA(lt_huitm) ).
    ADD 1 TO cv_index.
    READ TABLE lt_huitm INTO DATA(ls_upd_huitm) INDEX lv_index.

    ls_upd_huitm-vfdat = lv_datum.

    TRY.
        lo_ibdl_pack->/scwm/if_pack_bas~change_huitm( EXPORTING is_huitm = ls_upd_huitm
                                                      IMPORTING es_huitm = DATA(ls_c_huitm) ).
      CATCH /scwm/cx_basics .                           "#EC NO_HANDLER
        MESSAGE ID sy-msgid TYPE wmegc_severity_suc NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   DISPLAY LIKE wmegc_severity_err.
        cv_error = abap_true.
    ENDTRY.
    CLEAR ls_upd_huitm.

  ENDLOOP.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
*--------------------------------------------------------------------------
* Set Data change (for VFDAT) and Save
*--------------------------------------------------------------------------
  lo_ibdl_pack->set_data_changed( ).
  lo_ibdl_pack->save( iv_commit = abap_false ).


ENDFORM.




FORM query_pdi  USING iv_lgnum       TYPE /scwm/lgnum
                      iv_docno       TYPE /scdl/dl_docno
                      iv_itemno      TYPE /scdl/dl_itemno
             CHANGING ct_itm         TYPE /scwm/dlv_item_out_prd_tab
                      co_message_box TYPE REF TO /scdl/cl_sp_message_box
                      co_prd_mgmt    TYPE REF TO /scwm/cl_dlv_management_prd
                      cv_error       TYPE flag.

*  /scwm/cl_dlv_management_prd=>get_instance( RECEIVING eo_instance = lo_prd_mgmt ).
  DATA lt_docno        TYPE /scwm/dlv_docno_itemno_tab.
  DATA ls_read_options TYPE /scwm/dlv_query_contr_str.
  DATA lo_message_dlv  TYPE REF TO /scwm/cl_dm_message_no.
  DATA lt_messages     TYPE /scdl/dm_message_tab.
  DATA lx_dlv_query    TYPE REF TO /scdl/cx_delivery.
  DATA lt_selection    TYPE /scwm/dlv_selection_tab.
* Get instance
  co_prd_mgmt = /scwm/cl_dlv_management_prd=>get_instance( ).

* Fill selection table to select by Inbound Delivery Order number
  IF iv_itemno = space.
    lt_selection    = VALUE #( ( fieldname = 'DOCNO_H' sign = 'I' option = 'EQ' low = iv_docno ) ).
  ELSE.
    lt_docno      = VALUE #( ( docno  = iv_docno
                               itemno = iv_itemno
                               doccat = /scdl/if_dl_doc_c=>sc_doccat_inb_prd ) ).
  ENDIF.
  " read options
**  ls_read_options = VALUE #( item_part_select = abap_true lock_result = abap_true ).
  ls_read_options = VALUE #( data_retrival_only      = abap_true
                             mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance ).

  TRY.
      co_prd_mgmt->query(
        EXPORTING
          iv_whno         = iv_lgnum
          iv_doccat       = /scdl/if_dl_doc_c=>sc_doccat_inb_prd
          it_docno        = lt_docno
          it_selection    = lt_selection
          is_read_options = ls_read_options
        IMPORTING
          eo_message      = lo_message_dlv
          et_items        = ct_itm
      ).
    CATCH /scdl/cx_delivery INTO lx_dlv_query.
      lo_message_box->add_exception( EXPORTING io_exception = lx_dlv_query ).
      lt_messages = co_message_box->get_messages( ).
      PERFORM check_messages USING lt_messages
                          CHANGING cv_error.
  ENDTRY.
  lt_messages = lo_message_dlv->get_messages( ).
  PERFORM check_messages USING lt_messages
                      CHANGING cv_error.

ENDFORM.
