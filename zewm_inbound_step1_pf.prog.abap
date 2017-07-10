*&--------------------------------------------------------------------*
*& Report ZEWM_INBOUND_STEP1
*&---------------------------------------------------------------------*
*&   Inbound Process Step 1
*&    - Inbound Pack  (Create HU and pack partial quantity)
*&    - Goods Receipt (HU)
*&---------------------------------------------------------------------*
REPORT zewm_inbound_step1_pf.

DATA lo_message_box  TYPE REF TO /scdl/cl_sp_message_box       ##NEEDED.
DATA lv_error        TYPE abap_bool                            ##NEEDED.
DATA lo_prd_mgmt     TYPE REF TO /scwm/cl_dlv_management_prd   ##NEEDED.
DATA lt_pdi_itm      TYPE /scwm/dlv_item_out_prd_tab           ##NEEDED.
DATA lo_message_gm   TYPE REF TO /scdl/cl_dm_message           ##NEEDED.
DATA lv_huident      TYPE /scwm/huident                        ##NEEDED.
SELECTION-SCREEN BEGIN OF BLOCK f1 WITH FRAME TITLE lv_title.
PARAMETERS:
  p_lgnum  TYPE /scwm/lgnum       MEMORY ID /scwm/lgn OBLIGATORY,
  p_docno  TYPE /scdl/dl_docno_int                    OBLIGATORY,
  p_docitm TYPE /scdl/dl_itemno,
  p_pmat   TYPE /scwm/de_pmat     DEFAULT 'Z901-PACK-DUMMY',
  p_qty    TYPE /scwm/de_ui_quan,
  p_uom    TYPE /scwm/de_unit.
SELECTION-SCREEN END OF BLOCK f1.

INITIALIZATION.
   lv_title = '明細指定＝明細単位の部分入庫可能/明細未指定＝全明細一括処理'.

AT SELECTION-SCREEN.
  IF p_docitm IS INITIAL.
    IF p_qty IS NOT INITIAL OR p_uom IS NOT INITIAL.
      MESSAGE e100(/scwm/delivery) WITH '明細を指定しない場合は、数量、単位は指定できません'.
    ENDIF.
  ENDIF.


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
* Goods Receipts
*   - Pack to HU
*   - Post Goods Movement (Goods Receipts)
*--------------------------------------------------------------------------
  PERFORM gr_process USING lt_pdi_itm
                           p_lgnum
                           p_pmat
                           p_qty
                           p_uom
                  CHANGING lv_huident.


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


FORM gr_process  USING it_item    TYPE /scwm/dlv_item_out_prd_tab
                       iv_lgnum   TYPE /scwm/lgnum
*                       is_item    TYPE /scwm/dlv_item_out_prd_str
                       iv_mat     TYPE /scwm/de_pmat
                       iv_qty     TYPE /scwm/de_ui_quan
                       iv_uom     TYPE /scwm/de_unit
              CHANGING cv_huident TYPE /scwm/huident.
  DATA lt_docid        TYPE /scwm/tt_docid.
  DATA lv_matid        TYPE /scwm/de_matid.
  DATA ls_huhdr        TYPE /scwm/s_huhdr_int.
  DATA ls_material     TYPE /scwm/s_pack_stock.
  DATA ls_quantity     TYPE /scwm/s_quan.
  DATA lt_hu           TYPE /scwm/t_gm_hu.
  DATA ls_hu           TYPE /scwm/s_gm_hu.

  DATA lt_messages    TYPE /scdl/dm_message_tab.


  DATA lt_dlv         TYPE /scwm/dlv_docid_item_tab.
  DATA ls_dlv         TYPE /scwm/dlv_docid_item_str.
*--------------------------------------------------------------------------
* Instantiate Packing (/SCWM/CL_DLV_PACK_IBDL)
* the /SCWM/CL_DLV_PACK_IBDL should only be used for stocks that are not
* GR yet (‘planned stock’).
* After the GR has been posted, the class /SCWM/CL_WM_PACKING has to be used.
*--------------------------------------------------------------------------
  DATA lv_datum TYPE dats.
  DATA lv_index TYPE sy-index.

*--------------------------------------------------------------------------
* Get Material ID from Material Number
*--------------------------------------------------------------------------
  PERFORM get_matid_by_no USING iv_mat
                       CHANGING lv_matid.

  CLEAR lt_hu.
  READ TABLE it_item ASSIGNING FIELD-SYMBOL(<ls_item>) INDEX 1.
  lt_docid = VALUE #( ( docid = <ls_item>-docid ) ) .
  ls_dlv-docid = <ls_item>-docid.
  DATA(lo_ibdl_pack) = NEW /scwm/cl_dlv_pack_ibdl( ) ##NEEDED.
  lo_ibdl_pack->init( EXPORTING iv_lgnum = iv_lgnum
                                iv_doccat = /scdl/if_dl_doc_c=>sc_doccat_inb_prd
                                it_docid  = lt_docid ).
  DATA lv_cnt TYPE i.
  LOOP AT it_item ASSIGNING <ls_item>.

*--------------------------------------------------------------------------
* Create HU
*--------------------------------------------------------------------------
    ls_huhdr = lo_ibdl_pack->/scwm/if_pack_bas~create_hu(
                                EXPORTING iv_pmat = lv_matid ).
*--------------------------------------------------------------------------
* Pack to HU (Partial Quantity)
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
      RETURN.
    ENDIF.


    lo_ibdl_pack->get_hu_item( EXPORTING iv_guid_hu = ls_huhdr-guid_hu
                               IMPORTING es_huitm   = DATA(ls_huitm)
                                         et_huitm   = DATA(lt_huitm) ).
    ADD 1 TO lv_index.
    READ TABLE lt_huitm INTO DATA(ls_upd_huitm) INDEX lv_index.

    ls_upd_huitm-vfdat = lv_datum + 60.
    lo_ibdl_pack->/scwm/if_pack_bas~change_huitm( EXPORTING is_huitm = ls_upd_huitm
                                                  IMPORTING es_huitm = DATA(ls_c_huitm) ).
    CLEAR ls_upd_huitm.

    cv_huident = ls_huhdr-huident.
*--------------------------------------------------------------------------
* Goods Receipts
*--------------------------------------------------------------------------
    CLEAR ls_hu.
    ls_hu-lgnum   = p_lgnum.
    ls_hu-guid_hu = ls_huhdr-guid_hu.
    APPEND ls_hu TO lt_hu.

  ENDLOOP.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
*--------------------------------------------------------------------------
* Set Data change: Commit will be triggered after GR for this example
*--------------------------------------------------------------------------
  lo_ibdl_pack->set_data_changed( ).
  lo_ibdl_pack->save( iv_commit = abap_false ).

*--------------------------------------------------------------------------
* Goods Receipts
*--------------------------------------------------------------------------
  /scwm/cl_goods_movement=>/scwm/if_gm_dlv~post_hu(
          EXPORTING it_hu        = lt_hu
                    iv_gmcat     = /scwm/if_docflow_c=>sc_gr
          IMPORTING eo_message   = lo_message_gm ).

  IF lo_message_gm IS BOUND.
    CLEAR lt_messages.
    lt_messages = lo_message_gm->get_messages( ).
    PERFORM check_messages USING lt_messages
                        CHANGING lv_error.
    IF lv_error = abap_true.
      MESSAGE s100(/scwm/delivery) WITH '入庫処理が失敗しました' DISPLAY LIKE 'E'.
      /scwm/cl_tm=>cleanup( iv_reason = /scdl/cl_sp=>/scdl/if_sp1_transaction~sc_cleanup_rollback ).
      RETURN.
    ENDIF.
  ENDIF.
  MESSAGE s100(/scwm/delivery) WITH '入庫が完了しました（荷役単位：' lv_huident ')'.
  lo_prd_mgmt->save( EXPORTING iv_do_commit_work = abap_true
                               iv_wait           = abap_true ).
  /scwm/cl_tm=>cleanup( iv_reason = /scdl/cl_sp=>/scdl/if_sp1_transaction~sc_cleanup_end ).

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
