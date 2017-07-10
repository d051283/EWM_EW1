*&---------------------------------------------------------------------*
*& Report ZEWM_INBOUND_STEP2
*&---------------------------------------------------------------------*
*&   Inbound Process Step 2
*&    - Create Putaway Product Warehouse Task with Delivery Reference
*&    - Create Putaway HU Warehouse task with Stock Reference
*&---------------------------------------------------------------------*
REPORT zewm_inbound_step2_io.

DATA lt_pdi_itm     TYPE /scwm/dlv_item_out_prd_tab           ##NEEDED.
DATA lo_message_box TYPE REF TO /scdl/cl_sp_message_box       ##NEEDED.
DATA lo_message_dlv TYPE REF TO /scwm/cl_dm_message_no        ##NEEDED.
DATA lv_error       TYPE abap_bool                            ##NEEDED.
DATA lv_huident     TYPE /scwm/de_huident                     ##NEEDED.

PARAMETERS:
  p_lgnum  TYPE /scwm/lgnum          MEMORY ID /scwm/lgn    OBLIGATORY,
  p_ws     TYPE /scwm/de_workstation DEFAULT 'WC01',
  p_ibd    TYPE /scwm/sp_docno_pdi                          OBLIGATORY,
  p_ibditm TYPE /scdl/dl_itemno      DEFAULT 10,
  p_qty    TYPE /scdl/dl_quantity,
  p_uom    TYPE /scwm/de_unit,
  p_pmat   TYPE /scwm/de_pmat        DEFAULT 'Z901-PACK-DUMMY' OBLIGATORY,
  p_procty TYPE /scwm/de_procty      DEFAULT 'ZA01'            OBLIGATORY.


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

* Query Inbound Delivery
  PERFORM query_pdi USING p_lgnum
                          p_ibd
                          p_ibditm
                 CHANGING lt_pdi_itm
                          lo_message_box
                          lv_error.

*--------------------------------------------------------------------------
*   1. Putaway from GR Zone to Inbound Work Center(ZA2R-GR-ZONE --> ZA2R-WC01-IN1)
*   FM:/SCWM/TO_CREATE_WHR
*--------------------------------------------------------------------------
  IF lv_error = abap_false.
    READ TABLE lt_pdi_itm ASSIGNING FIELD-SYMBOL(<ls_item>) INDEX 1   ##NEEDED.
    PERFORM create_whr
      USING    p_lgnum
               p_ws
               p_qty
               p_pmat
               <ls_item>
      CHANGING lv_huident
               lv_error.
  ENDIF.
*--------------------------------------------------------------------------
*   2. Move Packed HU to OUT section of Inbound Work Center(ZA2R-WC01-IN1 --> ZA2R-WC01-OUT)
*   FM:/SCWM/TO_CREA_MOVE_HU
*--------------------------------------------------------------------------
  IF lv_error = abap_false.
    PERFORM create_hu_wt
      USING    p_lgnum
               p_ws
               lv_huident
               p_procty
      CHANGING lv_error.

  ENDIF.

  IF lv_error = abap_false.
    MESSAGE s100(/scwm/delivery) WITH '成功'.
    WRITE lv_huident.
  ELSE.
    MESSAGE s100(/scwm/delivery) WITH '失敗' DISPLAY LIKE 'E'.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  QUERY_PDO
*&---------------------------------------------------------------------*
FORM query_pdi
  USING    iv_lgnum       TYPE /scwm/lgnum
           iv_docno       TYPE /scwm/sp_docno_pdo
           iv_itemno      TYPE /scdl/dl_itemno
  CHANGING ct_itm         TYPE /scwm/dlv_item_out_prd_tab
           co_message_box TYPE REF TO /scdl/cl_sp_message_box
           cv_error       TYPE boole_d.

  DATA lo_prd_mgmt     TYPE REF TO /scwm/cl_dlv_management_prd.
  DATA ls_read_options TYPE /scwm/dlv_query_contr_str.
  DATA lt_docno        TYPE /scwm/dlv_docno_itemno_tab.
  DATA lt_messages     TYPE /scdl/dm_message_tab.

* Get instance
  lo_prd_mgmt = /scwm/cl_dlv_management_prd=>get_instance( ).

* Fill selection table to select by Inbound Delivery Order number
* & document item number
  lt_docno = VALUE #( ( docno  = iv_docno
                        itemno = iv_itemno
                        doccat = /scdl/if_dl_doc_c=>sc_doccat_inb_prd ) ).

* Set Read Options
  ls_read_options-item_part_select = abap_true.
  ls_read_options-lock_result      = abap_true.

  TRY.
      lo_prd_mgmt->query( EXPORTING it_docno        = lt_docno
                                    iv_doccat       = /scdl/if_dl_doc_c=>sc_doccat_inb_prd
                                    iv_whno         = iv_lgnum
                                    is_read_options = ls_read_options
                          IMPORTING et_items        = ct_itm
                                    eo_message      = lo_message_dlv ).
    CATCH /scdl/cx_delivery INTO DATA(lx_dlv_query).
      co_message_box->add_exception( EXPORTING io_exception = lx_dlv_query ).
      lt_messages = co_message_box->get_messages( ).
      PERFORM check_messages    USING lt_messages
                             CHANGING cv_error.
  ENDTRY.
  lt_messages = lo_message_dlv->get_messages( ).
  PERFORM check_messages    USING lt_messages
                         CHANGING cv_error.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_MESSAGES
*&---------------------------------------------------------------------*
FORM check_messages  USING    it_messages TYPE /scdl/dm_message_tab
                     CHANGING cv_error    TYPE abap_bool.

  cv_error = abap_false.

  LOOP AT it_messages ASSIGNING FIELD-SYMBOL(<fs_messages>).
    IF <fs_messages>-msgty = wmegc_severity_err.
      cv_error = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_WHR
*&---------------------------------------------------------------------*
FORM create_whr
  USING    iv_lgnum   TYPE /scwm/lgnum
           iv_ws      TYPE /scwm/de_workstation
           iv_qty     TYPE /scwm/rl03tanfme
           iv_pmat    TYPE /scwm/de_pmat
           is_item    TYPE /scwm/dlv_item_out_prd_str
  CHANGING ev_huident TYPE /scwm/de_huident
           ev_error   TYPE boole_d.

  DATA ls_ws         TYPE /scwm/tworkst.
  DATA ls_create_whr TYPE /scwm/s_to_prep_whr_int.
  DATA lt_create_whr TYPE /scwm/tt_to_prep_whr_int.
  DATA lv_severity   TYPE bapi_mtype.
  DATA lt_bapiret    TYPE bapirettab                 ##NEEDED.

* Destination Bin
  PERFORM get_dest_bin
    USING    iv_lgnum
             iv_ws
             'I'    " Inbound Section
    CHANGING ls_create_whr-nlpla
             ls_ws.
* Destination HU
  PERFORM create_empty_hu
    USING    iv_lgnum
             iv_pmat
             ls_create_whr-nlpla
             ls_ws
    CHANGING ev_huident.                 " Destination HU
  ls_create_whr-nlenr   = ev_huident.
  ls_create_whr-rdoccat = /scdl/if_dl_doc_c=>sc_doccat_inb_prd.
  ls_create_whr-rdocid  = is_item-docid.
  ls_create_whr-ritmid  = is_item-itemid.
  ls_create_whr-anfme   = iv_qty.
  ls_create_whr-altme   = is_item-qty-uom.
  APPEND ls_create_whr TO lt_create_whr.

  CALL FUNCTION '/SCWM/TO_CREATE_WHR'
    EXPORTING
      iv_lgnum      = iv_lgnum
      it_create_whr = lt_create_whr
      iv_squit      = abap_true
    IMPORTING
      et_bapiret    = lt_bapiret
      ev_severity   = lv_severity.

  IF lv_severity CA wmegc_severity_ea.
    ROLLBACK WORK.
    CALL METHOD /scwm/cl_tm=>cleanup( ).
    ev_error = abap_true.

  ELSE.
    COMMIT WORK AND WAIT.
    CALL METHOD /scwm/cl_tm=>cleanup( ).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DEST_BIN
*&---------------------------------------------------------------------*
FORM get_dest_bin
  USING    iv_lgnum   TYPE /scwm/lgnum
           iv_ws      TYPE /scwm/de_workstation
           iv_section TYPE char1
  CHANGING ev_nlpla   TYPE /scwm/ltap_nlpla
           es_ws      TYPE /scwm/tworkst.

  DATA lt_lagp        TYPE /scwm/tt_lagp.

* Destination Bin from Inbound Work Center
* a). Get WS Information
  CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
    EXPORTING
      iv_lgnum       = iv_lgnum
      iv_workstation = iv_ws
    IMPORTING
      es_workst      = es_ws.

* b). Get Bin of Inbound Section
  IF iv_section = 'I'.
    DATA(lt_lgber) = VALUE /scwm/tt_lgber_key( ( lgnum = iv_lgnum
                                                 lgtyp = es_ws-lgtyp
                                                 lgber = es_ws-i_section ) ).
  ELSE.
    lt_lgber       = VALUE /scwm/tt_lgber_key( ( lgnum = iv_lgnum
                                                 lgtyp = es_ws-lgtyp
                                                 lgber = es_ws-o_section ) ).
  ENDIF.

  CALL FUNCTION '/SCWM/LAGP_READ_LGBER'
    EXPORTING
      it_lgber = lt_lgber
    IMPORTING
      et_lagp  = lt_lagp.

  READ TABLE lt_lagp ASSIGNING FIELD-SYMBOL(<ls_lagp>) INDEX 1.
  ev_nlpla = <ls_lagp>-lgpla.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_EMPTY_HU
*&---------------------------------------------------------------------*
FORM create_empty_hu
  USING    iv_lgnum    TYPE /scwm/lgnum
           iv_pmat     TYPE /scwm/de_pmat
           iv_nlpla    TYPE /scwm/lgpla
           is_ws       TYPE /scwm/tworkst
  CHANGING ev_huident  TYPE /scwm/de_huident.

  DATA lo_sp_pack         TYPE REF TO /scwm/cl_wm_packing.
  DATA lo_sp_stock_fields TYPE REF TO /scwm/cl_ui_stock_fields.
  DATA lv_pmatid          TYPE /scwm/de_matid.

* Set Global Fields
  /scwm/cl_wm_packing=>set_global_fields( iv_lgnum = iv_lgnum ).

* Get packaging material id
  CREATE OBJECT lo_sp_stock_fields.
  CALL METHOD lo_sp_stock_fields->get_matid_by_no
    EXPORTING
      iv_matnr = iv_pmat
    RECEIVING
      ev_matid = lv_pmatid.

* Create Instance
  /scwm/cl_wm_packing=>get_instance(
    IMPORTING
      eo_instance = lo_sp_pack ).

* Initilize SP
  lo_sp_pack->init_by_workstation(
    EXPORTING
      is_workstation = is_ws ).

* Create HU
  lo_sp_pack->create_hu(
    EXPORTING
      iv_pmat    = lv_pmatid
      i_location = iv_nlpla
    RECEIVING
      es_huhdr   = DATA(ls_huhdr)
    EXCEPTIONS
      OTHERS     = 1 ).
  IF sy-subrc IS INITIAL.
    ev_huident = ls_huhdr-huident.
  ENDIF.

* Save
  lo_sp_pack->/scwm/if_pack_bas~save(
    EXPORTING
      iv_commit = abap_true
      iv_wait   = abap_true ).

* Final clean up
  /scwm/cl_tm=>cleanup( iv_lgnum = p_lgnum ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_HU_WT
*&---------------------------------------------------------------------*
FORM create_hu_wt
  USING    iv_lgnum   TYPE /scwm/lgnum
           iv_ws      TYPE /scwm/de_workstation
           iv_huident TYPE /scwm/de_huident
           iv_procty  TYPE /scwm/de_procty
  CHANGING ev_error   TYPE boole_d.

  DATA ls_ws       TYPE /scwm/tworkst.
  DATA lv_bin      TYPE /scwm/ltap_nlpla.
  DATA lt_crea_hu  TYPE /scwm/tt_to_crea_hu.
  DATA lv_severity TYPE bapi_mtype.
  DATA lt_bapiret  TYPE bapirettab           ##NEEDED.

* Destination Bin
  PERFORM get_dest_bin
    USING    iv_lgnum
             iv_ws
             'O'    " Outbound Section
    CHANGING lv_bin
             ls_ws.

* Create/Confirm Handling Unit Warehouse Task
  DATA(ls_crea_hu) = VALUE /scwm/s_to_crea_hu( huident = iv_huident
                                               squit   = abap_true
                                               nlpla   = lv_bin
                                               procty  = iv_procty ).

  APPEND ls_crea_hu TO lt_crea_hu.

  CALL FUNCTION '/SCWM/TO_CREATE_MOVE_HU'
    EXPORTING
      iv_lgnum       = iv_lgnum
      iv_update_task = abap_false
      iv_commit_work = abap_true
      it_create_hu   = lt_crea_hu
    IMPORTING
      et_bapiret     = lt_bapiret
      ev_severity    = lv_severity.

  IF lv_severity CA wmegc_severity_ea.
    ROLLBACK WORK.
    /scwm/cl_tm=>cleanup( ).
    ev_error = abap_true.
  ELSE.
    COMMIT WORK AND WAIT.
    /scwm/cl_tm=>cleanup( ).
  ENDIF.

ENDFORM.
