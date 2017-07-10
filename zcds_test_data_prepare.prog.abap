
REPORT  zcds_test_data_prepare.
*&---------------------------------------------------------------------*
*& Report  ZAD_PTEST_CREATE_DRO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

TYPE-POOLS abap.
TABLES /scwm/bapidlvdeadln.

DATA:
  lv_sdate           TYPE sy-datum,
  lv_edate           TYPE sy-datum,
  lv_sdatetime       TYPE tzntimestp,
  lv_edatetime       TYPE tzntimestp,
  lv_total_sec       TYPE i,
  lv_ran_int         TYPE i,
  lv_delivery        TYPE vbeln,
  lv_success         TYPE abap_bool,
  lv_numluw          TYPE i VALUE 1,
  lv_background_task TYPE abap_bool VALUE abap_false.


*** data buffer definition (war frhrt Klassenattribute)
DATA mv_docno TYPE vbeln.
DATA mv_delivery TYPE vbeln.
DATA mv_number_of_luw TYPE i.
DATA mv_number_of_doc TYPE i.
DATA mv_number_of_item TYPE i.
DATA mv_item_offset TYPE i VALUE 10.
DATA mv_item_interval TYPE i VALUE 10.
DATA ms_header_data TYPE /scwm/bapiobdlvhdr .
DATA ms_header_org TYPE /scwm/bapiobdlvhdrorg .
DATA mv_sender_system TYPE tbdls-logsys .
DATA ms_techn_control TYPE /scwm/bapidlvcontrol .
DATA ms_header_data_spl TYPE /scwm/spe_bapiobdlvhdr .
DATA:
  mt_header_partner
         TYPE TABLE OF /scwm/bapidlvpartner .
DATA:
  mt_header_partner_addr
         TYPE TABLE OF bapiaddr1 .
DATA:
  mt_header_deadlines
         TYPE TABLE OF /scwm/bapidlvdeadln .
DATA:
  mt_item_data
         TYPE TABLE OF /scwm/bapiobdlvitem .
DATA:
  mt_item_org
         TYPE TABLE OF /scwm/bapiobdlvitemorg .
DATA:
  mt_item_stock_trans
         TYPE TABLE OF /scwm/bapidlvitemsttr .
DATA:
  mt_item_coding_block
         TYPE TABLE OF /scwm/bapidlvcoblitem .
DATA:
  mt_item_reference_order
         TYPE TABLE OF /scwm/bapiobdlvitemrfo .
DATA:
  mt_item_ref_purchase_order
         TYPE TABLE OF /scwm/bapidlvitemrpo .
DATA:
  mt_item_serial_no
         TYPE TABLE OF /scwm/bapidlvitmserno .
DATA:
  mt_text_header
         TYPE TABLE OF /scwm/bapidlvtxthdr .
DATA:
  mt_text_lines
         TYPE TABLE OF /scwm/bapidlvtxtitem .
DATA:
  mt_handling_unit_header
         TYPE TABLE OF /scwm/bapidlvhdunhdr .
DATA:
  mt_handling_unit_item
         TYPE TABLE OF /scwm/bapidlvhdunitm .
DATA:
  mt_handling_unit_serno
         TYPE TABLE OF /scwm/bapidlvhdunserno .
DATA:
  mt_extension1
         TYPE TABLE OF bapiextc .
DATA:
  mt_extension2
         TYPE TABLE OF bapiext .
DATA:
  mt_return
         TYPE TABLE OF bapiret2 .
DATA:
  mt_tokenreference
         TYPE TABLE OF bapitokenreference .
DATA:
  mt_header_partner_guid
         TYPE TABLE OF /scwm/spe_bapidlvpartnerguid .
DATA:
  mt_item_data_spl
         TYPE TABLE OF /scwm/spe_bapiobdlvitm .
DATA:
  mt_batch_attributes
         TYPE TABLE OF bapi3060_allocation .
DATA:
  mt_batch_values_char
         TYPE TABLE OF bapi3060_valuation_char .
DATA:
  mt_batch_values_curr
         TYPE TABLE OF bapi3060_valuation_curr .
DATA:
  mt_batch_values_num
         TYPE TABLE OF bapi3060_valuation_num .
*** end data buffer definition

* technical parameters
SELECTION-SCREEN BEGIN OF BLOCK tech
    WITH   FRAME  TITLE text-f01.
PARAMETERS:
  p_sender TYPE tbdls-logsys DEFAULT 'B7VCLNT001',
  p_receiv TYPE tbdls-logsys DEFAULT 'B7VCLNT001',
  p_qname  TYPE trfcqnam DEFAULT ' '.          "'DAUMA_PERF_TEST'.
SELECTION-SCREEN END OF BLOCK tech.

* document structure parameters
SELECTION-SCREEN BEGIN OF BLOCK doc
    WITH   FRAME  TITLE text-f02.
PARAMETERS:
  p_numdoc TYPE i DEFAULT 1,
  p_numitm TYPE i DEFAULT 5 NO-DISPLAY,
  p_dlvtyp TYPE lfart DEFAULT 'LF',
  p_itmtyp TYPE pstyv DEFAULT 'TAN',
  p_liprio TYPE lprio DEFAULT '1' NO-DISPLAY,
  p_date   TYPE tzntstmps DEFAULT 2005040300000 NO-DISPLAY.
SELECT-OPTIONS
  s_date FOR /scwm/bapidlvdeadln-timestamp_utc.
SELECTION-SCREEN END OF BLOCK doc.

* org entities
SELECTION-SCREEN BEGIN OF BLOCK org
    WITH   FRAME  TITLE text-f03.
PARAMETERS:
*   p_sorg   TYPE   char4   DEFAULT 'EEG1',
  p_erpwh TYPE   char3   DEFAULT  'B20',
  p_plant TYPE   werks_d DEFAULT 'BP02',
  p_stloc TYPE   lgort_d DEFAULT 'AFS'.
SELECTION-SCREEN END OF BLOCK org.

* master data
SELECTION-SCREEN BEGIN OF BLOCK mast
    WITH   FRAME  TITLE text-f04.
PARAMETERS:
  p_cust   TYPE char10  DEFAULT 'CUST004',
  p_orcomb TYPE kzazu_d DEFAULT ' ' NO-DISPLAY,
  p_matnr  TYPE matnr   DEFAULT 'PROD-S01' NO-DISPLAY,
  p_matpre TYPE sstring DEFAULT '' NO-DISPLAY,
  p_preoff TYPE numc4   DEFAULT '0000' NO-DISPLAY,
  p_preadd TYPE numc4   DEFAULT '0000' NO-DISPLAY,
  p_qty    TYPE lfimg   DEFAULT '3.000' NO-DISPLAY,
  p_uom    TYPE meins   DEFAULT 'EA' NO-DISPLAY,
  p_qtydel TYPE numc2   DEFAULT '01' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK mast.

INITIALIZATION.
  lv_sdate = sy-datum - 1.
  lv_edate = sy-datum + 1.
  CONCATENATE lv_sdate '000000' INTO lv_sdatetime.
  CONCATENATE lv_edate '235959' INTO lv_edatetime.
  s_date-low = lv_sdatetime.
  s_date-high = lv_edatetime.
  s_date-option = 'BT'.
  s_date-sign = 'I'.
  APPEND s_date.

START-OF-SELECTION.

  DATA: lv_qty    TYPE lfimg,
        lv_preadd TYPE numc4,
        lv_numdoc TYPE i.

*  DO p_numluw TIMES.
  DO p_numdoc TIMES.

    CALL FUNCTION 'QF05_RANDOM_INTEGER'
      EXPORTING
        ran_int_max = 2
        ran_int_min = 1
      IMPORTING
        ran_int     = p_numitm.

    CALL METHOD cl_abap_tstmp=>subtract
      EXPORTING
        tstmp1 = s_date-high
        tstmp2 = s_date-low
      RECEIVING
        r_secs = lv_total_sec.

    CALL FUNCTION 'QF05_RANDOM_INTEGER'
      EXPORTING
        ran_int_max = lv_total_sec
        ran_int_min = 0
      IMPORTING
        ran_int     = lv_ran_int.

    CALL METHOD cl_abap_tstmp=>add
      EXPORTING
        tstmp = s_date-low
        secs  = lv_ran_int
      RECEIVING
        r_tstmp = p_date.

    PERFORM constructor USING
                          lv_numluw
                          p_numdoc
                          p_numitm.

* memory start qty
    lv_qty = p_qty.

    lv_numdoc = lv_numdoc + 1.

* increment erp_docno (mv_docno)
    PERFORM increment_docno.

* set start qty for first item
    p_qty = lv_qty.
* iteratively increasing product number offset
    lv_preadd = ( lv_numdoc - 1 ) * p_preadd.

* create interfaces data
    PERFORM generate_data USING p_date.

    IF p_qname IS NOT INITIAL.
      PERFORM setup_qrfc.
      lv_background_task = abap_true.
    ENDIF.

*  call RFC function module
    PERFORM call_save_replica USING lv_background_task
                              CHANGING lv_success.

    IF lv_success = abap_false.
      WRITE: / 'problem in rfc'(001).
      RETURN. "--------------------------------->
    ELSE.
      COMMIT WORK.
      WRITE: / 'Belegnummer ', mv_docno,
               ' verwendet.'.
    ENDIF.

    PERFORM clean_data.

  ENDDO.
*  ENDDO.

  IF p_numdoc EQ 1.
    MESSAGE mv_docno TYPE 'S'.
  ENDIF.


*************** end of main program **************************

FORM setup_qrfc.

  CALL FUNCTION 'TRFC_SET_QUEUE_NAME'
    EXPORTING
      qname              = p_qname
    EXCEPTIONS
      invalid_queue_name = 1
      OTHERS             = 2.

  CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
    EXPORTING
      qin_name           = p_qname
    EXCEPTIONS
      invalid_queue_name = 1
      OTHERS             = 2.

ENDFORM.                    "SETUP_QRFC
*&---------------------------------------------------------------------*
*&      Form  constructor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM constructor USING
           iv_number_of_luw TYPE i
           iv_number_of_doc TYPE i
           iv_number_of_item TYPE i.

  DATA total_doc TYPE nrquan.

  mv_number_of_doc  = iv_number_of_doc.
  mv_number_of_item = iv_number_of_item.

  total_doc = iv_number_of_doc .

* fetch total_doc document number from Number Range
* and get the first one

**  perform get_erp_docnos using     total_doc
**                         changing  mv_docno.

ENDFORM.                    " constructor
*&---------------------------------------------------------------------*
*&      Form  get_erp_docnos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TOTAL_DOC  text
*      <--P_MV_DOCNO  text
*----------------------------------------------------------------------*
FORM get_erp_docnos  USING    iv_total_numbers
                     CHANGING ev_erp_docno.

  DATA: lv_last_docno TYPE vbeln .

* get new erp numbers
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr   = '01'
      object        = '/SCWM/DLNO'
      ignore_buffer = ''
      quantity      = iv_total_numbers
    IMPORTING
      number        = lv_last_docno.

  ev_erp_docno = lv_last_docno - iv_total_numbers + 1 .

ENDFORM.                    " get_erp_docnos
*&---------------------------------------------------------------------*
*&      Form  call_save_replica
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_BACKGROUND_TASK  text
*      <--P_LV_SUCCESS  text
*----------------------------------------------------------------------*
FORM call_save_replica  USING    iv_background_task
                        CHANGING ev_success.

  IF iv_background_task = abap_true .
* make RFC with hardcoded R/3 data:
    CALL FUNCTION '/SCWM/OUTB_DLV_SAVEREPLICA'
      IN BACKGROUND TASK
      DESTINATION p_receiv      "shortcut RFCDEST = LOGSYS
      EXPORTING
        header_data         = ms_header_data
        header_org          = ms_header_org
        sender_system       = mv_sender_system
        techn_control       = ms_techn_control
        header_data_spl     = ms_header_data_spl
      IMPORTING
        delivery            = mv_delivery
      TABLES
        header_partner      = mt_header_partner
        header_partner_addr = mt_header_partner_addr
        header_deadlines    = mt_header_deadlines
        item_data           = mt_item_data
        item_org            = mt_item_org
        item_stock_trans    = mt_item_stock_trans
        item_coding_block   = mt_item_coding_block
*       item_reference_order    = mt_item_reference_order
*       item_ref_purchase_order = mt_item_ref_purchase_order
*       item_serial_no      = mt_item_serial_no
*       text_header         = mt_text_header
*       text_lines          = mt_text_lines
*       handling_unit_header    = mt_handling_unit_header
*       handling_unit_item  = mt_handling_unit_item
*       handling_unit_serno = mt_handling_unit_serno
*       extension1          = mt_extension1
*       extension2          = mt_extension2
        return              = mt_return
*       tokenreference      = mt_tokenreference
*       header_partner_guid = mt_header_partner_guid
        item_data_spl       = mt_item_data_spl
*       batch_attributes    = mt_batch_attributes
*       batch_values_char   = mt_batch_values_char
*       batch_values_curr   = mt_batch_values_curr
*       batch_values_num    = mt_batch_values_num
      EXCEPTIONS
        ex_enqueue          = 1
        ex_error            = 2
        OTHERS              = 3.

  ELSE. " nicht in Update Task

* make RFC with hardcoded R/3 data:
    CALL FUNCTION '/SCWM/OUTB_DLV_SAVEREPLICA'
      EXPORTING
        header_data         = ms_header_data
        header_org          = ms_header_org
        sender_system       = mv_sender_system
        techn_control       = ms_techn_control
        header_data_spl     = ms_header_data_spl
      IMPORTING
        delivery            = mv_delivery
      TABLES
        header_partner      = mt_header_partner
        header_partner_addr = mt_header_partner_addr
        header_deadlines    = mt_header_deadlines
        item_data           = mt_item_data
        item_org            = mt_item_org
        item_stock_trans    = mt_item_stock_trans
        item_coding_block   = mt_item_coding_block
*       item_reference_order    = mt_item_reference_order
*       item_ref_purchase_order = mt_item_ref_purchase_order
*       item_serial_no      = mt_item_serial_no
*       text_header         = mt_text_header
*       text_lines          = mt_text_lines
*       handling_unit_header    = mt_handling_unit_header
*       handling_unit_item  = mt_handling_unit_item
*       handling_unit_serno = mt_handling_unit_serno
*       extension1          = mt_extension1
*       extension2          = mt_extension2
        return              = mt_return
*       tokenreference      = mt_tokenreference
*       header_partner_guid = mt_header_partner_guid
        item_data_spl       = mt_item_data_spl
*       batch_attributes    = mt_batch_attributes
*       batch_values_char   = mt_batch_values_char
*       batch_values_curr   = mt_batch_values_curr
*       batch_values_num    = mt_batch_values_num
      EXCEPTIONS
        ex_enqueue          = 1
        ex_error            = 2
        OTHERS              = 3.

  ENDIF.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    WRITE: / 'problem in rfc'(001).
    ev_success = abap_false.
    RETURN.
  ELSE.
*    ev_delivery = mv_delivery .
    ev_success = abap_true.
  ENDIF.

ENDFORM.                    " call_save_replica
*&---------------------------------------------------------------------*
*&      Form  clean_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clean_data .

* note: do not clean mv_docno, it is incrimented .
  CLEAR:
    ms_header_data,
    ms_header_org,
    mv_sender_system,
    ms_techn_control,
    ms_header_data_spl,
    mt_header_partner,
    mt_header_partner_addr,
    mt_header_deadlines,
    mt_item_data,
    mt_item_org,
    mt_item_stock_trans,
    mt_item_coding_block,
    mt_item_reference_order,
    mt_item_ref_purchase_order,
    mt_item_serial_no,
    mt_text_header,
    mt_text_lines,
    mt_handling_unit_header,
    mt_handling_unit_item,
    mt_handling_unit_serno,
    mt_extension1,
    mt_extension2,
    mt_return,
    mt_tokenreference,
    mt_header_partner_guid,
    mt_item_data_spl,
    mt_batch_attributes,
    mt_batch_values_char,
    mt_batch_values_curr,
    mt_batch_values_num .

ENDFORM.                    " clean_data
*&---------------------------------------------------------------------*
*&      Form  increment_docno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM increment_docno .

* get new erp numbers
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr   = '01'
      object        = '/SCWM/DLNO'
      ignore_buffer = ''
*     quantity      = iv_total_numbers
    IMPORTING
      number        = mv_docno.

*  mv_docno = mv_docno + 1.

ENDFORM.                    " increment_docno
*&---------------------------------------------------------------------*
*&      Form  generate_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_data USING iv_date.

  DATA: lv_count  TYPE i VALUE 0,
        lv_itemno TYPE posnr VALUE 0,
        offset    LIKE mv_item_offset,
        lamda     LIKE mv_item_interval.

  offset = mv_item_offset .
  lamda  = mv_item_interval .

  PERFORM fill_header_data.
  PERFORM fill_header_data_spl.
  PERFORM fill_header_deadline USING iv_date.
  PERFORM fill_header_org.
  PERFORM fill_header_partner.
**  perform fill_header_partner_addr.

**  perform fill_extension1.
**  perform fill_extension2.
**  perform fill_handling_unit_header.
**  perform fill_handling_unit_item.
**  perform fill_handling_unit_serno.
**  perform fill_return.
  PERFORM fill_sender_system.
  PERFORM fill_techn_control.
**  perform fill_tokenreference.

  DO mv_number_of_item TIMES.
    lv_count = lv_count + 1 .
    IF lv_count = 2.
      p_matnr = 'PROD-M01'.
    ENDIF.
    lv_itemno = offset + ( lv_count - 1 ) * lamda .

    IF p_qtydel > 0 AND lv_count > 1.
      IF frac( ( lv_count - 1 ) / p_qtydel ) EQ 0.
        p_qty = p_qty + 1.
      ENDIF.
    ENDIF.

    PERFORM fill_item_data USING lv_itemno.
    PERFORM fill_item_org USING lv_itemno.
**    perform fill_item_coding_block.
**    perform fill_item_reference_order.
**    perform fill_item_ref_purchase_order.
**    perform fill_item_serial_no.
**    perform fill_item_stock_trans.
**    perform fill_text_header.
**    perform fill_text_lines.

  ENDDO.

ENDFORM.                    " generate_data
*&---------------------------------------------------------------------*
*&      Form  fill_header_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_header_data .

*  DATA ls_header_data LIKE ms_header_data.

  ms_header_data-deliv_numb     = mv_docno .
  ms_header_data-incoterms1     = 'CIF' .
  ms_header_data-incoterms2     = 'Morton' .
*  ms_header_data-ROUTE          =
  ms_header_data-ship_cond      = '01' .
  ms_header_data-total_wght     = 1 .
  ms_header_data-net_weight     = 1 .
  ms_header_data-unit_of_wt     = 'KG' .
  ms_header_data-unit_of_wt_iso = 'KGM' .
  ms_header_data-volume         = 1 .
  ms_header_data-volumeunit     =   'CCM' .
  ms_header_data-volumeunit_iso =   'CMQ' .
*  ms_header_data-NOSHPUNITS     = 	00000 .
*  ms_header_data-BILLOFLADING   =
  ms_header_data-trans_cat      = '031'.
*  ms_header_data-TRANSP_ID      =
*  ms_header_data-GRGISLIP_NO    =
*  ms_header_data-EXTDELV_NO     =
  ms_header_data-dlv_type       = p_dlvtyp.
*  ms_header_data-COMPL_DLV      =
*  ms_header_data-DLV_BLOCK      =
  ms_header_data-dlv_prio       = p_liprio.
*  ms_header_data-CUST_GROUP     =
*  ms_header_data-PICK_LOC       =
  ms_header_data-trans_grp      = '0001' .
*  ms_header_data-SHIP_BLOCK     = .
*  ms_header_data-ROUTESCHED     = .

ENDFORM.                    " fill_header_data
*&---------------------------------------------------------------------*
*&      Form  fill_header_data_spl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_header_data_spl .

  ms_header_data_spl-deliv_numb                 = mv_docno.
*  ms_header_data_spl-UNCHECKED                 =
*  ms_header_data_spl-GEOROUTE                  =
*  ms_header_data_spl-GEOROUTE_CHANGE_INDICATOR =
*  ms_header_data_spl-CARRIER_CHANGE_INDICATOR  =
  ms_header_data_spl-ordcombind                = p_orcomb.
*  ms_header_data_spl-UNIT_SYSTEM               =
*  ms_header_data_spl-INITIATOR_PROCESS         =

ENDFORM.                    " fill_header_data_spl
*&---------------------------------------------------------------------*
*&      Form  fill_header_deadline
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_DATE  text
*----------------------------------------------------------------------*
FORM fill_header_deadline  USING    iv_date.

  DATA: ls_header_deadline LIKE LINE OF mt_header_deadlines.

  ls_header_deadline-deliv_numb    = mv_docno .
  ls_header_deadline-timetype      = 'WSHDRLFDAT'.
  ls_header_deadline-timestamp_utc = iv_date.
  ls_header_deadline-timezone      = 'CET'.
  INSERT ls_header_deadline INTO TABLE mt_header_deadlines.

  ls_header_deadline-deliv_numb    = mv_docno .
  ls_header_deadline-timetype      = 'WSHDRWADAT'.
  ls_header_deadline-timestamp_utc = iv_date.
  ls_header_deadline-timezone      = 'CET'.
  INSERT ls_header_deadline INTO TABLE mt_header_deadlines.

  ls_header_deadline-deliv_numb    = mv_docno .
  ls_header_deadline-timetype      = 'WSHDRWADTI'.
  ls_header_deadline-timestamp_utc = 0.
  ls_header_deadline-timezone      = 'CET'.
  INSERT ls_header_deadline INTO TABLE mt_header_deadlines.

  ls_header_deadline-deliv_numb    = mv_docno .
  ls_header_deadline-timetype      = 'WSHDRLDDAT'.
  ls_header_deadline-timestamp_utc = iv_date.
  ls_header_deadline-timezone      = 'CET'.
  INSERT ls_header_deadline INTO TABLE mt_header_deadlines.

  ls_header_deadline-deliv_numb    = mv_docno .
  ls_header_deadline-timetype      = 'WSHDRTDDAT'.
  ls_header_deadline-timestamp_utc = iv_date.
  ls_header_deadline-timezone      = 'CET'.
  INSERT ls_header_deadline INTO TABLE mt_header_deadlines.

  ls_header_deadline-deliv_numb    = mv_docno .
  ls_header_deadline-timetype      = 'WSHDRKODAT'.
  ls_header_deadline-timestamp_utc = iv_date.
  ls_header_deadline-timezone      = 'CET'.
  INSERT ls_header_deadline INTO TABLE mt_header_deadlines.

ENDFORM.                    " fill_header_deadline
*&---------------------------------------------------------------------*
*&      Form  fill_header_org
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_header_org .

  ms_header_org-deliv_numb = mv_docno .
  ms_header_org-ship_point = '0001'.
*  ms_header_org-salesorg   = p_sorg.
* ms_header_org-LOADING_PT =
* ms_header_org-SALES_OFF  =
  ms_header_org-whse_no    = p_erpwh.
**** Q1P  ms_header_org-sales_dist = '000001'.
* ms_header_org-DOOR       =
* ms_header_org-UNLOAD_PT  =

ENDFORM.                    " fill_header_org
*&---------------------------------------------------------------------*
*&      Form  fill_header_partner
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_header_partner .

  DATA ls_header_partner LIKE LINE OF mt_header_partner.

  ls_header_partner-deliv_numb  = mv_docno .
  ls_header_partner-itm_number  = '000000'.
  ls_header_partner-partn_role  = 'AG' .
  PERFORM get_partner_ag USING mv_docno
                      CHANGING ls_header_partner-partner_no.

***  ls_header_partner-address_no  = '0000025231' .
* ls_header_partner-DESC_PARTN  =
* ls_header_partner-MANUAL_ADDR =
* ls_header_partner-SCA_CODE    =

  INSERT ls_header_partner INTO TABLE mt_header_partner .

  ls_header_partner-partn_role  = 'WE' .

  INSERT ls_header_partner INTO TABLE mt_header_partner .

ENDFORM.                    " fill_header_partner
*&---------------------------------------------------------------------*
*&      Form  fill_header_partner_addr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_header_partner_addr .

  DATA ls_header_partner_addr LIKE LINE OF mt_header_partner_addr.

  ls_header_partner_addr-addr_no  = '0000025231'.
  ls_header_partner_addr-formofaddr	= 'Mr.'.
  ls_header_partner_addr-name	       = 'Roundtrip Partner'.
  ls_header_partner_addr-name_2	= 'Cat/Ford 2'.
*ls_header_partner_addr-NAME_3  =
*ls_header_partner_addr-NAME_4  =
*ls_header_partner_addr-C_O_NAME  =
  ls_header_partner_addr-city    	= 'Walldorf'.
*ls_header_partner_addr-DISTRICT  =
*ls_header_partner_addr-CITY_NO	=
  ls_header_partner_addr-postl_cod1	= '69190'.
*ls_header_partner_addr-POSTL_COD2  =
*ls_header_partner_addr-POSTL_COD3  =
*ls_header_partner_addr-PO_BOX  =
*ls_header_partner_addr-PO_BOX_CIT  =
*ls_header_partner_addr-DELIV_DIS	=
  ls_header_partner_addr-street	= 'Lange Straﾟe 1'.
*ls_header_partner_addr-STREET_NO	=
*ls_header_partner_addr-STR_ABBR  =
  ls_header_partner_addr-house_no	= '1'.
*ls_header_partner_addr-STR_SUPPL1  =
*ls_header_partner_addr-STR_SUPPL2  =
*ls_header_partner_addr-LOCATION  =
*ls_header_partner_addr-BUILDING  =
*ls_header_partner_addr-FLOOR	=
*ls_header_partner_addr-ROOM_NO	=
  ls_header_partner_addr-country  = 'DE'.
  ls_header_partner_addr-langu  = 'D'.
  ls_header_partner_addr-region	= ' '.
  ls_header_partner_addr-sort1  = 'CF2'.
  ls_header_partner_addr-sort2  = 'CAT/FORD'.
  ls_header_partner_addr-time_zone  = 'CET'.
*ls_header_partner_addr-TAXJURCODE  =
*ls_header_partner_addr-ADR_NOTES	=
*ls_header_partner_addr-COMM_TYPE	=
*ls_header_partner_addr-TEL1_NUMBR  =
*ls_header_partner_addr-TEL1_EXT  =
*ls_header_partner_addr-FAX_NUMBER  =
*ls_header_partner_addr-FAX_EXTENS  =
  ls_header_partner_addr-street_lng	= 'Lange Straﾟe'.
*ls_header_partner_addr-DISTRCT_NO  =
*ls_header_partner_addr-CHCKSTATUS  =
*ls_header_partner_addr-PBOXCIT_NO  =
*ls_header_partner_addr-TRANSPZONE  =
*ls_header_partner_addr-HOUSE_NO2	=
*ls_header_partner_addr-E_MAIL  =
*ls_header_partner_addr-STR_SUPPL3  =
*ls_header_partner_addr-TITLE	=
  ls_header_partner_addr-countryiso	= 'DE'.
  ls_header_partner_addr-langu_iso  = 'DE'.
*ls_header_partner_addr-BUILD_LONG  =
*ls_header_partner_addr-REGIOGROUP  =
*ls_header_partner_addr-HOME_CITY	=
*ls_header_partner_addr-HOMECITYNO  =
*ls_header_partner_addr-PCODE1_EXT  =
*ls_header_partner_addr-PCODE2_EXT  =
*ls_header_partner_addr-PCODE3_EXT  =
*ls_header_partner_addr-PO_W_O_NO	=
*ls_header_partner_addr-PO_BOX_REG  =
*ls_header_partner_addr-POBOX_CTRY  =
*ls_header_partner_addr-PO_CTRYISO  =
*ls_header_partner_addr-HOMEPAGE  =
*ls_header_partner_addr-DONT_USE_S  =
*ls_header_partner_addr-DONT_USE_P  =
*ls_header_partner_addr-HOUSE_NO3	=
*ls_header_partner_addr-LANGU_CR  =
*ls_header_partner_addr-LANGUCRISO  =

***  insert ls_header_partner_addr into table mt_header_partner_addr.
* the same address for role AG and role WE
***  insert ls_header_partner_addr into table mt_header_partner_addr.

ENDFORM.                    " fill_header_partner_addr
*&---------------------------------------------------------------------*
*&      Form  fill_sender_system
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_sender_system .

  mv_sender_system = p_sender .

ENDFORM.                    " fill_sender_system
*&---------------------------------------------------------------------*
*&      Form  fill_techn_control
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_techn_control .

*ms_techn_control-DEBUG_FLG    = '' .
*ms_techn_control-UPD_IND      =
  ms_techn_control-recv_whs_no = p_erpwh.
  ms_techn_control-recv_sys    = p_receiv.
*ms_techn_control-DLV_TYPE     =

ENDFORM.                    " fill_techn_control
*&---------------------------------------------------------------------*
*&      Form  fill_item_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ITEMNO  text
*----------------------------------------------------------------------*
FORM fill_item_data  USING    iv_itemno.

  DATA: ls_item_data LIKE LINE OF mt_item_data,
        lv_itemno    TYPE posnr.

  ls_item_data-deliv_numb	   =  mv_docno .
  ls_item_data-itm_number	   =  iv_itemno .

  PERFORM get_product USING mv_docno iv_itemno
                      CHANGING ls_item_data-material.

  ls_item_data-mat_entrd       =  'Testreport' .
*  ls_item_data-short_text     =  'Test'.
*  ls_item_data-BATCH	   =  '' .
*  ls_item_data-CUST_MAT     =  '' .
  ls_item_data-dlv_qty     =  p_qty .
*  ls_item_data-sales_unit_iso =  'PCE' .
  ls_item_data-sales_unit     =  p_uom .
  ls_item_data-dlv_qty_stock  = p_qty .
  ls_item_data-base_uom	   =  p_uom.
*  ls_item_data-base_uom_iso     =  'PCE' .
*  ls_item_data-net_weight     =  '0.000' .
*  ls_item_data-gross_wt     =  '0.000' .
*  ls_item_data-unit_of_wt_iso =  'KGM' .
*  ls_item_data-unit_of_wt     =  'KG' .
*  ls_item_data-volume         =  '0.000' .
*  ls_item_data-volumeunit_iso =  '   ' .
*  ls_item_data-volumeunit  =  ' ' .
*  ls_item_data-STGE_BIN  =  '' .
  ls_item_data-hieraritem	=  '000000' .
*  ls_item_data-USEHIERITM  =  '' .
*  ls_item_data-loadinggrp  =  '0001' .
*  ls_item_data-trans_grp  =  '0001' .
*  ls_item_data-dlv_group  =  '000' .
*  ls_item_data-EAN_UPC	=  '' .
*  ls_item_data-BOMEXPL_NO  =  '' .
*  ls_item_data-REC_POINT	=  '' .
*  ls_item_data-MATFRGTGRP  =  '' .
  ls_item_data-item_categ	=  p_itmtyp.
*  ls_item_data-PART_DLV  =  '' .
  ls_item_data-overdel_unlim =  '' .
  ls_item_data-overdeltol	  =  ' 0.0' .
  ls_item_data-under_tol    =  ' 0.0' .
*  ls_item_data-BTCH_SPLIT    =  '' .
*  ls_item_data-BTCHEVALTYP	  =  '' .
*  ls_item_data-ITEM_TYPE	  =  '' .
*  ls_item_data-SPEC_STOCK    =  '' .
*  ls_item_data-PACKCNTRL	  =  '' .
*  ls_item_data-mat_grp_sm    =  ' ' .
*  ls_item_data-DB_CR_IND	  =  '' .
*  ls_item_data-PROMOTION	  =  '' .
*  ls_item_data-ENVT_RLVT	  =  '' .
*  ls_item_data-move_type    =  '601' .
*  ls_item_data-move_type_wm    =  '601' .
*  ls_item_data-STOCK_TYPE    =  '' .
*  ls_item_data-mvt_ind    =  'L' .
*  ls_item_data-CUM_BTCH_QTY     =  '0.000' .
*  ls_item_data-CUM_BTCH_GR_WT  = '0.000' .
*  ls_item_data-CUM_BTCH_NT_WT  = '0.000' .
*  ls_item_data-CUM_BTCH_VOL  =  '0.000' .
*  ls_item_data-CUMBTCHWTUN	       =  '' .
*  ls_item_data-CUMBTCHWTUN_ISO	=  '' .
*  ls_item_data-CUMBTCHVOLUN  =  '' .
*  ls_item_data-CUMBTCHVOLUN_ISO  =  '' .
*  ls_item_data-STOCK_CAT	=  '' .
*  ls_item_data-SP_STCK_NO  =  '' .
*  ls_item_data-matl_group  =  '01' .
*  ls_item_data-matl_type  =  'HAWA' .
*  ls_item_data-PROD_HIER	=  '' .
*  ls_item_data-MATL_GRP_1  =  '' .
*  ls_item_data-MATL_GRP_2  =  '' .
*  ls_item_data-MATL_GRP_3  =  '' .
*  ls_item_data-MATL_GRP_4  =  '' .
*  ls_item_data-MATL_GRP_5  =  '' .
*  ls_item_data-CUST_GRP2	=  '' .
*  ls_item_data-CUST_GRP3	=  '' .
*  ls_item_data-CUST_GRP4	=  '' .
*  ls_item_data-CUST_GRP5	=  '' .
*  ls_item_data-CUST_GRP1	=  '' .
  ls_item_data-del_qty_flo  =  p_qty .
*  ls_item_data-CONV_FACT	=  '0' .
  ls_item_data-dlv_qty_st_flo	=  p_qty .
  ls_item_data-sales_qty_denom  =  '1' .
  ls_item_data-sales_qty_num  =  '1' .
*  ls_item_data-FLG_LEAD_UNIT	=  '' .
*  ls_item_data-cumbtchqtysu_flo  =  '0' .
*  ls_item_data-cumbtchqtysu         =  '0' .
*  ls_item_data-VAL_TYPE         =  '' .
*  ls_item_data-MATERIAL_EXTERNAL	=  '' .
*  ls_item_data-MATERIAL_GUID	=  '' .
*  ls_item_data-MATERIAL_VERSION  =  '' .
*  ls_item_data-MAT_ENTRD_EXTERNAL  =  '' .
*  ls_item_data-MAT_ENTRD_GUID  =  '' .
*  ls_item_data-MAT_ENTRD_VERSION	=  '' .
*  ls_item_data-insplot         =  '0' .

  INSERT ls_item_data INTO TABLE mt_item_data .

ENDFORM.                    " fill_item_data
*&---------------------------------------------------------------------*
*&      Form  fill_item_org
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ITEMNO  text
*----------------------------------------------------------------------*
FORM fill_item_org  USING    iv_itemno.

  DATA: ls_item_org LIKE LINE OF mt_item_org .

  ls_item_org-deliv_numb = mv_docno .
  ls_item_org-itm_number = iv_itemno .
*  ls_item_org-SALES_OFF  =
*  ls_item_org-SALES_GRP  =
***  ls_item_org-distr_chan = '01'.
***  ls_item_org-division   = '01'.
  ls_item_org-plant      = p_plant.
  ls_item_org-stge_loc   = p_stloc.

  INSERT ls_item_org INTO TABLE mt_item_org.

ENDFORM.                    " fill_item_org
*&---------------------------------------------------------------------*
*&      Form  get_partner_ag
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MV_DOCNO  text
*      -->P_CHANING  text
*      -->P_LS_HEADER_PARTNER_PARTNER_NO  text
*----------------------------------------------------------------------*
FORM get_partner_ag  USING    iv_docno
                  CHANGING ev_partner.

  DATA: lv_item_order       TYPE i,
        lv_item_order_ch(2) TYPE n,
        lv_prt_order(3)     TYPE n,
        lv_frequency        TYPE i,
        lv_mod              TYPE i.

  lv_mod = iv_docno MOD 148 .
** partner 001 and 002 do not have ok data !
** use partner 003 - 150
  lv_prt_order = lv_mod + 3.
  CONCATENATE 'PF_CUST' lv_prt_order INTO ev_partner.

* test version with static partner as of A1P independent of DOCNO:
  ev_partner = p_cust.

ENDFORM.                    " get_partner_ag
*&---------------------------------------------------------------------*
*&      Form  get_partner_ag
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MV_DOCNO  text
*      -->P_CHANING  text
*      -->P_LS_HEADER_PARTNER_PARTNER_NO  text
*----------------------------------------------------------------------*
FORM get_partner  USING    iv_docno
                  CHANGING ev_partner.

  DATA: lv_item_order       TYPE i,
        lv_item_order_ch(2) TYPE n,
        lv_prt_order(3)     TYPE n,
        lv_frequency        TYPE i,
        lv_mod              TYPE i.

  lv_mod = iv_docno MOD 148 .
** partner 001 and 002 do not have ok data !
** use partner 003 - 150
  lv_prt_order = lv_mod + 3.
  CONCATENATE 'PF_CUST' lv_prt_order INTO ev_partner.

* test version with static partner as of A1P independent of DOCNO:
  ev_partner = p_cust.

ENDFORM.                    " get_partner
*&---------------------------------------------------------------------*
*&      Form  get_product
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MV_DOCNO  text
*      -->P_IV_ITEMNO  text
*      <--P_LS_ITEM_DATA_MATERIAL  text
*----------------------------------------------------------------------*
FORM get_product  USING    iv_docno
                           iv_itemno
                  CHANGING ev_productno.


  DATA: lv_item_order       TYPE i,
        lv_item_order_ch(2) TYPE n,
        lv_prod_order(5)    TYPE n,
        lv_frequency        TYPE i,
        lv_mod              TYPE i.


  IF p_matnr IS NOT INITIAL.
    ev_productno = p_matnr.
    RETURN.
  ENDIF.

  IF p_matpre IS NOT INITIAL.
*  get the order of the item
    lv_item_order = iv_itemno.
    lv_item_order =
     ( ( lv_item_order - mv_item_offset ) / mv_item_interval ) + 1 .

*  define the product to be used for this item order number
    lv_prod_order = lv_item_order + p_preoff + lv_preadd.
    CONCATENATE p_matpre lv_prod_order
         INTO ev_productno.
  ENDIF.

ENDFORM.                    " get_product
