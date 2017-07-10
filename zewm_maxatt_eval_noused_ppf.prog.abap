*&---------------------------------------------------------------------*
*& Report ZEWM_MAXATT_EVAL_NOUSED_PPF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
" ----------------------------------------------------------------------
" This code is produced by SAP Consulting/Max Attention
" Developed by: Christoph Persic
" This coding is not supported by SAP
" For any feedback/support please contact SAP Consulting / Support
" Any support can only be given base on decission and in case additional
" budget is agreed/approved
" Please contact your SAP representitive!
" ----------------------------------------------------------------------
REPORT ZEWM_MAXATT_EVAL_NOUSED_PPF.
************************************************************************
*** TABLES                                                           ***
************************************************************************
TABLES:
  ppfttrigg.

************************************************************************
*** TYPES                                                            ***
************************************************************************
TYPES:
  BEGIN OF ts_actions_cust,
    applctn   LIKE ppfttrigg-applctn,
    context   LIKE ppfttrigg-context,
    ttype     LIKE ppfttrigg-ttype,
    inactive  TYPE ppfdttiact,
    common    LIKE ppftconde-isinclprof,
    cmnprfl   LIKE ppftconde-inclprofil,
  END OF ts_actions_cust.

TYPES:
  BEGIN OF ts_actions_output,
    status    TYPE icon_d,
    applctn   LIKE ppfttrigg-applctn,
    context   LIKE ppfttrigg-context,
    ttype     LIKE ppfttrigg-ttype,
    inactive  TYPE ppfdttiact,
    common    LIKE ppftconde-isinclprof,
    count     TYPE i,
    l_date    TYPE sydatum,
    l_time    TYPE syuzeit,
    run_rfc   TYPE abap_bool,
    rfc_count TYPE i,
    rfc_date  TYPE sydatum,
    rfc_time  TYPE syuzeit,
  END OF ts_actions_output.

TYPES:
  tt_actions_output TYPE TABLE OF ts_actions_output.

************************************************************************
*** CONSTANTS                                                        ***
************************************************************************

CONSTANTS:
  gc_icon_green   TYPE icon_d VALUE '@5B@',
  gc_icon_yellow  TYPE icon_d VALUE '@5D@',
  gc_icon_rfc_run TYPE icon_d VALUE '@B4@',
  gc_icon_rfc_red TYPE icon_d VALUE '@B2@'.

************************************************************************
*** DATA                                                             ***
************************************************************************
DATA: gt_actions_cust   TYPE TABLE OF ts_actions_cust,
      gt_ppf_actions    TYPE TABLE OF ppfttrigg,
      gt_actions_output TYPE tt_actions_output.

DATA: gv_timestamp_from TYPE timestamp,
      gv_timestamp_to   TYPE timestamp.

************************************************************************
*** SELECTION SCREEN                                                 ***
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE actsel.
SELECT-OPTIONS:
  s_appl    FOR ppfttrigg-applctn,
  s_cntxt   FOR ppfttrigg-context,
  s_ttype   FOR ppfttrigg-ttype,
  s_appkey  FOR ppfttrigg-applkey,
  s_status  FOR ppfttrigg-status,
  s_metype  FOR ppfttrigg-metype.

SELECTION-SCREEN SKIP.
PARAMETERS:
  p_dptc    TYPE ppfddsptch AS LISTBOX VISIBLE LENGTH 43 DEFAULT '1',
  p_iaktv   TYPE ppfdttiact AS CHECKBOX DEFAULT abap_true,
  p_corct   TYPE abap_bool  AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN SKIP.

PARAMETERS:
  p_date_f  TYPE sydatum,
  p_time_f  TYPE syuzeit .

PARAMETERS:
  p_date_t  TYPE sydatum,
  p_time_t  TYPE syuzeit.
SELECTION-SCREEN END OF BLOCK b0.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE rfc.
PARAMETERS: p_rfc   TYPE bdl_userfc AS CHECKBOX DEFAULT ' ' USER-COMMAND rfc,
            p_dest  TYPE spiprstdest VALUE CHECK,
            p_evall RADIOBUTTON GROUP rg2,
            p_evina RADIOBUTTON GROUP rg2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE output.
PARAMETERS:
  p_ralv    RADIOBUTTON GROUP rbg1 DEFAULT 'X',
  p_rwrite  RADIOBUTTON GROUP rbg1.
SELECTION-SCREEN END OF BLOCK b2.

************************************************************************
*** AT SELECTION-SCREEN OUTPUT                                       ***
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_DEST'
      OR screen-name = 'P_EVALL'
      OR screen-name = 'P_EVINA'.
      IF p_rfc IS INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON p_dest.
  DATA: lv_count TYPE i.
  IF p_dest IS NOT INITIAL.
    SELECT COUNT(*) FROM rfcdes INTO lv_count WHERE rfcdest = p_dest.
    IF lv_count < 1.
      MESSAGE 'Invalid RFC Destination' TYPE 'E'.
    ENDIF.
  ENDIF.

************************************************************************
*** INITIALIZATION                                                   ***
************************************************************************
INITIALIZATION.
  actsel  = 'Selection of Actions'.  "#ECNOTEXT
  output  = 'Output Option'.         "#ECNOTEXT
  rfc     = 'PPF Check RFC Destination'.

************************************************************************
*** START-OF-SELECTION                                               ***
************************************************************************
START-OF-SELECTION.
  PERFORM initialize_selection_params.
  PERFORM read_customizing.
  PERFORM read_actions_per_cust_entry.
  PERFORM analyse_ppf_actions.
  IF p_corct IS INITIAL.
    PERFORM remove_correct_actions.
  ENDIF.

  IF p_rfc IS NOT INITIAL AND p_dest IS NOT INITIAL.
    PERFORM test_ppf_rfc.
  ENDIF.


************************************************************************
*** END-OF-SELECTION                                                 ***
************************************************************************
END-OF-SELECTION.
  IF p_ralv IS NOT INITIAL.
    PERFORM alv_grid_output.
  ELSEIF p_rwrite IS NOT INITIAL.
    PERFORM simple_write_output.
  ENDIF.

************************************************************************
*** SUBROUTINES / FORMS                                              ***
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  READ_CUSTOMIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_customizing .
  DATA: lv_query_string TYPE string.
  CLEAR: gt_actions_cust.

  CONCATENATE 'a~name IN s_appl'              "#ECNOTEXT
              'AND b~name IN s_cntxt'         "#ECNOTEXT
              'AND c~name IN s_ttype'         "#ECNOTEXT
  INTO lv_query_string SEPARATED BY space.

  IF p_iaktv IS INITIAL.
    CONCATENATE lv_query_string
                'AND c~inactive = abap_false'
    INTO lv_query_string SEPARATED BY space.
  ENDIF.

  SELECT a~name b~name c~name c~inactive b~isinclprof b~inclprofil
    APPENDING TABLE gt_actions_cust
    FROM ( ( ppftappl AS a
    JOIN ppftconde AS b
    ON a~name = b~applname )
    JOIN ppftttcu AS c
    ON b~name = c~contxtname )
    WHERE (lv_query_string).
ENDFORM.                    " READ_CUSTOMIZING
*&---------------------------------------------------------------------*
*&      Form  READ_ACTIONS_PER_CUST_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_actions_per_cust_entry .
  DATA: ls_actions_cust TYPE ts_actions_cust,
        lv_query_string TYPE string.

  CLEAR:
    gt_ppf_actions.

  CONCATENATE 'applctn = ls_actions_cust'                   "#ECNOTEXT
              'AND   context    = ls_actions_cust-context'  "#ECNOTEXT
              'AND   ttype      = ls_actions_cust-ttype'    "#ECNOTEXT
              'AND   applkey   IN s_appkey'                 "#ECNOTEXT
              'AND   status    IN s_status'                 "#ECNOTEXT
              'AND   metype    IN s_metype'                 "#ECNOTEXT
              'AND   DISPATCH  LIKE p_dptc'                 "#ECNOTEXT
  INTO lv_query_string SEPARATED BY space.

  IF p_date_f IS NOT INITIAL.
    CONCATENATE lv_query_string
                'AND timecreate GE gv_timestamp_from'   "#ECNOTEXT
    INTO lv_query_string SEPARATED BY space.
  ENDIF.

  IF p_date_t IS NOT INITIAL.
    CONCATENATE lv_query_string
                'AND timecreate LE gv_timestamp_to'     "#ECNOTEXT
    INTO lv_query_string SEPARATED BY space.
  ENDIF.

  LOOP AT gt_actions_cust INTO ls_actions_cust.
    SELECT * FROM ppfttrigg APPENDING TABLE gt_ppf_actions
      WHERE (lv_query_string).
  ENDLOOP.

ENDFORM.                    " READ_ACTIONS_PER_CUST_ENTRY
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_SELECTION_PARAMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_selection_params .

  IF p_dptc IS INITIAL.
    p_dptc = '%'.
  ELSE.
    REPLACE ALL OCCURRENCES OF '*' IN p_dptc WITH '%'.
  ENDIF.

  CONVERT DATE p_date_f TIME p_time_f INTO TIME STAMP gv_timestamp_from TIME ZONE sy-zonlo.
  CONVERT DATE p_date_t TIME p_time_t INTO TIME STAMP gv_timestamp_to   TIME ZONE sy-zonlo.
ENDFORM.                    " INITIALIZE_SELECTION_PARAMS
*&---------------------------------------------------------------------*
*&      Form  ANALYSE_PPF_ACTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM analyse_ppf_actions .

  FIELD-SYMBOLS: <fs_actions_cust>        LIKE LINE OF gt_actions_cust,
                 <fs_actions_cust_common> LIKE LINE OF gt_actions_cust,
                 <fs_ppf_actions>         LIKE LINE OF gt_ppf_actions.

  DATA: lt_ppf_actions_scoped TYPE TABLE OF ppfttrigg,
        ls_actions_output      TYPE ts_actions_output.

  CLEAR:
    gt_actions_output.

  LOOP AT gt_actions_cust ASSIGNING <fs_actions_cust>.
    CLEAR:
      lt_ppf_actions_scoped,
      ls_actions_output.
    IF <fs_actions_cust>-common IS INITIAL.
      LOOP AT gt_ppf_actions ASSIGNING <fs_ppf_actions> WHERE applctn = <fs_actions_cust>-applctn
                                                        AND context = <fs_actions_cust>-context
                                                        AND ttype = <fs_actions_cust>-ttype.
        APPEND <fs_ppf_actions> TO lt_ppf_actions_scoped.
      ENDLOOP.

    ELSE.
      LOOP AT gt_actions_cust ASSIGNING <fs_actions_cust_common> WHERE cmnprfl = <fs_actions_cust>-context.
        LOOP AT gt_ppf_actions ASSIGNING <fs_ppf_actions> WHERE applctn = <fs_actions_cust_common>-applctn
                                                      AND context = <fs_actions_cust_common>-context
                                                      AND ttype = <fs_actions_cust_common>-ttype.
          APPEND <fs_ppf_actions> TO lt_ppf_actions_scoped.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    SORT lt_ppf_actions_scoped BY timecreate DESCENDING.

    MOVE-CORRESPONDING <fs_actions_cust> TO ls_actions_output.
    DESCRIBE TABLE lt_ppf_actions_scoped LINES ls_actions_output-count.

    UNASSIGN <fs_ppf_actions>.
    READ TABLE lt_ppf_actions_scoped ASSIGNING <fs_ppf_actions> INDEX 1.
    IF sy-subrc IS INITIAL AND <fs_ppf_actions> IS ASSIGNED.
      ls_actions_output-status = gc_icon_green.
      CONVERT TIME STAMP <fs_ppf_actions>-timecreate TIME ZONE sy-zonlo
        INTO DATE ls_actions_output-l_date TIME ls_actions_output-l_time.
    ELSE.
      IF <fs_actions_cust>-inactive IS INITIAL.
        ls_actions_output-status = gc_icon_yellow.
      ELSE.
        ls_actions_output-status = gc_icon_green.
      ENDIF.
    ENDIF.
    APPEND ls_actions_output TO gt_actions_output.
  ENDLOOP.
ENDFORM.                    " ANALYSE_PPF_ACTIONS
*&---------------------------------------------------------------------*
*&      Form  SIMPLE_WRITE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM simple_write_output .

  DATA: lt_components TYPE abap_component_tab.

  DATA: lr_data_descr TYPE REF TO cl_abap_datadescr,
        lv_length     TYPE i,
        lv_length_opt TYPE i,
        lv_position   TYPE i,
        lv_line_length TYPE i.
  FIELD-SYMBOLS: <fs_actions_output> LIKE LINE OF gt_actions_output,
                 <fs_components>     LIKE LINE OF lt_components,
                 <fv_component>      TYPE any.

  PERFORM get_output_structure_info CHANGING lt_components.
  SORT gt_actions_output BY status ASCENDING.

*  List Header:
  READ TABLE gt_actions_output ASSIGNING <fs_actions_output> INDEX 1.
  IF <fs_actions_output> IS NOT ASSIGNED.
    WRITE: 'No Data to display.'.
    EXIT.
  ENDIF.

  "get line length for uline
  LOOP AT lt_components ASSIGNING <fs_components>.
    ASSIGN COMPONENT <fs_components>-name OF STRUCTURE <fs_actions_output> TO <fv_component>.
    DESCRIBE FIELD <fv_component> OUTPUT-LENGTH lv_length.
    IF lv_length LT 12.
      lv_length = 12.
    ENDIF.
    ADD lv_length TO lv_line_length.
  ENDLOOP.

  WRITE:sy-uline(lv_line_length).
  NEW-LINE.
  lv_position = 0.
  LOOP AT lt_components ASSIGNING <fs_components>.
    ASSIGN COMPONENT <fs_components>-name OF STRUCTURE <fs_actions_output> TO <fv_component>.
    DESCRIBE FIELD <fv_component> OUTPUT-LENGTH lv_length.
    IF lv_length LT 12.
      lv_length = 12.
    ENDIF.
    WRITE: AT lv_position sy-vline, <fs_components>-name COLOR COL_HEADING INTENSIFIED ON.
    ADD lv_length TO lv_position.
  ENDLOOP.
  WRITE: AT lv_position sy-vline.
  WRITE: / sy-uline(lv_line_length).
  NEW-LINE.

*  DATA:
  LOOP AT gt_actions_output ASSIGNING <fs_actions_output>.
    lv_position = 0.
    LOOP AT lt_components ASSIGNING <fs_components>.
      ASSIGN COMPONENT <fs_components>-name OF STRUCTURE <fs_actions_output> TO <fv_component>.
      DESCRIBE FIELD <fv_component> OUTPUT-LENGTH lv_length.
      IF lv_length LT 12.
        lv_length_opt = 12.
      ELSE.
        lv_length_opt = lv_length.
      ENDIF.
      WRITE: AT lv_position sy-vline, <fv_component>.
      ADD lv_length_opt TO lv_position.
    ENDLOOP.
    WRITE: AT lv_position sy-vline.
    WRITE: / sy-uline(lv_line_length).
    NEW-LINE.
  ENDLOOP.
ENDFORM.                    " SIMPLE_WRITE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid_output .
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv.

  PERFORM build_fieldcat CHANGING lt_fieldcat.

  ls_layout-zebra = abap_true.
  ls_layout-colwidth_optimize = abap_true.

  SORT gt_actions_output BY status ASCENDING.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat
*     IT_EVENTS          =
    TABLES
      t_outtab           = gt_actions_output.

ENDFORM.                    " ALV_GRID_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_STRUCTURE_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_output_structure_info CHANGING ct_components.
  DATA: ls_actions_output TYPE ts_actions_output.

  DATA: lr_strucdescr TYPE REF TO cl_abap_structdescr.

  lr_strucdescr ?= cl_abap_structdescr=>describe_by_data( p_data =  ls_actions_output ).
  ct_components = lr_strucdescr->get_components( ).
ENDFORM.                    " GET_OUTPUT_STRUCTURE_INFO
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM build_fieldcat  CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: lt_components TYPE abap_component_tab,
        ls_fieldcat   TYPE slis_fieldcat_alv,
        lv_ddic       TYPE dd_x031l_table,
        lt_ddic_object TYPE dd_x031l_table,
        ls_ddic_object LIKE LINE OF lt_ddic_object.

  FIELD-SYMBOLS: <fs_components> LIKE LINE OF lt_components.

  PERFORM get_output_structure_info CHANGING lt_components.

  LOOP AT lt_components ASSIGNING <fs_components>.
    CLEAR: ls_fieldcat.

    <fs_components>-type->get_ddic_object(
      RECEIVING
        p_object     = lt_ddic_object    " Dictionary Object
      EXCEPTIONS
        not_found    = 1
        no_ddic_type = 2
        OTHERS       = 3
    ).
    IF sy-subrc = 0.
      READ TABLE lt_ddic_object INTO ls_ddic_object INDEX 1.
      IF sy-subrc IS INITIAL.
        ls_fieldcat-datatype = ls_ddic_object-dtyp.
        ls_fieldcat-intlen   = ls_ddic_object-dblength.
      ENDIF.
    ENDIF.
    ls_fieldcat-fieldname = <fs_components>-name.
    ls_fieldcat-seltext_s = <fs_components>-name.
    ls_fieldcat-seltext_m = <fs_components>-name.
    ls_fieldcat-seltext_l = <fs_components>-name.
    ls_fieldcat-just      = 'L'.

    APPEND ls_fieldcat TO p_lt_fieldcat.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  REMOVE_CORRECT_ACTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM remove_correct_actions .
  DELETE gt_actions_output WHERE status = gc_icon_green AND inactive IS INITIAL.
ENDFORM.                    " REMOVE_CORRECT_ACTIONS
*&---------------------------------------------------------------------*
*&      Form  TEST_PPF_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test_ppf_rfc .
  FIELD-SYMBOLS: <fs_actions_output> LIKE LINE OF gt_actions_output,
                 <fs_tab>            TYPE tab512.

  DATA: ls_query      TYPE rfc_db_opt,
        lt_query      TYPE TABLE OF rfc_db_opt,
        lt_tab        TYPE TABLE OF tab512,
        lt_field      TYPE TABLE OF rfc_db_fld,
        ls_field      TYPE rfc_db_fld,
        lv_timestamp  TYPE string,
        lv_last_time  LIKE ppfttrigg-timecreate.

  LOOP AT gt_actions_output ASSIGNING <fs_actions_output>.
    IF p_evina IS NOT INITIAL AND <fs_actions_output>-status NE gc_icon_yellow.
      CONTINUE.
    ENDIF.
    CLEAR: ls_query,
           lt_query,
           lt_field,
           lt_tab,
           lt_tab.

    CONCATENATE '''' <fs_actions_output>-applctn '''' INTO ls_query-text.
    CONCATENATE 'applctn =' ls_query-text INTO ls_query-text SEPARATED BY space.
    APPEND ls_query TO lt_query.

    CONCATENATE '''' <fs_actions_output>-context '''' INTO ls_query-text.
    CONCATENATE 'and context =' ls_query-text INTO ls_query-text SEPARATED BY space.
    APPEND ls_query TO lt_query.

    CONCATENATE '''' <fs_actions_output>-ttype '''' INTO ls_query-text.
    CONCATENATE 'and ttype =' ls_query-text INTO ls_query-text SEPARATED BY space.
    APPEND ls_query TO lt_query.

    IF p_date_f IS NOT INITIAL.
      lv_timestamp = gv_timestamp_from.
      CONCATENATE '''' lv_timestamp '''' INTO ls_query-text.
      CONCATENATE 'and timecreate GE' ls_query-text INTO ls_query-text SEPARATED BY space.
      APPEND ls_query TO lt_query.
    ENDIF.

    IF p_date_t IS NOT INITIAL.
      lv_timestamp = gv_timestamp_to.
      CONCATENATE '''' lv_timestamp '''' INTO ls_query-text.
      CONCATENATE 'and timecreate LE' ls_query-text INTO ls_query-text SEPARATED BY space.
      APPEND ls_query TO lt_query.
    ENDIF.

    CALL FUNCTION 'RFC_READ_TABLE'
      EXPORTING
        query_table          = 'PPFTTRIGG'
      TABLES
        options              = lt_query
        fields               = lt_field
        data                 = lt_tab
      EXCEPTIONS
        table_not_available  = 1
        table_without_data   = 2
        option_not_valid     = 3
        field_not_valid      = 4
        not_authorized       = 5
        data_buffer_exceeded = 6
        OTHERS               = 7.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF lt_tab IS NOT INITIAL.
      <fs_actions_output>-run_rfc = abap_true.
      CLEAR lv_last_time.
      READ TABLE lt_field INTO ls_field WITH KEY fieldname  = 'TIMECREATE'.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_tab ASSIGNING <fs_tab>.
          IF lv_last_time LT <fs_tab>-wa+ls_field-offset(ls_field-length).
            lv_last_time = <fs_tab>-wa+ls_field-offset(ls_field-length).
          ENDIF.
        ENDLOOP.
        CONVERT TIME STAMP lv_last_time TIME ZONE sy-zonlo INTO DATE <fs_actions_output>-rfc_date TIME <fs_actions_output>-rfc_time.
        DESCRIBE TABLE lt_tab LINES <fs_actions_output>-rfc_count.
        IF <fs_actions_output>-status NE gc_icon_green.
          <fs_actions_output>-status = gc_icon_rfc_run.
        ENDIF.
      ENDIF.
    ELSE.
      IF <fs_actions_output>-status NE gc_icon_yellow AND <fs_actions_output>-inactive IS INITIAL.
        <fs_actions_output>-status = gc_icon_rfc_red.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " TEST_PPF_RFC
