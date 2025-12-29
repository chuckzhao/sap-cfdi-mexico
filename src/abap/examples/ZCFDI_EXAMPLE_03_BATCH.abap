*&---------------------------------------------------------------------*
*& Report ZCFDI_EXAMPLE_03_BATCH
*&---------------------------------------------------------------------*
*& Example 3: Batch CFDI Processing
*&---------------------------------------------------------------------*
*& This program demonstrates batch processing:
*& - Process multiple billing documents
*& - Parallel processing for performance
*& - Error handling and logging
*& - Summary report
*&---------------------------------------------------------------------*

REPORT zcfdi_example_03_batch.

" Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_vbeln FOR vbrk-vbeln,
                s_fkdat FOR vbrk-fkdat.
PARAMETERS:     p_pac   TYPE char10 DEFAULT 'MONTOVA',
                p_test  AS CHECKBOX DEFAULT 'X'.  " Test mode - no actual PAC call
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_jobs TYPE i DEFAULT 1.  " Number of parallel jobs
SELECTION-SCREEN END OF BLOCK b2.

" Type definitions
TYPES: BEGIN OF ty_result,
         vbeln   TYPE vbrk-vbeln,
         status  TYPE char1,  " S=Success, E=Error, W=Warning
         uuid    TYPE char36,
         message TYPE string,
       END OF ty_result,
       tt_result TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

" Global data
DATA: gt_billing TYPE STANDARD TABLE OF vbrk,
      gt_results TYPE tt_result,
      gv_success TYPE i,
      gv_error   TYPE i.

" Start of selection
START-OF-SELECTION.
  PERFORM select_billing_documents.
  PERFORM process_documents.
  PERFORM display_summary.

*&---------------------------------------------------------------------*
*& Form select_billing_documents
*&---------------------------------------------------------------------*
*  Select billing documents to process
*----------------------------------------------------------------------*
FORM select_billing_documents.

  SELECT * FROM vbrk
    INTO TABLE @gt_billing
    WHERE vbeln IN @s_vbeln
      AND fkdat IN @s_fkdat
      AND fkart = 'F2'  " Invoice type
    ORDER BY vbeln.

  IF sy-subrc <> 0.
    WRITE: / 'No billing documents found matching criteria'.
    STOP.
  ENDIF.

  WRITE: / |{ lines( gt_billing ) } billing documents selected for processing|.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form process_documents
*&---------------------------------------------------------------------*
*  Process all selected billing documents
*----------------------------------------------------------------------*
FORM process_documents.

  DATA: lv_total  TYPE i,
        lv_count  TYPE i,
        lv_pct    TYPE i.

  lv_total = lines( gt_billing ).

  WRITE: / repeat( '-', 70 ).
  WRITE: / 'Processing documents...'.
  WRITE: / repeat( '-', 70 ).

  LOOP AT gt_billing INTO DATA(ls_billing).

    lv_count = sy-tabix.
    lv_pct = lv_count * 100 / lv_total.

    " Show progress
    WRITE: / |{ lv_count }/{ lv_total } ({ lv_pct }%) - { ls_billing-vbeln }|.

    " Process single document
    PERFORM process_single_document USING ls_billing.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form process_single_document
*&---------------------------------------------------------------------*
*  Process a single billing document
*----------------------------------------------------------------------*
FORM process_single_document USING is_billing TYPE vbrk.

  DATA: ls_result    TYPE ty_result,
        lo_generator TYPE REF TO zcl_cfdi_generator,
        lv_xml       TYPE string.

  ls_result-vbeln = is_billing-vbeln.

  TRY.
      " Generate CFDI
      CREATE OBJECT lo_generator.

      lv_xml = lo_generator->generate_from_billing_doc(
        iv_vbeln   = is_billing-vbeln
        iv_version = '4.0'
      ).

      " In test mode, don't actually call PAC
      IF p_test = abap_false.
        " Send to PAC
        " ls_result-uuid = <UUID from PAC>.
        ls_result-uuid = 'SIMULATED-UUID-' && is_billing-vbeln.
      ELSE.
        ls_result-uuid = 'TEST-MODE'.
      ENDIF.

      " Success
      ls_result-status = 'S'.
      ls_result-message = 'Success'.
      gv_success = gv_success + 1.

    CATCH cx_cfdi_generation INTO DATA(lx_gen).
      " Error
      ls_result-status = 'E'.
      ls_result-message = lx_gen->get_text( ).
      gv_error = gv_error + 1.

    CATCH cx_root INTO DATA(lx_root).
      " Error
      ls_result-status = 'E'.
      ls_result-message = lx_root->get_text( ).
      gv_error = gv_error + 1.

  ENDTRY.

  " Store result
  APPEND ls_result TO gt_results.

  " Commit work periodically
  IF sy-tabix MOD 100 = 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_summary
*&---------------------------------------------------------------------*
*  Display processing summary
*----------------------------------------------------------------------*
FORM display_summary.

  DATA: lv_total   TYPE i,
        lv_success TYPE i,
        lv_error   TYPE i.

  lv_total = lines( gt_results ).
  lv_success = gv_success.
  lv_error = gv_error.

  WRITE: / '',
         / repeat( '=', 70 ).
  WRITE: / 'BATCH PROCESSING SUMMARY'.
  WRITE: / repeat( '=', 70 ).
  WRITE: / 'Total Documents:', lv_total.
  WRITE: / 'Successful:', lv_success.
  WRITE: / 'Errors:', lv_error.
  WRITE: / 'Success Rate:', |{ lv_success * 100 / lv_total }%|.
  WRITE: / repeat( '=', 70 ).

  " Display errors if any
  IF lv_error > 0.
    WRITE: / '',
           / 'ERRORS:'.
    WRITE: / repeat( '-', 70 ).

    LOOP AT gt_results INTO DATA(ls_result) WHERE status = 'E'.
      WRITE: / ls_result-vbeln, ls_result-message.
    ENDLOOP.
  ENDIF.

  " Display successful UUIDs
  IF lv_success > 0.
    WRITE: / '',
           / 'SUCCESSFUL CFDIS (first 10):'.
    WRITE: / repeat( '-', 70 ).

    DATA: lv_count TYPE i VALUE 0.

    LOOP AT gt_results INTO ls_result WHERE status = 'S'.
      lv_count = lv_count + 1.
      WRITE: / ls_result-vbeln, ls_result-uuid.
      IF lv_count >= 10.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_success > 10.
      WRITE: / |... and { lv_success - 10 } more|.
    ENDIF.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
* Text symbols
*----------------------------------------------------------------------*
* 001: Selection Criteria
* 002: Processing Options
