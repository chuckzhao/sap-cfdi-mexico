*&---------------------------------------------------------------------*
*& Report ZCFDI_EXAMPLE_02_WITH_PAC
*&---------------------------------------------------------------------*
*& Example 2: Complete CFDI Flow with PAC Integration
*&---------------------------------------------------------------------*
*& This program demonstrates the complete flow:
*& - Generate CFDI XML
*& - Validate XML
*& - Send to PAC for stamping
*& - Store UUID and stamped XML
*& - Update billing document status
*&---------------------------------------------------------------------*

REPORT zcfdi_example_02_with_pac.

" Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  p_vbeln TYPE vbrk-vbeln OBLIGATORY,
  p_pac   TYPE char10 DEFAULT 'MONTOVA'.
SELECTION-SCREEN END OF BLOCK b1.

" Global data
DATA: gv_xml        TYPE string,
      gv_signed_xml TYPE string,
      gv_uuid       TYPE char36,
      gt_errors     TYPE string_table.

" Start of selection
START-OF-SELECTION.
  PERFORM generate_cfdi.
  PERFORM validate_cfdi.
  PERFORM send_to_pac.
  PERFORM store_cfdi.
  PERFORM display_result.

*&---------------------------------------------------------------------*
*& Form generate_cfdi
*&---------------------------------------------------------------------*
*  Generate CFDI XML from billing document
*----------------------------------------------------------------------*
FORM generate_cfdi.

  DATA: lo_generator TYPE REF TO zcl_cfdi_generator.

  TRY.
      CREATE OBJECT lo_generator.

      " Generate XML
      gv_xml = lo_generator->generate_from_billing_doc(
        iv_vbeln   = p_vbeln
        iv_version = '4.0'
      ).

      WRITE: / '✓ CFDI XML generated successfully'.

    CATCH cx_cfdi_generation INTO DATA(lx_error).
      WRITE: / '✗ Error generating CFDI:', lx_error->get_text( ).
      STOP.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_cfdi
*&---------------------------------------------------------------------*
*  Validate CFDI before sending to PAC
*----------------------------------------------------------------------*
FORM validate_cfdi.

  DATA: lo_generator TYPE REF TO zcl_cfdi_generator.

  CREATE OBJECT lo_generator.

  " Validate XML
  gt_errors = lo_generator->validate_xml( gv_xml ).

  IF gt_errors IS INITIAL.
    WRITE: / '✓ CFDI validation passed'.
  ELSE.
    WRITE: / '✗ CFDI validation failed:'.
    LOOP AT gt_errors INTO DATA(lv_error).
      WRITE: /5 lv_error.
    ENDLOOP.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form send_to_pac
*&---------------------------------------------------------------------*
*  Send CFDI to PAC for stamping
*----------------------------------------------------------------------*
FORM send_to_pac.

  DATA: lo_pac TYPE REF TO object.  " Generic reference

  " Select PAC implementation based on parameter
  CASE p_pac.
    WHEN 'MONTOVA'.
      " CREATE OBJECT lo_pac TYPE zcl_pac_montova.

    WHEN 'EDICOM'.
      " CREATE OBJECT lo_pac TYPE zcl_pac_edicom.

    WHEN OTHERS.
      WRITE: / '✗ Unknown PAC provider:', p_pac.
      STOP.
  ENDCASE.

  " In real implementation:
  " 1. Call PAC API
  " 2. Handle authentication
  " 3. Send XML
  " 4. Receive stamped XML and UUID
  " 5. Handle errors and retries

  " Simulated for example:
  gv_signed_xml = gv_xml.  " Would be stamped XML from PAC
  gv_uuid = '12345678-1234-1234-1234-123456789012'.  " From PAC

  WRITE: / '✓ CFDI stamped by PAC'.
  WRITE: /5 'UUID:', gv_uuid.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form store_cfdi
*&---------------------------------------------------------------------*
*  Store UUID and stamped XML in SAP
*----------------------------------------------------------------------*
FORM store_cfdi.

  " In real implementation:
  " 1. Update billing document with UUID
  " 2. Store stamped XML in archive
  " 3. Update document status
  " 4. Create application log entry

  " Example update statement (pseudo-code):
  " UPDATE vbrk SET zzuuid = gv_uuid
  "              zzstamp_date = sy-datum
  "              zzstamp_time = sy-uzeit
  "         WHERE vbeln = p_vbeln.

  WRITE: / '✓ UUID and XML stored successfully'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_result
*&---------------------------------------------------------------------*
*  Display final result
*----------------------------------------------------------------------*
FORM display_result.

  WRITE: / repeat( '-', 70 ).
  WRITE: / 'CFDI Generation Complete'.
  WRITE: / repeat( '-', 70 ).
  WRITE: / 'Billing Document:', p_vbeln.
  WRITE: / 'UUID:', gv_uuid.
  WRITE: / 'PAC Provider:', p_pac.
  WRITE: / 'Status: Success'.
  WRITE: / repeat( '-', 70 ).

  " Next steps message
  WRITE: / '',
         / 'Next steps:',
         / '1. Generate PDF from stamped XML',
         / '2. Email XML and PDF to customer',
         / '3. Archive documents for 5 years',
         / '4. Verify UUID on SAT portal'.

ENDFORM.
