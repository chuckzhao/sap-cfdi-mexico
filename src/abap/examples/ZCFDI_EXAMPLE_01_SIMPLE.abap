*&---------------------------------------------------------------------*
*& Report ZCFDI_EXAMPLE_01_SIMPLE
*&---------------------------------------------------------------------*
*& Example 1: Simple CFDI Generation from Billing Document
*&---------------------------------------------------------------------*
*& This program demonstrates the simplest use case:
*& - Generate CFDI XML from an existing billing document
*& - Display the generated XML
*& - Show basic error handling
*&---------------------------------------------------------------------*

REPORT zcfdi_example_01_simple.

" Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_vbeln TYPE vbrk-vbeln OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

" Start of selection
START-OF-SELECTION.

  DATA: lo_generator TYPE REF TO zcl_cfdi_generator,
        lv_xml       TYPE string,
        lv_message   TYPE string.

  TRY.
      " Create instance of CFDI generator
      CREATE OBJECT lo_generator.

      " Generate CFDI XML from billing document
      lv_xml = lo_generator->generate_from_billing_doc(
        iv_vbeln   = p_vbeln
        iv_version = '4.0'
      ).

      " Display success message
      MESSAGE 'CFDI generated successfully' TYPE 'S'.

      " Display XML (first 1000 characters)
      WRITE: / 'Generated CFDI XML (preview):',
             / lv_xml+0(1000).

      " In real implementation, you would:
      " 1. Send XML to PAC for stamping
      " 2. Store the stamped XML
      " 3. Save UUID to billing document
      " 4. Generate PDF
      " 5. Email to customer

    CATCH cx_cfdi_generation INTO DATA(lx_gen).
      " Handle generation errors
      lv_message = lx_gen->get_text( ).
      MESSAGE lv_message TYPE 'E'.

    CATCH cx_cfdi_signature INTO DATA(lx_sig).
      " Handle signature errors
      lv_message = lx_sig->get_text( ).
      MESSAGE lv_message TYPE 'E'.

    CATCH cx_root INTO DATA(lx_root).
      " Handle any other errors
      lv_message = lx_root->get_text( ).
      MESSAGE lv_message TYPE 'E'.

  ENDTRY.
