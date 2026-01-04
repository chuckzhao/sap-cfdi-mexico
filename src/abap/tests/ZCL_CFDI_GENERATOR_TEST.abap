*----------------------------------------------------------------------*
* Unit Test Class: ZCL_CFDI_GENERATOR_TEST
*----------------------------------------------------------------------*
* Purpose: Unit tests for CFDI generator class
* Run with: SE38 -> Program -> Execute -> Unit Test (Ctrl+Shift+F10)
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Test Double for Billing Document Data
*----------------------------------------------------------------------*
CLASS lcl_test_data DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_test_vbrk
        RETURNING VALUE(rs_vbrk) TYPE vbrk,

      get_test_vbrp
        RETURNING VALUE(rt_vbrp) TYPE vbrp_tab,

      get_test_kna1
        RETURNING VALUE(rs_kna1) TYPE kna1,

      get_sample_xml
        RETURNING VALUE(rv_xml) TYPE string.
ENDCLASS.

CLASS lcl_test_data IMPLEMENTATION.

  METHOD get_test_vbrk.
    " Create test billing document header
    rs_vbrk-vbeln = '0090000001'.
    rs_vbrk-fkart = 'F2'.        " Invoice type
    rs_vbrk-fkdat = sy-datum.    " Billing date
    rs_vbrk-waerk = 'MXN'.       " Currency
    rs_vbrk-netwr = '10000.00'.  " Net value
    rs_vbrk-kunag = '0000001000'. " Customer
    rs_vbrk-bukrs = '1000'.      " Company code
    rs_vbrk-zlsch = 'B'.         " Payment method (Bank transfer)
  ENDMETHOD.

  METHOD get_test_vbrp.
    DATA: ls_vbrp TYPE vbrp.

    " Item 1
    ls_vbrp-vbeln = '0090000001'.
    ls_vbrp-posnr = '000010'.
    ls_vbrp-matnr = 'TEST-MAT-001'.
    ls_vbrp-arktx = 'Test Product Description'.
    ls_vbrp-fkimg = '10.000'.    " Quantity
    ls_vbrp-vrkme = 'EA'.        " Unit
    ls_vbrp-netwr = '5000.00'.   " Net value
    ls_vbrp-mwsbp = '800.00'.    " Tax amount (16%)
    APPEND ls_vbrp TO rt_vbrp.

    " Item 2
    ls_vbrp-posnr = '000020'.
    ls_vbrp-matnr = 'TEST-MAT-002'.
    ls_vbrp-arktx = 'Another Test Product'.
    ls_vbrp-fkimg = '5.000'.
    ls_vbrp-netwr = '5000.00'.
    ls_vbrp-mwsbp = '800.00'.
    APPEND ls_vbrp TO rt_vbrp.
  ENDMETHOD.

  METHOD get_test_kna1.
    " Create test customer data
    rs_kna1-kunnr = '0000001000'.
    rs_kna1-name1 = 'CLIENTE DE PRUEBA SA DE CV'.
    rs_kna1-stcd1 = 'XEXX010101000'.  " Generic test RFC
    rs_kna1-land1 = 'MX'.
    rs_kna1-pstlz = '06600'.          " Postal code
    rs_kna1-ort01 = 'Ciudad de México'.
  ENDMETHOD.

  METHOD get_sample_xml.
    " Return a sample valid CFDI XML for testing validation
    rv_xml = |<?xml version="1.0" encoding="UTF-8"?>| &&
             |<cfdi:Comprobante xmlns:cfdi="http://www.sat.gob.mx/cfd/4" | &&
             |xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" | &&
             |Version="4.0" Serie="A" Folio="1" Fecha="2024-01-15T10:30:00" | &&
             |FormaPago="03" SubTotal="10000.00" Moneda="MXN" Total="11600.00" | &&
             |TipoDeComprobante="I" MetodoPago="PUE" LugarExpedicion="06600" | &&
             |Exportacion="01">| &&
             |<cfdi:Emisor Rfc="XAXX010101000" Nombre="EMPRESA DE PRUEBA" | &&
             |RegimenFiscal="601"/>| &&
             |<cfdi:Receptor Rfc="XEXX010101000" Nombre="CLIENTE PRUEBA" | &&
             |DomicilioFiscalReceptor="06600" RegimenFiscalReceptor="601" | &&
             |UsoCFDI="G03"/>| &&
             |<cfdi:Conceptos>| &&
             |<cfdi:Concepto ClaveProdServ="01010101" NoIdentificacion="PROD001" | &&
             |Cantidad="10" ClaveUnidad="H87" Unidad="Pieza" | &&
             |Descripcion="Producto de prueba" ValorUnitario="1000.00" | &&
             |Importe="10000.00" ObjetoImp="02"/>| &&
             |</cfdi:Conceptos>| &&
             |<cfdi:Impuestos TotalImpuestosTrasladados="1600.00">| &&
             |<cfdi:Traslados>| &&
             |<cfdi:Traslado Base="10000.00" Impuesto="002" TipoFactor="Tasa" | &&
             |TasaOCuota="0.160000" Importe="1600.00"/>| &&
             |</cfdi:Traslados>| &&
             |</cfdi:Impuestos>| &&
             |</cfdi:Comprobante>|.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Unit Test Class
*----------------------------------------------------------------------*
CLASS ltc_cfdi_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_cfdi_generator.

    METHODS:
      setup,
      teardown,

      " Format tests
      test_format_datetime FOR TESTING,
      test_format_datetime_midnight FOR TESTING,

      " RFC validation tests
      test_validate_rfc_company_valid FOR TESTING,
      test_validate_rfc_person_valid FOR TESTING,
      test_validate_rfc_too_short FOR TESTING,
      test_validate_rfc_too_long FOR TESTING,
      test_validate_rfc_invalid_format FOR TESTING,
      test_validate_rfc_generic_public FOR TESTING,

      " XML validation tests
      test_validate_xml_valid FOR TESTING,
      test_validate_xml_missing_emisor FOR TESTING,
      test_validate_xml_missing_receptor FOR TESTING,
      test_validate_xml_malformed FOR TESTING,

      " Configuration tests
      test_get_configuration FOR TESTING.

ENDCLASS.

CLASS ltc_cfdi_generator IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut.
  ENDMETHOD.

  METHOD test_format_datetime.
    " Test date/time formatting to ISO 8601
    DATA(lv_result) = mo_cut->format_cfdi_datetime(
      iv_date = '20240115'
      iv_time = '103045'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '2024-01-15T10:30:45'
      msg = 'Date/time should be formatted as ISO 8601'
    ).
  ENDMETHOD.

  METHOD test_format_datetime_midnight.
    " Test midnight formatting
    DATA(lv_result) = mo_cut->format_cfdi_datetime(
      iv_date = '20240101'
      iv_time = '000000'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '2024-01-01T00:00:00'
      msg = 'Midnight should be formatted correctly'
    ).
  ENDMETHOD.

  METHOD test_validate_rfc_company_valid.
    " Test valid company RFC (12 chars)
    DATA(lv_result) = mo_cut->validate_rfc( 'ABC123456XY9' ).

    cl_abap_unit_assert=>assert_true(
      act = lv_result
      msg = 'Valid company RFC should return true'
    ).
  ENDMETHOD.

  METHOD test_validate_rfc_person_valid.
    " Test valid person RFC (13 chars)
    DATA(lv_result) = mo_cut->validate_rfc( 'ABCD123456XY9' ).

    cl_abap_unit_assert=>assert_true(
      act = lv_result
      msg = 'Valid person RFC should return true'
    ).
  ENDMETHOD.

  METHOD test_validate_rfc_too_short.
    " Test RFC that is too short
    DATA(lv_result) = mo_cut->validate_rfc( 'ABC12345' ).

    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = 'Short RFC should return false'
    ).
  ENDMETHOD.

  METHOD test_validate_rfc_too_long.
    " Test RFC that is too long
    DATA(lv_result) = mo_cut->validate_rfc( 'ABCD1234567890' ).

    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = 'Long RFC should return false'
    ).
  ENDMETHOD.

  METHOD test_validate_rfc_invalid_format.
    " Test RFC with invalid format (starts with numbers)
    DATA(lv_result) = mo_cut->validate_rfc( '123ABC456XY9' ).

    cl_abap_unit_assert=>assert_false(
      act = lv_result
      msg = 'Invalid format RFC should return false'
    ).
  ENDMETHOD.

  METHOD test_validate_rfc_generic_public.
    " Test generic public RFC (XAXX010101000)
    DATA(lv_result) = mo_cut->validate_rfc( 'XAXX010101000' ).

    cl_abap_unit_assert=>assert_true(
      act = lv_result
      msg = 'Generic public RFC should be valid'
    ).
  ENDMETHOD.

  METHOD test_validate_xml_valid.
    " Test validation of valid CFDI XML
    DATA(lv_xml) = lcl_test_data=>get_sample_xml( ).
    DATA(lt_errors) = mo_cut->validate_xml( lv_xml ).

    cl_abap_unit_assert=>assert_initial(
      act = lt_errors
      msg = 'Valid XML should have no errors'
    ).
  ENDMETHOD.

  METHOD test_validate_xml_missing_emisor.
    " Test validation with missing Emisor
    DATA(lv_xml) = |<?xml version="1.0"?>| &&
                   |<cfdi:Comprobante xmlns:cfdi="http://www.sat.gob.mx/cfd/4" | &&
                   |Version="4.0" Serie="A" Folio="1" Fecha="2024-01-15T10:30:00" | &&
                   |FormaPago="03" SubTotal="100.00" Moneda="MXN" Total="116.00" | &&
                   |TipoDeComprobante="I" MetodoPago="PUE" LugarExpedicion="06600" | &&
                   |Exportacion="01">| &&
                   |<cfdi:Receptor Rfc="XEXX010101000" Nombre="CLIENTE" UsoCFDI="G03"/>| &&
                   |<cfdi:Conceptos><cfdi:Concepto/></cfdi:Conceptos>| &&
                   |</cfdi:Comprobante>|.

    DATA(lt_errors) = mo_cut->validate_xml( lv_xml ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lt_errors
      msg = 'Missing Emisor should produce errors'
    ).

    " Check that error mentions Emisor
    DATA(lv_found) = abap_false.
    LOOP AT lt_errors INTO DATA(lv_error).
      IF lv_error CS 'Emisor'.
        lv_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true(
      act = lv_found
      msg = 'Error should mention missing Emisor'
    ).
  ENDMETHOD.

  METHOD test_validate_xml_missing_receptor.
    " Test validation with missing Receptor
    DATA(lv_xml) = |<?xml version="1.0"?>| &&
                   |<cfdi:Comprobante xmlns:cfdi="http://www.sat.gob.mx/cfd/4" | &&
                   |Version="4.0" Serie="A" Folio="1" Fecha="2024-01-15T10:30:00" | &&
                   |FormaPago="03" SubTotal="100.00" Moneda="MXN" Total="116.00" | &&
                   |TipoDeComprobante="I" MetodoPago="PUE" LugarExpedicion="06600" | &&
                   |Exportacion="01">| &&
                   |<cfdi:Emisor Rfc="XAXX010101000" Nombre="EMPRESA" RegimenFiscal="601"/>| &&
                   |<cfdi:Conceptos><cfdi:Concepto/></cfdi:Conceptos>| &&
                   |</cfdi:Comprobante>|.

    DATA(lt_errors) = mo_cut->validate_xml( lv_xml ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lt_errors
      msg = 'Missing Receptor should produce errors'
    ).
  ENDMETHOD.

  METHOD test_validate_xml_malformed.
    " Test validation of malformed XML
    DATA(lv_xml) = '<cfdi:Comprobante><not-closed>'.

    DATA(lt_errors) = mo_cut->validate_xml( lv_xml ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lt_errors
      msg = 'Malformed XML should produce errors'
    ).
  ENDMETHOD.

  METHOD test_get_configuration.
    " Test configuration retrieval
    DATA(ls_config) = mo_cut->get_configuration( ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_config-company_rfc
      msg = 'Configuration should have company RFC'
    ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_config-company_cp
      msg = 'Configuration should have postal code'
    ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Exception Class Tests
*----------------------------------------------------------------------*
CLASS ltc_cfdi_exceptions DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_generation_exception FOR TESTING,
      test_signature_exception FOR TESTING,
      test_exception_get_text FOR TESTING.

ENDCLASS.

CLASS ltc_cfdi_exceptions IMPLEMENTATION.

  METHOD test_generation_exception.
    " Test generation exception can be raised and caught
    TRY.
        RAISE EXCEPTION TYPE zcx_cfdi_generation
          EXPORTING
            textid = zcx_cfdi_generation=>billing_doc_not_found
            vbeln  = '0090000001'.

        cl_abap_unit_assert=>fail( 'Exception should have been raised' ).

      CATCH zcx_cfdi_generation INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Exception should be caught'
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_signature_exception.
    " Test signature exception can be raised and caught
    TRY.
        RAISE EXCEPTION TYPE zcx_cfdi_signature
          EXPORTING
            textid     = zcx_cfdi_signature=>certificate_not_found
            cert_alias = 'TEST_CERT'.

        cl_abap_unit_assert=>fail( 'Exception should have been raised' ).

      CATCH zcx_cfdi_signature INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_not_initial(
          act = lx_error
          msg = 'Exception should be caught'
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_exception_get_text.
    " Test exception text generation
    TRY.
        RAISE EXCEPTION TYPE zcx_cfdi_generation
          EXPORTING
            textid = zcx_cfdi_generation=>billing_doc_not_found
            vbeln  = '0090000001'.

      CATCH zcx_cfdi_generation INTO DATA(lx_error).
        DATA(lv_text) = lx_error->get_text( ).

        cl_abap_unit_assert=>assert_not_initial(
          act = lv_text
          msg = 'Exception text should not be empty'
        ).

        " Check text contains the document number
        cl_abap_unit_assert=>assert_char_cp(
          act = lv_text
          exp = '*0090000001*'
          msg = 'Exception text should contain document number'
        ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
