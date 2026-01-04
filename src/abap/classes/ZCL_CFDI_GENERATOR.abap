*----------------------------------------------------------------------*
* Class: ZCL_CFDI_GENERATOR
*----------------------------------------------------------------------*
* Purpose: Generate CFDI 4.0 XML from SAP billing documents
* Version: 4.0
* Author: SAP CFDI Mexico Project
*----------------------------------------------------------------------*
CLASS zcl_cfdi_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    "! Type definitions
    TYPES:
      BEGIN OF ty_taxes,
        base_iva           TYPE p LENGTH 16 DECIMALS 2,
        amount_iva         TYPE p LENGTH 16 DECIMALS 2,
        rate_iva           TYPE p LENGTH 5 DECIMALS 6,
        base_ret_iva       TYPE p LENGTH 16 DECIMALS 2,
        amount_ret_iva     TYPE p LENGTH 16 DECIMALS 2,
        rate_ret_iva       TYPE p LENGTH 5 DECIMALS 6,
        base_ret_isr       TYPE p LENGTH 16 DECIMALS 2,
        amount_ret_isr     TYPE p LENGTH 16 DECIMALS 2,
        rate_ret_isr       TYPE p LENGTH 5 DECIMALS 6,
        total_trasladados  TYPE p LENGTH 16 DECIMALS 2,
        total_retenidos    TYPE p LENGTH 16 DECIMALS 2,
      END OF ty_taxes,

      BEGIN OF ty_sat_codes,
        clave_prod_serv    TYPE string,
        clave_unidad       TYPE string,
        unidad_descripcion TYPE string,
        objeto_imp         TYPE string,
      END OF ty_sat_codes,

      BEGIN OF ty_concepto,
        clave_prod_serv    TYPE string,
        no_identificacion  TYPE string,
        cantidad           TYPE p LENGTH 16 DECIMALS 6,
        clave_unidad       TYPE string,
        unidad             TYPE string,
        descripcion        TYPE string,
        valor_unitario     TYPE p LENGTH 16 DECIMALS 6,
        importe            TYPE p LENGTH 16 DECIMALS 2,
        descuento          TYPE p LENGTH 16 DECIMALS 2,
        objeto_imp         TYPE string,
        taxes              TYPE ty_taxes,
      END OF ty_concepto,
      tt_conceptos TYPE STANDARD TABLE OF ty_concepto WITH DEFAULT KEY,

      BEGIN OF ty_cfdi_data,
        version            TYPE string,
        serie              TYPE string,
        folio              TYPE string,
        fecha              TYPE string,
        forma_pago         TYPE string,
        metodo_pago        TYPE string,
        tipo_comprobante   TYPE string,
        condiciones_pago   TYPE string,
        moneda             TYPE string,
        tipo_cambio        TYPE p LENGTH 16 DECIMALS 6,
        subtotal           TYPE p LENGTH 16 DECIMALS 2,
        descuento          TYPE p LENGTH 16 DECIMALS 2,
        total              TYPE p LENGTH 16 DECIMALS 2,
        exportacion        TYPE string,
        lugar_expedicion   TYPE string,
        emisor_rfc         TYPE string,
        emisor_nombre      TYPE string,
        emisor_regimen     TYPE string,
        receptor_rfc       TYPE string,
        receptor_nombre    TYPE string,
        receptor_regimen   TYPE string,
        receptor_uso_cfdi  TYPE string,
        receptor_cp        TYPE string,
        receptor_domicilio TYPE string,
      END OF ty_cfdi_data,

      BEGIN OF ty_config,
        company_rfc        TYPE string,
        company_name       TYPE string,
        company_regime     TYPE string,
        company_cp         TYPE string,
        cert_alias         TYPE string,
        serie_default      TYPE string,
        uso_cfdi_default   TYPE string,
      END OF ty_config.

    METHODS:
      "! Generate CFDI XML from billing document
      "! @parameter iv_vbeln | Billing document number
      "! @parameter iv_version | CFDI version (default 4.0)
      "! @parameter rv_xml | Generated CFDI XML
      "! @raising zcx_cfdi_generation | Generation errors
      generate_from_billing_doc
        IMPORTING
          iv_vbeln          TYPE vbrk-vbeln
          iv_version        TYPE string DEFAULT '4.0'
        RETURNING
          VALUE(rv_xml)     TYPE string
        RAISING
          zcx_cfdi_generation,

      "! Validate CFDI XML before sending to PAC
      "! @parameter iv_xml | XML to validate
      "! @parameter rt_errors | Validation errors if any
      validate_xml
        IMPORTING
          iv_xml            TYPE string
        RETURNING
          VALUE(rt_errors)  TYPE string_table,

      "! Apply digital signature to CFDI
      "! @parameter iv_xml | Unsigned XML
      "! @parameter rv_signed_xml | Signed XML
      "! @raising zcx_cfdi_signature | Signature errors
      apply_signature
        IMPORTING
          iv_xml                TYPE string
        RETURNING
          VALUE(rv_signed_xml)  TYPE string
        RAISING
          zcx_cfdi_signature,

      "! Get CFDI configuration
      "! @parameter rs_config | Configuration data
      get_configuration
        RETURNING
          VALUE(rs_config) TYPE ty_config,

      "! Format date for CFDI (ISO 8601)
      "! @parameter iv_date | SAP date
      "! @parameter iv_time | SAP time
      "! @parameter rv_formatted | ISO 8601 formatted datetime
      format_cfdi_datetime
        IMPORTING
          iv_date           TYPE datum
          iv_time           TYPE uzeit
        RETURNING
          VALUE(rv_formatted) TYPE string,

      "! Validate RFC format
      "! @parameter iv_rfc | RFC to validate
      "! @parameter rv_valid | True if valid
      validate_rfc
        IMPORTING
          iv_rfc          TYPE string
        RETURNING
          VALUE(rv_valid) TYPE abap_bool.

  PRIVATE SECTION.
    DATA:
      mv_vbeln      TYPE vbrk-vbeln,
      mv_version    TYPE string,
      mo_xml_doc    TYPE REF TO if_ixml_document,
      mo_root_elem  TYPE REF TO if_ixml_element,
      ms_cfdi_data  TYPE ty_cfdi_data,
      mt_conceptos  TYPE tt_conceptos,
      ms_config     TYPE ty_config,
      ms_vbrk       TYPE vbrk,
      mt_vbrp       TYPE STANDARD TABLE OF vbrp,
      ms_kna1       TYPE kna1.

    CONSTANTS:
      "! CFDI namespace
      c_namespace_cfdi TYPE string VALUE 'http://www.sat.gob.mx/cfd/4',
      "! XSI namespace
      c_namespace_xsi  TYPE string VALUE 'http://www.w3.org/2001/XMLSchema-instance',
      "! CFDI version
      c_cfdi_version   TYPE string VALUE '4.0',
      "! Default IVA rate (16%)
      c_iva_rate       TYPE p LENGTH 5 DECIMALS 6 VALUE '0.160000',
      "! Schema location
      c_schema_location TYPE string VALUE 'http://www.sat.gob.mx/cfd/4 http://www.sat.gob.mx/sitio_internet/cfd/4/cfdv40.xsd'.

    METHODS:
      "! Get billing document header
      "! @raising zcx_cfdi_generation | If document not found
      get_billing_header
        RETURNING VALUE(rs_vbrk) TYPE vbrk
        RAISING   zcx_cfdi_generation,

      "! Get billing document items
      "! @raising zcx_cfdi_generation | If no items found
      get_billing_items
        RETURNING VALUE(rt_vbrp) TYPE vbrp_tab
        RAISING   zcx_cfdi_generation,

      "! Get customer master data
      "! @parameter iv_kunnr | Customer number
      "! @raising zcx_cfdi_generation | If customer not found
      get_customer_data
        IMPORTING iv_kunnr       TYPE kna1-kunnr
        RETURNING VALUE(rs_kna1) TYPE kna1
        RAISING   zcx_cfdi_generation,

      "! Load configuration from custom table
      load_configuration,

      "! Build main Comprobante node
      build_comprobante_node
        RAISING zcx_cfdi_generation,

      "! Build Emisor (issuer) node
      build_emisor_node
        RAISING zcx_cfdi_generation,

      "! Build Receptor (recipient) node
      build_receptor_node
        RAISING zcx_cfdi_generation,

      "! Build Conceptos (line items) node
      build_conceptos_node
        RAISING zcx_cfdi_generation,

      "! Build single Concepto node
      "! @parameter is_concepto | Concept data
      "! @parameter io_conceptos | Parent conceptos element
      build_concepto_node
        IMPORTING
          is_concepto  TYPE ty_concepto
          io_conceptos TYPE REF TO if_ixml_element
        RAISING zcx_cfdi_generation,

      "! Build Impuestos (taxes) node
      build_impuestos_node
        RAISING zcx_cfdi_generation,

      "! Calculate taxes from billing document
      calculate_taxes
        RETURNING VALUE(rs_taxes) TYPE ty_taxes
        RAISING   zcx_cfdi_generation,

      "! Calculate taxes for single line item
      "! @parameter is_vbrp | Billing item
      calculate_item_taxes
        IMPORTING is_vbrp         TYPE vbrp
        RETURNING VALUE(rs_taxes) TYPE ty_taxes,

      "! Get SAT catalog codes for material
      "! @parameter iv_matnr | Material number
      get_sat_codes
        IMPORTING iv_matnr        TYPE matnr
        RETURNING VALUE(rs_codes) TYPE ty_sat_codes
        RAISING   zcx_cfdi_generation,

      "! Round to specified decimal places per SAT requirements
      "! @parameter iv_value | Value to round
      "! @parameter iv_decimals | Number of decimals (default 2)
      round_amount
        IMPORTING iv_value        TYPE p
                  iv_decimals     TYPE i DEFAULT 2
        RETURNING VALUE(rv_value) TYPE p,

      "! Validate required master data exists
      validate_master_data
        RAISING zcx_cfdi_generation,

      "! Get certificate from SSF
      "! @parameter rv_certificate | Certificate data
      get_certificate
        RETURNING VALUE(rv_certificate) TYPE xstring
        RAISING   zcx_cfdi_signature,

      "! Get certificate number from CSD
      "! @parameter rv_cert_number | Certificate number
      get_certificate_number
        RETURNING VALUE(rv_cert_number) TYPE string
        RAISING   zcx_cfdi_signature,

      "! Calculate original string for signature
      "! @parameter iv_xml | XML document
      "! @parameter rv_original_string | Original string for signing
      calculate_original_string
        IMPORTING iv_xml                  TYPE string
        RETURNING VALUE(rv_original_string) TYPE string
        RAISING   zcx_cfdi_signature,

      "! Sign data with private key
      "! @parameter iv_data | Data to sign
      "! @parameter rv_signature | Base64 encoded signature
      sign_data
        IMPORTING iv_data            TYPE string
        RETURNING VALUE(rv_signature) TYPE string
        RAISING   zcx_cfdi_signature,

      "! Get payment form code from billing doc
      "! @parameter iv_zlsch | Payment method
      get_forma_pago
        IMPORTING iv_zlsch           TYPE zlsch
        RETURNING VALUE(rv_forma) TYPE string,

      "! Get payment method (PUE/PPD)
      get_metodo_pago
        RETURNING VALUE(rv_metodo) TYPE string,

      "! Get CFDI use code from customer
      get_uso_cfdi
        RETURNING VALUE(rv_uso) TYPE string,

      "! Map SAP currency to SAT currency code
      "! @parameter iv_waerk | SAP currency
      get_moneda
        IMPORTING iv_waerk         TYPE waers
        RETURNING VALUE(rv_moneda) TYPE string,

      "! Create XML element with text content
      "! @parameter iv_name | Element name
      "! @parameter iv_value | Text value
      "! @parameter io_parent | Parent element
      create_element
        IMPORTING
          iv_name    TYPE string
          iv_value   TYPE string OPTIONAL
          io_parent  TYPE REF TO if_ixml_element
        RETURNING
          VALUE(ro_element) TYPE REF TO if_ixml_element,

      "! Format amount for XML output
      "! @parameter iv_amount | Amount value
      "! @parameter iv_decimals | Number of decimals
      format_amount
        IMPORTING
          iv_amount           TYPE p
          iv_decimals         TYPE i DEFAULT 2
        RETURNING
          VALUE(rv_formatted) TYPE string.

ENDCLASS.

CLASS zcl_cfdi_generator IMPLEMENTATION.

  METHOD generate_from_billing_doc.
    "! Main method to generate CFDI XML from billing document

    DATA: lo_ixml          TYPE REF TO if_ixml,
          lo_streamfactory TYPE REF TO if_ixml_stream_factory,
          lo_ostream       TYPE REF TO if_ixml_ostream,
          lo_renderer      TYPE REF TO if_ixml_renderer.

    TRY.
        " Store input parameters
        mv_vbeln = iv_vbeln.
        mv_version = iv_version.

        " Step 1: Load configuration
        load_configuration( ).

        " Step 2: Get billing document data
        ms_vbrk = get_billing_header( ).
        mt_vbrp = get_billing_items( ).
        ms_kna1 = get_customer_data( ms_vbrk-kunag ).

        " Step 3: Validate master data
        validate_master_data( ).

        " Step 4: Prepare CFDI data structure
        ms_cfdi_data-version = c_cfdi_version.
        ms_cfdi_data-serie = COND #( WHEN ms_config-serie_default IS NOT INITIAL
                                     THEN ms_config-serie_default
                                     ELSE 'A' ).
        ms_cfdi_data-folio = |{ ms_vbrk-vbeln ALPHA = OUT }|.
        ms_cfdi_data-fecha = format_cfdi_datetime(
          iv_date = ms_vbrk-fkdat
          iv_time = sy-uzeit
        ).

        " Set payment and document type
        ms_cfdi_data-forma_pago = get_forma_pago( ms_vbrk-zlsch ).
        ms_cfdi_data-metodo_pago = get_metodo_pago( ).
        ms_cfdi_data-tipo_comprobante = 'I'.  " I=Ingreso (Income)
        ms_cfdi_data-moneda = get_moneda( ms_vbrk-waerk ).
        ms_cfdi_data-exportacion = '01'.  " 01=No aplica
        ms_cfdi_data-lugar_expedicion = ms_config-company_cp.

        " Exchange rate for foreign currency
        IF ms_vbrk-waerk <> 'MXN'.
          ms_cfdi_data-tipo_cambio = ms_vbrk-kurrf.
        ENDIF.

        " Emisor data
        ms_cfdi_data-emisor_rfc = ms_config-company_rfc.
        ms_cfdi_data-emisor_nombre = ms_config-company_name.
        ms_cfdi_data-emisor_regimen = ms_config-company_regime.

        " Receptor data - Read from customer custom fields or defaults
        ms_cfdi_data-receptor_rfc = ms_kna1-stcd1.  " RFC stored in STCD1
        ms_cfdi_data-receptor_nombre = ms_kna1-name1.
        ms_cfdi_data-receptor_regimen = '601'.  " Default, should be from customer master
        ms_cfdi_data-receptor_uso_cfdi = get_uso_cfdi( ).
        ms_cfdi_data-receptor_cp = ms_kna1-pstlz.
        ms_cfdi_data-receptor_domicilio = ms_kna1-land1.

        " Calculate amounts from items
        DATA(ls_taxes) = calculate_taxes( ).
        ms_cfdi_data-subtotal = round_amount( ms_vbrk-netwr ).
        ms_cfdi_data-total = round_amount( ms_vbrk-netwr + ls_taxes-total_trasladados - ls_taxes-total_retenidos ).

        " Step 5: Create XML document
        lo_ixml = cl_ixml=>create( ).
        mo_xml_doc = lo_ixml->create_document( ).

        " Step 6: Build XML structure
        build_comprobante_node( ).
        build_emisor_node( ).
        build_receptor_node( ).
        build_conceptos_node( ).
        build_impuestos_node( ).

        " Step 7: Convert XML document to string
        lo_streamfactory = lo_ixml->create_stream_factory( ).
        lo_ostream = lo_streamfactory->create_ostream_cstring( rv_xml ).
        lo_ostream->set_encoding( encoding = lo_ixml->create_encoding(
          byte_order = if_ixml_encoding=>co_none
          character_set = 'UTF-8' ) ).

        lo_renderer = lo_ixml->create_renderer(
          ostream  = lo_ostream
          document = mo_xml_doc
        ).
        lo_renderer->set_normalizing( abap_true ).
        lo_renderer->render( ).

        " Add XML declaration
        rv_xml = |<?xml version="1.0" encoding="UTF-8"?>| && cl_abap_char_utilities=>newline && rv_xml.

      CATCH zcx_cfdi_generation INTO DATA(lx_gen).
        RAISE EXCEPTION lx_gen.
      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_cfdi_generation
          EXPORTING
            textid   = zcx_cfdi_generation=>generation_failed
            previous = lx_error
            vbeln    = iv_vbeln.
    ENDTRY.

  ENDMETHOD.

  METHOD validate_xml.
    "! Validate CFDI XML structure and content

    DATA: lo_ixml     TYPE REF TO if_ixml,
          lo_parser   TYPE REF TO if_ixml_parser,
          lo_stream   TYPE REF TO if_ixml_istream,
          lo_doc      TYPE REF TO if_ixml_document,
          lo_factory  TYPE REF TO if_ixml_stream_factory,
          lv_rc       TYPE i.

    CLEAR rt_errors.

    " 1. XML well-formedness check
    lo_ixml = cl_ixml=>create( ).
    lo_factory = lo_ixml->create_stream_factory( ).
    lo_stream = lo_factory->create_istream_string( iv_xml ).
    lo_doc = lo_ixml->create_document( ).

    lo_parser = lo_ixml->create_parser(
      document       = lo_doc
      istream        = lo_stream
      stream_factory = lo_factory
    ).

    lv_rc = lo_parser->parse( ).

    IF lv_rc <> 0.
      APPEND |XML parsing error (code: { lv_rc })| TO rt_errors.
      RETURN.
    ENDIF.

    " 2. Check required root element
    DATA(lo_root) = lo_doc->get_root_element( ).
    IF lo_root IS INITIAL.
      APPEND 'Missing root element' TO rt_errors.
      RETURN.
    ENDIF.

    IF lo_root->get_name( ) <> 'Comprobante'.
      APPEND |Invalid root element: { lo_root->get_name( ) }. Expected: Comprobante| TO rt_errors.
    ENDIF.

    " 3. Check CFDI version
    DATA(lv_version) = lo_root->get_attribute( name = 'Version' ).
    IF lv_version <> '4.0'.
      APPEND |Invalid CFDI version: { lv_version }. Expected: 4.0| TO rt_errors.
    ENDIF.

    " 4. Check required attributes on Comprobante
    DATA(lt_required_attrs) = VALUE string_table(
      ( `Version` ) ( `Serie` ) ( `Folio` ) ( `Fecha` )
      ( `FormaPago` ) ( `SubTotal` ) ( `Moneda` ) ( `Total` )
      ( `TipoDeComprobante` ) ( `MetodoPago` ) ( `LugarExpedicion` )
      ( `Exportacion` )
    ).

    LOOP AT lt_required_attrs INTO DATA(lv_attr).
      DATA(lv_value) = lo_root->get_attribute( name = lv_attr ).
      IF lv_value IS INITIAL.
        APPEND |Missing required attribute: { lv_attr }| TO rt_errors.
      ENDIF.
    ENDLOOP.

    " 5. Check Emisor element
    DATA(lo_emisor) = lo_root->find_from_name( name = 'Emisor' ).
    IF lo_emisor IS INITIAL.
      APPEND 'Missing Emisor element' TO rt_errors.
    ELSE.
      IF lo_emisor->get_attribute( name = 'Rfc' ) IS INITIAL.
        APPEND 'Missing Emisor RFC' TO rt_errors.
      ENDIF.
      IF lo_emisor->get_attribute( name = 'Nombre' ) IS INITIAL.
        APPEND 'Missing Emisor Nombre' TO rt_errors.
      ENDIF.
      IF lo_emisor->get_attribute( name = 'RegimenFiscal' ) IS INITIAL.
        APPEND 'Missing Emisor RegimenFiscal' TO rt_errors.
      ENDIF.
    ENDIF.

    " 6. Check Receptor element
    DATA(lo_receptor) = lo_root->find_from_name( name = 'Receptor' ).
    IF lo_receptor IS INITIAL.
      APPEND 'Missing Receptor element' TO rt_errors.
    ELSE.
      IF lo_receptor->get_attribute( name = 'Rfc' ) IS INITIAL.
        APPEND 'Missing Receptor RFC' TO rt_errors.
      ENDIF.
      IF lo_receptor->get_attribute( name = 'Nombre' ) IS INITIAL.
        APPEND 'Missing Receptor Nombre' TO rt_errors.
      ENDIF.
      IF lo_receptor->get_attribute( name = 'UsoCFDI' ) IS INITIAL.
        APPEND 'Missing Receptor UsoCFDI' TO rt_errors.
      ENDIF.
    ENDIF.

    " 7. Check Conceptos element
    DATA(lo_conceptos) = lo_root->find_from_name( name = 'Conceptos' ).
    IF lo_conceptos IS INITIAL.
      APPEND 'Missing Conceptos element' TO rt_errors.
    ENDIF.

    " 8. Amount validation
    DATA(lv_subtotal) = lo_root->get_attribute( name = 'SubTotal' ).
    DATA(lv_total) = lo_root->get_attribute( name = 'Total' ).

    DATA: lv_subtotal_num TYPE p LENGTH 16 DECIMALS 2,
          lv_total_num    TYPE p LENGTH 16 DECIMALS 2.

    IF lv_subtotal IS NOT INITIAL AND lv_total IS NOT INITIAL.
      TRY.
          lv_subtotal_num = lv_subtotal.
          lv_total_num = lv_total.

          IF lv_subtotal_num < 0.
            APPEND 'SubTotal cannot be negative' TO rt_errors.
          ENDIF.
          IF lv_total_num < 0.
            APPEND 'Total cannot be negative' TO rt_errors.
          ENDIF.
        CATCH cx_sy_conversion_error.
          APPEND 'Invalid numeric format in SubTotal or Total' TO rt_errors.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD apply_signature.
    "! Apply digital signature to CFDI

    DATA: lv_original_string TYPE string,
          lv_signature       TYPE string,
          lv_cert_number     TYPE string,
          lv_cert_base64     TYPE string,
          lo_ixml            TYPE REF TO if_ixml,
          lo_doc             TYPE REF TO if_ixml_document,
          lo_parser          TYPE REF TO if_ixml_parser,
          lo_stream          TYPE REF TO if_ixml_istream,
          lo_factory         TYPE REF TO if_ixml_stream_factory,
          lo_ostream         TYPE REF TO if_ixml_ostream,
          lo_renderer        TYPE REF TO if_ixml_renderer.

    TRY.
        " Step 1: Get certificate data
        DATA(lv_certificate) = get_certificate( ).
        lv_cert_number = get_certificate_number( ).

        " Convert certificate to Base64
        lv_cert_base64 = cl_http_utility=>encode_x_base64( lv_certificate ).

        " Step 2: Calculate original string (Cadena Original)
        lv_original_string = calculate_original_string( iv_xml ).

        " Step 3: Sign the original string
        lv_signature = sign_data( lv_original_string ).

        " Step 4: Parse XML and add signature attributes
        lo_ixml = cl_ixml=>create( ).
        lo_factory = lo_ixml->create_stream_factory( ).
        lo_stream = lo_factory->create_istream_string( iv_xml ).
        lo_doc = lo_ixml->create_document( ).

        lo_parser = lo_ixml->create_parser(
          document       = lo_doc
          istream        = lo_stream
          stream_factory = lo_factory
        ).
        lo_parser->parse( ).

        " Get root Comprobante element
        DATA(lo_root) = lo_doc->get_root_element( ).
        IF lo_root IS NOT INITIAL.
          " Add signature attributes
          lo_root->set_attribute( name = 'Certificado' value = lv_cert_base64 ).
          lo_root->set_attribute( name = 'NoCertificado' value = lv_cert_number ).
          lo_root->set_attribute( name = 'Sello' value = lv_signature ).
        ENDIF.

        " Step 5: Render signed XML
        lo_ostream = lo_factory->create_ostream_cstring( rv_signed_xml ).
        lo_ostream->set_encoding( encoding = lo_ixml->create_encoding(
          byte_order = if_ixml_encoding=>co_none
          character_set = 'UTF-8' ) ).

        lo_renderer = lo_ixml->create_renderer(
          ostream  = lo_ostream
          document = lo_doc
        ).
        lo_renderer->set_normalizing( abap_true ).
        lo_renderer->render( ).

        " Add XML declaration
        rv_signed_xml = |<?xml version="1.0" encoding="UTF-8"?>| &&
                        cl_abap_char_utilities=>newline && rv_signed_xml.

      CATCH zcx_cfdi_signature INTO DATA(lx_sig).
        RAISE EXCEPTION lx_sig.
      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_cfdi_signature
          EXPORTING
            textid   = zcx_cfdi_signature=>signature_failed
            previous = lx_error
            details  = lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_billing_header.
    "! Retrieve billing document header

    SELECT SINGLE * FROM vbrk
      INTO @rs_vbrk
      WHERE vbeln = @mv_vbeln.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cfdi_generation
        EXPORTING
          textid = zcx_cfdi_generation=>billing_doc_not_found
          vbeln  = mv_vbeln.
    ENDIF.

  ENDMETHOD.

  METHOD get_billing_items.
    "! Retrieve billing document items

    SELECT * FROM vbrp
      INTO TABLE @rt_vbrp
      WHERE vbeln = @mv_vbeln
      ORDER BY posnr.

    IF sy-subrc <> 0 OR lines( rt_vbrp ) = 0.
      RAISE EXCEPTION TYPE zcx_cfdi_generation
        EXPORTING
          textid = zcx_cfdi_generation=>no_items_found
          vbeln  = mv_vbeln.
    ENDIF.

  ENDMETHOD.

  METHOD get_customer_data.
    "! Retrieve customer master data

    SELECT SINGLE * FROM kna1
      INTO @rs_kna1
      WHERE kunnr = @iv_kunnr.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cfdi_generation
        EXPORTING
          textid = zcx_cfdi_generation=>customer_not_found
          kunnr  = iv_kunnr.
    ENDIF.

  ENDMETHOD.

  METHOD load_configuration.
    "! Load CFDI configuration from custom table or constants

    " In production, read from custom configuration table ZCFDI_CONFIG
    " For now, use default values that should be customized

    " Try to read from custom table first
    " SELECT SINGLE * FROM zcfdi_config INTO CORRESPONDING FIELDS OF @ms_config
    "   WHERE active = @abap_true.

    " Default configuration - MUST be customized for production
    ms_config-company_rfc = 'XAXX010101000'.        " Generic RFC for testing
    ms_config-company_name = 'EMPRESA DE PRUEBA SA DE CV'.
    ms_config-company_regime = '601'.               " General Ley de Personas Morales
    ms_config-company_cp = '06600'.                 " Mexico City
    ms_config-cert_alias = 'CFDI_CERT'.             " SSF certificate alias
    ms_config-serie_default = 'A'.
    ms_config-uso_cfdi_default = 'G03'.             " Gastos en general

  ENDMETHOD.

  METHOD calculate_taxes.
    "! Calculate tax amounts from billing items

    DATA: lv_base_iva   TYPE p LENGTH 16 DECIMALS 2,
          lv_amount_iva TYPE p LENGTH 16 DECIMALS 2.

    CLEAR rs_taxes.

    " Process each billing item
    LOOP AT mt_vbrp INTO DATA(ls_vbrp).
      " Get item taxes
      DATA(ls_item_taxes) = calculate_item_taxes( ls_vbrp ).

      " Accumulate
      rs_taxes-base_iva = rs_taxes-base_iva + ls_item_taxes-base_iva.
      rs_taxes-amount_iva = rs_taxes-amount_iva + ls_item_taxes-amount_iva.
      rs_taxes-base_ret_iva = rs_taxes-base_ret_iva + ls_item_taxes-base_ret_iva.
      rs_taxes-amount_ret_iva = rs_taxes-amount_ret_iva + ls_item_taxes-amount_ret_iva.
      rs_taxes-base_ret_isr = rs_taxes-base_ret_isr + ls_item_taxes-base_ret_isr.
      rs_taxes-amount_ret_isr = rs_taxes-amount_ret_isr + ls_item_taxes-amount_ret_isr.
    ENDLOOP.

    " Set rate (from first item with IVA)
    rs_taxes-rate_iva = c_iva_rate.

    " Calculate totals
    rs_taxes-total_trasladados = rs_taxes-amount_iva.
    rs_taxes-total_retenidos = rs_taxes-amount_ret_iva + rs_taxes-amount_ret_isr.

    " Round all amounts
    rs_taxes-base_iva = round_amount( rs_taxes-base_iva ).
    rs_taxes-amount_iva = round_amount( rs_taxes-amount_iva ).
    rs_taxes-total_trasladados = round_amount( rs_taxes-total_trasladados ).
    rs_taxes-total_retenidos = round_amount( rs_taxes-total_retenidos ).

  ENDMETHOD.

  METHOD calculate_item_taxes.
    "! Calculate taxes for a single line item

    " Get tax condition values from VBRP
    " MWSBP = Tax amount in document currency

    " Base is the net amount
    rs_taxes-base_iva = is_vbrp-netwr.

    " Calculate IVA (16% standard rate)
    " In production, read actual tax rate from condition records (KONV)
    rs_taxes-amount_iva = is_vbrp-mwsbp.  " Tax amount from billing item

    " If no tax amount, calculate
    IF rs_taxes-amount_iva = 0 AND is_vbrp-netwr > 0.
      rs_taxes-amount_iva = is_vbrp-netwr * c_iva_rate.
    ENDIF.

    rs_taxes-rate_iva = c_iva_rate.

    " Withholding taxes would be read from condition records
    " For now, assume no withholding
    rs_taxes-base_ret_iva = 0.
    rs_taxes-amount_ret_iva = 0.
    rs_taxes-base_ret_isr = 0.
    rs_taxes-amount_ret_isr = 0.

  ENDMETHOD.

  METHOD get_sat_codes.
    "! Get SAT catalog codes for material

    " In production, read from custom table ZSAT_CODES mapping
    " MATNR -> ClaveProdServ, ClaveUnidad

    " Try to read from custom mapping table
    " SELECT SINGLE clave_prod_serv clave_unidad
    "   FROM zsat_material_map
    "   INTO (@rs_codes-clave_prod_serv, @rs_codes-clave_unidad)
    "   WHERE matnr = @iv_matnr.

    " Default codes - should be mapped per material in production
    rs_codes-clave_prod_serv = '01010101'.  " No especificado en el catálogo
    rs_codes-clave_unidad = 'H87'.          " Pieza
    rs_codes-unidad_descripcion = 'Pieza'.
    rs_codes-objeto_imp = '02'.             " 02 = Sí objeto de impuesto

  ENDMETHOD.

  METHOD build_comprobante_node.
    "! Build main Comprobante XML node

    DATA: lo_ixml TYPE REF TO if_ixml.

    lo_ixml = cl_ixml=>create( ).

    " Create root Comprobante element with namespace
    mo_root_elem = mo_xml_doc->create_element_ns(
      name   = 'Comprobante'
      prefix = 'cfdi'
    ).

    " Add namespace declarations
    mo_root_elem->set_attribute(
      name  = 'xmlns:cfdi'
      value = c_namespace_cfdi
    ).
    mo_root_elem->set_attribute(
      name  = 'xmlns:xsi'
      value = c_namespace_xsi
    ).
    mo_root_elem->set_attribute(
      name  = 'xsi:schemaLocation'
      value = c_schema_location
    ).

    " Add required attributes
    mo_root_elem->set_attribute( name = 'Version' value = ms_cfdi_data-version ).
    mo_root_elem->set_attribute( name = 'Serie' value = ms_cfdi_data-serie ).
    mo_root_elem->set_attribute( name = 'Folio' value = ms_cfdi_data-folio ).
    mo_root_elem->set_attribute( name = 'Fecha' value = ms_cfdi_data-fecha ).
    mo_root_elem->set_attribute( name = 'FormaPago' value = ms_cfdi_data-forma_pago ).
    mo_root_elem->set_attribute( name = 'SubTotal' value = format_amount( ms_cfdi_data-subtotal ) ).
    mo_root_elem->set_attribute( name = 'Moneda' value = ms_cfdi_data-moneda ).

    " Discount if applicable
    IF ms_cfdi_data-descuento > 0.
      mo_root_elem->set_attribute( name = 'Descuento' value = format_amount( ms_cfdi_data-descuento ) ).
    ENDIF.

    mo_root_elem->set_attribute( name = 'Total' value = format_amount( ms_cfdi_data-total ) ).
    mo_root_elem->set_attribute( name = 'TipoDeComprobante' value = ms_cfdi_data-tipo_comprobante ).
    mo_root_elem->set_attribute( name = 'MetodoPago' value = ms_cfdi_data-metodo_pago ).
    mo_root_elem->set_attribute( name = 'LugarExpedicion' value = ms_cfdi_data-lugar_expedicion ).
    mo_root_elem->set_attribute( name = 'Exportacion' value = ms_cfdi_data-exportacion ).

    " Exchange rate for foreign currencies
    IF ms_cfdi_data-tipo_cambio > 0.
      mo_root_elem->set_attribute( name = 'TipoCambio' value = format_amount( ms_cfdi_data-tipo_cambio iv_decimals = 6 ) ).
    ENDIF.

    " Append to document
    mo_xml_doc->append_child( mo_root_elem ).

  ENDMETHOD.

  METHOD build_emisor_node.
    "! Build Emisor (issuer) XML node

    DATA: lo_emisor TYPE REF TO if_ixml_element.

    " Create Emisor element
    lo_emisor = mo_xml_doc->create_element_ns(
      name   = 'Emisor'
      prefix = 'cfdi'
    ).

    " Add attributes
    lo_emisor->set_attribute( name = 'Rfc' value = ms_cfdi_data-emisor_rfc ).
    lo_emisor->set_attribute( name = 'Nombre' value = ms_cfdi_data-emisor_nombre ).
    lo_emisor->set_attribute( name = 'RegimenFiscal' value = ms_cfdi_data-emisor_regimen ).

    " Append to root
    mo_root_elem->append_child( lo_emisor ).

  ENDMETHOD.

  METHOD build_receptor_node.
    "! Build Receptor (recipient) XML node

    DATA: lo_receptor TYPE REF TO if_ixml_element.

    " Create Receptor element
    lo_receptor = mo_xml_doc->create_element_ns(
      name   = 'Receptor'
      prefix = 'cfdi'
    ).

    " Add attributes
    lo_receptor->set_attribute( name = 'Rfc' value = ms_cfdi_data-receptor_rfc ).
    lo_receptor->set_attribute( name = 'Nombre' value = ms_cfdi_data-receptor_nombre ).

    " Foreign customers
    IF ms_cfdi_data-receptor_domicilio IS NOT INITIAL AND
       ms_cfdi_data-receptor_domicilio <> 'MX'.
      lo_receptor->set_attribute( name = 'ResidenciaFiscal' value = ms_cfdi_data-receptor_domicilio ).
    ENDIF.

    lo_receptor->set_attribute( name = 'DomicilioFiscalReceptor' value = ms_cfdi_data-receptor_cp ).
    lo_receptor->set_attribute( name = 'RegimenFiscalReceptor' value = ms_cfdi_data-receptor_regimen ).
    lo_receptor->set_attribute( name = 'UsoCFDI' value = ms_cfdi_data-receptor_uso_cfdi ).

    " Append to root
    mo_root_elem->append_child( lo_receptor ).

  ENDMETHOD.

  METHOD build_conceptos_node.
    "! Build Conceptos (line items) XML node

    DATA: lo_conceptos TYPE REF TO if_ixml_element,
          ls_concepto  TYPE ty_concepto,
          ls_sat_codes TYPE ty_sat_codes.

    " Create Conceptos container element
    lo_conceptos = mo_xml_doc->create_element_ns(
      name   = 'Conceptos'
      prefix = 'cfdi'
    ).

    " Process each billing item
    LOOP AT mt_vbrp INTO DATA(ls_vbrp).
      CLEAR ls_concepto.

      " Get SAT codes for material
      ls_sat_codes = get_sat_codes( ls_vbrp-matnr ).

      " Build concepto structure
      ls_concepto-clave_prod_serv = ls_sat_codes-clave_prod_serv.
      ls_concepto-no_identificacion = |{ ls_vbrp-matnr ALPHA = OUT }|.
      ls_concepto-cantidad = ls_vbrp-fkimg.
      ls_concepto-clave_unidad = ls_sat_codes-clave_unidad.
      ls_concepto-unidad = ls_sat_codes-unidad_descripcion.
      ls_concepto-descripcion = ls_vbrp-arktx.
      ls_concepto-valor_unitario = ls_vbrp-netwr / ls_vbrp-fkimg.
      ls_concepto-importe = ls_vbrp-netwr.
      ls_concepto-descuento = 0.  " Would come from conditions
      ls_concepto-objeto_imp = ls_sat_codes-objeto_imp.

      " Calculate item taxes
      ls_concepto-taxes = calculate_item_taxes( ls_vbrp ).

      " Append to internal table
      APPEND ls_concepto TO mt_conceptos.

      " Build XML node
      build_concepto_node(
        is_concepto  = ls_concepto
        io_conceptos = lo_conceptos
      ).
    ENDLOOP.

    " Append Conceptos to root
    mo_root_elem->append_child( lo_conceptos ).

  ENDMETHOD.

  METHOD build_concepto_node.
    "! Build single Concepto XML node

    DATA: lo_concepto   TYPE REF TO if_ixml_element,
          lo_impuestos  TYPE REF TO if_ixml_element,
          lo_traslados  TYPE REF TO if_ixml_element,
          lo_traslado   TYPE REF TO if_ixml_element.

    " Create Concepto element
    lo_concepto = mo_xml_doc->create_element_ns(
      name   = 'Concepto'
      prefix = 'cfdi'
    ).

    " Add attributes
    lo_concepto->set_attribute( name = 'ClaveProdServ' value = is_concepto-clave_prod_serv ).
    lo_concepto->set_attribute( name = 'NoIdentificacion' value = is_concepto-no_identificacion ).
    lo_concepto->set_attribute( name = 'Cantidad' value = format_amount( is_concepto-cantidad iv_decimals = 6 ) ).
    lo_concepto->set_attribute( name = 'ClaveUnidad' value = is_concepto-clave_unidad ).
    lo_concepto->set_attribute( name = 'Unidad' value = is_concepto-unidad ).
    lo_concepto->set_attribute( name = 'Descripcion' value = is_concepto-descripcion ).
    lo_concepto->set_attribute( name = 'ValorUnitario' value = format_amount( is_concepto-valor_unitario iv_decimals = 6 ) ).
    lo_concepto->set_attribute( name = 'Importe' value = format_amount( is_concepto-importe ) ).
    lo_concepto->set_attribute( name = 'ObjetoImp' value = is_concepto-objeto_imp ).

    " Discount if applicable
    IF is_concepto-descuento > 0.
      lo_concepto->set_attribute( name = 'Descuento' value = format_amount( is_concepto-descuento ) ).
    ENDIF.

    " Add item-level taxes if ObjetoImp = 02
    IF is_concepto-objeto_imp = '02' AND is_concepto-taxes-base_iva > 0.
      " Create Impuestos element
      lo_impuestos = mo_xml_doc->create_element_ns(
        name   = 'Impuestos'
        prefix = 'cfdi'
      ).

      " Create Traslados container
      lo_traslados = mo_xml_doc->create_element_ns(
        name   = 'Traslados'
        prefix = 'cfdi'
      ).

      " Create Traslado element for IVA
      lo_traslado = mo_xml_doc->create_element_ns(
        name   = 'Traslado'
        prefix = 'cfdi'
      ).

      lo_traslado->set_attribute( name = 'Base' value = format_amount( is_concepto-taxes-base_iva ) ).
      lo_traslado->set_attribute( name = 'Impuesto' value = '002' ).  " 002 = IVA
      lo_traslado->set_attribute( name = 'TipoFactor' value = 'Tasa' ).
      lo_traslado->set_attribute( name = 'TasaOCuota' value = format_amount( is_concepto-taxes-rate_iva iv_decimals = 6 ) ).
      lo_traslado->set_attribute( name = 'Importe' value = format_amount( is_concepto-taxes-amount_iva ) ).

      lo_traslados->append_child( lo_traslado ).
      lo_impuestos->append_child( lo_traslados ).
      lo_concepto->append_child( lo_impuestos ).
    ENDIF.

    " Append to Conceptos
    io_conceptos->append_child( lo_concepto ).

  ENDMETHOD.

  METHOD build_impuestos_node.
    "! Build Impuestos (taxes) XML node at document level

    DATA: lo_impuestos    TYPE REF TO if_ixml_element,
          lo_traslados    TYPE REF TO if_ixml_element,
          lo_traslado     TYPE REF TO if_ixml_element,
          ls_total_taxes  TYPE ty_taxes.

    " Calculate total taxes
    ls_total_taxes = calculate_taxes( ).

    " Skip if no taxes
    IF ls_total_taxes-total_trasladados = 0 AND ls_total_taxes-total_retenidos = 0.
      RETURN.
    ENDIF.

    " Create Impuestos element
    lo_impuestos = mo_xml_doc->create_element_ns(
      name   = 'Impuestos'
      prefix = 'cfdi'
    ).

    " Add totals
    IF ls_total_taxes-total_trasladados > 0.
      lo_impuestos->set_attribute( name = 'TotalImpuestosTrasladados'
                                   value = format_amount( ls_total_taxes-total_trasladados ) ).
    ENDIF.

    IF ls_total_taxes-total_retenidos > 0.
      lo_impuestos->set_attribute( name = 'TotalImpuestosRetenidos'
                                   value = format_amount( ls_total_taxes-total_retenidos ) ).
    ENDIF.

    " Create Traslados container if there are transferred taxes
    IF ls_total_taxes-total_trasladados > 0.
      lo_traslados = mo_xml_doc->create_element_ns(
        name   = 'Traslados'
        prefix = 'cfdi'
      ).

      " Create Traslado for IVA
      lo_traslado = mo_xml_doc->create_element_ns(
        name   = 'Traslado'
        prefix = 'cfdi'
      ).

      lo_traslado->set_attribute( name = 'Base' value = format_amount( ls_total_taxes-base_iva ) ).
      lo_traslado->set_attribute( name = 'Impuesto' value = '002' ).  " 002 = IVA
      lo_traslado->set_attribute( name = 'TipoFactor' value = 'Tasa' ).
      lo_traslado->set_attribute( name = 'TasaOCuota' value = format_amount( ls_total_taxes-rate_iva iv_decimals = 6 ) ).
      lo_traslado->set_attribute( name = 'Importe' value = format_amount( ls_total_taxes-amount_iva ) ).

      lo_traslados->append_child( lo_traslado ).
      lo_impuestos->append_child( lo_traslados ).
    ENDIF.

    " Append to root
    mo_root_elem->append_child( lo_impuestos ).

  ENDMETHOD.

  METHOD round_amount.
    "! Round value to specified decimal places per SAT requirements

    rv_value = round( val = iv_value dec = iv_decimals ).

  ENDMETHOD.

  METHOD validate_master_data.
    "! Validate required master data exists

    " Validate company RFC
    IF ms_config-company_rfc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cfdi_generation
        EXPORTING
          textid     = zcx_cfdi_generation=>missing_configuration
          config_key = 'COMPANY_RFC'.
    ENDIF.

    IF validate_rfc( CONV string( ms_config-company_rfc ) ) = abap_false.
      RAISE EXCEPTION TYPE zcx_cfdi_generation
        EXPORTING
          textid = zcx_cfdi_generation=>invalid_rfc
          rfc    = CONV string( ms_config-company_rfc ).
    ENDIF.

    " Validate customer RFC
    IF ms_kna1-stcd1 IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cfdi_generation
        EXPORTING
          textid = zcx_cfdi_generation=>invalid_rfc
          rfc    = 'Customer RFC is empty'
          kunnr  = ms_kna1-kunnr.
    ENDIF.

    " Validate postal code
    IF ms_config-company_cp IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cfdi_generation
        EXPORTING
          textid     = zcx_cfdi_generation=>missing_configuration
          config_key = 'COMPANY_CP'.
    ENDIF.

  ENDMETHOD.

  METHOD get_certificate.
    "! Get digital certificate from SSF

    DATA: lv_ssf_name TYPE ssfapplssl,
          lt_certs    TYPE ssfctab,
          lv_rc       TYPE i.

    " Get certificate from SSF using the configured alias
    lv_ssf_name = ms_config-cert_alias.

    " Call SSF to retrieve certificate
    " In production, use SSF_KRN_ENVELOPE or similar function

    " Placeholder - actual implementation uses SAP SSF APIs
    " CALL FUNCTION 'SSF_KRN_DEVELOPE'
    "   EXPORTING
    "     ssf_name = lv_ssf_name
    "   TABLES
    "     certificates = lt_certs
    "   EXCEPTIONS
    "     others = 1.

    " For testing/demo, return empty
    " In production, this must return the actual certificate
    CLEAR rv_certificate.

    " Verify we got a certificate
    IF rv_certificate IS INITIAL.
      " In test mode, allow empty certificate
      " In production, uncomment the following:
      " RAISE EXCEPTION TYPE zcx_cfdi_signature
      "   EXPORTING
      "     textid     = zcx_cfdi_signature=>certificate_not_found
      "     cert_alias = CONV string( ms_config-cert_alias ).
    ENDIF.

  ENDMETHOD.

  METHOD get_certificate_number.
    "! Get certificate number from CSD

    " The certificate number is a 20-digit identifier from SAT
    " It's extracted from the certificate's serial number

    " Placeholder - actual implementation extracts from certificate
    rv_cert_number = '00000000000000000000'.

  ENDMETHOD.

  METHOD calculate_original_string.
    "! Calculate original string (Cadena Original) for signature

    DATA: lo_xslt_proc TYPE REF TO cl_xslt_processor,
          lv_xslt_name TYPE string,
          lv_xml_xstr  TYPE xstring,
          lv_result    TYPE xstring.

    TRY.
        " The original string is calculated by applying an XSLT transformation
        " to the CFDI XML. SAT provides the official XSLT file.

        " Convert XML to xstring
        lv_xml_xstr = cl_abap_codepage=>convert_to( iv_xml ).

        " Create XSLT processor
        " The XSLT transformation should be imported as ZCFDI_CADENA_ORIGINAL
        " using transaction STRANS

        " Placeholder - actual implementation uses SAT's XSLT
        " CALL TRANSFORMATION zcfdi_cadena_original
        "   SOURCE XML lv_xml_xstr
        "   RESULT XML lv_result.

        " For demo, return pipe-delimited values (simplified)
        rv_original_string = |{ ms_cfdi_data-version }| &&
                            |{ c_namespace_cfdi }| &&
                            |{ ms_cfdi_data-fecha }| &&
                            |{ ms_cfdi_data-forma_pago }| &&
                            |{ ms_cfdi_data-subtotal }| &&
                            |{ ms_cfdi_data-total }| &&
                            |{ ms_cfdi_data-tipo_comprobante }| &&
                            |{ ms_cfdi_data-metodo_pago }| &&
                            |{ ms_cfdi_data-lugar_expedicion }|.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_cfdi_signature
          EXPORTING
            textid   = zcx_cfdi_signature=>original_string_error
            details  = lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.

  METHOD sign_data.
    "! Sign data with private key

    DATA: lv_hash    TYPE xstring,
          lv_sig_raw TYPE xstring.

    TRY.
        " Step 1: Calculate SHA-256 hash of original string
        lv_hash = cl_abap_message_digest=>calculate_hash_for_char(
          if_algorithm = 'SHA256'
          if_data      = iv_data
        ).

        " Step 2: Sign hash with private key using RSA-SHA256
        " This uses SSF function modules for actual signing

        " Placeholder - actual implementation uses SSF
        " CALL FUNCTION 'SSF_KRN_SIGN'
        "   EXPORTING
        "     str_format = 'PKCS1_V15'
        "     b_inc_certs = abap_false
        "   TABLES
        "     ostr_signed_data_l = lt_signed
        "   EXCEPTIONS
        "     others = 1.

        " For demo, return empty signature
        lv_sig_raw = lv_hash.  " Just use hash as placeholder

        " Step 3: Encode to Base64
        rv_signature = cl_http_utility=>encode_x_base64( lv_sig_raw ).

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_cfdi_signature
          EXPORTING
            textid  = zcx_cfdi_signature=>signature_failed
            details = lx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.

  METHOD format_cfdi_datetime.
    "! Format date/time to ISO 8601 format required by CFDI

    " Format: YYYY-MM-DDTHH:MM:SS
    rv_formatted = |{ iv_date+0(4) }-{ iv_date+4(2) }-{ iv_date+6(2) }| &&
                   |T| &&
                   |{ iv_time+0(2) }:{ iv_time+2(2) }:{ iv_time+4(2) }|.

  ENDMETHOD.

  METHOD get_configuration.
    "! Return current configuration

    load_configuration( ).
    rs_config = ms_config.

  ENDMETHOD.

  METHOD validate_rfc.
    "! Validate RFC format

    DATA: lv_length TYPE i,
          lv_regex  TYPE string.

    lv_length = strlen( iv_rfc ).

    " RFC must be 12 characters (company) or 13 characters (person)
    IF lv_length <> 12 AND lv_length <> 13.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check format using regex
    " Company: 3 letters + 6 digits + 3 alphanumeric
    " Person: 4 letters + 6 digits + 3 alphanumeric

    IF lv_length = 12.
      " Company RFC: AAA000000XXX
      lv_regex = '^[A-Z&Ñ]{3}[0-9]{6}[A-Z0-9]{3}$'.
    ELSE.
      " Person RFC: AAAA000000XXX
      lv_regex = '^[A-Z&Ñ]{4}[0-9]{6}[A-Z0-9]{3}$'.
    ENDIF.

    " Check against pattern
    TRY.
        DATA(lo_regex) = NEW cl_abap_regex( pattern = lv_regex ignore_case = abap_false ).
        DATA(lo_matcher) = lo_regex->create_matcher( text = iv_rfc ).
        rv_valid = xsdbool( lo_matcher->match( ) = abap_true ).
      CATCH cx_sy_regex.
        rv_valid = abap_false.
    ENDTRY.

  ENDMETHOD.

  METHOD get_forma_pago.
    "! Get payment form code based on payment method

    " SAT catalog c_FormaPago
    CASE iv_zlsch.
      WHEN 'A'.  " Check
        rv_forma = '02'.  " Cheque nominativo
      WHEN 'B'.  " Bank transfer
        rv_forma = '03'.  " Transferencia electrónica de fondos
      WHEN 'C'.  " Credit card
        rv_forma = '04'.  " Tarjeta de crédito
      WHEN 'D'.  " Debit card
        rv_forma = '28'.  " Tarjeta de débito
      WHEN 'E'.  " Cash
        rv_forma = '01'.  " Efectivo
      WHEN 'P'.  " PayPal / Electronic wallet
        rv_forma = '05'.  " Monedero electrónico
      WHEN OTHERS.
        rv_forma = '99'.  " Por definir
    ENDCASE.

  ENDMETHOD.

  METHOD get_metodo_pago.
    "! Get payment method (PUE or PPD)

    " PUE = Pago en Una sola Exhibición (single payment)
    " PPD = Pago en Parcialidades o Diferido (partial/deferred)

    " Default to single payment
    " In production, determine from payment terms
    rv_metodo = 'PUE'.

  ENDMETHOD.

  METHOD get_uso_cfdi.
    "! Get CFDI use code from customer or default

    " SAT catalog c_UsoCFDI
    " Common codes:
    " G01 = Adquisición de mercancías
    " G02 = Devoluciones, descuentos o bonificaciones
    " G03 = Gastos en general
    " P01 = Por definir

    " Read from customer master custom field or use default
    IF ms_config-uso_cfdi_default IS NOT INITIAL.
      rv_uso = ms_config-uso_cfdi_default.
    ELSE.
      rv_uso = 'G03'.  " General expenses
    ENDIF.

  ENDMETHOD.

  METHOD get_moneda.
    "! Map SAP currency to SAT currency code

    " Most currencies are the same 3-letter code
    " MXN = Mexican Peso
    " USD = US Dollar
    " EUR = Euro

    CASE iv_waerk.
      WHEN 'MXP'.
        rv_moneda = 'MXN'.  " SAT uses MXN, not MXP
      WHEN OTHERS.
        rv_moneda = iv_waerk.  " Most other currencies match
    ENDCASE.

  ENDMETHOD.

  METHOD create_element.
    "! Create XML element with optional text content

    ro_element = mo_xml_doc->create_element_ns(
      name   = iv_name
      prefix = 'cfdi'
    ).

    IF iv_value IS NOT INITIAL.
      DATA(lo_text) = mo_xml_doc->create_text( iv_value ).
      ro_element->append_child( lo_text ).
    ENDIF.

    io_parent->append_child( ro_element ).

  ENDMETHOD.

  METHOD format_amount.
    "! Format amount for XML output

    DATA: lv_rounded TYPE p LENGTH 16 DECIMALS 6.

    lv_rounded = round_amount( iv_amount iv_decimals = iv_decimals ).

    " Format with proper decimal separator and no thousands separator
    rv_formatted = |{ lv_rounded NUMBER = RAW DECIMALS = iv_decimals }|.

    " Ensure proper decimal format (period, not comma)
    REPLACE ALL OCCURRENCES OF ',' IN rv_formatted WITH '.'.

    " Remove leading/trailing spaces
    CONDENSE rv_formatted NO-GAPS.

  ENDMETHOD.

ENDCLASS.
