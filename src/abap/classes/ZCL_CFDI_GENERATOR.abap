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
        base_ret_iva       TYPE p LENGTH 16 DECIMALS 2,
        amount_ret_iva     TYPE p LENGTH 16 DECIMALS 2,
        total_trasladados  TYPE p LENGTH 16 DECIMALS 2,
        total_retenidos    TYPE p LENGTH 16 DECIMALS 2,
      END OF ty_taxes,

      BEGIN OF ty_sat_codes,
        clave_prod_serv    TYPE string,
        clave_unidad       TYPE string,
        unidad_descripcion TYPE string,
      END OF ty_sat_codes,

      BEGIN OF ty_concepto,
        clave_prod_serv    TYPE string,
        no_identificacion  TYPE string,
        cantidad           TYPE p LENGTH 16 DECIMALS 6,
        clave_unidad       TYPE string,
        unidad             TYPE string,
        descripcion        TYPE string,
        valor_unitario     TYPE p LENGTH 16 DECIMALS 2,
        importe            TYPE p LENGTH 16 DECIMALS 2,
        descuento          TYPE p LENGTH 16 DECIMALS 2,
        objeto_imp         TYPE string,
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
        moneda             TYPE string,
        tipo_cambio        TYPE p LENGTH 16 DECIMALS 6,
        subtotal           TYPE p LENGTH 16 DECIMALS 2,
        descuento          TYPE p LENGTH 16 DECIMALS 2,
        total              TYPE p LENGTH 16 DECIMALS 2,
        emisor_rfc         TYPE string,
        emisor_nombre      TYPE string,
        emisor_regimen     TYPE string,
        receptor_rfc       TYPE string,
        receptor_nombre    TYPE string,
        receptor_regimen   TYPE string,
        receptor_uso_cfdi  TYPE string,
        receptor_cp        TYPE string,
      END OF ty_cfdi_data.

    METHODS:
      "! Generate CFDI XML from billing document
      "! @parameter iv_vbeln | Billing document number
      "! @parameter iv_version | CFDI version (default 4.0)
      "! @parameter rv_xml | Generated CFDI XML
      "! @raising cx_cfdi_generation | Generation errors
      generate_from_billing_doc
        IMPORTING
          iv_vbeln          TYPE vbrk-vbeln
          iv_version        TYPE string DEFAULT '4.0'
        RETURNING
          VALUE(rv_xml)     TYPE string
        RAISING
          cx_cfdi_generation,

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
      "! @raising cx_cfdi_signature | Signature errors
      apply_signature
        IMPORTING
          iv_xml                TYPE string
        RETURNING
          VALUE(rv_signed_xml)  TYPE string
        RAISING
          cx_cfdi_signature,

      "! Get CFDI configuration
      "! @parameter rv_config | Configuration data
      get_configuration
        RETURNING
          VALUE(rv_config) TYPE string,

      "! Format date for CFDI (ISO 8601)
      "! @parameter iv_date | SAP date
      "! @parameter iv_time | SAP time
      "! @parameter rv_formatted | ISO 8601 formatted datetime
      format_cfdi_datetime
        IMPORTING
          iv_date           TYPE datum
          iv_time           TYPE uzeit
        RETURNING
          VALUE(rv_formatted) TYPE string.

  PRIVATE SECTION.
    DATA:
      mv_vbeln      TYPE vbrk-vbeln,
      mv_version    TYPE string,
      mo_xml_doc    TYPE REF TO if_ixml_document,
      ms_cfdi_data  TYPE ty_cfdi_data,
      mt_conceptos  TYPE tt_conceptos.

    CONSTANTS:
      "! CFDI namespace
      c_namespace_cfdi TYPE string VALUE 'http://www.sat.gob.mx/cfd/4',
      "! CFDI version
      c_cfdi_version   TYPE string VALUE '4.0'.

    METHODS:
      "! Get billing document header
      "! @raising cx_cfdi_generation | If document not found
      get_billing_header
        RETURNING VALUE(rs_vbrk) TYPE vbrk
        RAISING   cx_cfdi_generation,

      "! Get billing document items
      "! @raising cx_cfdi_generation | If no items found
      get_billing_items
        RETURNING VALUE(rt_vbrp) TYPE vbrp_tab
        RAISING   cx_cfdi_generation,

      "! Get customer master data
      "! @parameter iv_kunnr | Customer number
      "! @raising cx_cfdi_generation | If customer not found
      get_customer_data
        IMPORTING iv_kunnr       TYPE kna1-kunnr
        RETURNING VALUE(rs_kna1) TYPE kna1
        RAISING   cx_cfdi_generation,

      "! Build main Comprobante node
      build_comprobante_node
        RAISING cx_cfdi_generation,

      "! Build Emisor (issuer) node
      build_emisor_node
        RAISING cx_cfdi_generation,

      "! Build Receptor (recipient) node
      build_receptor_node
        RAISING cx_cfdi_generation,

      "! Build Conceptos (line items) node
      build_conceptos_node
        RAISING cx_cfdi_generation,

      "! Build Impuestos (taxes) node
      build_impuestos_node
        RAISING cx_cfdi_generation,

      "! Calculate taxes from billing document
      calculate_taxes
        RETURNING VALUE(rs_taxes) TYPE ty_taxes
        RAISING   cx_cfdi_generation,

      "! Get SAT catalog codes for material
      "! @parameter iv_matnr | Material number
      get_sat_codes
        IMPORTING iv_matnr        TYPE matnr
        RETURNING VALUE(rs_codes) TYPE ty_sat_codes
        RAISING   cx_cfdi_generation,

      "! Round to 2 decimal places per SAT requirements
      "! @parameter iv_value | Value to round
      round_to_2_decimals
        IMPORTING iv_value        TYPE p
        RETURNING VALUE(rv_value) TYPE p,

      "! Validate required master data exists
      validate_master_data
        RAISING cx_cfdi_generation,

      "! Get certificate from SSF
      "! @parameter rv_certificate | Certificate data
      get_certificate
        RETURNING VALUE(rv_certificate) TYPE xstring
        RAISING   cx_cfdi_signature,

      "! Calculate original string for signature
      "! @parameter iv_xml | XML document
      "! @parameter rv_original_string | Original string for signing
      calculate_original_string
        IMPORTING iv_xml                  TYPE string
        RETURNING VALUE(rv_original_string) TYPE string
        RAISING   cx_cfdi_signature.

ENDCLASS.

CLASS zcl_cfdi_generator IMPLEMENTATION.

  METHOD generate_from_billing_doc.
    "! Main method to generate CFDI XML from billing document

    TRY.
        " Store input parameters
        mv_vbeln = iv_vbeln.
        mv_version = iv_version.

        " Step 1: Get billing document data
        DATA(ls_vbrk) = get_billing_header( ).
        DATA(lt_vbrp) = get_billing_items( ).
        DATA(ls_kna1) = get_customer_data( ls_vbrk-kunag ).

        " Step 2: Validate master data
        validate_master_data( ).

        " Step 3: Prepare CFDI data structure
        ms_cfdi_data-version = c_cfdi_version.
        ms_cfdi_data-serie = 'A'.  " Configure as needed
        ms_cfdi_data-folio = ls_vbrk-vbeln.
        ms_cfdi_data-fecha = format_cfdi_datetime(
          iv_date = ls_vbrk-fkdat
          iv_time = ls_vbrk-fkdat  " Use system time if available
        ).

        " Set payment and document type
        ms_cfdi_data-forma_pago = '99'.    " To be defined
        ms_cfdi_data-metodo_pago = 'PUE'.  " Single payment
        ms_cfdi_data-tipo_comprobante = 'I'. " Income
        ms_cfdi_data-moneda = ls_vbrk-waerk.

        " Calculate amounts
        DATA(ls_taxes) = calculate_taxes( ).
        ms_cfdi_data-subtotal = round_to_2_decimals( ls_vbrk-netwr ).
        ms_cfdi_data-total = round_to_2_decimals( ls_vbrk-netwr + ls_taxes-total_trasladados ).

        " Step 4: Build XML structure
        build_comprobante_node( ).
        build_emisor_node( ).
        build_receptor_node( ).
        build_conceptos_node( ).
        build_impuestos_node( ).

        " Step 5: Convert XML document to string
        DATA(lo_ixml) = cl_ixml=>create( ).
        DATA(lo_streamfactory) = lo_ixml->create_stream_factory( ).
        DATA(lo_ostream) = lo_streamfactory->create_ostream_cstring( rv_xml ).
        DATA(lo_renderer) = lo_ixml->create_renderer(
          ostream = lo_ostream
          document = mo_xml_doc
        ).
        lo_renderer->render( ).

        " Step 6: Apply digital signature
        rv_xml = apply_signature( rv_xml ).

      CATCH cx_root INTO DATA(lx_error).
        " Re-raise as CFDI generation exception
        RAISE EXCEPTION TYPE cx_cfdi_generation
          EXPORTING
            textid = cx_cfdi_generation=>generation_failed
            previous = lx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD validate_xml.
    "! Validate CFDI XML structure and content

    " TODO: Implement validation logic
    " 1. Schema validation against CFDI 4.0 XSD
    " 2. Business rule validation
    " 3. SAT catalog code validation
    " 4. Amount calculation validation

    CLEAR rt_errors.

  ENDMETHOD.

  METHOD apply_signature.
    "! Apply digital signature to CFDI

    TRY.
        " Step 1: Get certificate from SSF
        DATA(lv_certificate) = get_certificate( ).

        " Step 2: Calculate original string (Cadena Original)
        DATA(lv_original_string) = calculate_original_string( iv_xml ).

        " Step 3: Sign with certificate
        " TODO: Implement actual signing logic using SSF

        " Step 4: Add signature to XML
        " TODO: Insert signature node into XML

        rv_signed_xml = iv_xml.  " Placeholder

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE cx_cfdi_signature
          EXPORTING
            textid = cx_cfdi_signature=>signature_failed
            previous = lx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD get_billing_header.
    "! Retrieve billing document header

    SELECT SINGLE * FROM vbrk
      INTO @rs_vbrk
      WHERE vbeln = @mv_vbeln.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cfdi_generation
        EXPORTING
          textid = cx_cfdi_generation=>billing_doc_not_found.
    ENDIF.

  ENDMETHOD.

  METHOD get_billing_items.
    "! Retrieve billing document items

    SELECT * FROM vbrp
      INTO TABLE @rt_vbrp
      WHERE vbeln = @mv_vbeln.

    IF sy-subrc <> 0 OR lines( rt_vbrp ) = 0.
      RAISE EXCEPTION TYPE cx_cfdi_generation
        EXPORTING
          textid = cx_cfdi_generation=>no_items_found.
    ENDIF.

  ENDMETHOD.

  METHOD get_customer_data.
    "! Retrieve customer master data

    SELECT SINGLE * FROM kna1
      INTO @rs_kna1
      WHERE kunnr = @iv_kunnr.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_cfdi_generation
        EXPORTING
          textid = cx_cfdi_generation=>customer_not_found.
    ENDIF.

  ENDMETHOD.

  METHOD calculate_taxes.
    "! Calculate tax amounts

    " TODO: Implement tax calculation logic
    " Get from billing document tax items
    " Calculate IVA (16%)
    " Calculate withholding taxes if applicable

    CLEAR rs_taxes.

  ENDMETHOD.

  METHOD get_sat_codes.
    "! Get SAT catalog codes for material

    " TODO: Implement SAT code lookup
    " Map internal material codes to SAT product/service codes
    " Map internal unit codes to SAT unit codes

    rs_codes-clave_prod_serv = '01010101'.  " Placeholder
    rs_codes-clave_unidad = 'H87'.          " Piece
    rs_codes-unidad_descripcion = 'Pieza'.

  ENDMETHOD.

  METHOD build_comprobante_node.
    "! Build main Comprobante XML node

    " TODO: Implement XML node creation
    " Create root element with all attributes
    " Add namespace declarations

  ENDMETHOD.

  METHOD build_emisor_node.
    "! Build Emisor (issuer) XML node

    " TODO: Implement emisor node
    " Add RFC, company name, tax regime

  ENDMETHOD.

  METHOD build_receptor_node.
    "! Build Receptor (recipient) XML node

    " TODO: Implement receptor node
    " Add customer RFC, name, tax regime, CFDI use

  ENDMETHOD.

  METHOD build_conceptos_node.
    "! Build Conceptos (line items) XML node

    " TODO: Implement conceptos node
    " Loop through billing items
    " Add each item with all required fields

  ENDMETHOD.

  METHOD build_impuestos_node.
    "! Build Impuestos (taxes) XML node

    " TODO: Implement impuestos node
    " Add tax totals and details

  ENDMETHOD.

  METHOD round_to_2_decimals.
    "! Round value to 2 decimal places per SAT requirements

    rv_value = round( val = iv_value dec = 2 ).

  ENDMETHOD.

  METHOD validate_master_data.
    "! Validate required master data exists

    " TODO: Implement master data validation
    " Check customer RFC exists
    " Check SAT codes assigned
    " Check tax classifications

  ENDMETHOD.

  METHOD get_certificate.
    "! Get digital certificate from SSF

    " TODO: Implement certificate retrieval
    " Use SSF API to get certificate
    " Verify certificate valid

  ENDMETHOD.

  METHOD calculate_original_string.
    "! Calculate original string (Cadena Original) for signature

    " TODO: Implement original string calculation
    " Apply XSLT transformation to XML
    " Return concatenated string

    rv_original_string = ''.  " Placeholder

  ENDMETHOD.

  METHOD format_cfdi_datetime.
    "! Format date/time to ISO 8601 format required by CFDI

    DATA: lv_date_iso TYPE string,
          lv_time_iso TYPE string.

    " Format: YYYY-MM-DDTHH:MM:SS
    CONCATENATE iv_date+0(4) '-' iv_date+4(2) '-' iv_date+6(2)
                'T'
                iv_time+0(2) ':' iv_time+2(2) ':' iv_time+4(2)
      INTO rv_formatted.

  ENDMETHOD.

  METHOD get_configuration.
    "! Get CFDI configuration from custom table

    " TODO: Implement configuration retrieval
    " Read from ZCFDI_CONFIG table
    " Return configuration as JSON or structure

    rv_config = ''.  " Placeholder

  ENDMETHOD.

ENDCLASS.
