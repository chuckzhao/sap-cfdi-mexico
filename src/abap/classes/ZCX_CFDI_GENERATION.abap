*----------------------------------------------------------------------*
* Exception Class: ZCX_CFDI_GENERATION
*----------------------------------------------------------------------*
* Purpose: Exception class for CFDI generation errors
* Used by: ZCL_CFDI_GENERATOR and related classes
*----------------------------------------------------------------------*
CLASS zcx_cfdi_generation DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.

    "! Message constants for exception handling
    CONSTANTS:
      BEGIN OF generation_failed,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_VBELN',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF generation_failed,

      BEGIN OF billing_doc_not_found,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_VBELN',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF billing_doc_not_found,

      BEGIN OF no_items_found,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_VBELN',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_items_found,

      BEGIN OF customer_not_found,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_KUNNR',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF customer_not_found,

      BEGIN OF invalid_rfc,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_RFC',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_rfc,

      BEGIN OF missing_sat_codes,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MV_MATNR',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_sat_codes,

      BEGIN OF invalid_tax_regime,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MV_REGIMEN',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_tax_regime,

      BEGIN OF xml_creation_error,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF xml_creation_error,

      BEGIN OF missing_configuration,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MV_CONFIG_KEY',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_configuration,

      BEGIN OF validation_error,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF validation_error.

    "! Billing document number
    DATA mv_vbeln TYPE vbrk-vbeln READ-ONLY.
    "! Customer number
    DATA mv_kunnr TYPE kna1-kunnr READ-ONLY.
    "! RFC (Tax ID)
    DATA mv_rfc TYPE string READ-ONLY.
    "! Material number
    DATA mv_matnr TYPE matnr READ-ONLY.
    "! Tax regime code
    DATA mv_regimen TYPE string READ-ONLY.
    "! Error details
    DATA mv_details TYPE string READ-ONLY.
    "! Configuration key
    DATA mv_config_key TYPE string READ-ONLY.

    "! Constructor
    "! @parameter textid | Text ID for message
    "! @parameter previous | Previous exception
    "! @parameter vbeln | Billing document number
    "! @parameter kunnr | Customer number
    "! @parameter rfc | RFC (Tax ID)
    "! @parameter matnr | Material number
    "! @parameter regimen | Tax regime code
    "! @parameter details | Error details
    "! @parameter config_key | Configuration key
    METHODS constructor
      IMPORTING
        textid     LIKE if_t100_message=>t100key OPTIONAL
        previous   LIKE previous OPTIONAL
        vbeln      TYPE vbrk-vbeln OPTIONAL
        kunnr      TYPE kna1-kunnr OPTIONAL
        rfc        TYPE string OPTIONAL
        matnr      TYPE matnr OPTIONAL
        regimen    TYPE string OPTIONAL
        details    TYPE string OPTIONAL
        config_key TYPE string OPTIONAL.

    "! Get error text
    "! @parameter result | Error text
    METHODS get_text REDEFINITION.

ENDCLASS.

CLASS zcx_cfdi_generation IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    " Set message attributes
    mv_vbeln = vbeln.
    mv_kunnr = kunnr.
    mv_rfc = rfc.
    mv_matnr = matnr.
    mv_regimen = regimen.
    mv_details = details.
    mv_config_key = config_key.

    " Set T100 message key
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = generation_failed.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

  METHOD get_text.

    " Return descriptive error text
    DATA lv_msgv1 TYPE sy-msgv1.
    DATA lv_msgv2 TYPE sy-msgv2.
    DATA lv_msgv3 TYPE sy-msgv3.
    DATA lv_msgv4 TYPE sy-msgv4.

    " Set message variables based on context
    lv_msgv1 = mv_vbeln.
    lv_msgv2 = mv_kunnr.
    lv_msgv3 = mv_matnr.

    " Build message text
    CASE if_t100_message~t100key.
      WHEN generation_failed.
        result = |CFDI generation failed for billing document { mv_vbeln }|.
        IF previous IS BOUND.
          result = result && |: { previous->get_text( ) }|.
        ENDIF.
      WHEN billing_doc_not_found.
        result = |Billing document { mv_vbeln } not found|.
      WHEN no_items_found.
        result = |No line items found for billing document { mv_vbeln }|.
      WHEN customer_not_found.
        result = |Customer { mv_kunnr } not found|.
      WHEN invalid_rfc.
        result = |Invalid RFC format: { mv_rfc }|.
      WHEN missing_sat_codes.
        result = |SAT catalog codes missing for material { mv_matnr }|.
      WHEN invalid_tax_regime.
        result = |Invalid tax regime: { mv_regimen }|.
      WHEN xml_creation_error.
        result = |XML creation error: { mv_details }|.
      WHEN missing_configuration.
        result = |Missing configuration: { mv_config_key }|.
      WHEN validation_error.
        result = |Validation error: { mv_details }|.
      WHEN OTHERS.
        result = |CFDI generation error|.
        IF mv_details IS NOT INITIAL.
          result = result && |: { mv_details }|.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
