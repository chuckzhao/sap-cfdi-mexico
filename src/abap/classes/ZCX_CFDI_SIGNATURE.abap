*----------------------------------------------------------------------*
* Exception Class: ZCX_CFDI_SIGNATURE
*----------------------------------------------------------------------*
* Purpose: Exception class for CFDI digital signature errors
* Used by: ZCL_CFDI_GENERATOR and certificate handling classes
*----------------------------------------------------------------------*
CLASS zcx_cfdi_signature DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.

    "! Message constants for exception handling
    CONSTANTS:
      BEGIN OF signature_failed,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '101',
        attr1 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF signature_failed,

      BEGIN OF certificate_not_found,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '102',
        attr1 TYPE scx_attrname VALUE 'MV_CERT_ALIAS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF certificate_not_found,

      BEGIN OF certificate_expired,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '103',
        attr1 TYPE scx_attrname VALUE 'MV_CERT_ALIAS',
        attr2 TYPE scx_attrname VALUE 'MV_EXPIRY_DATE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF certificate_expired,

      BEGIN OF certificate_not_valid_yet,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '104',
        attr1 TYPE scx_attrname VALUE 'MV_CERT_ALIAS',
        attr2 TYPE scx_attrname VALUE 'MV_VALID_FROM',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF certificate_not_valid_yet,

      BEGIN OF private_key_error,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '105',
        attr1 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF private_key_error,

      BEGIN OF original_string_error,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '106',
        attr1 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF original_string_error,

      BEGIN OF ssf_error,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '107',
        attr1 TYPE scx_attrname VALUE 'MV_SSF_CODE',
        attr2 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF ssf_error,

      BEGIN OF xslt_transformation_error,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '108',
        attr1 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF xslt_transformation_error,

      BEGIN OF hash_calculation_error,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '109',
        attr1 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF hash_calculation_error,

      BEGIN OF encoding_error,
        msgid TYPE symsgid VALUE 'ZCFDI',
        msgno TYPE symsgno VALUE '110',
        attr1 TYPE scx_attrname VALUE 'MV_DETAILS',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF encoding_error.

    "! Certificate alias
    DATA mv_cert_alias TYPE string READ-ONLY.
    "! Expiry date
    DATA mv_expiry_date TYPE datum READ-ONLY.
    "! Valid from date
    DATA mv_valid_from TYPE datum READ-ONLY.
    "! Error details
    DATA mv_details TYPE string READ-ONLY.
    "! SSF return code
    DATA mv_ssf_code TYPE i READ-ONLY.

    "! Constructor
    "! @parameter textid | Text ID for message
    "! @parameter previous | Previous exception
    "! @parameter cert_alias | Certificate alias
    "! @parameter expiry_date | Certificate expiry date
    "! @parameter valid_from | Certificate valid from date
    "! @parameter details | Error details
    "! @parameter ssf_code | SSF return code
    METHODS constructor
      IMPORTING
        textid      LIKE if_t100_message=>t100key OPTIONAL
        previous    LIKE previous OPTIONAL
        cert_alias  TYPE string OPTIONAL
        expiry_date TYPE datum OPTIONAL
        valid_from  TYPE datum OPTIONAL
        details     TYPE string OPTIONAL
        ssf_code    TYPE i OPTIONAL.

    "! Get error text
    "! @parameter result | Error text
    METHODS get_text REDEFINITION.

ENDCLASS.

CLASS zcx_cfdi_signature IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    " Set message attributes
    mv_cert_alias = cert_alias.
    mv_expiry_date = expiry_date.
    mv_valid_from = valid_from.
    mv_details = details.
    mv_ssf_code = ssf_code.

    " Set T100 message key
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = signature_failed.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

  METHOD get_text.

    " Return descriptive error text
    DATA lv_date_str TYPE string.

    CASE if_t100_message~t100key.
      WHEN signature_failed.
        result = |Digital signature failed|.
        IF mv_details IS NOT INITIAL.
          result = result && |: { mv_details }|.
        ENDIF.
        IF previous IS BOUND.
          result = result && | ({ previous->get_text( ) })|.
        ENDIF.
      WHEN certificate_not_found.
        result = |Certificate not found: { mv_cert_alias }|.
      WHEN certificate_expired.
        lv_date_str = |{ mv_expiry_date DATE = USER }|.
        result = |Certificate { mv_cert_alias } expired on { lv_date_str }|.
      WHEN certificate_not_valid_yet.
        lv_date_str = |{ mv_valid_from DATE = USER }|.
        result = |Certificate { mv_cert_alias } is not valid until { lv_date_str }|.
      WHEN private_key_error.
        result = |Private key error: { mv_details }|.
      WHEN original_string_error.
        result = |Original string (Cadena Original) generation failed: { mv_details }|.
      WHEN ssf_error.
        result = |SSF error (code { mv_ssf_code }): { mv_details }|.
      WHEN xslt_transformation_error.
        result = |XSLT transformation error: { mv_details }|.
      WHEN hash_calculation_error.
        result = |Hash calculation error: { mv_details }|.
      WHEN encoding_error.
        result = |Encoding error: { mv_details }|.
      WHEN OTHERS.
        result = |Signature error|.
        IF mv_details IS NOT INITIAL.
          result = result && |: { mv_details }|.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
