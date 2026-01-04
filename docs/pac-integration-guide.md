# PAC Integration Guide

This guide explains how to integrate additional PAC (Proveedor Autorizado de Certificación) providers with the SAP CFDI implementation.

## Table of Contents

- [Overview](#overview)
- [Supported PACs](#supported-pacs)
- [Adding a New PAC](#adding-a-new-pac)
- [PAC Interface Class](#pac-interface-class)
- [Authentication](#authentication)
- [Error Handling](#error-handling)
- [Testing](#testing)

## Overview

PAC providers are authorized by SAT (Servicio de Administración Tributaria) to stamp CFDI documents. This implementation supports multiple PACs through an extensible architecture.

### Architecture

```
┌─────────────────┐
│  CFDI Generator │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  PAC Interface  │ ◄── Abstract class
│  ZCL_PAC_BASE   │
└────────┬────────┘
         │
    ┌────┴────┐
    ▼         ▼
┌────────┐ ┌────────┐
│Montova │ │ Edicom │ ◄── Concrete implementations
└────────┘ └────────┘
```

## Supported PACs

| PAC | REST API | SOAP API | Add-on | Documentation |
|-----|----------|----------|--------|---------------|
| **Montova** | Yes | Yes | No | [Guide](../src/pac-integration/montova/README.md) |
| **Edicom** | Yes | Yes | Yes | [Guide](../src/pac-integration/edicom/README.md) |

## Adding a New PAC

### Step 1: Create PAC Class

Create a new class inheriting from the PAC base class:

```abap
CLASS zcl_pac_newprovider DEFINITION
  PUBLIC
  INHERITING FROM zcl_pac_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_environment TYPE string DEFAULT 'test',

      stamp REDEFINITION,
      cancel REDEFINITION,
      get_status REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      c_test_url TYPE string VALUE 'https://test.newprovider.com/api/v1',
      c_prod_url TYPE string VALUE 'https://api.newprovider.com/api/v1'.

    METHODS:
      get_auth_token
        RETURNING VALUE(rv_token) TYPE string
        RAISING zcx_pac_error,

      build_request
        IMPORTING iv_xml TYPE string
        RETURNING VALUE(rv_request) TYPE string.

ENDCLASS.

CLASS zcl_pac_newprovider IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    " Set URL based on environment
    IF iv_environment = 'production'.
      mv_base_url = c_prod_url.
    ELSE.
      mv_base_url = c_test_url.
    ENDIF.
  ENDMETHOD.

  METHOD stamp.
    " 1. Get authentication token
    DATA(lv_token) = get_auth_token( ).

    " 2. Build request
    DATA(lv_request) = build_request( iv_xml ).

    " 3. Call PAC API
    DATA(lo_http) = cl_http_client=>create_by_url( mv_base_url && '/stamp' ).
    lo_http->request->set_header_field(
      name = 'Authorization'
      value = |Bearer { lv_token }|
    ).
    lo_http->request->set_cdata( lv_request ).
    lo_http->send( ).
    lo_http->receive( ).

    " 4. Parse response
    DATA(lv_response) = lo_http->response->get_cdata( ).
    " Parse XML response and extract UUID, stamped XML

    rs_result-uuid = '...'.  " Extract from response
    rs_result-stamped_xml = '...'.  " Extract from response
    rs_result-timestamp = sy-datum.

  ENDMETHOD.

  METHOD cancel.
    " Implementation for CFDI cancellation
  ENDMETHOD.

  METHOD get_status.
    " Implementation for status check
  ENDMETHOD.

  METHOD get_auth_token.
    " Implement OAuth or API key authentication
  ENDMETHOD.

  METHOD build_request.
    " Build request body according to PAC API specs
  ENDMETHOD.

ENDCLASS.
```

### Step 2: Configure PAC Settings

Add configuration for the new PAC in `config/config.template.json`:

```json
{
  "pac_provider": "newprovider",
  "newprovider": {
    "test": {
      "rest_url": "https://test.newprovider.com/api/v1",
      "username": "YOUR_USERNAME",
      "password": "STORED_IN_SAP_SSF",
      "rfc": "YOUR_RFC"
    },
    "production": {
      "rest_url": "https://api.newprovider.com/api/v1",
      "username": "PROD_USERNAME",
      "password": "STORED_IN_SAP_SSF",
      "rfc": "YOUR_RFC"
    }
  }
}
```

### Step 3: Update Factory Method

Update the PAC factory to include the new provider:

```abap
METHOD create_pac.
  CASE iv_provider.
    WHEN 'MONTOVA'.
      CREATE OBJECT ro_pac TYPE zcl_pac_montova.
    WHEN 'EDICOM'.
      CREATE OBJECT ro_pac TYPE zcl_pac_edicom.
    WHEN 'NEWPROVIDER'.
      CREATE OBJECT ro_pac TYPE zcl_pac_newprovider.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zcx_pac_error
        EXPORTING textid = zcx_pac_error=>unknown_provider.
  ENDCASE.
ENDMETHOD.
```

### Step 4: Add Documentation

Create documentation in `src/pac-integration/newprovider/README.md`:

```markdown
# NewProvider PAC Integration

## Overview
Instructions for integrating with NewProvider PAC.

## Configuration
...

## API Reference
...

## Examples
...
```

## PAC Interface Class

All PAC implementations must implement these methods:

### stamp( )

Stamps (timbra) a CFDI document.

**Input:**
- `iv_xml` - Unsigned CFDI XML

**Output:**
- `rs_result-uuid` - UUID assigned by SAT
- `rs_result-stamped_xml` - Signed and stamped XML
- `rs_result-timestamp` - Timestamp of stamping

### cancel( )

Cancels a previously stamped CFDI.

**Input:**
- `iv_uuid` - UUID of CFDI to cancel
- `iv_reason` - Cancellation reason code
- `iv_substitute_uuid` - UUID of substitute CFDI (if applicable)

**Output:**
- `rs_result-status` - Cancellation status
- `rs_result-cancellation_date` - Date of cancellation

### get_status( )

Checks the status of a CFDI.

**Input:**
- `iv_uuid` - UUID to check

**Output:**
- `rs_result-status` - Current status (valid, cancelled, pending)
- `rs_result-cancellation_date` - Cancellation date if cancelled

## Authentication

### API Key Authentication

```abap
lo_http->request->set_header_field(
  name  = 'X-API-Key'
  value = lv_api_key
).
```

### OAuth 2.0 Authentication

```abap
" Get token
DATA(lo_token_client) = cl_http_client=>create_by_url( lv_token_url ).
lo_token_client->request->set_form_field( name = 'grant_type' value = 'client_credentials' ).
lo_token_client->request->set_form_field( name = 'client_id' value = lv_client_id ).
lo_token_client->request->set_form_field( name = 'client_secret' value = lv_client_secret ).
lo_token_client->send( ).
lo_token_client->receive( ).

" Parse token response
DATA(lv_token) = ...  " Extract access_token from response

" Use token
lo_http->request->set_header_field(
  name  = 'Authorization'
  value = |Bearer { lv_token }|
).
```

### Secure Credential Storage

Always store credentials in SAP SSF (Secure Store and Forward):

```abap
" Store credential
CALL FUNCTION 'SSF_PUT_CREDENTIAL'
  EXPORTING
    str_ssf_name = 'PAC_CREDENTIALS'
    str_userid   = 'PAC_USER'
    str_password = lv_password.

" Retrieve credential
CALL FUNCTION 'SSF_GET_CREDENTIAL'
  EXPORTING
    str_ssf_name = 'PAC_CREDENTIALS'
  IMPORTING
    str_userid   = lv_userid
    str_password = lv_password.
```

## Error Handling

### Common PAC Errors

| Code | Description | Resolution |
|------|-------------|------------|
| 301 | Invalid XML | Check XML structure |
| 302 | Invalid signature | Verify certificate |
| 303 | Duplicate UUID | Document already stamped |
| 401 | Authentication failed | Check credentials |
| 500 | Server error | Retry with exponential backoff |

### Retry Logic

```abap
METHOD stamp_with_retry.
  DATA: lv_attempts TYPE i VALUE 0,
        lv_max_attempts TYPE i VALUE 3,
        lv_wait_seconds TYPE i VALUE 2.

  WHILE lv_attempts < lv_max_attempts.
    TRY.
        rs_result = stamp( iv_xml ).
        RETURN.  " Success
      CATCH zcx_pac_error INTO DATA(lx_error).
        lv_attempts = lv_attempts + 1.
        IF lv_attempts < lv_max_attempts.
          " Wait before retry (exponential backoff)
          WAIT UP TO lv_wait_seconds SECONDS.
          lv_wait_seconds = lv_wait_seconds * 2.
        ELSE.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDWHILE.
ENDMETHOD.
```

## Testing

### Test with SAT Test Environment

1. Obtain test certificates from SAT
2. Register with PAC test environment
3. Use test RFCs (e.g., XAXX010101000)

### Mock PAC for Unit Testing

```abap
CLASS ltc_pac_mock DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_pac.
ENDCLASS.

CLASS ltc_pac_mock IMPLEMENTATION.
  METHOD zif_pac~stamp.
    rs_result-uuid = '12345678-1234-1234-1234-123456789012'.
    rs_result-stamped_xml = iv_xml.
    rs_result-timestamp = sy-datum.
  ENDMETHOD.
ENDCLASS.
```

## See Also

- [Montova Integration](../src/pac-integration/montova/README.md)
- [Edicom Integration](../src/pac-integration/edicom/README.md)
- [CFDI Overview](01-cfdi-overview/README.md)
- [Troubleshooting](04-troubleshooting/README.md)
