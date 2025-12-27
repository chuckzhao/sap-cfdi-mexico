# Edicom PAC Integration

## Overview

Edicom provides CFDI stamping via multiple integration methods including REST API, SOAP web services, and native SAP Add-on.

## Connection Details

### Production Environment
- **REST API**: `https://webservices.edicomgroup.com/cfdi/v1`
- **SOAP API**: `https://webservices.edicomgroup.com/cfdi/TimbradoService`
- **SAP Add-on**: Direct BAPI calls (no external HTTP required)

### Test Environment
- **REST API**: `https://webservices-test.edicomgroup.com/cfdi/v1`
- **SOAP API**: `https://webservices-test.edicomgroup.com/cfdi/TimbradoService`

## Authentication

### API Key Authentication
```http
Authorization: Bearer YOUR_API_KEY
X-Edicom-Client-Id: YOUR_CLIENT_ID
```

## Integration Methods

### Option 1: REST API

**POST** `/stamp`

**Request:**
```json
{
  "cfdiXml": "BASE64_ENCODED_XML",
  "rfc": "AAA010101AAA",
  "mode": "production"
}
```

**Response:**
```json
{
  "status": "success",
  "folio": "12345678-1234-1234-1234-123456789012",
  "stampedCfdi": "BASE64_STAMPED_XML",
  "qrCode": "BASE64_QR_IMAGE",
  "stampDateTime": "2024-12-27T10:30:00Z"
}
```

### Option 2: SOAP Web Service

**WSDL:** `https://webservices.edicomgroup.com/cfdi/TimbradoService?wsdl`

**Request:**
```xml
<soap:Envelope>
  <soap:Body>
    <edi:TimbraCFDI>
      <edi:Usuario>USERNAME</edi:Usuario>
      <edi:Password>PASSWORD</edi:Password>
      <edi:CFDI>BASE64_XML</edi:CFDI>
      <edi:Modo>production</edi:Modo>
    </edi:TimbraCFDI>
  </soap:Body>
</soap:Envelope>
```

### Option 3: SAP Add-on (Recommended)

Edicom provides a native SAP Add-on that installs directly in your SAP system.

**Benefits:**
- No external HTTP calls
- Direct BAPI integration
- Automatic error handling
- Simplified configuration
- Better performance

**Installation:**
1. Import transport provided by Edicom
2. Configure connection parameters
3. Assign authorizations
4. Test in sandbox

**Usage:**
```abap
CALL FUNCTION 'Z_EDICOM_STAMP_CFDI'
  EXPORTING
    iv_vbeln = lv_vbeln
    iv_xml   = lv_xml
  IMPORTING
    ev_uuid  = lv_uuid
    ev_xml   = lv_stamped_xml
  EXCEPTIONS
    error    = 1.
```

## Error Codes

| Code | Description | Action Required |
|------|-------------|-----------------|
| E001 | Authentication failed | Check credentials |
| E002 | Invalid XML format | Validate XML schema |
| E003 | Certificate error | Update certificate |
| E004 | SAT rejection | Review SAT logs |
| E005 | Duplicate UUID | Check for retries |

## Cancellation Process

**POST** `/cancel`

```json
{
  "uuid": "12345678-1234-1234-1234-123456789012",
  "rfc": "AAA010101AAA",
  "reason": "02",
  "substitutionUuid": "87654321-4321-4321-4321-210987654321"
}
```

## Features

### Included Services
- CFDI stamping (timbrado)
- CFDI cancellation
- UUID validation
- QR code generation
- PDF generation
- Document archiving

### Add-on Features
- Automatic retry logic
- Queue management
- Status monitoring
- Batch processing
- Reporting tools

## Pricing Model

- Monthly subscription
- Unlimited stamps included
- Additional services available
- Enterprise support
- Contact: mexico@edicomgroup.com

## Support

- **Email**: soporte@edicomgroup.com
- **Phone**: +52 55 1234 5678
- **Portal**: https://support.edicomgroup.com
- **Hours**: 24/7/365
