# Montova PAC Integration

## Overview

Montova is a certified PAC provider offering CFDI stamping services via REST and SOAP APIs.

## Connection Details

### Production Environment
- **REST API**: `https://timbrado.montova.com.mx/api/v1`
- **SOAP API**: `https://timbrado.montova.com.mx/services/Timbrado`

### Test Environment
- **REST API**: `https://timbrado-pruebas.montova.com.mx/api/v1`
- **SOAP API**: `https://timbrado-pruebas.montova.com.mx/services/Timbrado`

## Authentication

```json
{
  "username": "YOUR_USERNAME",
  "password": "YOUR_PASSWORD",
  "rfc": "YOUR_RFC"
}
```

## REST API Integration

### Stamp Invoice Endpoint

**POST** `/stamp`

**Request:**
```json
{
  "xml": "BASE64_ENCODED_XML",
  "rfc": "AAA010101AAA"
}
```

**Response (Success):**
```json
{
  "success": true,
  "uuid": "12345678-1234-1234-1234-123456789012",
  "stampedXml": "BASE64_ENCODED_STAMPED_XML",
  "satSeal": "DIGITAL_SEAL",
  "stampDate": "2024-12-27T10:30:00"
}
```

**Response (Error):**
```json
{
  "success": false,
  "errorCode": "301",
  "errorMessage": "Invalid XML structure"
}
```

## SOAP API Integration

### WSDL Location
`https://timbrado.montova.com.mx/services/Timbrado?wsdl`

### Stamp Operation

**Request:**
```xml
<soapenv:Envelope>
  <soapenv:Body>
    <tim:timbrarCFDI>
      <tim:usuario>USERNAME</tim:usuario>
      <tim:password>PASSWORD</tim:password>
      <tim:cfdi>BASE64_ENCODED_XML</tim:cfdi>
    </tim:timbrarCFDI>
  </soapenv:Body>
</soapenv:Envelope>
```

**Response:**
```xml
<soapenv:Envelope>
  <soapenv:Body>
    <tim:timbrarCFDIResponse>
      <tim:uuid>12345678-1234-1234-1234-123456789012</tim:uuid>
      <tim:cfdiTimbrado>BASE64_STAMPED_XML</tim:cfdiTimbrado>
    </tim:timbrarCFDIResponse>
  </soapenv:Body>
</soapenv:Envelope>
```

## Error Codes

| Code | Description | Solution |
|------|-------------|----------|
| 301 | Invalid XML structure | Check XML schema |
| 302 | Invalid digital signature | Verify certificate |
| 303 | Duplicate folio | Use new invoice number |
| 304 | Invalid RFC | Verify RFC in SAT |
| 305 | Invalid certificate | Renew certificate |

## SAP ABAP Integration Example

See `src/abap/classes/ZCL_PAC_MONTOVA.abap` for complete implementation.

## Pricing

- Pay-per-stamp model
- Volume discounts available
- Contact: sales@montova.com.mx
