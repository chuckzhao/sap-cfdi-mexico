# CFDI 4.0 Overview

## What is CFDI?

**CFDI (Comprobante Fiscal Digital por Internet)** is Mexico's electronic invoicing standard mandated by SAT (Servicio de Administración Tributaria - Tax Administration Service).

## Key Characteristics

### Mandatory for All Businesses
- All businesses in Mexico must issue CFDIs
- Replaces paper invoices completely
- Required for B2B, B2C, and B2G transactions
- Non-compliance results in significant penalties

### CFDI 4.0 (Current Version)
Effective from January 1, 2022:
- Enhanced data validation
- New required fields
- Updated catalogs
- Stricter validation rules
- Improved foreign trade complement

## CFDI Structure

### Main Components

```xml
<?xml version="1.0" encoding="UTF-8"?>
<cfdi:Comprobante
    xmlns:cfdi="http://www.sat.gob.mx/cfd/4"
    Version="4.0"
    Serie="A"
    Folio="12345"
    Fecha="2024-12-27T10:30:00"
    FormaPago="01"
    SubTotal="1000.00"
    Total="1160.00">
    
    <!-- Issuer Information -->
    <cfdi:Emisor Rfc="AAA010101AAA" Nombre="Company SA de CV"/>
    
    <!-- Recipient Information -->
    <cfdi:Receptor Rfc="BBB020202BBB" Nombre="Client SA de CV"/>
    
    <!-- Line Items -->
    <cfdi:Conceptos>
        <cfdi:Concepto ClaveProdServ="01010101" Cantidad="1"/>
    </cfdi:Conceptos>
    
    <!-- Taxes -->
    <cfdi:Impuestos TotalImpuestosTrasladados="160.00"/>
    
    <!-- PAC Stamp (added after timbrado) -->
    <cfdi:Complemento>
        <tfd:TimbreFiscalDigital UUID="12345678-1234-1234-1234-123456789012"/>
    </cfdi:Complemento>
</cfdi:Comprobante>
```

### Required Elements

1. **Header (Comprobante)**
   - Version: Always "4.0"
   - Serie/Folio: Invoice series and number
   - Date: Issue date/time
   - Payment form
   - Currency
   - Amounts (subtotal, taxes, total)

2. **Issuer (Emisor)**
   - RFC (Tax ID)
   - Company name
   - Tax regime

3. **Recipient (Receptor)**
   - RFC
   - Name
   - Tax regime
   - CFDI use (uso CFDI)

4. **Line Items (Conceptos)**
   - Product/Service code (clave prod/serv)
   - Quantity
   - Unit
   - Description
   - Unit price
   - Amount
   - Taxes

5. **Taxes (Impuestos)**
   - IVA (VAT - 16%)
   - IEPS (Special taxes if applicable)
   - ISR withholding (if applicable)
   - IVA withholding (if applicable)

6. **Digital Stamp (Timbre Fiscal Digital)**
   - Added by PAC after validation
   - UUID (Folio Fiscal)
   - SAT seal
   - PAC seal
   - Certification date

## CFDI Lifecycle

```
1. Generation       2. Validation      3. Stamping        4. Delivery
┌──────────┐       ┌──────────┐      ┌──────────┐       ┌──────────┐
│   SAP    │──────▶│   PAC    │─────▶│   SAT    │──────▶│ Customer │
│ Generate │       │ Validate │      │  Stamp   │       │ Receive  │
│   XML    │       │   XML    │      │   XML    │       │ PDF/XML  │
└──────────┘       └──────────┘      └──────────┘       └──────────┘
     │                   │                  │                  │
     │                   │                  │                  │
     └───────── Can take seconds to minutes ──────────────────┘
```

### Step-by-Step Process

1. **Invoice Creation in SAP**
   - Create billing document in SD module
   - System generates CFDI XML from billing data
   - Digital signature applied with company certificate

2. **Send to PAC**
   - XML sent to PAC via web service (REST/SOAP)
   - PAC validates XML structure and content
   - Checks against SAT catalogs and rules

3. **PAC Stamping (Timbrado)**
   - PAC adds TimbreFiscalDigital complement
   - Assigns UUID (Folio Fiscal)
   - Applies PAC digital seal
   - Forwards to SAT

4. **SAT Processing**
   - SAT receives and validates CFDI
   - Stores in central database
   - Makes available for verification

5. **Return to Issuer**
   - PAC returns stamped XML to SAP
   - SAP stores stamped version
   - UUID saved in billing document

6. **Delivery to Customer**
   - Customer receives XML (mandatory)
   - PDF representation (optional but common)
   - XML can be verified on SAT portal

## CFDI Types

### By Document Type

| Type | Code | Description |
|------|------|-------------|
| Income | I | Sales invoices (most common) |
| Expense | E | Purchase invoices |
| Transfer | T | Transfer of goods |
| Payroll | N | Payroll receipts |
| Payment | P | Payment complement |

### By Transaction Type

- **Sales Invoice**: Standard B2B/B2C sales
- **Credit Note**: Returns, discounts
- **Debit Note**: Additional charges
- **Payment Complement**: Proof of payment (required when payment date ≠ invoice date)
- **Withholding**: Tax withholdings

## Key SAT Catalogs

CFDIs must reference official SAT catalogs:

### C_FormaPago (Payment Method)
- 01: Cash
- 02: Check
- 03: Electronic transfer
- 04: Credit card
- 28: Debit card
- 99: To be defined

### C_MetodoPago (Payment Type)
- PUE: Single payment (Pago en una sola exhibición)
- PPD: Payment in installments (Pago en parcialidades o diferido)

### C_UsoCFDI (CFDI Use by Recipient)
- G01: Acquisition of goods
- G02: Returns, discounts, bonuses
- G03: General expenses
- I01: Construction
- I02: Office furniture
- I03: Transportation equipment
- I04: Computer equipment
- D04: Donations
- P01: To be defined

### C_ClaveProdServ (Product/Service Key)
- 5-8 digit codes from SAT catalog
- Must match product/service being sold
- Example: 01010101 for live cattle

### C_ClaveUnidad (Unit of Measure)
- SAT standardized units
- Example: H87 (piece), KGM (kilogram)

## Validation Rules

### Critical Validations

1. **RFC Validation**
   - Issuer RFC must be active in SAT
   - Recipient RFC must be valid
   - RFC format must match SAT rules

2. **Certificate Validation**
   - Digital certificate must be current
   - Must match issuer's RFC
   - Certificate must be registered with SAT

3. **Amounts Validation**
   - Subtotal + Taxes = Total
   - Decimal precision (2 decimals)
   - Rounding rules

4. **Catalog Validation**
   - All codes must exist in SAT catalogs
   - Must use current catalog versions
   - Correct combinations of codes

5. **Date/Time Validation**
   - Must be within allowed timeframe
   - Cannot be future-dated
   - Must be within 72 hours (best practice)

## Common Errors and Solutions

### Error: Invalid RFC
- **Cause**: RFC not registered or incorrect format
- **Solution**: Verify RFC on SAT portal, update master data

### Error: Invalid Certificate
- **Cause**: Expired or unregistered certificate
- **Solution**: Renew certificate, register with SAT

### Error: Invalid Product Code
- **Cause**: Product code not in SAT catalog
- **Solution**: Use correct code from C_ClaveProdServ catalog

### Error: Amount Mismatch
- **Cause**: Subtotal + Taxes ≠ Total
- **Solution**: Check calculation logic, rounding

### Error: Invalid Postal Code
- **Cause**: Incorrect postal code format or value
- **Solution**: Verify against SAT postal code catalog

## Compliance Requirements

### Issuance Timing
- Must issue CFDI at time of transaction
- Maximum 72 hours for some transactions
- Payment complements within specific timeframes

### Storage Requirements
- Must retain CFDIs for 5 years
- Both XML and PDF (if applicable)
- Must be available for SAT audits

### Cancellation Rules
- Can cancel within 24 hours without recipient approval
- After 24 hours, requires recipient acceptance
- Must provide cancellation reason
- Must use substitution CFDI for errors

## Benefits of CFDI

### For Businesses
- Automated tax compliance
- Reduced paper costs
- Faster processing
- Easier audits
- Improved cash flow (electronic payments)

### For SAT
- Real-time tax monitoring
- Reduced fraud
- Better tax collection
- Automated VAT validation
- Simplified audits

### For Customers
- Immediate invoice delivery
- Easy verification
- Secure storage
- Electronic expense management

## Version History

| Version | Effective Date | Key Changes |
|---------|---------------|-------------|
| 3.3 | July 2017 | Major restructuring |
| 4.0 | January 2022 | Current version, enhanced validations |

## Next Steps

- Review [Architecture Documentation](../02-architecture/README.md)
- Understand [SAP Integration](../03-abap-development/README.md)
- Choose [PAC Provider](../04-pac-integration/README.md)
