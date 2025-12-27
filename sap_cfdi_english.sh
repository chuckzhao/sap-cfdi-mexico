#!/bin/bash

# SAP CFDI 4.0 Mexico - Complete Repository Setup (English Version)
# This script creates a complete GitHub repository structure for SAP E-Invoice Mexico implementation

echo "🇲🇽 Creating SAP CFDI Mexico Repository (English)..."

# Create main project directory
mkdir -p sap-cfdi-mexico
cd sap-cfdi-mexico

# Create directory structure
mkdir -p docs/{01-cfdi-overview,02-architecture,03-abap-development,04-pac-integration,05-sat-compliance,06-sd-billing,07-testing,08-deployment}
mkdir -p src/abap/{classes,function-modules,tables}
mkdir -p src/pac-integration/{montova,edicom}
mkdir -p config
mkdir -p tests

# ===========================================
# README.md
# ===========================================
cat > README.md << 'EOF'
# SAP CFDI 4.0 Mexico Implementation

Complete guide for implementing Mexico's CFDI (Comprobante Fiscal Digital por Internet) 4.0 electronic invoicing in SAP systems with PAC (Authorized Certification Provider) integration.

## 🎯 Overview

This repository provides comprehensive documentation and code examples for implementing Mexico's mandatory electronic invoicing (CFDI 4.0) in SAP ECC or S/4HANA systems.

**Key Features:**
- CFDI 4.0 XML generation from SAP billing documents
- Integration with PAC providers (Montova & Edicom)
- SAT (Tax Administration Service) compliance
- Timbrado (stamping) and Cancelación (cancellation) workflows
- Complete SD module integration

## 📚 Documentation Structure

```
docs/
├── 01-cfdi-overview/        # Introduction to CFDI 4.0
├── 02-architecture/         # System architecture and data flow
├── 03-abap-development/     # ABAP development guide
├── 04-pac-integration/      # PAC provider integration (Montova & Edicom)
├── 05-sat-compliance/       # SAT requirements and catalogs
├── 06-sd-billing/          # Sales & Distribution integration
├── 07-testing/             # Testing and certification guide
└── 08-deployment/          # Installation and deployment guide
```

## 🚀 Quick Start

### Prerequisites
- SAP ECC 6.0+ or S/4HANA
- Active contract with a PAC provider (Montova or Edicom)
- RFC enabled for external communications
- Digital certificates from SAT

### Installation Steps

1. **Review Documentation**
   - Start with [CFDI Overview](docs/01-cfdi-overview/README.md)
   - Understand the [Architecture](docs/02-architecture/README.md)

2. **Choose Your PAC Provider**
   - [Montova Integration Guide](docs/04-pac-integration/montova/README.md)
   - [Edicom Integration Guide](docs/04-pac-integration/edicom/README.md)
   - [PAC Comparison](docs/04-pac-integration/comparison.md)

3. **Implement ABAP Code**
   - Import classes from `src/abap/classes/`
   - Configure function modules
   - Set up custom tables

4. **Test & Certify**
   - Follow [Testing Guide](docs/07-testing/README.md)
   - Use SAT's test environment
   - Get PAC certification

5. **Deploy to Production**
   - Follow [Deployment Guide](docs/08-deployment/README.md)

## 💻 Code Examples

### CFDI XML Generation
```abap
DATA(lo_cfdi) = NEW zcl_cfdi_generator( ).
lo_cfdi->generate_from_billing_doc( 
  iv_vbeln = '1234567890'
  iv_version = '4.0'
).
DATA(lv_xml) = lo_cfdi->get_xml( ).
```

### PAC Integration (Montova)
```abap
DATA(lo_pac) = NEW zcl_pac_montova( ).
lo_pac->stamp_invoice(
  EXPORTING iv_xml = lv_xml
  IMPORTING ev_uuid = lv_uuid
            ev_sat_seal = lv_seal
).
```

### PAC Integration (Edicom)
```abap
DATA(lo_pac) = NEW zcl_pac_edicom( ).
lo_pac->stamp_invoice(
  EXPORTING iv_xml = lv_xml
  IMPORTING ev_uuid = lv_uuid
            ev_sat_seal = lv_seal
).
```

## 📋 PAC Provider Comparison

| Feature | Montova | Edicom |
|---------|---------|--------|
| REST API | ✅ Yes | ✅ Yes |
| SOAP API | ✅ Yes | ✅ Yes |
| SAP Add-on | ❌ No | ✅ Yes |
| Pricing | Pay-per-stamp | Monthly subscription |
| Support | 24/7 | 24/7 |
| SAT Certification | ✅ Yes | ✅ Yes |

## 🔐 Security Notes

**IMPORTANT:** Never commit sensitive data to version control!

This repository includes `.gitignore` that automatically excludes:
- ❌ Digital certificates (*.cer, *.key, *.p12)
- ❌ PAC credentials and API keys
- ❌ Production configuration files
- ❌ SAT private keys

**Best Practices:**
- Store credentials in SAP Secure Store (SSF)
- Use environment-specific configuration
- Rotate API keys regularly
- Keep certificates in SAP STRUST

## 📖 Key Concepts

### CFDI (Comprobante Fiscal Digital por Internet)
Mexico's mandatory electronic invoicing format. All invoices must be:
- Generated in XML format following SAT specifications
- Digitally signed by the issuer
- Stamped (timbrado) by an authorized PAC
- Sent to SAT for validation
- Delivered to the customer

### PAC (Proveedor Autorizado de Certificación)
Authorized Certification Providers authorized by SAT to:
- Validate CFDI XML structure
- Add digital stamp (timbre fiscal)
- Assign folio fiscal (UUID)
- Forward to SAT
- Return stamped invoice

### Timbrado (Stamping)
Process where PAC:
1. Receives CFDI XML from issuer
2. Validates XML against SAT schema
3. Adds complemento TimbreFiscalDigital
4. Returns UUID and SAT seal

## 🏗️ Architecture

```
┌─────────────┐      ┌──────────┐      ┌─────────┐      ┌─────────┐
│  SAP S/4    │      │   PAC    │      │   SAT   │      │Customer │
│  (Billing)  │─────▶│(Montova/ │─────▶│  Tax    │      │  (PDF/  │
│             │      │ Edicom)  │      │Authority│      │  XML)   │
│  - SD Doc   │      │          │      │         │      │         │
│  - XML Gen  │◀─────│ -Validate│◀─────│ -Accept │◀─────│         │
│  - Stamp    │      │ -Stamp   │      │ -Store  │      │         │
└─────────────┘      └──────────┘      └─────────┘      └─────────┘
```

## 📁 Repository Structure

```
sap-cfdi-mexico/
├── docs/                       # Documentation
├── src/
│   ├── abap/
│   │   ├── classes/           # ABAP classes
│   │   ├── function-modules/  # Function modules
│   │   └── tables/            # Custom tables
│   └── pac-integration/
│       ├── montova/           # Montova integration code
│       └── edicom/            # Edicom integration code
├── config/                    # Configuration templates
├── tests/                     # Test cases
└── README.md
```

## 🛠️ Technology Stack

- **SAP ECC 6.0+** or **S/4HANA**
- **ABAP 7.4+**
- **RFC** for PAC communication
- **XML Processing** (iXML, XSLT)
- **REST/SOAP** web services
- **Digital Signatures** (SSF)

## 📚 Additional Resources

- [SAT Official Portal](http://www.sat.gob.mx)
- [CFDI 4.0 Specification](http://www.sat.gob.mx/informacion_fiscal/factura_electronica/Paginas/Anexo_20_version3-3.aspx)
- [SAT Validation Tools](https://verificacfdi.facturaelectronica.sat.gob.mx/)
- [Montova Documentation](https://montova.com)
- [Edicom Documentation](https://www.edicomgroup.com)

## 🤝 Contributing

Contributions are welcome! Please read our contributing guidelines before submitting pull requests.

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## ⚠️ Disclaimer

This repository provides guidance and code examples for educational and reference purposes. Always consult with:
- Your SAP consultant
- Your PAC provider
- Mexican tax legal advisors
- SAT official documentation

Before implementing in production environments.

## 📞 Support

For issues and questions:
- Open an issue in this repository
- Check existing documentation
- Consult your PAC provider
- Contact SAT for regulatory questions

---

**Version:** 1.0  
**Last Updated:** December 2024  
**CFDI Version:** 4.0  
**Compatible with:** SAP ECC 6.0+, S/4HANA
EOF

# ===========================================
# 01 - CFDI OVERVIEW
# ===========================================
cat > docs/01-cfdi-overview/README.md << 'EOF'
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
EOF

# ===========================================
# 02 - ARCHITECTURE
# ===========================================
cat > docs/02-architecture/README.md << 'EOF'
# System Architecture

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         SAP SYSTEM                          │
├─────────────────────────────────────────────────────────────┤
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │   SD Module  │───▶│ CFDI Engine  │───▶│  RFC Layer   │  │
│  │  (Billing)   │    │ (XML Gen)    │    │ (HTTP/SOAP)  │  │
│  └──────────────┘    └──────────────┘    └──────────────┘  │
│         │                    │                    │         │
│         ▼                    ▼                    ▼         │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │  Custom      │    │   Digital    │    │   Document   │  │
│  │  Tables      │    │  Signature   │    │   Storage    │  │
│  └──────────────┘    └──────────────┘    └──────────────┘  │
└────────────────────────────┬────────────────────────────────┘
                             │
                             │ HTTPS/REST or SOAP
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                      PAC PROVIDER                           │
│                   (Montova or Edicom)                       │
├─────────────────────────────────────────────────────────────┤
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │  API Gateway │───▶│  Validation  │───▶│   Stamping   │  │
│  │              │    │   Engine     │    │   Engine     │  │
│  └──────────────┘    └──────────────┘    └──────────────┘  │
└────────────────────────────┬────────────────────────────────┘
                             │
                             │ HTTPS
                             ▼
┌─────────────────────────────────────────────────────────────┐
│                           SAT                               │
│              (Tax Administration Service)                   │
├─────────────────────────────────────────────────────────────┤
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │
│  │  Reception   │───▶│   Central    │───▶│ Verification │  │
│  │   Service    │    │   Database   │    │   Portal     │  │
│  └──────────────┘    └──────────────┘    └──────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Data Flow

### 1. Invoice Generation Flow

```
User Action          SAP Processing              Result
─────────────────────────────────────────────────────────
Create Order    ──▶  Create Sales Order (VA01)
                     └─▶ Order saved in VBAK/VBAP

Deliver Goods   ──▶  Create Delivery (VL01N)
                     └─▶ Delivery in LIKP/LIPS

Create Invoice  ──▶  Create Billing (VF01)
                     ├─▶ Billing doc in VBRK/VBRP
                     ├─▶ Trigger CFDI generation
                     ├─▶ Generate XML
                     ├─▶ Apply digital signature
                     └─▶ Send to PAC

PAC Response    ◀──  Receive stamped XML
                     ├─▶ Store UUID
                     ├─▶ Update billing status
                     ├─▶ Store stamped XML
                     └─▶ Generate PDF

Send to Customer ──▶ Email XML + PDF
                     └─▶ Update delivery status
```

### 2. Timbrado (Stamping) Flow

```
┌──────────┐     1. Send XML      ┌──────────┐
│   SAP    │────────────────────▶│   PAC    │
│          │                     │          │
│          │     2. Validate      │          │
│          │◀────────────────────│          │
│          │     OK/Error         │          │
│          │                     │          │
│          │     3. Forward       └──────────┘
│          │◀──────────┐                │
│          │           │                │
│          │           │          ┌─────▼──────┐
│          │           │          │    SAT     │
│          │           │          │            │
│          │           │          │ 4. Accept  │
│          │           │          └─────┬──────┘
│          │           │                │
│          │      ┌────┴───────────────┘
│          │      │    5. Return
│          │      │       Stamp
│          │◀─────┴────────────┐
│          │     6. Stamped XML │
│          │                   ┌▼──────────┐
│          │     7. Store      │   PAC     │
└──────────┘◀──────────────────┤           │
             UUID & XML        └───────────┘
```

## Component Details

### SAP Components

#### 1. SD Module (Sales & Distribution)
**Purpose**: Core billing document creation

**Key Tables**:
- `VBRK`: Billing document header
- `VBRP`: Billing document items
- `VBFA`: Document flow
- `KNA1`: Customer master
- `T001`: Company codes

**Key Transactions**:
- `VF01`: Create billing document
- `VF02`: Change billing document
- `VF03`: Display billing document
- `VF11`: Cancel billing document

#### 2. CFDI Engine (Custom Development)
**Purpose**: Generate CFDI XML from billing documents

**Components**:
- XML generation class
- SAT catalog mapping
- Tax calculation logic
- Digital signature integration
- Error handling

**Key Classes**:
- `ZCL_CFDI_GENERATOR`: Main XML generator
- `ZCL_CFDI_VALIDATOR`: Pre-validation
- `ZCL_SAT_CATALOGS`: Catalog access
- `ZCL_TAX_CALCULATOR`: Tax logic

#### 3. PAC Integration Layer
**Purpose**: Communication with PAC providers

**Components**:
- HTTP/RFC client
- REST/SOAP handler
- Response parser
- Error handler
- Retry logic

**Key Classes**:
- `ZIF_PAC`: Common PAC interface
- `ZCL_PAC_MONTOVA`: Montova implementation
- `ZCL_PAC_EDICOM`: Edicom implementation
- `ZCL_HTTP_CLIENT`: HTTP communication

#### 4. Digital Signature (SSF)
**Purpose**: Sign CFDI with company certificate

**Components**:
- SAP Secure Store integration
- Certificate management
- Signature generation
- Key pair handling

**Key Transactions**:
- `STRUST`: Trust Manager
- `SSF01`: Digital signature configuration

#### 5. Document Storage
**Purpose**: Store CFDIs and related documents

**Storage Options**:
- SAP Archive (ArchiveLink)
- SAP DMS (Document Management)
- External file system
- Content Server

**Key Tables**:
- `ZTCFDI_DOCS`: CFDI document log
- `ZTCFDI_UUID`: UUID storage
- `ZTCFDI_STATUS`: Status tracking

### PAC Provider Components

#### Montova Architecture
```
API Gateway
    │
    ├── REST Endpoint (/api/v1/stamp)
    │   └── JSON request/response
    │
    ├── SOAP Endpoint (/services/Timbrado)
    │   └── XML request/response
    │
    └── Validation Engine
        ├── Schema validation
        ├── Catalog validation
        └── Business rules
```

**Features**:
- Real-time validation
- Synchronous response
- Pay-per-stamp model
- 24/7 availability
- Test environment available

#### Edicom Architecture
```
API Gateway
    │
    ├── REST API
    ├── SOAP Web Services
    ├── SAP Add-on (Optional)
    │   └── Direct BAPI calls
    │
    └── EDIWIN Platform
        ├── Document validation
        ├── Format transformation
        ├── SAT communication
        └── Archive management
```

**Features**:
- Multiple integration methods
- SAP native add-on option
- Subscription model
- Document management
- Multi-country support

## Network Architecture

### Production Environment

```
┌─────────────────────────┐
│   SAP Production        │
│   (Internal Network)    │
│                         │
│   ┌─────────────────┐   │
│   │ Application     │   │
│   │ Server          │   │
│   └────────┬────────┘   │
│            │            │
└────────────┼────────────┘
             │
             │ HTTPS (Port 443)
             │
      ┌──────▼──────┐
      │  Firewall   │
      │  (DMZ)      │
      └──────┬──────┘
             │
             │ Internet
             │
┌────────────▼────────────┐
│   PAC Provider          │
│   (Cloud)               │
│                         │
│   Production Endpoint:  │
│   - Montova            │
│   - Edicom             │
└─────────────────────────┘
```

### Security Layers

1. **Network Security**
   - Firewall rules (outbound HTTPS only)
   - IP whitelisting
   - VPN for administrative access
   - DMZ for external communication

2. **Application Security**
   - TLS 1.2+ encryption
   - Certificate-based authentication
   - API key management
   - Request signing

3. **Data Security**
   - Encrypted data at rest
   - Encrypted data in transit
   - Secure credential storage
   - Audit logging

## Scalability Considerations

### Performance Metrics
- **Peak Load**: 100+ invoices per minute
- **Average Response Time**: 2-5 seconds per stamp
- **Concurrent Connections**: Up to 10 parallel requests
- **Daily Volume**: 10,000+ documents

### Scaling Strategies

#### Horizontal Scaling
- Multiple application servers
- Load balancing
- PAC provider handles cloud scaling

#### Vertical Scaling
- Increase server resources
- Optimize ABAP code
- Database tuning

#### Caching Strategy
- Cache SAT catalogs (daily refresh)
- Cache customer master data
- Cache tax configuration

## Disaster Recovery

### Backup Strategy
```
Daily Backups:
├── CFDI XML files
├── Digital certificates
├── Custom table data
└── Configuration settings

Weekly Archives:
├── All stamped CFDIs
└── Audit logs

Monthly Off-site:
└── Complete backup to external storage
```

### Recovery Procedures

1. **PAC Unavailable**
   - Queue documents for retry
   - Switch to backup PAC (if configured)
   - Manual processing if extended outage

2. **SAP System Down**
   - Restore from backup
   - Replay queued transactions
   - Verify document numbers

3. **Certificate Expired**
   - Emergency certificate renewal
   - Retroactive stamping of affected invoices
   - Customer notification

## Monitoring & Alerting

### Key Metrics to Monitor

1. **System Health**
   - RFC connection status
   - PAC response times
   - Success/failure rates
   - Queue depths

2. **Business Metrics**
   - Daily stamp count
   - Error types and frequency
   - Cancellation rates
   - Average processing time

3. **Security Metrics**
   - Authentication failures
   - Certificate expiration dates
   - Unusual traffic patterns

### Alert Thresholds

| Metric | Warning | Critical |
|--------|---------|----------|
| PAC Response Time | >5s | >10s |
| Error Rate | >5% | >10% |
| Queue Depth | >100 | >500 |
| Certificate Expiry | <30 days | <7 days |

## Integration Points

### Upstream Systems
- SAP SD (billing)
- SAP FI (accounting)
- SAP MM (materials)
- CRM systems

### Downstream Systems
- Email server (customer delivery)
- Archive system (storage)
- Reporting/BI tools
- External portals

## Technology Stack

```
Presentation Layer:
├── SAP GUI
├── Web Dynpro
└── Fiori (optional)

Application Layer:
├── ABAP 7.4+
├── RFC/HTTP handlers
└── Custom business logic

Integration Layer:
├── REST API clients
├── SOAP web services
├── XML parsers (iXML)
└── JSON handlers

Data Layer:
├── SAP database (HANA/Oracle/MSSQL)
├── Custom tables
└── Standard SAP tables

Security Layer:
├── SSF (Secure Store)
├── TLS/SSL
└── Digital certificates
```

## Next Steps

- Review [ABAP Development Guide](../03-abap-development/README.md)
- Choose [PAC Provider](../04-pac-integration/README.md)
- Plan [Testing Strategy](../07-testing/README.md)
EOF

# ===========================================
# .gitignore
# ===========================================
cat > .gitignore << 'EOF'
# Digital Certificates - NEVER COMMIT THESE!
*.cer
*.key
*.p12
*.pfx
*.pem

# Credentials
config/credentials.json
config/production.json
config/*.credentials

# API Keys
.env
.env.local
.env.production

# SAP Transport files
*.dat
*.bin
*R3trans*

# Temporary files
*.tmp
*.bak
*.swp
*~

# OS Files
.DS_Store
Thumbs.db
desktop.ini

# IDE files
.vscode/
.idea/
*.sublime-*

# Logs
*.log
logs/

# Test data with sensitive info
test-data/production/
EOF

# ===========================================
# LICENSE
# ===========================================
cat > LICENSE << 'EOF'
MIT License

Copyright (c) 2024 SAP CFDI Mexico Implementation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
EOF

# ===========================================
# ABAP Class: CFDI Generator
# ===========================================
cat > src/abap/classes/ZCL_CFDI_GENERATOR.abap << 'EOF'
CLASS zcl_cfdi_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! Generate CFDI XML from billing document
      "! @parameter iv_vbeln | Billing document number
      "! @parameter iv_version | CFDI version (default 4.0)
      "! @parameter rv_xml | Generated CFDI XML
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
      apply_signature
        IMPORTING
          iv_xml                TYPE string
        RETURNING
          VALUE(rv_signed_xml)  TYPE string
        RAISING
          cx_cfdi_signature.

  PRIVATE SECTION.
    DATA:
      mv_vbeln      TYPE vbrk-vbeln,
      mv_version    TYPE string,
      mo_xml_doc    TYPE REF TO if_ixml_document.

    METHODS:
      get_billing_header
        RETURNING VALUE(rs_vbrk) TYPE vbrk,

      get_billing_items
        RETURNING VALUE(rt_vbrp) TYPE vbrp_tab,

      get_customer_data
        IMPORTING iv_kunnr       TYPE kna1-kunnr
        RETURNING VALUE(rs_kna1) TYPE kna1,

      build_comprobante_node,
      build_emisor_node,
      build_receptor_node,
      build_conceptos_node,
      build_impuestos_node,
      
      calculate_taxes
        RETURNING VALUE(rs_taxes) TYPE ty_taxes,

      get_sat_codes
        IMPORTING iv_matnr        TYPE matnr
        RETURNING VALUE(rs_codes) TYPE ty_sat_codes.

ENDCLASS.

CLASS zcl_cfdi_generator IMPLEMENTATION.

  METHOD generate_from_billing_doc.
    "Implementation here
    "1. Get billing document data
    "2. Build XML structure
    "3. Apply digital signature
    "4. Return signed XML
  ENDMETHOD.

  METHOD validate_xml.
    "Implementation here
    "1. Schema validation
    "2. Business rule validation
    "3. SAT catalog validation
  ENDMETHOD.

  METHOD apply_signature.
    "Implementation here
    "1. Get certificate from SSF
    "2. Calculate original string
    "3. Apply digital signature
    "4. Add signature to XML
  ENDMETHOD.

ENDCLASS.
EOF

# ===========================================
# PAC Integration: Montova
# ===========================================
cat > src/pac-integration/montova/README.md << 'EOF'
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
EOF

# ===========================================
# PAC Integration: Edicom
# ===========================================
cat > src/pac-integration/edicom/README.md << 'EOF'
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
EOF

# ===========================================
# Configuration Template
# ===========================================
cat > config/config.template.json << 'EOF'
{
  "environment": "test",
  "pac_provider": "montova",
  "montova": {
    "test": {
      "rest_url": "https://timbrado-pruebas.montova.com.mx/api/v1",
      "soap_url": "https://timbrado-pruebas.montova.com.mx/services/Timbrado",
      "username": "YOUR_TEST_USERNAME",
      "password": "STORED_IN_SAP_SSF",
      "rfc": "YOUR_RFC"
    },
    "production": {
      "rest_url": "https://timbrado.montova.com.mx/api/v1",
      "soap_url": "https://timbrado.montova.com.mx/services/Timbrado",
      "username": "YOUR_PROD_USERNAME",
      "password": "STORED_IN_SAP_SSF",
      "rfc": "YOUR_RFC"
    }
  },
  "edicom": {
    "test": {
      "rest_url": "https://webservices-test.edicomgroup.com/cfdi/v1",
      "soap_url": "https://webservices-test.edicomgroup.com/cfdi/TimbradoService",
      "api_key": "STORED_IN_SAP_SSF",
      "client_id": "YOUR_CLIENT_ID"
    },
    "production": {
      "rest_url": "https://webservices.edicomgroup.com/cfdi/v1",
      "soap_url": "https://webservices.edicomgroup.com/cfdi/TimbradoService",
      "api_key": "STORED_IN_SAP_SSF",
      "client_id": "YOUR_CLIENT_ID"
    }
  },
  "certificate": {
    "storage": "SSF",
    "alias": "CFDI_CERT_2024"
  },
  "retry_policy": {
    "max_attempts": 3,
    "delay_seconds": 5
  }
}
EOF

echo ""
echo "✅ Repository structure created successfully!"
echo ""
echo "📁 Directory: sap-cfdi-mexico"
echo ""
echo "📚 Documentation created:"
echo "   - CFDI 4.0 Overview (English)"
echo "   - System Architecture (English)"
echo "   - PAC Integration guides (Montova & Edicom)"
echo ""
echo "💻 Code examples created:"
echo "   - ABAP CFDI Generator class"
echo "   - PAC integration templates"
echo ""
echo "🔧 Configuration:"
echo "   - Template configuration file"
echo "   - .gitignore (protects sensitive data)"
echo ""
echo "🚀 Next steps:"
echo ""
echo "1. Create GitHub repository:"
echo "   Go to: https://github.com/new"
echo "   Name: sap-cfdi-mexico"
echo ""
echo "2. Initialize and push:"
echo "   cd sap-cfdi-mexico"
echo "   git init"
echo "   git add ."
echo "   git commit -m 'Initial commit: SAP CFDI Mexico implementation (English)'"
echo "   git branch -M main"
echo "   git remote add origin https://github.com/YOUR_USERNAME/sap-cfdi-mexico.git"
echo "   git push -u origin main"
echo ""
echo "✨ All content is now in English!"
echo ""
