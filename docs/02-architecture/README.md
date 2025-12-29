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
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
│   System     │                 │   Provider   │                 │ (Tax Admin)  │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  1. Send unsigned CFDI XML     │                                │
       │───────────────────────────────▶│                                │
       │                                │                                │
       │                                │  2. Validate XML               │
       │                                │     - Schema check             │
       │                                │     - Business rules           │
       │                                │     - SAT catalogs             │
       │                                │                                │
       │  If validation fails:          │                                │
       │◀───────────────────────────────│                                │
       │  Error response (stop here)    │                                │
       │                                │                                │
       │                                │  3. Forward validated XML      │
       │                                │───────────────────────────────▶│
       │                                │                                │
       │                                │                                │  4. SAT validates
       │                                │                                │     and stores CFDI
       │                                │                                │
       │                                │  5. SAT confirms acceptance    │
       │                                │◀───────────────────────────────│
       │                                │                                │
       │                                │  6. PAC generates stamp:       │
       │                                │     - Create UUID              │
       │                                │     - Add TimbreFiscalDigital  │
       │                                │     - Apply PAC seal           │
       │                                │     - Apply SAT seal           │
       │                                │                                │
       │  7. Return stamped CFDI        │                                │
       │◀───────────────────────────────│                                │
       │     - UUID (Folio Fiscal)      │                                │
       │     - Stamped XML              │                                │
       │     - Certification date       │                                │
       │                                │                                │
       │  8. Store in SAP:              │                                │
       │     - Save UUID to billing doc │                                │
       │     - Archive stamped XML      │                                │
       │     - Update document status   │                                │
       │                                │                                │
```

#### Detailed Step-by-Step Explanation

**Step 1: SAP Sends Unsigned XML**
- SAP generates CFDI XML from billing document
- Applies company's digital signature
- Sends to PAC via REST or SOAP API

**Step 2: PAC Validation**
- Validates XML structure against CFDI 4.0 schema
- Checks business rules (amounts, dates, etc.)
- Verifies codes against SAT catalogs
- Validates company's digital signature
- If any validation fails, returns error to SAP immediately

**Step 3: PAC Forwards to SAT**
- Only if validation passes
- PAC sends validated XML to SAT's reception service
- This happens in real-time

**Step 4: SAT Processing**
- SAT performs its own validation
- Stores CFDI in national database
- Makes invoice available in public verification portal

**Step 5: SAT Confirmation**
- SAT returns acceptance confirmation to PAC
- Includes authorization for PAC to proceed with stamping

**Step 6: PAC Creates Stamp**
- Generates unique UUID (Folio Fiscal)
- Creates TimbreFiscalDigital complement containing:
  - UUID
  - Certification date/time
  - PAC digital seal
  - SAT digital seal
  - Certificate numbers
- Adds this complement to original XML

**Step 7: Return to SAP**
- PAC sends stamped XML back to SAP
- Includes UUID and all seal information
- This is now the legally valid CFDI

**Step 8: SAP Storage**
- Stores UUID in billing document table
- Archives complete stamped XML
- Updates document status to "Stamped"
- Ready to send to customer

#### Timing
- **Normal process**: 2-5 seconds total
- **Peak times**: Up to 10 seconds
- **Timeout**: Usually set at 30 seconds

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
