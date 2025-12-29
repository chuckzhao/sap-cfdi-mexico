# Deployment Guide

Complete guide for deploying SAP CFDI 4.0 implementation from development to production.

## Table of Contents

- [Overview](#overview)
- [Pre-Deployment Checklist](#pre-deployment-checklist)
- [Environment Setup](#environment-setup)
- [Certificate Management](#certificate-management)
- [Transport Management](#transport-management)
- [Configuration](#configuration)
- [Testing Strategy](#testing-strategy)
- [Production Deployment](#production-deployment)
- [Post-Deployment](#post-deployment)
- [Rollback Procedures](#rollback-procedures)

## Overview

This guide covers the complete deployment process for CFDI 4.0 implementation across SAP landscape:

```
Development (DEV) → Quality (QA) → Production (PRD)
```

**Estimated Timeline:**
- Development: Configuration and customization
- Quality Testing: Thorough validation
- Production: Final deployment

## Pre-Deployment Checklist

### Legal Requirements

- [ ] RFC registered and active with SAT
- [ ] Digital certificate (CSD) obtained from SAT
- [ ] Digital certificate valid for at least 6 months
- [ ] PAC contract signed and active
- [ ] Legal entity information verified
- [ ] Tax regime codes confirmed

### SAP System Requirements

- [ ] SAP ECC 6.0+ or S/4HANA installed
- [ ] SAP Basis 7.40 or higher
- [ ] SD module configured
- [ ] FI module configured
- [ ] SSF (Secure Store and Forward) available
- [ ] HTTP/HTTPS connectivity enabled
- [ ] Adequate system resources (CPU, memory, disk)

### Access Requirements

- [ ] ABAP development authorization (S_DEVELOP)
- [ ] Transport authorization (S_CTS_ADMI)
- [ ] SSF administration access (STRUST)
- [ ] Table maintenance access for configuration
- [ ] Authorization to create custom Z* objects

### PAC Requirements

- [ ] PAC test environment credentials
- [ ] PAC production environment credentials
- [ ] API endpoints documented
- [ ] Service level agreement (SLA) confirmed
- [ ] Support contact information

## Environment Setup

### Development Environment (DEV)

#### 1. Create Development Package

```abap
Transaction: SE80

Package Name: Z_CFDI_MX
Description: CFDI 4.0 Mexico Implementation
Application Component: FI-LOC (Financial Accounting - Localization)
Software Component: HOME (Local Objects)
Package Type: Development Package
```

#### 2. Create Transport Request

```abap
Transaction: SE09/SE10

Request Type: Customizing Request
Description: CFDI 4.0 Implementation - Initial Setup
Target System: <Your landscape>
```

#### 3. Import ABAP Objects

Import in this order:
1. Type definitions and structures
2. Exception classes
3. Generator classes
4. PAC integration classes
5. Utility classes
6. Test programs

```bash
# From repository root
Transaction: SE38 or SE80
- Import classes from src/abap/classes/
- Activate all objects
- Run syntax checks
```

### Quality Environment (QA)

#### Setup Steps

1. **Import Transport**
   ```abap
   Transaction: STMS
   - Import from DEV
   - Perform import with test mode first
   - Verify all objects imported successfully
   ```

2. **Configure Test Data**
   - Create test customers
   - Create test materials
   - Create test sales orders
   - Generate test billing documents

3. **Configure PAC Test Connection**
   - Use PAC test/sandbox environment
   - Configure test credentials
   - Test connectivity

### Production Environment (PRD)

Production setup covered in [Production Deployment](#production-deployment) section.

## Certificate Management

### Certificate Types

**CSD (Certificado de Sello Digital)**
- Used for signing CFDI documents
- Obtained from SAT
- Valid for 4 years
- Required for all CFDI issuance

**FIEL (Firma Electrónica Avanzada)**
- Used for SAT portal access
- Personal certificate (not used for CFDI signing)
- Valid for 4 years

### Obtaining Certificates from SAT

1. **Access SAT Portal**
   - Visit: https://www.sat.gob.mx
   - Login with FIEL

2. **Request CSD Certificate**
   ```
   Navigation: Trámites > Certificado de Sello Digital
   - Fill out application form
   - Generate private key and CSR
   - Submit request
   - Download certificate when approved (usually 24-48 hours)
   ```

3. **Certificate Files Received**
   - Certificate file: `.cer` (public certificate)
   - Private key: `.key` (encrypted private key)
   - Password: Provided during generation

### Import Certificate to SAP

#### Using Transaction STRUST

1. **Access STRUST**
   ```abap
   Transaction: STRUST
   ```

2. **Import Certificate**
   ```
   Steps:
   1. Select SSL Client (Standard) or create custom PSE
   2. PSE > Import Certificate
   3. Select .cer file from SAT
   4. Import private key (.key file)
   5. Enter key password
   6. Give it an alias: CFDI_CERT_2024
   7. Save
   ```

3. **Verify Certificate**
   ```
   - Check valid from/to dates
   - Verify RFC matches your company RFC
   - Test certificate can be used for signing
   ```

#### Certificate Renewal Process

**3 months before expiration:**
1. Request new certificate from SAT
2. Import new certificate to SAP (different alias)
3. Test with new certificate
4. Update configuration to use new certificate
5. Monitor old certificate expiration

**Configuration for automatic alerts:**
```abap
" Set up job to check certificate expiration
" Alert if less than 90 days remaining
```

## Transport Management

### Transport Strategy

```
DEV → QA → PRD
 └─── Transport Request ────┘
```

### Creating Transports

#### 1. Workbench Transport (Code)
```abap
Transaction: SE09

Request Type: Workbench Request
Description: CFDI 4.0 - Core Classes v1.0
Owner: <Your User>

Objects to include:
- Classes (ZCL_*)
- Exception classes (ZCX_*)
- Type groups (if any)
- Function groups (if any)
```

#### 2. Customizing Transport (Configuration)
```abap
Transaction: SE09

Request Type: Customizing Request
Description: CFDI 4.0 - Configuration Tables

Objects to include:
- Custom tables
- Configuration entries
- Number ranges
```

### Transport Checklist

Before releasing transport:
- [ ] All objects activated
- [ ] Syntax check passed
- [ ] Extended program check passed (SLIN)
- [ ] Code inspector passed (SCI)
- [ ] Unit tests executed successfully
- [ ] Documentation updated
- [ ] Transport documented in CHANGELOG

### Release Process

```abap
Transaction: SE09

1. Select your transport request
2. Check all tasks completed
3. Release all tasks
4. Release transport request
5. Verify transport in STMS
6. Document transport number
```

## Configuration

### System Configuration Tables

Create custom configuration table:

```abap
Table: ZCFDI_CONFIG

Fields:
- MANDT (Client)
- ENVIRONMENT (DEV/QA/PRD)
- PAC_PROVIDER (Montova/Edicom)
- REST_URL
- SOAP_URL
- USERNAME (stored reference)
- PASSWORD (stored in SSF)
- RFC
- CERT_ALIAS
- RETRY_MAX
- RETRY_DELAY
- ACTIVE (checkbox)
```

### Configuration per Environment

#### Development
```
Environment: DEV
PAC Provider: Montova
URL: https://timbrado-pruebas.montova.com.mx/api/v1
Username: dev_user
RFC: Your RFC
Certificate: CFDI_CERT_DEV
Max Retries: 3
Active: Yes
```

#### Quality
```
Environment: QA
PAC Provider: Montova
URL: https://timbrado-pruebas.montova.com.mx/api/v1
Username: qa_user
RFC: Your RFC
Certificate: CFDI_CERT_QA
Max Retries: 3
Active: Yes
```

#### Production
```
Environment: PRD
PAC Provider: Montova
URL: https://timbrado.montova.com.mx/api/v1
Username: prod_user
RFC: Your RFC
Certificate: CFDI_CERT_PRD
Max Retries: 5
Active: Yes
```

### Number Ranges

Configure number ranges for CFDI:

```abap
Transaction: SNRO

Object: ZCFDI_NR
Number Range:
- From: 0000000001
- To: 9999999999
- External: No
- Buffering: Yes, Main memory buffering
- Buffer size: 100
```

## Testing Strategy

### Unit Testing

Test individual classes:

```abap
Transaction: SE80 or SE37

Test cases:
1. XML generation with valid data
2. XML validation
3. Certificate loading
4. Signature application
5. Error handling
6. Configuration loading
```

### Integration Testing

Test complete flow:

1. **Create Test Billing Document**
   ```abap
   Transaction: VF01
   - Create simple invoice
   - Note billing document number
   ```

2. **Generate CFDI**
   ```abap
   - Run CFDI generation program
   - Verify XML structure
   - Check all required fields
   - Validate against XSD schema
   ```

3. **Sign CFDI**
   ```abap
   - Apply digital signature
   - Verify signature block
   - Validate certificate used
   ```

4. **Send to PAC**
   ```abap
   - Send to PAC test environment
   - Verify successful stamping
   - Receive and validate UUID
   - Store stamped XML
   ```

5. **Verify Storage**
   ```abap
   - Check UUID stored in billing doc
   - Verify XML archived correctly
   - Generate PDF
   - Test customer delivery
   ```

### Performance Testing

Test system performance:

```abap
Test Scenarios:
1. Single invoice: < 5 seconds total
2. Batch of 100 invoices: < 5 minutes
3. Concurrent processing: 10 users
4. Peak load: 1000 invoices/hour
```

### User Acceptance Testing (UAT)

Involve business users:

- [ ] Test standard invoice creation
- [ ] Test credit note creation
- [ ] Test payment complement
- [ ] Verify PDF output format
- [ ] Test customer email delivery
- [ ] Verify cancellation process
- [ ] Test error scenarios and recovery

## Production Deployment

### Pre-Production Steps

**1 Week Before:**
- [ ] Schedule deployment window
- [ ] Notify stakeholders
- [ ] Prepare rollback plan
- [ ] Complete all testing in QA
- [ ] Get formal sign-off from business
- [ ] Schedule go-live support

**1 Day Before:**
- [ ] Verify PAC production credentials
- [ ] Import production certificates
- [ ] Prepare production configuration
- [ ] Backup current system
- [ ] Verify rollback procedure
- [ ] Confirm support team availability

### Deployment Day

#### Morning (4 hours before cutover)

1. **System Backup**
   ```
   - Complete database backup
   - Backup configuration tables
   - Document current system state
   ```

2. **Import Transport**
   ```abap
   Transaction: STMS
   - Import transport to PRD
   - Activate all objects
   - Run post-import checks
   ```

3. **Configure Production**
   ```abap
   - Update configuration table
   - Set production PAC URLs
   - Configure production credentials
   - Set production certificate alias
   - Verify number ranges
   ```

4. **Smoke Tests**
   ```abap
   - Test XML generation
   - Test certificate loading
   - Test PAC connectivity
   - Verify configuration
   ```

#### Cutover (Go-Live)

1. **Process Test Invoice**
   ```
   - Create real billing document (low value)
   - Generate CFDI
   - Send to PAC production
   - Verify UUID received
   - Validate with SAT portal
   ```

2. **Monitor**
   ```
   - Watch for errors in system log (SM21)
   - Monitor application log (SLG1)
   - Check PAC response times
   - Verify UUID storage
   ```

3. **Enable for Users**
   ```
   - Enable automatic CFDI generation
   - Notify users system is live
   - Provide user support
   ```

## Post-Deployment

### Day 1 Activities

- [ ] Monitor all CFDI generations
- [ ] Track PAC response times
- [ ] Review error logs
- [ ] Collect user feedback
- [ ] Address any issues immediately
- [ ] Document lessons learned

### Week 1 Activities

- [ ] Daily error log review
- [ ] Performance monitoring
- [ ] User training if needed
- [ ] Fine-tune configuration
- [ ] Update documentation
- [ ] Plan optimizations

### Ongoing Monitoring

**Daily:**
- Check for failed CFDIs
- Monitor PAC connectivity
- Review error logs

**Weekly:**
- Performance metrics
- Volume statistics
- Error trends

**Monthly:**
- Certificate expiration check
- PAC service review
- System optimization review
- Disaster recovery test

## Rollback Procedures

### When to Rollback

Critical issues requiring rollback:
- System crashes or instability
- Massive CFDI generation failures
- PAC integration completely broken
- Data corruption
- Performance degradation making system unusable

### Rollback Steps

1. **Stop CFDI Processing**
   ```abap
   - Disable automatic generation
   - Stop all batch jobs
   - Notify users
   ```

2. **Restore Previous Version**
   ```abap
   Transaction: STMS
   - Import previous version transport
   - Restore configuration tables from backup
   - Restore certificates if changed
   ```

3. **Verify System**
   ```abap
   - Test basic functionality
   - Verify old version working
   - Check data integrity
   ```

4. **Communicate**
   - Notify stakeholders
   - Document issue
   - Plan corrective actions

### Partial Rollback

If only specific component failing:
- Disable just that component
- Fallback to manual process
- Fix and redeploy component only

## Best Practices

### Deployment Best Practices

1. **Always deploy during low-usage hours**
2. **Have rollback plan ready**
3. **Test in QA first, thoroughly**
4. **Keep stakeholders informed**
5. **Monitor closely after deployment**
6. **Document everything**

### Operational Best Practices

1. **Monitor certificate expiration**
2. **Keep PAC credentials current**
3. **Regularly review error logs**
4. **Maintain test environment**
5. **Keep documentation updated**
6. **Regular disaster recovery tests**

## Support Contacts

### Internal
- SAP Basis Team: [Contact]
- ABAP Development: [Contact]
- Business Process Owner: [Contact]

### External
- PAC Provider Support: [Contact]
- SAT Support: 55-627-22-728
- System Integrator: [Contact]

## Appendix

### A. Transport Content Checklist

```
Classes:
☐ ZCL_CFDI_GENERATOR
☐ ZCL_CFDI_VALIDATOR
☐ ZCL_PAC_MONTOVA
☐ ZCL_PAC_EDICOM
☐ ZCX_CFDI_GENERATION
☐ ZCX_CFDI_SIGNATURE

Tables:
☐ ZCFDI_CONFIG
☐ ZCFDI_LOG

Programs:
☐ ZCFDI_GENERATE
☐ ZCFDI_BATCH
```

### B. Configuration Template

See [config.template.json](../../config/config.template.json)

### C. Troubleshooting

See [Troubleshooting Guide](../04-troubleshooting/README.md)

---

**Document Version:** 1.0
**Last Updated:** December 2024
**Next Review:** March 2025
