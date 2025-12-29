# Troubleshooting Guide

Comprehensive troubleshooting guide for common CFDI implementation issues.

## Table of Contents

- [Quick Diagnostic](#quick-diagnostic)
- [Certificate Issues](#certificate-issues)
- [PAC Integration Issues](#pac-integration-issues)
- [XML Generation Issues](#xml-generation-issues)
- [Performance Issues](#performance-issues)
- [SAT Validation Errors](#sat-validation-errors)
- [Debugging Techniques](#debugging-techniques)
- [Error Code Reference](#error-code-reference)
- [Support Escalation](#support-escalation)

## Quick Diagnostic

### Problem: CFDI Generation Fails

**Quick checks:**

1. **Check System Log**
   ```abap
   Transaction: SM21
   Filter: Application errors in last hour
   ```

2. **Check Application Log**
   ```abap
   Transaction: SLG1
   Object: CFDI
   Subobject: GENERATION
   ```

3. **Verify Configuration**
   ```abap
   Transaction: SE16
   Table: ZCFDI_CONFIG
   Check environment settings
   ```

4. **Test Certificate**
   ```abap
   Transaction: STRUST
   Verify certificate valid and accessible
   ```

### Decision Tree

```
CFDI Generation Failed
│
├─ XML Generation Error?
│  ├─ Missing Data → Check billing document
│  ├─ Invalid Data → Validate SAT catalogs
│  └─ System Error → Check dumps (ST22)
│
├─ Certificate Error?
│  ├─ Not Found → Check STRUST
│  ├─ Expired → Renew certificate
│  └─ Invalid → Verify certificate matches RFC
│
├─ PAC Error?
│  ├─ Connection Failed → Check network/firewall
│  ├─ Authentication Failed → Verify credentials
│  └─ Validation Failed → Check XML against PAC rules
│
└─ Other Error?
   └─ Check detailed logs (SLG1, SM21, ST22)
```

## Certificate Issues

### Error: Certificate Not Found

**Symptom:**
```
Error: SSF_KRN_ERROR - Certificate alias 'CFDI_CERT_2024' not found
```

**Solution:**

1. **Verify Certificate in STRUST**
   ```abap
   Transaction: STRUST
   Steps:
   1. Select SSL Client (Standard)
   2. Look for certificate alias
   3. If not found, import certificate
   ```

2. **Import Certificate**
   ```abap
   STRUST > PSE > Import
   - Select .cer file
   - Import .key file
   - Enter password
   - Save with correct alias
   ```

3. **Update Configuration**
   ```abap
   Update ZCFDI_CONFIG table with correct alias
   ```

### Error: Certificate Expired

**Symptom:**
```
Error: Certificate expired on [DATE]
SAT Error: 305 - Invalid certificate
```

**Solution:**

1. **Obtain New Certificate from SAT**
   - Access SAT portal
   - Request new CSD certificate
   - Download when approved

2. **Import New Certificate**
   ```abap
   Transaction: STRUST
   - Import new certificate with new alias
   - Keep old certificate until cutover
   ```

3. **Test New Certificate**
   ```abap
   - Test with sample CFDI
   - Verify stamping works
   ```

4. **Update Configuration**
   ```abap
   - Update certificate alias in config
   - Monitor first few CFDIs
   - Remove old certificate after 1 week
   ```

### Error: Certificate RFC Mismatch

**Symptom:**
```
PAC Error: RFC in certificate (AAA010101AAA) does not match
RFC in CFDI (BBB020202BBB)
```

**Solution:**

1. **Verify Certificate RFC**
   ```abap
   Transaction: STRUST
   - Double-click certificate
   - Check Subject DN contains correct RFC
   ```

2. **Verify CFDI RFC**
   ```abap
   - Check company code master data
   - Verify RFC in configuration
   - Check XML emisor RFC field
   ```

3. **Ensure Match**
   - Certificate RFC must match emisor RFC
   - Get new certificate if wrong RFC
   - Cannot use one company's cert for another

## PAC Integration Issues

### Error: Connection Timeout

**Symptom:**
```
Error: HTTP timeout after 30 seconds
PAC connection failed
```

**Solution:**

1. **Test Network Connectivity**
   ```abap
   Transaction: SMICM
   Go to: Trace File > HTTP Trace Level 3

   Test URL connectivity:
   - Ping PAC server
   - Test HTTPS from browser
   - Check firewall rules
   ```

2. **Check SAP Configuration**
   ```abap
   Transaction: SMICM
   Services:
   - Verify HTTP/HTTPS service active
   - Check port configuration
   - Verify SSL configuration
   ```

3. **Firewall Configuration**
   ```
   Outbound rules needed:
   - Montova:
     - timbrado.montova.com.mx:443
     - timbrado-pruebas.montova.com.mx:443
   - Edicom:
     - webservices.edicomgroup.com:443
     - webservices-test.edicomgroup.com:443
   ```

4. **Increase Timeout**
   ```abap
   If network slow:
   - Increase timeout in code (from 30 to 60 seconds)
   - Configure retry logic
   - Monitor PAC performance
   ```

### Error: Authentication Failed

**Symptom:**
```
PAC Error: 401 Unauthorized
Invalid credentials
```

**Solution:**

1. **Verify Credentials**
   ```abap
   Check configuration table:
   - Username correct?
   - Password not expired?
   - Using correct environment (test vs. prod)?
   ```

2. **Test Credentials Manually**
   ```bash
   # Test with curl or Postman
   curl -X POST https://timbrado-pruebas.montova.com.mx/api/v1/stamp \
     -u username:password \
     -H "Content-Type: application/json"
   ```

3. **Reset Password**
   - Contact PAC provider
   - Reset password in PAC portal
   - Update in SAP configuration
   - Test immediately

4. **Check Account Status**
   - Verify account not suspended
   - Check billing current with PAC
   - Verify contract active

### Error: PAC Validation Failed

**Symptom:**
```
PAC Error 301: Invalid XML structure
PAC Error 302: Invalid digital signature
PAC Error 304: Invalid RFC
```

**Solutions by Error Code:**

**Error 301 - Invalid XML:**
```abap
1. Validate XML against XSD schema
2. Check for special characters
3. Verify encoding (UTF-8)
4. Check all required fields present
5. Validate against CFDI 4.0 spec
```

**Error 302 - Invalid Signature:**
```abap
1. Verify certificate valid
2. Check signature algorithm
3. Regenerate original string
4. Re-sign with correct certificate
```

**Error 304 - Invalid RFC:**
```abap
1. Verify RFC format (AAA010101AAA)
2. Check RFC active on SAT portal
3. Verify RFC in certificate matches
4. Check recipient RFC valid
```

## XML Generation Issues

### Error: Missing Required Field

**Symptom:**
```
Error: Required field 'UsoCFDI' missing
XML validation failed
```

**Solution:**

1. **Check Master Data**
   ```abap
   Missing fields usually come from incomplete master data:

   Customer Master (XD03):
   - RFC (Tax ID)
   - Tax classification
   - CFDI usage code
   - Postal code

   Material Master (MM03):
   - Product code (SAT catalog)
   - Unit of measure (SAT catalog)
   - Tax classification
   ```

2. **Update Master Data**
   ```abap
   Transaction: XD02 (Customer)
   - Add missing RFC
   - Set tax classification
   - Configure CFDI fields

   Transaction: MM02 (Material)
   - Add SAT product code
   - Set SAT unit code
   - Configure tax codes
   ```

3. **Configure Defaults**
   ```abap
   Set default values for optional fields:
   - Default UsoCFDI: G03 (General expenses)
   - Default payment method: 99 (To be defined)
   - Default payment form: PUE or PPD
   ```

### Error: Invalid Amount Calculation

**Symptom:**
```
PAC Error: Subtotal + Taxes ≠ Total
Amount validation failed
```

**Solution:**

1. **Check Rounding**
   ```abap
   CFDI requires 2 decimal precision:

   Wrong:
   Subtotal: 1000.00
   Tax (16%): 160.001  ← Too many decimals
   Total: 1160.001

   Correct:
   Subtotal: 1000.00
   Tax (16%): 160.00
   Total: 1160.00
   ```

2. **Verify Tax Calculation**
   ```abap
   Check pricing procedure (V/08):
   - Tax condition types correct
   - Tax percentages match SAT requirements
   - Withholding taxes calculated correctly
   ```

3. **Fix Calculation Logic**
   ```abap
   Use proper rounding:

   DATA: lv_tax TYPE p DECIMALS 2.

   lv_tax = lv_subtotal * '0.16'.
   lv_tax = round( val = lv_tax dec = 2 ).  " Force 2 decimals
   lv_total = lv_subtotal + lv_tax.
   ```

### Error: Invalid SAT Catalog Code

**Symptom:**
```
Error: Product code '12345' not found in SAT catalog
Error: Invalid unit code 'EA'
```

**Solution:**

1. **Check SAT Catalogs**
   ```
   Download latest catalogs from:
   http://omawww.sat.gob.mx/tramitesyservicios/Paginas/catalogos_emision_cfdi_complemento.htm

   Common codes:
   Product/Service (ClaveProdServ):
   - 01010101: Live animals - cattle
   - 43231513: Computer servers
   - 50161500: Computer services

   Unit codes (ClaveUnidad):
   - H87: Piece (pieza)
   - KGM: Kilogram
   - E48: Service unit
   ```

2. **Update Material Master**
   ```abap
   Transaction: MM02
   - Add correct SAT product code
   - Set correct SAT unit code
   - Map internal codes to SAT codes
   ```

3. **Create Mapping Table**
   ```abap
   Create table: ZCFDI_SAT_MAPPING

   Internal Code | SAT Code | Description
   EA            | H87      | Each/Piece
   KG            | KGM      | Kilogram
   SRV           | E48      | Service unit
   ```

## Performance Issues

### Problem: Slow CFDI Generation

**Symptom:**
```
Each CFDI takes > 30 seconds
Batch processing very slow
Users complaining about delays
```

**Solutions:**

1. **Profile Performance**
   ```abap
   Transaction: SAT (Runtime Analysis)
   - Run generation with trace
   - Identify bottlenecks
   - Focus on database selects
   ```

2. **Optimize Database Calls**
   ```abap
   Bad (Multiple selects):
   LOOP AT lt_items INTO ls_item.
     SELECT SINGLE * FROM mara WHERE matnr = ls_item-matnr.
   ENDLOOP.

   Good (Single select):
   SELECT * FROM mara
     INTO TABLE @lt_mara
     FOR ALL ENTRIES IN @lt_items
     WHERE matnr = @lt_items-matnr.
   ```

3. **Enable Buffering**
   ```abap
   - Buffer configuration table (ZCFDI_CONFIG)
   - Buffer SAT catalog mappings
   - Cache frequently used certificates
   ```

4. **Parallel Processing**
   ```abap
   For batch jobs:
   - Use parallel processing (multiple work processes)
   - Process in chunks of 100
   - Implement progress tracking
   ```

### Problem: PAC Response Slow

**Symptom:**
```
PAC response time > 10 seconds per CFDI
Timeout errors during peak hours
```

**Solutions:**

1. **Check PAC Performance**
   - Contact PAC provider
   - Review SLA metrics
   - Consider upgrading service tier

2. **Implement Async Processing**
   ```abap
   Instead of:
   1. Generate XML
   2. Send to PAC (wait for response)
   3. Continue

   Use:
   1. Generate XML
   2. Queue for PAC submission
   3. Background job processes queue
   4. Update document when stamped
   ```

3. **Optimize XML Size**
   - Remove unnecessary comments
   - Compress whitespace
   - Only include required fields

## SAT Validation Errors

### Common SAT Errors

| Error | Description | Solution |
|-------|-------------|----------|
| 101 | Invalid RFC format | Verify RFC follows pattern: AAA010101AAA |
| 201 | Invalid certificate | Check certificate not expired/revoked |
| 301 | Invalid XML schema | Validate against CFDI 4.0 XSD |
| 401 | Invalid postal code | Use SAT postal code catalog |
| 501 | Invalid product code | Use valid SAT product code |

### Validation Process

```
Your System → PAC → SAT
                ↓
           Validation:
           - Schema
           - Business rules
           - Catalogs
           - RFC status
                ↓
           Accept/Reject
```

## Debugging Techniques

### Enable Debug Trace

```abap
Transaction: SE38
Program: ZCFDI_GENERATE

In code, add:
BREAK-POINT.  " At strategic points

Or use:
Transaction: /h (debugging mode)
```

### Check Application Log

```abap
Transaction: SLG1

Object: CFDI
Subobject: GENERATION

Add logging in code:
CALL FUNCTION 'BAL_LOG_MSG_ADD'
  EXPORTING
    i_log_handle = lv_log_handle
    i_s_msg      = ls_message.
```

### Analyze HTTP Traffic

```abap
Transaction: SMICM

Menu: Trace File > Increase Trace Level
Set: Trace Level 3

This logs all HTTP communication for analysis
```

### Check Short Dumps

```abap
Transaction: ST22

Recent dumps:
- ABAP runtime errors
- Detailed error information
- Where clause analysis
- Proposed solutions
```

## Error Code Reference

### Internal Error Codes (Custom)

| Code | Description | Component |
|------|-------------|-----------|
| CFDI-001 | Billing document not found | Generator |
| CFDI-002 | Customer master data incomplete | Generator |
| CFDI-003 | Material master data incomplete | Generator |
| CFDI-010 | XML generation failed | Generator |
| CFDI-020 | Certificate error | Signature |
| CFDI-030 | PAC connection failed | PAC Interface |
| CFDI-031 | PAC authentication failed | PAC Interface |
| CFDI-032 | PAC validation failed | PAC Interface |
| CFDI-040 | UUID storage failed | Storage |

### PAC Error Codes

**Montova:**
| Code | Description |
|------|-------------|
| 301 | Invalid XML structure |
| 302 | Invalid digital signature |
| 303 | Duplicate folio |
| 304 | Invalid RFC |
| 305 | Invalid certificate |

**Edicom:**
| Code | Description |
|------|-------------|
| E001 | Authentication failed |
| E002 | Invalid XML format |
| E003 | Certificate error |
| E004 | SAT rejection |
| E005 | Duplicate UUID |

## Support Escalation

### Level 1: Self-Service

1. Check this troubleshooting guide
2. Review [FAQ](../05-faq/README.md)
3. Search GitHub issues
4. Check PAC provider documentation

### Level 2: Internal Support

Contact:
- SAP Basis team (system issues)
- ABAP development team (code issues)
- Finance team (business process issues)

Provide:
- Error message
- Billing document number
- Transaction code used
- System logs (SM21, SLG1)
- Short dump (ST22) if any

### Level 3: PAC Support

Contact PAC provider when:
- PAC validation errors
- Connection issues (after network checked)
- Authentication issues (after credentials verified)
- Performance issues on PAC side

Provide:
- RFC
- Timestamp of error
- Error code/message
- Sample XML (if requested)
- Request ID (if available)

### Level 4: SAT Support

Contact SAT for:
- Certificate issues
- RFC validation issues
- Regulatory questions

Contact: 55-627-22-728

## Preventive Measures

### Regular Checks

**Daily:**
- Monitor error logs
- Check certificate expiration (< 90 days)
- Verify PAC connectivity

**Weekly:**
- Review performance metrics
- Update SAT catalogs if changed
- Test disaster recovery

**Monthly:**
- Full system health check
- Review and update documentation
- Test backup and restore

### Monitoring Setup

```abap
Create monitoring job:

Program: ZCFDI_MONITOR
Variant: DAILY_CHECK

Checks:
- Certificate expiration
- PAC connectivity
- Error rate trends
- Performance metrics

Alert if:
- Certificate < 30 days
- Error rate > 5%
- PAC response time > 10s
- Failed CFDIs > 10/day
```

## Additional Resources

- [CFDI Overview](../01-cfdi-overview/README.md)
- [Architecture](../02-architecture/README.md)
- [Deployment Guide](../03-deployment/README.md)
- [FAQ](../05-faq/README.md)

---

**Document Version:** 1.0
**Last Updated:** December 2024
**Next Review:** March 2025
