# Frequently Asked Questions (FAQ)

Common questions and answers about CFDI 4.0 implementation in SAP.

## Table of Contents

- [General Questions](#general-questions)
- [Technical Questions](#technical-questions)
- [Business Process Questions](#business-process-questions)
- [Legal & Compliance Questions](#legal--compliance-questions)
- [Performance & Scaling Questions](#performance--scaling-questions)
- [Cost & ROI Questions](#cost--roi-questions)
- [Best Practices](#best-practices)

## General Questions

### What is CFDI?

**CFDI (Comprobante Fiscal Digital por Internet)** is Mexico's mandatory electronic invoicing standard managed by SAT (Tax Administration Service). All businesses in Mexico must issue CFDIs for their transactions.

### What version should I use?

**CFDI 4.0** has been mandatory since January 1, 2022. CFDI 3.3 is no longer accepted. This implementation supports CFDI 4.0 only.

### Do I need a PAC provider?

**Yes.** A PAC (Proveedor Autorizado de Certificación) is required to stamp your CFDIs. PACs are SAT-certified intermediaries that validate and add the fiscal stamp to your invoices.

Popular PAC providers:
- Montova
- Edicom
- Finkok
- SW Sapien

### Can I use this with S/4HANA?

**Yes.** This implementation works with:
- SAP ECC 6.0 or higher
- SAP S/4HANA (all versions)
- Minimum SAP Basis 7.40

### Is this compatible with SAP Business One?

**No.** This implementation is designed for SAP ECC and S/4HANA. SAP Business One requires a different approach, often using add-ons like:
- Artware CFDI for SAP B1
- Microsiga CFDI add-on

### How much does it cost?

**Implementation costs:**
- ABAP development: This code is free (MIT License)
- PAC costs: ~$200-500 USD/month (unlimited stamps)
- Certificates from SAT: Free
- Project implementation: Varies by consultant

### How long does implementation take?

**Typical timeline:**
- Basic implementation: 4-6 weeks
- Complex scenarios: 8-12 weeks
- Testing and go-live: 2-4 weeks

**Total: 2-4 months** depending on complexity and resources.

## Technical Questions

### What ABAP version is required?

**Minimum:** SAP Basis 7.40

**Recommended:** SAP Basis 7.50 or higher for better performance and features.

### Do I need SAP PI/PO for integration?

**No.** Direct HTTP/HTTPS calls to PAC are sufficient. However, SAP PI/PO can be used if:
- Your company standard requires it
- You need complex message routing
- You want centralized integration monitoring

### Can I use REST or must I use SOAP?

**Both are supported.** Most modern PACs offer both REST and SOAP APIs.

**Recommendation:** Use REST for:
- Simpler implementation
- Better performance
- Easier debugging

Use SOAP if:
- PAC only offers SOAP
- Your company standards require it

### How do I handle certificate expiration?

**Process:**

1. **3 months before expiration:**
   - Request new certificate from SAT
   - Import to SAP with new alias
   - Test in parallel

2. **1 month before:**
   - Update configuration
   - Perform cutover testing
   - Notify users

3. **After expiration:**
   - Remove old certificate
   - Monitor for issues

**Certificates are valid for 4 years.**

### What happens if PAC is down?

**Built-in retry logic:**
1. First attempt fails → Wait 5 seconds, retry
2. Second attempt fails → Wait 10 seconds, retry
3. Third attempt fails → Queue for later processing

**Manual process:**
- Queue invoices
- Wait for PAC recovery
- Batch process queued invoices
- Typically resolved within hours

### Can I generate CFDI offline?

**No.** CFDI requires:
- Connection to PAC for stamping
- Connection to SAT for validation
- Internet connectivity mandatory

**Workaround for poor connectivity:**
- Generate XML offline
- Queue for submission
- Batch submit when connected

### How do I migrate from 3.3 to 4.0?

**CFDI 3.3 is no longer valid.** You must use 4.0.

**Migration steps:**
1. Implement CFDI 4.0 (this solution)
2. Update all master data with new fields
3. Test thoroughly
4. Cut over to 4.0
5. Cannot revert to 3.3

**Note:** No data migration needed - old CFDIs remain as 3.3, new ones are 4.0.

## Business Process Questions

### When should the CFDI be generated?

**At invoicing time:**
```
Sales Order (VA01) → Delivery (VL01N) → Billing (VF01) → CFDI Generation
                                                              ↓
                                                      Immediately after billing
```

**Timing requirements:**
- Generate within 72 hours of transaction (SAT rule)
- Best practice: Immediate generation
- Can be automatic or manual

### Can I generate CFDI before delivery?

**Generally no.** CFDI should represent actual transaction:
- Invoice created = CFDI generated
- Goods not yet delivered = No CFDI yet

**Exception:** Advance invoices (allowed if properly configured)

### What if customer provides wrong RFC?

**Before stamping:**
- Correct in billing document
- Regenerate CFDI
- Submit to PAC

**After stamping:**
- Cannot modify stamped CFDI
- Must cancel original CFDI
- Create new CFDI with correct RFC
- Issue substitution CFDI

### How do I handle credit notes?

**Process:**
1. Create credit memo in SAP (VF01)
2. Generate CFDI for credit note
3. Reference original invoice UUID
4. Send to customer

**CFDI Type:** Egreso (E)

**Key field:** UUID of original invoice in "Related Documents"

### Can I cancel a CFDI?

**Yes, but with restrictions:**

**Within 24 hours:**
- Can cancel without customer approval
- Must provide cancellation reason
- Recommended for errors only

**After 24 hours:**
- Requires customer acceptance
- Customer has 72 hours to respond
- If no response, SAT auto-approves after 72 hours

**Cannot cancel:**
- CFDIs used in tax declarations
- CFDIs older than current fiscal year

### What is a payment complement?

**When invoice date ≠ payment date:**

```
Day 1: Issue invoice → Generate CFDI (Type I)
       Payment method: PPD (Pago en parcialidades)

Day 15: Receive payment → Generate Payment Complement
        References original CFDI UUID
        Shows payment details
```

**Required when:**
- Credit sales (payment terms)
- Payment date different from invoice date
- Partial payments

### How do I handle foreign customers?

**For exports:**

**Customer RFC:** Use "XEXX010101000" (generic foreign RFC)

**Additional fields:**
- Export flag
- Destination country code
- Customs information (if applicable)

**Currency:** Can be USD, EUR, or other (SAT catalog)

**Exchange rate:** Must specify if not MXN

### Can I batch process CFDIs?

**Yes.** Recommended for:
- End of day processing
- Bulk invoice runs
- Migration scenarios

**Implementation:**
```abap
Program: ZCFDI_BATCH
- Select all unbilled documents
- Process in chunks of 100
- Log results
- Email summary
```

## Legal & Compliance Questions

### Is CFDI mandatory?

**Yes.** For all businesses in Mexico since 2014. CFDI 4.0 specifically since January 1, 2022.

**Penalties for non-compliance:**
- Fines: 17,020 to 85,100 MXN
- Possible business suspension
- Criminal charges in severe cases

### How long must I retain CFDIs?

**5 years minimum.**

**Requirements:**
- Keep both XML and PDF
- Must be available for SAT audits
- Storage can be electronic
- Backup recommended

### Can I use one certificate for multiple companies?

**No.** Each legal entity (RFC) needs its own certificate.

**If you have multiple companies:**
- Each company needs own CSD certificate
- Each certificate tied to specific RFC
- Cannot share certificates

### What if SAT changes requirements?

**SAT occasionally updates:**
- CFDI specifications (e.g., 3.3 → 4.0)
- Catalog codes
- Validation rules

**When changes occur:**
- SAT provides transition period (usually 6-12 months)
- Update code and configuration
- Retest thoroughly
- Deploy before deadline

**This implementation:**
- Designed to be flexible
- Can be updated for new requirements
- Community maintains updates

### Do I need to keep paper invoices?

**No.** CFDI completely replaces paper invoices.

**However:**
- Some companies keep copies for convenience
- Not legally required
- XML is the legal document

### Can customers claim deductions with CFDI?

**Yes.** Customers need:
- XML file (mandatory)
- PDF file (optional but common)
- Ability to verify UUID on SAT portal

**Your responsibility:**
- Send XML to customer (mandatory)
- Send PDF to customer (courtesy)
- Ensure UUID is valid

## Performance & Scaling Questions

### How many CFDIs per hour can I process?

**Typical performance:**
- Single CFDI: 2-5 seconds
- Batch processing: ~720-1,800 per hour
- Parallel processing: 2,000-5,000 per hour

**Factors affecting performance:**
- SAP system resources
- PAC response time
- Network latency
- Complexity of CFDI

### What if I process 10,000 invoices per day?

**Recommended architecture:**

1. **Background Processing**
   ```
   - Queue invoices for processing
   - Multiple background jobs in parallel
   - Process during off-peak hours
   ```

2. **Parallel Processing**
   ```
   - Use multiple work processes
   - Distribute load across application servers
   - Monitor resource usage
   ```

3. **PAC Considerations**
   - Verify PAC can handle volume
   - May need enterprise tier service
   - Consider load balancing

### Will CFDI generation slow down invoicing?

**Depends on implementation:**

**Synchronous (not recommended for high volume):**
```
Create Invoice → Generate CFDI (wait) → Continue
                     ↑
                  User waits
```

**Asynchronous (recommended):**
```
Create Invoice → Queue CFDI → Continue immediately
                     ↓
              Background processing
```

**Best practice:** Asynchronous for production use.

### How do I handle peak periods?

**Strategies:**

1. **Pre-generate during slow periods**
2. **Increase background jobs during peak**
3. **Use batch processing overnight**
4. **Scale SAP application servers**
5. **Upgrade PAC service tier**

## Cost & ROI Questions

### What are the ongoing costs?

**Monthly costs:**
- PAC provider: $200-500 USD/month (unlimited)
- Certificate renewal: Free (every 4 years)
- Maintenance/support: Variable
- SAP system: Existing infrastructure

**Annual: ~$2,400-6,000 USD**

### Is there a per-CFDI cost?

**Depends on PAC pricing model:**

**Unlimited plans (most common):**
- Fixed monthly fee
- Unlimited CFDIs
- Recommended for medium/large businesses

**Pay-per-stamp:**
- Per CFDI cost (~$0.50-1.00 MXN)
- Good for low volume (<1,000/month)

### What if I switch PAC providers?

**Easy to switch:**
1. Sign contract with new PAC
2. Get credentials
3. Update configuration in SAP
4. Test in development
5. Cut over to production

**No data migration needed** - CFDIs are stored in SAP, not at PAC.

### What's the ROI?

**Cost savings:**
- Eliminate paper invoices
- Reduce postal costs
- Faster payment cycles
- Reduced errors
- Automated compliance

**Time savings:**
- Automatic generation
- No manual data entry
- Instant delivery to customers

**Typical ROI: 6-18 months**

## Best Practices

### Master Data Management

**Keep clean master data:**

✅ **Do:**
- Regular master data audits
- Validate RFCs against SAT
- Keep SAT codes updated
- Document data standards

❌ **Don't:**
- Skip master data validation
- Use generic/placeholder data
- Ignore SAT catalog updates

### Error Handling

✅ **Do:**
- Implement comprehensive logging
- Set up monitoring and alerts
- Have retry logic
- Plan for PAC downtime

❌ **Don't:**
- Ignore errors silently
- Assume PAC always available
- Skip error notification

### Testing

✅ **Do:**
- Test in development first
- Use PAC test environment
- Test all scenarios
- Involve business users
- Test error scenarios

❌ **Don't:**
- Test in production
- Skip test environment
- Only test happy path
- Deploy without UAT

### Certificate Management

✅ **Do:**
- Monitor expiration dates
- Renew 3 months early
- Test new certificates
- Keep backups secure
- Document renewal process

❌ **Don't:**
- Wait until expired
- Skip testing new certificates
- Store passwords in code
- Share certificates between companies

### Performance

✅ **Do:**
- Use asynchronous processing
- Batch process when possible
- Monitor performance metrics
- Optimize database queries
- Cache configuration data

❌ **Don't:**
- Process synchronously in UI
- Ignore performance issues
- Over-engineer initially

### Documentation

✅ **Do:**
- Document customizations
- Keep runbooks updated
- Document error resolutions
- Maintain change log

❌ **Don't:**
- Skip documentation
- Assume knowledge transfer
- Leave code uncommented

### Security

✅ **Do:**
- Use SSF for certificates
- Encrypt PAC credentials
- Follow SAP authorization concepts
- Regular security audits

❌ **Don't:**
- Hardcode credentials
- Store certificates in source code
- Give broad authorizations
- Commit sensitive data to Git

## Still Have Questions?

### Check These Resources:

1. **Documentation**
   - [CFDI Overview](../01-cfdi-overview/README.md)
   - [Architecture](../02-architecture/README.md)
   - [Deployment Guide](../03-deployment/README.md)
   - [Troubleshooting](../04-troubleshooting/README.md)

2. **External Resources**
   - [SAT Portal](http://www.sat.gob.mx/)
   - [CFDI 4.0 Specifications](http://omawww.sat.gob.mx/tramitesyservicios/Paginas/documentos/Anexo_20_Guia_de_llenado_CFDI.pdf)
   - [SAT Catalogs](http://www.sat.gob.mx/consultas/92764/comprobante-fiscal-digital-por-internet)

3. **Community**
   - [GitHub Issues](../../issues)
   - [GitHub Discussions](../../discussions)
   - SAP Community - Mexico Localization

4. **Professional Support**
   - Contact your SAP consultant
   - Engage your PAC provider support
   - SAT helpline: 55-627-22-728

---

**Document Version:** 1.0
**Last Updated:** December 2024
**Next Review:** March 2025

**Didn't find your answer?** [Open a discussion](../../discussions) and we'll help!
