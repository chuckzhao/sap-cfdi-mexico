# CFDI ABAP Examples

This directory contains example ABAP programs demonstrating various CFDI generation scenarios.

## Example Programs

### Example 1: Simple CFDI Generation
**Program:** `ZCFDI_EXAMPLE_01_SIMPLE.abap`

The simplest use case - generate CFDI from a single billing document.

**Use when:**
- Learning the basic API
- Testing configuration
- Manual CFDI generation

**Features:**
- Basic CFDI generation
- Simple error handling
- XML display

**Usage:**
```abap
" Run transaction
Transaction: SE38
Program: ZCFDI_EXAMPLE_01_SIMPLE

" Enter parameters
Billing Document: 0090000123

" Execute
```

### Example 2: Complete Flow with PAC
**Program:** `ZCFDI_EXAMPLE_02_WITH_PAC.abap`

Complete end-to-end CFDI flow including PAC integration.

**Use when:**
- Implementing production solution
- Testing PAC integration
- Understanding complete workflow

**Features:**
- CFDI generation
- XML validation
- PAC integration
- UUID storage
- Status updates

**Usage:**
```abap
" Run transaction
Transaction: SE38
Program: ZCFDI_EXAMPLE_02_WITH_PAC

" Enter parameters
Billing Document: 0090000123
PAC Provider: MONTOVA (or EDICOM)

" Execute
```

**Flow:**
```
1. Generate CFDI XML
2. Validate XML structure
3. Send to PAC for stamping
4. Receive UUID and stamped XML
5. Store in SAP
6. Display result
```

### Example 3: Batch Processing
**Program:** `ZCFDI_EXAMPLE_03_BATCH.abap`

Process multiple billing documents in batch.

**Use when:**
- End-of-day processing
- Processing backlog
- High-volume scenarios

**Features:**
- Multiple document selection
- Progress tracking
- Error handling per document
- Summary report
- Test mode

**Usage:**
```abap
" Run transaction
Transaction: SE38
Program: ZCFDI_EXAMPLE_03_BATCH

" Enter parameters
Billing Documents: 0090000001 to 0090001000
Billing Date: 01.01.2024 to 31.01.2024
PAC Provider: MONTOVA
Test Mode: [X] (check for testing)
Parallel Jobs: 1

" Execute
```

**Output:**
```
Processing documents...
1/100 (1%) - 0090000001
2/100 (2%) - 0090000002
...

BATCH PROCESSING SUMMARY
Total Documents: 100
Successful: 98
Errors: 2
Success Rate: 98%
```

## Running the Examples

### Prerequisites

1. **ABAP Development Access**
   ```
   Authorization: S_DEVELOP
   Transaction: SE38 or SE80
   ```

2. **CFDI Classes Installed**
   ```
   - ZCL_CFDI_GENERATOR
   - Exception classes (ZCX_*)
   ```

3. **Test Data**
   ```
   - Billing documents in SAP
   - Customer master data with RFC
   - Materials with SAT codes
   ```

### Installation

1. **Create Programs in SAP**
   ```abap
   Transaction: SE38 > Create
   Program Name: ZCFDI_EXAMPLE_01_SIMPLE
   Type: Executable Program

   Copy code from example file
   Activate
   ```

2. **Repeat for Each Example**
   - ZCFDI_EXAMPLE_01_SIMPLE
   - ZCFDI_EXAMPLE_02_WITH_PAC
   - ZCFDI_EXAMPLE_03_BATCH

### Testing

1. **Start with Example 1**
   - Use known billing document
   - Verify XML generation
   - Check for errors

2. **Move to Example 2**
   - Configure PAC credentials
   - Test in PAC test environment
   - Verify UUID received

3. **Try Example 3**
   - Start with small batch (5-10 documents)
   - Use test mode
   - Review summary report

## Customization

### Adding Custom Logic

All examples can be extended:

```abap
" Add custom validation
PERFORM custom_validation USING ls_billing.

" Add custom PAC integration
lo_pac = zcl_custom_pac_provider=>create( ).

" Add custom storage
PERFORM custom_storage USING gv_uuid gv_xml.
```

### Creating New Examples

Use existing examples as templates:

1. Copy existing example
2. Modify for your scenario
3. Add specific business logic
4. Document usage

**Example scenarios:**
- Credit note processing
- Payment complement generation
- Foreign customer handling
- Multi-company processing
- Custom PDF generation

## Common Modifications

### Change PAC Provider

```abap
" In selection screen
PARAMETERS: p_pac TYPE char10 DEFAULT 'EDICOM'.  " Changed from MONTOVA

" In code
CASE p_pac.
  WHEN 'EDICOM'.
    CREATE OBJECT lo_pac TYPE zcl_pac_edicom.
ENDCASE.
```

### Add Email Notification

```abap
" After successful generation
PERFORM send_email USING ls_result-vbeln
                        ls_result-uuid
                        lv_customer_email.
```

### Add to Background Job

```abap
" Schedule as background job
Transaction: SM36

Job Name: ZCFDI_DAILY_BATCH
Step: ZCFDI_EXAMPLE_03_BATCH
Variant: Create variant with default parameters
Schedule: Daily at 23:00
```

## Troubleshooting

### Example Program Doesn't Run

**Check:**
1. All classes activated (SE80)
2. Authorization assigned
3. Test data exists

### CFDI Generation Fails

**Check:**
1. Billing document exists
2. Customer has RFC
3. Materials have SAT codes
4. Certificate configured

### PAC Connection Fails

**Check:**
1. Network connectivity (ping PAC server)
2. Credentials correct
3. PAC service active
4. Firewall rules

## Best Practices

### Development
- Always test in development system first
- Use test mode before real PAC calls
- Start with single documents
- Add logging for troubleshooting

### Production
- Schedule batch jobs during off-hours
- Monitor error logs daily
- Keep backup of generated XMLs
- Implement retry logic for failures

### Performance
- Process in batches of 100-500
- Use parallel processing for large volumes
- Commit work periodically
- Monitor resource usage

## Additional Resources

- [Main Documentation](../../../docs/)
- [Troubleshooting Guide](../../../docs/04-troubleshooting/README.md)
- [FAQ](../../../docs/05-faq/README.md)
- [ABAP Class Documentation](../classes/ZCL_CFDI_GENERATOR.abap)

## Support

For questions about examples:
1. Check code comments
2. Review main documentation
3. Open GitHub issue
4. Contact development team

---

**Happy Coding!** 🚀
