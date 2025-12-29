# System Architecture & Complete Scenarios

## Architecture Overview

```
SAP → CFDI Engine → PAC → SAT
```

## Complete Scenario Documentation

This document contains all 40 scenarios with detailed flows.

---

## Scenario 1: Invoice Generation Flow (Normal Process)

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

**Key Steps:**
1. Create billing document in SAP
2. Generate CFDI XML with all required fields
3. Apply company digital signature
4. Send to PAC for validation and stamping
5. Receive UUID and stamped XML
6. Store in SAP and send to customer

---

## Scenario 2: Timbrado (Stamping) Flow

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
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
```

**Timing:**
- Normal: 2-5 seconds
- Peak: up to 10 seconds
- Timeout: 30 seconds

---

## Scenario 3: Invoice Update (Cancellation + Substitution)

**Important:** Cannot update stamped CFDI. Must cancel and reissue.

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  SCENARIO: Invoice has error   │                                │
       │  (wrong amount, customer, etc) │                                │
       │                                │                                │
       │  STEP 1: Cancel original CFDI  │                                │
       │───────────────────────────────▶│───────────────────────────────▶│
       │  Reason: "01" (with relation)  │                                │
       │                                │                                │
       │  STEP 2: Create new billing    │                                │
       │          with correct data     │                                │
       │                                │                                │
       │  STEP 3: Generate new CFDI     │                                │
       │───────────────────────────────▶│                                │
       │  <CfdiRelacionados>            │                                │
       │    TipoRelacion="04"           │                                │
       │    (Substitution)              │                                │
       │    UUID="[cancelled-uuid]"     │                                │
       │  </CfdiRelacionados>           │                                │
       │                                │                                │
       │                                │  4. Validate new CFDI          │
       │                                │  5. Verify cancellation        │
       │                                │───────────────────────────────▶│
       │                                │                                │
       │  8. Receive new stamped CFDI   │                                │
       │◀───────────────────────────────│                                │
       │     - New UUID                 │                                │
       │     - Linked to cancelled UUID │                                │
```

**Common Update Scenarios:**
- Wrong amount → Cancel + reissue
- Wrong customer → Cancel + reissue
- Wrong tax rate → Cancel + reissue
- Missing items → Cancel + reissue

---

## Scenario 4a: Cancellation Within 24 Hours

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  1. User initiates cancel      │                                │
       │     (VF11 or custom)           │                                │
       │                                │                                │
       │  2. Send cancellation request  │                                │
       │───────────────────────────────▶│                                │
       │  - UUID to cancel              │                                │
       │  - Reason code                 │                                │
       │  - Substitution UUID (if any)  │                                │
       │                                │                                │
       │                                │  3. Forward to SAT             │
       │                                │───────────────────────────────▶│
       │                                │                                │
       │                                │                                │  4. SAT validates:
       │                                │                                │     - < 24 hours ✅
       │                                │                                │     - Not cancelled
       │                                │                                │
       │                                │  5. Approved immediately       │
       │                                │◀───────────────────────────────│
       │                                │                                │
       │  6. Confirmation                │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  7. Update SAP:                │                                │
       │     - Mark cancelled           │                                │
       │     - Reverse accounting       │                                │
       │                                │                                │
       │  8. Notify customer            │                                │
```

**No customer approval needed within 24 hours!**

---

## Scenario 4b: Cancellation After 24 Hours

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│     SAP      │     │     PAC      │     │     SAT      │     │   Customer   │
└──────┬───────┘     └──────┬───────┘     └──────┬───────┘     └──────┬───────┘
       │                    │                    │                    │
       │  1. Request cancel │                    │                    │
       │────────────────────▶                    │                    │
       │                    │                    │                    │
       │                    │  2. Forward        │                    │
       │                    │────────────────────▶                    │
       │                    │                    │                    │
       │                    │                    │  3. Notify customer│
       │                    │                    │────────────────────▶
       │                    │                    │                    │
       │  Status: Pending   │                    │                    │
       │◀────────────────────                    │                    │
       │  (wait 72 hours)   │                    │                    │
       │                    │                    │                    │
       │                    │                    │  Customer options: │
       │                    │                    │  - Accept          │
       │                    │                    │  - Reject          │
       │                    │                    │  - No response     │
       │                    │                    │    (auto after 72h)│
       │                    │                    │                    │
       │                    │                    │  5. Response        │
       │                    │                    │◀────────────────────
       │                    │                    │                    │
       │  7. Final result   │                    │                    │
       │◀────────────────────◀───────────────────                    │
```

**Requires customer approval or 72-hour timeout**

**Cancellation Reason Codes:**
- 01: With relation (will reissue)
- 02: Without relation (error, won't reissue)
- 03: Operation did not occur
- 04: Nominative to global

---

## Scenario 5: Invoice Reversal (Credit Note)

**Credit notes are NOT cancellations. New CFDI type "Egreso".**

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  SCENARIO: Customer returns    │                                │
       │  goods or needs discount       │                                │
       │                                │                                │
       │  1. Create credit memo (VF01)  │                                │
       │     Type G2 - Credit Memo      │                                │
       │                                │                                │
       │  2. Generate CREDIT CFDI       │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    TipoDeComprobante="E"       │                                │
       │    (Egreso = Credit)           │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <CfdiRelacionados>            │                                │
       │    TipoRelacion="01"           │                                │
       │    (Credit note)               │                                │
       │    UUID="[original-invoice]"   │                                │
       │  </CfdiRelacionados>           │                                │
       │                                │                                │
       │  Items with NEGATIVE amounts   │                                │
       │                                │                                │
       │                                │  3. Validate                   │
       │                                │     - Type = Egreso            │
       │                                │     - Related to original      │
       │                                │───────────────────────────────▶│
       │                                │                                │
       │  7. Receive credit note UUID   │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  8. Update SAP:                │                                │
       │     - Store credit UUID        │                                │
       │     - Post accounting reversal │                                │
       │     - Update customer balance  │                                │
       │                                │                                │
       │  NOTE: Original invoice        │                                │
       │  remains VALID (not cancelled) │                                │
```

**Credit Note vs Cancellation:**

| Aspect | Credit Note | Cancellation |
|--------|-------------|--------------|
| Original | Remains valid | Becomes invalid |
| New CFDI | Yes (Type E) | No |
| Use Case | Returns/discounts | Invoice errors |

---

## Scenario 6: Data Inconsistency - SAP vs PAC vs SAT

### 6a: SAP Thinks Stamped, PAC Failed

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  1. Send XML                   │                                │
       │───────────────────────────────▶│                                │
       │                                │                                │
       │                                │  2. Validation FAILS           │
       │                                │     (but SAP doesn't receive   │
       │                                │      error - network timeout)  │
       │                                │                                │
       │  3. Timeout - SAP assumes      │                                │
       │     success ❌                 │                                │
       │                                │                                │
       │  SAP: Stamped (WRONG)          │  PAC: Failed                   │
       │  UUID: Fake                    │  UUID: None                    │
       │                                │                                │
       │  DETECTION:                    │                                │
       │  - Customer can't verify       │                                │
       │  - Reconciliation report       │                                │
       │                                │                                │
       │  RESOLUTION:                   │                                │
       │  4. Query PAC for status       │                                │
       │───────────────────────────────▶│                                │
       │                                │                                │
       │  5. Confirm: Not stamped       │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  6. Reset SAP status           │                                │
       │  7. Retry stamping ✅          │                                │
```

### 6b: PAC Stamped, SAP Missed Response

```
       │  1. Send XML                   │                                │
       │───────────────────────────────▶│                                │
       │                                │  2-3. Stamp created ✅         │
       │                                │───────────────────────────────▶│
       │                                │                                │
       │  4. Response lost ❌           │  SAT: Stamped ✅               │
       │◀─ ✗ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ │  UUID: ABC123                  │
       │                                │                                │
       │  SAP: Failed (WRONG)           │  PAC: Stamped ✅               │
       │                                │                                │
       │  RESOLUTION:                   │                                │
       │  5. Query PAC by invoice#      │                                │
       │───────────────────────────────▶│                                │
       │                                │                                │
       │  6. Receive existing UUID      │                                │
       │◀───────────────────────────────│                                │
       │  7. Store in SAP ✅            │                                │
```

---

## Scenario 7: Payment Complement Flow

**Required when payment date ≠ invoice date**

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Day 1: Issue Invoice          │                                │
       │  - Amount: $10,000             │                                │
       │  - MetodoPago: PPD             │                                │
       │  - FormaPago: 99 (TBD)         │                                │
       │  [Normal stamping...]          │                                │
       │                                │                                │
       │  Day 15: Payment Received      │                                │
       │  - Customer pays $5,000        │                                │
       │  - Posted in SAP FI            │                                │
       │                                │                                │
       │  1. Detect payment clearing    │                                │
       │                                │                                │
       │  2. Generate Payment Complement│                                │
       │───────────────────────────────▶│                                │
       │  Type: "P" (Pago)              │                                │
       │  <Pago>                        │                                │
       │    FechaPago="2024-12-15"      │                                │
       │    FormaDePagoP="03"           │                                │
       │    Monto="5000.00"             │                                │
       │    <DoctoRelacionado>          │                                │
       │      IdDocumento="[UUID]"      │                                │
       │      NumParcialidad="1"        │                                │
       │      ImpSaldoAnt="10000"       │                                │
       │      ImpPagado="5000"          │                                │
       │      ImpSaldoInsoluto="5000"   │                                │
       │    </DoctoRelacionado>         │                                │
       │  </Pago>                       │                                │
       │                                │                                │
       │  3. Stamp payment ✅           │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  Day 30: Second payment $5,000 │                                │
       │  [Repeat with NumParcialidad=2]│                                │
```

**Key Fields:**
- NumParcialidad: Payment number (1, 2, 3...)
- ImpSaldoAnt: Balance before
- ImpPagado: Amount paid
- ImpSaldoInsoluto: Remaining balance

**Rules:**
- Required when MetodoPago = "PPD"
- Must issue within 10 days of payment
- Not required when MetodoPago = "PUE"

---

## Scenario 8: Global Invoice (B2C Consolidation)

**For retail: Many small cash sales consolidated daily/monthly**

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Throughout day:               │                                │
       │  - 100+ small B2C sales        │                                │
       │  - No individual CFDIs         │                                │
       │                                │                                │
       │  End of Day: 11:59 PM          │                                │
       │                                │                                │
       │  1. Consolidate all B2C        │                                │
       │                                │                                │
       │  2. Create Global Invoice      │                                │
       │───────────────────────────────▶│                                │
       │  <Receptor>                    │                                │
       │    Rfc="XAXX010101000"         │                                │
       │    (Public in general)         │                                │
       │    Nombre="PUBLICO EN GENERAL" │                                │
       │    UsoCFDI="S01"               │                                │
       │  </Receptor>                   │                                │
       │                                │                                │
       │  <Conceptos>                   │                                │
       │    - Aggregated by product     │                                │
       │    - Total quantities          │                                │
       │    Example:                    │                                │
       │    Coffee - 150 units          │                                │
       │    Pastry - 200 units          │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  3. Stamp global invoice ✅    │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  IF customer requests          │                                │
       │  individual CFDI later:        │                                │
       │                                │                                │
       │  4. Create individual CFDI     │                                │
       │     with relation to global    │                                │
       │  TipoRelacion="04"             │                                │
       │                                │                                │
       │  5. Cancel portion of global   │                                │
```

**Global Invoice Rules:**
- RFC: "XAXX010101000" (mandatory)
- UsoCFDI: "S01" (mandatory)
- Max per line: $5,000 MXN
- Cash sales only
- Customer can't deduct taxes

---

✅ **Part 1 Complete: Scenarios 1-8 created**

## Scenario 9: Certificate Expiration

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  WARNING PHASE (30 days before)│                                │
       │                                │                                │
       │  1. Automated check detects    │                                │
       │     certificate expiring       │                                │
       │     Expires: 2024-12-31        │                                │
       │     Today: 2024-12-01          │                                │
       │     Warning: 30 days left      │                                │
       │                                │                                │
       │  2. Alert team                 │                                │
       │     - Email notification       │                                │
       │     - Dashboard warning        │                                │
       │                                │                                │
       │  3. Request new cert from SAT  │                                │
       │     (Takes 3-5 days)           │                                │
       │                                │                                │
       │  4. Download new certificate   │                                │
       │     Valid: 2025-01-01 to 2026  │                                │
       │                                │                                │
       │  5. Import to SAP STRUST       │                                │
       │     Keep old one active        │                                │
       │                                │                                │
       │  TRANSITION (Last week)        │                                │
       │                                │                                │
       │  6. Update configuration       │                                │
       │     Set new cert as primary    │                                │
       │                                │                                │
       │  7. Test with new cert         │                                │
       │───────────────────────────────▶│───────────────────────────────▶│
       │                                │                                │
       │  10. Test successful ✅        │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  CUTOVER (Jan 1)               │                                │
       │  11. Switch to new cert        │                                │
       │  12. Monitor first 24 hours    │                                │
       │                                │                                │
       │  EMERGENCY: If expired         │                                │
       │  - Queue all invoices          │                                │
       │  - Rush new cert (24-48h)      │                                │
       │  - Process queued invoices     │                                │
```

**Certificate Management Timeline:**
- 60 days: Alert team
- 45 days: Request new cert
- 30 days: Import to SAP
- 14 days: Test new cert
- 7 days: Update config
- Day 0: Activate new cert
- Day 1+: Monitor closely

---

## Scenario 10: PAC Switching (Montova to Edicom)

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│     SAP      │     │   Montova    │     │   Edicom     │     │     SAT      │
└──────┬───────┘     └──────┬───────┘     └──────┬───────┘     └──────┬───────┘
       │                    │                    │                    │
       │  CURRENT: Montova  │                    │                    │
       │  DECISION: Switch to Edicom             │                    │
       │                    │                    │                    │
       │  PREPARATION       │                    │                    │
       │                    │                    │                    │
       │  1. Contract Edicom│                    │                    │
       │     - Get credentials                   │                    │
       │     - Get test env │                    │                    │
       │                    │                    │                    │
       │  2. Parallel setup │                    │                    │
       │     Install both   │                    │                    │
       │                    │                    │                    │
       │  3. Test Edicom    │                    │                    │
       │───────────────────────────────────────▶│                    │
       │                    │                    │                    │
       │  5. Verify test ✅ │                    │                    │
       │◀───────────────────────────────────────│                    │
       │                    │                    │                    │
       │  CUTOVER (Weekend) │                    │                    │
       │                    │                    │                    │
       │  6. Update config  │                    │                    │
       │     PAC=edicom     │                    │                    │
       │                    │                    │                    │
       │  7. Stop Montova   │                    │                    │
       │  ✗────────────────▶│                    │                    │
       │                    │                    │                    │
       │  8. First prod     │                    │                    │
       │───────────────────────────────────────▶│───────────────────▶│
       │                    │                    │                    │
       │  11. Success! ✅   │                    │                    │
       │◀───────────────────────────────────────│◀───────────────────│
       │                    │                    │                    │
       │  HISTORICAL DATA   │                    │                    │
       │  Keep Montova for old queries          │                    │
```

**Switching Checklist:**
- ✅ Contract new PAC
- ✅ Get test credentials
- ✅ Install integration code
- ✅ Test thoroughly
- ✅ Plan weekend cutover
- ✅ Update configuration
- ✅ Test first production stamp
- ✅ Keep old PAC for historical queries

---

## Scenario 11: Mass Stamping Failure Recovery

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  End of month: 1000 invoices   │                                │
       │                                │                                │
       │  Invoices 1-500: Success ✅    │                                │
       │───────────────────────────────▶│───────────────────────────────▶│
       │                                │                                │
       │  Invoice 501: Timeout ❌       │                                │
       │  Batch job fails               │                                │
       │  501-1000: Not processed       │                                │
       │                                │                                │
       │  DETECTION:                    │                                │
       │  1. Job status: Failed         │                                │
       │  2. Check count                │                                │
       │     Expected: 1000             │                                │
       │     Actual: 500                │                                │
       │     Missing: 500               │                                │
       │                                │                                │
       │  RECOVERY:                     │                                │
       │                                │                                │
       │  3. Query SAP unstamped        │                                │
       │     Result: 500 invoices       │                                │
       │                                │                                │
       │  4. Check if any stamped       │                                │
       │     (timeout after stamp)      │                                │
       │                                │                                │
       │  5. Query PAC for period       │                                │
       │───────────────────────────────▶│                                │
       │                                │                                │
       │  6. Receive PAC list           │                                │
       │◀───────────────────────────────│                                │
       │  600 UUIDs (100 more!)         │                                │
       │                                │                                │
       │  7. Reconcile:                 │                                │
       │     - 500 in SAP ✅            │                                │
       │     - 100 stamped not in SAP ❌│                                │
       │     - 400 not stamped ⏳       │                                │
       │                                │                                │
       │  8. Import missing 100 UUIDs   │                                │
       │                                │                                │
       │  9. Retry 400 in small batches │                                │
       │     (50 at a time)             │                                │
       │───────────────────────────────▶│───────────────────────────────▶│
       │  ✅ Success                    │                                │
       │                                │                                │
       │  10. Verify: All done ✅       │                                │
```

**Batch Size Recommendations:**
- Normal: 100-200 per batch
- Peak: 50 per batch
- After failure: 20-50 per batch
- Night: 500+ per batch

---

## Scenario 12: Retroactive Stamping (System Downtime)

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Monday 9 AM: System down ❌   │                                │
       │  - Server crash                │                                │
       │  - Network issue               │                                │
       │                                │                                │
       │  During downtime:              │                                │
       │  - Invoices created manually   │                                │
       │  - Paper documents             │                                │
       │                                │                                │
       │  Tuesday 2 PM: Restored ✅     │                                │
       │                                │                                │
       │  1. Identify invoices from     │                                │
       │     Monday 9AM-Tuesday 2PM     │                                │
       │     Total: 150 invoices        │                                │
       │                                │                                │
       │  2. Check SAT rules:           │                                │
       │     - Can stamp within 72 hours│                                │
       │     - Must use ORIGINAL date   │                                │
       │                                │                                │
       │  3. Generate CFDI with         │                                │
       │     ORIGINAL timestamp         │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    Fecha="2024-12-23T10:30:00" │                                │
       │    (Monday time, not Tuesday!) │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │                                │  5. Validate timestamp         │
       │                                │     Monday-Tuesday < 72h ✅    │
       │                                │───────────────────────────────▶│
       │                                │                                │
       │  8. Success! ✅                │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  Note: CFDI shows:             │                                │
       │  - Invoice date: Monday 10:30  │                                │
       │  - Stamp date: Tuesday 15:00   │                                │
       │  This is LEGAL ✅              │                                │
       │                                │                                │
       │  IF > 72 HOURS:                │                                │
       │  - Contact SAT                 │                                │
       │  - Request special permission  │                                │
       │  - Provide proof of downtime   │                                │
```

**Timing Rules:**
- < 72 hours: Stamp with original date ✅
- > 72 hours: Need SAT permission
- Discovered late: Cancel and reissue

---

## Scenario 13: Customer RFC Change

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Customer notifies:            │                                │
       │  RFC changed due to merger     │                                │
       │                                │                                │
       │  Old RFC: ABC010101ABC         │                                │
       │  New RFC: XYZ020202XYZ         │                                │
       │                                │                                │
       │  Existing: 50 open invoices    │                                │
       │  with old RFC, not yet paid    │                                │
       │                                │                                │
       │  1. Update customer master     │                                │
       │     (XD02)                     │                                │
       │     KNA1-STCD1: XYZ020202XYZ   │                                │
       │                                │                                │
       │  2. New invoice with new RFC   │                                │
       │───────────────────────────────▶│                                │
       │  <Receptor Rfc="XYZ020202XYZ"/>│                                │
       │                                │                                │
       │  5. Stamp successful ✅        │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  OLD INVOICES:                 │                                │
       │                                │                                │
       │  OPTION 1: Leave as-is ✅      │                                │
       │  - Old invoices remain valid   │                                │
       │  - Old RFC was correct when    │                                │
       │    issued                      │                                │
       │  - No action needed            │                                │
       │                                │                                │
       │  OPTION 2: Cancel & reissue    │                                │
       │  (if customer requests)        │                                │
       │                                │                                │
       │  For each old invoice:         │                                │
       │  6a. Cancel old CFDI           │                                │
       │      Reason: "01"              │                                │
       │                                │                                │
       │  6b. Create new with new RFC   │                                │
       │───────────────────────────────▶│                                │
       │  <Receptor Rfc="XYZ..." /> ✅  │                                │
       │  TipoRelacion="04"             │                                │
       │                                │                                │
       │  PAYMENT COMPLEMENTS:          │                                │
       │  Use NEW RFC in payment        │                                │
       │  complement (even if invoice   │                                │
       │  has old RFC) ✅               │                                │
```

**Decision Matrix:**

| Situation | Action | Reason |
|-----------|--------|--------|
| Unpaid invoices | Leave as-is | RFC was correct when issued |
| Customer requests | Cancel + reissue | Customer preference |
| Payment after change | Use new RFC | Current RFC valid |

---

## Scenario 14: Foreign Currency Invoice

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Sale to USA customer          │                                │
       │  Order: $10,000 USD            │                                │
       │  Exchange: 17.50 MXN/USD       │                                │
       │                                │                                │
       │  1. Create billing in USD      │                                │
       │                                │                                │
       │  2. Generate CFDI              │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    Moneda="USD"                │                                │
       │    TipoCambio="17.50"          │                                │
       │    SubTotal="10000.00"         │                                │
       │    Total="11600.00"            │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  CRITICAL: Exchange rate       │                                │
       │  must be SAT's official rate   │                                │
       │  (from DOF - Diario Oficial)   │                                │
       │                                │                                │
       │                                │  3. Validate exchange rate     │
       │                                │───────────────────────────────▶│
       │                                │     Query SAT rate for date    │
       │                                │                                │
       │                                │  4. Return: 17.50 ✅           │
       │                                │◀───────────────────────────────│
       │                                │                                │
       │                                │  5. Rates match ✅             │
       │                                │                                │
       │  IF RATE DOESN'T MATCH:        │                                │
       │  Invoice: 17.50                │                                │
       │  SAT: 17.52 ❌                 │                                │
       │                                │                                │
       │  7. Error returned             │                                │
       │◀───────────────────────────────│◀───────────────────────────────│
       │  "Must use 17.52, not 17.50"   │                                │
       │                                │                                │
       │  8. Regenerate with correct    │                                │
       │     rate 17.52 ✅              │                                │
```

**Foreign Currency Rules:**
- ✅ Must use SAT's official rate (from DOF)
- ✅ Rate published daily by Banco de México
- ❌ Cannot use bank rates
- ❌ Cannot use company rates
- ❌ Cannot approximate

**Supported Currencies:**
USD, EUR, GBP, JPY, CAD, and others in SAT catalog

---

## Scenario 15: SAT System Downtime

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  1. Attempt to stamp           │                                │
       │───────────────────────────────▶│                                │
       │                                │                                │
       │                                │  2. Forward to SAT             │
       │                                │───────────────────────────────▶│
       │                                │                                ✗
       │                                │  3. SAT down - no response     
       │                                │                                
       │                                │  4. Timeout (30 seconds)       
       │                                │                                
       │                                │  5. PAC retries (3x)           
       │                                │───────────────────────────────▶
       │                                │     All fail                   
       │                                │                                
       │  6. Error to SAP               │                                
       │◀───────────────────────────────│                                
       │  "SAT unavailable - retry"     │                                
       │                                │                                
       │  IMMEDIATE ACTIONS:            │                                
       │                                │                                
       │  7. Check SAT status           │                                
       │     Visit: sat.gob.mx/status   │                                
       │                                │                                
       │  8. Determine scope:           │                                
       │     - Complete SAT down?       │                                
       │     - Partial?                 │                                
       │     - Expected duration?       │                                
       │                                │                                
       │  QUEUING:                      │                                
       │                                │                                
       │  9. Store in queue             │                                
       │     Table: ZTCFDI_QUEUE        │                                
       │     Status: 'SAT_DOWN'         │                                
       │                                │                                
       │  10. Schedule retry job        │                                
       │      - Every 15 minutes        │                                
       │      - Max 96 attempts (24h)   │                                
       │                                │                                
       │  RETRY LOOP:                   │                                
       │                                │                                
       │  12. Retry #1 - Still down ✗   │                                
       │  [Wait 15 minutes]             │                                
       │                                │                                
       │  13. Retry #2                  │                                
       │───────────────────────────────▶│───────────────────────────────▶│
       │                                │     SAT back! ✅               
       │                                │                                
       │  16. Success!                  │                                
       │◀───────────────────────────────│                                
       │                                │                                
       │  17. Process all queued        │                                
```

**SAT Downtime Response:**
- 0-5 min: Detect and alert
- 5-30 min: Start queuing, notify team
- During: Retry every 15 min, update stakeholders
- Recovery: Process queue, verify all successful

---

## Scenario 16: Withholding Tax (Retención)

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Service provider subject to   │                                │
       │  withholding tax               │                                │
       │                                │                                │
       │  Invoice: $10,000              │                                │
       │  IVA 16%: $1,600               │                                │
       │  Subtotal: $11,600             │                                │
       │                                │                                │
       │  Withholdings:                 │                                │
       │  - ISR 10%: -$1,000            │                                │
       │  - IVA 10.67%: -$1,067         │                                │
       │  Net: $9,533                   │                                │
       │                                │                                │
       │  1. Create invoice CFDI        │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    SubTotal="10000"            │                                │
       │    Total="11600"               │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <Impuestos>                   │                                │
       │    <Traslados>                 │                                │
       │      IVA: 1600                 │                                │
       │    </Traslados>                │                                │
       │    <Retenciones>               │                                │
       │      ISR: 1000                 │                                │
       │      IVA: 1067                 │                                │
       │    </Retenciones>              │                                │
       │    TotalRetenidos="2067"       │                                │
       │  </Impuestos>                  │                                │
       │                                │                                │
       │  4. Stamp invoice ✅           │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  5. Customer must also issue   │                                │
       │     WITHHOLDING RECEIPT        │                                │
       │     (Separate CFDI)            │                                │
       │                                │                                │
       │  6. Customer sends receipt     │                                │
       │     showing withholdings       │                                │
       │                                │                                │
       │  7. Use for tax filing         │                                │
       │     Deduct withheld amounts    │                                │
```

**Withholding Rates:**

| Service | ISR | IVA | Total |
|---------|-----|-----|-------|
| Professional | 10% | 10.67% | ~20.67% |
| Freight | 4% | 4% | 8% |
| Rent | 10% | 10.67% | ~20.67% |
| Construction | 6% | 0% | 6% |

**Two-Document System:**
1. Invoice CFDI (Provider → Customer)
2. Withholding Receipt CFDI (Customer → Provider)

Both required for compliance!

---

✅ **Part 2 Complete: Scenarios 9-16 added**

## Scenario 17: Split Invoice (One Order, Multiple Invoices)

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Large order with multiple     │                                │
       │  deliveries                    │                                │
       │                                │                                │
       │  Order: 1000000                │                                │
       │  Total: $50,000                │                                │
       │  Items: 100 units              │                                │
       │                                │                                │
       │  Week 1: Delivery 1 (40 units) │                                │
       │                                │                                │
       │  1. Create billing #1          │                                │
       │     Amount: $20,000            │                                │
       │                                │                                │
       │  2. Generate CFDI 1            │                                │
       │───────────────────────────────▶│                                │
       │  <Conceptos>                   │                                │
       │    Descripcion="Product X      │                                │
       │      - Part 1/3"               │                                │
       │    Cantidad="40"               │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  3. Stamp CFDI 1 ✅            │                                │
       │◀───────────────────────────────│                                │
       │     UUID-1: ABC-111            │                                │
       │                                │                                │
       │  Week 2: Delivery 2 (35 units) │                                │
       │                                │                                │
       │  4. Create billing #2          │                                │
       │     Amount: $17,500            │                                │
       │                                │                                │
       │  5. Generate CFDI 2            │                                │
       │───────────────────────────────▶│                                │
       │  <Conceptos>                   │                                │
       │    Descripcion="Product X      │                                │
       │      - Part 2/3"               │                                │
       │    Cantidad="35"               │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  OPTIONAL: Link to previous    │                                │
       │  <CfdiRelacionados>            │                                │
       │    TipoRelacion="07"           │                                │
       │    UUID="ABC-111"              │                                │
       │  </CfdiRelacionados>           │                                │
       │                                │                                │
       │  6. Stamp CFDI 2 ✅            │                                │
       │◀───────────────────────────────│                                │
       │     UUID-2: DEF-222            │                                │
       │                                │                                │
       │  Week 3: Final (25 units)      │                                │
       │  [Same process...]             │                                │
       │                                │                                │
       │  VERIFICATION:                 │                                │
       │  Total: $20k + $17.5k + $12.5k │                                │
       │       = $50,000 ✅             │                                │
       │  Units: 40 + 35 + 25 = 100 ✅  │                                │
```

**Best Practices:**
- Clear description "Part X of Y"
- Link invoices with TipoRelacion="07"
- Track in document flow (VBFA)
- Communicate with customer

---

## Scenario 18: Advance Payment

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Customer pays 50% advance     │                                │
       │                                │                                │
       │  Order: $100,000               │                                │
       │  Advance: $50,000              │                                │
       │                                │                                │
       │  Day 1: Receive advance        │                                │
       │                                │                                │
       │  1. Post payment (F-29)        │                                │
       │     Dr. Bank       $50,000     │                                │
       │        Cr. Customer $50,000    │                                │
       │                                │                                │
       │  2. Generate ADVANCE CFDI      │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    SubTotal="50000.00"         │                                │
       │    Descuento="50000.00"        │                                │
       │    (100% discount!)            │                                │
       │    Total="0.00"                │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <Conceptos>                   │                                │
       │    Descripcion="Advance"       │                                │
       │    Importe="50000"             │                                │
       │    Descuento="50000"           │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  3. Stamp advance CFDI ✅      │                                │
       │◀───────────────────────────────│                                │
       │     UUID-ADVANCE               │                                │
       │                                │                                │
       │  Day 30: Deliver goods         │                                │
       │                                │                                │
       │  4. Create final invoice       │                                │
       │     Full amount: $100,000      │                                │
       │                                │                                │
       │  5. Generate FINAL CFDI        │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    SubTotal="100000"           │                                │
       │    Total="116000"              │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <CfdiRelacionados>            │                                │
       │    TipoRelacion="07"           │                                │
       │    (Application of advance)    │                                │
       │    UUID="UUID-ADVANCE"         │                                │
       │  </CfdiRelacionados>           │                                │
       │                                │                                │
       │  6. Stamp final CFDI ✅        │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  7. Customer pays balance      │                                │
       │     $50,000                    │                                │
       │                                │                                │
       │  Result:                       │                                │
       │  Advance CFDI: Total=0         │                                │
       │  Final CFDI: Total=$116,000    │                                │
       │  Net payment: $66,000          │                                │
```

**Key Points:**
- Advance CFDI has Total=0 (100% discount)
- Final invoice shows full amount
- Link final to advance UUID
- Payment complement only for balance

---

## Scenario 19: Free of Charge Items

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Sending free samples          │                                │
       │                                │                                │
       │  1. Create delivery VL01N      │                                │
       │     Value: $0                  │                                │
       │                                │                                │
       │  2. Generate FREE CFDI         │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    SubTotal="1000.00"          │                                │
       │    Descuento="1000.00"         │                                │
       │    Total="0.00"                │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <Conceptos>                   │                                │
       │    Descripcion="Sample -       │                                │
       │      No charge"                │                                │
       │    Cantidad="10"               │                                │
       │    ValorUnitario="100"         │                                │
       │    Importe="1000"              │                                │
       │    Descuento="1000"            │                                │
       │    ObjetoImp="01"              │                                │
       │    (Not subject to tax)        │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  3. Stamp free CFDI ✅         │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  WARRANTY REPLACEMENT:         │                                │
       │                                │                                │
       │  4. Customer returns defective │                                │
       │                                │                                │
       │  5. Issue credit note (Type E) │                                │
       │     for returned item          │                                │
       │                                │                                │
       │  6. Send replacement free      │                                │
       │                                │                                │
       │  7. Generate FREE CFDI         │                                │
       │───────────────────────────────▶│                                │
       │  <CfdiRelacionados>            │                                │
       │    TipoRelacion="01"           │                                │
       │    UUID="[credit-note]"        │                                │
       │  </CfdiRelacionados>           │                                │
       │                                │                                │
       │  Descripcion="Warranty         │                                │
       │    replacement - No charge"    │                                │
       │  Descuento="100%"              │                                │
```

**Free Items Rules:**

| Scenario | Type | Descuento | ObjetoImp | Link? |
|----------|------|-----------|-----------|-------|
| Samples | I | 100% | 01 | No |
| Warranty | I | 100% | 01 | Yes |
| Promotion | I | 100% | 01 | Optional |
| Donation | I | 100% | 01 | No |

---

## Scenario 20: Export Invoice

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Export to USA customer        │                                │
       │  No Mexican RFC                │                                │
       │                                │                                │
       │  1. Create export order        │                                │
       │     Currency: USD              │                                │
       │     Incoterm: FOB              │                                │
       │                                │                                │
       │  2. Generate EXPORT CFDI       │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    Exportacion="02"            │                                │
       │    (Definitive export)         │                                │
       │    Moneda="USD"                │                                │
       │    TipoCambio="17.50"          │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <Receptor                     │                                │
       │    Rfc="XEXX010101000"         │                                │
       │    (Generic foreign)           │                                │
       │    ResidenciaFiscal="USA"      │                                │
       │    NumRegIdTrib="12-3456789"   │                                │
       │    (US Tax ID)                 │                                │
       │    UsoCFDI="S01"               │                                │
       │  </Receptor>                   │                                │
       │                                │                                │
       │  <Conceptos>                   │                                │
       │    Importe="10000" (USD)       │                                │
       │    ObjetoImp="01"              │                                │
       │    (Not subject to tax)        │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  <Impuestos>                   │                                │
       │    TotalImpuestos="0.00"       │                                │
       │    (0% IVA for exports)        │                                │
       │  </Impuestos>                  │                                │
       │                                │                                │
       │  4. Stamp export CFDI ✅       │                                │
       │◀───────────────────────────────│                                │
       │                                │                                │
       │  MUST also add:                │                                │
       │  FOREIGN TRADE COMPLEMENT      │                                │
       │  (Comercio Exterior)           │                                │
       │                                │                                │
       │  <Complemento>                 │                                │
       │    <ComercioExterior>          │                                │
       │      TipoOperacion="2"         │                                │
       │      Incoterm="FOB"            │                                │
       │      Destinatario...           │                                │
       │      Mercancia...              │                                │
       │    </ComercioExterior>         │                                │
       │  </Complemento>                │                                │
```

**Export Requirements:**
- ✅ Exportacion = "02"
- ✅ RFC = "XEXX010101000"
- ✅ ResidenciaFiscal = Country
- ✅ NumRegIdTrib = Foreign tax ID
- ✅ IVA = 0%
- ✅ Foreign Trade Complement

---

## Scenario 21: Debit Note

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  After invoice, additional     │                                │
       │  charges discovered            │                                │
       │                                │                                │
       │  Original: $10,000             │                                │
       │  UUID: AAA-111                 │                                │
       │                                │                                │
       │  Additional charges:           │                                │
       │  - Late delivery penalty       │                                │
       │  - Extra freight               │                                │
       │  Total: $1,000                 │                                │
       │                                │                                │
       │  1. Create debit note (VF01)   │                                │
       │     Type L2                    │                                │
       │                                │                                │
       │  2. Generate DEBIT CFDI        │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    TipoDeComprobante="I"       │                                │
       │    (Income - adds receivable)  │                                │
       │    SubTotal="1000"             │                                │
       │    Total="1160"                │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <CfdiRelacionados>            │                                │
       │    TipoRelacion="02"           │                                │
       │    (Debit note)                │                                │
       │    UUID="AAA-111"              │                                │
       │  </CfdiRelacionados>           │                                │
       │                                │                                │
       │  <Conceptos>                   │                                │
       │    Descripcion="Additional     │                                │
       │      charges for AAA-111"      │                                │
       │    Importe="1000"              │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  3. Stamp debit note ✅        │                                │
       │◀───────────────────────────────│                                │
       │     UUID: BBB-222              │                                │
       │                                │                                │
       │  Customer now owes:            │                                │
       │  Original: $11,600             │                                │
       │  Debit: $1,160                 │                                │
       │  Total: $12,760                │                                │
```

**Debit vs Credit:**

| Aspect | Debit Note | Credit Note |
|--------|------------|-------------|
| Type | I (Income) | E (Egreso) |
| Effect | Increases AR | Decreases AR |
| Relation | "02" | "01" |
| Use | Add charges | Returns/discounts |

---

## Scenario 22: Zero-Rated Tax (0% IVA)

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Products subject to 0% IVA    │                                │
       │  (not exempt!)                 │                                │
       │                                │                                │
       │  Examples:                     │                                │
       │  - Basic food                  │                                │
       │  - Medicines                   │                                │
       │  - Books                       │                                │
       │                                │                                │
       │  1. Invoice for bread          │                                │
       │     Amount: $1,000             │                                │
       │                                │                                │
       │  2. Generate CFDI with 0% IVA  │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    SubTotal="1000"             │                                │
       │    Total="1000"                │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <Conceptos>                   │                                │
       │    ClaveProdServ="50121500"    │                                │
       │    Descripcion="Bread"         │                                │
       │    Importe="1000"              │                                │
       │    ObjetoImp="02"              │                                │
       │    (YES subject to tax)        │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  <Impuestos>                   │                                │
       │    <Traslados>                 │                                │
       │      <Traslado                 │                                │
       │        Impuesto="002" (IVA)    │                                │
       │        TasaOCuota="0.000000"   │                                │
       │        (0% rate)               │                                │
       │        Base="1000"             │                                │
       │        Importe="0.00"          │                                │
       │      />                        │                                │
       │    </Traslados>                │                                │
       │  </Impuestos>                  │                                │
       │                                │                                │
       │  CRITICAL DIFFERENCE:          │                                │
       │                                │                                │
       │  0% IVA (Zero-rated):          │                                │
       │  - ObjetoImp = "02" (Yes)      │                                │
       │  - TasaOCuota = "0.000000"     │                                │
       │  - Seller CAN recover input IVA│                                │
       │                                │                                │
       │  Exempt:                       │                                │
       │  - ObjetoImp = "01" (No)       │                                │
       │  - No tax element              │                                │
       │  - Seller CANNOT recover IVA   │                                │
```

**Tax Treatment:**

| Category | ObjetoImp | Rate | Recovery | Example |
|----------|-----------|------|----------|---------|
| Standard | 02 | 16% | ✅ | Most goods |
| Zero-rated | 02 | 0% | ✅ | Food, medicine |
| Exempt | 01 | N/A | ❌ | Education |

---

## Scenario 23: Mixed Tax Rates

```
┌──────────────┐                 ┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │                 │     SAT      │
└──────┬───────┘                 └──────┬───────┘                 └──────┬───────┘
       │                                │                                │
       │  Invoice with different tax    │                                │
       │  rates per item                │                                │
       │                                │                                │
       │  Item 1: Computer (16% IVA)    │                                │
       │  Item 2: Book (0% IVA)         │                                │
       │  Item 3: Training (Exempt)     │                                │
       │                                │                                │
       │  1. Generate CFDI              │                                │
       │───────────────────────────────▶│                                │
       │  <Comprobante                  │                                │
       │    SubTotal="12000"            │                                │
       │    Total="13920"               │                                │
       │  </Comprobante>                │                                │
       │                                │                                │
       │  <Conceptos>                   │                                │
       │    <!-- Computer -->           │                                │
       │    <Concepto                   │                                │
       │      Importe="10000"           │                                │
       │      ObjetoImp="02">           │                                │
       │      <Impuestos>               │                                │
       │        <Traslado               │                                │
       │          TasaOCuota="0.16"     │                                │
       │          Importe="1600"/>      │                                │
       │      </Impuestos>              │                                │
       │    </Concepto>                 │                                │
       │                                │                                │
       │    <!-- Book -->               │                                │
       │    <Concepto                   │                                │
       │      Importe="1000"            │                                │
       │      ObjetoImp="02">           │                                │
       │      <Impuestos>               │                                │
       │        <Traslado               │                                │
       │          TasaOCuota="0.00"     │                                │
       │          Importe="0"/>         │                                │
       │      </Impuestos>              │                                │
       │    </Concepto>                 │                                │
       │                                │                                │
       │    <!-- Training -->           │                                │
       │    <Concepto                   │                                │
       │      Importe="1000"            │                                │
       │      ObjetoImp="01">           │                                │
       │      <!-- No tax -->           │                                │
       │    </Concepto>                 │                                │
       │  </Conceptos>                  │                                │
       │                                │                                │
       │  <Impuestos>                   │                                │
       │    <!-- Summary -->            │                                │
       │    <Traslado                   │                                │
       │      TasaOCuota="0.16"         │                                │
       │      Base="10000"              │                                │
       │      Importe="1600"/>          │                                │
       │    <Traslado                   │                                │
       │      TasaOCuota="0.00"         │                                │
       │      Base="1000"               │                                │
       │      Importe="0"/>             │                                │
       │    TotalImpuestos="1600"       │                                │
       │  </Impuestos>                  │                                │
       │                                │                                │
       │  VALIDATION:                   │                                │
       │  SubTotal: $12,000             │                                │
       │  IVA 16%: $1,600               │                                │
       │  IVA 0%: $0                    │                                │
       │  Total: $13,600 ✅             │                                │
```

**Key Points:**
- Each item has own tax rate
- Summary must match line totals
- Different ObjetoImp per item
- Validation: line sum = header total

---

## Scenario 24: Intercompany Invoice

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│  Company A   │     │     PAC      │     │  Company B   │     │     SAT      │
│  (Seller)    │     │              │     │  (Buyer)     │     │              │
└──────┬───────┘     └──────┬───────┘     └──────┬───────┘     └──────┬───────┘
       │                    │                    │                    │
       │  Company A provides services to Company B                    │
       │  (Same corporate group)                                      │
       │                    │                    │                    │
       │  1. Create billing │                    │                    │
       │     Amount: $50,000│                    │                    │
       │                    │                    │                    │
       │  2. Generate CFDI  │                    │                    │
       │────────────────────▶                    │                    │
       │  <Emisor           │                    │                    │
       │    Rfc="AAA..." /> │                    │                    │
       │  <Receptor         │                    │                    │
       │    Rfc="BBB..." /> │                    │                    │
       │                    │                    │                    │
       │  IMPORTANT:        │                    │                    │
       │  Must use arm's    │                    │                    │
       │  length pricing!   │                    │                    │
       │                    │                    │                    │
       │  3. Stamp CFDI ✅  │                    │                    │
       │◀────────────────────                    │                    │
       │     UUID: III-111  │                    │                    │
       │                    │                    │                    │
       │  4. Send to Co B   │                    │                    │
       │────────────────────────────────▶        │                    │
       │                    │                    │                    │
       │                    │  5. Co B receives  │                    │
       │                    │     and records    │                    │
       │                    │                    │                    │
       │  TRANSFER PRICING: │                    │                    │
       │  If price ≠ market:│                    │                    │
       │  - May trigger audit                    │                    │
       │  - Requires TP documentation            │                    │
       │  - Both companies must align            │                    │
       │                    │                    │                    │
       │  CONSOLIDATION:    │                    │                    │
       │  - Financial statements: eliminate      │                    │
       │  - Tax: report separately               │                    │
       │  - CFDI REQUIRED even within group      │                    │
```

**Intercompany Rules:**
- ✅ CFDI required (even same group)
- ✅ Arm's length pricing
- ✅ Transfer pricing documentation
- ✅ Both companies report separately
- ⚠️ SAT monitors related party transactions

---

✅ **Part 3 Complete: Scenarios 17-24 added**

## Scenario 25: Rounding Differences

```
Issue: Line item taxes don't sum to header total

Item 1: $333.33 × 16% = $53.33
Item 2: $333.33 × 16% = $53.33  
Item 3: $333.34 × 16% = $53.34
Sum: $160.00 ✅

Header calculation:
$1000.00 × 16% = $160.00 ✅

MATCHES! But if rounding creates mismatch:
53.32 + 53.33 + 53.34 = 159.99
Header: 160.00
Difference: 0.01 ❌

SOLUTION: Adjust last line item
Item 3 tax: 53.35 (instead of 53.34)
Now: 53.32 + 53.33 + 53.35 = 160.00 ✅
```

**SAT Rules:**
- All amounts: 2 decimals
- Line sum MUST equal header
- Tolerance: 0.00 (zero!)
- Adjust last line if needed

---

## Scenario 26: Duplicate Detection

```
┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │
└──────┬───────┘                 └──────┬───────┘
       │                                │
       │  User creates same invoice 2x  │
       │                                │
       │  1st: Invoice 1900001 ✅       │
       │───────────────────────────────▶│
       │  Stamped, UUID: AAA-111        │
       │                                │
       │  2nd: Invoice 1900002          │
       │  (Same customer/amount/date)   │
       │                                │
       │  DETECTION:                    │
       │                                │
       │  Method 1: Before stamping     │
       │  Check SAP for duplicates:     │
       │  SELECT * WHERE                │
       │    kunnr = 'ABC'               │
       │    AND netwr = 10000           │
       │    AND fkdat = today           │
       │    AND status = 'STAMPED'      │
       │                                │
       │  Found match: Invoice 1900001  │
       │                                │
       │  Alert user:                   │
       │  "Possible duplicate - Continue?│
       │                                │
       │  Method 2: PAC detection       │
       │  If user proceeds:             │
       │───────────────────────────────▶│
       │  Serie A, Folio 1900002        │
       │                                │
       │  Error: "Folio already used"   │
       │◀───────────────────────────────│
```

**Prevention:**
- Lock invoice numbers
- Sequential only
- Check before billing
- Validation warnings
- User authorization controls

---

## Scenario 27: Year-End Closing

```
December 31, 2024

PRE-CLOSING:

1. Identify unstamped invoices
   Query all 2024 without UUID
   Found: 50 unstamped

2. Check timing
   Recent (Dec 29-31): Within 72h ✅
   Older: Need investigation ❌

3. Stamp recent invoices
   Batch process all within window

4. Older invoices:
   - Investigate why
   - Contact SAT if needed
   - Cancel/reissue if error

RECONCILIATION:

5. Generate report
   Total invoices: 10,000
   Stamped: 9,950
   Cancelled: 45
   Pending: 5

6. Query PAC for 2024
   All stamps for year

7. Compare
   SAP: 9,950 ✅
   PAC: 9,950 ✅
   Match!

ACCOUNTING CLOSE:

8. Final FI posting
   All invoices posted
   All payments cleared

9. Tax reports
   IVA report
   ISR report
   Annual return data

10. Archive CFDIs
    Export all 2024 XMLs
    Secure storage
    Backup externally

January 1, 2025

11. New certificate check
12. Update number ranges
13. First 2025 invoice test
```

**Year-End Checklist:**
- ✅ All invoices stamped
- ✅ SAP/PAC reconciled
- ✅ Accounting posted
- ✅ Tax reports ready
- ✅ XMLs archived
- ✅ Certificate current
- ✅ New year configured

---

## Scenario 28: Customer Bankruptcy

```
Customer declares bankruptcy
Outstanding: $500,000 in invoices

DECISION:

Option 1: Keep CFDIs active (RECOMMENDED)
- CFDIs remain valid
- File with bankruptcy court
- Claim as creditor
- DO NOT cancel

Reason: Cancelled CFDIs weaken legal claim

Option 2: Cancel if requested (RARE)
Only if:
- Court order
- Trustee authorization
- Legal requirement

Then:
1. Verify legal authority
2. Cancel CFDIs (Reason "03")
3. Write off in accounting
4. Document incident

TAX IMPLICATIONS:

If CFDIs active:
- Revenue recognized
- IVA paid
- Can claim bad debt deduction

If CFDIs cancelled:
- Revenue reversed
- IVA credit
- Amend tax returns
```

**Recommendation:**
Keep CFDIs active for bankruptcy proceedings

---

## Scenario 29: Recurring Billing (Subscriptions)

```
┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │
└──────┬───────┘                 └──────┬───────┘
       │                                │
       │  SaaS subscription             │
       │  $5,000/month × 12 months      │
       │                                │
       │  Jan 1: Month 1                │
       │  1. Auto-create invoice        │
       │     "SaaS subscription Jan"    │
       │───────────────────────────────▶│
       │  2. Stamp ✅ UUID-JAN          │
       │◀───────────────────────────────│
       │                                │
       │  Feb 1: Month 2                │
       │  3. Auto-create invoice        │
       │     "SaaS subscription Feb"    │
       │───────────────────────────────▶│
       │  OPTIONAL: Link to previous    │
       │  TipoRelacion="07"             │
       │  UUID="UUID-JAN"               │
       │                                │
       │  4. Stamp ✅ UUID-FEB          │
       │◀───────────────────────────────│
       │                                │
       │  [Continue monthly...]         │
       │                                │
       │  Jun 15: Customer cancels      │
       │                                │
       │  5. Pro-rata calculation       │
       │     Jun 1-15 = 15 days         │
       │     Amount: $2,500 (half)      │
       │                                │
       │  6. Final invoice              │
       │     Cantidad="0.5"             │
       │     ValorUnitario="5000"       │
       │     Importe="2500"             │
       │───────────────────────────────▶│
       │                                │
       │  7. Stamp final ✅             │
       │◀───────────────────────────────│
```

**Best Practices:**
- Schedule batch job for 1st of month
- Include period in description
- Sequential linking optional
- Auto-send to customers
- Pro-rata for cancellations

---

## Scenario 30: Drop Shipment

```
┌──────────────┐   ┌──────────────┐   ┌──────────────┐
│   Reseller   │   │ Manufacturer │   │   Customer   │
│   (You)      │   │  (Supplier)  │   │              │
└──────┬───────┘   └──────┬───────┘   └──────┬───────┘
       │                  │                  │
       │  Customer orders $10,000            │
       │  Your cost: $7,000                  │
       │                  │                  │
       │  1. Place PO     │                  │
       │──────────────────▶                  │
       │  $7,000          │                  │
       │                  │  2. Ships to     │
       │                  │     customer ────▶
       │                  │     (not you)    │
       │                  │                  │
       │  CFDI FLOW:      │                  │
       │                  │                  │
       │  3. Mfr→You CFDI │                  │
       │◀──────────────────                  │
       │  $7,000          │                  │
       │  UUID-A          │                  │
       │                  │                  │
       │  4. You→Customer CFDI               │
       │─────────────────────────────────────▶
       │  $10,000         │                  │
       │  Ship-to: Customer address          │
       │  UUID-B          │                  │
       │                  │                  │
       │  Two CFDIs required:                │
       │  - Mfr→You: $7k (cost)              │
       │  - You→Customer: $10k (sale)        │
```

**Key Points:**
- Two separate CFDIs required
- Manufacturer invoices reseller
- Reseller invoices end customer
- Ship-to shows final destination

---

## Scenario 31: Consignment Sales

```
┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │
└──────┬───────┘                 └──────┬───────┘
       │                                │
       │  Transfer inventory to         │
       │  customer warehouse            │
       │  (consignment)                 │
       │                                │
       │  Month 1: Transfer 100 units   │
       │                                │
       │  1. Create transfer order      │
       │                                │
       │  2. Generate TRANSFER CFDI     │
       │───────────────────────────────▶│
       │  Type: "T" (Traslado)          │
       │  Total: "0.00"                 │
       │  Quantity: 100                 │
       │  Value: 0                      │
       │                                │
       │  3. Stamp transfer ✅          │
       │◀───────────────────────────────│
       │     UUID-TRANSFER              │
       │                                │
       │  Month 2: Customer uses 30     │
       │                                │
       │  4. Receive consumption report │
       │     30 units consumed          │
       │                                │
       │  5. Create sales invoice       │
       │     Amount: $3,000             │
       │                                │
       │  6. Generate SALES CFDI        │
       │───────────────────────────────▶│
       │  Type: "I" (Income)            │
       │  <CfdiRelacionados>            │
       │    TipoRelacion="05"           │
       │    UUID="UUID-TRANSFER"        │
       │  </CfdiRelacionados>           │
       │                                │
       │  7. Stamp sales ✅             │
       │◀───────────────────────────────│
       │                                │
       │  Inventory:                    │
       │  Transferred: 100              │
       │  Invoiced: 30                  │
       │  Remaining: 70                 │
```

**Consignment Types:**

| Event | Type | Total | Tax | Link |
|-------|------|-------|-----|------|
| Transfer | T | 0.00 | 0% | No |
| Sale | I | Full | 16% | Yes |
| Return | T | 0.00 | 0% | Yes |

---

## Scenario 32: Milestone Billing (Projects)

```
┌──────────────┐                 ┌──────────────┐
│     SAP      │                 │     PAC      │
└──────┬───────┘                 └──────┬───────┘
       │                                │
       │  Software project: $100,000    │
       │  Duration: 4 months            │
       │                                │
       │  Milestones:                   │
       │  1. Requirements (20%) $20k    │
       │  2. Design (30%) $30k          │
       │  3. Development (40%) $40k     │
       │  4. Testing (10%) $10k         │
       │                                │
       │  Month 1: Milestone 1 done     │
       │                                │
       │  1. Create invoice #1          │
       │     Amount: $20,000            │
       │                                │
       │  2. Generate CFDI              │
       │───────────────────────────────▶│
       │  <Conceptos>                   │
       │    Descripcion="Milestone 1    │
       │      - Requirements (20%)"     │
       │    Cantidad="1"                │
       │    ClaveUnidad="E48"           │
       │    ValorUnitario="20000"       │
       │  </Conceptos>                  │
       │                                │
       │  3. Stamp M1 ✅                │
       │◀───────────────────────────────│
       │     UUID-M1                    │
       │                                │
       │  Month 3: Milestone 2 done     │
       │                                │
       │  4. Create invoice #2          │
       │     Amount: $30,000            │
       │                                │
       │  5. Generate CFDI              │
       │───────────────────────────────▶│
       │  <CfdiRelacionados>            │
       │    TipoRelacion="07"           │
       │    UUID="UUID-M1"              │
       │  </CfdiRelacionados>           │
       │  Descripcion="Milestone 2      │
       │    - Design (Cumulative 50%)"  │
       │                                │
       │  6. Stamp M2 ✅                │
       │◀───────────────────────────────│
       │                                │
       │  [Continue for M3, M4...]      │
       │                                │
       │  Final: All milestones         │
       │  Total: $20k+$30k+$40k+$10k    │
       │       = $100,000 ✅            │
```

**Milestone Best Practices:**
- Clear description with %
- Link to previous milestones
- Track cumulative progress
- Only bill completed work
- No credit if cancelled early

---

✅ **Part 4 Complete: Scenarios 25-32 added**

## Scenario 33: Hotel/Hospitality

```
Guest checkout with multiple charges

Guest: John Smith
Dec 1-5 (4 nights)

Charges:
- Room (4 nights): $8,000
- Restaurant: $2,000
- Minibar: $500
- Spa: $1,500
Total: $12,000

CFDI Generation:

<Comprobante Total="14,260">
  <Conceptos>
    <!-- Room -->
    <Concepto 
      ClaveProdServ="90101501"
      Descripcion="Room - 4 nights"
      Cantidad="4"
      Importe="8000"
      ObjetoImp="02">
      <Impuestos>
        <!-- Room: 16% IVA + 3% Lodging -->
        <Traslado Impuesto="002" TasaOCuota="0.16" Importe="1280"/>
        <Traslado Impuesto="003" TasaOCuota="0.03" Importe="240"/>
      </Impuestos>
    </Concepto>
    
    <!-- Restaurant -->
    <Concepto 
      Descripcion="Restaurant"
      Importe="2000"
      ObjetoImp="02">
      <Impuestos>
        <Traslado Impuesto="002" TasaOCuota="0.16" Importe="320"/>
      </Impuestos>
    </Concepto>
    
    <!-- Other services: 16% IVA only -->
  </Conceptos>
  
  <Impuestos TotalImpuestos="2260">
    <!-- IVA: 1280+320+80+240 = 1920 -->
    <!-- Lodging: 240 -->
  </Impuestos>
</Comprobante>

FOREIGN TOURIST:
If no Mexican RFC:
  Rfc="XEXX010101000"
  ResidenciaFiscal="USA"
```

**Hotel Tax Rates:**

| Service | IVA | Lodging | Total |
|---------|-----|---------|-------|
| Room | 16% | 3% | 19% |
| Restaurant | 16% | - | 16% |
| Other | 16% | - | 16% |

---

## Scenario 34: Construction Long-term Project

```
2-year construction: $50 million
Monthly progress billing

Month 1: 5% complete = $2.5M

CFDI with Withholdings:

<Comprobante SubTotal="2500000" Total="2750000">
  <Conceptos>
    <Concepto 
      ClaveProdServ="72141500"
      Descripcion="Construction progress
        - Month 1 (5%)"
      Importe="2500000"
      ObjetoImp="02">
      <Impuestos>
        <Retenciones>
          <!-- 6% ISR withholding -->
          <Retencion Impuesto="001" Importe="150000"/>
        </Retenciones>
        <Traslados>
          <!-- 16% IVA -->
          <Traslado Importe="400000"/>
        </Traslados>
      </Impuestos>
    </Concepto>
  </Conceptos>
</Comprobante>

Invoice total:
Subtotal: $2,500,000
+ IVA 16%: $400,000
- ISR 6%: ($150,000)
Net: $2,750,000

Month 2: Link to Month 1
<CfdiRelacionados>
  TipoRelacion="07"
  UUID="[month-1-uuid]"
</CfdiRelacionados>

Continue monthly until 100%
```

**Construction Withholding:**
- ISR: 6% (on subtotal)
- IVA: 0% withholding

---

## Scenario 35: Healthcare Services

```
Patient: Maria Lopez
Surgical procedure

Charges:
- Surgery: $50,000
- Hospital stay: $20,000
- Medications: $5,000
- Lab tests: $3,000
Total: $78,000

CFDI Generation:

<Comprobante Total="78000">
  <Conceptos>
    <!-- Surgery -->
    <Concepto 
      ClaveProdServ="85101500"
      Descripcion="Surgical procedure"
      Importe="50000"
      ObjetoImp="01">
      <!-- EXEMPT - No tax -->
    </Concepto>
    
    <!-- Hospital stay -->
    <Concepto 
      Descripcion="Hospital stay - 2 days"
      Importe="20000"
      ObjetoImp="01">
      <!-- EXEMPT -->
    </Concepto>
    
    <!-- Medications -->
    <Concepto 
      Descripcion="Prescription meds"
      Importe="5000"
      ObjetoImp="02">
      <Impuestos>
        <!-- Medicine: 0% IVA -->
        <Traslado TasaOCuota="0.00" Importe="0"/>
      </Impuestos>
    </Concepto>
    
    <!-- Lab tests -->
    <Concepto 
      Descripcion="Laboratory tests"
      Importe="3000"
      ObjetoImp="01">
      <!-- EXEMPT -->
    </Concepto>
  </Conceptos>
  
  <Impuestos TotalImpuestos="0.00"/>
</Comprobante>

Total: $78,000 (No IVA)

INSURANCE PAYMENT:
80% from insurance: $62,400
20% from patient: $15,600

Generate payment complements for each
```

**Healthcare Tax Treatment:**

| Service | Treatment | Rate |
|---------|-----------|------|
| Medical | Exempt | 0% |
| Hospital | Exempt | 0% |
| Medicines | Zero-rated | 0% |
| Labs | Exempt | 0% |
| Dental | IVA | 16% |
| Cosmetic | IVA | 16% |

---

## Scenario 36: Wrong Customer Selected

```
Intended: Customer A (RFC: AAA...)
Actual: Customer B (RFC: BBB...)

1. Create invoice to B ❌
   Invoice: 1900100
   
2. Generate & stamp CFDI
   <Receptor Rfc="BBB..." /> ❌
   UUID: WRONG-UUID

3. User realizes error

IMMEDIATE CORRECTION:

4. Cancel wrong CFDI
   Reason: "01" (with relation)

5. Create invoice to A ✅
   Invoice: 1900101

6. Generate correct CFDI
   <Receptor Rfc="AAA..." /> ✅
   <CfdiRelacionados>
     TipoRelacion="04"
     UUID="WRONG-UUID"
   </CfdiRelacionados>

7. Stamp correct CFDI ✅
   UUID: CORRECT-UUID

8. Update accounting
   - Clear Customer B AR
   - Post to Customer A AR

PREVENTION:
Implement validation BADI:
IF kunnr <> expected THEN
  Display warning
ENDIF
```

---

## Scenario 37: Testing in Production

```
Developer testing in production ❌

1. Create test invoice
   Customer: "TEST"
   Amount: $999,999.99
   
2. Generate CFDI
   Connected to PROD PAC! ❌

4. Stamped in production SAT! ❌
   UUID: TEST-UUID

5. Developer realizes error
   "Oh no! I'm in production!"

IMMEDIATE ACTION:

6. Cancel test CFDI
   Reason: "02" (error)

7. Delete test billing doc

8. Document incident

PREVENTION:

9. Safeguards:
   a) Client-specific colors
      - DEV: Blue
      - QA: Yellow
      - PROD: Red background
   
   b) Warning popup
      "You are in PRODUCTION"
   
   c) Separate user IDs
      Dev users → no prod access
   
   d) Test customer block
      Block stamping for 
      customer# > 9000000
```

---

## Scenario 38: Price Change After Delivery

```
Day 1: Delivered & invoiced
- 100 units @ $100 = $10,000
- UUID: ORIGINAL

Day 5: Price negotiation
- New price: $95/unit
- Total: $9,500
- Difference: $500 credit

SOLUTION 1: Credit Note (PREFERRED)

1. Create credit memo
   Amount: $500

2. Generate CREDIT CFDI
   Type: "E"
   <CfdiRelacionados>
     TipoRelacion="01"
     UUID="ORIGINAL"
   </CfdiRelacionados>
   <Conceptos>
     Descripcion="Price adjustment
       - 100 units @ $5/unit"
     Importe="500"
   </Conceptos>

Result:
Original: $10,000
Credit: -$500
Net: $9,500 ✅

SOLUTION 2: Cancel & Reissue
(More work, less preferred)

5. Cancel original
   Reason: "01"

6. Create new invoice
   Amount: $9,500
   TipoRelacion="04"
```

---

## Scenario 39: Government Customer (B2G)

```
Customer: Secretaría de Hacienda
Contract: ABC-2025-001
Amount: $500,000

GOVERNMENT CFDI:

<Comprobante>
  <Receptor
    Rfc="SHC940917KL0"
    UsoCFDI="G03"
    (Government expenses)
  />
  
  <Conceptos>
    <Concepto 
      Descripcion="IT services
        - Contract ABC-2025-001
        Month 1"
      NumIdentificacion="ABC-2025-001"
      (Contract reference)
    />
  </Conceptos>
</Comprobante>

PAYMENT PROCESS:

1. Submit to agency portal
   - Upload CFDI XML
   - Upload supporting docs
   - Contract reference

2. Agency validates (30-60 days)

3. Payment scheduled (60-90 days)

4. Generate payment complement
   MetodoPago: "PPD"
```

**B2G Requirements:**
- ✅ Contract number in description
- ✅ Specific product codes
- ✅ Vendor registration
- ✅ UsoCFDI for government
- ✅ Portal submission

**Common UsoCFDI:**
- G01: Acquisition of goods
- G02: Returns/discounts
- G03: General expenses
- I01-I08: Investments

---

## Scenario 40: Educational Institution

```
Customer: UNAM (Universidad)
Product: Educational books
Quantity: 1,000 books
Amount: $50,000

EDUCATION CFDI:

<Comprobante Total="50000">
  <Receptor
    Rfc="UNA871210N41"
    UsoCFDI="P01"
    (Authorized entities)
  />
  
  <Conceptos>
    <Concepto 
      ClaveProdServ="55101503"
      (Educational books)
      Cantidad="1000"
      ValorUnitario="50"
      Importe="50000"
      ObjetoImp="02">
      <Impuestos>
        <Traslados>
          <Traslado 
            Impuesto="002"
            TasaOCuota="0.00"
            (0% for books)
            Base="50000"
            Importe="0.00"
          />
        </Traslados>
      </Impuestos>
    </Concepto>
  </Conceptos>
</Comprobante>

Total: $50,000 (No IVA)

KEY DIFFERENCES:

Educational services:
- ObjetoImp="01" (Exempt)
- Cannot recover input IVA

Educational materials (books):
- ObjetoImp="02" (Yes, tax)
- TasaOCuota="0.00" (0%)
- CAN recover input IVA
```

---

## 🎉 ALL 40 SCENARIOS COMPLETE!

You now have comprehensive documentation for:

### Normal Operations (4)
1. Invoice Generation
2. Timbrado Flow
7. Payment Complement
8. Global Invoice

### Changes & Corrections (8)
3. Invoice Update
4. Cancellation (24h / >24h)
5. Credit Note
13. RFC Change
17. Split Invoice
36. Wrong Customer
38. Price Change

### Data Issues (4)
6a-d. SAP/PAC/SAT sync problems

### Special Transactions (11)
14. Foreign Currency
16. Withholding
18. Advance Payment
19. Free Items
20. Export
21. Debit Note
22. Zero-Rated
23. Mixed Taxes
24. Intercompany

### Operations (7)
9. Certificate Expiration
10. PAC Switching
11. Mass Failure
12. Retroactive
15. SAT Downtime
25. Rounding
26. Duplicates
27. Year-End
28. Bankruptcy
37. Testing in Prod

### Recurring & Projects (2)
29. Subscriptions
32. Milestones

### Logistics (2)
30. Drop Shipment
31. Consignment

### Industry (5)
33. Hotel
34. Construction
35. Healthcare
39. Government
40. Education

---

✅ **Part 5 Complete: All 40 scenarios documented!**

---

# QUICK REFERENCE GUIDES

## Decision Tree: What CFDI Type?

```
Is this a sale/service BY you?
│
├─ YES → Type "I" (Ingreso)
│   ├─ Full price? → Standard Invoice
│   ├─ Free? → 100% Discount
│   ├─ Export? → Exportacion="02"
│   └─ Advance? → 100% Discount + Link
│
└─ NO → What are you doing?
    ├─ Reducing invoice? → Type "E" (Credit), Rel="01"
    ├─ Adding charges? → Type "I" (Debit), Rel="02"
    ├─ Moving goods? → Type "T" (Transfer), Total=0
    ├─ Recording payment? → Type "P" (Payment Complement)
    └─ Payroll? → Type "N" (Nomina)
```

## Decision Tree: Cancel or Credit?

```
Need to correct invoice?
│
├─ WRONG DATA (customer, amount)?
│   └─ CANCEL + REISSUE
│       Reason="01", TipoRelacion="04"
│
├─ RETURNED GOODS?
│   └─ CREDIT NOTE
│       Type="E", TipoRelacion="01"
│       Original stays valid
│
├─ PRICE DISCOUNT?
│   └─ CREDIT NOTE
│       Type="E", for difference
│
├─ NEVER HAPPENED?
│   └─ CANCEL ONLY
│       Reason="03", no replacement
│
└─ DUPLICATE?
    └─ CANCEL duplicate
        Reason="02", keep correct one
```

## Decision Tree: Tax Treatment

```
What am I selling?
│
├─ GOODS
│   ├─ Food/Medicine/Books? → 0% IVA, Obj="02"
│   ├─ Export? → 0% IVA, Obj="01"
│   └─ Other? → 16% IVA, Obj="02"
│
└─ SERVICES
    ├─ Medical/Education? → EXEMPT, Obj="01"
    ├─ Professional? → 16% IVA + 10% ISR withholding
    ├─ Construction? → 16% IVA + 6% ISR withholding
    └─ Other? → 16% IVA, Obj="02"
```

---

## Common Error Codes

| Code | Message | Cause | Solution |
|------|---------|-------|----------|
| 301 | Invalid XML | Schema error | Validate XSD |
| 302 | Invalid signature | Certificate | Check cert |
| 303 | Duplicate folio | Used before | Next number |
| 304 | Invalid RFC | Not in SAT | Verify RFC |
| 305 | Invalid cert | Expired | Renew cert |
| 306 | Amount mismatch | Calculation | Check math |
| 307 | Invalid date | >72hrs | Fix timestamp |
| 308 | Invalid code | Wrong catalog | Check SAT |
| 309 | Missing field | Incomplete | Add data |
| 310 | Invalid relation | Bad UUID | Check link |

---

## Severity Matrix

| Issue | Severity | Action | Timeline |
|-------|----------|--------|----------|
| Cert expires 7 days | 🔴 CRITICAL | Renew now | Today |
| Cert expires 30 days | 🟡 WARNING | Schedule | This week |
| SAT down | 🔴 CRITICAL | Queue invoices | Monitor |
| PAC slow | 🟡 WARNING | Monitor | 24 hours |
| Single fail | 🟢 NORMAL | Retry | 1 hour |
| 5% failing | 🟡 WARNING | Investigate | 4 hours |
| 20% failing | 🔴 CRITICAL | Stop batch | Immediate |
| Duplicate UUID | 🔴 CRITICAL | Cancel dup | 1 hour |
| Unstamped >24h | 🟡 WARNING | Stamp now | 2 hours |
| Unstamped >72h | 🔴 CRITICAL | Contact SAT | Immediate |

---

## Timing Guidelines

| Activity | Recommended | Maximum | Penalty |
|----------|-------------|---------|---------|
| Stamp after create | Immediate | 72 hours | Rejection |
| Cancel <24hrs | Hours | 24 hours | None |
| Cancel >24hrs | Send request | 72h auto | Approval needed |
| Payment complement | With payment | 10 days | Penalty |
| SAT audit response | Immediate | As specified | Fines |
| Cert renewal | 60 days before | 30 days | Interruption |
| Year-end recon | Dec 31 | Jan 15 | Delays |

---

## Monthly Checklist

**Day 1:**
- ✅ Process recurring invoices
- ✅ Generate subscriptions
- ✅ Check certificate expiration

**Day 15:**
- ✅ Mid-month reconciliation
- ✅ Review failed stamps
- ✅ Check PAC quota

**Last Day:**
- ✅ Final reconciliation
- ✅ Monthly reports
- ✅ Archive CFDIs
- ✅ Verify all stamped
- ✅ Close period

**Monthly:**
- ✅ Pay PAC invoice
- ✅ Backup XMLs
- ✅ Test disaster recovery
- ✅ Review error logs
- ✅ Update SAT catalogs

---

## SQL Monitoring Queries

**Check unstamped:**
```sql
SELECT vbeln, fkdat, netwr, kunnr
FROM vbrk v
LEFT JOIN ztcfdi_status z ON v.vbeln = z.vbeln
WHERE v.fkdat >= '2025-01-01'
  AND z.uuid IS NULL
  AND v.fksto = ''
ORDER BY fkdat DESC;
```

**Check duplicates:**
```sql
SELECT uuid, COUNT(*) as count, 
       STRING_AGG(vbeln, ', ') as invoices
FROM ztcfdi_status
WHERE uuid IS NOT NULL
GROUP BY uuid
HAVING COUNT(*) > 1;
```

**Check failures:**
```sql
SELECT vbeln, error_code, error_message, 
       timestamp, retry_count
FROM ztcfdi_errors
WHERE error_date >= CURRENT_DATE - 7
  AND resolved = ''
ORDER BY timestamp DESC;
```

**Monthly reconciliation:**
```sql
SELECT 
  DATE_TRUNC('month', fkdat) as month,
  COUNT(*) as total,
  SUM(CASE WHEN uuid IS NOT NULL THEN 1 ELSE 0 END) as stamped,
  SUM(CASE WHEN cancelled = 'X' THEN 1 ELSE 0 END) as cancelled,
  SUM(CASE WHEN uuid IS NULL AND cancelled = '' THEN 1 ELSE 0 END) as pending
FROM vbrk v
LEFT JOIN ztcfdi_status z ON v.vbeln = z.vbeln
WHERE fkdat >= '2025-01-01'
GROUP BY month
ORDER BY month DESC;
```

---

## Troubleshooting Flowchart

```
Invoice won't stamp?
│
├─ RFC valid? → No → Fix customer master
├─ Certificate valid? → No → Renew certificate
├─ XML structure OK? → No → Fix generation
├─ PAC connection OK? → No → Check network
├─ SAT status OK? → No → Queue and wait
├─ Amounts correct? → No → Fix rounding
└─ SAT codes valid? → No → Update codes
   └─ If all OK → Contact PAC support
```

---

## Emergency Contacts Template

```
┌─────────────────────────────────────┐
│  CFDI EMERGENCY CONTACTS            │
├─────────────────────────────────────┤
│  PAC - Montova:                     │
│  Support: soporte@montova.com.mx    │
│  Emergency: [24/7 NUMBER]           │
│                                     │
│  PAC - Edicom:                      │
│  Support: soporte@edicomgroup.com   │
│  Emergency: [24/7 NUMBER]           │
│                                     │
│  SAT:                               │
│  General: 55 627 22 728             │
│  Portal: sat.gob.mx                 │
│                                     │
│  Internal:                          │
│  SAP Basis: [NAME] [PHONE]          │
│  Finance: [NAME] [PHONE]            │
│  Tax Advisor: [NAME] [PHONE]        │
│                                     │
│  Escalation:                        │
│  1. Developer (0-2h)                │
│  2. Basis (2-4h)                    │
│  3. Finance (4-8h)                  │
│  4. Director (8h+)                  │
└─────────────────────────────────────┘
```

---

## Implementation Timeline

### Phase 1: Planning (Weeks 1-2)
- ✅ Review documentation
- ✅ Choose PAC
- ✅ Obtain certificate
- ✅ Design tables
- ✅ Map catalogs

### Phase 2: Development (Weeks 3-8)
- ✅ CFDI generator
- ✅ PAC integration
- ✅ Error handling
- ✅ Reports
- ✅ Unit testing

### Phase 3: Testing (Weeks 9-12)
- ✅ PAC sandbox
- ✅ End-to-end
- ✅ Error scenarios
- ✅ Performance
- ✅ UAT

### Phase 4: Certification (Weeks 13-14)
- ✅ PAC certification
- ✅ SAT validation
- ✅ Fix issues
- ✅ Final approval

### Phase 5: Go-Live (Week 15)
- ✅ Cutover
- ✅ Monitor 24/7
- ✅ Support users
- ✅ Fix immediately

### Phase 6: Stabilization (Weeks 16-20)
- ✅ Monitor
- ✅ Tune performance
- ✅ Refine processes
- ✅ Complete training

---

## Success Metrics

### Technical
- Stamp Success: >99%
- Avg Stamp Time: <5s
- Availability: >99.5%
- Failed Stamps: <1%
- Cert Uptime: 100%

### Business
- Daily Volume: Track
- Complaints: <0.1%
- SAT Audits: 0 issues
- Cancel Rate: <2%
- Payment Delay: <30d

### Operational
- MTTS: <3s
- Error Resolution: <2h
- Reconciliation: 100%
- Year-End On-Time: Yes

---

## Additional Resources

### SAT Resources
- [SAT Portal](http://www.sat.gob.mx)
- [CFDI 4.0 Spec](http://www.sat.gob.mx/informacion_fiscal/factura_electronica/)
- [Validation Tool](https://verificacfdi.facturaelectronica.sat.gob.mx/)

### PAC Documentation
- [Montova](https://montova.com)
- [Edicom](https://www.edicomgroup.com)

### Key SAT Catalogs
- c_FormaPago (Payment methods)
- c_MetodoPago (PUE/PPD)
- c_UsoCFDI (CFDI usage)
- c_ClaveProdServ (Products/services)
- c_ClaveUnidad (Units of measure)
- c_TipoRelacion (Relationship types)

---

# 🎉 COMPLETE REPOSITORY READY!

## What You Have

✅ **Complete Structure**
- Professional README
- 40 detailed scenarios
- Quick reference guides
- Decision trees
- Error handling
- SQL queries
- Implementation plan

✅ **Ready for GitHub**
- All files created
- Security configured
- Documentation complete
- Code templates included

✅ **Production Ready**
- Best practices
- Real-world scenarios
- Complete error handling
- Monitoring tools

## Next Steps

1. **Review**: Check all generated files
2. **Customize**: Add your company info
3. **Git Init**: Initialize repository
4. **Push**: Upload to GitHub
5. **Share**: With your team

## Git Commands

```bash
cd sap-cfdi-mexico
git init
git add .
git commit -m "Complete SAP CFDI Mexico implementation - 40 scenarios"
git branch -M main
git remote add origin https://github.com/YOUR_USERNAME/sap-cfdi-mexico.git
git push -u origin main
```

---

**Repository Version**: 1.0  
**Total Scenarios**: 40  
**CFDI Version**: 4.0  
**Last Updated**: December 2024

**Thank you for using this comprehensive guide!** 🇲🇽🚀
