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
