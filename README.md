# SAP CFDI 4.0 Mexico Implementation

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![SAP](https://img.shields.io/badge/SAP-ECC%20%7C%20S%2F4HANA-blue)](https://www.sap.com)
[![CFDI](https://img.shields.io/badge/CFDI-4.0-green)](http://www.sat.gob.mx/)

Complete SAP implementation for Mexican electronic invoicing (CFDI 4.0) with comprehensive documentation covering 40 detailed business scenarios.

## 📋 Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Documentation](#documentation)
- [PAC Integration](#pac-integration)
- [Architecture](#architecture)
- [Contributing](#contributing)
- [License](#license)
- [Support](#support)

## 🎯 Overview

This repository provides a complete implementation guide for CFDI (Comprobante Fiscal Digital por Internet) 4.0 in SAP systems, compliant with Mexican tax regulations (SAT). It includes ABAP code, integration patterns, and detailed documentation for 40 real-world scenarios.

**CFDI 4.0** is Mexico's mandatory electronic invoicing standard, required for all businesses operating in Mexico since January 2022.

## ✨ Features

- **Complete CFDI 4.0 Implementation**
  - Full XML generation compliant with SAT specifications
  - Digital signature integration (CSD/FIEL)
  - Automatic validation against SAT catalogs

- **40 Documented Scenarios**
  - Sales invoices, credit notes, debit notes
  - Payment complements
  - Foreign trade complements
  - Withholding receipts
  - Special tax scenarios

- **Multi-PAC Support**
  - Montova PAC integration (REST/SOAP)
  - Edicom PAC integration (REST/SOAP/Add-on)
  - Extensible architecture for other PACs

- **Production-Ready ABAP Code**
  - Modular class design
  - Error handling and retry logic
  - Configurable for multiple environments
  - Secure credential management

- **Comprehensive Documentation**
  - Step-by-step implementation guides
  - Flow diagrams for all scenarios
  - Troubleshooting guides
  - Best practices

## 🔧 Prerequisites

### SAP System Requirements
- SAP ECC 6.0 or higher, or SAP S/4HANA
- SAP Basis 7.40 or higher
- Sales and Distribution (SD) module
- Financial Accounting (FI) module

### Technical Requirements
- ABAP development access (S_DEVELOP authorization)
- SSF (Secure Store and Forward) for certificate management
- HTTP/HTTPS connectivity to PAC providers
- XML processing capabilities (iXML library)

### Legal Requirements
- Active RFC (Tax ID) registered with SAT
- Valid Digital Certificate (CSD) from SAT
- Contract with certified PAC provider
- Legal entity established in Mexico

### Knowledge Requirements
- ABAP programming experience
- SAP SD billing process knowledge
- Basic understanding of CFDI requirements
- XML and web services concepts

## 📥 Installation

### Step 1: Clone Repository
```bash
git clone https://github.com/yourusername/sap-cfdi-mexico.git
cd sap-cfdi-mexico
```

### Step 2: Transport ABAP Code
1. Create transport request in SAP (SE09/SE10)
2. Import ABAP classes from `src/abap/classes/`
3. Activate all objects
4. Assign required authorizations

### Step 3: Configure Certificates
1. Import your CSD certificate to SAP SSF (STRUST)
2. Note the certificate alias for configuration
3. Verify certificate validity period

### Step 4: Configure PAC Integration
1. Copy `config/config.template.json` to custom table or configuration
2. Update with your PAC credentials:
   ```json
   {
     "pac_provider": "montova",
     "montova": {
       "test": {
         "rest_url": "https://timbrado-pruebas.montova.com.mx/api/v1",
         "username": "YOUR_USERNAME",
         "password": "STORED_IN_SAP_SSF",
         "rfc": "YOUR_RFC"
       }
     }
   }
   ```

### Step 5: Test Configuration
1. Run test program with sample billing document
2. Verify XML generation
3. Test PAC connection (use test environment)
4. Validate stamped CFDI received

## 🚀 Quick Start

### Basic Usage

```abap
" Generate CFDI from billing document
DATA: lo_cfdi TYPE REF TO zcl_cfdi_generator,
      lv_xml  TYPE string,
      lv_uuid TYPE string.

CREATE OBJECT lo_cfdi.

TRY.
    " Generate XML
    lv_xml = lo_cfdi->generate_from_billing_doc(
      iv_vbeln = '0090000123'  " Billing document
      iv_version = '4.0'
    ).

    " Send to PAC and receive UUID
    " Implementation in PAC integration class

  CATCH cx_cfdi_generation INTO DATA(lx_error).
    " Handle error
    WRITE: / lx_error->get_text( ).
ENDTRY.
```

### Full Example

See [Example Programs](src/abap/examples/) for complete working examples including:
- Simple invoice generation
- Credit note processing
- Payment complement creation
- Error handling patterns

## 📚 Documentation

### Core Documentation
1. **[CFDI Overview](docs/01-cfdi-overview/README.md)**
   - What is CFDI?
   - CFDI structure and components
   - Legal requirements
   - SAT catalogs reference

2. **[Architecture & 40 Scenarios](docs/02-architecture/README.md)**
   - System architecture
   - Complete scenario documentation
   - Flow diagrams
   - Error handling

3. **[Deployment Guide](docs/03-deployment/README.md)**
   - Environment setup
   - Certificate management
   - Production checklist
   - Performance tuning

4. **[Troubleshooting](docs/04-troubleshooting/README.md)**
   - Common errors and solutions
   - Debugging guides
   - Performance issues
   - PAC-specific problems

5. **[FAQ](docs/05-faq/README.md)**
   - Frequently asked questions
   - Best practices
   - Tips and tricks

## 🔌 PAC Integration

This implementation supports multiple PAC (Proveedor Autorizado de Certificación) providers:

### Supported PACs

| PAC | REST API | SOAP API | Native Add-on | Documentation |
|-----|----------|----------|---------------|---------------|
| **Montova** | ✅ | ✅ | ❌ | [Guide](src/pac-integration/montova/README.md) |
| **Edicom** | ✅ | ✅ | ✅ | [Guide](src/pac-integration/edicom/README.md) |

### Adding New PAC
See [PAC Integration Guide](docs/pac-integration-guide.md) for instructions on adding support for additional PAC providers.

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         SAP System                          │
│                                                             │
│  ┌──────────────┐      ┌──────────────┐     ┌───────────┐  │
│  │ SD Billing   │─────▶│ CFDI Engine  │────▶│    PAC    │  │
│  │  (VF01/02)   │      │  Generator   │     │ Interface │  │
│  └──────────────┘      └──────────────┘     └─────┬─────┘  │
│                              │                     │        │
│                              ▼                     │        │
│                    ┌──────────────────┐            │        │
│                    │ Certificate Mgmt │            │        │
│                    │      (SSF)       │            │        │
│                    └──────────────────┘            │        │
└────────────────────────────────────────────────────┼────────┘
                                                     │
                                                     ▼
                                            ┌─────────────────┐
                                            │   PAC Provider  │
                                            │  (Montova/      │
                                            │   Edicom)       │
                                            └────────┬────────┘
                                                     │
                                                     ▼
                                            ┌─────────────────┐
                                            │       SAT       │
                                            │  (Tax Authority)│
                                            └─────────────────┘
```

### Key Components
- **CFDI Generator**: Core XML generation engine
- **PAC Interface**: Multi-provider integration layer
- **Certificate Management**: Secure certificate handling via SSF
- **Validation Engine**: Pre-submission validation
- **Error Handler**: Comprehensive error management

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### How to Contribute
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Code Standards
- Follow SAP ABAP naming conventions
- Include unit tests for new functionality
- Update documentation for changes
- Add examples for new scenarios

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 💬 Support

### Getting Help
- **Documentation**: Check the [docs](docs/) folder
- **Issues**: Report bugs via [GitHub Issues](../../issues)
- **Discussions**: Ask questions in [GitHub Discussions](../../discussions)

### Community
- Share your implementation experiences
- Contribute new scenarios
- Report bugs and suggest features
- Help others in the community

## 🔗 Useful Links

- [SAT Official Portal](http://www.sat.gob.mx/)
- [CFDI 4.0 Technical Documentation (Spanish)](http://omawww.sat.gob.mx/tramitesyservicios/Paginas/documentos/Anexo_20_Guia_de_llenado_CFDI.pdf)
- [SAT Catalogs](http://www.sat.gob.mx/consultas/92764/comprobante-fiscal-digital-por-internet)
- [SAP Community - Mexico Localization](https://community.sap.com/)

## 📊 Version History

See [CHANGELOG.md](CHANGELOG.md) for detailed version history.

---

**Current Version**: 1.0.0
**Last Updated**: December 2024
**Status**: Production Ready

---

Made with ❤️ for the SAP Mexico community
