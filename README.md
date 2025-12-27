# SAP CFDI 4.0 para México - Integración con Montova y Edicom

[![SAP](https://img.shields.io/badge/SAP-S%2F4HANA-blue)](https://www.sap.com/)
[![CFDI](https://img.shields.io/badge/CFDI-4.0-green)](http://www.sat.gob.mx/)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

> Documentación técnica completa para la implementación de Facturación Electrónica CFDI 4.0 en SAP S/4HANA con integración a PAC (Montova y Edicom).

## 📋 Contenido

- [Resumen](#resumen)
- [Arquitectura](#arquitectura)
- [Requisitos](#requisitos)
- [Inicio Rápido](#inicio-rápido)
- [Estructura del Proyecto](#estructura-del-proyecto)
- [Proveedores PAC Soportados](#proveedores-pac-soportados)
- [Características](#características)

## 🔍 Resumen

Este repositorio contiene la documentación técnica y guías de implementación para:

- **Generación de CFDI 4.0** en SAP S/4HANA
- **Integración con PAC** (Proveedor Autorizado de Certificación):
  - Montova
  - Edicom
- **Módulo SD** (Facturación de Ventas)
- **Cumplimiento SAT** (Servicio de Administración Tributaria)
- **Timbrado fiscal** automático
- **Cancelación de CFDI**
- **Almacenamiento legal** (5 años)

## 🏗️ Arquitectura

### Flujo General CFDI

```
┌─────────────────────────────────────────────────────────┐
│                SAP S/4HANA (On-Premise)                 │
│                                                         │
│  ┌──────────────┐     ┌──────────────────────────┐    │
│  │  SD Billing  │ --> │  CFDI XML Generation     │    │
│  │  (VBRK/VBRP) │     │  - RFC Validation        │    │
│  │              │     │  - SAT Catalogs          │    │
│  └──────────────┘     │  - CFDI 4.0 Structure    │    │
│                       └───────────┬──────────────┘    │
│                                   │                    │
│                       ┌───────────▼──────────────┐    │
│                       │  PAC Interface Connector │    │
│                       │  - Montova Adapter       │    │
│                       │  - Edicom Adapter        │    │
│                       └───────────┬──────────────┘    │
└───────────────────────────────────┼──────────────────┘
                                    │
                    ┌───────────────┴───────────────┐
                    │                               │
            ┌───────▼────────┐          ┌──────────▼────────┐
            │  Montova PAC   │          │   Edicom PAC      │
            │  - Validation  │          │   - Validation    │
            │  - Signature   │          │   - Signature     │
            │  - SAT Submit  │          │   - SAT Submit    │
            └───────┬────────┘          └──────────┬────────┘
                    │                               │
                    └───────────────┬───────────────┘
                                    │
                            ┌───────▼────────┐
                            │      SAT       │
                            │  (Tax Auth)    │
                            │  Returns UUID  │
                            └───────┬────────┘
                                    │
                    ┌───────────────┴───────────────┐
                    │                               │
            ┌───────▼────────┐          ┌──────────▼────────┐
            │  Montova       │          │   Edicom          │
            │  Returns:      │          │   Returns:        │
            │  - UUID        │          │   - UUID          │
            │  - Timbre      │          │   - Timbre        │
            │  - PDF         │          │   - PDF           │
            └───────┬────────┘          └──────────┬────────┘
                    │                               │
                    └───────────────┬───────────────┘
                                    │
                            ┌───────▼────────┐
                            │   SAP S/4HANA  │
                            │   Store UUID   │
                            │   Update Status│
                            └────────────────┘
```

## ✅ Requisitos

### Sistema SAP

- **SAP S/4HANA On-Premise** o **SAP ECC 6.0** con EHP7+
- **Módulo SD** activado (Facturación de Ventas)
- **Localización México** instalada (recomendado)
- Acceso para desarrollo ABAP

### Requisitos Fiscales México

- **RFC** (Registro Federal de Contribuyentes) activo
- **Certificados SAT**:
  - CSD (Certificado de Sello Digital) - vigente
  - FIEL (Firma Electrónica) - para cancelaciones
- **Contrato con PAC** (Montova o Edicom)
- **Ambiente de pruebas SAT** acceso

### Conocimientos Técnicos

- ABAP Object-Oriented Programming
- XML/XSLT transformations
- Web Services (SOAP/REST)
- Estructura CFDI 4.0
- SAT Catálogos

## 🚀 Inicio Rápido

### 1. Clonar Repositorio

```bash
git clone https://github.com/tu-usuario/sap-cfdi-mexico.git
cd sap-cfdi-mexico
```

### 2. Revisar Documentación

- [Introducción a CFDI 4.0](docs/01-cfdi-overview/cfdi-introduction.md)
- [Arquitectura del Sistema](docs/02-architecture/overview.md)
- [Desarrollo ABAP](docs/03-abap-development/cfdi-generation.md)

### 3. Configurar PAC

**Opción A: Montova**
- Ver [Guía Montova](docs/04-pac-integration/montova-setup.md)
- Configurar credenciales y endpoints

**Opción B: Edicom**
- Ver [Guía Edicom](docs/04-pac-integration/edicom-setup.md)
- Instalar add-on (si aplica)

### 4. Implementar Código ABAP

```abap
" Ejemplo básico de uso
DATA(lo_cfdi) = NEW zcl_cfdi_generator( ).

lo_cfdi->generate_from_billing(
  EXPORTING
    iv_vbeln = '0090000123'  " Billing document
  IMPORTING
    ev_xml   = DATA(lv_cfdi_xml)
    ev_uuid  = DATA(lv_uuid)
).
```

### 5. Probar en Ambiente SAT

- Usar ambiente de pruebas SAT
- Certificar con PAC
- Validar timbrado

## 📁 Estructura del Proyecto

```
sap-cfdi-mexico/
├── README.md
├── docs/
│   ├── 01-cfdi-overview/
│   │   ├── cfdi-introduction.md        # Introducción CFDI 4.0
│   │   ├── legal-requirements.md       # Requisitos legales SAT
│   │   └── sat-catalogs.md             # Catálogos SAT
│   │
│   ├── 02-architecture/
│   │   ├── overview.md                 # Arquitectura general
│   │   ├── data-flow.md                # Flujo de datos
│   │   └── integration-patterns.md     # Patrones de integración
│   │
│   ├── 03-abap-development/
│   │   ├── cfdi-generation.md          # Generación CFDI XML
│   │   ├── database-design.md          # Tablas Z personalizadas
│   │   ├── rfc-validation.md           # Validación RFC
│   │   └── class-reference.md          # Referencia de clases
│   │
│   ├── 04-pac-integration/
│   │   ├── montova-setup.md            # Configuración Montova
│   │   ├── montova-api.md              # API Reference Montova
│   │   ├── edicom-setup.md             # Configuración Edicom
│   │   ├── edicom-api.md               # API Reference Edicom
│   │   └── comparison.md               # Comparación PACs
│   │
│   ├── 05-sat-compliance/
│   │   ├── cfdi-4.0-structure.md       # Estructura CFDI 4.0
│   │   ├── digital-certificates.md     # CSD/FIEL management
│   │   ├── cancellation.md             # Proceso de cancelación
│   │   └── validation-rules.md         # Reglas de validación SAT
│   │
│   ├── 06-sd-billing-integration/
│   │   ├── billing-document-flow.md    # Flujo SD billing
│   │   ├── pricing-integration.md      # Integración con pricing
│   │   ├── tax-calculation.md          # Cálculo de impuestos
│   │   └── output-determination.md     # Output type config
│   │
│   ├── 07-testing/
│   │   ├── unit-testing.md             # Pruebas unitarias
│   │   ├── integration-testing.md      # Pruebas de integración
│   │   ├── sat-certification.md        # Proceso de certificación
│   │   └── test-scenarios.md           # Escenarios de prueba
│   │
│   └── 08-deployment/
│       ├── prerequisites.md            # Pre-requisitos
│       ├── installation.md             # Instalación paso a paso
│       ├── configuration.md            # Configuración SAP
│       └── go-live.md                  # Plan de arranque
│
├── src/
│   ├── abap/
│   │   ├── classes/
│   │   │   ├── ZCL_CFDI_GENERATOR.abap
│   │   │   ├── ZCL_PAC_MONTOVA.abap
│   │   │   ├── ZCL_PAC_EDICOM.abap
│   │   │   ├── ZCL_RFC_VALIDATOR.abap
│   │   │   └── ZCL_SAT_CATALOGS.abap
│   │   │
│   │   ├── tables/
│   │   │   ├── ZCFDI_HEADER.abap      # Cabecera CFDI
│   │   │   ├── ZCFDI_ITEMS.abap       # Conceptos CFDI
│   │   │   └── ZCFDI_STATUS.abap      # Estado timbrado
│   │   │
│   │   └── functions/
│   │       └── Z_CFDI_GENERATE.abap
│   │
│   ├── cfdi-templates/
│   │   ├── cfdi-4.0-template.xml      # Plantilla CFDI 4.0
│   │   ├── cfdi-cancel-template.xml   # Plantilla cancelación
│   │   └── complementos/              # Complementos (Pago, etc)
│   │
│   └── pac-integration/
│       ├── montova/
│       │   ├── montova-wsdl.xml
│       │   ├── montova-api-spec.md
│       │   └── examples/
│       │
│       └── edicom/
│           ├── edicom-wsdl.xml
│           ├── edicom-api-spec.md
│           └── examples/
│
├── tests/
│   ├── unit/
│   │   └── test-cases.md
│   ├── integration/
│   │   └── integration-scenarios.md
│   └── sat-certification/
│       └── certification-checklist.md
│
└── config/
    ├── sat-catalogs/
    │   ├── c_FormaPago.json           # Formas de pago
    │   ├── c_MetodoPago.json          # Métodos de pago
    │   ├── c_UsoCFDI.json             # Uso de CFDI
    │   ├── c_RegimenFiscal.json       # Régimen fiscal
    │   └── c_ClaveUnidad.json         # Claves de unidad
    │
    ├── certificates/
    │   └── README.md                   # Gestión de certificados
    │
    └── pac-configs/
        ├── montova-config.json
        └── edicom-config.json
```

## 🔌 Proveedores PAC Soportados

### Montova

**Características:**
- ✅ API REST y SOAP
- ✅ Timbrado en tiempo real
- ✅ Portal web para consultas
- ✅ Almacenamiento 5 años
- ✅ Soporte técnico 24/7

**Integración:**
```abap
DATA(lo_pac) = NEW zcl_pac_montova(
  iv_usuario = 'your_user'
  iv_password = 'your_password'
  iv_ambiente = 'PRODUCCION'  " o 'PRUEBAS'
).

DATA(lv_uuid) = lo_pac->timbrar_cfdi(
  iv_cfdi_xml = lv_cfdi_xml
).
```

**Documentación:** [Montova Setup Guide](docs/04-pac-integration/montova-setup.md)

### Edicom

**Características:**
- ✅ Add-on SAP disponible
- ✅ Integración IDOC
- ✅ Web Services SOAP
- ✅ Gestión de certificados
- ✅ Dashboard de monitoreo

**Integración:**
```abap
DATA(lo_pac) = NEW zcl_pac_edicom(
  iv_client_id = 'your_client'
  iv_api_key = 'your_key'
  iv_endpoint = 'https://mx.edicomgroup.com/cfdi'
).

DATA(lv_uuid) = lo_pac->timbrar_cfdi(
  iv_cfdi_xml = lv_cfdi_xml
).
```

**Documentación:** [Edicom Setup Guide](docs/04-pac-integration/edicom-setup.md)

### Comparación PACs

| Característica | Montova | Edicom |
|----------------|---------|--------|
| **Integración SAP** | Web Service | Add-on + Web Service |
| **API Type** | REST/SOAP | SOAP/IDOC |
| **Facilidad Setup** | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Documentación** | Excelente | Muy Buena |
| **Soporte** | 24/7 | Business Hours |
| **Portal Web** | ✅ Completo | ✅ Completo |
| **Costo** | Competitivo | Premium |
| **Recomendado Para** | Implementaciones nuevas | Empresas grandes |

Ver [Comparación Detallada](docs/04-pac-integration/comparison.md)

## ✨ Características Principales

### CFDI 4.0 Completo

- ✅ **Facturas de Ingreso** (tipo "I")
- ✅ **Notas de Crédito** (tipo "E")
- ✅ **Recibos de Pago** (tipo "P")
- ✅ **Traslado** (tipo "T")
- ✅ **Nómina** (tipo "N")

### Validaciones SAT

- ✅ **RFC** - Validación formato y lista negra SAT
- ✅ **Catálogos SAT** - Validación contra catálogos oficiales
- ✅ **Reglas de Negocio** - 100+ validaciones SAT
- ✅ **Montos y Totales** - Validación aritmética

### Integración SD

- ✅ **Billing Documents** automático
- ✅ **Pricing** integration
- ✅ **Tax Calculation** (IVA, IEPS, ISR)
- ✅ **Output Type** configuration
- ✅ **Partner Functions** mapping

### Seguridad

- ✅ **CSD Management** - Certificados de Sello Digital
- ✅ **Digital Signature** - Sellado CFDI
- ✅ **Audit Trail** - Trazabilidad completa
- ✅ **Error Logging** - Registro de errores

### Monitoreo

- ✅ **Dashboard** - Estado de facturación
- ✅ **Reports** - Reportes SAP
- ✅ **Alerts** - Notificaciones de errores
- ✅ **Statistics** - KPIs de timbrado

## 📚 Documentación Adicional

### Guías de Inicio

1. [¿Qué es CFDI 4.0?](docs/01-cfdi-overview/cfdi-introduction.md)
2. [Requisitos Legales SAT](docs/01-cfdi-overview/legal-requirements.md)
3. [Arquitectura del Sistema](docs/02-architecture/overview.md)

### Desarrollo

1. [Generación de CFDI XML](docs/03-abap-development/cfdi-generation.md)
2. [Validación de RFC](docs/03-abap-development/rfc-validation.md)
3. [Diseño de Base de Datos](docs/03-abap-development/database-design.md)

### Integración PAC

1. [Configuración Montova](docs/04-pac-integration/montova-setup.md)
2. [Configuración Edicom](docs/04-pac-integration/edicom-setup.md)
3. [Comparación de PACs](docs/04-pac-integration/comparison.md)

### Cumplimiento SAT

1. [Estructura CFDI 4.0](docs/05-sat-compliance/cfdi-4.0-structure.md)
2. [Certificados Digitales](docs/05-sat-compliance/digital-certificates.md)
3. [Proceso de Cancelación](docs/05-sat-compliance/cancellation.md)

## 🤝 Contribuir

Las contribuciones son bienvenidas! Por favor:

1. Fork el repositorio
2. Crear branch de feature (`git checkout -b feature/nueva-funcionalidad`)
3. Commit cambios (`git commit -m 'Agregar nueva funcionalidad'`)
4. Push al branch (`git push origin feature/nueva-funcionalidad`)
5. Abrir Pull Request

## 📄 Licencia

Este proyecto está licenciado bajo MIT License - ver [LICENSE](LICENSE) para detalles.

## 📞 Soporte

- **Issues**: [GitHub Issues](https://github.com/tu-usuario/sap-cfdi-mexico/issues)
- **Discussions**: [GitHub Discussions](https://github.com/tu-usuario/sap-cfdi-mexico/discussions)

## 🗺️ Roadmap

### Versión 1.0 (Actual)
- ✅ Generación CFDI 4.0
- ✅ Integración Montova
- ✅ Integración Edicom
- ✅ Facturación SD
- ✅ Validaciones SAT

### Versión 2.0 (Planeado)
- 🔄 Complemento de Pago
- 🔄 Carta Porte 3.0
- 🔄 Nómina 1.2
- 🔄 Comercio Exterior
- 🔄 Dashboard Fiori

---

**Nota**: Esta documentación está basada en CFDI 4.0 vigente y requisitos SAT actuales. Siempre verificar con el SAT los requisitos más recientes.

Última Actualización: Diciembre 2024
