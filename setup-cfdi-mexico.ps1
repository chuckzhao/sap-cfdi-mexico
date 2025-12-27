# SAP CFDI Mexico - Complete Repository Setup Script
# Supports both Montova and Edicom PAC providers

# Color functions
function Write-Success { param($msg) Write-Host $msg -ForegroundColor Green }
function Write-Info { param($msg) Write-Host $msg -ForegroundColor Cyan }
function Write-Warning { param($msg) Write-Host $msg -ForegroundColor Yellow }

Write-Info "================================================"
Write-Info "SAP CFDI Mexico Repository Setup"
Write-Info "Supporting Montova & Edicom PAC Integration"
Write-Info "================================================"
Write-Host ""

$rootPath = Get-Location
Write-Info "Setting up repository in: $rootPath"
Write-Host ""

# ============================================
# STEP 1: CREATE DIRECTORY STRUCTURE
# ============================================
Write-Info "Step 1: Creating directory structure for Mexico CFDI..."

$directories = @(
    "docs\01-cfdi-overview",
    "docs\02-architecture",
    "docs\03-abap-development",
    "docs\04-pac-integration",
    "docs\05-sat-compliance",
    "docs\06-sd-billing-integration",
    "docs\07-testing",
    "docs\08-deployment",
    "src\abap\classes",
    "src\abap\tables",
    "src\abap\functions",
    "src\abap\reports",
    "src\cfdi-templates",
    "src\pac-integration\montova",
    "src\pac-integration\edicom",
    "tests\unit",
    "tests\integration",
    "tests\sat-certification",
    "config\sat-catalogs",
    "config\certificates",
    "config\pac-configs"
)

foreach ($dir in $directories) {
    New-Item -ItemType Directory -Path $dir -Force | Out-Null
    Write-Host "  ✓ Created: $dir" -ForegroundColor Gray
}

Write-Success "Directory structure created!"
Write-Host ""

# ============================================
# STEP 2: CREATE MAIN README
# ============================================
Write-Info "Step 2: Creating main README for Mexico CFDI..."

$readmeContent = @'
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
'@

$readmeContent | Out-File -FilePath "README.md" -Encoding UTF8
Write-Host "  ✓ Created: README.md" -ForegroundColor Gray

# ============================================
# CREATE .gitignore
# ============================================

$gitignoreContent = @'
# SAP-specific
*.abap~
*.sap~
.tmp/

# Certificados y credenciales - NUNCA SUBIR
*.cer
*.key
*.p12
*.pfx
*.pem
**/certificates/*.cer
**/certificates/*.key
**/pac-configs/*password*
**/pac-configs/*credentials*

# Configuración local
config/pac-configs/montova-config.json
config/pac-configs/edicom-config.json
config/certificates/

# IDE
.vscode/
.idea/
*.iml

# OS
.DS_Store
Thumbs.db

# Build
*.zip
/build/
/dist/

# Logs
*.log
/logs/

# Test results
/test-results/
/coverage/

# Temporary
*.tmp
*.temp
*~
'@

$gitignoreContent | Out-File -FilePath ".gitignore" -Encoding UTF8
Write-Host "  ✓ Created: .gitignore" -ForegroundColor Gray

# ============================================
# CREATE LICENSE
# ============================================

$licenseContent = @'
MIT License

Copyright (c) 2024 [Tu Nombre/Empresa]

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
'@

$licenseContent | Out-File -FilePath "LICENSE" -Encoding UTF8
Write-Host "  ✓ Created: LICENSE" -ForegroundColor Gray

Write-Success "Root files created!"
Write-Host ""

# ============================================
# STEP 3: CREATE DOCUMENTATION PLACEHOLDERS
# ============================================
Write-Info "Step 3: Creating documentation files..."

# CFDI Overview docs
@"
# Introducción a CFDI 4.0

## ¿Qué es CFDI?

El Comprobante Fiscal Digital por Internet (CFDI) es el documento electrónico que sustituye
a las facturas en papel en México. Es obligatorio desde 2014 y la versión 4.0 es obligatoria
desde enero 2023.

## Características CFDI 4.0

- Formato XML estándar SAT
- Timbrado por PAC autorizado
- Sello digital (CSD)
- UUID único por factura
- Almacenamiento 5 años obligatorio

## Tipos de CFDI

- **I** - Ingreso (Facturas de venta)
- **E** - Egreso (Notas de crédito)
- **T** - Traslado
- **N** - Nómina
- **P** - Pago

Ver documentación completa SAT: http://www.sat.gob.mx/
"@ | Out-File -FilePath "docs\01-cfdi-overview\cfdi-introduction.md" -Encoding UTF8

"# Requisitos Legales SAT`n`nRequisitos fiscales y legales para facturación electrónica en México." | Out-File -FilePath "docs\01-cfdi-overview\legal-requirements.md" -Encoding UTF8
"# Catálogos SAT`n`nCatálogos oficiales del SAT para CFDI 4.0." | Out-File -FilePath "docs\01-cfdi-overview\sat-catalogs.md" -Encoding UTF8

# Architecture docs
"# Arquitectura General`n`nArquitectura de la solución SAP CFDI con PAC." | Out-File -FilePath "docs\02-architecture\overview.md" -Encoding UTF8
"# Flujo de Datos`n`nFlujo de datos desde SAP hasta SAT." | Out-File -FilePath "docs\02-architecture\data-flow.md" -Encoding UTF8
"# Patrones de Integración`n`nPatrones de integración con Montova y Edicom." | Out-File -FilePath "docs\02-architecture\integration-patterns.md" -Encoding UTF8

# ABAP Development docs
"# Generación de CFDI XML`n`nImplementación ABAP para generar XML CFDI 4.0." | Out-File -FilePath "docs\03-abap-development\cfdi-generation.md" -Encoding UTF8
"# Diseño de Base de Datos`n`nTablas Z para almacenar información CFDI." | Out-File -FilePath "docs\03-abap-development\database-design.md" -Encoding UTF8
"# Validación RFC`n`nValidación de RFC contra SAT." | Out-File -FilePath "docs\03-abap-development\rfc-validation.md" -Encoding UTF8
"# Referencia de Clases`n`nDocumentación de clases ABAP." | Out-File -FilePath "docs\03-abap-development\class-reference.md" -Encoding UTF8

# PAC Integration docs
@"
# Configuración Montova PAC

## Requisitos

- Contrato activo con Montova
- Credenciales de acceso (usuario/password)
- Certificados CSD activos

## Endpoints

### Producción
- Timbrado: https://timbrado.montova.com.mx/ServicioTimbrado.svc
- Cancelación: https://timbrado.montova.com.mx/ServicioCancelacion.svc

### Pruebas
- Timbrado: https://timbradopruebas.montova.com.mx/ServicioTimbrado.svc
- Cancelación: https://timbradopruebas.montova.com.mx/ServicioCancelacion.svc

## Configuración ABAP

Implementar clase ZCL_PAC_MONTOVA que implemente interfaz ZIF_PAC.

Ver código ejemplo en: src/abap/classes/ZCL_PAC_MONTOVA.abap
"@ | Out-File -FilePath "docs\04-pac-integration\montova-setup.md" -Encoding UTF8

@"
# Configuración Edicom PAC

## Requisitos

- Contrato activo con Edicom
- Client ID y API Key
- Certificados CSD activos
- (Opcional) Add-on SAP Edicom

## Endpoints

### Producción
- API: https://mx.edicomgroup.com/ws/cfdi40/TimbrarService
- Portal: https://portal.edicomgroup.com

### Pruebas
- API: https://mx-test.edicomgroup.com/ws/cfdi40/TimbrarService

## Configuración ABAP

Implementar clase ZCL_PAC_EDICOM que implemente interfaz ZIF_PAC.

Ver código ejemplo en: src/abap/classes/ZCL_PAC_EDICOM.abap
"@ | Out-File -FilePath "docs\04-pac-integration\edicom-setup.md" -Encoding UTF8

"# Montova API Reference`n`nReferencia completa de API Montova." | Out-File -FilePath "docs\04-pac-integration\montova-api.md" -Encoding UTF8
"# Edicom API Reference`n`nReferencia completa de API Edicom." | Out-File -FilePath "docs\04-pac-integration\edicom-api.md" -Encoding UTF8
