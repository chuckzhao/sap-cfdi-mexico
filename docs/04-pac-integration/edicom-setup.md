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
