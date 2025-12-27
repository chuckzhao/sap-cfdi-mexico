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
