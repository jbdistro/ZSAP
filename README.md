# SAP Role Management Programs

This repository contains ABAP reports used to analyze la relacion entre usuarios, roles y transacciones.  Todos los textos de las interfaces estan en español.

## Programas

- **Z_INTEGRATED_USER_ROLE_MANAGEMENT**: Reporte principal que permite ver relaciones de Rol/Usuario, Rol/Transaccion y Transaccion/Autorizacion.
- **Z_ROLE_USER_TRANS**: Nuevo reporte que vincula usuarios, roles y transacciones.
- **Z_ROLE_USER_TRANS_AUTH**: Nuevo reporte que extiende el anterior incluyendo objetos de autorizacion.
- **Z_MM60_ULTIMA_COMPRA**: Reporte independiente de MM60.

## Extracción de niveles de uso

El script `parse_usage_excel.py` genera `usage_mapping.json` leyendo `PCE_RISE_V1.69.XLSX`. Este archivo mapea el nivel (GD, GB o GC) para cada combinacion de Objeto, Campo y Valor de autorizacion.

```bash
python3 parse_usage_excel.py
```

Se requiere `json` y librerías estándar de Python; no se necesitan dependencias externas.
