REPORT Z_FUES.
*---------------------------------------------------------------------*
* Report  Z_FUES - Roles, Perfiles y Autorizaciones por Usuario SAP    *
*---------------------------------------------------------------------*
* Autor           : Juan Basañes Cantoni                               *
* Empresa         : Distrocuyo                                         *
* Fecha creación  : 31/07/2025                                         *
* Transport       : <TR‑número>                                        *
* Paquete         : <Paquete de desarrollo Z>                          *
* Versión         : 1.0                                                *
*---------------------------------------------------------------------*
* Propósito       : Proporcionar un análisis consolidado de:           *
*                  • Asignaciones Usuario‑Rol                          *
*                  • Roles y sus transacciones                         *
*                  • Autorizaciones por transacción (objetos y campos) *
*                  • Transacciones disponibles por usuario             *
*                  • Objetos de autorización por usuario               *
*                  • Perfil por usuario (vista ampliada)               *
*---------------------------------------------------------------------*
* Historial de cambios :                                               *
* 1.0  31/07/2025  Juan Basañes           – Versión inicial completa    *
*---------------------------------------------------------------------*
* Tablas utilizadas : AGR_DEFINE, AGR_USERS, USR02, AGR_TCODE, AGR_1251, *
*                     UST04, UST10C                                   *
*---------------------------------------------------------------------*
* Descripción detallada :                                              *
* Este report utiliza SALV Table para presentar vistas ALV con resumen  *
* y detalle, filtrando por roles, usuarios, grupos, objetos o perfiles. *
*---------------------------------------------------------------------*
* Observaciones :                                                      *
* • No modifica datos del sistema SAP.                                 *
* • Incluye filtros de inactividad de usuario/rol.                      *
* • Cumple estándares ABAP: comentarios claros, nombres descriptivos,   *
*   control de excepciones, legibilidad.                               *
*---------------------------------------------------------------------*
* Recomendaciones técnicas :                                           *
* • Compatible con ATC, Code Inspector y transporte CTS.               *
* • Se sugiere documentar también la transacción asociada (SE93)        *
*   y completar la documentación F1 interna vía SE80 → Documentación.   *
*---------------------------------------------------------------------*

*========================================================================*
* Declaración de estructuras, tablas internas y parámetros del report (1)*
*========================================================================*

*--- Tablas base utilizadas en las consultas
TABLES: agr_define,      " Definición de roles
        agr_users,       " Usuarios asignados a roles
        usr02,           " Datos maestros de usuarios
        agr_tcodes,      " Transacciones por rol
        agr_1251,        " Objetos de autorización por rol
        ust04,           " Perfiles asignados a usuarios
        usr11.          " Texto descriptivo de perfiles

*--- Estructura: Relación Usuario ↔ Rol
TYPES: BEGIN OF ty_user_role,
         role_name      TYPE agr_define-agr_name,
         user_id        TYPE xubname,
         user_group     TYPE usr02-class,
         from_date      TYPE datum,
         to_date        TYPE datum,
         role_inactive  TYPE c LENGTH 1,
         roles_per_user TYPE i,
         users_per_role TYPE i,
         user_inactive  TYPE c LENGTH 1,
       END OF ty_user_role.

*--- Estructura: Relación Usuario ↔ Transacción
TYPES: BEGIN OF ty_user_tcode,
         user_id     TYPE xubname,
         user_group  TYPE usr02-class,
         role_name   TYPE agr_define-agr_name,
         transaction TYPE tcode,
         description TYPE tstct-ttext,
         fues_level  TYPE char15,
       END OF ty_user_tcode.

*--- Estructura: Relación Usuario ↔ Objeto de autorización
TYPES: BEGIN OF ty_user_object,
         user_id     TYPE xubname,
         user_group  TYPE usr02-class,
         role_name   TYPE agr_define-agr_name,
         auth_object TYPE agr_1251-object,
         auth_field  TYPE agr_1251-field,
         auth_value  TYPE agr_1251-low,
         fues_level  TYPE char15,
       END OF ty_user_object.

*--- Estructura: Relación Usuario ↔ Perfil
TYPES: BEGIN OF ty_user_profile,
         user_id      TYPE xubname,
         user_group   TYPE usr02-class,
         profile      TYPE ust04-profile,
         profile_text TYPE usr11-ptext,
       END OF ty_user_profile.

*--- Estructura: Relación Rol ↔ Transacción
TYPES: BEGIN OF ty_role_transaction,
         role_name   TYPE agr_define-agr_name,
         transaction TYPE tcode,
         description TYPE tstct-ttext,
         fues_level  TYPE char15,
       END OF ty_role_transaction.

*--- Estructura: Relación Transacción ↔ Autorización
TYPES: BEGIN OF ty_transaction_auth,
         transaction TYPE tcode,
         role_name   TYPE agr_define-agr_name,
         description TYPE tstct-ttext,
         auth_object TYPE agr_1251-object,
         auth_field  TYPE agr_1251-field,
         auth_value  TYPE agr_1251-low,
         fues_level  TYPE char15,
       END OF ty_transaction_auth.

*--- Estructura: Mapeo Transacción ↔ Nivel FUES
TYPES: BEGIN OF ty_tcode_fues,
         transaction TYPE tcode,
         fues_level  TYPE char15,
       END OF ty_tcode_fues.

*--- Estructura: Mapeo Objeto ↔ Nivel FUES
TYPES: BEGIN OF ty_auth_fues,
         auth_object TYPE agr_1251-object,
         auth_field  TYPE agr_1251-field,
         auth_value  TYPE agr_1251-low,
         fues_level  TYPE char15,
       END OF ty_auth_fues.

*--- Estructura: Nivel FUES por Rol
TYPES: BEGIN OF ty_role_fues,
         role_name  TYPE agr_define-agr_name,
         fues_level TYPE char15,
         adv_ratio  TYPE p DECIMALS 2,
         core_ratio TYPE p DECIMALS 2,
         users_active TYPE i,
         users_total  TYPE i,
       END OF ty_role_fues.

*--- Estructura: Vista básica de Usuario con nivel FUES
TYPES: BEGIN OF ty_user_basic,
         user_id    TYPE xubname,
         user_group TYPE usr02-class,
         inactive   TYPE c LENGTH 1,
         locked     TYPE c LENGTH 1,
         valid_from TYPE datum,
         valid_to   TYPE datum,
         roles_total TYPE i,
         roles_active TYPE i,
         fues_level TYPE char15,
       END OF ty_user_basic.

*--- Estructura: Resumen genérico de conteo (usuarios, roles, etc.)
TYPES: BEGIN OF ty_summary,
         description TYPE string,
         value       TYPE string,
       END OF ty_summary.

*--- Declaración de tablas internas y objeto ALV
DATA: gt_user_role         TYPE STANDARD TABLE OF ty_user_role,
      gt_role_transaction  TYPE STANDARD TABLE OF ty_role_transaction,
      gt_transaction_auth  TYPE STANDARD TABLE OF ty_transaction_auth,
      gt_user_tcode        TYPE STANDARD TABLE OF ty_user_tcode,
      gt_user_object       TYPE STANDARD TABLE OF ty_user_object,
      gt_user_profile      TYPE STANDARD TABLE OF ty_user_profile,
      gt_user_basic        TYPE STANDARD TABLE OF ty_user_basic,
      gt_fues_tcode        TYPE SORTED TABLE  OF ty_tcode_fues WITH UNIQUE KEY transaction,
      gt_fues_auth         TYPE HASHED TABLE  OF ty_auth_fues WITH UNIQUE KEY auth_object auth_field auth_value,
      gt_fues_role         TYPE STANDARD TABLE OF ty_role_fues,
      gt_summary           TYPE STANDARD TABLE OF ty_summary,
      lo_alv               TYPE REF TO cl_salv_table,
      gv_fues_enabled      TYPE abap_bool VALUE abap_false.


*========================================================================*
* Pantalla de selección: vistas, filtros, flags(2) *
*========================================================================*

*--- Bloque 1: Selección de vista
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-b01. " Selección de vista

  PARAMETERS rb_user   RADIOBUTTON GROUP rb1 DEFAULT 'X'.  " Vista: Rol-Usuario
  PARAMETERS rb_role   RADIOBUTTON GROUP rb1.              " Vista: Rol–Transacción
  PARAMETERS r_usr_tx  RADIOBUTTON GROUP rb1.              " Vista: Usuario–Transacción
  PARAMETERS r_usrobj  RADIOBUTTON GROUP rb1.              " Vista: Usuario–Objeto
  PARAMETERS r_uprof   RADIOBUTTON GROUP rb1.              " Vista: Usuario–Perfil
  PARAMETERS r_ufues RADIOBUTTON GROUP rb1.                " Vista: Usuarios (Nivel FUES)
  PARAMETERS r_rfues  RADIOBUTTON GROUP rb1.               " Vista: Roles (Nivel FUES)
  PARAMETERS rb_trans RADIOBUTTON GROUP rb1.               " Vista: Transacción–Autorización

SELECTION-SCREEN END OF BLOCK blk1.


*--- Bloque 2: Filtros por valores específicos
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-b02. " Filtros

  SELECT-OPTIONS:
    s_role   FOR agr_define-agr_name,     " Filtro por rol
    s_user   FOR usr02-bname,             " Filtro por usuario
    s_group  FOR usr02-class,             " Filtro por grupo
    s_tcode  FOR agr_tcodes-tcode,        " Filtro por transacción
    s_prof   FOR ust04-profile,           " Filtro por perfil
    s_object FOR agr_1251-object,         " Filtro por objeto de autorización
    s_fdate  FOR agr_users-from_dat,      " Filtro por fecha desde
    s_tdate  FOR agr_users-to_dat.        " Filtro por fecha hasta

  PARAMETERS p_file TYPE rlgrap-filename. " Archivo de niveles FUES

SELECTION-SCREEN END OF BLOCK blk2.


*--- Bloque 3: Parámetros de control booleanos
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE TEXT-b03. " Opciones

  PARAMETERS p_inact  AS CHECKBOX DEFAULT ' '.  " Incluir usuarios inactivos
  PARAMETERS p_rnousr AS CHECKBOX DEFAULT ' '.  " Mostrar roles sin usuarios
  PARAMETERS p_unorol AS CHECKBOX DEFAULT ' '.  " Mostrar usuarios sin roles

SELECTION-SCREEN END OF BLOCK blk3.

*======================================================================*
* Lógica principal: Dispatcher de vistas según opción seleccionada (3) *
*======================================================================*
START-OF-SELECTION.
  IF rb_role = 'X' OR r_usr_tx = 'X' OR r_usrobj = 'X' OR r_ufues = 'X' OR r_rfues = 'X' OR rb_trans = 'X'.
    PERFORM load_fues_data.
  ENDIF.
  CASE 'X'.
    WHEN rb_user.   PERFORM process_user_role_view.          " Vista Rol-Usuario
    WHEN rb_role.   PERFORM process_role_transaction_view.   " Vista Rol-Transacción
    WHEN r_usr_tx.  PERFORM process_user_tcode_view.         " Vista Usuario-Transacción
    WHEN r_usrobj.  PERFORM process_user_object_view.        " Vista Usuario-Objeto
    WHEN r_uprof.   PERFORM process_user_profile_view.       " Vista Usuario-Perfil
    WHEN r_ufues.   PERFORM process_user_fues_view.          " Vista Usuarios (Nivel FUES)
    WHEN r_rfues.   PERFORM process_role_fues_view.          " Vista Roles (Nivel FUES)
    WHEN rb_trans.  PERFORM process_transaction_auth_view.   " Vista Transacción-Autorización
  ENDCASE.

*=====================================================================*
* Vista Usuario ↔ Rol (4.1)                                           *
*=====================================================================*
FORM process_user_role_view.
  PERFORM get_user_role_data.        " Selección de asignaciones activas Usuario ↔ Rol
  PERFORM add_roles_without_users.   " Añadir roles definidos que no tengan usuarios asignados
  PERFORM add_users_without_roles.   " Añadir usuarios sin asignaciones a ningún rol
  PERFORM calculate_counts.          " Calcular cantidad de roles por usuario y usuarios por rol
  PERFORM apply_user_role_filters.   " Filtrar resultados según flags de exclusión
  PERFORM build_user_role_summary.   " Construir resumen cuantitativo de la vista
  PERFORM display_user_role_alv.     " Mostrar datos en tabla ALV SALV
ENDFORM.

*=====================================================================*
* Cálculo de nivel FUES para cada rol                                  *
*=====================================================================*
FORM calculate_role_fues.
  CLEAR gt_fues_role.

  IF gt_transaction_auth IS INITIAL.
    PERFORM get_transaction_auth_data.
  ENDIF.
  IF gt_fues_tcode IS INITIAL.
    PERFORM calculate_transaction_fues.
  ENDIF.

  LOOP AT gt_role_transaction INTO DATA(ls_rt).
    DATA(lv_level) = 'No disponible'.

    LOOP AT gt_transaction_auth ASSIGNING FIELD-SYMBOL(<fs_ta>)
         WHERE role_name = ls_rt-role_name
           AND transaction = ls_rt-transaction.
      CASE <fs_ta>-fues_level.
        WHEN 'AVANZADO'.
          lv_level = 'AVANZADO'.
          EXIT.
        WHEN 'CORE'.
          IF lv_level <> 'AVANZADO'.
            lv_level = 'CORE'.
          ENDIF.
        WHEN 'SELF SERV'.
          IF lv_level = 'No disponible'.
            lv_level = 'SELF SERV'.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    IF lv_level = 'No disponible'.
      READ TABLE gt_fues_tcode ASSIGNING FIELD-SYMBOL(<fs_tx>)
           WITH KEY transaction = ls_rt-transaction BINARY SEARCH.
      IF sy-subrc = 0.
        lv_level = <fs_tx>-fues_level.
      ENDIF.
    ENDIF.

    READ TABLE gt_fues_role ASSIGNING FIELD-SYMBOL(<fs_role>)
         WITH KEY role_name = ls_rt-role_name BINARY SEARCH.
    IF sy-subrc <> 0.
      APPEND VALUE ty_role_fues( role_name = ls_rt-role_name
                                 fues_level = 'No disponible'
                                 users_active = 0
                                 users_total  = 0 ) TO gt_fues_role.
      SORT gt_fues_role BY role_name.
      READ TABLE gt_fues_role ASSIGNING <fs_role>
           WITH KEY role_name = ls_rt-role_name BINARY SEARCH.
    ENDIF.

    CASE lv_level.
      WHEN 'AVANZADO'.
        <fs_role>-fues_level = 'AVANZADO'.
        <fs_role>-adv_ratio = <fs_role>-adv_ratio + 1.
      WHEN 'CORE'.
        IF <fs_role>-fues_level <> 'AVANZADO'.
          <fs_role>-fues_level = 'CORE'.
        ENDIF.
        <fs_role>-core_ratio = <fs_role>-core_ratio + 1.
      WHEN 'SELF SERV'.
        IF <fs_role>-fues_level = 'No disponible'.
          <fs_role>-fues_level = 'SELF SERV'.
        ENDIF.
    ENDCASE.

    ls_rt-fues_level = lv_level.
    MODIFY gt_role_transaction FROM ls_rt INDEX sy-tabix.
  ENDLOOP.

  LOOP AT gt_fues_role ASSIGNING <fs_role>.
    DATA(lv_count) = REDUCE i( INIT c = 0 FOR wa IN gt_role_transaction WHERE ( role_name = <fs_role>-role_name ) NEXT c = c + 1 ).
    IF lv_count > 0.
      <fs_role>-adv_ratio = <fs_role>-adv_ratio / lv_count * 100.
      <fs_role>-core_ratio = <fs_role>-core_ratio / lv_count * 100.
    ENDIF.
  ENDLOOP.

  SORT gt_fues_role BY role_name.
ENDFORM.

*=====================================================================*
* Contar usuarios por rol                                             *
*=====================================================================*
FORM get_role_user_counts.
  SELECT r~agr_name AS role_name,
         r~uname    AS user_id,
         r~from_dat AS from_date,
         r~to_dat   AS to_date
    FROM agr_users AS r
    INNER JOIN usr02 AS u ON u~bname = r~uname
    WHERE r~agr_name IN @s_role
      AND r~uname    IN @s_user
      AND u~class    IN @s_group
      AND ( @p_inact = 'X' OR u~gltgv >= @sy-datum OR u~gltgv = '00000000' )
    INTO TABLE @DATA(lt_ru).

  SORT lt_ru BY role_name user_id.
  DELETE ADJACENT DUPLICATES FROM lt_ru COMPARING role_name user_id.

  LOOP AT lt_ru INTO DATA(ls_ru).
    READ TABLE gt_fues_role ASSIGNING FIELD-SYMBOL(<fs_rf>)
         WITH KEY role_name = ls_ru-role_name BINARY SEARCH.
    IF sy-subrc <> 0.
      APPEND VALUE ty_role_fues( role_name = ls_ru-role_name
                                 fues_level = 'No disponible'
                                 users_active = 0
                                 users_total  = 0 ) TO gt_fues_role.
      SORT gt_fues_role BY role_name.
      READ TABLE gt_fues_role ASSIGNING <fs_rf>
           WITH KEY role_name = ls_ru-role_name BINARY SEARCH.
    ENDIF.

    <fs_rf>-users_total = <fs_rf>-users_total + 1.
    IF ls_ru-from_date <= sy-datum AND ( ls_ru-to_dat = '00000000' OR ls_ru-to_dat >= sy-datum ).
      <fs_rf>-users_active = <fs_rf>-users_active + 1.
    ENDIF.
  ENDLOOP.

  SORT gt_fues_role BY role_name.
ENDFORM.

*=====================================================================*
* Cálculo de nivel FUES por usuario                                    *
*=====================================================================*
*=====================================================================*
* Carga de archivo Excel de clasificación FUES - SOLUTION 1 (Recommended)
*=====================================================================*
FORM load_fues_data.
  " Sin dependencias FDT. XLS con ALSM_EXCEL_TO_INTERNAL_TABLE, CSV/TSV/TXT con GUI_UPLOAD.
  DATA: lt_raw_data   TYPE TABLE OF string,
        lv_filename   TYPE string,
        lv_line       TYPE string,
        lt_fields     TYPE TABLE OF string,
        lv_rule       TYPE string,
        lv_level      TYPE char15,
        lv_extension  TYPE string,
        lt_parts      TYPE STANDARD TABLE OF string,
        lv_first_line TYPE abap_bool,
        lv_type       TYPE string,
        lv_step       TYPE string,
        lv_object     TYPE agr_1251-object,
        lv_field      TYPE agr_1251-field,
        lv_value      TYPE agr_1251-low.

  CLEAR: gt_fues_tcode, gt_fues_auth.
  gv_fues_enabled = abap_false.

  IF p_file IS INITIAL.
    MESSAGE 'No se cargaron datos FUES válidos. Se continúa sin mapa FUES.' TYPE 'I'.
    RETURN.  " FUES opcional
  ENDIF.

  lv_filename = p_file.

  " Eliminar comillas que puedan provenir de rutas copiadas
  REPLACE ALL OCCURRENCES OF '"' IN lv_filename WITH ''.

  " Determinar extensión
  SPLIT lv_filename AT '.' INTO TABLE lt_parts.
  READ TABLE lt_parts INDEX lines( lt_parts ) INTO lv_extension.
  TRANSLATE lv_extension TO LOWER CASE.

  " Solo se soporta CSV/TSV/TXT con columnas de autorización.
  IF lv_extension = 'xls' OR lv_extension = 'xlsx'.
    MESSAGE 'Utilice archivo CSV con columnas: Regla, Tipo, Objeto, Campo, Valor.' TYPE 'W'.
  ENDIF.

  " === Leer CSV/TSV/TXT y parsear ===
  TRY.
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename            = lv_filename
          filetype            = 'ASC'     " texto
          has_field_separator = space     " NO separar aquí
        TABLES
          data_tab            = lt_raw_data
        EXCEPTIONS
          OTHERS              = 1.
    CATCH cx_root INTO DATA(lx_txt).
      MESSAGE lx_txt->get_text( ) TYPE 'I'.
      sy-subrc = 1.
  ENDTRY.

  " Si la lectura vía GUI_UPLOAD falla, intentar leer desde el servidor de aplicaciones
  IF sy-subrc <> 0 OR lt_raw_data IS INITIAL.
    " Si la ruta parece ser del front-end (ej. contiene ':' o '\'),
    " no intentar leer desde el servidor de aplicaciones
    IF lv_filename CS ':' OR lv_filename CS '\'.
      MESSAGE 'Error al leer el archivo FUES. Verifique ruta y formato (CSV/TSV).' TYPE 'W'.
      RETURN.
    ENDIF.

    CLEAR lt_raw_data.
    OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE 'Error al leer el archivo FUES. Verifique ruta y formato (CSV/TSV).' TYPE 'W'.
      RETURN.
    ENDIF.
    DO.
      READ DATASET lv_filename INTO lv_line.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      APPEND lv_line TO lt_raw_data.
    ENDDO.
    CLOSE DATASET lv_filename.
  ENDIF.

  lv_first_line = abap_true.
  LOOP AT lt_raw_data INTO lv_line.
    IF lv_first_line = abap_true.
      lv_first_line = abap_false.
      CONTINUE. " saltar encabezado
    ENDIF.

    REPLACE ALL OCCURRENCES OF '"' IN lv_line WITH ''.

    CLEAR lt_fields.
    SPLIT lv_line AT cl_abap_char_utilities=>horizontal_tab INTO TABLE lt_fields.
    IF lines( lt_fields ) <= 1.
      CLEAR lt_fields. SPLIT lv_line AT ';' INTO TABLE lt_fields.
    ENDIF.
    IF lines( lt_fields ) <= 1.
      CLEAR lt_fields. SPLIT lv_line AT ',' INTO TABLE lt_fields.
    ENDIF.

    IF lines( lt_fields ) >= 8.
      READ TABLE lt_fields INDEX 1 INTO lv_step.
      READ TABLE lt_fields INDEX 4 INTO lv_type.
      IF lv_step IS NOT INITIAL.
        CONDENSE lv_step NO-GAPS.
      ENDIF.
      TRANSLATE lv_type TO UPPER CASE.
      IF lv_step <> '100' AND lv_step <> '200' AND lv_step <> '300'.
        CONTINUE.
      ENDIF.
      IF lv_type <> 'AUTH'.
        CONTINUE.
      ENDIF.

      READ TABLE lt_fields INDEX 2 INTO lv_rule.
      READ TABLE lt_fields INDEX 6 INTO lv_object.
      READ TABLE lt_fields INDEX 7 INTO lv_field.
      READ TABLE lt_fields INDEX 8 INTO lv_value.

      TRANSLATE lv_rule TO UPPER CASE.
      CONDENSE: lv_rule  NO-GAPS,
                lv_object NO-GAPS,
                lv_field  NO-GAPS,
                lv_value  NO-GAPS.

      CLEAR lv_level.
      IF strlen( lv_rule ) >= 2.
        CASE lv_rule+0(2).
          WHEN 'GB'. lv_level = 'AVANZADO'.
          WHEN 'GC'. lv_level = 'CORE'.
          WHEN 'GD'. lv_level = 'SELF SERV'.
        ENDCASE.
      ENDIF.

      IF lv_object IS NOT INITIAL AND lv_field IS NOT INITIAL AND lv_value IS NOT INITIAL AND lv_level IS NOT INITIAL.
        READ TABLE gt_fues_auth ASSIGNING FIELD-SYMBOL(<fs_auth>)
             WITH TABLE KEY auth_object = lv_object auth_field = lv_field auth_value = lv_value.
        IF sy-subrc = 0.
          CASE lv_level.
            WHEN 'AVANZADO'. <fs_auth>-fues_level = 'AVANZADO'.
            WHEN 'CORE'.
              IF <fs_auth>-fues_level <> 'AVANZADO'.
                <fs_auth>-fues_level = 'CORE'.
              ENDIF.
            WHEN 'SELF SERV'.
              IF <fs_auth>-fues_level = '' OR <fs_auth>-fues_level = 'No disponible'.
                <fs_auth>-fues_level = 'SELF SERV'.
              ENDIF.
          ENDCASE.
        ELSE.
          INSERT VALUE ty_auth_fues(
            auth_object = lv_object
            auth_field  = lv_field
            auth_value  = lv_value
            fues_level  = lv_level ) INTO TABLE gt_fues_auth.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF gt_fues_auth IS NOT INITIAL.
    gv_fues_enabled = abap_true.
    MESSAGE |Se cargaron { lines( gt_fues_auth ) } objetos FUES.| TYPE 'I'.
  ELSE.
    MESSAGE 'No se cargaron datos FUES válidos. Se continúa sin mapa FUES.' TYPE 'I'.
  ENDIF.
ENDFORM.

*=====================================================================*
* Calcular nivel FUES por Transacción                                 *
*=====================================================================*
FORM calculate_transaction_fues.
  CLEAR gt_fues_tcode.

  LOOP AT gt_transaction_auth ASSIGNING FIELD-SYMBOL(<fs_ta>).
    DATA(lv_level) = 'No disponible'.

    READ TABLE gt_fues_auth ASSIGNING FIELD-SYMBOL(<fs_fues>)
         WITH TABLE KEY auth_object = <fs_ta>-auth_object
                              auth_field  = <fs_ta>-auth_field
                              auth_value  = <fs_ta>-auth_value.
    IF sy-subrc = 0.
      lv_level = <fs_fues>-fues_level.
    ENDIF.

    <fs_ta>-fues_level = lv_level.

    READ TABLE gt_fues_tcode ASSIGNING FIELD-SYMBOL(<fs_map>)
         WITH TABLE KEY transaction = <fs_ta>-transaction.
    IF sy-subrc <> 0.
      INSERT VALUE ty_tcode_fues( transaction = <fs_ta>-transaction
                                  fues_level  = lv_level ) INTO TABLE gt_fues_tcode.
    ELSE.
      CASE lv_level.
        WHEN 'AVANZADO'.
          <fs_map>-fues_level = 'AVANZADO'.
        WHEN 'CORE'.
          IF <fs_map>-fues_level <> 'AVANZADO'.
            <fs_map>-fues_level = 'CORE'.
          ENDIF.
        WHEN 'SELF SERV'.
          IF <fs_map>-fues_level = 'No disponible'.
            <fs_map>-fues_level = 'SELF SERV'.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDLOOP.
  SORT gt_transaction_auth BY transaction role_name auth_object auth_field.
ENDFORM.

*=====================================================================*
* Alternative: Simple file reading approach using OPEN DATASET        *
*=====================================================================*
FORM load_fues_excel_alt.
  " Alternative approach for systems where GUI_UPLOAD might not work
  " This approach reads the file as a simple text file
  DATA: lv_filename TYPE string,
        lv_line     TYPE string,
        lt_fields   TYPE TABLE OF string,
        lv_tcode    TYPE tcode,
        lv_rule     TYPE string,
        lv_level    TYPE char15,
        lv_counter  TYPE i.

  lv_filename = p_file.

  " Simple message - this alternative is for reference only
  MESSAGE 'Use the main LOAD_FUES_DATA form. This alternative is for reference.' TYPE 'I'.

  " Alternative approach using text processing (commented out for reference)
  " You can implement this if needed for specific file formats

ENDFORM.

*=====================================================================*
* Vista Rol ↔ Transacción (4.2)                                       *
*=====================================================================*
FORM process_role_transaction_view.
  PERFORM get_role_transaction_data.  " Obtener las transacciones vinculadas a cada rol
  IF gv_fues_enabled = abap_true.
    PERFORM calculate_role_fues.      " Determinar nivel FUES por rol
  ENDIF.
  PERFORM build_role_trans_summary.  " Construir resumen estadístico de la vista
  PERFORM display_role_trans_alv.    " Mostrar datos en tabla ALV SALV
ENDFORM.

*=====================================================================*
* Vista Usuario ↔ Transacción (4.3)                                   *
*=====================================================================*
FORM process_user_tcode_view.
  PERFORM get_user_tcode_data.       " Determinar transacciones disponibles por usuario (vía rol)
  PERFORM build_user_tcode_summary.  " Generar resumen de uso por usuario o grupo
  PERFORM display_user_tcode_alv.    " Mostrar datos en tabla ALV SALV
ENDFORM.

*=====================================================================*
* Vista Usuario ↔ Objeto de autorización (4.4)                        *
*=====================================================================*
FORM process_user_object_view.
  PERFORM get_user_object_data.       " Obtener objetos de autorización disponibles por usuario
  PERFORM build_user_object_summary. " Construir resumen de objetos únicos por usuario o rol
  PERFORM display_user_object_alv.   " Mostrar datos en tabla ALV SALV
ENDFORM.

*=====================================================================*
* Vista Usuario ↔ Perfil (4.5)                                        *
*=====================================================================*
FORM process_user_profile_view.
  PERFORM get_user_profile_data.       " Obtener perfiles asignados a cada usuario
  PERFORM build_user_profile_summary.  " Construir resumen de perfiles
  PERFORM display_user_profile_alv.    " Mostrar datos en tabla ALV SALV
ENDFORM.

*=====================================================================*
* Vista Usuarios con nivel FUES (4.6)                                *
*=====================================================================*
FORM process_user_fues_view.
  PERFORM get_user_basic_data.       " Obtener usuarios y estado
  IF gv_fues_enabled = abap_true.
    PERFORM calculate_user_basic_fues. " Calcular nivel FUES por usuario
  ENDIF.
  PERFORM build_user_basic_summary.  " Generar resumen de usuarios
  PERFORM display_user_basic_alv.    " Mostrar datos en tabla ALV SALV
ENDFORM.

*=====================================================================*
* Vista Roles con nivel FUES (4.7)                                    *
*=====================================================================*
FORM process_role_fues_view.
  PERFORM get_role_transaction_data. " Obtener transacciones por rol
  IF gv_fues_enabled = abap_true.
    PERFORM calculate_role_fues.     " Calcular nivel FUES por rol
  ENDIF.
  PERFORM get_role_user_counts.      " Contar usuarios por rol
  PERFORM build_role_fues_summary.   " Generar resumen de roles
  PERFORM display_role_fues_alv.     " Mostrar datos en tabla ALV SALV
ENDFORM.

*=====================================================================*
* Vista Transacción ↔ Autorización (4.6)                              *
*=====================================================================*
FORM process_transaction_auth_view.
  PERFORM get_transaction_auth_data.  " Obtener objetos de autorización y valores por transacción
  IF gv_fues_enabled = abap_true.
    PERFORM calculate_transaction_fues. " Determinar nivel FUES por transacción
  ENDIF.
  PERFORM build_trans_auth_summary.  " Generar resumen de autorizaciones y transacciones
  PERFORM display_trans_auth_alv.    " Mostrar datos en tabla ALV SALV
ENDFORM.

*=====================================================================*
* Obtener asignaciones activas Usuario ↔ Rol desde tabla AGR_USERS    *
*=====================================================================*
FORM get_user_role_data.
  DATA: lt_temp           LIKE gt_user_role,
        lv_current_date   TYPE datum,
        lv_fdate_exists   TYPE abap_bool,
        lv_tdate_exists   TYPE abap_bool.

  " Fecha actual del sistema
  lv_current_date = sy-datum.

  " Validación de existencia de filtros de fecha
  lv_fdate_exists = xsdbool( s_fdate[] IS NOT INITIAL ).
  lv_tdate_exists = xsdbool( s_tdate[] IS NOT INITIAL ).

  " Selección con joins de AGR_DEFINE, AGR_USERS y USR02
  SELECT a~agr_name  AS role_name,
         r~uname     AS user_id,
         u~class     AS user_group,
         r~from_dat  AS from_date,
         r~to_dat    AS to_date,
         CASE WHEN r~from_dat > @lv_current_date OR r~to_dat < @lv_current_date
              THEN 'X' ELSE ' ' END AS role_inactive,
         CASE WHEN u~gltgv <> '00000000' AND u~gltgv < @lv_current_date
              THEN 'X' ELSE ' ' END AS user_inactive
    FROM agr_define AS a
    INNER JOIN agr_users AS r ON a~agr_name = r~agr_name
    LEFT JOIN usr02 AS u ON r~uname = u~bname
    WHERE a~agr_name IN @s_role
      AND r~uname    IN @s_user
      AND u~class    IN @s_group
      AND ( @lv_fdate_exists = @abap_false OR r~from_dat IN @s_fdate )
      AND ( @lv_tdate_exists = @abap_false OR r~to_dat   IN @s_tdate )
      AND ( @p_inact = 'X' OR u~gltgv >= @lv_current_date OR u~gltgv = '00000000' )
    INTO CORRESPONDING FIELDS OF TABLE @lt_temp.

  " Inicializar contadores para cada línea
  LOOP AT lt_temp ASSIGNING FIELD-SYMBOL(<fs_temp>).
    <fs_temp>-roles_per_user = 0.
    <fs_temp>-users_per_role = 0.
  ENDLOOP.

  gt_user_role = lt_temp.

  " Validación de resultados y mensaje en caso vacío sin excepciones activas
  IF sy-subrc <> 0 AND p_rnousr = ' ' AND p_unorol = ' '.
    MESSAGE 'No se hallaron resultados para los criterios seleccionados.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.

*=====================================================================*
* Agregar roles sin usuarios asignados (LEFT JOIN NULL en AGR_USERS) *
*=====================================================================*
FORM add_roles_without_users.
  IF p_rnousr = 'X'.
    SELECT a~agr_name
      FROM agr_define AS a
      LEFT JOIN agr_users AS r ON a~agr_name = r~agr_name
      WHERE a~agr_name IN @s_role
        AND r~uname IS NULL
      INTO TABLE @DATA(lt_roles_no_user).

    LOOP AT lt_roles_no_user ASSIGNING FIELD-SYMBOL(<ls_role>).
      APPEND VALUE #(
        role_name      = <ls_role>-agr_name
        role_inactive  = ' '
        users_per_role = 0 ) TO gt_user_role.
    ENDLOOP.
  ENDIF.
ENDFORM.

*=====================================================================*
* Agregar usuarios sin ningún rol asignado (LEFT JOIN NULL en AGR_USERS) *
*=====================================================================*
FORM add_users_without_roles.
  DATA lv_current_date TYPE datum.
  lv_current_date = sy-datum.

  IF p_unorol = 'X'.
    SELECT u~bname, u~class, u~gltgv
      FROM usr02 AS u
      LEFT JOIN agr_users AS r ON u~bname = r~uname
      WHERE u~bname IN @s_user
        AND u~class IN @s_group
        AND r~uname IS NULL
      INTO TABLE @DATA(lt_users_no_role).

    LOOP AT lt_users_no_role ASSIGNING FIELD-SYMBOL(<ls_user>).
      DATA(lv_user_inactive) = COND #(
        WHEN <ls_user>-gltgv <> '00000000' AND <ls_user>-gltgv < lv_current_date
        THEN 'X' ELSE ' ' ).

      APPEND VALUE #(
        user_id        = <ls_user>-bname
        user_group     = <ls_user>-class
        user_inactive  = lv_user_inactive
        roles_per_user = 0 ) TO gt_user_role.
    ENDLOOP.
  ENDIF.
ENDFORM.

*=====================================================================*
* Cálculo de contadores: roles por usuario y usuarios por rol        *
*=====================================================================*
FORM calculate_counts.
  DATA: lt_user_role_pairs TYPE STANDARD TABLE OF ty_user_role,
        lt_role_user_pairs TYPE STANDARD TABLE OF ty_user_role.

  " Obtener subconjuntos únicos para cálculo de cantidades
  lt_user_role_pairs = gt_user_role.
  SORT lt_user_role_pairs BY user_id role_name.
  DELETE ADJACENT DUPLICATES FROM lt_user_role_pairs COMPARING user_id role_name.

  lt_role_user_pairs = gt_user_role.
  SORT lt_role_user_pairs BY role_name user_id.
  DELETE ADJACENT DUPLICATES FROM lt_role_user_pairs COMPARING role_name user_id.

  " Contar roles por usuario y usuarios por rol
  LOOP AT gt_user_role ASSIGNING FIELD-SYMBOL(<fs_output>).
    IF <fs_output>-user_id IS NOT INITIAL.
      <fs_output>-roles_per_user = REDUCE i(
        INIT cnt = 0
        FOR <pair> IN lt_user_role_pairs
        WHERE ( user_id = <fs_output>-user_id AND role_name IS NOT INITIAL )
        NEXT cnt = cnt + 1 ).
    ENDIF.

    IF <fs_output>-role_name IS NOT INITIAL.
      <fs_output>-users_per_role = REDUCE i(
        INIT cnt = 0
        FOR <pair> IN lt_role_user_pairs
        WHERE ( role_name = <fs_output>-role_name AND user_id IS NOT INITIAL )
        NEXT cnt = cnt + 1 ).
    ENDIF.
  ENDLOOP.
ENDFORM.

*=====================================================================*
* Aplicar filtros finales sobre roles sin usuarios y viceversa        *
*=====================================================================*
FORM apply_user_role_filters.
  " Si no se marcó la opción para incluir roles sin usuarios,
  " eliminamos aquellos sin nombre o sin asignaciones
  IF p_rnousr IS INITIAL.
    DELETE gt_user_role WHERE role_name IS INITIAL OR users_per_role = 0.
  ENDIF.

  " Si no se marcó la opción para incluir usuarios sin roles,
  " eliminamos aquellos sin ID o sin asignaciones
  IF p_unorol IS INITIAL.
    DELETE gt_user_role WHERE user_id IS INITIAL OR roles_per_user = 0.
  ENDIF.
ENDFORM.

*=====================================================================*
* Construcción del resumen de datos de vista Usuario ↔ Rol            *
*=====================================================================*
FORM build_user_role_summary.
  DATA: lv_users          TYPE i,
        lv_roles          TYPE i,
        lv_inactive_users TYPE i,
        lv_inactive_roles TYPE i,
        lv_users_no_role  TYPE i,
        lv_roles_no_user  TYPE i,
        lt_users          TYPE STANDARD TABLE OF xubname,
        lt_roles          TYPE STANDARD TABLE OF agr_name,
        ls_output         TYPE ty_user_role.

  " Usuarios únicos
  LOOP AT gt_user_role INTO ls_output WHERE user_id IS NOT INITIAL.
    APPEND ls_output-user_id TO lt_users.
  ENDLOOP.
  SORT lt_users. DELETE ADJACENT DUPLICATES FROM lt_users.
  lv_users = lines( lt_users ).

  " Roles únicos
  LOOP AT gt_user_role INTO ls_output WHERE role_name IS NOT INITIAL.
    APPEND ls_output-role_name TO lt_roles.
  ENDLOOP.
  SORT lt_roles. DELETE ADJACENT DUPLICATES FROM lt_roles.
  lv_roles = lines( lt_roles ).

  " Usuarios inactivos
  DATA lt_inactive_users TYPE STANDARD TABLE OF xubname.
  LOOP AT gt_user_role INTO ls_output WHERE user_inactive = 'X' AND user_id IS NOT INITIAL.
    APPEND ls_output-user_id TO lt_inactive_users.
  ENDLOOP.
  SORT lt_inactive_users. DELETE ADJACENT DUPLICATES FROM lt_inactive_users.
  lv_inactive_users = lines( lt_inactive_users ).

  " Roles inactivos
  DATA lt_inactive_roles TYPE STANDARD TABLE OF agr_name.
  LOOP AT gt_user_role INTO ls_output WHERE role_inactive = 'X' AND role_name IS NOT INITIAL.
    APPEND ls_output-role_name TO lt_inactive_roles.
  ENDLOOP.
  SORT lt_inactive_roles. DELETE ADJACENT DUPLICATES FROM lt_inactive_roles.
  lv_inactive_roles = lines( lt_inactive_roles ).

  " Usuarios sin roles
  lv_users_no_role = REDUCE i( INIT cnt = 0 FOR ls_row IN gt_user_role
    WHERE ( role_name IS INITIAL AND user_id IS NOT INITIAL )
    NEXT cnt = cnt + 1 ).

  " Roles sin usuarios
  lv_roles_no_user = REDUCE i( INIT cnt = 0 FOR ls_row IN gt_user_role
    WHERE ( user_id IS INITIAL AND role_name IS NOT INITIAL )
    NEXT cnt = cnt + 1 ).

  " Armar tabla resumen
  CLEAR gt_summary.
  APPEND VALUE #( description = 'Usuarios únicos'       value = |{ lv_users }| )          TO gt_summary.
  APPEND VALUE #( description = 'Roles únicos'          value = |{ lv_roles }| )          TO gt_summary.
  APPEND VALUE #( description = 'Usuarios inactivos'    value = |{ lv_inactive_users }| ) TO gt_summary.
  APPEND VALUE #( description = 'Roles inactivos'       value = |{ lv_inactive_roles }| ) TO gt_summary.
  APPEND VALUE #( description = 'Usuarios sin roles'    value = |{ lv_users_no_role }| )  TO gt_summary.
  APPEND VALUE #( description = 'Roles sin usuarios'    value = |{ lv_roles_no_user }| )  TO gt_summary.
ENDFORM.

*=====================================================================*
* Obtener transacciones asociadas a cada rol (AGR_TCODE)              *
*=====================================================================*
FORM get_role_transaction_data.
  SELECT a~agr_name AS role_name,
         t~tcode    AS transaction,
         s~ttext    AS description
    FROM agr_define AS a
    INNER JOIN agr_tcodes AS t ON a~agr_name = t~agr_name
    LEFT JOIN tstct AS s ON t~tcode = s~tcode AND s~sprsl = @sy-langu
    WHERE a~agr_name IN @s_role
      AND t~tcode    IN @s_tcode
    INTO TABLE @gt_role_transaction.

  LOOP AT gt_role_transaction ASSIGNING FIELD-SYMBOL(<fs_rt>).
    <fs_rt>-fues_level = 'No disponible'.
    READ TABLE gt_fues_tcode ASSIGNING FIELD-SYMBOL(<fs_map>)
         WITH KEY transaction = <fs_rt>-transaction.
    IF sy-subrc = 0.
      <fs_rt>-fues_level = <fs_map>-fues_level.
    ENDIF.
  ENDLOOP.

  " Validación de existencia de datos
  IF sy-subrc <> 0.
    MESSAGE 'No se hallaron transacciones para los roles seleccionados.' TYPE 'I'.
  ENDIF.
ENDFORM.

*=====================================================================*
* Obtener relación Usuario ↔ Transacción                              *
*=====================================================================*
FORM get_user_tcode_data.
  FIELD-SYMBOLS:
    <ls_ut>  LIKE LINE OF gt_user_tcode,
    <fs_map> LIKE LINE OF gt_fues_tcode.
  SELECT r~uname    AS user_id,
         u~class    AS user_group,
         t~agr_name AS role_name,
         t~tcode    AS transaction,
         s~ttext    AS description
    FROM agr_users AS r
    INNER JOIN agr_tcodes AS t ON r~agr_name = t~agr_name
    LEFT JOIN tstct AS s ON t~tcode = s~tcode AND s~sprsl = @sy-langu
    LEFT JOIN usr02 AS u ON r~uname = u~bname
    WHERE r~uname    IN @s_user
      AND t~tcode    IN @s_tcode
      AND r~agr_name IN @s_role
      AND u~class    IN @s_group
      AND ( @p_inact = 'X' OR u~gltgv >= @sy-datum OR u~gltgv = '00000000' )
    INTO TABLE @gt_user_tcode.

  " Validación de existencia de datos
  IF sy-subrc <> 0.
    MESSAGE 'No se halló relación Usuario-Transacción con los criterios dados.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_user_tcode BY user_id user_group transaction role_name.
  DELETE ADJACENT DUPLICATES FROM gt_user_tcode COMPARING user_id user_group transaction role_name.

  LOOP AT gt_user_tcode ASSIGNING <ls_ut>.
    <ls_ut>-fues_level = 'No disponible'.
  ENDLOOP.

  IF gv_fues_enabled = abap_true.
    IF gt_fues_tcode IS INITIAL.
      IF gt_transaction_auth IS INITIAL.
        PERFORM get_transaction_auth_data.
      ENDIF.
      PERFORM calculate_transaction_fues.
    ENDIF.
    LOOP AT gt_user_tcode ASSIGNING <ls_ut>.
      READ TABLE gt_fues_tcode ASSIGNING <fs_map> WITH KEY transaction = <ls_ut>-transaction.
      IF sy-subrc = 0.
        <ls_ut>-fues_level = <fs_map>-fues_level.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

*=====================================================================*
* Resumen de relación Usuario ↔ Transacción                           *
*=====================================================================*
FORM build_user_tcode_summary.
  DATA: lv_users TYPE i,
        lv_tx    TYPE i,
        lt_users TYPE STANDARD TABLE OF xubname,
        lt_tx    TYPE STANDARD TABLE OF tcode.

  LOOP AT gt_user_tcode INTO DATA(ls1).
    APPEND ls1-user_id     TO lt_users.
    APPEND ls1-transaction TO lt_tx.
  ENDLOOP.

  SORT lt_users. DELETE ADJACENT DUPLICATES FROM lt_users. lv_users = lines( lt_users ).
  SORT lt_tx.    DELETE ADJACENT DUPLICATES FROM lt_tx.    lv_tx    = lines( lt_tx ).

  CLEAR gt_summary.
  APPEND VALUE #( description = 'Usuarios únicos'      value = |{ lv_users }| )               TO gt_summary.
  APPEND VALUE #( description = 'Transacciones únicas' value = |{ lv_tx }| )                  TO gt_summary.
  APPEND VALUE #( description = 'Asignaciones (filas)' value = |{ lines( gt_user_tcode ) }| ) TO gt_summary.
ENDFORM.

*=====================================================================*
* Obtener relación Usuario ↔ Objeto de autorización                   *
*=====================================================================*
FORM get_user_object_data.
  SELECT r~uname    AS user_id,
         u~class    AS user_group,
         a~agr_name AS role_name,
         au~object  AS auth_object,
         au~field   AS auth_field,
         au~low     AS auth_value
    FROM agr_users AS r
    INNER JOIN agr_define AS a ON r~agr_name = a~agr_name
    INNER JOIN agr_1251 AS au ON a~agr_name = au~agr_name
    LEFT JOIN usr02 AS u ON r~uname = u~bname
    WHERE r~uname    IN @s_user
      AND a~agr_name IN @s_role
      AND au~object  IN @s_object
      AND u~class    IN @s_group
      AND ( @p_inact = 'X' OR u~gltgv >= @sy-datum OR u~gltgv = '00000000' )
    INTO CORRESPONDING FIELDS OF TABLE @gt_user_object.

  " Validación de existencia de datos
  IF sy-subrc <> 0.
    MESSAGE 'No se halló relación Usuario-Objeto con los criterios dados.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_user_object BY user_id user_group auth_object auth_field auth_value role_name.
  DELETE ADJACENT DUPLICATES FROM gt_user_object
    COMPARING user_id user_group auth_object auth_field auth_value role_name.

  LOOP AT gt_user_object ASSIGNING FIELD-SYMBOL(<fs_uo>).
    <fs_uo>-fues_level = 'No disponible'.
    IF gv_fues_enabled = abap_true.
      READ TABLE gt_fues_auth ASSIGNING FIELD-SYMBOL(<fs_fues>)
           WITH TABLE KEY auth_object = <fs_uo>-auth_object
                                    auth_field  = <fs_uo>-auth_field
                                    auth_value  = <fs_uo>-auth_value.
      IF sy-subrc = 0.
        <fs_uo>-fues_level = <fs_fues>-fues_level.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*=====================================================================*
* Obtener relación Usuario ↔ Perfil                                   *
*=====================================================================*
FORM get_user_profile_data.
  CLEAR gt_user_profile.

  SELECT u~bname      AS user_id
         ,u~class     AS user_group
         ,p~profile   AS profile
         ,tx~ptext    AS profile_text
    FROM ust04  AS p
    INNER JOIN usr02 AS u  ON u~bname  = p~bname
    LEFT  JOIN usr11 AS tx ON tx~profn = p~profile
                          AND tx~langu = @sy-langu
    WHERE p~bname   IN @s_user
      AND u~class   IN @s_group
      AND p~profile IN @s_prof
      AND ( @p_inact = 'X'
            OR u~gltgv >= @sy-datum
            OR u~gltgv = '00000000' )
    INTO TABLE @gt_user_profile.

  IF gt_user_profile IS INITIAL.
    MESSAGE 'No se halló relación Usuario-Perfil con los criterios dados.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_user_profile BY user_id profile.
  DELETE ADJACENT DUPLICATES FROM gt_user_profile COMPARING user_id profile.
ENDFORM.

*=====================================================================*
* Obtener usuarios básicos y estado de actividad                      *
*=====================================================================*
FORM get_user_basic_data.
  TYPES: BEGIN OF ty_user,
           user_id    TYPE usr02-bname,
           user_group TYPE usr02-class,
           valid_from TYPE usr02-gltgv,
           valid_to   TYPE usr02-gltgb,
           uflag      TYPE usr02-uflag,
         END OF ty_user.

  TYPES: BEGIN OF ty_role_cnt,
           user_id TYPE agr_users-uname,
           total   TYPE i,
           active  TYPE i,
         END OF ty_role_cnt.

  DATA: lt_users      TYPE STANDARD TABLE OF ty_user,
        lt_role_cnt   TYPE STANDARD TABLE OF ty_role_cnt,
        ls_user       TYPE ty_user,
        ls_cnt        TYPE ty_role_cnt,
        ls_user_basic TYPE ty_user_basic,
        lv_valid      TYPE abap_bool.

  CLEAR gt_user_basic.

  SELECT bname    AS user_id,
         class    AS user_group,
         gltgv    AS valid_from,
         gltgb    AS valid_to,
         uflag
    FROM usr02
    WHERE bname IN @s_user
      AND class IN @s_group
      AND ( @p_inact = 'X' OR gltgb >= @sy-datum OR gltgb = '00000000' )
    INTO TABLE @lt_users.

    IF lt_users IS NOT INITIAL.
      SELECT uname      AS user_id,
             from_dat   AS from_date,
             to_dat     AS to_date
        FROM agr_users
        FOR ALL ENTRIES IN @lt_users
        WHERE uname = @lt_users-user_id
          AND agr_name IN @s_role
        INTO TABLE @DATA(lt_roles).

      LOOP AT lt_roles INTO DATA(ls_role).
        READ TABLE lt_role_cnt INTO ls_cnt WITH KEY user_id = ls_role-user_id.
        IF sy-subrc = 0.
          ls_cnt-total = ls_cnt-total + 1.
          IF ls_role-from_date <= sy-datum AND ( ls_role-to_date = '00000000' OR ls_role-to_date >= sy-datum ).
            ls_cnt-active = ls_cnt-active + 1.
          ENDIF.
          MODIFY lt_role_cnt FROM ls_cnt INDEX sy-tabix.
        ELSE.
          CLEAR ls_cnt.
          ls_cnt-user_id = ls_role-user_id.
          ls_cnt-total = 1.
          IF ls_role-from_date <= sy-datum AND ( ls_role-to_date = '00000000' OR ls_role-to_date >= sy-datum ).
            ls_cnt-active = 1.
          ENDIF.
          APPEND ls_cnt TO lt_role_cnt.
        ENDIF.
      ENDLOOP.
    ENDIF.

  LOOP AT lt_users INTO ls_user.
    READ TABLE lt_role_cnt INTO ls_cnt WITH KEY user_id = ls_user-user_id.
    IF sy-subrc <> 0.
      CLEAR ls_cnt.
    ENDIF.

    ls_user_basic-user_id     = ls_user-user_id.
    ls_user_basic-user_group  = ls_user-user_group.
    ls_user_basic-locked      = COND #( WHEN ls_user-uflag <> '0' THEN 'X' ELSE ' ' ).
    ls_user_basic-valid_from  = ls_user-valid_from.
    ls_user_basic-valid_to    = ls_user-valid_to.
    ls_user_basic-roles_total = ls_cnt-total.
    ls_user_basic-roles_active = ls_cnt-active.

    lv_valid = xsdbool( ls_user-valid_from <= sy-datum AND
                        ( ls_user-valid_to = '00000000' OR ls_user-valid_to >= sy-datum ) ).

    IF ls_user_basic-roles_active = 0 AND lv_valid = abap_false AND ls_user_basic-locked = 'X'.
      ls_user_basic-inactive = 'X'.
    ELSEIF ls_user_basic-roles_active > 0 AND lv_valid = abap_false.
      ls_user_basic-inactive = 'F'.
    ELSEIF ls_user_basic-roles_active = 0 AND lv_valid = abap_true.
      ls_user_basic-inactive = 'R'.
    ELSEIF ls_user_basic-roles_active = 0 AND lv_valid = abap_false.
      ls_user_basic-inactive = 'F'.
    ELSE.
      ls_user_basic-inactive = ' '.
    ENDIF.

    ls_user_basic-fues_level = 'No disponible'.
    APPEND ls_user_basic TO gt_user_basic.
  ENDLOOP.

  SORT gt_user_basic BY user_id.
ENDFORM.

*=====================================================================*
* Calcular nivel FUES para cada usuario                               *
*=====================================================================*
FORM calculate_user_basic_fues.
  DATA: lt_user_obj TYPE STANDARD TABLE OF ty_user_object.

  SELECT r~uname    AS user_id,
         au~object  AS auth_object,
         au~field   AS auth_field,
         au~low     AS auth_value
    FROM agr_users AS r
    INNER JOIN agr_1251 AS au ON r~agr_name = au~agr_name
    INNER JOIN usr02   AS u  ON u~bname   = r~uname
    WHERE r~uname  IN @s_user
      AND u~class IN @s_group
      AND ( @p_inact = 'X' OR u~gltgb >= @sy-datum OR u~gltgb = '00000000' )
    INTO CORRESPONDING FIELDS OF TABLE @lt_user_obj.

  LOOP AT lt_user_obj INTO DATA(ls_obj).
    READ TABLE gt_fues_auth ASSIGNING FIELD-SYMBOL(<fs_fues>)
         WITH TABLE KEY auth_object = ls_obj-auth_object
                                   auth_field  = ls_obj-auth_field
                                   auth_value  = ls_obj-auth_value.
    IF sy-subrc = 0.
      READ TABLE gt_user_basic ASSIGNING FIELD-SYMBOL(<fs_user>)
           WITH KEY user_id = ls_obj-user_id.
      IF sy-subrc = 0.
        CASE <fs_fues>-fues_level.
          WHEN 'AVANZADO'.
            <fs_user>-fues_level = 'AVANZADO'.
          WHEN 'CORE'.
            IF <fs_user>-fues_level <> 'AVANZADO'.
              <fs_user>-fues_level = 'CORE'.
            ENDIF.
          WHEN 'SELF SERV'.
            IF <fs_user>-fues_level = 'No disponible'.
              <fs_user>-fues_level = 'SELF SERV'.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*=====================================================================*
* Resumen de Usuarios con nivel FUES                                 *
*=====================================================================*
FORM build_user_basic_summary.
  DATA: lv_adv      TYPE i,
        lv_core     TYPE i,
        lv_self     TYPE i,
        lv_active   TYPE i,
        lv_inactive TYPE i,
        lv_fdate    TYPE i,
        lv_norole   TYPE i,
        lv_score    TYPE decfloat16.

  LOOP AT gt_user_basic INTO DATA(ls_ub).
    CASE ls_ub-inactive.
      WHEN 'X'.
        lv_inactive += 1.
      WHEN 'F'.
        lv_fdate += 1.
      WHEN 'R'.
        lv_norole += 1.
      WHEN OTHERS.
        lv_active += 1.
    ENDCASE.

    IF ls_ub-inactive = ' '.
      CASE ls_ub-fues_level.
        WHEN 'AVANZADO'.
          lv_adv += 1.
        WHEN 'CORE'.
          lv_core += 1.
        WHEN 'SELF SERV'.
          lv_self += 1.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  lv_score = lv_adv.
  lv_score += lv_core * '0.2'.
  lv_score += lv_self / '30'.

  CLEAR gt_summary.
  APPEND VALUE #( description = 'Usuarios AVANZADO (activos)'  value = |{ lv_adv }| )  TO gt_summary.
  APPEND VALUE #( description = 'Usuarios CORE (activos)'      value = |{ lv_core }| ) TO gt_summary.
  APPEND VALUE #( description = 'Usuarios SELF SERV (activos)' value = |{ lv_self }| ) TO gt_summary.
  APPEND VALUE #( description = 'Usuarios activos'             value = |{ lv_active }| ) TO gt_summary.
  APPEND VALUE #( description = 'Usuarios inactivos (X)'       value = |{ lv_inactive }| ) TO gt_summary.
  APPEND VALUE #( description = 'Usuarios fecha inválida (F)'  value = |{ lv_fdate }| )    TO gt_summary.
  APPEND VALUE #( description = 'Usuarios sin roles (R)'       value = |{ lv_norole }| )  TO gt_summary.
  APPEND VALUE #( description = 'Puntaje FUES'                 value = |{ lv_score DECIMALS = 2 }| ) TO gt_summary.
ENDFORM.

*=====================================================================*
* Resumen de Roles con nivel FUES                                     *
*=====================================================================*
FORM build_role_fues_summary.
  DATA: lv_total TYPE i,
        lv_adv   TYPE i,
        lv_core  TYPE i,
        lv_self  TYPE i.

  LOOP AT gt_fues_role INTO DATA(ls_rf).
    lv_total += 1.
    CASE ls_rf-fues_level.
      WHEN 'AVANZADO'.
        lv_adv += 1.
      WHEN 'CORE'.
        lv_core += 1.
      WHEN 'SELF SERV'.
        lv_self += 1.
    ENDCASE.
  ENDLOOP.

  CLEAR gt_summary.
  APPEND VALUE #( description = 'Roles totales'   value = |{ lv_total }| ) TO gt_summary.
  APPEND VALUE #( description = 'Roles AVANZADO'  value = |{ lv_adv }| )   TO gt_summary.
  APPEND VALUE #( description = 'Roles CORE'      value = |{ lv_core }| )  TO gt_summary.
  APPEND VALUE #( description = 'Roles SELF SERV' value = |{ lv_self }| )  TO gt_summary.
ENDFORM.

*=====================================================================*
* Resumen general de vista Rol ↔ Transacción                          *
*=====================================================================*
FORM build_role_trans_summary.
  DATA: lv_roles TYPE i,
        lv_tx    TYPE i,
        lt_roles TYPE STANDARD TABLE OF agr_name,
        lt_tx    TYPE STANDARD TABLE OF tcode.

  LOOP AT gt_role_transaction INTO DATA(ls_rt).
    APPEND ls_rt-role_name   TO lt_roles.
    APPEND ls_rt-transaction TO lt_tx.
  ENDLOOP.

  " Quitar duplicados y contar
  SORT lt_roles. DELETE ADJACENT DUPLICATES FROM lt_roles. lv_roles = lines( lt_roles ).
  SORT lt_tx.    DELETE ADJACENT DUPLICATES FROM lt_tx.    lv_tx    = lines( lt_tx ).

  " Generar resumen
  CLEAR gt_summary.
  APPEND VALUE #( description = 'Roles únicos'          value = |{ lv_roles }| )                      TO gt_summary.
  APPEND VALUE #( description = 'Transacciones únicas'  value = |{ lv_tx }| )                         TO gt_summary.
  APPEND VALUE #( description = 'Asignaciones (filas)'  value = |{ lines( gt_role_transaction ) }| )  TO gt_summary.
ENDFORM.

*=====================================================================*
* Obtener relación Transacción ↔ Objeto de autorización                *
*=====================================================================*
FORM get_transaction_auth_data.
  " Si el mapa FUES está cargado, limitar la búsqueda a esas autorizaciones
  IF gv_fues_enabled = abap_true AND gt_fues_auth IS NOT INITIAL.
    SELECT t~tcode    AS transaction,
           a~agr_name AS role_name,
           s~ttext    AS description,
           au~object  AS auth_object,
           au~field   AS auth_field,
           au~low     AS auth_value,
           'No disponible' AS fues_level
      FROM agr_tcodes AS t
      INNER JOIN agr_define AS a ON t~agr_name = a~agr_name
      LEFT JOIN tstct AS s ON t~tcode = s~tcode AND s~sprsl = @sy-langu
      INNER JOIN agr_1251 AS au ON a~agr_name = au~agr_name
      INNER JOIN @gt_fues_auth AS f ON au~object = f~auth_object
                                   AND au~field  = f~auth_field
                                   AND au~low    = f~auth_value
      WHERE t~tcode    IN @s_tcode
        AND a~agr_name IN @s_role
      INTO TABLE @gt_transaction_auth.
  ELSE.
    SELECT t~tcode    AS transaction,
           a~agr_name AS role_name,
           s~ttext    AS description,
           au~object  AS auth_object,
           au~field   AS auth_field,
           au~low     AS auth_value,
           'No disponible' AS fues_level
      FROM agr_tcodes AS t
      INNER JOIN agr_define AS a ON t~agr_name = a~agr_name
      LEFT JOIN tstct AS s ON t~tcode = s~tcode AND s~sprsl = @sy-langu
      LEFT JOIN agr_1251 AS au ON a~agr_name = au~agr_name
      WHERE t~tcode    IN @s_tcode
        AND a~agr_name IN @s_role
        AND au~object  IN @s_object
      INTO TABLE @gt_transaction_auth.
  ENDIF.

  " Validación de existencia de datos
  IF sy-subrc <> 0.
    MESSAGE 'No se hallaron autorizaciones para las transacciones seleccionadas.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_transaction_auth BY transaction role_name auth_object auth_field.
ENDFORM.

*=====================================================================*
* Resumen de relación Transacción ↔ Autorización                      *
*=====================================================================*
FORM build_trans_auth_summary.
  DATA: lv_tx    TYPE i,
        lv_objs  TYPE i,
        lt_tx    TYPE STANDARD TABLE OF tcode,
        lt_objs  TYPE STANDARD TABLE OF agr_1251-object.

  LOOP AT gt_transaction_auth INTO DATA(ls_ta).
    APPEND ls_ta-transaction TO lt_tx.
    APPEND ls_ta-auth_object TO lt_objs.
  ENDLOOP.

  SORT lt_tx.   DELETE ADJACENT DUPLICATES FROM lt_tx.   lv_tx   = lines( lt_tx ).
  SORT lt_objs. DELETE ADJACENT DUPLICATES FROM lt_objs. lv_objs = lines( lt_objs ).

  CLEAR gt_summary.
  APPEND VALUE #( description = 'Transacciones únicas'     value = |{ lv_tx }| )                        TO gt_summary.
  APPEND VALUE #( description = 'Objetos de autorización'  value = |{ lv_objs }| )                      TO gt_summary.
  APPEND VALUE #( description = 'Asignaciones (filas)'     value = |{ lines( gt_transaction_auth ) }| ) TO gt_summary.
ENDFORM.
*=====================================================================*
* Resumen de relación Usuario ↔ Objeto de autorización                *
*=====================================================================*
FORM build_user_object_summary.
  DATA: lv_users TYPE i,
        lv_objs  TYPE i,
        lt_users TYPE STANDARD TABLE OF xubname,
        lt_objs  TYPE STANDARD TABLE OF agr_1251-object.

  LOOP AT gt_user_object INTO DATA(ls2).
    APPEND ls2-user_id     TO lt_users.
    APPEND ls2-auth_object TO lt_objs.
  ENDLOOP.

  SORT lt_users. DELETE ADJACENT DUPLICATES FROM lt_users. lv_users = lines( lt_users ).
  SORT lt_objs.  DELETE ADJACENT DUPLICATES FROM lt_objs.  lv_objs  = lines( lt_objs ).

  CLEAR gt_summary.
  APPEND VALUE #( description = 'Usuarios únicos'                value = |{ lv_users }| )                TO gt_summary.
  APPEND VALUE #( description = 'Objetos de autorización únicos' value = |{ lv_objs }| )                 TO gt_summary.
  APPEND VALUE #( description = 'Asignaciones (filas)'           value = |{ lines( gt_user_object ) }| ) TO gt_summary.
ENDFORM.

*=====================================================================*
* Resumen de relación Usuario ↔ Perfil                                *
*=====================================================================*
FORM build_user_profile_summary.
  DATA: lt_users   TYPE STANDARD TABLE OF xubname,
        lt_prof    TYPE STANDARD TABLE OF ust04-profile,
        lv_users   TYPE i,
        lv_prof    TYPE i.

  LOOP AT gt_user_profile INTO DATA(ls_up).
    APPEND ls_up-user_id TO lt_users.
    APPEND ls_up-profile TO lt_prof.
  ENDLOOP.

  SORT lt_users. DELETE ADJACENT DUPLICATES FROM lt_users. lv_users = lines( lt_users ).
  SORT lt_prof.  DELETE ADJACENT DUPLICATES FROM lt_prof.  lv_prof  = lines( lt_prof ).

  CLEAR gt_summary.
  APPEND VALUE #( description = 'Usuarios únicos'         value = |{ lv_users }| )          TO gt_summary.
  APPEND VALUE #( description = 'Perfiles únicos'         value = |{ lv_prof }| )           TO gt_summary.
  APPEND VALUE #( description = 'Asignaciones (filas)'    value = |{ lines( gt_user_profile ) }| ) TO gt_summary.
ENDFORM.

*=====================================================================*
* Presentación ALV: Usuario–Rol (5.1)                                 *
*=====================================================================*
FORM display_user_role_alv.
  TRY.
      " Crear tabla ALV a partir de gt_user_role
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = gt_user_role ).

      " Resumen en pie de lista sin encabezado destacado
      DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).

      DATA(lv_row) = 1.
      LOOP AT gt_summary INTO DATA(ls_s1).
        lo_grid->create_label( row = lv_row column = 1 text = ls_s1-description ).
        lo_grid->create_text(  row = lv_row column = 2 text = ls_s1-value ).
        lv_row = lv_row + 1.
      ENDLOOP.
      lo_alv->set_end_of_list( lo_grid ).

      " Configuraciones visuales y funciones
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Detalle de asignaciones Usuario-Rol' ).

      " Ajuste de nombres de columnas
      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'ROLE_NAME'      )->set_medium_text( 'Rol' ).               CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USER_ID'        )->set_medium_text( 'Usuario' ).           CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USER_GROUP'     )->set_medium_text( 'Grupo' ).             CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'FROM_DATE'      )->set_medium_text( 'Desde' ).             CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'TO_DATE'        )->set_medium_text( 'Hasta' ).             CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'ROLE_INACTIVE'  )->set_medium_text( 'Rol inactivo' ).      CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'ROLES_PER_USER' )->set_medium_text( 'Roles x Usuario' ).   CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USERS_PER_ROLE' )->set_medium_text( 'Usuarios x Rol' ).    CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USER_INACTIVE'  )->set_medium_text( 'Usuario inactivo' ).  CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg_ur).
      MESSAGE lx_msg_ur->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any_ur).
      MESSAGE lx_any_ur->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*=====================================================================*
* Presentación ALV: Rol–Transacción (5.2)                             *
*=====================================================================*
FORM display_role_trans_alv.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = gt_role_transaction ).
      DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).

      DATA(lv_row) = 1.
      LOOP AT gt_summary INTO DATA(ls_rt_s).
        lo_grid->create_label( row = lv_row column = 1 text = ls_rt_s-description ).
        lo_grid->create_text(  row = lv_row column = 2 text = ls_rt_s-value ).
        lv_row = lv_row + 1.
      ENDLOOP.
      lo_alv->set_end_of_list( lo_grid ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Detalle de transacciones por rol' ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'ROLE_NAME'  )->set_medium_text( 'Rol' ).         CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'TRANSACTION')->set_medium_text( 'Transacción' ). CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'DESCRIPTION')->set_medium_text( 'Descripción' ). CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'FUES_LEVEL' )->set_medium_text( 'Nivel FUES' ).  CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg_rt).
      MESSAGE lx_msg_rt->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any_rt).
      MESSAGE lx_any_rt->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*=====================================================================*
* Presentación ALV: Usuario–Transacción (5.3)                         *
*=====================================================================*
FORM display_user_tcode_alv.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = gt_user_tcode ).
      DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).

      DATA lv_row TYPE i VALUE 1.
      LOOP AT gt_summary INTO DATA(ls_a).
        lo_grid->create_label( row = lv_row column = 1 text = ls_a-description ).
        lo_grid->create_text(  row = lv_row column = 2 text = ls_a-value ).
        lv_row = lv_row + 1.
      ENDLOOP.
      lo_alv->set_end_of_list( lo_grid ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Transacciones disponibles por usuario (vía roles)' ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'USER_ID'    )->set_medium_text( 'Usuario' ).      CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USER_GROUP' )->set_medium_text( 'Grupo' ).        CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'ROLE_NAME'  )->set_medium_text( 'Rol' ).          CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'TRANSACTION')->set_medium_text( 'Transacción' ).  CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'DESCRIPTION')->set_medium_text( 'Descripción' ).  CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'FUES_LEVEL' )->set_medium_text( 'Nivel FUES' ).    CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg_ut).
      MESSAGE lx_msg_ut->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any_ut).
      MESSAGE lx_any_ut->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*=====================================================================*
* Presentación ALV: Usuario–Objeto de Autorización (5.4)              *
*=====================================================================*
FORM display_user_object_alv.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = gt_user_object ).
      DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).

      DATA lv_row TYPE i VALUE 1.
      LOOP AT gt_summary INTO DATA(ls_b).
        lo_grid->create_label( row = lv_row column = 1 text = ls_b-description ).
        lo_grid->create_text(  row = lv_row column = 2 text = ls_b-value ).
        lv_row = lv_row + 1.
      ENDLOOP.
      lo_alv->set_end_of_list( lo_grid ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Objetos de autorización por usuario (vía roles)' ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'USER_ID'    )->set_medium_text( 'Usuario' ).      CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USER_GROUP' )->set_medium_text( 'Grupo' ).        CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'ROLE_NAME'  )->set_medium_text( 'Rol' ).          CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'AUTH_OBJECT')->set_medium_text( 'Objeto' ).       CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'AUTH_FIELD' )->set_medium_text( 'Campo' ).        CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'AUTH_VALUE' )->set_medium_text( 'Valor' ).        CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'FUES_LEVEL' )->set_medium_text( 'FUES' ).         CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg_uo).
      MESSAGE lx_msg_uo->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any_uo).
      MESSAGE lx_any_uo->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*=====================================================================*
* Presentación ALV: Usuario–Perfil (5.5)                              *
*=====================================================================*
FORM display_user_profile_alv.
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_user_profile ).

      " Resumen en pie de lista
      DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).

      DATA(lv_row) = 1.
      LOOP AT gt_summary INTO DATA(ls_up_s).
        lo_grid->create_label( row = lv_row column = 1 text = ls_up_s-description ).
        lo_grid->create_text(  row = lv_row column = 2 text = ls_up_s-value ).
        lv_row = lv_row + 1.
      ENDLOOP.
      lo_alv->set_end_of_list( lo_grid ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Perfiles asignados por usuario' ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'USER_ID'     )->set_medium_text( 'Usuario' ).      CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USER_GROUP'  )->set_medium_text( 'Grupo' ).        CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'PROFILE'     )->set_medium_text( 'Perfil' ).       CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'PROFILE_TEXT')->set_medium_text( 'Descripción' ).  CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg_up).
      MESSAGE lx_msg_up->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any_up).
      MESSAGE lx_any_up->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*=====================================================================*
* Presentación ALV: Usuarios con nivel FUES (5.6)                     *
*=====================================================================*
FORM display_user_basic_alv.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = gt_user_basic ).
      DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_sum_ub) CHANGING t_table = gt_summary ).
      lo_sum_ub->get_columns( )->get_column( 'DESCRIPTION' )->set_medium_text( 'Resumen' ).
      lo_sum_ub->get_columns( )->get_column( 'VALUE' )->set_medium_text( 'Cantidad' ).
      lo_sum_ub->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_grid->create_control( row = 1 column = 1 control = lo_sum_ub ).
      lo_alv->set_end_of_list( lo_grid ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Usuarios con nivel FUES' ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'USER_ID'    )->set_medium_text( 'Usuario' ).      CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USER_GROUP' )->set_medium_text( 'Grupo' ).        CATCH cx_salv_not_found. ENDTRY.
      TRY.
        lo_cols->get_column( 'INACTIVE' )->set_medium_text( 'Inactivo' ).
        lo_cols->get_column( 'INACTIVE' )->set_output_length( 3 ).
      CATCH cx_salv_not_found.
      ENDTRY.
      TRY.
        lo_cols->get_column( 'LOCKED' )->set_medium_text( 'Bloqueado' ).
        lo_cols->get_column( 'LOCKED' )->set_output_length( 3 ).
      CATCH cx_salv_not_found.
      ENDTRY.
      TRY. lo_cols->get_column( 'VALID_FROM'  )->set_medium_text( 'Inicio validez' ). CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'VALID_TO'    )->set_medium_text( 'Fin validez' ).    CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'ROLES_TOTAL' )->set_medium_text( 'Roles totales' ).  CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'ROLES_ACTIVE' )->set_medium_text( 'Roles activos' ). CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'FUES_LEVEL' )->set_medium_text( 'Nivel FUES' ).      CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg_uf).
      MESSAGE lx_msg_uf->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any_uf).
      MESSAGE lx_any_uf->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*=====================================================================*
* Presentación ALV: Roles con nivel FUES (5.6)                        *
*=====================================================================*
FORM display_role_fues_alv.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = gt_fues_role ).
      DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_sum) CHANGING t_table = gt_summary ).
      lo_sum->get_columns( )->get_column( 'DESCRIPTION' )->set_medium_text( 'Resumen' ).
      lo_sum->get_columns( )->get_column( 'VALUE' )->set_medium_text( 'Cantidad' ).
      lo_sum->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_grid->create_control( row = 1 column = 1 control = lo_sum ).
      lo_alv->set_end_of_list( lo_grid ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Roles con nivel FUES' ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'ROLE_NAME'    )->set_medium_text( 'Rol' ).            CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'FUES_LEVEL'   )->set_medium_text( 'Nivel FUES' ).     CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USERS_ACTIVE' )->set_medium_text( 'Usuarios activos' ).CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USERS_TOTAL'  )->set_medium_text( 'Usuarios totales' ).CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg_rf).
      MESSAGE lx_msg_rf->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any_rf).
      MESSAGE lx_any_rf->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

*=====================================================================*
* Presentación ALV: Transacción–Autorización (5.6)                    *
*=====================================================================*
FORM display_trans_auth_alv.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = gt_transaction_auth ).
      DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).

      DATA(lv_row) = 1.
      LOOP AT gt_summary INTO DATA(ls_ta_s).
        lo_grid->create_label( row = lv_row column = 1 text = ls_ta_s-description ).
        lo_grid->create_text(  row = lv_row column = 2 text = ls_ta_s-value ).
        lv_row = lv_row + 1.
      ENDLOOP.
      lo_alv->set_end_of_list( lo_grid ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Objetos y campos de autorización por transacción' ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'TRANSACTION')->set_medium_text( 'Transacción' ). CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'ROLE_NAME'  )->set_medium_text( 'Rol' ).         CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'DESCRIPTION')->set_medium_text( 'Descripción' ). CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'AUTH_OBJECT')->set_medium_text( 'Objeto' ).      CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'AUTH_FIELD' )->set_medium_text( 'Campo' ).       CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'AUTH_VALUE' )->set_medium_text( 'Valor' ).       CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'FUES_LEVEL' )->set_medium_text( 'Nivel' ).       CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg_ta).
      MESSAGE lx_msg_ta->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any_ta).
      MESSAGE lx_any_ta->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.
*=====================================================================*
* Helper: Normalización de encabezados (6)                            *
* Convierte el texto recibido a mayúsculas, reemplaza espacios por   *
* guiones bajos y elimina caracteres especiales/tabuladores.         *
* Útil para mapear columnas del Excel a nombres de campos válidos.   *
*=====================================================================*
FORM normalize_head USING    iv_text TYPE string
                    CHANGING cv_name TYPE string.
  cv_name = iv_text.
  TRANSLATE cv_name TO UPPER CASE.
  REPLACE ALL OCCURRENCES OF ' ' IN cv_name WITH '_'.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN cv_name WITH ''.
  CONDENSE cv_name NO-GAPS.
ENDFORM.
