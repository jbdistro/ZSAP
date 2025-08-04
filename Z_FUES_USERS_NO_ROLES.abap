REPORT Z_FUES_USERS_NO_ROLES.

* Programa para listar usuarios sin roles asignados
TABLES: usr02, agr_users.

SELECT-OPTIONS s_user  FOR usr02-bname.
SELECT-OPTIONS s_group FOR usr02-class.
PARAMETERS p_inact AS CHECKBOX DEFAULT ' '. " Incluir inactivos

TYPES: BEGIN OF ty_user,
         user_id    TYPE usr02-bname,
         user_group TYPE usr02-class,
         active     TYPE c LENGTH 1,
         fues_level TYPE char15,
       END OF ty_user.

DATA: gt_users TYPE STANDARD TABLE OF ty_user,
      lo_alv   TYPE REF TO cl_salv_table.

START-OF-SELECTION.
  PERFORM get_users_without_roles.
  PERFORM display_alv.

FORM get_users_without_roles.
  SELECT u~bname AS user_id,
         u~class AS user_group,
         CASE WHEN u~gltgv = '00000000' OR u~gltgv >= @sy-datum THEN 'X' ELSE ' ' END AS active
    FROM usr02 AS u
    WHERE u~bname IN @s_user
      AND u~class IN @s_group
      AND ( @p_inact = 'X' OR u~gltgv >= @sy-datum OR u~gltgv = '00000000' )
      AND NOT EXISTS ( SELECT 1 FROM agr_users AS r WHERE r~uname = u~bname )
    INTO TABLE @gt_users.

  LOOP AT gt_users ASSIGNING FIELD-SYMBOL(<fs_user>).
    <fs_user>-fues_level = 'SELF SERV'.
  ENDLOOP.
ENDFORM.

FORM display_alv.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = gt_users ).
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( 'Usuarios sin roles asignados' ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      TRY. lo_cols->get_column( 'USER_ID' )->set_medium_text( 'Usuario' ).       CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'USER_GROUP' )->set_medium_text( 'Grupo' ).      CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'ACTIVE' )->set_medium_text( 'Activo' ).        CATCH cx_salv_not_found. ENDTRY.
      TRY. lo_cols->get_column( 'FUES_LEVEL' )->set_medium_text( 'Nivel FUES' ). CATCH cx_salv_not_found. ENDTRY.

      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg).
      MESSAGE lx_msg->get_text( ) TYPE 'E'.
    CATCH cx_root INTO DATA(lx_any).
      MESSAGE lx_any->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.
