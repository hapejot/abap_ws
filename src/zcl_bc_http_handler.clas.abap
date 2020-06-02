CLASS zcl_bc_http_handler DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_http_handler IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA:
      d             TYPE string,
      parts         TYPE string_table,
      lr_row        TYPE REF TO data,
      tab_desc      TYPE REF TO cl_abap_typedescr,
      header_fields TYPE tihttpnvp,
      fields        TYPE tihttpnvp,
      dummy         TYPE string.

    FIELD-SYMBOLS:
                   <row> TYPE any.



    parts = VALUE #(
      ( |<html>| )
      ( |<body>| ) ).

    " find table name
    fields = VALUE tihttpnvp( ).

    APPEND |<ol>| TO parts.

    CALL METHOD server->request->get_header_fields
      CHANGING
        fields = header_fields.    " Header fields

    LOOP AT header_fields INTO DATA(hfield).
      APPEND |<li>{ hfield-name } = { hfield-value }</li>| TO parts.
    ENDLOOP.


    CALL METHOD server->request->get_form_fields_cs
      CHANGING
        fields = fields.   " Form fields
    LOOP AT fields INTO DATA(field).
      APPEND |<li>{ field-name } = { field-value }</li>| TO parts.
    ENDLOOP.
    APPEND |</ol>| TO parts.

    DATA(tabname) = VALUE #( fields[ name = 't' ]-value OPTIONAL ).

    IF tabname IS INITIAL.
      SPLIT header_fields[ name = '~path_info_expanded' ]-value AT '/' INTO dummy tabname.
    ENDIF.
    tabname = to_upper( tabname ).

    IF tabname IS NOT INITIAL.

      CALL METHOD cl_abap_typedescr=>describe_by_name
        EXPORTING
          p_name         = tabname    " Type name
        RECEIVING
          p_descr_ref    = tab_desc    " Reference to description object
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2.
      IF sy-subrc = 0.

        " read data
        APPEND  |<table>| TO parts.


        APPEND '<tr>' TO parts.
        LOOP AT CAST cl_abap_structdescr( tab_desc )->components INTO DATA(field_desc).
          APPEND |<th>{ field_desc-name }</th>| TO parts.
        ENDLOOP.
        APPEND '</tr>' TO parts.

        CREATE DATA lr_row TYPE (tabname).
        ASSIGN lr_row->* TO <row>.

        SELECT * FROM (tabname) INTO <row>.
          APPEND '<tr>' TO parts.
          LOOP AT CAST cl_abap_structdescr( tab_desc )->components INTO field_desc.
            ASSIGN COMPONENT field_desc-name OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
            APPEND |<td>{ <val> }</td>| TO parts.
          ENDLOOP.
          APPEND '</tr>' TO parts.
        ENDSELECT.


*      ( |  <tr>                    | )
*      ( |    <th>Firstname</th>    | )
*      ( |    <th>Lastname</th>     | )
*      ( |    <th>Age</th>          | )
*      ( |  </tr>                   | )
*      ( |  <tr>                    | )
*      ( |    <td>Jill</td>         | )
*      ( |    <td>Smith</td>        | )
*      ( |    <td>50</td>           | )
*      ( |  </tr>                   | )
*      ( |  <tr>                    | )
*      ( |    <td>Eve</td>          | )
*      ( |    <td>Jackson</td>      | )
*      ( |    <td>94</td>           | )
*      ( |  </tr>                   | )

        APPEND  |</table>                  |   TO parts.
      ENDIF.
    ENDIF.
    APPEND LINES OF VALUE string_table(
      ( |</body>| )
      ( |</html>| )
    ) TO parts.


    CONCATENATE LINES OF parts INTO d.

*    server->response->set_content_type( content_type = 'application/html' ).

    server->response->append_cdata2(
      EXPORTING
        data     = d    " Character data
*        encoding = CO_ENCODING_RAW    " CO_ENCODING_RAW | CO_ENCODING_URL | CO_ENCODING_HTML
*        offset   = 0    " Offset into character data
*        length   = -1    " Length of character data
    ).
  ENDMETHOD.
ENDCLASS.
