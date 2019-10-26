CLASS zcl_bc_web_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF cell,
             row   TYPE i,
             field TYPE string,
             value TYPE string,
           END OF cell,
           ty_data TYPE STANDARD TABLE OF cell WITH KEY row field,
           BEGIN OF row,
             id   TYPE i,
             name TYPE string,
           END OF row,
           table_of_row TYPE STANDARD TABLE OF row WITH EMPTY KEY.
    CLASS-METHODS test
      EXPORTING
        lt_tab TYPE table_of_row.
    METHODS constructor
      IMPORTING
        host TYPE string OPTIONAL
        path TYPE string OPTIONAL
          PREFERRED PARAMETER host.
    METHODS get_data
      CHANGING
        cs_tab TYPE STANDARD TABLE.
    METHODS exec.
  PROTECTED SECTION.
    METHODS parse_row
      IMPORTING
        i_row    TYPE REF TO if_ixml_element
        i_rownum TYPE i
      CHANGING
        lt_data  TYPE ty_data.
    METHODS parse_table
      IMPORTING
        i_issues       TYPE REF TO if_ixml_node
      RETURNING
        VALUE(lt_data) TYPE ty_data.
    METHODS handle_result_doc
      IMPORTING
        i_doc TYPE REF TO cl_xml_document.
  PRIVATE SECTION.



    DATA: m_host      TYPE string,
          m_path      TYPE string,
          mt_result   TYPE zcl_bc_web_service=>ty_data,
          m_limit     TYPE i VALUE 100,
          mt_metadata TYPE zcl_bc_xml_to_data=>tt_meta_to_data.
    METHODS: set_metadata
      IMPORTING
        i_metadata TYPE zcl_bc_xml_to_data=>tt_meta_to_data OPTIONAL.






ENDCLASS.



CLASS zcl_bc_web_service IMPLEMENTATION.


  METHOD constructor.

    m_host = host.
    m_path = path.
  ENDMETHOD.


  METHOD exec.
    DATA: client TYPE REF TO if_http_client,
          lt_tab TYPE swxmlnodes,
          lt_ret TYPE sysubrc,
          last   TYPE d.

    last = sy-datum - 200.

    DATA(updated_on) = |updated_on=%3E%3D{ last(4)  }-{ last+4(2) }-{ last+6(2) }T00:00:00Z|.
    DATA(limit) = |limit={ m_limit }|.
    DATA(url) = |{ |https://{ m_host }{ m_path }?{ limit }&key=3d5ca87f29ba6467e217aca97413fd8d3e7a06d6| }|.
* using abap's cl_http_class to make http request. Passing created url string.
* Recieved client is assigned to global go_client.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = url   " URL
*       proxy_host         =     " logische Destination (Wird bei Funktionsaufruf angegeben)
*       proxy_service      =     " Portnummer
*       ssl_id             =     " SSL Identität
*       sap_username       =     " R/3-System, Anmeldename des Benutzers
*       sap_client         =     " R/3-System, Mandantennummer aus Anmeldung
      IMPORTING
        client             = client   " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    CALL METHOD client->send
*      EXPORTING
*        timeout                    = CO_TIMEOUT_DEFAULT    " Timeout von Antwortwartezeit
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    CALL METHOD client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.


*    IF sy-subrc <> 0.
*      go_client->get_last_error(
*        IMPORTING message = DATA(rmsg) ).
*      cl_demo_output=>display( rmsg ).
*      RETURN.
*    ENDIF.

*    returning response data

    DATA(cdata) = client->response->get_cdata( ).

*    clean-up
    client->close( ).
    CLEAR client.

    DATA(conv) = NEW zcl_bc_xml_to_data( ).
    conv->set_source( cdata ).
    conv->set_metadata( meta = mt_metadata ).
    conv->convert( ).

*    DATA(doc) = NEW cl_xml_document( ).
*    doc->parse_string( stream = cdata ).
*    handle_result_doc( doc ).
  ENDMETHOD.


  METHOD get_data.
    FIELD-SYMBOLS: <fs_row>   TYPE any,
                   <fs_value> TYPE any.
    LOOP AT mt_result INTO DATA(ls_cell).
      IF lines( cs_tab ) < ls_cell-row.
        APPEND INITIAL LINE TO cs_tab ASSIGNING <fs_row>.
      ELSE.
        ASSIGN cs_tab[ ls_cell-row ] TO <fs_row>.
      ENDIF.
      ASSIGN COMPONENT ls_cell-field OF STRUCTURE <fs_row> TO <fs_value>.
      IF <fs_value> IS ASSIGNED.
        <fs_value> = ls_cell-value.
        UNASSIGN <fs_value>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_result_doc.

    DATA(issues) = i_doc->find_node( 'projects' ).
    DATA(lt_data) = parse_table( issues ).

    mt_result = lt_data.
  ENDMETHOD.


  METHOD parse_row.

    DATA(node) = i_row->get_first_child( ).
    WHILE node IS BOUND.
      DATA(val) = CAST if_ixml_element( node ).
      APPEND VALUE cell(
          row   = i_rownum
          field = val->get_name( )
          value = val->get_value( )
      ) TO lt_data.
      node = node->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_table.


    DATA(row) = CAST if_ixml_element( i_issues->get_first_child( ) ).
    DATA(rownum) = 1.
    WHILE row IS BOUND.
      parse_row( EXPORTING
            i_row    = row
            i_rownum = rownum
            CHANGING
                lt_data = lt_data ).
      ADD 1 TO rownum.
      row = CAST if_ixml_element( row->get_next( ) ).
    ENDWHILE.

  ENDMETHOD.


  METHOD set_metadata.

    mt_metadata = i_metadata.
  ENDMETHOD.


  METHOD test.
    DATA(srv) = NEW zcl_bc_web_service( host = 'easyredmine-test.internal.xc' path = '/projects.xml' ).
    srv->set_metadata( VALUE zcl_bc_xml_to_data=>tt_meta_to_data(
        ( parent = 'projects'   child = 'project' is_table = abap_true )
        ( parent = 'project'    child = 'id'                            )
        ( parent = 'project'    child = 'name'                          )
        ( parent = 'project'    child = 'identifier'                    )
        ( parent = 'project'    child = 'description'                   )
        ( parent = 'project'    child = 'homepage'                      )
        ( parent = 'project'    child = 'parent'                        )
        ( parent = 'project'    child = 'status'                        )
        ( parent = 'project'    child = 'is_public'                     )
        ( parent = 'project'    child = 'easy_is_easy_template'         )
        ( parent = 'project'    child = 'easy_due_date'                 )
        ( parent = 'project'    child = 'easy_external_id'              )
        ( parent = 'project'    child = 'author'                        )
        ( parent = 'project'    child = 'sum_time_entries'              )
        ( parent = 'project'    child = 'sum_estimated_hours'           )
        ( parent = 'project'    child = 'custom_fields'                 )
        ( parent = 'project'    child = 'created_on'                    )
        ( parent = 'project'    child = 'updated_on'                    )
        ( parent = 'project'    child = 'start_date'                    )
        ( parent = 'project'    child = 'due_date'                      )
          )  ).
    srv->exec( ).
    srv->get_data( CHANGING cs_tab = lt_tab ).
  ENDMETHOD.
ENDCLASS.
