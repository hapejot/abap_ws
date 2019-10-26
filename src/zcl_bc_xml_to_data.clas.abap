CLASS zcl_bc_xml_to_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_meta_to_xml,
        parent   TYPE string,
        child    TYPE string,
        is_table TYPE abap_bool,
        rownum   TYPE i,
        tab      TYPE i,
      END OF ts_meta_to_xml,
      tt_meta_to_xml TYPE STANDARD TABLE OF ts_meta_to_xml WITH EMPTY KEY,
      BEGIN OF ts_meta_to_data,
        parent   TYPE string,
        child    TYPE string,
        is_table TYPE abap_bool,
        xpath    TYPE string,
      END OF ts_meta_to_data,
      tt_meta_to_data TYPE STANDARD TABLE OF ts_meta_to_data WITH EMPTY KEY,
      BEGIN OF cell,
        tab   TYPE i,
        row   TYPE i,
        field TYPE string,
        value TYPE string,
      END OF cell,
      ty_data TYPE STANDARD TABLE OF cell WITH KEY row field.

    TYPES:
      BEGIN OF ts_meta,
        parent   TYPE string,
        child    TYPE string,
        is_table TYPE abap_bool,
      END OF ts_meta,
      tt_meta TYPE STANDARD TABLE OF ts_meta WITH EMPTY KEY.

    METHODS:     set_source
      IMPORTING
        i_cdata TYPE string,
      set_metadata
        IMPORTING meta TYPE tt_meta_to_data,
      convert.
  PROTECTED SECTION.
    METHODS:
      handle_result_doc,
      parse_row IMPORTING i_row    TYPE REF TO if_ixml_element
                          i_tab    TYPE i
                          i_rownum TYPE i,
      parse_table.
  PRIVATE SECTION.
    DATA:
      m_cdata     TYPE string,
      tab         TYPE i,
      doc         TYPE REF TO cl_xml_document,
      mt_meta     TYPE zcl_bc_xml_to_data=>tt_meta_to_data,
      mt_tab_data TYPE  ty_data.
    METHODS extract_element
      IMPORTING
        io_element TYPE REF TO if_ixml_node
        iv_parent  TYPE string.



ENDCLASS.



CLASS zcl_bc_xml_to_data IMPLEMENTATION.


  METHOD convert.
    doc = NEW cl_xml_document( ).
    doc->parse_string( stream = m_cdata ).
    handle_result_doc( ).
  ENDMETHOD.


  METHOD extract_element.
    DATA(children) = io_element->get_children( ).
    DO children->get_length( ) TIMES.
      DATA(child) = children->get_item( index = sy-index - 1 ).
      IF child IS INITIAL.
        EXIT.
      ENDIF.
      DATA(cname) = child->get_name( ).
      DATA(xpath) = |{ iv_parent }/{ cname }|.
      DATA(meta) = REF #( mt_meta[ xpath = xpath ] OPTIONAL ).
      IF meta IS INITIAL.
        APPEND VALUE #( xpath = xpath ) TO mt_meta.
      ENDIF.
      extract_element(
        EXPORTING
          io_element = child
          iv_parent  = xpath
      ).
    ENDDO.

    DATA(attrs) = io_element->get_attributes( ).
    IF attrs IS BOUND.
      DO.
        DATA(attr) = attrs->get_item( sy-index - 1 ).
        IF attr IS INITIAL.
          EXIT.
        ENDIF.
        cname = attr->get_name( ).
        xpath = |{ iv_parent }/@{ cname }|.
        meta = REF #( mt_meta[ xpath = xpath ] OPTIONAL ).
        IF meta IS INITIAL.
          APPEND VALUE #( xpath = xpath ) TO mt_meta.
        ENDIF.
      ENDDO.
    ENDIF.

  ENDMETHOD.


  METHOD handle_result_doc.
    TYPES:BEGIN OF ts_row,
            id   TYPE string,
            name TYPE string,
          END OF ts_row.
    DATA:
      rows   TYPE STANDARD TABLE OF ts_row,
      l_size TYPE sytabix,
      l_rc   TYPE sysubrc,
      meta   TYPE REF TO zcl_bc_xml_to_data=>ts_meta_to_data.

    DATA(first) = doc->get_first_node( ).
    DATA(name) = first->get_name( ).
    extract_element( io_element = first
                     iv_parent  = name ).
  ENDMETHOD.


  METHOD parse_row.

    DATA(name) = i_row->get_name( ).
    DATA(node) = i_row->get_first_child( ).
    WHILE node IS BOUND.
      DATA(val) = CAST if_ixml_element( node ).
      DATA(cname) = val->get_name( ).
      DATA(meta) = REF #( mt_meta[ parent = name child = cname ] OPTIONAL ).
      IF meta IS NOT BOUND.
        APPEND VALUE #( parent = name child = cname ) TO mt_meta.
      ENDIF.
      APPEND VALUE cell(
          tab   = i_tab
          row   = i_rownum
          field = cname
          value = val->get_value( )
      ) TO mt_tab_data.
      node = node->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD parse_table.


*    DATA(row) = CAST if_ixml_element( i_issues->get_first_child( ) ).
*    DATA(rownum) = 1.
*    WHILE row IS BOUND.
*      parse_row( EXPORTING
*            i_row    = row
*            i_rownum = rownum
*            CHANGING
*                lt_data = lt_data ).
*      ADD 1 TO rownum.
*      row = CAST if_ixml_element( row->get_next( ) ).
*    ENDWHILE.

  ENDMETHOD.


  METHOD set_metadata.
    mt_meta = meta.
  ENDMETHOD.


  METHOD set_source.
    m_cdata = i_cdata.
  ENDMETHOD.
ENDCLASS.
