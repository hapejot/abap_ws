CLASS zcl_bc_pjl_trace DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_exclude_class TYPE RANGE OF classname,
          mv_key           TYPE satr_tab_key,
          ms_header        TYPE satrh_header,
          mt_zeiten        TYPE satr_zeiten,
          mt_aggtab        TYPE satr_t_agg_out,
          mt_traceprog     TYPE satr_t_traceprog,
          mt_tracetext     TYPE satr_t_tracetext,
          mt_tracemeth     TYPE satr_t_tracemeth,
          mt_austab        TYPE satr_t_austab_gesamt,
          mt_hit1          TYPE satr_t_hit1,
          mt_hit2          TYPE satr_t_hit2,
          mt_tracestat     TYPE satr_t_tracestat,
          mt_header_i      TYPE satri_info,
          mt_traceitab     TYPE satr_t_itab,
          mt_tracepaket    TYPE satr_t_tracepaket,
          mt_austab_n      TYPE satr_t_austab_n,
          mt_intf_names    TYPE slat_t_interfaces,
          mt_intf_list     TYPE slat_t_intf_list,
          mt_slat_paket    TYPE slat_t_packages,
          mt_slat_programs TYPE slat_t_programs,
          mt_dir           TYPE satr_directory_table,
          ms_current_entry TYPE satr_directory.
    METHODS get_dir
      IMPORTING
        i_out TYPE REF TO if_oo_adt_classrun_out.
ENDCLASS.



CLASS ZCL_BC_PJL_TRACE IMPLEMENTATION.


  METHOD get_dir.

    mt_dir = cl_abap_trace_data=>get_directory( ).
    i_out->write( mt_dir ).

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA: l_num TYPE numc06 VALUE 1,
          l_row LIKE LINE OF mt_austab_n,
          BEGIN OF ls_key,
            cprog TYPE sycprog,
            datum TYPE sydatum,
            uzeit TYPE syuzeit,
          END OF ls_key.
    mt_exclude_class = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZCL_BC_BAPI_MAPPER' ) ).
    get_dir( out ).

    ms_current_entry = mt_dir[ satr_key+2(6) = l_num ].

    TRY.
        ls_key = ms_current_entry-satr_key.
        mv_key = CORRESPONDING #( ls_key ).
        cl_abap_trace_data=>get_data(
          EXPORTING
            i_satr_key      = mv_key
          IMPORTING
            i_header        = ms_header
            i_zeiten        = mt_zeiten
            i_aggtab        = mt_aggtab
            i_traceprog     = mt_traceprog
            i_tracetext     = mt_tracetext
            i_tracemeth     = mt_tracemeth
            i_austab        = mt_austab
            i_hit1          = mt_hit1
            i_hit2          = mt_hit2
            i_tracestat     = mt_tracestat
            i_header_i      = mt_header_i
            i_traceitab     = mt_traceitab
            i_tracepaket    = mt_tracepaket
            i_austab_n      = mt_austab_n
            i_intf_names    = mt_intf_names
            i_intf_list     = mt_intf_list
            i_slat_paket    = mt_slat_paket
            i_slat_programs = mt_slat_programs
        ).
      CATCH cx_sat_tracedata_persistence.
        "handle exception
    ENDTRY.
    DATA(num) = 100000.
    LOOP AT mt_austab_n INTO l_row.
      DATA(prog) = mt_traceprog[ l_row-progindex ].
      IF prog-paket CP 'Z*'.
*        out->write( l_row ).
*        DATA(caller) =  l_austab_n[ l_row-caller ].
        IF prog-clsname IN mt_exclude_class.
          CONTINUE.
        ENDIF.
        DATA(caller_object) = |{ prog-clsname }|.
        DATA(txt) = VALUE #( mt_tracetext[ l_row-textindex ]-tracetext OPTIONAL ).
        DATA(target_object) = |{ txt }|.
        DATA(d) = COND string(  WHEN l_row-event = '>' THEN `->` ELSE `<-` ).
        CASE l_row-id.
          WHEN 'm'. " method call
            CASE l_row-event.
              WHEN '>'.
                out->write( |{ caller_object } { d } { target_object } : { mt_tracemeth[ l_row-methindex ]-methode }| ).
            ENDCASE.
*          WHEN 'S'. " db
*            out->write( |{ caller_object } { d } DB :  { l_row-subid } { l_tracetext[ l_row-textindex ]-tracetext }| ).
        ENDCASE.
        num = num - 1.
      ENDIF.
      IF num = 0. EXIT. ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.