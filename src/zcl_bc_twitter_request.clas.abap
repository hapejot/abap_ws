CLASS zcl_bc_twitter_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_apc_wsp_event_handler.
    METHODS select_tweet
      IMPORTING
        i_number     TYPE string
      RETURNING
        VALUE(cdata) TYPE string.
    METHODS select_conversation
      IMPORTING
        i_number     TYPE string
      RETURNING
        VALUE(cdata) TYPE string.
    METHODS start_stream
              RAISING
                cx_apc_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA log_handle TYPE balloghndl.
    METHODS create_client
      IMPORTING
        i_url           TYPE string
      RETURNING
        VALUE(r_client) TYPE REF TO if_http_client.
    METHODS set_bearer_token
      IMPORTING
        request TYPE REF TO if_http_request.
    METHODS fetch_response_data
      IMPORTING
        i_client       TYPE REF TO if_http_client
      RETURNING
        VALUE(r_cdata) TYPE string.
    METHODS select_recent
      IMPORTING
        i_query      TYPE string
      RETURNING
        VALUE(cdata) TYPE string.

    DATA      message TYPE string.
ENDCLASS.



CLASS zcl_bc_twitter_request IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA: app  TYPE REF TO zcl_bc_twitter_request,
          res  TYPE string,
          json TYPE REF TO /ui5/cl_json_parser.
    CREATE OBJECT app.
*    res = app->select_tweet( |1294926394342940672| ).
*    out->write( res ).
*    out->write( json->value( path = '/data/text' ) ).

*    res = app->select_conversation( json->value( '/data/conversation_id' ) ).
*    out->write( res ).

*    json = NEW /ui5/cl_json_parser(  ).
*    res = app->select_recent( '@rki_updates' ).
*    json->parse( res ).
*    out->write( json->m_entries ).
    TRY.
        app->start_stream( ).
      CATCH cx_apc_error INTO DATA(lx_err).
        out->write( lx_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD select_conversation.
    DATA: url    TYPE string,
          client TYPE REF TO if_http_client.

*curl --request GET \
*  --url 'https://api.twitter.com/2/tweets/search/recent?query=conversation_id:1279940000004973111&tweet.fields=in_reply_to_user_id,author_id,created_at,conversation_id' \
*  --header 'Authorization: Bearer $BEARER_TOKEN'

    url = |https://api.twitter.com/2/tweets/search/recent?query=conversation_id:{ i_number }|.

    client = create_client( url ).
    set_bearer_token( client->request ).
    cdata = fetch_response_data( client ).
*    clean-up
    client->close( ).
    CLEAR client.
  ENDMETHOD.

  METHOD select_recent.
    DATA: url    TYPE string,
          client TYPE REF TO if_http_client.

*curl --request GET \
*  --url 'https://api.twitter.com/2/tweets/search/recent?query=conversation_id:1279940000004973111&tweet.fields=in_reply_to_user_id,author_id,created_at,conversation_id' \
*  --header 'Authorization: Bearer $BEARER_TOKEN'

    url = |https://api.twitter.com/2/tweets/search/recent?query={ i_query }|.

    client = create_client( url ).
    set_bearer_token( client->request ).
    cdata = fetch_response_data( client ).
*    clean-up
    client->close( ).
    CLEAR client.
  ENDMETHOD.

  METHOD start_stream.
    DATA: url    TYPE string.

*curl --request GET \
*  --url 'https://api.twitter.com/2/tweets/search/recent?query=conversation_id:1279940000004973111&tweet.fields=in_reply_to_user_id,author_id,created_at,conversation_id' \
*  --header 'Authorization: Bearer $BEARER_TOKEN'

    url = |https://api.twitter.com/2/tweets/search/stream|.

    DATA(client) = cl_apc_wsp_client_manager=>create_by_url(
                       i_url           = url
*                   i_protocol      =
                       i_event_handler = me
*                   i_proxy         =
*                   i_ssl_id        = 'ANONYM'
                   ).
    DATA(req) = client->get_context( )->get_initial_request( ).
    req->set_header_field(
      EXPORTING
        i_name  = 'Authorization'    " Name of the header field
        i_value =  'Bearer AAAAAAAAAAAAAAAAAAAAAGF8GwEAAAAAIbrwYCiOEoIRpyQ0bIROPNBMECM%3D1XkS81JPto0AXLT1j3SKDxfpFsGnwU7Oggst3mdbICrC8YdEVT'   " HTTP header field value
    ).
    client->connect(
*  EXPORTING
*    i_timeout                    = CO_TIMEOUT_DEFAULT    " Waiting time for the reponse
*    i_vscan_profile_incoming_msg =     " Virus Scan Profile
*    i_vscan_profile_outgoing_msg =     " Virus Scan Profile
    ).

    DATA(msg_manager) = CAST if_apc_wsp_message_manager( client->get_message_manager( ) ).
    DATA(msg) = CAST if_apc_wsp_message( msg_manager->create_message( ) ).

    DO 100 TIMES.
      " wait for incoming message sent by peer
      CLEAR message.
      WAIT FOR PUSH CHANNELS UNTIL message IS NOT INITIAL UP TO 10 SECONDS.
      IF sy-subrc = 8.
        WRITE: / 'Timeout occured !'.
      ELSE.
        WRITE: / 'Received message: ', message.
      ENDIF.
    ENDDO.


*    set_bearer_token( client ).
*    cdata = fetch_response_data( client ).
*    clean-up
*    client->close( ).
*    CLEAR client.
  ENDMETHOD.

  METHOD select_tweet.
    DATA: url    TYPE string,
          client TYPE REF TO if_http_client.

    url = |https://api.twitter.com/2/tweets/{ i_number }?tweet.fields=conversation_id&expansions=referenced_tweets.id|.

    client = create_client( url ).
    set_bearer_token( client->request ).
    cdata = fetch_response_data( client ).
*    clean-up
    client->close( ).
    CLEAR client.

  ENDMETHOD.

  METHOD fetch_response_data.

    CALL METHOD i_client->send
*      EXPORTING
*        timeout                    = CO_TIMEOUT_DEFAULT    " Timeout von Antwortwartezeit
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    CALL METHOD i_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    r_cdata = i_client->response->get_cdata( ).


  ENDMETHOD.



  METHOD set_bearer_token.

    request->if_http_entity~set_header_field(
      EXPORTING
        name  = 'Authorization'    " Name of the header field
        value =  'Bearer AAAAAAAAAAAAAAAAAAAAAGF8GwEAAAAAIbrwYCiOEoIRpyQ0bIROPNBMECM%3D1XkS81JPto0AXLT1j3SKDxfpFsGnwU7Oggst3mdbICrC8YdEVT'   " HTTP header field value
    ).

  ENDMETHOD.



  METHOD create_client.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = i_url   " URL
      IMPORTING
        client             = r_client   " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.


  ENDMETHOD.



  METHOD if_apc_wsp_event_handler~on_close.

  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_error.

  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_message.
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = log_handle   " Log handle
        i_msgty          = 'I'   " Message type (A, E, W, I, S)
*       i_probclass      = '4'    " Problem class (1, 2, 3, 4)
        i_text           = 'on message'   " Message data
*       i_s_context      =     " Context information for free text message
*       i_s_params       =     " Parameter set for free text message
*       i_detlevel       = '1'    " Application Log: Level of Detail
*  IMPORTING
*       e_s_msg_handle   =     " Message handle
*       e_msg_was_logged =     " Message collected
*       e_msg_was_displayed =     " Message output
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    " message handling section
    TRY.
        " save the message
        message = i_message->get_text( ).

      CATCH cx_apc_error INTO DATA(lx_apc_error).
        message = lx_apc_error->get_text( ).
    ENDTRY.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
*       i_client             = SY-MANDT    " Client in which the new log is to be saved
*       i_in_update_task     = SPACE    " Save in UPDATE TASK
*       i_save_all           = SPACE    " Save all logs in memory
        i_t_log_handle       = VALUE bal_t_logh( ( log_handle ) )    " Table of log handles
        i_2th_connection     = abap_true   " FALSE: No secondary connection
        i_2th_connect_commit = abap_true    " FALSE: No COMMIT in module
*       i_link2job           = 'X'    " Boolean Variable (X=true, -=false, space=unknown)
*      IMPORTING
*       e_new_lognumbers     =     " Table of new log numbers
*       e_second_connection  =     " Name of Secondary Connection
      EXCEPTIONS
        log_not_found        = 1
        save_not_allowed     = 2
        numbering_error      = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD if_apc_wsp_event_handler~on_open.
    DATA: log_header TYPE bal_s_log.
    log_header = VALUE #(   object = 'TWABAP'
                            subobject = 'WEBSOCKET'
                            extnumber = 'DEFAULT'    ).
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = log_header    " Log header data
      IMPORTING
        e_log_handle            = log_handle    " Log handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
