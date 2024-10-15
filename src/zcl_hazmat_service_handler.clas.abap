**********************************************************************
*  Copyright 2024 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************
CLASS zcl_hazmat_service_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_input_parameters,
        name TYPE string,
        type TYPE string,

      END OF ty_input_parameters .
    TYPES:
      tt_input_parameters TYPE STANDARD TABLE OF ty_input_parameters WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_datastore_for_rag,
        name TYPE string,

      END OF ty_datastore_for_rag .
    TYPES:
      tt_datastore_for_rag TYPE STANDARD TABLE OF ty_datastore_for_rag WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_prompt_json,
        category         TYPE string,
        prompt_id        TYPE string,
        prompt           TYPE string,
        input_parameters TYPE tt_input_parameters,
        datastore_fo_rag TYPE tt_datastore_for_rag,
        workflow_id      TYPE string,
        prompt_enabled   TYPE string,

      END OF ty_prompt_json .

    TYPES:
      BEGIN OF t_post_payload,
        prompt_id   TYPE string,
        prompt      TYPE string,
        workflow_id TYPE string,
        product     TYPE string,
        image_blob  TYPE string,
      END OF t_post_payload.

    INTERFACES if_http_extension .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ltyp_s_error,
        status_code         TYPE i,
        status_text         TYPE string,
        msgid               LIKE sy-msgid,
        msgty               LIKE sy-msgty,
        msgno               LIKE sy-msgno,
        msgv1               LIKE sy-msgv1,
        msgv2               LIKE sy-msgv2,
        msgv3               LIKE sy-msgv3,
        msgv4               LIKE sy-msgv4,
        set_authenticate(1) TYPE c,
      END OF ltyp_s_error .
  types:
    BEGIN OF ltyp_s_parameter,
        name  TYPE string,
        value TYPE string,
        usage TYPE i,
      END OF ltyp_s_parameter .
  types:
    ltyp_t_parameters TYPE STANDARD TABLE OF ltyp_s_parameter .

  data GO_SERVER type ref to IF_HTTP_SERVER .
  data GV_MODE type STRING .
  data GV_MANDT type SYMANDT .
  constants C_PARAMETER_UNKNOWN type I value 0 ##NO_TEXT.
  constants C_PARAMETER_USED type I value 1 ##NO_TEXT.
  data GV_COMMAND type STRING .
  data GT_PARAMETERS type LTYP_T_PARAMETERS .
  data GS_ERROR type LTYP_S_ERROR .
  data GV_SP type STRING .
  data GV_WF_ID type STRING .
  data GV_GCS_BUCKET type STRING .

  methods PARSE_URI .
  methods URL_HEX_DECODE
    importing
      !IV_VALUE type STRING
    returning
      value(RV_RESULT) type STRING .
  methods GET_PROMPTS
    returning
      value(RV_PROMPT_JSON) type STRING
    raising
      /GOOG/CX_SDK .
  methods GET_PRODUCTS
    returning
      value(RV_PRODUCTS_JSON) type STRING .
ENDCLASS.



CLASS ZCL_HAZMAT_SERVICE_HANDLER IMPLEMENTATION.


  METHOD get_products.

    SELECT matnr AS product_id,
           maktx AS product_description
      FROM zhazmat_products
        INTO TABLE @DATA(lt_products).

    rv_products_json = /ui2/cl_json=>serialize( data = lt_products pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

  ENDMETHOD.


  METHOD get_prompts.

    DATA: lv_xfile TYPE xstring.

    " Create client for accessing Storage API
    DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = 'DEMO_AIPLATFORM' ).

    lo_client->add_common_qparam( iv_name  = 'alt'
                                  iv_value = 'media' ).

    " Get All prompts saved in GCS bucket
    DATA(lv_p_object) = |{ zcl_hazmat_gcs_data_ingester=>c_source_gcs_folder-prompts }/prompts.json|.

    TRY.

        lo_client->get_objects(
          EXPORTING
            iv_p_bucket = gv_gcs_bucket
            iv_p_object = lv_p_object
          IMPORTING
            ev_ret_code = DATA(lv_ret_code)
            ev_err_text = DATA(lv_err_text)
            es_raw      = lv_xfile ).

      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        DATA(lv_msg) = lo_exception->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    IF lo_client->is_success( lv_ret_code ).
      " Convert the file data to string
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring  = lv_xfile
          im_encoding = 'UTF-8'
        IMPORTING
          ex_string   = rv_prompt_json.
    ELSE.
      lv_msg = lv_ret_code && ':' && lv_err_text.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.

    DATA lv_not_implemented type string.
    lv_not_implemented = |Processing workflow of this prompt is not implemented and it's for you to explore. | &&
                         cl_abap_char_utilities=>newline &&
                         |Please take  this prototype to the next level by cnding your own "Prompt Processing Workflow" and don't forget to have fun! 💥|.

    " 1. Get prompt repository (Using GCS)
    " 2. Process prompts with parameters: Prompt structure: Prompt ID, Workflow ID, Product ID etc.

    go_server = server.
    go_server->response->if_http_entity~suppress_content_type( ).

    parse_uri( ).
    TRY.
        CASE gv_mode.
          WHEN 'GET'.
            CASE gv_command.
                " Get list of all prompts stored in GCS
              WHEN 'getPromptRepo'.

                server->response->set_cdata(
                  EXPORTING
                    data = get_prompts( ) ).

                " Get list of all products stored in Database table
              WHEN 'getProducts'.
                server->response->set_cdata(
                  EXPORTING
                    data = get_products( ) ).
              WHEN OTHERS.

            ENDCASE.

          WHEN 'POST'.

            CASE gv_command.
                " Process the incoming prompt
              WHEN 'processPrompt'.
                DATA(lo_entity) = go_server->request.
                DATA(lv_cdata) = lo_entity->get_cdata( ).
                DATA(ls_post_payload) = VALUE t_post_payload( ).

                CALL METHOD /goog/cl_json=>deserialize
                  EXPORTING
                    json        = lv_cdata
                    pretty_name = /goog/cl_json=>pretty_mode-camel_case
                  CHANGING
                    data        = ls_post_payload.

                " Based on the workflow id, call the specific workflow for retrieval
                CASE ls_post_payload-workflow_id.
                  WHEN 'W-SDS-001'.
                    server->response->set_cdata(
                      EXPORTING
                        data = zcl_hazmat_workflow_handler=>wf_get_sds_data( iv_prompt = ls_post_payload-prompt iv_gcs_bucket = gv_gcs_bucket ) ).
                  WHEN 'W-PIC-001'.
                    server->response->set_cdata(
                      EXPORTING
                        data = zcl_hazmat_workflow_handler=>wf_get_pictogram_data( iv_prompt = ls_post_payload-prompt iv_image_base64 = ls_post_payload-image_blob iv_gcs_bucket = gv_gcs_bucket ) ).
                  WHEN 'W-FUN-001'.
                    server->response->set_cdata(
                      EXPORTING
                        data = zcl_hazmat_workflow_handler=>wf_func_calling( iv_prompt = ls_post_payload-prompt iv_gcs_bucket = gv_gcs_bucket ) ).
                  WHEN 'W-WSG-004'.
                    server->response->set_cdata(
                      EXPORTING
                        data = zcl_hazmat_workflow_handler=>wf_get_wsg_data( iv_prompt = ls_post_payload-prompt iv_gcs_bucket = gv_gcs_bucket ) ).
                  WHEN OTHERS.
                    server->response->set_cdata( EXPORTING data = lv_not_implemented ).
                ENDCASE.


              WHEN OTHERS.

            ENDCASE.

        ENDCASE.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        DATA(lv_msg) = lo_cx_sdk->get_text( ).

    ENDTRY.

  ENDMETHOD.


  METHOD parse_uri.

    DATA: lo_scms_scan_obj  TYPE REF TO cl_scms_virus_scan_info.

    DATA: lv_uri            TYPE string,
          lv_script         TYPE string,
          lv_params         TYPE string,
          lv_path           TYPE string,
          lv_dummy          TYPE string ##NEEDED,
          lv_extension      TYPE string,
          lv_ext1           TYPE string ##NEEDED,
          lv_ext2           TYPE string ##NEEDED,
          lv_ext3           TYPE string ##NEEDED,
          lv_scanperformed1 TYPE string,
          lv_index          TYPE i,
          lv_scanperformed  TYPE c LENGTH 1,
          lv_key            TYPE string.

    DATA: ls_parameter TYPE ltyp_s_parameter,
          ls_param     TYPE ltyp_s_parameter.

    gv_mode = go_server->request->get_header_field( '~REQUEST_METHOD' ).

    lv_uri    = go_server->request->get_header_field( '~REQUEST_URI' ).
    lv_script = go_server->request->get_header_field( '~SCRIPT_NAME' ).

    CLEAR:
          gv_command,
          gt_parameters.

    SPLIT lv_uri AT '/' INTO TABLE DATA(lt_tokens).
    DATA(lv_uri_method) = lt_tokens[ lines( lt_tokens ) ].

    SPLIT lv_uri_method AT '?' INTO gv_command lv_params.
    SPLIT lv_params AT '=' INTO lv_key gv_gcs_bucket.

    IF gv_gcs_bucket is INITIAL.
      gv_gcs_bucket = zif_hazmat_constants=>c_app_bucket_name.
    ENDIF.


  endmethod.


  METHOD url_hex_decode.

    DATA lv_pos TYPE i.
    DATA lv_len TYPE i.
    DATA lv_c    TYPE c LENGTH 1.
    DATA lv_rest TYPE i.
    DATA lv_x    TYPE x LENGTH 1.

    IF NOT iv_value CA '%'.
      rv_result = iv_value.
      EXIT.
    ENDIF.

    lv_pos = 0.
    lv_len = strlen( iv_value ).
    CLEAR rv_result.

    WHILE lv_pos < lv_len.
      lv_c = iv_value+lv_pos(1).
      IF lv_c = '%'.
        lv_pos = lv_pos + 1.
        lv_rest = lv_len - lv_pos.
        IF lv_rest < 2.
          gs_error-status_code = '400'.
          EXIT.
        ENDIF.
        lv_x = iv_value+lv_pos(2).
        lv_pos = lv_pos + 2.

        CALL FUNCTION 'SCMS_BIN_TO_TEXT'
          EXPORTING
            bin_line  = lv_x
          IMPORTING
            text_line = lv_c
          EXCEPTIONS
            failed    = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
          gs_error-status_code = 500.
        ENDIF.
      ELSE.
        lv_pos = lv_pos + 1.
      ENDIF.
      IF lv_c IS INITIAL.
        CONCATENATE rv_result gv_sp INTO rv_result.
      ELSE.
        CONCATENATE rv_result lv_c INTO rv_result.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
