************************************************************************************************************************
* Copyright 2024 Google LLC                                                                                            *
* ABAP SDK for Google Cloud is made available as "Software" under the agreement governing your use of                  *
* Google Cloud Platform including the Service Specific Terms available at                                              *
*                                                                                                                      *
* https://cloud.google.com/terms/service-terms                                                                         *
*                                                                                                                      *
* Without limiting the generality of the above terms, you may not modify or distribute ABAP SDK for Google Cloud       *
* without express written permission from Google.                                                                      *
************************************************************************************************************************
CLASS lcl_hazmat_application DEFINITION.

  PUBLIC SECTION.
    METHODS:
      get_prompt_f4   RETURNING VALUE(rv_prompt) TYPE string,
      get_scenario_f4 RETURNING VALUE(rv_scenario) TYPE string,
      get_product_f4 RETURNING VALUE(rv_product)  TYPE maktx,
      get_file_name_f4 RETURNING VALUE(rv_file_name) TYPE string,
      validate,
      execute,
      constructor.

  PRIVATE SECTION.

    TYPES: BEGIN OF mty_input_params,
             name TYPE string,
             type TYPE string,
           END OF mty_input_params,
           mtt_input_params TYPE STANDARD TABLE OF mty_input_params WITH DEFAULT KEY,
           BEGIN OF mty_datastore,
             name TYPE string,
           END OF mty_datastore,
           mtt_datastore TYPE STANDARD TABLE OF mty_datastore WITH DEFAULT KEY,
           BEGIN OF mty_prompt_repo,
             category        TYPE string,
             promptid        TYPE string,
             prompt          TYPE string,
             inputparameters TYPE mtt_input_params,
             datastoreforag  TYPE mtt_datastore,
             workflowid      TYPE string,
             promptenabled   TYPE string,
           END OF mty_prompt_repo,

           BEGIN OF mty_values,
             value TYPE rcm_string,
           END OF mty_values,

           BEGIN OF mty_prompt_values,
             value   TYPE rcm_string,
             enabled TYPE boolean,
           END OF mty_prompt_values.

    DATA: mt_scenarios   TYPE STANDARD TABLE OF mty_values,
          mt_prompts     TYPE STANDARD TABLE OF mty_prompt_values,
          mt_products    TYPE STANDARD TABLE OF mty_values,
          mt_prompt_repo TYPE STANDARD TABLE OF mty_prompt_repo,
          mv_image_data  TYPE string,
          mv_gcs_bucket  TYPE string.

    METHODS:
      get_prompts    IMPORTING iv_scenario       TYPE string,
      get_prompt_repo,
      get_products,
      get_scenarios,
      get_file_data RETURNING VALUE(rv_image_string) TYPE string,
      read_dynp_data.

ENDCLASS.

CLASS lcl_hazmat_application IMPLEMENTATION.

  METHOD constructor.

    get_prompt_repo( ).
    p_gcs = zif_hazmat_constants=>c_app_bucket_name.

    LOOP AT SCREEN.
      IF ( screen-group1 = 'LC2' ) OR
         ( screen-group1 = 'LC1' ).
        screen-input     = 0.
        screen-invisible = 1.
      ELSEIF screen-name = 'P_UPROM'.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD execute.
    DATA: lv_msg              TYPE string,
          lv_data             TYPE string,
          lv_image_base64     TYPE string,
          lv_prompt           TYPE string,
          lv_input_is_photo   TYPE boolean,
          lv_input_is_product TYPE boolean.

    TRY.
        DATA(ls_prompt_repo) = mt_prompt_repo[ prompt = p_prom ].

        LOOP AT ls_prompt_repo-inputparameters ASSIGNING FIELD-SYMBOL(<ls_param>).
          IF <ls_param>-name = 'Photo'.
            lv_input_is_photo = abap_true.
            mv_image_data = get_file_data( ).
            lv_image_base64 = cl_http_utility=>encode_base64( unencoded = mv_image_data ).
            lv_prompt = p_prom.
          ELSEIF <ls_param>-name = 'Product'.
            lv_input_is_product = abap_true.
            lv_prompt = p_uprom.
          ENDIF.
        ENDLOOP.

        mv_gcs_bucket = p_gcs.

        CASE ls_prompt_repo-workflowid.
          WHEN 'W-SDS-001'.
            lv_data = zcl_hazmat_workflow_handler=>wf_get_sds_data( iv_prompt = lv_prompt iv_gcs_bucket = mv_gcs_bucket ).
          WHEN 'W-PIC-001'.
            lv_data = zcl_hazmat_workflow_handler=>wf_get_pictogram_data( iv_prompt = lv_prompt iv_image_base64 = lv_image_base64 iv_gcs_bucket = mv_gcs_bucket ).
          WHEN 'W-FUN-001'.
            lv_data = zcl_hazmat_workflow_handler=>wf_func_calling( iv_prompt = lv_prompt iv_gcs_bucket = mv_gcs_bucket ).
          WHEN 'W-WSG-004'.
            lv_data = zcl_hazmat_workflow_handler=>wf_get_wsg_data( iv_prompt = lv_prompt iv_gcs_bucket = mv_gcs_bucket ).
        ENDCASE.


        cl_demo_output=>begin_section( 'Response from Gemini' ).
        cl_demo_output=>write_text( lv_data ).
        cl_demo_output=>display( ).

      CATCH cx_sy_itab_line_not_found.
        lv_msg = 'Prompt is not available in the prompt repo!'.
    ENDTRY.

    IF lv_msg IS NOT INITIAL.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.


  ENDMETHOD.

  METHOD validate.
    DATA: lv_msg              TYPE string,
          lv_input_is_photo   TYPE boolean,
          lv_input_is_product TYPE boolean.

    TRY.
        DATA(ls_prompt_repo) = mt_prompt_repo[ prompt = p_prom ].
        " prompt should belong to the selected scenario.
        IF ls_prompt_repo-category <> p_scen.
          lv_msg = 'Prompt does not exist in the selected business scenario!'.
        ENDIF.

        IF ls_prompt_repo-promptenabled = abap_false.
          lv_msg = 'Prompt is not enabled!'.
        ENDIF.
        LOOP AT ls_prompt_repo-inputparameters ASSIGNING FIELD-SYMBOL(<ls_param>).
          IF <ls_param>-name = 'Photo'.
            lv_input_is_photo = abap_true.
          ELSEIF <ls_param>-name = 'Product'.
            lv_input_is_product = abap_true.
            IF p_prod IS NOT INITIAL AND p_prom IS NOT INITIAL.
              p_uprom = p_prom.
              REPLACE ALL OCCURRENCES OF '<Product>' IN p_uprom WITH p_prod.
            ENDIF.
          ENDIF.
        ENDLOOP.

        LOOP AT SCREEN.
          IF ( lv_input_is_product = abap_true AND screen-group1 = 'LC2' ) OR
             ( lv_input_is_photo   = abap_true AND ( screen-group1 = 'LC1' ) ).
            screen-input     = 0.
            screen-invisible = 1.
          ENDIF.
          IF lv_input_is_product = abap_true AND screen-name = 'P_UPROM'.
            screen-input     = 0.
          ELSEIF lv_input_is_photo = abap_true AND screen-group1 = 'LC3' .
            screen-invisible = 1.
            screen-input     = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found.
        IF p_prom IS NOT INITIAL.
          lv_msg = 'Prompt is not available in the prompt repo!'.
        ENDIF.
    ENDTRY.

    IF lv_msg IS NOT INITIAL.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.


  ENDMETHOD.

  METHOD get_file_data.

    DATA: lt_data    TYPE STANDARD TABLE OF x255,
          lv_content TYPE string,
          lv_length  TYPE i.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = p_imag    " Name of file
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length   " File length
      CHANGING
        data_tab                = lt_data    " Transfer table for file contents
      EXCEPTIONS
        OTHERS                  = 19 ).

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length  = lv_length
      IMPORTING
        text_buffer   = lv_content
*        output_length =
      TABLES
        binary_tab    = lt_data
      EXCEPTIONS
        failed        = 1
        others        = 2
      .
    IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
*    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*      EXPORTING
*        input_length = lv_length
*      IMPORTING
*        buffer       = lv_content
*      TABLES
*        binary_tab   = lt_data
*      EXCEPTIONS
*        failed       = 1
*        OTHERS       = 2.
*


*    IF sy-subrc = 0.
      rv_image_string = lv_content.
*    ENDIF.

  ENDMETHOD.

  METHOD get_file_name_f4.

    DATA: lt_filetab TYPE filetable,
          lv_rc      TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      CHANGING
        file_table              = lt_filetab
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1                " "Open File" dialog failed
        cntl_error              = 2                " Control error
        error_no_gui            = 3                " No GUI available
        not_supported_by_gui    = 4                " GUI does not support this
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      TRY.
          rv_file_name = lt_filetab[ 1 ].
        CATCH cx_sy_itab_line_not_found.
          MESSAGE 'Please select a file'(001) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD get_products.

    IF mt_products IS INITIAL.
      SELECT maktx AS value
        FROM zhazmat_products
          INTO CORRESPONDING FIELDS OF TABLE @mt_products
        ORDER BY value.

      IF sy-subrc = 0.
        DELETE ADJACENT DUPLICATES FROM mt_products COMPARING value.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_prompts.
    DATA(lt_repo) = mt_prompt_repo.
    DELETE lt_repo WHERE category <> iv_scenario.
    mt_prompts = VALUE #( FOR ls_prompt_repo IN lt_repo ( value = ls_prompt_repo-prompt
                                                        enabled = SWITCH #( ls_prompt_repo-promptenabled
                                                                            WHEN 'Yes' THEN abap_true
                                                                            ELSE abap_false )
                                                                            ) ).
    SORT mt_prompts BY enabled DESCENDING value.
    DELETE ADJACENT DUPLICATES FROM mt_prompts COMPARING value.
  ENDMETHOD.

  METHOD get_prompt_repo.

    DATA: lv_xfile       TYPE xstring,
          lv_prompt_json TYPE string.

    " Create client for accessing Storage API
    DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = 'DEMO_AIPLATFORM' ).

    lo_client->add_common_qparam( iv_name  = 'alt'
                                  iv_value = 'media' ).

    " Get All prompts saved in GCS bucket
    DATA(lv_p_object) = |{ zcl_hazmat_gcs_data_ingester=>c_source_gcs_folder-prompts }/prompts.json|.

    TRY.

        lo_client->get_objects(
          EXPORTING
            iv_p_bucket = zif_hazmat_constants=>c_app_bucket_name
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
          ex_string   = lv_prompt_json.

      /goog/cl_json=>deserialize(
        EXPORTING
          json             = lv_prompt_json
          CHANGING
          data             = mt_prompt_repo
      ).


    ELSE.
      lv_msg = lv_ret_code && ':' && lv_err_text.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD read_dynp_data.
    DATA:lt_dynpdata TYPE TABLE OF dynpread,
         ls_dynpdata TYPE dynpread.
    FIELD-SYMBOLS: <ls_dynpdata> TYPE dynpread.
    CONSTANTS: lc_param_prompt   TYPE string VALUE 'P_PROM',
               lc_param_scenario TYPE string VALUE 'P_SCEN',
               lc_param_product  TYPE string VALUE 'P_PROD'.

    ls_dynpdata-fieldname = lc_param_scenario.
    APPEND ls_dynpdata TO lt_dynpdata.

    ls_dynpdata-fieldname = lc_param_prompt.
    APPEND ls_dynpdata TO lt_dynpdata.

    ls_dynpdata-fieldname = lc_param_product.
    APPEND ls_dynpdata TO lt_dynpdata.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = lt_dynpdata
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc <> 0.
      MESSAGE 'Internal Error occured while reading the dynro values'(014) TYPE 'E'.
    ENDIF.
    LOOP AT lt_dynpdata ASSIGNING <ls_dynpdata>.
      IF <ls_dynpdata>-fieldname = lc_param_product.
        p_prod = <ls_dynpdata>-fieldvalue.
      ELSEIF <ls_dynpdata>-fieldname = lc_param_prompt.
        p_prom = <ls_dynpdata>-fieldvalue.
      ELSEIF <ls_dynpdata>-fieldname = lc_param_scenario.
        p_scen = <ls_dynpdata>-fieldvalue.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_product_f4.

    DATA: lt_ret_tab   TYPE STANDARD TABLE OF ddshretval,
          lt_field_tab TYPE STANDARD TABLE OF dfies,
          ls_ret_tab   TYPE ddshretval,
          lv_value     TYPE help_info-fldvalue.

    get_products( ).
    read_dynp_data( ).
    lv_value = p_prod.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'VALUE'
        value_org       = 'S'
        value           = lv_value
      TABLES
        value_tab       = mt_products
        field_tab       = lt_field_tab
        return_tab      = lt_ret_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE lt_ret_tab INTO ls_ret_tab INDEX 1.
      IF sy-subrc = 0.
        rv_product = ls_ret_tab-fieldval.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_prompt_f4.

    DATA: lt_ret_tab   TYPE STANDARD TABLE OF ddshretval,
          lt_field_tab TYPE STANDARD TABLE OF dfies,
          ls_ret_tab   TYPE ddshretval,
          lv_value     TYPE help_info-fldvalue.

    read_dynp_data( ).

    IF p_scen IS INITIAL.
      MESSAGE 'Please provide Business scenario' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    get_prompts( iv_scenario = p_scen ).
    lv_value = p_prom.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'VALUE'
        value_org       = 'S'
        value           = lv_value
      TABLES
        value_tab       = mt_prompts
        field_tab       = lt_field_tab
        return_tab      = lt_ret_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE lt_ret_tab INTO ls_ret_tab INDEX 1.
      IF sy-subrc = 0.
        rv_prompt = ls_ret_tab-fieldval.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_scenario_f4.

    DATA: lt_ret_tab   TYPE STANDARD TABLE OF ddshretval,
          lt_field_tab TYPE STANDARD TABLE OF dfies,
          ls_ret_tab   TYPE ddshretval,
          lv_value     TYPE help_info-fldvalue.

    get_scenarios( ).
    read_dynp_data( ).
    lv_value = p_scen.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'VALUE'
        value_org       = 'S'
        value           = lv_value
      TABLES
        value_tab       = mt_scenarios
        field_tab       = lt_field_tab
        return_tab      = lt_ret_tab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE lt_ret_tab INTO ls_ret_tab INDEX 1.
      IF sy-subrc = 0.
        rv_scenario = ls_ret_tab-fieldval.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_scenarios.
    IF mt_scenarios IS INITIAL.
      mt_scenarios = VALUE #( FOR ls_prompt_repo IN mt_prompt_repo ( value = ls_prompt_repo-category ) ).
      SORT mt_scenarios BY value.
      DELETE ADJACENT DUPLICATES FROM mt_scenarios COMPARING value.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
