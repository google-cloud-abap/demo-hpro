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
class ZCL_HAZMAT_WORKFLOW_HANDLER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_wsg_chunk_json,
        id      TYPE string,
        content TYPE string,
      END OF ty_wsg_chunk_json .
  types:
    ty_t_float TYPE STANDARD TABLE OF f WITH DEFAULT KEY .
  types:
    BEGIN OF ty_sds_chunk_json,
        id        TYPE string,
        content   TYPE string,
        embedding TYPE ty_t_float,

      END OF ty_sds_chunk_json .

  class-methods WF_FUNC_CALLING
    importing
      !IV_PROMPT type STRING
      !IV_GCS_BUCKET type STRING
    returning
      value(RV_RESPONSE) type STRING .
  class-methods WF_GET_SDS_DATA
    importing
      !IV_PROMPT type STRING
      !IV_GCS_BUCKET type STRING
    returning
      value(RV_RESPONSE) type STRING .
  class-methods WF_GET_WSG_DATA
    importing
      !IV_PROMPT type STRING
      !IV_GCS_BUCKET type STRING
    returning
      value(RV_RESPONSE) type STRING .
  class-methods WF_GET_PICTOGRAM_DATA
    importing
      !IV_PROMPT type STRING optional
      !IV_IMAGE_BASE64 type STRING optional
      !IV_GCS_BUCKET type STRING
    returning
      value(RV_RESPONSE) type STRING .
  PROTECTED SECTION.
private section.

  class-methods GET_PRODUCT_ID
    importing
      !IV_PROMPT type STRING
    returning
      value(RV_PRODUCT_ID) type STRING .
  class-methods GET_WSG_CHUNK_DATA
    importing
      !IV_WSG_CHUNK_ID type STRING
      !IV_GCS_BUCKET type STRING
    returning
      value(RV_CHUNK_TEXT) type STRING .
  class-methods GET_PICTOGRAM_DATA
    importing
      !IV_PICTOGRAM_ID type STRING
      !IV_GCS_BUCKET type STRING
    returning
      value(RV_PICTOGRAM_DESCRIPTION) type STRING .
  class-methods GET_SDS_CHUNK_DATA
    importing
      !IV_SDS_CHUNK_ID type STRING
      !IV_GCS_BUCKET type STRING
    returning
      value(RV_CHUNK_TEXT) type STRING .
ENDCLASS.



CLASS ZCL_HAZMAT_WORKFLOW_HANDLER IMPLEMENTATION.


  METHOD get_pictogram_data.

    DATA: lv_xfile  TYPE xstring.

    TRY.
        " Create client for accessing Storage API
        DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = zif_hazmat_constants=>c_client_key ).

        lo_client->add_common_qparam( iv_name  = 'alt'
                                      iv_value = 'media' ).

        " Get the pictogram description stored in GCS bucket
        DATA(lv_p_object) = |{ zcl_hazmat_gcs_data_ingester=>c_source_gcs_folder-pict_desc }/{ iv_pictogram_id }.txt|.

        lo_client->get_objects(
          EXPORTING
            iv_p_bucket = iv_gcs_bucket
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
      "Convert Xstring response/File data to string data
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring  = lv_xfile
          im_encoding = 'UTF-8'
        IMPORTING
          ex_string   = rv_pictogram_description.
    ELSE.
      lv_msg = lv_ret_code && ':' && lv_err_text.
    ENDIF.


  ENDMETHOD.


  METHOD get_product_id.

    TRY.
        DATA(lo_vector_search_prod) = NEW /goog/cl_vector_search( iv_search_key = zif_hazmat_constants=>c_hpro_prod_sk  ). "HPRO: Vector Index for Products
        DATA(ls_nearest_product) = lo_vector_search_prod->find_neighbors_by_string( iv_search_string         = iv_prompt           " Search String
                                                                                    iv_embeddings_model_key  = zif_hazmat_constants=>c_model_key_text_embed  " Model Key for Embeddings
                                                     )->get_nearest_neighbor( ).

        rv_product_id = ls_nearest_product-datapoint_id.

      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        DATA(lv_msg) = lo_exception->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_sds_chunk_data.

    DATA: lv_xfile          TYPE xstring,
          ls_sds_chunk_data TYPE ty_sds_chunk_json,
          lv_string         TYPE string.


    TRY.
        " Create client for accessing Storage API
        DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = zif_hazmat_constants=>c_client_key ).

        lo_client->add_common_qparam( iv_name  = 'alt'
                                      iv_value = 'media' ).

        " Get the SDS chunk stored as embeddings in GCS bucket
        DATA(lv_p_object) = |{ zcl_hazmat_gcs_data_ingester=>c_target_gcs_folder-sds }/{ iv_sds_chunk_id }.json|.


        lo_client->get_objects(
          EXPORTING
            iv_p_bucket = iv_gcs_bucket
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
          ex_string   = lv_string.
      /goog/cl_json_util=>deserialize_json( EXPORTING iv_json          = lv_string
                                                      iv_pretty_name   = /ui2/cl_json=>pretty_mode-extended
                                            IMPORTING es_data          = ls_sds_chunk_data ).
      IF ls_sds_chunk_data IS NOT INITIAL.
        rv_chunk_text = ls_sds_chunk_data-content.
      ENDIF.
    ELSE.
      lv_msg = lv_ret_code && ':' && lv_err_text.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD get_wsg_chunk_data.

    DATA: lv_xfile          TYPE xstring,
          lv_string         TYPE string,
          ls_wsg_chunk_data TYPE ty_wsg_chunk_json.

    TRY.
        " Create client for accessing Storage API
        DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = zif_hazmat_constants=>c_client_key ).

        lo_client->add_common_qparam( iv_name  = 'alt'
                                      iv_value = 'media' ).

        " Get the SDS chunk stored as embeddings in GCS bucket
        DATA(lv_p_object) = |{ zcl_hazmat_gcs_data_ingester=>c_target_gcs_folder-wsg }/{ iv_wsg_chunk_id }.json|.

        lo_client->get_objects(
          EXPORTING
            iv_p_bucket = iv_gcs_bucket
            iv_p_object = lv_p_object
          IMPORTING
            ev_ret_code = DATA(lv_ret_code)
            ev_err_text = DATA(lv_err_text)
            es_raw      = lv_xfile ).
      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        DATA(lv_msg) = lo_exception->get_text( ).
        RETURN.

    ENDTRY.

    IF lo_client->is_success( lv_ret_code ).
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring  = lv_xfile
          im_encoding = 'UTF-8'
        IMPORTING
          ex_string   = lv_string.
      /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_string
                                                      iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                            IMPORTING es_data        = ls_wsg_chunk_data ).
      IF ls_wsg_chunk_data IS NOT INITIAL.
        rv_chunk_text = ls_wsg_chunk_data-content.
      ENDIF.
    ELSE.
      lv_msg = lv_ret_code && ':' && lv_err_text.
    ENDIF.

  ENDMETHOD.


  METHOD wf_func_calling.

    DATA: lt_parameters TYPE /goog/cl_generative_model=>tt_parameter_properties.

    DATA(lv_prompt) = iv_prompt.

    " logic to identify and fetch product ID from the prompt.
    DATA(lv_product_id) = get_product_id( iv_prompt = lv_prompt ).

    " Add the product ID as context to the existing prompt.
    lv_prompt = |Product ID: "{ lv_product_id }". { lv_prompt } |.

    TRY.
        " Instantiate an object for accessing generative model
        DATA(lo_model) = NEW /goog/cl_generative_model( iv_model_key = zif_hazmat_constants=>c_model_key_gemini_flash ).

        " Specify details for parameter Product
        APPEND VALUE #( parameter_name = 'product'
                        type           = 'string'
                        description    = '6 character Product ID'
                        is_required    = abap_true ) TO lt_parameters.

        " Function calling with SAP Function Module
        DATA(lv_response) = lo_model->add_function_declaration( iv_name        = 'ZFM_HAZMAT_GET_PRODUCT_DATA'
                                                                iv_description = 'Get all information for a given product ID'
                                                                it_parameters  = lt_parameters
                                   )->set_auto_invoke_sap_function( abap_true
                                   )->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                           iv_harm_block_threshold = 'BLOCK_NONE'
                                   )->generate_content( iv_prompt_text = lv_prompt
                                   )->get_text( ).

        rv_response = lv_response.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        rv_response = lo_cx_sdk->get_text( ) .
    ENDTRY.

  ENDMETHOD.


  METHOD wf_get_pictogram_data.


    TRY.
        DATA ls_image TYPE /goog/cl_embeddings_model=>ty_image.

        " Create multimodal embeddings for the input image
        ls_image-bytes_base64_encoded = iv_image_base64.
        DATA(lt_multi_embed) = NEW /goog/cl_embeddings_model( iv_model_key = zif_hazmat_constants=>c_model_key_mm_embed
                                    )->gen_image_embeddings( iv_image = ls_image
                                    )->get_vector( ).

        " Using the generated multimodal embeddings, perform vector search on the pictorgrams vector data
        DATA(lo_vector_search_pictogram) = NEW /goog/cl_vector_search( iv_search_key = zif_hazmat_constants=>c_hpro_pictogram_sk ). "HPRO: Vector Index for SDS documents
        DATA(ls_nearest_pictogram) = lo_vector_search_pictogram->find_neighbors_by_embedding( it_embeddings = lt_multi_embed
                                                              )->get_nearest_neighbor(  ).

        " Add the searched info as additional context for search.
        " Here we get the enterprise specific descriptions for the pictogram and add that as context.
        DATA(lv_prompt) = iv_prompt && ' Respond based on the below context. Context: ' && get_pictogram_data( iv_pictogram_id = ls_nearest_pictogram-datapoint_id iv_gcs_bucket = iv_gcs_bucket ).

        " Use the RAG-ed prompt  to generate the response:
        " Note that nidel used is - 'gemini-1.0-pro-vision-001'
        DATA(lo_model) = NEW /goog/cl_generative_model( iv_model_key = zif_hazmat_constants=>c_model_key_gemini_vision ).

        lo_model->set_inline_data( iv_mime_type = 'image/gif'
                                   iv_data      = iv_image_base64 ).

        DATA(lv_response) = lo_model->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                           iv_harm_block_threshold = 'BLOCK_NONE'
                                   )->generate_content( iv_prompt_text = lv_prompt
                                   )->get_text( ).

        rv_response = lv_response.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        rv_response = lo_cx_sdk->get_text( ) .
    ENDTRY.

  ENDMETHOD.


  METHOD wf_get_sds_data.

    TRY.

        " Logic to identify and fetch product ID .
        DATA(lv_product_id) = get_product_id( iv_prompt = iv_prompt ).

        " Create a new prompt which will be used for Searching in the SDS Vector Store. This contains additional
        " context about the product ID
        DATA(lv_query_sds_chunk) = iv_prompt &&
                                   'Metadata: Product ID - ' &&
                                   lv_product_id.

        " Get SDS Chunk with additional context containing product ID
        DATA(lo_vector_search_sds) = NEW /goog/cl_vector_search( iv_search_key = zif_hazmat_constants=>c_hpro_sds_sk ). "HPRO: Vector Index for SDS documents

        DATA(ls_nearest_chunk) = lo_vector_search_sds->find_neighbors_by_string( iv_search_string         = lv_query_sds_chunk   " Search String
                                                                                 iv_embeddings_dimensions = zif_hazmat_constants=>c_text_dimension
                                                                                 iv_embeddings_model_key  = zif_hazmat_constants=>c_model_key_text_embed   " Model Key for Embeddings
                                                    )->get_nearest_neighbor(  ).

        IF ls_nearest_chunk-datapoint_id IS NOT INITIAL.
          " Create a new Prompt for generative language model using Gemini.
          " This contains an additional context with the SDS chunk data.
          DATA(lv_prompt) = iv_prompt &&
                            'Context: ' &&
                            get_sds_chunk_data( iv_sds_chunk_id = ls_nearest_chunk-datapoint_id iv_gcs_bucket = iv_gcs_bucket ).

          DATA(lo_model) = NEW /goog/cl_generative_model( iv_model_key = zif_hazmat_constants=>c_model_key_gemini_flash ).

          " Get response from API using the RAG-ed prompt.
          rv_response = lo_model->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                       iv_harm_block_threshold = 'BLOCK_NONE'
                               )->generate_content( lv_prompt
                               )->get_text( ).

        ENDIF.


      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        DATA(lv_msg) = lo_cx_sdk->get_text( ).
        rv_response = lv_msg.
    ENDTRY.
  ENDMETHOD.


  METHOD wf_get_wsg_data.

    DATA(lv_prompt) = iv_prompt.

    TRY.

        " Create a client for searching the WSG vector Index
        DATA(lo_vector_search_wsg) = NEW /goog/cl_vector_search( iv_search_key = zif_hazmat_constants=>c_hpro_wsg_sk ). "HPRO: Vector Index for SDS documents

        " Use the Prompt embedding to search the WSG Chunk
        DATA(ls_nearest_document) = lo_vector_search_wsg->find_neighbors_by_string( iv_search_string         = lv_prompt
                                                                                    IV_EMBEDDINGS_DIMENSIONS  = zif_hazmat_constants=>c_text_dimension
                                                                                    iv_embeddings_model_key  = zif_hazmat_constants=>c_model_key_text_embed
                                                       )->get_nearest_neighbor( ).

        " Create a new Query with context added from the GUID.
        lv_prompt = lv_prompt && ' Context: ' && get_wsg_chunk_data( iv_wsg_chunk_id = ls_nearest_document-datapoint_id iv_gcs_bucket = iv_gcs_bucket ).

        " Create a client for Generative AI Model
        DATA(lo_model) = NEW /goog/cl_generative_model( iv_model_key = zif_hazmat_constants=>c_model_key_gemini_flash ).

        " Use the new prompt query to respond to the updated prompt with context.
        DATA(lv_response) = lo_model->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                           iv_harm_block_threshold = 'BLOCK_NONE'
                                   )->generate_content( iv_prompt_text = lv_prompt
                                   )->get_text( ).

        rv_response = lv_response.

      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        rv_response = lo_cx_sdk->get_text( ) .
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
