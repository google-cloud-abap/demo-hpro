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
class ZCL_HAZMAT_GCS_DATA_INGESTER definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_section_data,
        section_id      TYPE string,  "Section ID
        section_header  TYPE string,  "Section Header
        section_content TYPE string,  "Section Content
      END OF ty_section_data .
  types:
    tt_section_data TYPE STANDARD TABLE OF ty_section_data WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ty_document_data,
        chemical_name   TYPE string,          "Hazardous Material Name
        section_details TYPE tt_section_data, "Section Details
      END OF ty_document_data .
  types:
    BEGIN OF ty_embedding_data,
        guid            TYPE guid_16,     "Unique Guid
        matnr           TYPE matnr,       "Material
        maktx           TYPE maktx,       "Material Description
        werks           TYPE werks_d,     "Plant
        lgort           TYPE lgort_d,     "Storage location
        lgobe           TYPE srmgsstorpl, "Storage location description
        section_id      TYPE i,           "Section ID
        section_header  TYPE string,      "Section Header
        section_content TYPE string,      "Section Content
        revised_content TYPE string,      "Revised Content: To be used by embeddings
      END OF ty_embedding_data .
  types:
    BEGIN OF ty_embedding_data_product,
        matnr           TYPE matnr,       "Material
        maktx           TYPE maktx,       "Material Description
        werks           TYPE werks_d,     "Plant
        lgort           TYPE lgort_d,     "Storage location
        lgobe           TYPE srmgsstorpl, "Storage location description
        revised_content TYPE string,      "Revised Content: To be used by embeddings
      END OF ty_embedding_data_product .
  types:
    BEGIN OF ty_embedding_data_wsg,
        guid            TYPE guid_16,     "Unique Guid
        section_id      TYPE string,      "Section ID
        section_header  TYPE string,      "Section Header
        section_content TYPE string,      "Section Content
        revised_content TYPE string,      "Revised Content: To be used by embeddings
      END OF ty_embedding_data_wsg .
  types:
    BEGIN OF ty_embedding_data_pic,
        id              TYPE string,      "Pictogram ID
        image_content   TYPE string,      "Image Content
        revised_content TYPE string,      "Revised Content: To be used by embeddings
      END OF ty_embedding_data_pic .

  constants:
    BEGIN OF c_data_type,
        sds_documents TYPE c VALUE 'S',
        wsg_documents TYPE c VALUE 'W',
        pic_images    TYPE c VALUE 'I',
        product_data  TYPE c VALUE 'P',
      END OF c_data_type .
  constants:
    BEGIN OF c_source_gcs_folder,
        sds       TYPE string VALUE 'hazmat-sds' ##NO_TEXT,
        prod      TYPE string VALUE 'hazmat-prod' ##NO_TEXT,
        pictogram TYPE string VALUE 'hazmat-pictogram' ##NO_TEXT,
        pict_desc TYPE string VALUE 'hazmat-pictogram-descriptions' ##NO_TEXT,
        wsg       TYPE string VALUE 'hazmat-wsg',
        prompts   TYPE string VALUE 'hazmat-prompts',
      END OF c_source_gcs_folder .
  constants:
    BEGIN OF c_target_gcs_folder,
        pictogram  TYPE string VALUE 'hazmat-pictogram-embeddings' ##NO_TEXT,
        sds        TYPE string VALUE 'hazmat-sds-embeddings' ##NO_TEXT,
        product    TYPE string VALUE 'hazmat-prod-embeddings' ##NO_TEXT,
        wsg        TYPE string VALUE 'hazmat-wsg-embeddings' ##NO_TEXT,
        wsg_chunks TYPE string VALUE 'hazmat-wsg-chunks' ##NO_TEXT,
        sds_chunks TYPE string VALUE 'hazmat-sds-chunks' ##NO_TEXT,
      END OF c_target_gcs_folder .

  methods CONSTRUCTOR
    importing
      !IV_KEY_NAME type /GOOG/KEYNAME default 'DEMO_AIPLATFORM'
      !IV_MODEL_KEY_GEMINI type /GOOG/MODEL_KEY
      !IV_MODEL_KEY_EMBEDDINGS type /GOOG/MODEL_KEY
      !IV_SRC_BUCKET type STRING
      !IV_TGT_BUCKET type STRING
    raising
      /GOOG/CX_SDK .
  methods EXECUTE
    importing
      !IV_DATA_TYPE type C default C_DATA_TYPE-SDS_DOCUMENTS
    exporting
      !EV_ERR_TEXT type STRING .
  PROTECTED SECTION.
private section.

  data:
    mt_documents              TYPE STANDARD TABLE OF ty_document_data .
  data:
    mt_embedding_data         TYPE STANDARD TABLE OF ty_embedding_data .
  data:
    mt_embedding_data_wsg     TYPE STANDARD TABLE OF ty_embedding_data_wsg .
  data:
    mt_embedding_data_pic     TYPE STANDARD TABLE OF ty_embedding_data_pic .
  data:
    mt_embedding_data_product TYPE STANDARD TABLE OF ty_embedding_data_product .
  data MS_BUCKET_DATA type /GOOG/CL_STORAGE_V1=>TY_016 .
  data MV_GCS_FOLDER_PREFIX type STRING .
  data MV_DATA_TYPE type C .
  data MO_GCS_CLIENT type ref to /GOOG/CL_STORAGE_V1 .
  data MO_MODEL type ref to /GOOG/CL_GENERATIVE_MODEL .
  data MV_SRC_BUCKET_NAME type STRING .
  data MV_TGT_BUCKET_NAME type STRING .
  data MV_FIRST_CALL type ABAP_BOOLEAN .
  data MV_KEY_NAME type /GOOG/KEYNAME .
  data MV_MODEL_KEY_GEMINI type /GOOG/MODEL_KEY .
  data MV_MODEL_KEY_EMBEDDINGS type /GOOG/MODEL_KEY .

  methods INGEST_SDS_DATA
    exporting
      !EV_ERR_TEXT type STRING .
  methods INGEST_WSG_DATA
    exporting
      !EV_ERR_TEXT type STRING .
  methods INGEST_PRODUCT_DATA
    exporting
      !EV_ERR_TEXT type STRING .
  methods INGEST_PICT_DATA
    exporting
      !EV_ERR_TEXT type STRING .
  methods SPLIT_WSG_IN_CHUNKS
    importing
      !IV_FILE_GCS_URI type STRING
    exporting
      !EV_ERR_TEXT type STRING .
  methods SPLIT_SDS_IN_CHUNKS
    importing
      !IV_FILE_GCS_URI type STRING
    exporting
      !EV_ERR_TEXT type STRING .
  methods GET_DOCUMENTS_FROM_GCS
    exporting
      !EV_ERR_TEXT type STRING .
  methods CREATE_SDS_EMBED_SEND_TO_GCS
    exporting
      !EV_ERR_TEXT type STRING .
  methods CREATE_PROD_EMBED_SEND_TO_GCS
    exporting
      !EV_ERR_TEXT type STRING .
  methods CREATE_WSG_EMBED_SEND_TO_GCS
    exporting
      !EV_ERR_TEXT type STRING .
  methods CREATE_PICT_EMBED_SEND_TO_GCS
    exporting
      !EV_ERR_TEXT type STRING .
  methods GET_MASTER_DATA_FOR_WSG .
  methods GET_MASTER_DATA_FOR_SDS .
  methods GET_MASTER_DATA_PRODUCT .
  methods SAVE_WSG_CHUNKS_TO_GCS .
  methods SAVE_SDS_CHUNKS_TO_GCS .
ENDCLASS.



CLASS ZCL_HAZMAT_GCS_DATA_INGESTER IMPLEMENTATION.


  METHOD constructor.

**  Create API Client using the client key
    mo_gcs_client = NEW /goog/cl_storage_v1(       iv_key_name   = iv_key_name ).
    mo_model      = NEW /goog/cl_generative_model( iv_model_key  = iv_model_key_gemini ).

    mv_key_name         = iv_key_name.
    mv_model_key_gemini = iv_model_key_gemini.
    mv_model_key_embeddings = iv_model_key_embeddings.
    mv_src_bucket_name  = iv_src_bucket.
    mv_tgt_bucket_name  = iv_tgt_bucket.

  ENDMETHOD.


  METHOD create_prod_embed_send_to_gcs.


    DATA ls_embedding_template TYPE /goog/cl_embeddings_model=>ty_embeddings_template.
    DATA ls_addln_params TYPE /goog/cl_embeddings_model=>ty_addln_params.

    TRY.

        " Create a client for Text-embeddings
        DATA(lo_client) = NEW /goog/cl_embeddings_model( iv_model_key = zif_hazmat_constants=>c_model_key_text_embed ).
        LOOP AT mt_embedding_data_product ASSIGNING FIELD-SYMBOL(<ls_emdedding>).
          " Additional optional parameters
          ls_addln_params-task_type = /goog/cl_embeddings_model=>c_retrieval_document.
          ls_addln_params-title = |{ <ls_emdedding>-matnr }|.
          ls_addln_params-output_dimensionality = zif_hazmat_constants=>c_text_dimension.


          CLEAR: ls_embedding_template.
          ls_embedding_template-id = <ls_emdedding>-matnr.
          ls_embedding_template-content = <ls_emdedding>-revised_content.
          ls_embedding_template-source =  'SAP-ZCL_HAZMAT_GCS_DATA_INGESTER'.

          GET TIME STAMP FIELD ls_embedding_template-feature_timestamp.

          " Create embedding with template record
          lo_client->gen_text_embeddings_by_struct( is_input        = ls_embedding_template
                                                    is_addln_params = ls_addln_params
                                                                      )->collect( ).

        ENDLOOP.

        " Send the cumulated embedding data to GCS
        DATA(lv_filename) = |{ c_target_gcs_folder-product }/embeddings-sap-products.json|.
        lo_client->send_struct_to_gcs( iv_bucket_name = mv_tgt_bucket_name iv_file_name = lv_filename ).


      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        ev_err_text = lo_exception->get_text( ).
    ENDTRY.


    IF ev_err_text IS INITIAL.
      DATA(lv_msg) = |Embeddings successfully stored in GCS bucket: { mv_tgt_bucket_name }.|.
      MESSAGE lv_msg TYPE 'I'.
    ENDIF.

  ENDMETHOD.


  METHOD create_sds_embed_send_to_gcs.

    DATA ls_embedding_template TYPE /goog/cl_embeddings_model=>ty_embeddings_template.
    DATA ls_addln_params TYPE /goog/cl_embeddings_model=>ty_addln_params.

    LOOP AT mt_embedding_data ASSIGNING FIELD-SYMBOL(<ls_emdedding>).
      "Additional optional parameters
      ls_addln_params-task_type = /goog/cl_embeddings_model=>c_retrieval_document.
      ls_addln_params-title     = |{ <ls_emdedding>-matnr }:{ <ls_emdedding>-section_id }:{ <ls_emdedding>-section_header }|.
      ls_addln_params-output_dimensionality = zif_hazmat_constants=>c_text_dimension.

      TRY.
          DATA(lo_client) = NEW /goog/cl_embeddings_model( iv_model_key = mv_model_key_embeddings ).

          CLEAR: ls_embedding_template.

          ls_embedding_template-id      = <ls_emdedding>-guid.
          ls_embedding_template-content = <ls_emdedding>-revised_content.
          ls_embedding_template-source  = 'SAP-ZCL_HAZMAT_GCS_DATA_INGESTER'.

          GET TIME STAMP FIELD ls_embedding_template-feature_timestamp.

          "Create embedding with template record
          lo_client->gen_text_embeddings_by_struct( is_input        = ls_embedding_template
                                                    is_addln_params = ls_addln_params ).

          DATA(lv_filename) = |{ c_target_gcs_folder-sds }/{ <ls_emdedding>-guid }.json|.
          lo_client->send_struct_to_gcs(  iv_bucket_name = mv_tgt_bucket_name
                                          iv_file_name = lv_filename ).

          DATA(lv_msg) = |Embedding data ingestion for Product: { <ls_emdedding>-matnr }, LGORT: { <ls_emdedding>-lgort }, Section: { <ls_emdedding>-section_id } successful.|.
          MESSAGE lv_msg TYPE 'S'.

        CATCH /goog/cx_sdk INTO DATA(lo_exception).
          ev_err_text = lo_exception->get_text( ).
          lv_msg = |Product: { <ls_emdedding>-matnr }, LGORT: { <ls_emdedding>-lgort }, Section: { <ls_emdedding>-section_id } failed:{ ev_err_text }|.
          MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDLOOP.

    IF ev_err_text IS INITIAL.
      lv_msg = |Embeddings successfully stored in GCS bucket: { mv_tgt_bucket_name }.|.
      MESSAGE lv_msg TYPE 'I'.
    ENDIF.
  ENDMETHOD.


  METHOD create_wsg_embed_send_to_gcs.

    DATA ls_embedding_template TYPE /goog/cl_embeddings_model=>ty_embeddings_template.
    DATA ls_addln_params TYPE /goog/cl_embeddings_model=>ty_addln_params.

    TRY.
        " Create a client for creation of text embeddings
        DATA(lo_client) = NEW /goog/cl_embeddings_model( iv_model_key = zif_hazmat_constants=>c_model_key_text_embed ).

        " Create embeddings for each knowledge chunk
        LOOP AT mt_embedding_data_wsg ASSIGNING FIELD-SYMBOL(<ls_emdedding>).
          " Addtional optional parameters
          ls_addln_params-task_type = /goog/cl_embeddings_model=>c_retrieval_document.
          ls_addln_params-title = |{ <ls_emdedding>-guid }:{ <ls_emdedding>-section_id }|.
          ls_addln_params-output_dimensionality = zif_hazmat_constants=>c_text_dimension.

          CLEAR: ls_embedding_template.
          ls_embedding_template-id = <ls_emdedding>-guid.
          ls_embedding_template-content = <ls_emdedding>-revised_content.
          ls_embedding_template-source =  'SAP-ZCL_HAZMAT_GCS_DATA_INGESTER'.

          GET TIME STAMP FIELD ls_embedding_template-feature_timestamp.

          " Create embedding with template record
          lo_client->gen_text_embeddings_by_struct( is_input        = ls_embedding_template
                                                    is_addln_params = ls_addln_params ).

          " Send each WSG Chunk as a separate file in GCS. The WSG chunk contains embedding for the knowledge chunk and additional context.
          DATA(lv_filename) = |{ c_target_gcs_folder-wsg }/{ <ls_emdedding>-guid }.json|.
          lo_client->send_struct_to_gcs( iv_bucket_name = mv_tgt_bucket_name iv_file_name = lv_filename ).

        ENDLOOP.


      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        ev_err_text = lo_exception->get_text( ).
    ENDTRY.

    IF ev_err_text IS INITIAL.
      DATA(lv_msg) = |Embeddings successfully stored in GCS bucket: { mv_tgt_bucket_name }.|.
      MESSAGE lv_msg TYPE 'I'.
    ENDIF.
  ENDMETHOD.


  METHOD get_documents_from_gcs.

    TRY.
        " Call method storage.objects.list to get all objects stored within the source bucket.
        mo_gcs_client->list_objects(
          EXPORTING
            iv_p_bucket = mv_src_bucket_name
            iv_q_prefix = |{ mv_gcs_folder_prefix }/|
          IMPORTING
            es_output   = ms_bucket_data
            ev_err_text = ev_err_text ).

        IF ev_err_text IS INITIAL.
          DATA(lv_msg) = |{ lines( ms_bucket_data-items ) } files successfully read from GCS bucket: { mv_src_bucket_name }.|.
          MESSAGE lv_msg TYPE 'I'.
        ELSE.
          MESSAGE ev_err_text TYPE 'E'.
        ENDIF.
      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        ev_err_text = lo_exception->get_text( ).
        MESSAGE ev_err_text TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD get_master_data_for_sds.
    DATA: ls_data  TYPE ty_embedding_data,
          lr_maktx TYPE RANGE OF maktg.

    CLEAR: mt_embedding_data.
    " Create range table for Material Descriptions
    lr_maktx = VALUE #( FOR ls_doc IN mt_documents ( sign = 'I' option = 'EQ' low = to_upper( ls_doc-chemical_name ) ) ).
    SORT lr_maktx.
    DELETE ADJACENT DUPLICATES FROM lr_maktx.

    " Search data corresponding to Hazardous materials using material description
    IF lr_maktx IS NOT INITIAL.
      SELECT *
        FROM zhazmat_products
       WHERE maktg IN @lr_maktx
        INTO TABLE @DATA(lt_material_data).

      IF sy-subrc = 0.
        DATA(lv_msg) = |Master Data found for { sy-dbcnt } products.|.
        MESSAGE lv_msg TYPE 'I'.
      ENDIF.
    ENDIF.

    " Translate Material Names to upper case for easy search
    LOOP AT mt_documents ASSIGNING FIELD-SYMBOL(<ls_doc>).
      <ls_doc>-chemical_name = to_upper( <ls_doc>-chemical_name ).
      CONDENSE <ls_doc>-chemical_name.
    ENDLOOP.

    SORT mt_documents BY chemical_name.
    " Generate data structure for embeddings creation
    LOOP AT lt_material_data ASSIGNING FIELD-SYMBOL(<ls_material_data>).
      TRY.

          <ls_doc> = mt_documents[ chemical_name =  <ls_material_data>-maktg ].
          " Add Product data for reference against each SDS Document
          ls_data = CORRESPONDING #( <ls_material_data> ).

          " Populate extracted knowledge chunk data for each Product
          LOOP AT <ls_doc>-section_details ASSIGNING FIELD-SYMBOL(<ls_section>).

            ls_data-section_id      = <ls_section>-section_id.
            ls_data-section_header  = <ls_section>-section_header.
            ls_data-section_content = <ls_section>-section_content.

            ls_data-revised_content = |Product ID:{ ls_data-matnr },| &&
                                      |Product Description:{ ls_data-maktx },| &&
                                      |Storage Location:{ ls_data-lgort },| &&
                                      |Storage Location Description:{ ls_data-lgobe },| &&
                                      |Plant:{ ls_data-werks },| &&
                                      |Section Id:{ ls_data-section_id },| &&
                                      |Section Header:{ ls_data-section_header },| &&
                                      |Section Content:{ ls_data-section_content }|.

            " Add a guid to the data. This will be used later for retrieval
            CALL FUNCTION 'GUID_CREATE'
              IMPORTING
                ev_guid_16 = ls_data-guid.

            APPEND ls_data TO mt_embedding_data.

          ENDLOOP.
        CATCH cx_sy_itab_line_not_found.
          lv_msg = |Product: { <ls_material_data>-maktg } not found in SAP material master |.
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.
    ENDLOOP.

    IF mt_embedding_data IS INITIAL.
      MESSAGE 'No embedding data was created' TYPE 'E' DISPLAY LIKE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD get_master_data_product.

    DATA: ls_data  TYPE ty_embedding_data_product,
          lr_maktx TYPE RANGE OF maktx.

    " Get product master data
    SELECT *
      FROM zhazmat_products
     WHERE maktg IN @lr_maktx
      INTO TABLE @DATA(lt_material_data).

    " Create the corresponding data structure for embeddings creation.
    LOOP AT lt_material_data ASSIGNING FIELD-SYMBOL(<ls_material_data>).

      ls_data-matnr = <ls_material_data>-matnr.
      ls_data-maktx = <ls_material_data>-maktx.
      ls_data-lgort = <ls_material_data>-lgort.
      ls_data-werks = <ls_material_data>-werks.
      ls_data-lgobe = <ls_material_data>-lgobe.

      ls_data-revised_content = |Product ID:{ ls_data-matnr },| &&
                                |Product Description:{ ls_data-maktx },| &&
                                |Storage Location:{ ls_data-lgort },| &&
                                |Storage Location Description:{ ls_data-lgobe },| &&
                                |Plant:{ ls_data-werks }| .

      APPEND ls_data TO mt_embedding_data_product.
    ENDLOOP.

  ENDMETHOD.


  METHOD split_wsg_in_chunks.

    DATA:
      ls_document    TYPE ty_document_data,
      lv_instruction TYPE string,
      lv_prompt      TYPE string.


    IF mv_first_call = abap_false.
      mv_first_call = abap_true.

      lv_instruction =  'You will receive a document containing Warehouse Safety Guidelines' &&
                        'Your task is to:' &&
                        'Identify all section headings within the document.' &&
                        'Extract the content within each section.' &&
                        'Output a structured representation of the extracted information.'.


      mo_model = mo_model->set_system_instructions( iv_text = lv_instruction
                        )->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                iv_harm_block_threshold = 'BLOCK_NONE'
                                      ).

    ENDIF.

    lv_prompt = 'Analyze the following document and extract section-specific content:' &&
                'Output the results in JSON format with no additional text or formatting with the following structure:' &&
                'JSON' &&
                '{' &&
                '"SECTION_DETAILS": [' &&
                '{' &&
                '"SECTION_ID": "[ID of the section from the predefined list]",' &&
                '"SECTION_HEADER": "[Matched section header from the predefined list]",' &&
                '"SECTION_CONTENT": "[Extracted content from the section, If the document has more than 8 pages, limit this section length to maximum of 75 tokens]"' &&
                '},' &&
                '// ... (Repeat for each extracted section)' &&
                ']' &&
                '}' &&
                'Do no format the data. There should be no additional text or formatting.'.


    TRY.

        DATA(lv_response) = mo_model->clear_file_data(
                                      )->set_file_data( iv_mime_type          = 'application/pdf'
                                                        iv_file_uri           = iv_file_gcs_uri
                                      )->generate_content( lv_prompt
                                      )->get_text( ).

        IF lv_response IS NOT INITIAL.

          IF lv_response(8) = '```json#' .
            lv_response = lv_response+8.
            DATA(lv_new_length) = strlen( lv_response ) - strlen( '#```' ).
            lv_response = lv_response(lv_new_length).
          ENDIF.

          /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_response
                                                          iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                                IMPORTING es_data        = ls_document ).
          IF ls_document IS NOT INITIAL.
            APPEND ls_document TO mt_documents.
            DATA(lv_msg) = |{ iv_file_gcs_uri } was successfully chunked!|.
            MESSAGE lv_msg TYPE 'I'.
          ELSE.
            ev_err_text = 'Unable to deserialize the response!'.
          ENDIF.

        ENDIF.


      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        ev_err_text = lo_exception->get_text( ).

    ENDTRY.


  ENDMETHOD.


  METHOD create_pict_embed_send_to_gcs.

    DATA ls_image TYPE /goog/cl_embeddings_model=>ty_image.
    DATA ls_embedding_template TYPE /goog/cl_embeddings_model=>ty_embeddings_template.
    DATA ls_addln_params TYPE /goog/cl_embeddings_model=>ty_addln_params.

    TRY.
        " Create a client for multimodal embeddings
        DATA(lo_multi_embed) = NEW /goog/cl_embeddings_model( iv_model_key = zif_hazmat_constants=>c_model_key_mm_embed ).

        " Convert each pictogram into embedding data
        LOOP AT ms_bucket_data-items ASSIGNING FIELD-SYMBOL(<ls_data>).
          DATA(lv_file_uri) = |gs://{ <ls_data>-bucket }/{ <ls_data>-name }|.
          SPLIT <ls_data>-name AT '.' INTO DATA(lv_entity_id) DATA(lv_extension).

          "Additional optional parameters
          ls_addln_params-task_type    = /goog/cl_embeddings_model=>c_retrieval_document.
          ls_addln_params-title        = lv_entity_id.

          CLEAR: ls_embedding_template.
          ls_embedding_template-id     = lv_entity_id.
          ls_embedding_template-source = 'SAP-ZCL_HAZMAT_GCS_DATA_INGESTER'.

          GET TIME STAMP FIELD ls_embedding_template-feature_timestamp.

          " Generate multimodal embeddings(Default dimension 1408) for an image stored in GCS
          " The embeddings are also collected in an internal table using the COLLECT() method.
          ls_image-gcs_uri = lv_file_uri.
          lo_multi_embed->gen_image_embeddings_by_struct( iv_image = ls_image
                                                          is_input = ls_embedding_template
                                                          is_addln_params = ls_addln_params
                       )->collect( ).

        ENDLOOP.

        " Send the collected embedding data to GCS
        DATA(lv_filename) = |{ c_target_gcs_folder-pictogram }/embeddings-pictograms.json|.
        lo_multi_embed->send_struct_to_gcs( iv_bucket_name = mv_tgt_bucket_name iv_file_name = lv_filename ).

      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        ev_err_text = lo_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD execute.
    mv_data_type = iv_data_type.

    "Based on the type of data to be ingested, updated the GCS folder and then run data ingestion.
    CASE mv_data_type.
      WHEN c_data_type-sds_documents.
        mv_gcs_folder_prefix = c_source_gcs_folder-sds.
        ingest_sds_data( IMPORTING ev_err_text = ev_err_text ).

      WHEN c_data_type-wsg_documents.
        mv_gcs_folder_prefix = c_source_gcs_folder-wsg.
        ingest_wsg_data( IMPORTING ev_err_text = ev_err_text ).

      WHEN c_data_type-pic_images.
        mv_gcs_folder_prefix = c_source_gcs_folder-pictogram.
        ingest_pict_data( IMPORTING ev_err_text = ev_err_text ).

      WHEN c_data_type-product_data.
        ingest_product_data( IMPORTING ev_err_text = ev_err_text ).
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD get_master_data_for_wsg.
    DATA: ls_data  TYPE ty_embedding_data_wsg.

    " Prepare data for embeddings creation
    LOOP AT mt_documents ASSIGNING FIELD-SYMBOL(<ls_data>).

      LOOP AT <ls_data>-section_details ASSIGNING FIELD-SYMBOL(<ls_section>).

        ls_data-section_id      = <ls_section>-section_id.
        ls_data-section_header  = <ls_section>-section_header.
        ls_data-section_content = <ls_section>-section_content.

        ls_data-revised_content = |Section Id:{ ls_data-section_id },| &&
                                  |Section Header:{ ls_data-section_header },| &&
                                  |Section Content:{ ls_data-section_content }|.

        " Add a guid to the data. This will be used later for retrieval
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = ls_data-guid.

        APPEND ls_data TO mt_embedding_data_wsg.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD ingest_pict_data.
    " Get the pictograms from source GCS bucket
    get_documents_from_gcs( ).

    " Create multimodal embeddings for the pictograms and send to GCS.
    create_pict_embed_send_to_gcs(
      IMPORTING
        ev_err_text = ev_err_text
    ).

  ENDMETHOD.


  METHOD ingest_product_data.
    " Get product master data from SAP
    get_master_data_product( ).
    " Create embeddings for the structured product data and send to GCS
    create_prod_embed_send_to_gcs(
      IMPORTING
        ev_err_text = ev_err_text
    ).

  ENDMETHOD.


  METHOD ingest_sds_data.
    DATA: lv_err_text TYPE string.
    " Get documents stored in GCS Source bucket
    get_documents_from_gcs( ).

    " For each SDS Document, create knowledge chunks
    LOOP AT ms_bucket_data-items ASSIGNING FIELD-SYMBOL(<ls_data>).
      CLEAR: lv_err_text.
      DATA(lv_file_uri) = |gs://{ <ls_data>-bucket }/{ <ls_data>-name }|.

      split_sds_in_chunks(
        EXPORTING
          iv_file_gcs_uri = lv_file_uri
        IMPORTING
          ev_err_text     = lv_err_text ).

      IF lv_err_text IS NOT INITIAL.
        " Continue generation of other chunks even in case of failure.
        DATA(lv_msg) = |Chunking Failed { lv_file_uri }: { lv_err_text }|.
        MESSAGE lv_err_text TYPE 'I'.
      ENDIF.
    ENDLOOP.

    IF mt_documents IS NOT INITIAL.
      " Enrich the knowledge chunks with corresponding product data (unstructured data)
      get_master_data_for_sds( ).
      save_sds_chunks_to_gcs( ).
      " Create Embeddings and store in GCS Target Bucket
      create_sds_embed_send_to_gcs( ).
    ENDIF.

  ENDMETHOD.


  METHOD ingest_wsg_data.
    " Get source documents from GCS Bucket
    get_documents_from_gcs( ).

    " For each of the documents stored in corresponding Bucket, split the document into data chunks
    LOOP AT ms_bucket_data-items ASSIGNING FIELD-SYMBOL(<ls_data>).
      DATA(lv_file_uri) = |gs://{ <ls_data>-bucket }/{ <ls_data>-name }|.
      split_wsg_in_chunks(
        EXPORTING
          iv_file_gcs_uri = lv_file_uri
        IMPORTING
          ev_err_text     = DATA(lv_err_text) ).
      IF lv_err_text IS NOT INITIAL.
        DATA(lv_msg) = |Chunking Failed { lv_file_uri }: { lv_err_text }|.
        MESSAGE lv_err_text TYPE 'I'.
      ENDIF.
    ENDLOOP.

    " MT_DOCUMENTS gets populated from the SPLIT_WSG_IN_CHUNKS method.
    IF mt_documents IS NOT INITIAL.
      " Enrich the chunked data with additional context
      get_master_data_for_wsg( ).
      save_wsg_chunks_to_gcs( ).
      " Create embeddings for the WSG knowledge chunks with additional context (unstructured data) and send to GCS.
      create_wsg_embed_send_to_gcs( ).
    ENDIF.
  ENDMETHOD.


  METHOD split_sds_in_chunks.

    DATA: ls_document    TYPE ty_document_data,
          lv_instruction TYPE string,
          lv_prompt      TYPE string.

    TRY.

        CLEAR: ev_err_text.

        IF mv_first_call = abap_false.
          mv_first_call = abap_true.

          lv_instruction =  'You will receive a document containing sections with headings. These sections correspond to a' &&
                            ' predefined list of possible section headings (Section ID: Section Header):' &&
                            '\n"1": "Identification"' &&
                            '\n"2": "Hazard identification"' &&
                            '\n"3": "Composition / information on ingredients"' &&
                            '\n"4": "First-aid measures"' &&
                            '\n"5": "Fire-fighting measures"' &&
                            '\n"6": "Accidental release measures"' &&
                            '\n"7": "Handling and storage"' &&
                            '\n"8": "Exposure controls / personal protection"' &&
                            '\n"9": "Physical and chemical properties"' &&
                            '\n"10": "Stability and reactivity"' &&
                            '\n"11": "Toxicological information"' &&
                            '\n"12": "Ecological information"' &&
                            '\n"13": "Disposal considerations"' &&
                            '\n"14": "Transport information"' &&
                            '\n"15": "Regulatory information"' &&
                            '\n"16": "Other information"' &&

                            '\nYour task is to:' &&
                            '\nIdentify all section headings within the document.' &&
                            '\nMatch each identified heading to the predefined list.' &&
                            '\nExtract the content within each matched section.' &&
                            '\nExclude any sections not present in the predefined list.' &&
                            '\nOutput a structured representation of the extracted information.' &&
                            '\nReturn the result sorted by section id.'.


          mo_model = mo_model->set_system_instructions( iv_text = lv_instruction
                            )->add_safety_settings( iv_harm_category        = 'HARM_CATEGORY_DANGEROUS_CONTENT'
                                                    iv_harm_block_threshold = 'BLOCK_NONE'
                                          ).

        ENDIF.

        lv_prompt = 'Analyze the following document and extract section-specific content:' &&
                    'Output the results in JSON format with the following structure:' &&
                    'JSON' &&
                    '{' &&
                    '"CHEMICAL_NAME": "[Name of the chemical extracted from the document]",' &&
                    '"SECTION_DETAILS": [' &&
                    '{' &&
                    '"SECTION_ID": "[ID of the section from the predefined list]",' &&
                    '"SECTION_HEADER": "[Matched section header from the predefined list]",' &&
                    '"SECTION_CONTENT": "[Extracted content from the section, If the document has more than 8 pages, limit this section length to maximum of 75 tokens]"' &&
                    '},' &&
                    '// ... (Repeat for each extracted section)' &&
                    ']' &&
                    '}' &&
                    '\nDo no format or pretty print the json data'.

        DATA(lv_response) = CONV string( mo_model->clear_file_data(
                                      )->set_file_data( iv_mime_type          = 'application/pdf'
                                                        iv_file_uri           = iv_file_gcs_uri
                                      )->generate_content( lv_prompt
                                      )->get_text( ) ).

        IF lv_response IS NOT INITIAL .

          /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_response
                                                          iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                                IMPORTING es_data        = ls_document ).
          IF ls_document IS NOT INITIAL.
            REPLACE ALL OCCURRENCES OF ',' IN ls_document-chemical_name WITH ''.
            CONDENSE: ls_document-chemical_name.
            APPEND ls_document TO mt_documents.
            DATA(lv_msg) = |{ iv_file_gcs_uri } was successfully chunked into { lines( ls_document-section_details ) } sections!|.
            MESSAGE lv_msg TYPE 'S'.
          ELSE.
            ev_err_text = iv_file_gcs_uri && ': Unable to deserialize the response!' .
          ENDIF.

        ELSE.
          ev_err_text = iv_file_gcs_uri && ': No response received! Please check finish reason for :'.
        ENDIF.

      CATCH /goog/cx_sdk INTO DATA(lo_exception).
        ev_err_text = lo_exception->get_text( ).
    ENDTRY.


  ENDMETHOD.


  METHOD save_sds_chunks_to_gcs.
    DATA: lv_data	TYPE xstring,
          lv_json TYPE string.
    TRY.

        DATA(lo_gcs_client) = NEW /goog/cl_storage_v1( iv_key_name = 'DEMO_AIPLATFORM' ).

        LOOP AT mt_embedding_data ASSIGNING FIELD-SYMBOL(<ls_data>).
          /goog/cl_json=>serialize(
            EXPORTING
              data             = <ls_data>
            RECEIVING
              r_json           = lv_json
          ).

          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              text     = lv_json
              encoding = '4110'
            IMPORTING
              buffer   = lv_data
            EXCEPTIONS
              failed   = 1
              OTHERS   = 2.
          IF sy-subrc <> 0.
            DATA(lv_msg) = 'Error converting to XSTRING'.
            MESSAGE lv_msg TYPE 'E'.
          ENDIF.

          DATA(lv_filename) = |{ c_target_gcs_folder-sds_chunks }/{ <ls_data>-guid }.json|.

          lo_gcs_client->insert_objects(
            EXPORTING
              iv_q_name       = lv_filename
              iv_p_bucket     = mv_tgt_bucket_name
              is_data         = lv_data
              iv_content_type = 'application/json'
            IMPORTING
              es_output       = DATA(ls_output)                " Object
              ev_ret_code     = DATA(lv_ret_code)                " Return Code
              ev_err_text     = DATA(lv_err_text)                " Error Text
          ).

          IF lo_gcs_client->is_success( lv_ret_code ).
*            MESSAGE 'Data saved' TYPE 'S'.
          ENDIF.

        ENDLOOP.
      CATCH /goog/cx_sdk.
    ENDTRY.
  ENDMETHOD.


  METHOD save_wsg_chunks_to_gcs.
    DATA: lv_data	TYPE xstring,
          lv_json TYPE string.
    TRY.

        DATA(lo_gcs_client) = NEW /goog/cl_storage_v1( iv_key_name = 'DEMO_AIPLATFORM' ).

        LOOP AT mt_embedding_data_wsg ASSIGNING FIELD-SYMBOL(<ls_data>).
          /goog/cl_json=>serialize(
            EXPORTING
              data             = <ls_data>
            RECEIVING
              r_json           = lv_json
          ).

          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              text     = lv_json
              encoding = '4110'
            IMPORTING
              buffer   = lv_data
            EXCEPTIONS
              failed   = 1
              OTHERS   = 2.
          IF sy-subrc <> 0.
            DATA(lv_msg) = 'Error converting to XSTRING'.
            MESSAGE lv_msg TYPE 'E'.
          ENDIF.

          DATA(lv_filename) = |{ c_target_gcs_folder-wsg_chunks }/{ <ls_data>-guid }.json|.

          lo_gcs_client->insert_objects(
            EXPORTING
              iv_q_name       = lv_filename
              iv_p_bucket     = mv_tgt_bucket_name
              is_data         = lv_data
              iv_content_type = 'application/json'
            IMPORTING
              es_output       = DATA(ls_output)                " Object
              ev_ret_code     = DATA(lv_ret_code)                " Return Code
              ev_err_text     = DATA(lv_err_text)                " Error Text
          ).

          IF lo_gcs_client->is_success( lv_ret_code ).
            MESSAGE 'Data saved' TYPE 'S'.
          ENDIF.

        ENDLOOP.
      CATCH /goog/cx_sdk.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
