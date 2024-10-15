import streamlit as st
import json
import requests
import helper
import pandas as pd
import base64
import toml

# --- Configuration ---
st.set_page_config(
    page_title="H-Pro", page_icon="🚀", layout="wide"
)
st.title("HAZMAT Pro (Hpro)")

# --- Load Configuration ---
config = toml.load("config/config.toml")  # Load from config.toml
api_config = config["api"]

# --- API Endpoints ---
GET_PROMPT_API = api_config["get_prompt_api"]
GET_PRODUCT_API = api_config["get_product_api"]
POST_PROMPT_API = api_config["post_prompt_api"]

# --- Fetch Prompts ---
#@st.cache_data  # Cache the API call to improve performance
def fetch_prompts():
    return helper.call_sap_rest_api(GET_PROMPT_API)

all_prompts = fetch_prompts()

# --- Category Selection ---
categories = list({category["category"] for category in all_prompts})
user_scenario = st.selectbox(
    "Select Business Scenario:", categories, index=None, placeholder="Select a use case"
)

if user_scenario:
    st.write("You selected:", user_scenario)

    # --- Filter Prompts ---
    filtered_prompts = helper.find_json_objects(all_prompts, user_scenario)
    prompts_df = pd.DataFrame(
        filtered_prompts, 
        columns=["prompt", "promptID", "workflowID", "inputParameters", "promptEnabled"] 
    )

    # --- Prompt Selection ---
    selected_row = st.radio(
        "Select a prompt:",
        options=prompts_df.index,
        format_func=lambda x: prompts_df.loc[x, "prompt"],
    )

    selected_prompt = prompts_df.loc[selected_row, "prompt"]
    selected_prompt_id = prompts_df.loc[selected_row, "promptID"]
    selected_workflow_id = prompts_df.loc[selected_row, "workflowID"]
    selected_input_params = prompts_df.loc[selected_row, "inputParameters"]

    if selected_prompt:
        # ---  Display Prompt Status ---
        prompt_enabled = (
            True
            if prompts_df.loc[selected_row, "promptEnabled"] == "Yes"
            else False
        )
        if prompt_enabled:
            st.write(f"You selected: {selected_prompt}")
        else:
            st.write(f"You selected: {selected_prompt} (Disabled)")

        # --- Input Parameters ---
        enable_product_list = helper.input_param_exist(
            selected_input_params, "Product"
        )
        enable_image_upload = helper.input_param_exist(
            selected_input_params, "Photo"
        )

        selected_product = None
        uploaded_file = None
        b64_encoded = None

        if enable_product_list:
            # --- Product Selection ---
            @st.cache_data  # Cache API call
            def fetch_products():
                return helper.call_sap_rest_api(GET_PRODUCT_API)

            product_list = fetch_products()
            selected_product = st.selectbox(
                "Select a Product:",
                [p["productDescription"] for p in product_list],
                index=None,
                placeholder="Select a product",
            )
            if selected_product:
                selected_prompt = selected_prompt.replace("<Product>", selected_product)
                st.text_area("Your query", value=selected_prompt, key=selected_product)

        if enable_image_upload:
            # --- Image Upload ---
            uploaded_file = st.file_uploader("Upload Image", type=["jpg", "png", "gif"])
            if uploaded_file:
                bytes_data = uploaded_file.getvalue()
                b64_encoded = base64.b64encode(bytes_data).decode()
                st.image(bytes_data, caption="Uploaded Image", width=300)
                st.text_area("Your Query", value=selected_prompt.replace(" <Photo>", ""))

        # --- Call Gemini ---
        if st.button("Get Answer from Gemini"):
            if enable_image_upload and not uploaded_file:
                st.error("Upload a file")
            else:
                payload = {
                    "prompt": selected_prompt,
                    "promptId": selected_prompt_id,
                    "workflowId": selected_workflow_id,
                    "product": selected_product,
                    "imageBlob": b64_encoded,
                }
                print(payload)

                json_payload = json.dumps(payload)
                gemini_response = helper.call_sap_rest_api(
                    POST_PROMPT_API, json_payload, "POST"
                )

                if gemini_response:
                    container = st.container(border=True)
                    container.markdown(
                        "***Gemini Response:*** " + "\n" + "\n" + gemini_response.text
                    )
                else:
                    st.error("Error getting response from Gemini.")
else:
    st.markdown("Please select a scenario")