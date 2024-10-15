import requests
import streamlit as st
import toml

# --- Load Configuration ---
config = toml.load("config/config.toml")  # Load from config.toml
api_config = config["api"]

# --- API Configuration ---
BASE_URL = api_config["base_url"]
PORT = api_config["port"]
USERNAME = api_config["username"]
PASSWORD = api_config["password"]

def call_sap_rest_api(api_path, payload=None, method="GET"):
    """Calls an SAP REST API with basic authentication."""

    url = f"{BASE_URL}:{PORT}{api_path}"
    headers = {"Content-Type": "application/json"}

    print(f"Calling the API at: {url}")
    try:
        if method == "GET":
            response = requests.get(
                url,
                auth=(USERNAME, PASSWORD),
                headers=headers,
                json=payload,
                verify=False,
            )
            response.raise_for_status()
            return response.json()
        if method == "POST":
            response = requests.post(
                url,
                auth=(USERNAME, PASSWORD),
                data=payload,
                headers=headers,
                verify=False,
            )
            response.raise_for_status()
            return response
    except requests.exceptions.RequestException as e:
        print(f"Error calling SAP REST API: {e}")
        st.error(f"API Error: {e}")  # Display error in Streamlit
        return None


def find_json_objects(json_data, search_string):
    """Finds JSON objects containing a specific string."""
    results = []
    if isinstance(json_data, list):
        for item in json_data:
            if isinstance(item, dict):
                results.extend(find_json_objects(item, search_string))
    elif isinstance(json_data, dict):
        for key, value in json_data.items():
            if isinstance(value, (list, dict)):
                results.extend(find_json_objects(value, search_string))
            elif isinstance(value, str) and search_string in value:
                results.append(json_data)
    return results


def input_param_exist(data, param_name):
    """Checks if a specific input parameter exists."""
    return any(item["name"] == param_name for item in data)