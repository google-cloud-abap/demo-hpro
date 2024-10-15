#!/bin/bash
# ./clean-up.sh your-project-id repo-name unique-id

# Check if project ID, unique ID, and repo name are provided
if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
    echo "Error: Please provide GCP project ID, unique ID, and repository name as arguments."
    echo "Usage: ./cleanup.sh <project-id> <repo-name> <unique-id>"
    exit 1
fi

PROJECT_ID="$1"
REPO_NAME="$2"
UNIQUE_ID="$3"
BASE_BUCKET="hazmat-data-files-${UNIQUE_ID}"

# 1. Install the Google Cloud SDK (if not already installed)
if ! command -v gcloud &> /dev/null
then
    echo "Google Cloud SDK not found. Please install it first."
    exit 1
fi

# 2. Authenticate with Google Cloud
#gcloud auth login

# 3. Set Your Project
gcloud config set project "$PROJECT_ID"

# 4. Delete the Base Bucket
echo "Deleting bucket gs://${BASE_BUCKET}..."
gsutil rm -r gs://${BASE_BUCKET}

# 5. Remove the Cloned Repository
echo "Removing cloned repository ${REPO_NAME}..."
rm -rf ${REPO_NAME}

echo "Cleanup complete!"