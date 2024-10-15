#!/bin/bash
# ./setup-data-files.sh your-project-id https://github.com/your-username/your-repo.git unique-id region

# Check if project ID, repo URL, unique ID, and region are provided
if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ] || [ -z "$4" ]; then
    echo "Error: Please provide GCP project ID, GitHub repository URL, a unique ID, and region as arguments."
    echo "Usage: ./upload_to_gcs.sh <project-id> <github-repo-url> <unique-id> <region>"
    exit 1
fi

PROJECT_ID="$1"
REPO_URL="$2"
REPO_NAME=$(basename "$REPO_URL" .git) # Extract repo name from URL
UNIQUE_ID="$3"
REGION="$4" 
BASE_BUCKET="hazmat-data-files-${UNIQUE_ID}"
DATA_FILES_DIR="data-files"

# 1. Clone the GitHub Repository
git clone "$REPO_URL"

# 2. Navigate to the Repository
cd "$REPO_NAME"

# 3. Install the Google Cloud SDK (if not already installed)
if ! command -v gcloud &> /dev/null
then
    echo "Google Cloud SDK not found. Please install it first."
    exit 1
fi

# 4. Authenticate with Google Cloud
# gcloud auth login

# 5. Set Your Project
gcloud config set project "$PROJECT_ID"

# 6. Create the Base Bucket in the specified region
if gsutil ls -b gs://${BASE_BUCKET} &> /dev/null; then # Check if bucket exists
    echo "Error: Bucket gs://${BASE_BUCKET} already exists. Please choose a different unique ID or delete the existing bucket."
    exit 1
else
    gsutil mb -l $REGION gs://${BASE_BUCKET} 
fi

# 7. Upload Files to Folders within the Base Bucket
for folder in hazmat-pictogram hazmat-pictogram-embeddings hazmat-pictogram-descriptions hazmat-prod hazmat-prod-embeddings hazmat-sds hazmat-sds-chunks hazmat-sds-embeddings hazmat-wsg hazmat-wsg-chunks hazmat-wsg-embeddings hazmat-prompts; do
    echo "Uploading $folder..."
    gsutil -m cp -r ${DATA_FILES_DIR}/$folder gs://${BASE_BUCKET}/${folder} 
done

echo "Upload complete!"