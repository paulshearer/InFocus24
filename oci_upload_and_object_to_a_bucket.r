# Load necessary libraries
library(httr)
library(jsonlite)
library(openssl)
library(digest)
library(pdftools)
library(png)
library(tools)
library(fs)
library(keyring)


# Define OCI configuration
mykeyring ="OCI_InformedCIO2_Creds"
region <- "us-ashburn-1"
namespace <- "idgtdok6ra14" # Replace with your actual namespace from the URL
bucket_name <- "your_bucket_name" # Replace with your actual bucket name
directory_path <- "c://zz//" # Specify the location of the directory
org_pdfs_path <- "c://zz//ORG_PDFs//" # Directory to move original PDFs

#get secrets
tenancy_ocid <- key_get("OCI_Tenancy", keyring = mykeyring)
user_ocid <- key_get("OCI_User", keyring = mykeyring)
fingerprint  <- key_get("OCI_Fingerprint", keyring = mykeyring)
private_key_path <- key_get("OCI_Key_Path", keyring = mykeyring)


# Create the directory if it doesn't exist
dir_create(org_pdfs_path)

# Read the private key
tryCatch({
  private_key <- read_key(private_key_path)
  cat("Private key successfully read.\n")
}, error = function(e) {
  stop("Error reading private key: ", e$message)
})

# Helper function to create a request signature
create_signature <- function(verb, host, endpoint, date, private_key, tenancy_ocid, user_ocid, fingerprint, body = NULL) {
  headers <- c(
    paste("date:", date),
    paste("host:", host),
    paste("(request-target):", tolower(verb), endpoint)
  )
  
  if (!is.null(body)) {
    content_sha256 <- digest::digest(body, algo = "sha256", serialize = FALSE)
    headers <- c(headers, paste("content-length:", length(body)))
    headers <- c(headers, paste("x-content-sha256:", base64_encode(charToRaw(content_sha256))))
  }
  
  signing_string <- paste(headers, collapse = "\n")
  cat("Signing string:\n", signing_string, "\n")
  
  # Create the signature
  signature <- openssl::signature_create(charToRaw(signing_string), private_key, hash = sha256)
  signature_base64 <- base64_encode(signature)
  cat("Signature:\n", signature_base64, "\n")
  
  authorization_header <- paste(
    "Signature version=\"1\",",
    "headers=\"date host (request-target)",
    if (!is.null(body)) " content-length x-content-sha256" else "",
    "\",",
    "keyId=\"", tenancy_ocid, "/", user_ocid, "/", fingerprint, "\",",
    "algorithm=\"rsa-sha256\",",
    "signature=\"", signature_base64, "\"",
    sep = ""
  )
  
  return(authorization_header)
}

# Function to check if an object exists in the bucket and retrieve its URL
object_exists <- function(private_key, tenancy_ocid, user_ocid, fingerprint, region, namespace, bucket_name, object_name) {
  host <- paste0("idgtdok6ra14.objectstorage.", region, ".oci.customer-oci.com")
  endpoint <- paste0("/n/", namespace, "/b/", bucket_name, "/o/", URLencode(object_name, reserved = TRUE))
  url <- paste0("https://", host, endpoint)
  
  date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
  
  authorization_header <- create_signature(
    "HEAD",
    host,
    endpoint,
    date,
    private_key,
    tenancy_ocid,
    user_ocid,
    fingerprint
  )
  
  response <- httr::HEAD(
    url,
    add_headers(
      Authorization = authorization_header,
      Date = date,
      Host = host
    )
  )
  
  if (response$status_code == 200) {
    return(url)
  } else {
    return(NULL)
  }
}

# Function to upload an object to a bucket with a progress bar
upload_object <- function(private_key, tenancy_ocid, user_ocid, fingerprint, region, namespace, bucket_name, file_path, object_name) {
  host <- paste0("idgtdok6ra14.objectstorage.", region, ".oci.customer-oci.com")
  endpoint <- paste0("/n/", namespace, "/b/", bucket_name, "/o/", URLencode(object_name, reserved = TRUE))
  url <- paste0("https://", host, endpoint)
  
  date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
  
  file_content <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  content_length <- length(file_content)
  
  authorization_header <- create_signature(
    "PUT",
    host,
    endpoint,
    date,
    private_key,
    tenancy_ocid,
    user_ocid,
    fingerprint,
    file_content
  )
  
  response <- httr::PUT(
    url,
    add_headers(
      Authorization = authorization_header,
      Date = date,
      Host = host,
      `Content-Length` = content_length,
      `x-content-sha256` = base64_encode(charToRaw(digest::digest(file_content, algo = "sha256", serialize = FALSE)))
    ),
    body = upload_file(file_path),
    progress()
  )
  
  if (response$status_code == 200) {
    cat("Successfully uploaded object: ", object_name, "\n")
    return(url)
  } else {
    cat("Failed to upload object. Status code:", response$status_code, "\n")
    cat("Response:", content(response, "text"), "\n")
    return(NULL)
  }
}

# Initialize DataFrame to store file names and URLs
receipt_urls <- data.frame(FileName = character(), URL = character(), stringsAsFactors = FALSE)

# Loop through all files in the directory, skipping subfolders
file_list <- list.files(directory_path, full.names = TRUE, recursive = FALSE)
for (file_path in file_list) {
  if (file.info(file_path)$isdir) {
    next
  }
  
  file_name <- basename(file_path)
  object_name <- file_name
  original_file_path <- file_path
  
  # Convert PDFs to PNGs
  if (tolower(file_ext(file_path)) == "pdf") {
    pdf_images <- pdf_render_page(file_path, page = 1)
    png_path <- sub("\\.pdf$", ".png", file_path)
    writePNG(pdf_images, png_path)
    file_path <- png_path
    object_name <- basename(png_path)
  }
  
  # Check if the object already exists
  existing_url <- object_exists(private_key, tenancy_ocid, user_ocid, fingerprint, region, namespace, bucket_name, object_name)
  if (!is.null(existing_url)) {
    cat("Object already exists: ", object_name, "\n")
    receipt_urls <- rbind(receipt_urls, data.frame(FileName = file_name, URL = existing_url, stringsAsFactors = FALSE))
    if (tolower(file_ext(original_file_path)) == "pdf") {
      # Move the original PDF file to the ORG_PDFs directory
      file_move(original_file_path, file.path(org_pdfs_path, file_name))
    }
    next
  }
  
  # Upload the object and store the URL in the DataFrame
  url <- upload_object(private_key, tenancy_ocid, user_ocid, fingerprint, region, namespace, bucket_name, file_path, object_name)
  if (!is.null(url)) {
    receipt_urls <- rbind(receipt_urls, data.frame(FileName = file_name, URL = url, stringsAsFactors = FALSE))
  }
  
  if (tolower(file_ext(original_file_path)) == "pdf") {
    # Move the original PDF file to the ORG_PDFs directory
    file_move(original_file_path, file.path(org_pdfs_path, file_name))
  }
}

# Display the DataFrame
print(receipt_urls)
