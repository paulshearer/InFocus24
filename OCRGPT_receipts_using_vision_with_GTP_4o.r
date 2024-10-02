library(httr)
library(jsonlite)
library(keyring)
library(fs)
library(jsonvalidate)

# Create keys if needed
# key_set("OpenAI_API_Key", keyring = mykeyring)  # This will prompt for the key value input

# Get secrets
api_key <- key_get("OpenAI_API_Key", keyring = mykeyring)

# Function to pretty-print JSON
pretty_JSON <- function(response, encoding = "UTF-8") {
  json_string <- rawToChar(response$content)
  start_pos <- regexpr("\\{", json_string)
  end_pos <- regexpr("\\}", rev(strsplit(json_string, "")[[1]]))
  end_pos <- nchar(json_string) - end_pos + 1
  json_string <- substr(json_string, start_pos, end_pos)
  json_object <- fromJSON(json_string)
  message_content <- json_object$choices$message$content
  cleaned_json_string <- gsub("```json\\n|\\n```", "", message_content)
  json_object_cleaned <- fromJSON(cleaned_json_string)
  pretty_json <- prettify(cleaned_json_string)
  return(pretty_json)
}

# Function to validate JSON against the schema
validate_json <- function(json_string, schema) {
  validator <- json_validator(schema, engine = "ajv")
  result <- validator(json_string, verbose = TRUE)
  if (isTRUE(result)) {
    return(list(valid = TRUE, errors = NULL))
  } else {
    errors <- attr(result, "errors")
    return(list(valid = FALSE, errors = errors))
  }
}

# Function to check and apportion tax to line items
check_and_apportion_tax <- function(json_object) {
  total_tax <- as.numeric(json_object$total_tax)
  items <- json_object$items
  
  # Check if total tax is present but line item taxes are missing or zero
  line_item_tax_present <- sapply(items, function(item) {
    extended_tax <- as.numeric(item$Item_Extended_Tax)
    if (is.na(extended_tax)) {
      extended_tax <- 0
    }
    extended_tax > 0
  })
  missing_tax_items <- sum(!line_item_tax_present) > 0
  
  if (total_tax > 0 && missing_tax_items) {
    # Calculate the total extended price for all items
    total_extended_price <- sum(sapply(items, function(item) as.numeric(item$Item_Extended_Price)))
    
    # Apportion the total tax to each item based on their share of the total extended price
    items <- lapply(items, function(item) {
      item_extended_price <- as.numeric(item$Item_Extended_Price)
      apportioned_tax <- round((item_extended_price / total_extended_price) * total_tax, 2)
      item$Item_Extended_Tax <- apportioned_tax
      # Calculate Item_Unit_Tax
      item_qty <- as.numeric(item$Item_QTY)
      if (item_qty > 0) {
        item$Item_Unit_Tax <- round(apportioned_tax / item_qty, 2)
      } else {
        item$Item_Unit_Tax <- 0
      }
      return(item)
    })
    
    # Ensure all line item taxes add up to the total tax by adjusting the last item
    sum_item_taxes <- sum(sapply(items, function(item) as.numeric(item$Item_Extended_Tax)))
    tax_difference <- round(total_tax - sum_item_taxes, 2)
    
    if (tax_difference != 0) {
      last_item <- items[[length(items)]]
      last_item$Item_Extended_Tax <- last_item$Item_Extended_Tax + tax_difference
      # Recalculate Item_Unit_Tax for the last item
      item_qty <- as.numeric(last_item$Item_QTY)
      if (item_qty > 0) {
        last_item$Item_Unit_Tax <- round(last_item$Item_Extended_Tax / item_qty, 2)
      } else {
        last_item$Item_Unit_Tax <- 0
      }
      items[[length(items)]] <- last_item
    }
    
    # Update the json_object with the apportioned taxes
    json_object$items <- items
  }
  
  return(json_object)
}

# Corrected QA function
qa_json <- function(json_object) {
  subtotal_amount <- round(as.numeric(json_object$subtotal_amount), 2)
  total_tax <- round(as.numeric(json_object$total_tax), 2)
  total_with_tax <- round(as.numeric(json_object$total_with_tax), 2)
  items <- json_object$items
  
  sum_extended_price <- round(sum(sapply(items, function(item) {
    extended_price <- as.numeric(item$Item_Extended_Price)
    if (is.na(extended_price)) {
      extended_price <- 0
    }
    return(extended_price)
  })), 2)
  
  # Handle NA values when summing Item_Extended_Tax
  sum_item_taxes <- round(sum(sapply(items, function(item) {
    extended_tax <- as.numeric(item$Item_Extended_Tax)
    if (is.na(extended_tax)) {
      extended_tax <- 0
    }
    return(extended_tax)
  })), 2)
  
  total_calculated_subtotal <- sum_extended_price
  total_calculated_tax <- sum_item_taxes
  total_calculated_total <- round(total_calculated_subtotal + total_calculated_tax, 2)
  
  valid_subtotal <- subtotal_amount == total_calculated_subtotal
  valid_total_tax <- total_tax == total_calculated_tax
  valid_total <- total_with_tax == total_calculated_total
  
  print(paste("    Sum of Item_Extended_Price:", sum_extended_price)) # Debug statement
  print(paste("    Subtotal Amount:", subtotal_amount)) # Debug statement
  print(paste("    Sum of Item_Extended_Tax:", sum_item_taxes)) # Debug statement
  print(paste("    Total Tax:", total_tax)) # Debug statement
  print(paste("    Calculated Total with Tax:", total_calculated_total)) # Debug statement
  print(paste("    Total with Tax:", total_with_tax)) # Debug statement
  
  return(list(
    valid = valid_subtotal && valid_total_tax && valid_total,
    sum_extended_price = sum_extended_price,
    subtotal_amount = subtotal_amount,
    sum_item_taxes = sum_item_taxes,
    total_tax = total_tax,
    total_calculated_total = total_calculated_total,
    total_with_tax = total_with_tax
  ))
}

# Define the updated JSON schema
schema <- '{
  "type": "object",
  "properties": {
    "company": { "type": "string" },
    "company_address": { "type": "string" },
    "distance": { "type": "number" },
    "receipt_number": { "type": "string" },
    "transaction_date": { "type": "string", "format": "date" },
    "subtotal_amount": { "type": "number" },
    "total_tax": { "type": "number" },
    "total_with_tax": { "type": "number" },
    "items": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "Item_Part_Number": { "type": "string" },
          "Item_QTY": { "type": "integer" },
          "Item_Description": { "type": "string" },
          "Item_Unit_Price": { "type": "number" },
          "Item_Extended_Price": { "type": "number" },
          "Item_Unit_Tax": { "type": "number" },
          "Item_Extended_Tax": { "type": "number" }
        },
        "required": ["Item_Part_Number", "Item_QTY", "Item_Description", "Item_Unit_Price", "Item_Extended_Price"]
      }
    }
  },
  "required": ["company", "company_address", "distance", "receipt_number", "transaction_date", "subtotal_amount", "total_tax", "total_with_tax", "items"]
}'

# Define the API endpoint
url <- "https://api.openai.com/v1/chat/completions"

# Path to the folder containing JSON files
folder_path <- "c://JSON//"

# Path to the bad JSON folder
bad_json_folder <- file.path(folder_path, "bad_JSON")
dir_create(bad_json_folder)

# Loop through the receipt URLs
for (i in 1:nrow(receipt_urls)) {
  json_file_path <- paste0(folder_path, receipt_urls$FileName[i], ".json")
  
  # Check if the JSON file already exists
  if (file.exists(json_file_path)) {
    print(paste0("File already exists: ", json_file_path, " - Skipping"))
    next
  }
  
  print(paste0("Processing Receipts #", i, " of #", nrow(receipt_urls), "- ", receipt_urls$FileName[i]))
  
  body <- list(
    model = "gpt-4o",
    messages = list(
      list(
        role = "user",
        content = list(
          list(
            type = "text",
            text = paste0("Extract and classify the following receipt details."
                          ,"Sometimes the Item Part Number will be right before the description."
                          ,"sometimes a single line item will be split across multiples rows where extended_cost may be\n"
                          ,"on one row and the quantity and unit price is on another row."
                          , "Format the JSON and the JSON keys as per the schema:\n"
                          , schema
                          , "\nIgnore any data in the receipt that is not listed in the JSON fields above."
                          ,"\nReturn nothing but the JSON. Make sure to only return the keys listed above in the JSON example."
                          ,"\nDo not respond with any other text like 'Here is the extracted and classified receipt information formatted in JSON'."
                          ,"\nIf it is a Return Receipt return put RETURN RECEIPT for all character fields and 0 for all numeric values."
                          ,"\nSometimes the Item_QTY will spill onto the next line of a receipt."
                          ,"Only return the JSON.")
          ),
          list(
            type = "image_url",
            image_url = list(
              url = receipt_urls$URL[i]
            )
          )
        )
      )
    ),
    max_tokens = 3000,
    temperature = 0
  )
  
  # Function to perform the POST request with retries
  perform_post_request <- function(body, retries = 3) {
    for (attempt in 1:retries) {
      tryCatch({
        response <- POST(
          url,
          add_headers(
            `Content-Type` = "application/json",
            Authorization = paste("Bearer", api_key)
          ),
          body = toJSON(body, auto_unbox = TRUE),
          encode = "json",
          httr::timeout(480)
        )
        
        if (status_code(response) >= 200 && status_code(response) < 300) {
          print("Request was successful")
          return(response)
        } else {
          print(paste("Request failed with status:", status_code(response)))
          print(content(response, as = "text"))
        }
      }, error = function(e) {
        print(paste("An error occurred:", e$message))
        print(paste0("error at - ", Sys.time()))
      }, warning = function(w) {
        print(paste("A warning occurred:", w$message))
        print(paste0("warning at - ", Sys.time()))
      })
      
      # Wait before retrying
      Sys.sleep(5)
    }
    
    return(NULL)
  }
  
  # Function to process the receipt and validate JSON
  process_receipt <- function(body, json_file_path, retries = 3) {
    for (attempt in 1:retries) {
      response <- perform_post_request(body)
      
      if (is.null(response)) {
        next
      }
      
      json_string <- pretty_JSON(response)
      validation_result <- validate_json(json_string, schema)
      
      if (validation_result$valid) {
        json_object <- fromJSON(json_string, simplifyDataFrame = FALSE) # Ensure items are parsed as lists
        
        # Check if items exist and contain required fields
        if (length(json_object$items) == 0 || !all(sapply(json_object$items, function(item) {
          all(c("Item_Extended_Price", "Item_Part_Number", "Item_QTY") %in% names(item))
        }))) {
          print("Items array is empty or missing required fields. Retrying...")
          next
        }
        
        # Check and apportion tax if necessary
        json_object <- check_and_apportion_tax(json_object)
        
        # QA the JSON after tax check and apportionment
        qa_result <- qa_json(json_object)
        
        if (qa_result$valid) {
          # Convert the updated json_object back to a JSON string
          json_string <- toJSON(json_object, pretty = TRUE, auto_unbox = TRUE)
          
          write(json_string, json_file_path)
          return(TRUE)
        } else {
          log_message <- paste("   QA failed for", json_file_path, ":\n",
                               "Subtotal amount (from JSON):", qa_result$subtotal_amount,
                               "Sum of item extended prices:", qa_result$sum_extended_price,
                               "Total tax (from JSON):", qa_result$total_tax,
                               "Sum of item taxes:", qa_result$sum_item_taxes,
                               "Total with tax (from JSON):", qa_result$total_with_tax,
                               "Calculated total with tax:", qa_result$total_calculated_total)
          print(log_message)
          log_file_path <- file.path(bad_json_folder, paste0(path_file(json_file_path), ".log"))
          write(log_message, log_file_path)
        }
      } else {
        log_message <- paste("    Validation failed for", json_file_path, ":\n", paste(validation_result$errors, collapse = "\n"))
        print(log_message)
        log_file_path <- file.path(bad_json_folder, paste0(path_file(json_file_path), ".log"))
        write(log_message, log_file_path)
      }
    }
    
    # If it fails after all retries, move the JSON file to bad_JSON folder
    write(json_string, json_file_path)
    file_move(json_file_path, file.path(bad_json_folder, path_file(json_file_path)))
    return(FALSE)
  }
  
  # Process the receipt and validate JSON
  process_receipt(body, json_file_path)
}

cat("Processing and validation complete. Invalid JSON files have been moved to:", bad_json_folder, "\n")

