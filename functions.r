library(stringr)

# Function to recursively resolve imports
resolve_imports <- function(file_path, base_dir = NULL, processed_files = character()) {
  if (is.null(base_dir)) {
    base_dir <- dirname(file_path)
  }
  
  # Check for circular imports
  if (file_path %in% processed_files) {
    warning("Circular import detected for file: ", file_path)
    return(character())
  }
  
  processed_files <- c(processed_files, file_path)
  
  # Read the file content
  content <- readLines(file_path)
  
  # Process each line for imports
  resolved_content <- character()
  
  for (line in content) {
    # Check for import statements
    if (str_detect(line, '^import\\s+"([^"]+)"')) {
      import_file <- str_match(line, '^import\\s+"([^"]+)"')[1,2]
      import_path <- file.path(base_dir, import_file)
      
      if (file.exists(import_path)) {
        cat("Importing:", import_path, "\n")
        # Recursively resolve imports in the imported file
        imported_content <- resolve_imports(import_path, dirname(import_path), processed_files)
        resolved_content <- c(resolved_content, imported_content)
      } else {
        warning("Import file not found: ", import_path)
      }
    } else {
      # Keep the original line
      resolved_content <- c(resolved_content, line)
    }
  }
  
  return(resolved_content)
}

# Main reading function
read_ins <- function(file_path) {
  cat("Processing file:", file_path, "\n")
  
  # Step 1: Resolve all imports recursively
  full_content <- resolve_imports(file_path)
  
  # Step 2: Remove comments and clean lines
  content_clean <- full_content %>% 
    str_replace("!.*", "") %>%  # Remove comments
    str_trim() %>%             # Remove leading/trailing whitespace
    .[. != ""]                 # Remove empty lines
  
  # Step 3: Parse the cleaned content
  result <- list(
    model = list(),
    st = list(),
    group = list(),
    pft = list()
  )
  
  current_type <- NA
  current_name <- NA
  current_params <- list()
  bracket_count <- 0
  in_section <- FALSE
  
  # Process each line
  for (i in seq_along(content_clean)) {
    line <- content_clean[i]
    
    # Check for st (stand type) declaration
    if (str_detect(line, "^st\\s+\"([^\"]+)\"\\s*\\(")) {
      # Save previous section if exists
      if (in_section && !is.na(current_name)) {
        result[[current_type]][[current_name]] <- current_params
      }
      
      # Start new st
      current_name <- str_match(line, "^st\\s+\"([^\"]+)\"")[1,2]
      current_type <- "st"
      current_params <- list(imports = character())
      bracket_count <- 1
      in_section <- TRUE
      
    } else if (str_detect(line, "^group\\s+\"([^\"]+)\"\\s*\\(")) {
      # Save previous section if exists
      if (in_section && !is.na(current_name)) {
        result[[current_type]][[current_name]] <- current_params
      }
      
      # Start new group
      current_name <- str_match(line, "^group\\s+\"([^\"]+)\"")[1,2]
      current_type <- "group"
      current_params <- list(imports = character())
      bracket_count <- 1
      in_section <- TRUE
      
    } else if (str_detect(line, "^pft\\s+\"([^\"]+)\"\\s*\\(")) {
      # Save previous section if exists
      if (in_section && !is.na(current_name)) {
        result[[current_type]][[current_name]] <- current_params
      }
      
      # Start new pft
      current_name <- str_match(line, "^pft\\s+\"([^\"]+)\"")[1,2]
      current_type <- "pft"
      current_params <- list(imports = character())
      bracket_count <- 1
      in_section <- TRUE
      
    } else if (!in_section) {
      # Only parse model parameters if we're NOT in a section
      # Handle param statements (special case)
      if (str_detect(line, '^param\\s+')) {
        # Store the entire line after "param" as the value with key "param"
        param_value <- str_trim(str_sub(line, 6))  # Remove "param" prefix
        if (is.null(result$model$param)) {
          result$model$param <- character()
        }
        result$model$param <- c(result$model$param, param_value)
      } else if (str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_]*\\s+")) {
        # Regular model parameter (key-value pair) - but NOT if it's a section start
        tokens <- str_split(line, "\\s+", n = 2)[[1]]
        if (length(tokens) >= 2) {
          param_name <- tokens[1]
          param_value <- str_trim(tokens[2])
          
          # Skip if this looks like a section start that wasn't caught
          if (param_name %in% c("st", "group", "pft")) {
            next
          }
          
          # Remove quotes if present
          param_value <- str_remove_all(param_value, "^\"|\"$")
          
          # Convert to numeric if possible
          param_value_numeric <- suppressWarnings(as.numeric(param_value))
          if (!is.na(param_value_numeric)) {
            param_value <- param_value_numeric
          }
          
          result$model[[param_name]] <- param_value
        }
      } else if (str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_]*\\s*\".*\"$")) {
        # Model parameter with quoted string value
        tokens <- str_split(line, "\\s+", n = 2)[[1]]
        if (length(tokens) >= 2) {
          param_name <- tokens[1]
          param_value <- str_trim(tokens[2])
          
          # Skip if this looks like a section start
          if (param_name %in% c("st", "group", "pft")) {
            next
          }
          
          param_value <- str_remove_all(param_value, "^\"|\"$")
          result$model[[param_name]] <- param_value
        }
      }
    } else if (in_section) {
      # Handle brackets
      if (str_detect(line, "\\(")) {
        bracket_count <- bracket_count + str_count(line, "\\(")
      }
      if (str_detect(line, "\\)")) {
        bracket_count <- bracket_count - str_count(line, "\\)")
      }
      
      # Check if section ended
      if (bracket_count == 0) {
        result[[current_type]][[current_name]] <- current_params
        in_section <- FALSE
        current_name <- NA
        current_type <- NA
        next
      }
      
      # Parse content inside section
      tokens <- str_split(line, "\\s+")[[1]]
      tokens <- tokens[tokens != ""]
      
      if (length(tokens) == 0) next
      
      # Check for standalone names (inheritance for groups and pfts)
      if ((current_type == "group" || current_type == "pft" || current_type == "st") &&
          length(tokens) == 1 && 
          !str_detect(tokens[1], "^-?[0-9]")) {
        # This is a group/stand name for inheritance
        current_params$imports <- c(current_params$imports, tokens[1])
      } else if (length(tokens) >= 2) {
        # Regular parameter
        param_name <- tokens[1]
        param_values <- tokens[-1]
        
        # Remove quotes from individual values if they are strings
        param_values <- sapply(param_values, function(x) {
          if (str_detect(x, '^".*"$')) {
            str_remove_all(x, '^"|"$')
          } else {
            x
          }
        })
        
        # Convert to numeric if possible
        param_values_numeric <- suppressWarnings(as.numeric(param_values))
        if (!any(is.na(param_values_numeric))) {
          param_values <- param_values_numeric
        }
        
        # Store single values as scalars, multiple values as vectors
        if (length(param_values) == 1) {
          current_params[[param_name]] <- param_values[[1]]
        } else {
          current_params[[param_name]] <- param_values
        }
      }
    }
  }
  
  # Save the last section if exists
  if (in_section && !is.na(current_name)) {
    result[[current_type]][[current_name]] <- current_params
  }
  
  return(result)
}

# Main writing function
write_ins <- function(params, output_file) {
  lines <- character()
  
  # Helper function to format parameter values
  format_value <- function(value) {
    if (is.numeric(value)) {
      return(as.character(value))
    } else if (is.character(value)) {
      return(paste0('"', value, '"'))
    } else if (length(value) > 1) {
      # Vector of values - write space-separated on one line
      return(paste(sapply(value, function(x) {
        if (is.character(x)) paste0('"', x, '"') else as.character(x)
      }), collapse = " "))
    } else {
      return(as.character(value))
    }
  }
  
  # Write model parameters
  if (length(params$model) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! MODEL PARAMETERS")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    # Write param statements first (each on separate lines)
    if (!is.null(params$model$param)) {
      for (param_line in params$model$param) {
        lines <- c(lines, paste("param", param_line))
      }
      lines <- c(lines, "")
    }
    
    # Write other model parameters
    for (param_name in names(params$model)) {
      if (param_name != "param") {
        value <- params$model[[param_name]]
        lines <- c(lines, paste(param_name, format_value(value)))
      }
    }
    lines <- c(lines, "")
  }
  
  # Write groups
  if (length(params$group) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! PARAMETER GROUPS")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    for (group_name in names(params$group)) {
      lines <- c(lines, paste0('group "', group_name, '" ('))
      lines <- c(lines, "")
      
      group_data <- params$group[[group_name]]
      
      # Write imports first
      if (!is.null(group_data$imports)) {
        for (import_name in group_data$imports) {
          lines <- c(lines, paste("  ", import_name))
        }
        lines <- c(lines, "")
      }
      
      # Write other parameters - each on single line even if multi-value
      other_params <- group_data[!names(group_data) %in% "imports"]
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        #lines <- c(lines, paste("  ", param_name, format_value(value)))
        lines <- c(lines, paste("  ",param_name,str_flatten(format_value(value),collapse = " ")))
      }
      
      lines <- c(lines, ")")
      lines <- c(lines, "")
    }
  }
  
  # Write stand types (st)
  if (length(params$st) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! STAND TYPES")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    for (st_name in names(params$st)) {
      lines <- c(lines, paste0('st "', st_name, '" ('))
      lines <- c(lines, "")
      
      st_data <- params$st[[st_name]]
      
      # Write imports first
      if (!is.null(st_data$imports)) {
        for (import_name in st_data$imports) {
          lines <- c(lines, paste("  ", import_name))
        }
        lines <- c(lines, "")
      }
      
      # Write other parameters - each on single line even if multi-value
      other_params <- st_data[!names(st_data) %in% "imports"]
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        lines <- c(lines, paste("  ", param_name, format_value(value)))
      }
      
      lines <- c(lines, ")")
      lines <- c(lines, "")
    }
  }
  
  # Write PFTs
  if (length(params$pft) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! PLANT FUNCTIONAL TYPES")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    for (pft_name in names(params$pft)) {
      lines <- c(lines, paste0('pft "', pft_name, '" ('))
      lines <- c(lines, "")
      
      pft_data <- params$pft[[pft_name]]
      
      # Write imports first
      if (!is.null(pft_data$imports)) {
        for (import_name in pft_data$imports) {
          lines <- c(lines, paste("  ", import_name))
        }
        lines <- c(lines, "")
      }
      
      # Write other parameters (include should come early)
      other_params <- pft_data[!names(pft_data) %in% "imports"]
      
      # Write include parameter first if it exists
      if (!is.null(other_params$include)) {
        lines <- c(lines, paste("  include", format_value(other_params$include)))
        other_params$include <- NULL
      }
      
      # Write remaining parameters - each on single line even if multi-value
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        #lines <- c(lines, paste("  ", param_name, format_value(value)))
        lines <- c(lines, paste("  ",param_name,str_flatten(format_value(value),collapse = " ")))
      }
      
      lines <- c(lines, ")")
      lines <- c(lines, "")
    }
  }
  
  # Write to file
  writeLines(lines, output_file)
  cat("INS file written to:", output_file, "\n")
}