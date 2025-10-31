library(stringr)

#-------------------------------------------------------------------------------
#
# Reader functions
#
#-------------------------------------------------------------------------------

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
        #cat("Importing:", import_path, "\n")
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

# Main function that now includes import resolution
read_ins <- function(file_path) {
  #cat("Processing file:", file_path, "\n")
  
  # Step 1: Resolve all imports recursively
  full_content <- resolve_imports(file_path)
  
  # Step 2: Remove comments and clean lines
  content_clean <- full_content %>% 
    str_replace("!.*", "") %>%  # Remove comments
    str_trim() %>%             # Remove leading/trailing whitespace
    .[. != ""]                 # Remove empty lines
  
  # Step 3: Parse the cleaned content (same as before)
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
    
    # Check for model parameters (outside st/groups/pfts)
    if (!in_section) {
      if (str_detect(line, "^st\\s+\"([^\"]+)\"\\s*\\(") || 
          str_detect(line, "^group\\s+\"([^\"]+)\"\\s*\\(") || 
          str_detect(line, "^pft\\s+\"([^\"]+)\"\\s*\\(")) {
        # We're entering a section, so process any pending model parameters
      } else if (str_detect(line, "^param\\s+\"([^\"]+)\"\\s*\\(([^)]+)\\)")) {
        # Model parameter with param keyword
        matches <- str_match(line, "^param\\s+\"([^\"]+)\"\\s*\\(([^)]+)\\)")
        param_name <- matches[1,2]
        param_value <- str_trim(matches[1,3])
        result$model[[param_name]] <- param_value
      } else if (str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_]*\\s+")) {
        # Regular model parameter (key-value pair)
        tokens <- str_split(line, "\\s+", n = 2)[[1]]
        if (length(tokens) >= 2) {
          param_name <- tokens[1]
          param_value <- str_trim(tokens[2])
          
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
          param_value <- str_remove_all(param_value, "^\"|\"$")
          result$model[[param_name]] <- param_value
        }
      }
    }
    
    # Check for st (stand type) declaration
    if (str_detect(line, "^st\\s+\"([^\"]+)\"\\s*\\(")) {
      # Save previous section if exists
      if (in_section && !is.na(current_name)) {
        result[[current_type]][[current_name]] <- current_params
      }
      
      # Start new st
      current_name <- str_match(line, "^st\\s+\"([^\"]+)\"")[1,2]
      current_type <- "st"
      current_params <- list()
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
      current_params <- list()
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
      current_params <- list()
      bracket_count <- 1
      in_section <- TRUE
      
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
          !str_detect(tokens[1], "^-?[0-9]") &&
          !tokens[1] %in% c("common")) {
        # This is a group/stand name for inheritance
        if (is.null(current_params$imports)) {
          current_params$imports <- character()
        }
        current_params$imports <- c(current_params$imports, tokens[1])
      } else if (length(tokens) >= 2) {
        # Regular parameter
        param_name <- tokens[1]
        param_values <- tokens[-1]
        
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

#-------------------------------------------------------------------------------
#
# Writer functions
#
#-------------------------------------------------------------------------------

write_ins <- function(params, output_file) {
  lines <- character()
  
  # Helper function to format parameter values
  format_value <- function(value) {
    if (is.numeric(value)) {
      return(as.character(value))
    } else if (is.character(value)) {
      return(paste0('"', value, '"'))
    } else if (is.list(value) && length(value) > 1) {
      # Vector of values
      return(paste(sapply(value, format_value), collapse = " "))
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
    
    for (param_name in names(params$model)) {
      value <- params$model[[param_name]]
      lines <- c(lines, paste(param_name, format_value(value)))
    }
    lines <- c(lines, "")
  }
  
  # Write stand types (st)
  if (length(params$st) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! STAND TYPES")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    for (st_name in names(params$st)) {
      lines <- c(lines, paste0('st "', st_name, '" ('))
      
      st_data <- params$st[[st_name]]
      
      # Write imports first
      if (!is.null(st_data$imports)) {
        for (import_name in st_data$imports) {
          lines <- c(lines, paste(" ", import_name))
        }
      }
      
      # Write other parameters
      other_params <- st_data[!names(st_data) %in% "imports"]
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        lines <- c(lines, paste(" ", param_name, format_value(value)))
      }
      
      lines <- c(lines, ")")
      lines <- c(lines, "")
    }
  }
  
  # Write groups
  if (length(params$group) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! PARAMETER GROUPS")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    for (group_name in names(params$group)) {
      lines <- c(lines, paste0('group "', group_name, '" ('))
      
      group_data <- params$group[[group_name]]
      
      # Write imports first
      if (!is.null(group_data$imports)) {
        for (import_name in group_data$imports) {
          lines <- c(lines, paste(" ", import_name))
        }
      }
      
      # Write other parameters
      other_params <- group_data[!names(group_data) %in% "imports"]
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        lines <- c(lines, paste(" ", param_name, format_value(value)))
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
      
      pft_data <- params$pft[[pft_name]]
      
      # Write imports first
      if (!is.null(pft_data$imports)) {
        for (import_name in pft_data$imports) {
          lines <- c(lines, paste(" ", import_name))
        }
      }
      
      # Write other parameters (include should come early)
      other_params <- pft_data[!names(pft_data) %in% "imports"]
      
      # Write include parameter first if it exists
      if (!is.null(other_params$include)) {
        lines <- c(lines, paste(" include", format_value(other_params$include)))
        other_params$include <- NULL
      }
      
      # Write remaining parameters
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        lines <- c(lines, paste(" ", param_name, format_value(value)))
      }
      
      lines <- c(lines, ")")
      lines <- c(lines, "")
    }
  }
  
  # Write to file
  writeLines(lines, output_file)
  cat("INS file written to:", output_file, "\n")
}

# Additional utility function to create a modified version of parameters
modify_and_write_ins <- function(params, modifications, output_file) {
  # Apply modifications
  # modifications should be a list with structure: 
  # list(category = list(name = list(parameter = new_value, ...), ...)
  
  for (category in names(modifications)) {
    if (category %in% names(params)) {
      for (name in names(modifications[[category]])) {
        if (name %in% names(params[[category]])) {
          for (param_name in names(modifications[[category]][[name]])) {
            params[[category]][[name]][[param_name]] <- modifications[[category]][[name]][[param_name]]
          }
        }
      }
    }
  }
  
  # Write modified parameters
  write_vegetation_params(params, output_file)
}

# Function to compare two parameter sets and show differences
compare_parameters <- function(params1, params2) {
  cat("=== PARAMETER COMPARISON ===\n")
  
  # Compare model parameters
  cat("\nModel parameters differences:\n")
  all_model_params <- unique(c(names(params1$model), names(params2$model)))
  for (param in all_model_params) {
    val1 <- params1$model[[param]]
    val2 <- params2$model[[param]]
    if (!identical(val1, val2)) {
      cat("  ", param, ":", format_value(val1), "->", format_value(val2), "\n")
    }
  }
  
  # Compare PFTs
  cat("\nPFT differences:\n")
  all_pfts <- unique(c(names(params1$pft), names(params2$pft)))
  for (pft in all_pfts) {
    pft1 <- params1$pft[[pft]]
    pft2 <- params2$pft[[pft]]
    
    if (is.null(pft1)) {
      cat("  ", pft, ": ADDED\n")
    } else if (is.null(pft2)) {
      cat("  ", pft, ": REMOVED\n")
    } else {
      # Compare parameters within PFT
      all_params <- unique(c(names(pft1), names(pft2)))
      for (param in all_params) {
        val1 <- pft1[[param]]
        val2 <- pft2[[param]]
        if (!identical(val1, val2)) {
          cat("  ", pft, ".", param, ":", format_value(val1), "->", format_value(val2), "\n")
        }
      }
    }
  }
}

# Helper function used in comparison
format_value <- function(value) {
  if (is.numeric(value)) {
    return(as.character(value))
  } else if (is.character(value)) {
    return(paste0('"', value, '"'))
  } else if (is.list(value) && length(value) > 1) {
    return(paste0("c(", paste(sapply(value, as.character), collapse = ", "), ")"))
  } else {
    return(as.character(value))
  }
}