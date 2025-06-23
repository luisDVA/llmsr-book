# Script to render book with babelquarto and add hex grid to index files
render_book_with_hex <- function(site_url = NULL, profile = NULL, debug = TRUE) {
  library(fs)
  library(yaml)
  
  cat("Starting book rendering process...\n")
  
  # Step 1: Render the book with babelquarto
  cat("Step 1: Rendering book with babelquarto...\n")
  babelquarto::render_book(site_url = site_url, profile = profile)
  
  # Read the quarto config to find output directory
  config <- yaml::read_yaml("_quarto.yml")
  output_dir <- config[["project"]][["output-dir"]] %||% "_book"
  cat(paste0("Output directory from config: ", output_dir, "\n"))
  
  # Check for common output directories if specified one doesn't exist
  if (!dir_exists(output_dir)) {
    common_dirs <- c("_book", "_site", "docs", "public", "build")
    for (dir in common_dirs) {
      if (dir_exists(dir)) {
        output_dir <- dir
        cat(paste0("Found output directory: ", output_dir, "\n"))
        break
      }
    }
  }
  
  if (!dir_exists(output_dir)) {
    stop("Output directory not found")
  }
  
  # Step 2: Generate the hex grid using the existing tilecode.R script
  cat("Step 2: Generating hex grid using tilecode.R...\n")
  
  # Source the tilecode.R script directly
  source("tilecode.R")
  
  # Wait a moment for the file to be written
  Sys.sleep(1)
  
  # Check if the hex grid file was created in temp_hexsession folder
  hex_grid_folder <- "temp_hexsession"
  hex_grid_file <- file.path(hex_grid_folder, "_hexout.htm")
  
  if (!dir_exists(hex_grid_folder)) {
    stop("Hex grid folder not found: ", hex_grid_folder)
  }
  
  if (!file_exists(hex_grid_file)) {
    # Check for .html extension
    hex_grid_file <- file.path(hex_grid_folder, "_hexout.html")
    if (!file_exists(hex_grid_file)) {
      # Look for any HTML file in the folder
      html_files <- dir_ls(hex_grid_folder, glob = "*.htm*")
      if (length(html_files) > 0) {
        hex_grid_file <- html_files[1]
        cat(paste0("Found alternative hex grid file: ", hex_grid_file, "\n"))
      } else {
        stop("No hex grid HTML file found in ", hex_grid_folder)
      }
    }
  }
  
  cat(paste0("Hex grid generated: ", hex_grid_file, "\n"))
  
  # Step 3: Inject the hex grid into index files
  cat("Step 3: Injecting hex grid into index files...\n")
  
  # Read the hex grid HTML
  hex_grid_html <- readLines(hex_grid_file)
  
  # Extract the body content
  body_start <- grep("<body", hex_grid_html)
  body_end <- grep("</body>", hex_grid_html)
  body_content <- hex_grid_html[(body_start[1]+1):(body_end[1]-1)]
  
  # Extract head scripts
  head_start <- grep("<head", hex_grid_html)
  head_end <- grep("</head>", hex_grid_html)
  
  head_scripts <- character(0)
  if (length(head_start) > 0 && length(head_end) > 0) {
    head_content <- hex_grid_html[(head_start[1]+1):(head_end[1]-1)]
    script_lines <- grep("<script", head_content)
    
    for (i in seq_along(script_lines)) {
      script_start <- script_lines[i]
      script_end <- NULL
      
      # Find matching end script tag
      for (j in script_start:length(head_content)) {
        if (grepl("</script>", head_content[j])) {
          script_end <- j
          break
        }
      }
      
      if (!is.null(script_end)) {
        script <- head_content[script_start:script_end]
        head_scripts <- c(head_scripts, script)
      }
    }
  }
  
  # Function to inject widget into HTML file
  inject_widget <- function(html_file) {
    if (!file_exists(html_file)) {
      cat(paste0("HTML file not found: ", html_file, "\n"))
      return(FALSE)
    }
    
    cat(paste0("Injecting widget into ", html_file, "...\n"))
    
    # Read the HTML content
    html_content <- readLines(html_file)
    
    # Save a copy of the original file for debugging
    if (debug) {
      debug_file <- paste0(html_file, ".original")
      writeLines(html_content, debug_file)
      cat(paste0("Saved original HTML to: ", debug_file, "\n"))
      
      # Print file info
      cat(paste0("File size: ", file_size(html_file), " bytes\n"))
      cat(paste0("Lines in file: ", length(html_content), "\n"))
    }
    
    # Look for the explicit marker comment
    marker_comment <- "<!-- INSERT HEX GRID HERE -->"
    marker_indices <- grep(marker_comment, html_content, fixed = TRUE)
    
    if (length(marker_indices) == 0) {
      cat("Warning: Marker comment not found in HTML file.\n")
      
      # Try with different whitespace or case variations
      alt_markers <- c(
        "<!--INSERT HEX GRID HERE-->",
        "<!-- INSERT HEX GRID HERE-->",
        "<!--INSERT HEX GRID HERE -->",
        "<!-- insert hex grid here -->"
      )
      
      for (marker in alt_markers) {
        marker_indices <- grep(marker, html_content, fixed = TRUE)
        if (length(marker_indices) > 0) {
          cat(paste0("Found alternative marker: ", marker, "\n"))
          break
        }
      }
      
      # If still no marker, use fallback insertion point
      if (length(marker_indices) == 0) {
        cat("Using fallback insertion point...\n")
        
        # Dump the first 100 lines for debugging
        if (debug) {
          cat("First 100 lines of HTML file:\n")
          cat(paste(head(html_content, 100), collapse = "\n"), "\n")
        }
        
        # Fallback to inserting after the first heading or paragraph
        h1_indices <- grep("<h1", html_content)
        if (length(h1_indices) > 0) {
          # Find the end of the h1 tag
          for (i in h1_indices[1]:length(html_content)) {
            if (grepl("</h1>", html_content[i])) {
              marker_indices <- i
              break
            }
          }
        }
        
        # If still no marker, try after a paragraph
        if (length(marker_indices) == 0) {
          p_indices <- grep("<p>", html_content)
          if (length(p_indices) > 0) {
            # Find the end of the first paragraph
            for (i in p_indices[1]:length(html_content)) {
              if (grepl("</p>", html_content[i])) {
                marker_indices <- i
                break
              }
            }
          }
        }
        
        # If still no marker, use a position near the top
        if (length(marker_indices) == 0) {
          body_indices <- grep("<body", html_content)
          if (length(body_indices) > 0) {
            marker_indices <- body_indices[1] + 2
          } else {
            marker_indices <- min(20, length(html_content))
          }
        }
      }
    } else {
      cat(paste0("Found explicit marker at line ", marker_indices[1], "\n"))
    }
    
    # Use the first marker found
    inject_point <- marker_indices[1]
    
    # Inject the widget with unique identifiers for each file
    # Create a unique ID based on the filename
    file_id <- gsub("[^a-zA-Z0-9]", "", basename(html_file))
    
    modified_content <- c(
      html_content[1:inject_point],
      paste0("<!-- BEGIN HEX GRID INJECTION (", file_id, ") -->"),
      paste0("<div id='hexgrid-", file_id, "' class='hexsession-container' style='margin: 20px 0;'>"),
      body_content,
      "</div>",
      paste0("<!-- END HEX GRID INJECTION (", file_id, ") -->"),
      html_content[(inject_point+1):length(html_content)]
    )
    
    # Inject scripts into head with unique IDs to avoid conflicts
    if (length(head_scripts) > 0) {
      head_end <- grep("</head>", modified_content)
      if (length(head_end) > 0) {
        # Modify script content to use unique IDs
        modified_scripts <- head_scripts
        for (i in seq_along(modified_scripts)) {
          # Add file_id to any widget IDs to ensure uniqueness
          modified_scripts[i] <- gsub("htmlwidget-([0-9a-zA-Z]+)", 
                                      paste0("htmlwidget-", file_id, "-\\1"), 
                                      modified_scripts[i])
        }
        
        modified_content <- c(
          modified_content[1:(head_end[1]-1)],
          paste0("<!-- BEGIN HEX GRID SCRIPTS (", file_id, ") -->"),
          modified_scripts,
          paste0("<!-- END HEX GRID SCRIPTS (", file_id, ") -->"),
          modified_content[head_end[1]:length(modified_content)]
        )
      }
    }
    
    # Write the modified HTML
    writeLines(modified_content, html_file)
    
    # Save a copy of the modified file for debugging
    if (debug) {
      debug_file <- paste0(html_file, ".modified")
      writeLines(modified_content, debug_file)
      cat(paste0("Saved modified HTML to: ", debug_file, "\n"))
    }
    
    cat(paste0("Successfully injected widget into ", html_file, "\n"))
    
    return(TRUE)
  }
  
  # Specific files to inject into - English and Spanish index files
  main_index_html <- file.path(output_dir, "index.html")
  es_index_html <- file.path(output_dir, "es", "index.es.html") # Note the correct filename
  
  # Inject into English index
  if (file_exists(main_index_html)) {
    inject_widget(main_index_html)
  } else {
    cat("Warning: English index file not found: ", main_index_html, "\n")
  }
  
  # Inject into Spanish index
  if (file_exists(es_index_html)) {
    inject_widget(es_index_html)
  } else {
    cat("Warning: Spanish index file not found: ", es_index_html, "\n")
    
    # Check for other possible Spanish index filenames
    alt_es_files <- c(
      file.path(output_dir, "es", "index.html"),
      file.path(output_dir, "es.html"),
      file.path(output_dir, "index.es.html")
    )
    
    for (alt_file in alt_es_files) {
      if (file_exists(alt_file)) {
        cat(paste0("Found alternative Spanish index: ", alt_file, "\n"))
        inject_widget(alt_file)
        break
      }
    }
  }
  
  # Step 4: Clean up
  cat("Step 4: Cleaning up...\n")
  if (dir_exists(hex_grid_folder)) {
    dir_delete(hex_grid_folder)
    cat(paste0("Removed temporary folder: ", hex_grid_folder, "\n"))
  }
  
  cat("Book rendering complete with interactive hex grid!\n")
  return(TRUE)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}