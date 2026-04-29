# ============================================================================
# EPBC Monitoring System - GitHub Actions Version
# Version 4.0 - Simplified output (no HTML dependencies)
# ============================================================================

# ============================================================================
# SETUP
# ============================================================================

library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(tabulapdf)
library(pdftools)
library(httr)

# ============================================================================
# CONFIGURATION
# ============================================================================

CONFIG <- list(
  trigger_species = data.frame(
    scientific = c(
      "Lathamus discolor",
      "Anthochaera phrygia",
      "Thinornis cucullatus",
      "Botaurus poiciloptilus",
      "Zanda latirostris",
      "Calyptorhynchus banksii naso",
      "Zanda baudinii",
      "Numenius madagascariensis",
      "Acanthiza insularis",
      "Acanthornis magna greeniana",
      "Calyptorhynchus banksii graptogyne",
      "Leipoa ocellata",
      "Atrichornis clamosus",
      "Pezoporus flaviventris",
      "Pedionomus torquatus",
      "Erythrotriorchis radiatus",
      "Pezoporus occidentalis"
    ),
    common = c(
      "swift parrot",
      "regent honeyeater",
      "hooded plover",
      "australasian bittern",
      "carnaby's black-cockatoo",
      "forest red-tailed black-cockatoo",
      "baudin's black-cockatoo",
      "eastern curlew",
      "king island brown thornbill",
      "king island scrubtit",
      "south-eastern red-tailed black-cockatoo",
      "malleefowl",
      "noisy scrub-bird",
      "western ground parrot",
      "plains-wanderer",
      "red goshawk",
      "night parrot"
    ),
    stringsAsFactors = FALSE
  ),

  download_folder    = "epbc_pdfs",
  tracking_file      = "data/processed_referrals.csv",
  output_folder      = "output"
)

# ============================================================================
# FUNCTION 1: Scrape EPBC URLs
# ============================================================================

scrape_epbc_urls_simple <- function() {

  cat("Starting Chrome session...\n")
  b <- ChromoteSession$new()

  tryCatch({
    cat("Navigating to EPBC portal...\n")
    b$Page$navigate("https://epbcpublicportal.environment.gov.au/open-for-comments/")
    b$Page$loadEventFired()
    Sys.sleep(5)

    cat("Loading dynamic content...\n")
    b$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight);")
    Sys.sleep(3)

    html_result  <- b$Runtime$evaluate("document.documentElement.outerHTML")
    html_content <- html_result$result$value
    parsed_page  <- read_html(html_content)

    all_links  <- parsed_page %>% html_nodes("a") %>% html_attr("href")
    link_texts <- parsed_page %>% html_nodes("a") %>% html_text(trim = TRUE)

    valid_indices <- !is.na(all_links) & all_links != "" & all_links != "#"
    clean_links   <- all_links[valid_indices]
    clean_texts   <- link_texts[valid_indices]

    relevant_indices <- grepl("project|referral|proposal|comment|detail", clean_links, ignore.case = TRUE) |
      grepl("project|referral|proposal|comment|view|details", clean_texts, ignore.case = TRUE)

    relevant_urls  <- clean_links[relevant_indices]
    relevant_texts <- clean_texts[relevant_indices]

    unique_indices <- !duplicated(relevant_urls)
    relevant_urls  <- relevant_urls[unique_indices]
    relevant_texts <- relevant_texts[unique_indices]

    full_urls <- ifelse(
      grepl("^http", relevant_urls),
      relevant_urls,
      ifelse(
        grepl("^/", relevant_urls),
        paste0("https://epbcpublicportal.environment.gov.au", relevant_urls),
        paste0("https://epbcpublicportal.environment.gov.au/", relevant_urls)
      )
    )

    link_types <- ifelse(
      grepl("project",  full_urls, ignore.case = TRUE), "project",
      ifelse(
        grepl("referral", full_urls, ignore.case = TRUE), "referral",
        ifelse(
          grepl("proposal", full_urls, ignore.case = TRUE), "proposal",
          ifelse(
            grepl("comment", full_urls, ignore.case = TRUE), "comment",
            "other"
          )
        )
      )
    )

    results <- data.frame(
      url          = full_urls,
      link_text    = relevant_texts,
      type         = link_types,
      extracted_at = Sys.time(),
      stringsAsFactors = FALSE
    )

    cat("Found", nrow(results), "relevant URLs\n")
    return(results)

  }, error = function(e) {
    cat("Error occurred:", e$message, "\n")
    return(NULL)
  }, finally = {
    b$close()
  })
}

# ============================================================================
# FUNCTION 2: Find PDF on referral page
# ============================================================================

find_pdf_with_chromote <- function(referral_url_or_df, epbc_number_or_row = NULL) {

  if (is.data.frame(referral_url_or_df)) {
    urls_df    <- referral_url_or_df
    row_number <- ifelse(is.null(epbc_number_or_row), 1, epbc_number_or_row)
    if (row_number > nrow(urls_df)) stop("Row number exceeds dataframe size")
    referral_url <- urls_df$url[row_number]
    epbc_number  <- urls_df$link_text[row_number]
  } else {
    referral_url <- referral_url_or_df
    epbc_number  <- epbc_number_or_row
    if (is.null(epbc_number)) stop("epbc_number is required when passing individual URL")
  }

  message("Looking for PDF with browser: ", epbc_number)
  b <- ChromoteSession$new()

  tryCatch({
    b$Page$navigate(referral_url)
    b$Page$loadEventFired()
    Sys.sleep(5)

    html_result  <- b$Runtime$evaluate("document.documentElement.outerHTML")
    html_content <- html_result$result$value

    pdf_pattern <- "_entity/sharepointdocumentlocation/[^\"'\\s]+\\?file=[^\"'\\s]*\\.pdf[^\"'\\s]*"
    pdf_matches <- str_extract_all(html_content, pdf_pattern)[[1]]

    if (length(pdf_matches) > 0) {
      clean_path <- sub("^/", "", pdf_matches[1])
      pdf_url    <- paste0("https://epbcpublicportal.environment.gov.au/", clean_path)
      message("Found PDF")
      return(pdf_url)
    }

    message("No PDF found")
    return(NULL)

  }, finally = {
    b$close()
  })
}

# ============================================================================
# FUNCTION 3: Process all PDFs
# ============================================================================

process_all_pdfs <- function(urls_df) {
  urls_df$pdf_url <- NA

  for (i in 1:nrow(urls_df)) {
    tryCatch({
      message(paste("Processing row", i, "of", nrow(urls_df)))
      urls_df$pdf_url[i] <- find_pdf_with_chromote(urls_df, i)
      Sys.sleep(2)
    }, error = function(e) {
      message("Error processing row ", i, ": ", e$message)
      urls_df$pdf_url[i] <- NA
    })
  }

  return(urls_df)
}

# ============================================================================
# FUNCTION 4: Download PDFs
# ============================================================================

download_pdfs_for_search <- function(urls_df, download_folder = CONFIG$download_folder) {

  if (!dir.exists(download_folder)) {
    dir.create(download_folder, recursive = TRUE)
    message("Created folder: ", download_folder)
  }

  successful_downloads <- 0
  failed_downloads     <- 0

  for (i in 1:nrow(urls_df)) {

    if (is.na(urls_df$pdf_url[i])) {
      message("Row ", i, " (", urls_df$link_text[i], "): No PDF URL - skipping")
      failed_downloads <- failed_downloads + 1
      next
    }

    tryCatch({
      message("Downloading ", i, " of ", nrow(urls_df), ": ", urls_df$link_text[i])

      epbc_for_filename <- gsub("/", "_", urls_df$link_text[i])
      epbc_for_filename <- gsub("[^A-Za-z0-9_]", "", epbc_for_filename)
      filename          <- paste0(epbc_for_filename, "_referral.pdf")
      filepath          <- file.path(download_folder, filename)

      response <- GET(urls_df$pdf_url[i],
                      write_disk(filepath, overwrite = TRUE),
                      timeout(60))

      if (status_code(response) == 200 && file.exists(filepath)) {
        file_size <- round(file.size(filepath) / 1024, 2)
        message("Successfully downloaded: ", filename, " (", file_size, " KB)")
        successful_downloads <- successful_downloads + 1
      } else {
        message("Failed to download (HTTP ", status_code(response), "): ", urls_df$link_text[i])
        failed_downloads <- failed_downloads + 1
        if (file.exists(filepath)) file.remove(filepath)
      }

      Sys.sleep(2)

    }, error = function(e) {
      message("Error downloading ", urls_df$link_text[i], ": ", e$message)
      failed_downloads <- failed_downloads + 1
    })
  }

  message("\n=== Download Summary ===")
  message("Successful: ", successful_downloads)
  message("Failed: ", failed_downloads)

  return(list(
    successful = successful_downloads,
    failed     = failed_downloads,
    folder     = download_folder
  ))
}

# ============================================================================
# FUNCTION 5: Extract species tables from PDF
# ============================================================================

extract_species_tables <- function(pdf_path) {

  empty_result <- data.frame(
    direct_impact   = character(),
    indirect_impact = character(),
    species         = character(),
    common_name     = character(),
    stringsAsFactors = FALSE
  )

  tryCatch({
    total_pages <- pdftools::pdf_info(pdf_path)$pages
    all_species <- empty_result

    for (page_num in 1:total_pages) {

      tables_on_page <- tryCatch({
        suppressWarnings(suppressMessages(
          tabulapdf::extract_tables(pdf_path, pages = page_num)
        ))
      }, error = function(e) list())

      if (length(tables_on_page) == 0) next

      for (tbl in tables_on_page) {
        tbl <- as.data.frame(tbl)
        if (nrow(tbl) == 0) next

        # CASE 1: Clean 4-column table
        if (ncol(tbl) >= 4) {
          col1 <- toupper(as.character(tbl[[1]]))
          col2 <- toupper(as.character(tbl[[2]]))

          if (any(col1 %in% c("YES", "NO")) && any(col2 %in% c("YES", "NO"))) {
            for (i in 1:nrow(tbl)) {
              direct   <- toupper(trimws(as.character(tbl[[1]][i])))
              indirect <- toupper(trimws(as.character(tbl[[2]][i])))
              species  <- as.character(tbl[[3]][i])
              common   <- as.character(tbl[[4]][i])

              if (direct %in% c("YES", "NO") && indirect %in% c("YES", "NO")) {
                all_species <- rbind(all_species, data.frame(
                  direct_impact   = direct,
                  indirect_impact = indirect,
                  species         = trimws(species),
                  common_name     = trimws(common),
                  stringsAsFactors = FALSE
                ))
              }
            }
          }
        }

        # CASE 2: Mangled 1-column table with \t delimiters
        if (ncol(tbl) == 1) {
          all_text <- paste(as.character(tbl[[1]]), collapse = "\n")

          if (grepl("(Yes|No)\t(Yes|No)\t", all_text, ignore.case = TRUE)) {
            lines <- unlist(strsplit(all_text, "\n"))

            for (line in lines) {
              match <- str_match(line, "(?i)^(Yes|No)\t(Yes|No)\t([^\t]+)\t(.+)$")

              if (!is.na(match[1, 1])) {
                all_species <- rbind(all_species, data.frame(
                  direct_impact   = toupper(trimws(match[1, 2])),
                  indirect_impact = toupper(trimws(match[1, 3])),
                  species         = trimws(match[1, 4]),
                  common_name     = trimws(match[1, 5]),
                  stringsAsFactors = FALSE
                ))
              }
            }
          }
        }
      }
    }

    if (nrow(all_species) == 0) {
      message("No species found in PDF")
      return(empty_result)
    }

    all_species <- unique(all_species)
    all_species$species     <- gsub("\\s+", " ", trimws(all_species$species))
    all_species$common_name <- gsub("\\s+", " ", trimws(all_species$common_name))
    all_species <- all_species[
      !is.na(all_species$species) &
        all_species$species != "" &
        !grepl("^species$", all_species$species, ignore.case = TRUE),
    ]

    message("Extracted ", nrow(all_species), " species rows")
    return(all_species)

  }, error = function(e) {
    message("Error extracting from PDF: ", e$message)
    return(empty_result)
  })
}

# ============================================================================
# FUNCTION 6: Search for trigger species
# ============================================================================

search_trigger_species <- function(trigger_species_df = CONFIG$trigger_species,
                                   pdf_folder = CONFIG$download_folder,
                                   urls_df = NULL) {

  pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

  if (length(pdf_files) == 0) {
    message("No PDF files found in folder: ", pdf_folder)
    return(data.frame(
      referral_number = character(),
      trigger_species = character(),
      impact_type     = character(),
      pdf_url         = character(),
      stringsAsFactors = FALSE
    ))
  }

  trigger_scientific <- tolower(gsub("\\s+", " ", trimws(trigger_species_df$scientific)))
  trigger_common     <- tolower(gsub("\\s+", " ", trimws(trigger_species_df$common)))

  results <- data.frame(
    referral_number = character(),
    trigger_species = character(),
    impact_type     = character(),
    pdf_url         = character(),
    stringsAsFactors = FALSE
  )

  for (pdf_file in pdf_files) {

    filename        <- basename(pdf_file)
    referral_number <- gsub("_referral\\.pdf$", "", filename)
    referral_number <- gsub("_", "/", referral_number)

    message("Searching: ", referral_number)

    species_table <- extract_species_tables(pdf_file)

    if (nrow(species_table) == 0) {
      message("  No species tables found")
      next
    }

    pdf_species <- tolower(gsub("\\s+", " ", trimws(species_table$species)))
    pdf_common  <- tolower(gsub("\\s+", " ", trimws(species_table$common_name)))

    found_species      <- character()
    found_impact_types <- character()

    for (i in seq_along(trigger_scientific)) {

      sci_match    <- which(grepl(trigger_scientific[i], pdf_species, fixed = TRUE))
      common_match <- which(grepl(trigger_common[i],     pdf_common,  fixed = TRUE))
      matched_rows <- unique(c(sci_match, common_match))

      for (row_idx in matched_rows) {
        direct   <- toupper(trimws(species_table$direct_impact[row_idx]))
        indirect <- toupper(trimws(species_table$indirect_impact[row_idx]))

        has_direct   <- grepl("^YES", direct)
        has_indirect <- grepl("^YES", indirect)

        if (has_direct || has_indirect) {
          impact_type <- paste0(
            if (has_direct) "Direct" else "",
            if (has_direct && has_indirect) " & " else "",
            if (has_indirect) "Indirect" else ""
          )

          species_name       <- str_to_title(trigger_species_df$common[i])
          found_species      <- c(found_species, species_name)
          found_impact_types <- c(found_impact_types, impact_type)

          message("  FOUND: ", species_name, " - ", impact_type, " impact")
        }
      }
    }

    if (length(found_species) > 0) {
      pdf_url <- NA
      if (!is.null(urls_df) && "pdf_url" %in% names(urls_df) && "link_text" %in% names(urls_df)) {
        url_match <- urls_df[urls_df$link_text == referral_number, "pdf_url"]
        if (length(url_match) > 0 && !is.na(url_match[1])) pdf_url <- url_match[1]
      }

      results <- rbind(results, data.frame(
        referral_number = referral_number,
        trigger_species = paste(unique(found_species), collapse = ", "),
        impact_type     = paste(unique(found_impact_types), collapse = ", "),
        pdf_url         = pdf_url,
        stringsAsFactors = FALSE
      ))
    }
  }

  message("\n=== Search Summary ===")
  message("PDFs searched: ", length(pdf_files))
  message("Referrals with trigger species: ", nrow(results))

  return(results)
}

# ============================================================================
# FUNCTION 7: Load processed referrals
# ============================================================================

load_processed_referrals <- function() {
  if (file.exists(CONFIG$tracking_file)) {
    df <- read.csv(CONFIG$tracking_file, stringsAsFactors = FALSE)
    if (!"impact_type"     %in% names(df)) df$impact_type     <- NA
    if (!"referral_number" %in% names(df)) df$referral_number <- character()
    if (!"trigger_species" %in% names(df)) df$trigger_species <- character()
    if (!"pdf_url"         %in% names(df)) df$pdf_url         <- character()
    if (!"date_processed"  %in% names(df)) df$date_processed  <- character()
    return(df)
  } else {
    return(data.frame(
      referral_number = character(),
      trigger_species = character(),
      impact_type     = character(),
      pdf_url         = character(),
      date_processed  = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# ============================================================================
# FUNCTION 8: Save processed referrals
# ============================================================================

save_processed_referrals <- function(processed_df) {
  data_dir <- dirname(CONFIG$tracking_file)
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  write.csv(processed_df, CONFIG$tracking_file, row.names = FALSE)
}

# ============================================================================
# FUNCTION 9: Identify new referrals
# ============================================================================

identify_new_referrals <- function(current_results, processed_referrals) {
  if (nrow(current_results) == 0) {
    return(data.frame(
      referral_number = character(),
      trigger_species = character(),
      impact_type     = character(),
      pdf_url         = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (nrow(processed_referrals) == 0) return(current_results)

  return(current_results[
    !current_results$referral_number %in% processed_referrals$referral_number,
  ])
}

# ============================================================================
# FUNCTION 10: Create text output
# ============================================================================

create_text_output <- function(referrals_df, output_folder = CONFIG$output_folder) {

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  output_file <- file.path(output_folder, "new_epbc_referrals.txt")

  if (nrow(referrals_df) == 0) {
    writeLines(
      c(
        paste("EPBC Referrals Update -", format(Sys.Date(), "%d %B %Y")),
        "",
        "No new referrals containing trigger species found."
      ),
      output_file
    )
  } else {
    header <- c(
      paste("EPBC Referrals Update -", format(Sys.Date(), "%d %B %Y")),
      paste("Found", nrow(referrals_df), "new referral(s) with trigger species"),
      paste("Note: referrals are open for comment for 10 business days"),
      "",
      paste(rep("-", 80), collapse = ""),
      sprintf("%-20s %-40s %-15s", "Referral Number", "Trigger Species", "Impact Type"),
      paste(rep("-", 80), collapse = "")
    )

    rows <- sprintf("%-20s %-40s %-15s",
                    referrals_df$referral_number,
                    referrals_df$trigger_species,
                    referrals_df$impact_type)

    writeLines(c(header, rows), output_file)
  }

  cat("Created output file:", output_file, "\n")
  return(output_file)
}

# ============================================================================
# MAIN FUNCTION
# ============================================================================

run_epbc_monitoring <- function() {

  cat("=== EPBC TRIGGER SPECIES MONITORING ===\n")
  cat("Started at:", as.character(Sys.time()), "\n\n")

  # Clear PDFs from previous run
  if (dir.exists(CONFIG$download_folder)) {
    old_files <- list.files(CONFIG$download_folder, pattern = "\\.pdf$", full.names = TRUE)
    if (length(old_files) > 0) {
      file.remove(old_files)
      cat("Cleared", length(old_files), "old PDF files\n")
    }
  }

  # Load previously processed referrals
  processed_referrals <- load_processed_referrals()
  cat("Loaded", nrow(processed_referrals), "previously processed referrals\n")

  # Step 1: Scrape EPBC URLs
  cat("\n1. Scraping EPBC referral URLs...\n")
  urls <- scrape_epbc_urls_simple()
  if (is.null(urls) || nrow(urls) == 0) stop("Failed to scrape URLs from EPBC portal")
  cat("Found", nrow(urls), "URLs\n")

  # Step 2: Find PDF URLs
  cat("\n2. Finding PDF URLs for each referral...\n")
  urls_with_pdfs <- process_all_pdfs(urls)
  cat("Found", sum(!is.na(urls_with_pdfs$pdf_url)), "PDF URLs\n")

  # Step 3: Download PDFs
  cat("\n3. Downloading PDFs...\n")
  download_result <- download_pdfs_for_search(urls_with_pdfs, CONFIG$download_folder)
  if (download_result$successful == 0) stop("No PDFs were successfully downloaded")
  cat("Successfully downloaded", download_result$successful, "PDFs\n")

  # Step 4: Search for trigger species
  cat("\n4. Searching for trigger species...\n")
  current_results <- search_trigger_species(
    trigger_species_df = CONFIG$trigger_species,
    pdf_folder         = CONFIG$download_folder,
    urls_df            = urls_with_pdfs
  )
  cat("Found", nrow(current_results), "referrals with trigger species\n")

  # Step 5: Identify new referrals
  cat("\n5. Identifying new referrals...\n")
  new_referrals <- identify_new_referrals(current_results, processed_referrals)
  cat("Found", nrow(new_referrals), "NEW referrals\n")

  # Step 6: Create text output
  cat("\n6. Creating text output...\n")
  output_file <- create_text_output(new_referrals)

  # Step 7: Update tracking file
  if (nrow(new_referrals) > 0) {
    cat("\n7. Updating tracking file...\n")

    updated_processed <- rbind(
      processed_referrals,
      data.frame(
        referral_number = new_referrals$referral_number,
        trigger_species = new_referrals$trigger_species,
        impact_type     = new_referrals$impact_type,
        pdf_url         = new_referrals$pdf_url,
        date_processed  = as.character(Sys.Date()),
        stringsAsFactors = FALSE
      )
    )

    save_processed_referrals(updated_processed)
    cat("Updated tracking file with", nrow(new_referrals), "new referrals\n")
    cat("\nALERT: New referrals found!\n")

  } else {
    cat("\nNo new referrals found.\n")
  }

  # Clean up PDFs
  old_files <- list.files(CONFIG$download_folder, pattern = "\\.pdf$", full.names = TRUE)
  if (length(old_files) > 0) {
    file.remove(old_files)
    cat("Cleaned up", length(old_files), "temporary PDF files\n")
  }

  cat("\n=== MONITORING COMPLETE ===\n")
  cat("Finished at:", as.character(Sys.time()), "\n")
  cat("Output file:", output_file, "\n")
}

# Run
run_epbc_monitoring()
