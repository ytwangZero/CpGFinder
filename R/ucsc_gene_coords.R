# UCSC API functions
ucsc_gene_coords <- function(gene, genome = "hg38") {
  req <- request("https://api.genome.ucsc.edu/search") |>
    req_url_query(search = gene, genome = genome)
  
  resp <- req_perform(req)
  
  if (resp_status(resp) >= 400) {
    stop(sprintf("UCSC API request failed (HTTP %s).", resp_status(resp)))
  }
  
  txt <- resp_body_string(resp)
  x <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  
  pm <- x$positionMatches
  if (is.null(pm) || length(pm) == 0) {
    stop("No positionMatches returned. Check gene symbol and genome build.")
  }
  
  rows_list <- list()
  
  for (track in pm) {
    matches <- track$matches
    if (is.null(matches) || length(matches) == 0) next
    
    track_name <- track$trackName %||% track$name %||% NA_character_
    
    for (m in matches) {
      rows_list[[length(rows_list) + 1]] <- data.frame(
        track = track_name,
        posName = m$posName %||% NA_character_,
        position = m$position %||% NA_character_,
        canonical = m$canonical %||% NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(rows_list) == 0) {
    stop("No usable matches found in positionMatches.")
  }
  
  rows <- do.call(rbind, rows_list)
  
  rows$posName_lc <- tolower(rows$posName)
  gene_lc <- tolower(gene)
  
  pick <- rows[rows$track == "hgnc" & rows$posName_lc == gene_lc, , drop = FALSE]
  if (nrow(pick) == 0) {
    pick <- rows[rows$track == "knownGene" &
                   rows$canonical %in% TRUE &
                   grepl(paste0("^", gene, "\\b"), rows$posName, ignore.case = TRUE), ,
                 drop = FALSE]
  }
  if (nrow(pick) == 0) {
    pick <- rows[grepl(paste0("^", gene, "\\b"), rows$posName, ignore.case = TRUE), , drop = FALSE]
  }
  if (nrow(pick) == 0) pick <- rows[1, , drop = FALSE]
  
  pos <- pick$position[1]
  if (is.na(pos) || !grepl(":", pos) || !grepl("-", pos)) {
    stop("No parsable 'position' string returned.")
  }
  
  chrom <- sub(":.*$", "", pos)
  start <- as.integer(sub("^.*:(\\d+)-.*$", "\\1", pos))
  end   <- as.integer(sub("^.*-(\\d+)$", "\\1", pos))
  
  data.frame(
    gene = gene,
    genome = genome,
    chrom = chrom,
    start = start,
    end = end,
    source_track = pick$track[1],
    posName = pick$posName[1],
    stringsAsFactors = FALSE
  )
}