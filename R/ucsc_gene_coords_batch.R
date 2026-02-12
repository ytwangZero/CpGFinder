ucsc_gene_coords_batch <- function(genes, genome = "hg38", sleep_sec = 1) {
  genes <- unique(trimws(genes))
  genes <- genes[nzchar(genes)]
  
  out <- vector("list", length(genes))
  
  for (i in seq_along(genes)) {
    g <- genes[i]
    
    res <- tryCatch(
      ucsc_gene_coords(g, genome = genome),
      error = function(e) {
        data.frame(
          gene = g,
          genome = genome,
          chrom = NA_character_,
          start = NA_integer_,
          end = NA_integer_,
          source_track = NA_character_,
          posName = NA_character_,
          stringsAsFactors = FALSE
        ) |>
          transform(error_message = conditionMessage(e))
      }
    )
    
    if (!"error_message" %in% names(res)) res$error_message <- NA_character_
    
    out[[i]] <- res
    
    if (sleep_sec > 0 && i < length(genes)) Sys.sleep(sleep_sec)
  }
  
  do.call(rbind, out)
}