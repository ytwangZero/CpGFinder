# Function to find CpGs for genes
find_cpgs_for_genes <- function(gene_coords, annotation_data) {
  res <- NULL
  
  for (i in 1:nrow(gene_coords)) {
    if (is.na(gene_coords$start[i]) | is.na(gene_coords$end[i])) next
    
    tmp <- annotation_data %>% 
      filter(chr == gene_coords$chrom[i],
             pos >= gene_coords$start[i],
             pos <= gene_coords$end[i]) %>% 
      mutate(gene = gene_coords$gene[i],
             genome = gene_coords$genome[i])
    
    if (nrow(tmp) == 0) next
    
    if (is.null(res)) {
      res <- tmp
    } else {
      res <- rbind(res, tmp)
    }
  }
  
  res
}