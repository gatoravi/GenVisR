#' Mutation Sample Cutoff
#' 
#' Subset a MAF file keeping only entries in a selection of genes
#' @name mutation_sample_subset
#' @param x a data frame in long format with columns 'gene', 'trv_type'
#' @param genes character vector listing genes to plot
#' @return a subset data frame

mutation_sample_subset <- function(x, genes)
{
  genes <- c(genes, NA)
  x <- x[(x$gene %in% genes), ]
  
  return(x)
}