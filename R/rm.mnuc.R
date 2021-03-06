#' Remove multinucleotide codes
#' 
#' Given a data frame with columns reference and variants remove all multinucleotides from data
#' @name rm.mnuc
#' @param x Object of class data frame containing columns 'reference', 'variant'
#' @return Object of class data frame with multi nucleotide codes removed

rm.mnuc <- function(x)
{
  original_size <- nrow(x)
  
  # Find and multi nucleotide codes
  x <- x[grep('[ACGT]{2,}', x$reference, perl=TRUE, invert=TRUE),]
  x <- x[grep('[ACGT]{2,}', x$variant, perl=TRUE, invert=TRUE),]
  
  new_size <- nrow(x)
  
  if(new_size != original_size)
  {
      warning("Multi Nucleotide codes are not currently supported, removed: ", original_size - new_size, " multi nucleotides present in data") 
  }
  
  return(x)
}