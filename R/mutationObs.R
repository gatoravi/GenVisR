#' format mutation observations
#' 
#' Create a data frame of mutation observations
#' @name mutationObs
#' @param data object of class data frame with columns trv_type and amino acid change
#' @return object of class data frame giving mutation observations

mutationObs <- function(data, fill_value, label_column, rep.fact, rep.dist.lmt, attr.fact, adj.max, adj.lmt, iter.max)
{
  ###################################################################
  ####### Function to extract mutations and their coordinates #######
  ###################################################################
  
  # extract the mutation types
  fill <- as.character(data[,eval(fill_value)])
  
  # extract the mutation coordinates
  mutation_coord <- data$amino_acid_change
  mutation_coord <- as.numeric(gsub("[\\D]+", "", mutation_coord, perl=TRUE))
  
  # combine mutation type and mutation coord into a data frame
  mutation_data <- as.data.frame(cbind(mutation_coord, fill))
  mutation_data$mutation_coord <- as.numeric(as.character(mutation_data$mutation_coord))
  colnames(mutation_data) <- c('mutation_coord', eval(fill_value))
  
  # add extra column giving height of Y axis for points to be plotted
  mutation_data$height_max <- 2
  
  # extract optional labels for points to be plotted
  if(!is.null(label_column))
  {
    mutation_data$labels <- as.character(data[,eval(label_column)])
  }
  
  # Dodge mutation coordinates on the x axis
  message("applying force field to observed mutations")
  mutation_data <- mutation_data[order(mutation_coord),] 
  mutation_data$coord_x_dodge <- dodge_coord_x(as.vector(mutation_data$mutation_coord), rep.fact=rep.fact, rep.dist.lmt=rep.dist.lmt, attr.fact=attr.fact, adj.max=adj.max, adj.lmt=adj.lmt, iter.max=iter.max)
  
  # Redefine and return grouping information and then dodge y coordinates
  mutation_data$group <- group_mutation_coord(as.vector(mutation_data$mutation_coord))
  mutation_data$coord_y_dodge <- dodge_coord_y(mutation_data, track='top')
  
  return(mutation_data)
}