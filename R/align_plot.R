#' align plots on an axis
#' 
#' given a list of plots, align them on plotting space
#' @name align_plot
#' @param plot_list list of ggplot objects
#' @param axis character string to specify the axis to align plotting space on, one of both, width, height
#' @return ggplotGrob object
#' @import gridExtra
#' @import gtable

align_plot <- function(plot_list, axis='both')
{
    # convert all ggplot objects to grob obects
    plots <- lapply(plot_list, ggplotGrob)
  
    # if specified align the plot widths
    if(axis == 'width' | axis == 'both')
    {
        # Obtain the max width in all plots in the list
        maxwidth <- do.call(grid::unit.pmax, lapply(plots, extr_ggplotGrob_width))
    
        # set the max width for all plots in the list
        plots <- lapply(plots, assign_ggplotGrob_width, maxwidth)
    }
  
    # if specified alter the plot heights
    if(axis == 'height' | axis == 'both')
    {
        # Obtain the max height in all plots in the list
        maxheight <- do.call(grid::unit.pmax, lapply(plots, extr_ggplotGrob_height))
    
        # set the max height for all plots in the list
        plots <- lapply(plots, assign_ggplotGrob_width, maxheight)		
    }
  
    # Combine the plots of now equal widths/heights
    plots <- do.call(arrangeGrob, plots)
  
    return(plots)	
}