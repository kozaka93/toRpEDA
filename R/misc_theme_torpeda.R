#' toRpEDA theme for ggplot2 objects
#'
#' @import ggplot2
#' @return theme for ggplot2 objects
#' @export
#' @rdname theme_torpeda

theme_torpeda <- function() {
  theme_bw(base_line_size = 0) %+replace%
    theme(axis.ticks = element_blank(), legend.background = element_blank(),
          legend.key = element_blank(), panel.background = element_blank(),
          panel.border = element_blank(), strip.background = element_blank(),
          plot.background = element_blank(), complete = TRUE,
          legend.direction = 'horizontal', legend.position = 'top',
          axis.line.y = element_line(color = 'white'),
          axis.ticks.y = element_line(color = 'white'),
          axis.title = element_text(color = 'black'),
          plot.title = element_text(color = 'black', size = 16, hjust = 0),
          plot.subtitle = element_text(color = 'black', hjust = 0),
          axis.text = element_text(color = 'black', size = 10),
          strip.text = element_text(color = 'black', size = 12, hjust = 0),
          axis.text.x = element_text(color = 'black'),
          axis.text.y = element_text(color = 'black'),
          panel.grid.major.y = element_line(color = 'grey90', size = 0.5, linetype = 1),
          panel.grid.minor.y = element_line(color = 'grey90', size = 0.5,  linetype = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank())

}

#' toRpEDA color palettes for ggplot2 objects
#'
#' @param n number of colors for color palette
#'
#' @return color palette as vector of charactes
#' @export
#' @rdname colors_torpeda

colors_discrete_torpeda <- function(n = 2) {
  if (n == 1) return("#537cc2")
  if (n == 2) return(c( "#537cc2", "#ff6a00"))
  if (n == 3) return(c( "#537cc2", "#ff6a00", "#000000"))
  if (n == 4) return(c( "#537cc2", "#ff6a00", "#000000", "#8ecae6"))
  if (n == 5) return(c( "#537cc2", "#ff6a00", "#000000", "#8ecae6", "#ffb703"))
  if (n == 6) return(c( "#537cc2", "#ff6a00", "#000000", "#8ecae6", "#ffb703", "#023047"))
  if (n == 7) return(c( "#537cc2", "#ff6a00", "#000000", "#8ecae6", "#ffb703", "#023047", "#fb8500"))
  if (n == 8) return(c( "#537cc2", "#ff6a00", "#000000", "#8ecae6", "#ffb703", "#023047", "#fb8500", "#219ebc"))
  c("#537cc2", "#ff6a00", "#000000", "#8ecae6", "#ffb703", "#023047", "#fb8500", "#219ebc")[((0:(n-1)) %% 8) + 1]
}


#' @export
#' @rdname colors_torpeda

colors_diverging_torpeda <- function() {
  c("#537cc2", "#ff6a00")
}

