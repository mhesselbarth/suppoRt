#' Function to plot simulation envelopes
#'
#' This functions provides a plotting style for envelope objects of the spatstat
#' package (for more information please see ?spatstat::envelope). The location of the
#' observed value in relation to the simulation envelopes of the null model data is
#' indicated by an additional colour bar at the bottom of the plot.
#'
#' @param input [\code{envelope(1)}]\cr Envelope object of the spatstat package
#' @param labels [\code{string(3)}]\cr Name of the labels
#' \cr 1 = obs > hi
#' \cr 2 = lo < obs < hi
#' \cr 3 = obs < lo
#' @param title [\code{string(1)}]\cr Title of the plot
#' @param xlab [\code{string(1)}]\cr x label of the plot
#' @param ylab [\code{string(1)}]\cr y label of the plot
#' @param size [\code{numeric(1)}]\cr Size of the colour bar
#' @param full_fun [\code{logical(1)}]\cr Plot full function or only line to indicate
#' @param standarized [\code{logical(1)}]\cr If TRUE obs = obs - theo
#' deviation from null model

#' @export
plot_envelope <- function(input,
                          labels = c('clustering', ' randomness', 'segregation'),
                          title = NULL, xlab = NULL, ylab = NULL,
                          size = 5,
                          full_fun = T,
                          standarized = F){

  if(!is(input, 'envelope') && !is(input, 'data.frame')){stop('Please provide envelope object or dataframe')}

  if(length(labels) !=  3){
    labels <- c('clustering', ' randomness', 'segregation')
    print('Not enough labels provided - using clustering, randomness and segregation')
  }

  if(is.null(xlab)){xlab <- 'r'}
  if(is.null(ylab)){ylab <- 'f(r)'}

  data <- input %>%
    tibble::as.tibble() %>%
    setNames(c('r', 'obs', 'theo', 'lo', 'hi'))

  if(standarized == TRUE){
    data <- data %>%
      dplyr::mutate(obs = obs - theo,
                    lo = lo - theo ,
                    hi = hi - theo,
                    theo = theo - theo)
  }

  data <- data %>%
    dplyr::mutate(type = dplyr::case_when(obs > hi ~ labels[1],
                                          obs >=  lo & obs <=  hi ~ labels[2],
                                          obs < lo ~ labels[3]))

  color_scale <- c('#440154FF',
                   '#238A8DFF',
                   '#FDE725FF')
  names(color_scale) <- labels

  if(full_fun == T){

    gg_plot <- data %>%
      ggplot2::ggplot() +
      ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = lo, ymax = hi), fill = 'grey') +
      ggplot2::geom_line(ggplot2::aes(x = r, y = obs, linetype = 'Observed'), size = 0.5) +
      ggplot2::geom_line(ggplot2::aes(x = r, y = theo, linetype = 'Theoretical'), size = 0.5) +
      ggplot2::geom_line(ggplot2::aes(x = r, y = min(c(lo, obs)), colour = type, group = 'x'), size = size) +
      ggplot2::scale_color_manual(name = '', values = color_scale) +
      ggplot2::scale_linetype_manual(name = '', values = c(1,2)) +
      ggplot2::labs(x = xlab, y = ylab, title = title) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(legend.position = 'bottom')
  }

  else{
    gg_plot <-  data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x = r, y = 0, colour = type, group = 'x'), size = 5) +
      ggplot2::coord_cartesian(ylim = c(0, 0.1)) +
      ggplot2::scale_color_manual(name = '', values = color_scale) +
      ggplot2::labs(x = xlab, y = '', title = title) +
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(legend.position = 'bottom',
                     strip.background = ggplot2::element_blank(),
                     axis.line.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }
  return(gg_plot)
}

