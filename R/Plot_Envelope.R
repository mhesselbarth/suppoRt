#' plot_envelope
#'
#'
#' @details
#' This functions provides a plotting style for envelope objects of the spatstat
#' package (for more information please see ?spatstat::envelope). The location of the
#' observed value in relation to the simulation envelopes of the null model data is
#' indicated by an additional colour bar at the bottom of the plot.
#'
#' @param input Envelope object of the spatstat package
#' @param labels Name of the labels
#' \cr 1 = obs > hi
#' \cr 2 = lo < obs < hi
#' \cr 3 = obs < lo
#' @param title Title of the plot
#' @param xlab x label of the plot
#' @param ylab y label of the plot
#' @param size Size of the colour bar
#' @param full_fun Plot full function or only line to indicate
#' @param standarized If TRUE obs = obs - theo deviation from null model

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

  data <- tibble::as.tibble(input)
  names(data) <- c('r', 'obs', 'theo', 'lo', 'hi')

  if(standarized == TRUE){
    data <- dplyr::mutate(data, obs = obs - theo,
                                lo = lo - theo ,
                                hi = hi - theo,
                                theo = theo - theo)
  }

  data <- dplyr::mutate(data, type = dplyr::case_when(obs > hi ~ labels[1],
                                                      obs >=  lo & obs <=  hi ~ labels[2],
                                                      obs < lo ~ labels[3]))

  color_scale <- c('#440154FF',
                   '#238A8DFF',
                   '#FDE725FF')
  names(color_scale) <- labels

  if(full_fun == TRUE){

    gg_plot <- ggplot2::ggplot(data) +
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
    gg_plot <- ggplot2::ggplot(data) +
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

