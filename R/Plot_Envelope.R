#' Function to plot simulation envelopes
#'
#' This functions provides a plotting style for envelope objects of the spatstat
#' package (for more information please see ?spatstat::envelope)
#' @param input [\code{envelope(1)}]\cr Envelope object of the spatstat package
#' @param labels [\code{string(3)}]\cr Name of the labels.
#' \cr 1 = obs > hi
#' \cr 2 = lo < obs < hi
#' \cr 3 = obs < lo

#' @importFrom magrittr "%>%"
#' @export
Plot.Envelope <- function(input, labels=c('clustering', ' randomness', 'segregation')){

  if(!is(input, "envelope")){stop('Please provide envelope object of the spatstat package')}

  if(length(labels) != 3){
    labels <- c('clustering', ' randomness', 'segregation')
    print('Not enough labels provided - using clustering, randomness and segregation')
  }

  # color_scale <- c(labels[1] = "#440154FF",
  #                  labels[2] = "#238A8DFF",
  #                  labels[3] = "#FDE725FF")

  input %>%
    tibble::as.tibble() %>%
    dplyr::mutate(type=dplyr::case_when(obs > hi ~ labels[1],
                                        obs > lo & obs < hi ~ labels[2],
                                        obs < lo ~ labels[3])) %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(aes(x=r, ymin=lo, ymax=hi), fill='grey', alpha=0.3) +
    ggplot2::geom_line(aes(x=r, y=obs, linetype='Observed')) +
    ggplot2::geom_line(aes(x=r, y=theo, linetype='Theoretical')) +
    ggplot2::geom_line(aes(x=r, y=min(c(lo, obs)), colour=type, group=type), size=2.5) +
    ggplot2::scale_color_manual(name='', values=c("#440154FF", "#238A8DFF", "#FDE725FF")) +
    # ggplot2::scale_color_manual(name='', values=color_scale) +
    ggplot2::scale_linetype_manual(name='', values=c(1,2)) +
    ggplot2::labs(x='r', y='f(r)') +
    ggplot2::theme_bw()
}

