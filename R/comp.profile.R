#' comp.profile
#'
#' plots concentrations?
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
comp.profile <- function(x) {
  cols = names(x)
  plotdf = x %>%
    tibble::rowid_to_column() %>%
    tidyr::pivot_longer(-rowid) %>%
    dplyr::mutate(name = factor(name, levels = cols))
  plotdf %>%
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, group = rowid)) +
    ggplot2::geom_line(color = "red", alpha = .1) +
    ggplot2::theme_bw() +
    ggplot2::ylab('Concentration (ppm)') +
    ggplot2::xlab('')
}


