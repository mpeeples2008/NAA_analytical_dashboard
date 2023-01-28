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
  x = x %>% dplyr::mutate_all(list(function(c) case_when(!is.finite(c)~0,TRUE~c)))
  lims <- range(x)
  plot(seq(1:ncol(x)),x[1,],xaxt='n',type='l',col=rgb(1,0,0,0.1),xlab='',ylab='Concentration (ppm)',ylim=c(floor(lims[1]),ceiling(lims[2])))
  for (i in 2:nrow(x)) {
  points(seq(1:ncol(x)),x[i,],type='l',col=rgb(1,0,0,0.1))
  }
}
