#' Convert to z score
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
zScore = function(x){
  as.data.frame(scale(prop.table(as.matrix(x), 1) * 100)) %>% dplyr::mutate_all(round,3)
}
