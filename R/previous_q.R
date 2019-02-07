#' Funcion previous_q:
#' Dado un entero n (1 <= n <= 4) se obtiene n-1 correspondiente al trimestre (Q) anterior (módulo 4)
#' @param n Entero entre 1 y 4
#'
#' @return n-1 (mod 4)
#' @export
#'
#' @examples

previous_q <- function(n){
  stopifnot(0 < n && n < 5)
  q <- (n-1) %% 4
  if(q == 0) q = 4
  return(q)
}
