#' Funcion aux: Cuenta renglones distintos a NA
#'
#' Esta función cuenta el número de entradas distintas a NA que hay en la columna `c` en la tabla `df`
#' @param df
#' @param c Columna de tipo String
#'
#' @return Número de filas que no sean NA
#' @export
#'
#' @examples

calcBaseIPN <- function(df, c) {
  stopifnot(nrow(df) > 0) # Detener todo si no hay tabla
  d <- makeSym(c)
  n <- sum(!is.na(df %>% dplyr::select(!!d)))
  return(n)
}
