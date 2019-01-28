#' Calculo del IPN
#'
#' Esta función calcula el IPN con base en la columna cuyo nombre está en la
#' variable `lab` (la cual puede contener el nombre de la columna en
#' tipo `string` o `symbol`), que tiene clasificados a los usuarios
#' como detractor, pasivo o promotor. Esta función se utiliza principalmente para
#' imprimir el IPN dentro de la gráfica de anillo, pero puede utilizarse para
#' calcular IPNs en general, mientras se tenga la tabla una columna que
#' clasifique a los usuarios con niveles "Detractor", "Pasivo" y "Promotor".
#' @param df Base de datos
#' @param column Columna de calificaciones en formato string.
#'
#' @return
#' @export
#'
#' @examples
calcIPN <- function(df, column) {

  stopifnot(nrow(df) > 0) # Detener todo si no hay tabla
  stopifnot(ncol(df) > 0) # Detener todo si no hay tabla

  # Verificando que "columna" sea "character" o "symbol"
  # (i.e. un nombre de columna) y transformando en "symbol" para dplyr:
  c <- makeSym(column)

  # Tabla con el número de detractores, pasivos y promotores
  nps <- df %>%
    dplyr::select(!!c) %>%
    dplyr::filter(!is.na(!!c)) %>%
    dplyr::group_by(!!c) %>%
    dplyr::summarise(n = n())

  # Base total
  n <- calcBaseIPN(df, column)

  # ipn de interés redondeado a dos decimales
  i <- (100*abs(nps[3,2]-nps[1,2])/n) %>% pull()
  ipn <- round(i,2)

  return(ipn)
}
