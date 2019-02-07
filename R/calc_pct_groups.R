#' Funcion auxiliar para generacion de porcentaje por factores
#'
#' @param df
#' @param col
#' @param q
#' @param negocio
#'
#' @return
#' @export
#'
#' @examples
calc_pct_groups <- function(df, col, q, negocio){
  s <- sym(col)
  client <- df %>%
    dplyr::select(!!s) %>%
    dplyr::filter(!is.na(!!s))
  n <- nrow(df)
  grouped <- client %>%
    dplyr::group_by(!!s) %>%
    dplyr::summarise(f = n()) %>%
    dplyr::mutate(pct = (f/n)) %>%
    dplyr::mutate(Negocio = eval(negocio),
                  Q = eval(q)) %>%
    dplyr::select(-f)

  names(grouped) = c("variable", "value", "Negocio", "Q")
  grouped = grouped %>% dplyr::select(Q, Negocio, variable, value)
  return(grouped)
}
