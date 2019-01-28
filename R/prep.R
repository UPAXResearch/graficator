#' Data prep para la grafica grid
#'
#'this function and the two following it create the intermediate tables function 'makeTiles' needs
#' "ipn" is the column name for the NPS question, and 'prob' is the name of the column containing the question
#' "Con qué probabilidad seguiría siendo cliente de XYZ', o, 'con qué probabilidad volvería a obtener un crédito etc'
#' @param data
#' @param ipn
#' @param prob
#'
#' @return
#' @export
#'
#' @examples
prep <- function(data, ipn, prob) {
  df <- data %>%
    dplyr::filter(!is.na(!!sym(prob))) %>%
    mutate(grupo1 = if_else(!!sym(ipn) < 9, "DP", "P")) %>%
    mutate(grupo2 = if_else(!!sym(prob) < 9, "DP", "P")) %>%
    dplyr::group_by(grupo1, grupo2) %>%
    dplyr::summarise(freq = n()) %>%
    ungroup(group1, group2) %>%
    mutate(pct = 100*freq/sum(freq))

  return(df)
}
