#' Función auxiliar que fundo una tabla de ion historico
#'
#' @param dfh
#' @param negocio
#'
#' @return df
#' @export
#'
#' @examples
fundir_hist = function(dfh, negocio){
  data <- dfh %>%
    dplyr::filter(Negocio == negocio) %>%
    reshape2::melt(id.vars = c("Q", "Negocio")) %>%
    dplyr::mutate_at(
      .funs = function(x) {
        gsub("^(\\dQ)\\s([0-9]+)", "\\2 \\1", x, perl = T)
      },
      .vars = vars("Q")) %>%
    dplyr::arrange(Q, variable) %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(value = value/100)
  return(data)
}
