#' Funcion aux para tabla menciones
#'
#' @param df
#' @param variable
#'
#' @return
#' @export
#'
#' @examples
getLevelsTable <- function(df, variable) {
  # print("funcion getLevelsTable")
  # print(variable)
  if (variable == "") {
    cn <- c("Mención", "Porcentaje", "Frecuencia")
  } else {
    cn <- c("Mención", getLevels(df, variable), "Total")
    # print(cn)
  }
  return(cn)
}
