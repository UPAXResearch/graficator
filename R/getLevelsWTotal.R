#' Funcion aux para tabla menciones
#'
#' @param df
#' @param variable
#'
#' @return
#' @export
#'
#' @examples
getLevelsWTotal <- function(df, variable) {

  nivelesT <- c(getLevels(df, variable), "Total")
  return(nivelesT)
}
