#' Funcion aux para tabla menciones
#'
#' @param df
#' @param variable
#'
#' @return
#' @export
#'
#' @examples
getLevels <- function(df, variable) {

  niveles <- levels(droplevels(df[,variable]))
  # print(niveles)
  return(niveles)
}
