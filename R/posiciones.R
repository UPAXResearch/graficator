#' Función auxiliar (Pendiente descripción)
#'
#' Pendiente descripción
#'
#' @param df
#'
#' @return Pendiente descripción
#' @export
#'
#' @examples
posiciones <- function(df){
  pos <- df %>% select(2:((ncol(.)-1)/2)) %>%
    # summarise_all(funs(mean(.))) %>%
    slice(1) %>%
    unlist(., use.names=FALSE)

  return(c(0, cumsum(pos)[-length(pos)]) + pos/2)
}
