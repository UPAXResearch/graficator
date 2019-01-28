#' Función auxiliar de limpieza para grafica grid
#'
#'Esta funcion filtra la base y convierte las preguntas de escala 0 a 10 a nunéricas
#' @param data
#' @param cols
#' @param filt
#'
#' @return DF con las columnas limpias
#' @export
#'
#' @examples
cleaning <- function(data, cols,filt = NULL){
  if(is.null(filt)){
    df = data
  } else{
    df = data %>% dplyr::filter(!!sym(filt))
  }
  df <- df %>%
    mutate_at(.funs = ~ gsub("^(\\d+).*", "\\1", .x),
              .vars = cols) %>%
    mutate_at(.funs = as.numeric,
              .vars = cols)

  return(df)
}
