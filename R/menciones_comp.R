#' Tabla de menciones con comparativo al Q anterior
#'
#' Esta función recibe los datos del actual Q y el Q anterior y con ello calcula
#' el la frecuencia de menciones y porcentajes de éstas. Luego realiza un left_join
#' con fuzzymatching (utilizando distancia de texto coseno, modificable) por la izquierda,
#' y con ello saca las diferencias porcentuales del Q anterior a este.
#' Esta función es una versión más elaborada de makeMenciones y la manda a llamar
#' para su funcionamiento.
#'
#' @param df Base de datos actual
#' @param df_old Base de datos del Q pasado
#' @param menciones_col Vector con los nombres de las columnas donde están las menciones de `df`
#' @param menciones_col_old Vector con los nombres de las columnas donde están las menciones de `df_old`
#' @param q Número de levantamiento (i.e. 3Q)
#' @param tipo Tipo de menciones que se quieren ("Positivas", "Negativas" o "Todas")
#' @param cruce Columna con la cual se van a cruzar las menciones
#' @param filtro Tipos de clientes: "Promotor", "Pasivo", "Detractor" o "Pasivo + Detractor"
#' @param filtro_col Columna en `df` en la cual está el filtro buscado
#' @param filtro_col_old Columna en `df_old` en la cual está el filtro buscado
#'
#' @return Tabla con columnas: mención, casos, porcentaje actual, porcentaje pasado, diferencia y código html para imprimir
#' @export
#'
#' @examples
menciones_comp <- function(df,
                           df_old,
                           menciones_col,
                           menciones_col_old,
                           q,
                           tipo = "Todas",
                           cruce = "Ninguno",
                           filtro = "Ninguno",
                           filtro_col = "",
                           filtro_col_old = filtro_col) {

  q_prev <- previous_q(q)
  nombres <- c(
    "Mención",
    paste0("Casos (", q, "Q)"),
    paste0("Porcentaje (", q, "Q) %"),
    paste0("Porcentaje (", q_prev, "Q) %"),
    "Diferencia (pp)")

  arrow <- function(x){
    if(is.na(x) || x == 0){return("")}
    else if(x > 0){
      return("<i class=\"glyphicon glyphicon-chevron-up\" style=\"color:#298428\"></i>")
    } else {
      return("<i class=\"glyphicon glyphicon-chevron-down\" style=\"color:#cc3232\"></i>")
    }
  }

  if (cruce != "Ninguno"){
    res <- makeMenciones(df,
                         menciones_col,
                         tipo,
                         cruce,
                         filtro,
                         filtro_col)
    return(res)

  } else {
    t1 <- makeMenciones(df,
                        menciones_col,
                        tipo,
                        cruce,
                        filtro,
                        filtro_col)
    t2 <- makeMenciones(df_old,
                        menciones_col_old,
                        tipo,
                        cruce,
                        filtro,
                        filtro_col_old)

    res <- t1 %>%
      fuzzyjoin::stringdist_left_join(
        t2 %>% select(c(1, 3)),
        by = c(Mención = "Mención"),
        method = "cosine",
        max_dist = 0.005,
        distance_col = "dist") %>%
      dplyr::select(Mención.x, Casos, Porcentaje.x, Porcentaje.y) %>%
      dplyr::mutate(diff = round(Porcentaje.x - Porcentaje.y,1)) %>%
      dplyr::mutate_at(function(x) ifelse(is.na(x), "", x),
                       .vars = c("Porcentaje.y", "diff")) %>%
      dplyr::rowwise(.) %>%
      dplyr::mutate(asdf = arrow(diff))
    colnames(res) <- c(nombres,"Cambio")
    return(res)

  }
}
