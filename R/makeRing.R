#' Grafica de disco para IPN
#'
#' Esta función genera la gráfica de anillo de los porcentajes correspondientes
#' a los niveles de la columna `column` de la tabla `df`, ignorando NAs.
#' La gráfica se genera utilizando plotly.
#' Esta función ignora NAs, y presupone que se van a graficar tres niveles:
#' "Detractor", "Pasivo" y "Promotor", en ese orden, coloréandolos
#' de rojo, amarillo y verde, respectivamente.
#' Es posible graficar la frecuencia de un mayor número de niveles, pero
#' plotly escogerá los colores de manera automática, a menos de que se
#' especifiquen en los parámetros de la función.
#'
#' @param df Base de datos
#' @param column Columna donde se encuentran las calificaciones para el calculo del IPN
#' @param title Titulo de la gráfica, argumento se le pasa a plotly
#' @param textsize Tamaño del texto, argumento se le pasa a plotly
#' @param colors Colores de promoteres, pasivos y detractores, argumento se le pasa a plotly
#'
#' @return Objeto tipo plot_ly
#' @export
#'
#' @examples
makeRing <- function(df,
                     column,
                     title = "",
                     textsize = 16,
                     colors = c('cc3232', 'e7b416', '298428')){

  # El parámetro color son los colores que se le van a asociar a los tres
  #niveles: "Detractor", "Pasivo" y "Promotor", en ese orden.

  # Verificando que lab_var sea character y convirtiendo a sym
  lab <- makeSym(column)

  # Se calcula el IPN general para imprimirse en el centro de la gráfica
  ipn <- calcIPN(df, column)

  # Aquí se genera la gráfica
  plot <- df %>%
    dplyr::filter(!is.na(!!lab)) %>%
    dplyr::group_by(!!lab) %>%
    dplyr::summarise(freq = n()) %>%
    plot_ly(labels = ~eval(lab), values = ~freq,
            textinfo = 'label+percent',
            marker=list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1.5)),
            insidetextfont = list(color = '#FFFFFF', size = textsize)) %>%
    add_pie(hole = 0.6) %>%
    layout(title = title,
           showlegend = F,
           autosize = T,
           xaxis = list(showgrid = F, zeroline = F, showticklabels = F, automargin=T),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = F, automargin=T),
           # margin = list(t = 50, b = 30),
           annotations = list(
             # Aquí se pone la base en la esquina inferior derecha
             list(x = 0.90,
                  xref='paper',
                  y = -0.08,
                  yref='paper',
                  text =paste('Base: ', calcBaseIPN(df,column)),
                  showarrow = F),
             # Aquí se pone el IPN total en el hueco del anillo
             list(x= 0.5,
                  xref = 'paper',
                  y = 0.5,
                  yref = 'paper',
                  text = sprintf(paste0("<b>", "IPN: ",ipn, "</b>")),
                  showarrow = F,
                  font = list(family = 'sans', size = textsize))
           )
    )
  return(plot)
}
