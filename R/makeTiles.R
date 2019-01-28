#' Grafica Tiles/grid
#'
#' @param data
#' @param ipn
#' @param prob
#' @param ytitle
#'
#' @return
#' @export
#'
#' @examples
makeTiles <- function(data, ipn, prob, ytitle) {

  tamanog = 20
  tamanom = 16
  tamanoc = 12

  df_pct <- prep_pct(data, ipn, prob)
  df_freq <- prep_freq(data, ipn, prob)
  plot <- plotly_empty() %>%
    layout(title = F,
           xaxis = list(title = "Calificación IPN",
                        showgrid = FALSE,
                        showline = F,
                        showticklabels = F,
                        zeroline = FALSE,
                        domain = c(0.00, 0.8)),
           yaxis = list(title = ytitle,
                        showgrid = FALSE,
                        showline = F,
                        showticklabels = F,
                        zeroline = FALSE),
           shapes = list(
             list(type = "rect",
                  fillcolor = 'cc3232',
                  line = list(color = "FFFFFF",
                              width = 10.0),
                  opacity = 1,
                  x0 = 0.0, x1 = 0.8,
                  y0 = 0.0, y1 = 0.8,
                  xref = 'x',
                  yref = 'paper'),
             list(type = "rect",
                  fillcolor = '298428',
                  line = list(color = "FFFFFF",
                              width = 10.0),
                  opacity = 1,
                  x0 = 0.8, x1 = 1,
                  y0 = 0.8, y1 = 1,
                  xref = 'x',
                  yref = 'paper'),
             list(type = "rect",
                  fillcolor = 'FF3E3E',
                  line = list(color = "FFFFFF",
                              width = 10.0),
                  opacity = 0.9,
                  x0 = 0.0, x1 = 0.8,
                  y0 = 0.8, y1 = 1,
                  xref = 'x',
                  yref = 'paper'),
             list(type = "rect",
                  fillcolor = '47E546',
                  line = list(color = "FFFFFF",
                              width = 10.0),
                  opacity = 1,
                  x0 = 0.8, x1 = 1,
                  y0 = 0, y1 = 0.8,
                  xref = 'x',
                  yref = 'paper')
           )) %>%
    # labeling the y-axis
    add_annotations(xref = 'x', yref = 'paper', x = 0.02, y = c(0.78,0.98),
                    xanchor = 'left',
                    text = paste0(round(df_pct$DP[1:2],1),"%"),
                    font = list(family = 'sans', size = tamanog,
                                color = 'FFFFFF'),
                    showarrow = FALSE, align = 'left') %>%
    add_annotations(xref = 'x', yref = 'paper', x = 0.775, y = c(0.78,0.98),
                    xanchor = 'right',
                    text = c("Baja lealtad,<br>baja recomendación",
                             "Alta lealtad,<br>baja recomendación"),
                    font = list(family = 'sans', size = tamanoc,
                                color = 'FFFFFF'),
                    showarrow = FALSE, align = 'right') %>%
    add_annotations(xref = 'x', yref = 'paper', x = 0.98, y = c(0.78,0.98),
                    xanchor = 'right',
                    text = paste0(round(df_pct$P[1:2],1),"%"),
                    font = list(family = 'sans', size = tamanog,
                                color = 'FFFFFF'),
                    showarrow = FALSE, align = 'right') %>%
    add_annotations(xref = 'paper', yref = 'paper', x = 0.8, y = c(0.78),
                    xanchor = 'left',
                    text = "Baja lealtad,<br>alta recomendación",
                    font = list(family = 'sans', size = tamanoc,
                                color = c('47E546')),
                    showarrow = FALSE, align = 'left') %>%
    add_annotations(xref = 'paper', yref = 'paper', x = 0.8, y = c(0.98),
                    xanchor = 'left',
                    text = "Alta lealtad,<br>alta recomendación",
                    font = list(family = 'sans', size = tamanoc,
                                color = c('298428')),
                    showarrow = FALSE, align = 'left') %>%
    add_annotations(xref = 'x', yref = 'paper', x = 0.98, y = c(0.02,0.86),
                    xanchor = 'right',
                    text = paste0("Casos: ", round(df_freq$P[1:2],1)),
                    font = list(family = 'sans', size = tamanom,
                                color = 'FFFFFF'),
                    showarrow = FALSE, align = 'right') %>%
    add_annotations(xref = 'x', yref = 'paper', x = 0.02, y = c(0.02,0.86),
                    xanchor = 'left',
                    text = paste0("Casos: ", round(df_freq$DP[1:2],1)),
                    font = list(family = 'sans', size = tamanom,
                                color = 'FFFFFF'),
                    showarrow = FALSE, align = 'left')

  return(plot)

}
