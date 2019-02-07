#' Generador tabla IPN historico
#'
#'Entran unicamente labels
#'
#' @param ipn_hist Eje y
#' @param Q_s Labels en el eje x
#'
#' @return plotly plot
#' @export
#'
#' @examples
make_historico_plot = function( ipn_hist, Q_s){
  data <- data.frame(Q_s, ipn_hist)
  data$Q_s = factor(data$Q_s, levels = Q_s)
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )

  ax_x <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = TRUE,
    showgrid = TRUE
  )

  ax_y <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )

  t <- list(
    family = "sans serif",
    size = 14,
    color = toRGB("grey50"))


  p <- plot_ly(data, x = ~Q_s, y = ~ipn_hist, type = 'scatter', text = paste0(ipn_hist, "%")  ,mode = 'lines+markers') %>%
    layout(title = "IPN Histórico",xaxis = ax_x, yaxis = ax_y, showlegend = FALSE) %>%
    add_text(textfont = t, textposition = "top")
  return(p)
}
