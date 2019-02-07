#' Genera grafica de ipn historico por tipo cliente
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
ipn_hist_tipo_plot = function(data){
  plot <- ggplot(data, aes(x = Q, y = value))
  plot2 <- plot + geom_point(aes(colour = variable))
  plot3 <- plot2 + geom_line(aes(group = variable, colour = variable))
  plot4 <- plot3 +
    geom_text(aes(label = paste0(100*value,"%"), y = value + 0.03,colour = variable), vjust = -1.5) +
    scale_y_continuous(name = NULL,
                       breaks = seq(0, 0.80, by = 0.10),
                       minor_breaks = seq(0.05, .75, 0.10),
                       limits = c(0,.80),
                       labels = scales::percent) +
    scale_x_discrete(name = NULL,
                     expand = expand_scale(add = 0.25)) +
    scale_color_manual(values = c("Detractores" = "#A4150B", "Pasivos" = "#FAC152", "Promotores" = "#01B140")) +
    theme_minimal() +
    theme(panel.grid.major.y = element_line(linetype = "dotted"),
          panel.grid.minor.y = element_line(linetype = "dotted"),
          legend.title=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank())
  plot5 = ggplotly(plot4)

  return(plot5)
}
