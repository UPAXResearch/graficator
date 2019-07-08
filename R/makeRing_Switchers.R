makeRing_Switchers<-function (df, column, title = "", textsize = 16, colors = c("cc3232",
                                                                             "e7b416","ffff9d", "298428"))
{
  lab <- makeSym(column)
  #ipn <- calcIPN(df, column)
  plot <-
    # df[!is.na(df[,lab]),lab]
    df %>%
    dplyr::filter(!is.na(!(!lab))) %>%
    dplyr::group_by(!(!lab)) %>%
    dplyr::summarise(freq = n()) %>%
    plot_ly(labels = ~eval(lab),
                                             values = ~freq, textinfo = "label+percent", marker = list(colors = colors,
                                                                                                       line = list(color = "#FFFFFF", width = 1.5)), insidetextfont = list(color = "#FFFFFF",
                                                                                                                                                                           size = textsize)) %>% add_pie(hole = 0.6) %>% layout(title = title,
                                                                                                                                                                                                                                showlegend = F, autosize = T, xaxis = list(showgrid = F,
                                                                                                                                                                                                                                                                           zeroline = F, showticklabels = F, automargin = T),
                                                                                                                                                                                                                                yaxis = list(showgrid = F, zeroline = F, showticklabels = F,
                                                                                                                                                                                                                                             automargin = T), annotations = list(list(x = 0.9,
                                                                                                                                                                                                                                                                                      xref = "paper", y = -0.08, yref = "paper", text = paste("Base: ",
                                                                                                                                                                                                                                                                                                                                              calcBaseIPN(df, column)), showarrow = F)
                                                                                                                                                                                                                                                                                 # , list(x = 0.5,
                                                                                                                                                                                                                                                                                 #                                                                                                             xref = "paper", y = 0.5, yref = "paper",
                                                                                                                                                                                                                                                                                 #                                                                                                             text = sprintf(paste0("<b>",
                                                                                                                                                                                                                                                                                 #                                                                                                              "IPN: ", ipn, "</b>")),
                                                                                                                                                                                                                                                                                 #                                                                                                             showarrow = F, font = list(family = "sans",
                                                                                                                                                                                                                                                                                 #                                                                                                             size = textsize))
                                                                                                                                                                                                                                                                                 ))
  return(plot)
}
