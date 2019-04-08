#' Grafica de barras horizontales para IPN con diferencia del Q pasado
#'
#' @param df
#' @param label_var
#' @param level_var
#' @param df_ant
#' @param filtros
#'
#' @return Plotly plot
#' @export
#'
#' @examples
makeBar_hist <- function(df, label_var, level_var, df_ant, lever_var_ant, filtros = NULL,q){

  top_labels <- c("Detractores", "Pasivos", "Promotores")
  colors <- c('cc3232', 'e7b416', '298428')
  d <- prepros(df, label_var, level_var, filtros)
  d_ant = prepros(df_ant, label_var, lever_var_ant, filtros)

  d_join = d %>% left_join(d_ant, by = setNames(lever_var_ant, level_var), suffix = c("", ".ant"))
  # (d[,"fPromotor"]-d[,"fDetractor"])/d[,"fTotal"]

  d_IPN = d_join %>% mutate(IPN = round(100*(fPromotor - fDetractor)/fTotal,1) , IPN_ant = round(100*(fPromotor.ant - fDetractor.ant)/fTotal.ant, 1)) %>%
    mutate(IPN.dif = IPN - IPN_ant)

  d_dif = d_IPN %>% mutate(Sn = ifelse(IPN.dif == 0, "-", "")) %>%
    mutate(Pos = ifelse(IPN.dif > 0, paste0("+", round(IPN.dif,1), "pp"), "")) %>%
    mutate(Neg = ifelse(IPN.dif < 0, paste0(round(IPN.dif,1), "pp"), "")) %>%
    mutate(Sn = ifelse(is.na(IPN.dif), "", Sn)) %>%
    mutate(Pos = ifelse(is.na(IPN.dif), "", Pos)) %>%
    mutate(Neg = ifelse(is.na(IPN.dif), "", Neg))

  fontsize = 13
  lab <- sym(label_var)
  lev <- sym(level_var)

  d_character <- d_dif %>%
    mutate_at(vars(level_var), as.character) %>%
    mutate_at(vars(level_var), function(x)ifelse(is.na(x),"Sin categorizar",x))

  nivelesy <- rev(unique(d_character[,level_var]))
  d_character[,level_var] <- factor(d_character[,level_var], levels=nivelesy)



  N <- length(nivelesy)

  labels <- unique(d_character[,level_var])

  plot <- d_character %>%
    plot_ly(x = ~Detractor, y = ~eval(lev),
            type = 'bar',
            orientation = 'h',
            marker = list(color = colors[1],
                          line = list(color = 'FFFFFF', width = 1)),
            hoverinfo = 'text',
            text = ~paste0('Casos: ', fDetractor,
                           '<br>', round(Detractor, 2), "%")) %>%
    add_trace(x = ~Pasivo, marker = list(color = colors[2],
                                         line = list(color = 'FFFFFF', width = 1)),
              hoverinfo = 'text',
              text = ~paste0('Casos: ', fPasivo,
                             '<br>', round(Pasivo, 2), "%")) %>%
    add_trace(x = ~Promotor, marker = list(color = colors[3],
                                           line = list(color = 'FFFFFF', width = 1)),
              hoverinfo = 'text',
              text = ~paste0('Casos: ', fPromotor,
                             '<br>', round(Promotor, 2), "%")) %>%
    layout(xaxis = list(title = "",
                        automargin = F,
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.10, 0.95)),
           yaxis = list(title = "",
                        automargin = T,
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           # autosize = T, #height=150+N*35, #width='auto',
           barmode = 'stack',
           paper_bgcolor = 'FFFFFF', plot_bgcolor = 'FFFFFF',
           ## margin = list(l = 120, r = 0, t = 100, b = 80),
           # margin = list(l = 100, r = 0, t = 70, b = 50),
           margin = list(l = 90, r = 0),
           showlegend = FALSE,
           title=paste0("Desglose de resultados IPN ",q,"Q 2019")) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.099, y = rev(nivelesy),
                    xanchor = 'right',
                    text = labels,
                    font = list(family = 'sans', size = fontsize,
                                color = 'rgb(87,87,87)'),
                    showarrow = FALSE, align = 'right') %>%
    # labeling the percentages of each bar (x_axis)
    add_annotations(xref = 'paper', yref = 'y',
                    x = 0.108, y = rev(nivelesy),
                    text = paste0(round(d[,"Detractor"],1), '%'),
                    font = list(family = 'sans', size = fontsize,
                                color = 'FFFFFF'),
                    showarrow = FALSE) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = ~Detractor + Pasivo/2, y = rev(nivelesy),
                    text = paste0(round(d[,"Pasivo"],1), '%'),
                    font = list(family = 'sans', size = fontsize,
                                color = 'FFFFFF'),
                    showarrow = FALSE) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = 97, y = rev(nivelesy),
                    text = paste0(round(d[,"Promotor"],1), '%'),
                    font = list(family = 'sans', size = fontsize,
                                color = 'FFFFFF'),
                    showarrow = FALSE) %>%
    # labeling the in-group NPS
    add_annotations(xref = 'x', yref = 'y',
                    x = 104, y = rev(nivelesy),
                    # xanchor = 'left',
                    text = paste0("IPN:", round(100*(d[,"fPromotor"]-d[,"fDetractor"])/d[,"fTotal"],1)),
                    font = list(family = 'sans', size = fontsize,
                                color = '404040',
                                align = "left"),
                    showarrow = FALSE) %>%
    # Diferencia Positivas
    add_annotations(xref = 'x', yref = 'y',
                    x = 112, y = rev(nivelesy),
                    xanchor = 'left',
                    text =  d_dif %>% pull(Pos),
                    font = list(family = 'sans', size = fontsize,
                                color = '008000',
                                align = "left"),
                    showarrow = FALSE) %>%
    #Diferencias Negativas
    add_annotations(xref = 'x', yref = 'y',
                    x = 112, y = rev(nivelesy),
                    xanchor = 'left',
                    text =  d_dif %>% pull(Neg),
                    font = list(family = 'sans', size = fontsize,
                                color = 'FF0000',
                                align = "left"),
                    showarrow = FALSE) %>%

    # Sin diferencia
    add_annotations(xref = 'x', yref = 'y',
                    x = 112, y = rev(nivelesy),
                    xanchor = 'left',
                    text =  d_dif %>% pull(Sn),
                    font = list(family = 'sans', size = fontsize,
                                color = '000000',
                                align = "left"),
                    showarrow = FALSE) %>%
    # labeling the first Likert scale (on the top)
    add_annotations(xref = 'x', yref = 'y',
                    x = posiciones(d),
                    y = N + 0.10,
                    text = top_labels,
                    font = list(family = 'sans', size = fontsize,
                                color = 'rgb(67, 67, 67)'),
                    showarrow = FALSE) %>%
    add_annotations(xref='paper', yref='paper',
                    x = 0.5, y = -0.10,
                    text =paste('Base: ', sum(!is.na(df %>% select(!!lab)))),
                    showarrow = F,
                    font = list(size = fontsize))
  return(plot)
}
