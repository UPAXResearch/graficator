#' Generador de tabla de menciones
#'
#' @param df
#' @param menciones_col
#' @param tipo
#' @param cruce
#' @param filtro
#' @param filtro_col
#'
#' @return
#' @export
#'
#' @examples

makeMenciones <- function(df, menciones_col, tipo = "Todas", cruce = "Ninguno",
                          filtro = "Ninguno",
                          filtro_col = "") {
  # print(cruce)
  # print(tipo)
  # print(filtro)
  # print(filtro_col)

  ##Funcion aux
  aux = function(x){
    return(gsub("^<br>Casos:\\s$" , "", x, perl = TRUE))
  }

  if(tipo == "Todas"){
    tipo <- NULL
  } else if (tipo == "Positivas") {
    tipo <- T
  } else if (tipo == "Negativas") {
    tipo <- F
  } else {
    print("Variable `tipo` inválida. Opciones: `Positivas`, `Negativas`, `Todas`")
  }

  if (filtro == "Pasivo + Detractor") {
    filtro = c("Pasivo", "Detractor")
  }

  df <- df %>%
  {if(filtro_col %in% names(df) && all(filtro %in% levels(df[,filtro_col]))) filter(., !!sym(filtro_col) %in% filtro) else .}

  if(cruce == "Ninguno"){
    cruce <- NULL
  } else {
    niveles <- getLevels(df, cruce)
    nivelesT <- getLevelsWTotal(df, cruce)
  }

  trash <- c("Otro", "-", "", "No contesto", "NO SABE / NO CONTESTO", "NADA / NINGUNO")
  N <- nrow(df)
  # TODO: Sacar de la funcion:
  catm <- read_csv("datos/catalogoMenciones.csv",
                   col_names = c("negocio", "mencion", "clasificacion")) %>%
    mutate(clasificacion = as.logical(clasificacion))
  # cat <- cat %>% mutate(clasificacion = as.logical(clasificacion))

  menciones <- data.frame(table(unlist(df[,menciones_col]))) %>%
    dplyr::select(mencion = Var1, freq = Freq) %>%
    filter(!mencion %in% trash) %>%
    arrange(desc(freq)) %>%
    as_tibble(.)

  tmp <- stringdist_left_join(menciones,
                              catm %>% dplyr::select(mencion,clasificacion),
                              by = "mencion") %>%
    dplyr::select(mencion = mencion.x, clasificacion) %>%
    unique(.)

  tmp <- menciones %>%
    inner_join(tmp, by = "mencion") %>%
    mutate(cruce = "Total", pct = 100*freq/N) %>%
    dplyr::select(mencion, cruce, freq, pct, clasificacion)

  sf <- tmp %>%
    group_by(clasificacion) %>%
    arrange(desc(freq)) %>%
    filter(!is.na(clasificacion))
  # {if (!is.null(tipo)) filter(clasificacion) } %>%
  # {if (!is.null(tipo)) filter(clasificacion == tipo) } %>%
  # top_n(10, freq)

  if (!is.null(tipo)) {
    sf <- sf %>% filter(clasificacion == tipo)
  }
  sf <- sf %>% top_n(10, freq)

  # View(sf) #################################

  # menciones_col <- menciones
  # df<-raw

  if (!is.null(cruce)) {
    tot <- 0
    tdf <- NULL
    for (l in niveles) {
      dftemp <- df %>%
        filter(!!sym(cruce) == l)
      n <- nrow(dftemp)

      t <- data.frame(table(unlist(dftemp[,menciones_col]))) %>%
        dplyr::select(mencion = Var1, freq = Freq) %>%
        filter(!mencion %in% trash) %>%
        mutate(cruce = l) %>%
        mutate(pct = 100*freq/n) %>%
        arrange(desc(freq)) %>%
        as_tibble(.)

      tdf <- rbind(tdf, t)

      tot <- tot + n
      # print(n)
    }

    tmp2 <- stringdist_left_join(tdf,
                                 catm %>% dplyr::select(mencion,clasificacion),
                                 by = "mencion") %>%
      dplyr::select(mencion = mencion.x, clasificacion) %>%
      unique(.)

    tmp2 <- tdf %>%
      inner_join(tmp2, by = "mencion") %>%
      dplyr::select(mencion, cruce, freq, pct, clasificacion)

    cf <- tmp2 %>%
      group_by(cruce, clasificacion) %>%
      arrange(desc(freq)) %>%
      filter(!is.na(clasificacion)) %>%
      top_n(10, freq) %>%
      semi_join(sf, by = "mencion")

    # View(cf)
    # levels(cf$cruce)
    sf <- sf %>% rbind(cf) %>% ungroup(clasificacion, cruce)
    sf$cruce <- factor(sf$cruce, levels = nivelesT)

    # print(levels(sf$cruce)) ##################
  }
  sf = sf %>% ungroup(clasificacion)

  if(!is.null(cruce)){

    cruce_original = levels(sf$cruce)
    cruce_limpio = make.names(cruce_original, unique = TRUE, allow_ = FALSE)

    cruce_catalogo = data.frame(original = cruce_original, limpio = cruce_limpio) %>%
      mutate_at(vars(original,limpio),
                funs(factor(., levels = unique(.))))

    tmp = sf %>% inner_join(cruce_catalogo, by = c("cruce" = "original"))

    pct = tmp %>% reshape2::dcast(list("mencion" ,"limpio"), value.var = "pct") %>% arrange(desc(Total)) %>% mutate_if(is.numeric, function(x) ifelse(is.na(x), "", paste0(round(x,2),"%" )))
    freq = tmp %>% reshape2::dcast(list("mencion" ,"limpio"), value.var = "freq") %>% arrange(desc(Total)) %>% mutate_if(is.numeric, function(x) ifelse(is.na(x), "", x))
    temp = pct %>% inner_join(freq, by = "mencion")
    longitud = length(names(temp))
    columnas = names(pct)[-1]
    columnas_x = names(temp)[-1][1:(longitud/2)]
    columnas_y = names(temp)[-1][((longitud/2)+1):longitud]
    u = temp %>% dplyr::select(mencion)
    for(i in 1:(longitud/2)){
      v = temp %>% unite_(columnas[i], c(columnas_x[i], columnas_y[i]), sep = "<br>Casos: ") %>% mutate_all(funs(aux)) %>% dplyr::select(!!sym(columnas[i]))
      u = cbind(u, v)
    }
    colnames(u) <- getLevelsTable(df, cruce)
    # View(u) #<- data.frame(u, col.names = c("Mención", cruce_original), check.names = F)

  } else{
    u = sf %>% dplyr::select(-cruce,-clasificacion) %>% mutate(Mención = mencion, Casos = freq, Porcentaje = round(pct,1)) %>% dplyr::select(-mencion,-pct,-freq)
  }
  return(u)
}

library(tidyverse)
library(fuzzyjoin)
# df <- readRDS('datos/IPN_4Q_completa_050219.rds') %>% filter(NICK.USUARIO != "callcenter")
df <- readRDS('datos/IPN_4Q_completa_050219.rds') %>% filter(EKT == "1")
df_old <- readRDS('datos/base.rds')
menciones_col <- juicioFinal::nombresR(df, "P56")
menciones_col_old <- juicioFinal::nombresR(df_old, "P56")
# tipo <- "Positivas"
tipo <- "Negativas"
cruce <- "Ninguno"
filtro <- "Ninguno"
filtro_col <- ""
filtro_col_old <- ""
q <- 4

menciones_comp <- function(df, df_old,
                           menciones_col, menciones_col_old,
                           q,
                           tipo = "Todas",
                           cruce = "Ninguno",
                           filtro = "Ninguno",
                           filtro_col = "", filtro_col_old = filtro_col) {

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

  if (cruce == "Ninguno"){
    res <- makeMenciones(df,
                         menciones_col,
                         tipo,
                         cruce,
                         filtro,
                         filtro_col)
    colnames(res) <- nombres[1:3]
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
      dplyr::mutate(diff = Porcentaje.x - Porcentaje.y) %>%
      dplyr::mutate_at(function(x) ifelse(is.na(x), "", x),
                       .vars = c("Porcentaje.y", "diff")) %>%
      dplyr::rowwise(.) %>%
      dplyr::mutate(asdf = arrow(diff))
    colnames(res) <- c(nombres,"")
    return(res)

  }
}
