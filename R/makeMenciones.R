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

  trash <- c("Otro", "-", "", "No contesto", "NO SABE / NO CONTESTO", "NADA / NINGUNO", " ")
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
    as_tibble(.) %>%
    mutate(mencion = as.character(mencion))

  tmp <- stringdist_left_join(menciones,
                              catm %>% dplyr::select(mencion,clasificacion),
                              by = "mencion") %>%
    dplyr::select(mencion = mencion.x, clasificacion) %>%
    unique(.)

  tmp <- menciones %>%
    inner_join(tmp, by = "mencion") %>%
    mutate(cruce = "Total", pct = 100*freq/N) %>%
    dplyr::select(mencion, cruce, freq, pct, clasificacion)

  # sf <- tmp %>%
  #   group_by(clasificacion) %>%
  #   arrange(desc(freq)) %>%
  #   filter(!is.na(clasificacion))

  #Test
  sf <- tmp %>%
    group_by(clasificacion) %>%
    arrange(desc(freq))

  # {if (!is.null(tipo)) filter(clasificacion) } %>%
  # {if (!is.null(tipo)) filter(clasificacion == tipo) } %>%
  # top_n(10, freq)

  if (!is.null(tipo)) {
    sf <- sf %>% filter(clasificacion == tipo)
  }
  # sf <- sf %>% top_n(10, freq)

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

    # cf <- tmp2 %>%
    #   group_by(cruce, clasificacion) %>%
    #   arrange(desc(freq)) %>%
    #   filter(!is.na(clasificacion)) %>%
    #   top_n(10, freq) %>%
    #   semi_join(sf, by = "mencion")

    #Test
    cf <- tmp2 %>%
      group_by(cruce, clasificacion) %>%
      arrange(desc(freq)) %>%
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
      mutate_at(vars(original,limpio), funs(factor(., levels = unique(.))))

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
    #colnames(u) <- getLevelsTable(df, cruce)
    # colnames(u) <- ifelse(length(u)!=length(getLevelsTable(df, cruce)),c('Mención',gsub("\\."," ",columnas)),
    #                       getLevelsTable(df, cruce))
    ifelse(length(u)!=length(getLevelsTable(df, cruce)),colnames(u)<-c('Mención',gsub("\\."," ",columnas)),colnames(u)<-getLevelsTable(df, cruce))
    #colnames(u) <- nombrestabla

    # View(u) #<- data.frame(u, col.names = c("Mención", cruce_original), check.names = F)

  } else{
    u = sf %>% dplyr::select(-cruce,-clasificacion) %>% mutate(Mención = mencion, Casos = freq, Porcentaje = round(pct,1)) %>% dplyr::select(-mencion,-pct,-freq)
  }
  return(u)
}
