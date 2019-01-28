#' Función auxiliar para generación de la gráfica de barras
#'
#' Descripción pendiente
#' @param df
#' @param label_var
#' @param level_var
#' @param filtros
#'
#' @return Descripción pendiente
#' @export
#'
#' @examples
prepros <- function(df, label_var, level_var, filtros = NULL) {

  lab <- sym(label_var)
  lev <- sym(level_var)

  local_df <- df %>%
    filter(!is.na(!!lab)) %>%
    group_by(!!lab, !!lev) %>%
    summarise(freq = n()) %>%
    reshape2::dcast(list(eval(level_var), label_var),
                    value.var = "freq",
                    fun.aggregate = sum) %>%
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) %>%
    mutate(cTotal = rowSums(.[-1]))

  local_df[,level_var] <- factor(local_df[,level_var],
                                 levels = rev(unique(local_df[,level_var])))

  local_df2 <- local_df %>%
    mutate_at(vars("Promotor","Pasivo","Detractor"), funs(.*100/cTotal)) %>%
    mutate_at(vars("cTotal"), funs(.*100/sum(cTotal/2)))

  local_df2 <-local_df %>%
    select(-!!lev) %>%
    rename(fDetractor=Detractor,
           fPasivo = Pasivo,
           fPromotor = Promotor,
           fTotal = cTotal) %>%
    cbind(local_df2, .)

  return(local_df2)
  # return(names(local_df))
}
