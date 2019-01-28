#' Data prep para grafica grid
#'
#' @param data
#' @param ipn
#' @param prob
#'
#' @return
#' @export
#'
#' @examples
prep_pct <- function(data, ipn, prob) {

  df <- prep(data, ipn, prob)

  df_pct <-reshape2::dcast(df, grupo2 ~ grupo1,
                           value.var = "pct") %>%
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) %>%
    mutate(cTotal = rowSums(.[-1]))

  return(df_pct)
}
