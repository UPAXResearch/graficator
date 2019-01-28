#' Convierte string a simbolo
#'
#' Esta función verifica que el nombre de columna almacenado en la varible `c`
#' sea tipo "character" o tipo "symbol" y la transforma a tipo "symbol" en caso
#' negativo.
#' Convierto el `string` contenido en `c` a un objeto de tipo `symbol`
#' para luego poder usarlo dentro de las evaluaciones no estándares de dplyr:
#' escribir `!!n` = `!!sym(c)` saca el texto *contenido* en `c`
#' y se lo da a dplyr de manera que lo pueda utilizar (recordemos que dplyr)
#' requiere de el texto sin comillas, por lo que tenemos que hacer esto para
#' ingresar variables en sus métodos.
#' @param c String a convertir
#'
#' @return Simbolo de R
#' @export
#'
#' @examples
#' makeSym("abc")

makeSym <- function(c) {
  # `c` debe tener longitud 1
  stopifnot(length(c)==1)
  # `c` debe ser tipo "character" o "symbol"
  stopifnot(typeof(c) %in% c("character", "symbol"))

  if (typeof(c) == "character") {
    n <- sym(c) # convirtiendo a símbolo
  } else {
    n <- c # dejando como símbolo
  }
  return(n)
}
