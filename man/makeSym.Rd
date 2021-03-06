% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeSym.R
\name{makeSym}
\alias{makeSym}
\title{Convierte string a simbolo}
\usage{
makeSym(c)
}
\arguments{
\item{c}{String a convertir}
}
\value{
Simbolo de R
}
\description{
Esta función verifica que el nombre de columna almacenado en la varible `c`
sea tipo "character" o tipo "symbol" y la transforma a tipo "symbol" en caso
negativo.
Convierto el `string` contenido en `c` a un objeto de tipo `symbol`
para luego poder usarlo dentro de las evaluaciones no estándares de dplyr:
escribir `!!n` = `!!sym(c)` saca el texto *contenido* en `c`
y se lo da a dplyr de manera que lo pueda utilizar (recordemos que dplyr)
requiere de el texto sin comillas, por lo que tenemos que hacer esto para
ingresar variables en sus métodos.
}
\examples{
makeSym("abc")
}
