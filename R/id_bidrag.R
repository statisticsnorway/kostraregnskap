

#' Tekststreng som angir bidrag til `sum(sign*belop)`
#' 
#' Mer presist: Bidrag til `sum(sign[ind > 0] * data[["belop"]][ind])`,
#' der `ind` er vektor med radnummer i data og 
#' der  `sign` har samme lengde som `ind`. 
#' 
#'
#' @param ind Radnummer i data   
#' @param sign Faktor data skal multipliseres med. Samme lengde som `ind`.
#' @param data En data frame
#' @param id   Variabel i data med id
#' @param belop Variabel i data med belop
#'
#' @return En tekststreng. Dvs.lengde-1-character-variabel.  
#' @export
#'
#' @examples
#' # Genererer input
#' df <- data.frame(id = paste0("id", 1:9), belop = 11 * 1:9)
#' df
#' ind <- c(8, 5, 7)
#' sign <- c(1, -1, 2)
#' 
#' # Den aktuelle summen er 187
#' sum(sign * df[["belop"]][ind])
#' 
#' # Bidrag til denne summen
#' id_bidrag(ind, sign, df)
#' 
#' # Sum med to første indekser
#' sum(c(1, -1) * df[["belop"]][c(8, 5)])
#' 
#' # Bidrag til denne summen, nå med '+' og '-'
#' id_bidrag(c(8, 5), c("+", "-"), df)
#' 
#' # 0-indekser som ignoreres kan være med
#' ind2 <- c(8, 0, 5, 0, 7)
#' sign2 <- c(1, -1, -1, -3, 2)
#' 
#' # Summen beregnes generelt slik
#' sum(sign2[ind2 > 0] * df[["belop"]][ind2])
#' 
#' # 0-indekser ignoreres av id_bidrag
#' id_bidrag(ind2, sign2, df)
id_bidrag <- function(ind, sign, data, id = "id", belop = "belop") {
  if (!sum(ind)) {
    return("")
  }
  paste0("[", paste0("{'UUID': '", data[["id"]][ind], "', 'sign': '", 
                     sign[ind > 0], "', 'belop': ", 
                     data[[belop]][ind], "}", collapse = ", "), "]")
}