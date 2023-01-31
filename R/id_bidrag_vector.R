
#' `id_bidrag` med multippel `ind`-parameter
#' 
#' \code{\link{id_bidrag}} med multippel `ind`-parameter i matrise slik at output er en vektor.
#' Samt en tilsvarende funksjon som gir resultatbeløp som en numerisk vektor. 
#'
#' @param sign Faktor data skal multipliseres med. Samme lengde som `ind` nedenfor
#' @param ind  Radnummer i `ind_matrix` som skal brukes    
#' @param ind_matrix Matrise slik at hver kolonne i `ind_matrix[ind, ]` kan brukes som `ind`-parameter til `id_bidrag` sammen med `sign`
#' @param data `data`-parameter til `id_bidrag`
#' @param fun_id_bidrag Funksjonen `id_bidrag`. Kan endres. 
#'
#' @return Vektor med multippel `id_bidrag`-output
#' @export
#'
#' @examples
#' # Genererer input
#' df <- data.frame(id = paste0("id", 1:9), belop = 11 * 1:9)
#' df
#' ind_matrix <- matrix(0, 7, 5)
#' ind_matrix[2:4, 1] <- c(8, 5, 7)
#' ind_matrix[5:7, 2] <- c(8, 5, 7)
#' ind_matrix[, 3] <- 1:7
#' ind_matrix[c(1, 7), 5] <- 9
#' ind_matrix
#' sign <- c(1, -1, 2)
#' 
#' # Her er 1. element som i 1. id_bidrag-eksempel
#' id_bidrag_vector(sign, 2:4, ind_matrix, df)
#' belop_vector(sign, 2:4, ind_matrix, df)
#' 
#' # Her er 2. element som i 1. id_bidrag-eksempel
#' id_bidrag_vector(sign, 5:7, ind_matrix, df)
#' belop_vector(sign, 5:7, ind_matrix, df)
#' 
#' id_bidrag_vector(1:2, c(1, 7), ind_matrix, df)
#' belop_vector(1:2, c(1, 7), ind_matrix, df)
#' 
#' # Også mulig med bare en rad i matrisa
#' id_bidrag_vector(-1, 7, ind_matrix, df)
#' belop_vector(-1, 7, ind_matrix, df)
#' 
#' # Faktisk også mulig med ingen rader
#' id_bidrag_vector(integer(0), integer(0), ind_matrix, df)
#' belop_vector(integer(0), integer(0), ind_matrix, df)
id_bidrag_vector <- function(sign, ind, ind_matrix, data, fun_id_bidrag = id_bidrag) {
  indm <- ind_matrix[ind, , drop = FALSE]
  apply(indm, 2, fun_id_bidrag, sign, data)
}


#' @rdname id_bidrag_vector
#' @export
belop_vector <- function(sign, ind, ind_matrix, data, fun_id_bidrag = function(ind, sign, data) sum(sign[ind > 0] * data[["belop"]][ind])) {
  indm <- ind_matrix[ind, , drop = FALSE]
  apply(indm, 2, fun_id_bidrag, sign, data)
}

  
  
