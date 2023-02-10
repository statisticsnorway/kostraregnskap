
  
#' `id_bidrag` med multippel `ind`-parameter og multippel `sign`-parameter 
#' 
#' \code{\link{id_bidrag}} med multippel `ind`-parameter i matrise 
#' og multippel `sign`-parameter i matrise slik at output er en matrise.
#' Også mulig med \code{\link{fun_belop}} som alternativ input-funksjon slik at output blir 
#' resultatbeløp som en numerisk matrise. 
#' Beregningene går via \code{\link{id_bidrag_vector}}.
#'
#' @param sign_matrix Matrise der ikke-0 i hver kolonne gir opphav til `sign`- 
#'      og `ind`-input til `id_bidrag_vector`.   
#'      Det er fordelaktig om denne matrisa er av sparse type siden den kommer til å bli konvertert til en slik. 
#' @param ind_matrix Input til `id_bidrag_vector`.  
#'      Denne matrisa kan være sparse, men den vil internt bli konvertert til en vanlig matrise. 
#' @inheritParams id_bidrag_vector
#'
#' @return En vanlig matrise
#' @export
#' @importFrom SSBtools dummy_aggregate
#' @keywords internal
#'
#' @examples
#' # Genererer input
#' 
#' df <- data.frame(id = paste0("id", 1:9), belop = 11 * 1:9)
#' df
#' 
#' ind_matrix <- matrix(0, 7, 5)
#' ind_matrix[2:4, 1] <- c(8, 5, 7)
#' ind_matrix[5:7, 2] <- c(8, 5, 7)
#' ind_matrix[, 3] <- 1:7
#' ind_matrix[c(1, 7), 5] <- 9
#' colnames(ind_matrix) <- paste0("im", 1:5)
#' ind_matrix
#' 
#' sign_matrix <- matrix(0, 7, 6)
#' sign_matrix[2:4, 1] <- c(1, -1, 2)
#' sign_matrix[5:7, 2] <- c(1, -1, 2)
#' sign_matrix[c(1, 7), 3] <- 1:2
#' sign_matrix[7, 4] <- -1
#' sign_matrix[, 6] <- rep(1, 7)
#' colnames(sign_matrix) <- paste0("sm", 1:6)
#' sign_matrix
#' 
#' # Sparse versjon av sign_matrix
#' sign_Matrix <- Matrix::Matrix(sign_matrix)
#' sign_Matrix
#' 
#' # Output med fun_belop og id_bidrag (denne printes ikke)
#' id_bidrag_matrix(sign_matrix, ind_matrix, df, fun_id_bidrag = fun_belop)
#' out <- id_bidrag_matrix(sign_matrix, ind_matrix, df)
#' 
#' # Med mindre matriser som input (en av dem er sparse)
#' id_bidrag_matrix(sign_matrix[2:3, c(1, 6)], ind_matrix[4:5, c(1, 3)], df, fun_belop)
#' id_bidrag_matrix(sign_Matrix[2:3, c(1, 6)], ind_matrix[4:5, c(1, 3)], df)
#' 
id_bidrag_matrix <- function(sign_matrix, ind_matrix, data, fun_id_bidrag = id_bidrag) {
  ind_matrix <- as.matrix(ind_matrix)
  df_seq_len <- data.frame(ind = seq_len(nrow(ind_matrix)))
  a <- as.matrix(dummy_aggregate(data = df_seq_len, x = sign_matrix, 
          vars = c(ibv = "ind"), 
          fun = c(ibv = function(..., fun_vector_data, fun_id_bidrag)
                            id_bidrag_vector(..., data = fun_vector_data, 
          fun_id_bidrag = fun_id_bidrag)),
          ind_matrix = as.matrix(ind_matrix), fun_vector_data = data, 
          fun_id_bidrag = fun_id_bidrag, dummy = FALSE, dots2dots = TRUE, 
          forward_dots = TRUE, do_unmatrix = FALSE, keep_names = FALSE)[[1]])
  colnames(a) <- colnames(ind_matrix)
  rownames(a) <- colnames(sign_matrix)
  a
}












