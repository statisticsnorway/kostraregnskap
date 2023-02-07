
#' `UUIDgenerate` med `use.time = TRUE`
#' 
#' \code{\link{UUIDgenerate}} fra pakken `uuid` med `use.time = TRUE`
#' 
#' @param n number of UUIDs to generate
#'
#' @return id-er
#' @export
#' @importFrom uuid UUIDgenerate
#'
#' @examples
#' uuid_generate_time(2)
uuid_generate_time = function(n){
  UUIDgenerate(use.time = TRUE, n = n)
}

