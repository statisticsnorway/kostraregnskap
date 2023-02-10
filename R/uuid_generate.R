
#' `UUIDgenerate` with `use.time` specified
#' 
#' \code{\link{UUIDgenerate}} from package `uuid` with `use.time` specified 
#' * **`uuid_generate`**  uses default `use.time`
#' * **`uuid_generate_time`**  uses `use.time = TRUE`
#' * **`uuid_generate_random`**  uses `use.time = FALSE`
#' 
#' @param n number of UUIDs to generate
#'
#' @return character vector
#' @export
#' @importFrom uuid UUIDgenerate
#'
#' @examples
#' uuid_generate_time(1)
#' uuid_generate_time(2)
#' uuid_generate_random(3)
uuid_generate = function(n){
  UUIDgenerate(n = n)
}

#' @rdname uuid_generate
#' @export
uuid_generate_time = function(n){
  UUIDgenerate(use.time = TRUE, n = n)
}

#' @rdname uuid_generate
#' @export
uuid_generate_random = function(n){
  UUIDgenerate(use.time = FALSE, n = n)
}