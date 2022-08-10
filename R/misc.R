
#' not in
#' 
#' return true if x not in y
#' 
#' @param x vector or NULL: the values to be matched
#' @param y vector or NULL: the values to be matched against
#' @return if elements of \code{x} are in \code{y}
#' @usage x \%nin\% y
#' @examples
#' letters[1:10] %!in% letters[1:3] # 30% of the second arg ar in the first
#' @export
#' @rdname nin
"%nin%" <- function(x, y) {
    !(x %in% y)
}