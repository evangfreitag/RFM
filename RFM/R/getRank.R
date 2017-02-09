#' getRank
#'
#' This function computes finds the ranking for a single cluster.
#' @param x Either Recency, Frequency, or Monetary.
#' @param depth an integer. The rank (i.e. given the rank, the corresponding cluster is returned).
#' @param f a function. Either the maximum or minimum.
#' @keywords Rank, RFM, kmeans++
#' @export
#' @examples
#' getRank()

getRank <- function(x,depth,f){
	if(depth > 1){
	for(i in 1:(depth-1)){
		w1 <- which(x == f(x))
		x <- x[-w1]
		}
	}
	return(f(x))
}