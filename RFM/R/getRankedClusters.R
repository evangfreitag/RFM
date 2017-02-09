#' getRankedClusters
#'
#' This function ranks the results of clusterings.
#' @param data a dataset with columns named Date, Recency, Frequency, and Monetary.
#' @param listIDs a list of vectors of User IDs corresponding to their respective clusters.
#' @param colname1 .
#' @param colname2 .
#' @param f a function. Either the maximum or minimum is used (depending on which variable is being ranked).
#' @keywords Ranking, RFM, kmeans++
#' @export
#' @examples
#' getRankedClusters()

getRankedClusters <- function(data,listIDs,colname1,colname2,f){
	mu1 <- 0
	for(i in 1:length(listIDs)){mu1[i] <- mean(data[listIDs[[i]],colname1])}
	for(i in 1:length(mu1)){
		if(i < length(mu1)){
			data[listIDs[[which(mu1 == getRank(mu1,i,f))]],colname2] <- ((length(mu1) - i)+1)
		}
		else{data[listIDs[[which(mu1 == getRank(mu1,i,f))]],colname2] <- 1}
	}
	return(data)
}