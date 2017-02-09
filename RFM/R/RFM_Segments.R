#' RFM_Segments
#'
#' This function computes the RFM_Segmentation for a dataset with columns named Date, Recency, Frequency, and Monetary.
#' @param data a dataset with columns named Date, Recency, Frequency, and Monetary.
#' @param N_Recency_Clusters an integer indicating the number of Recency Clusters to be computed.
#' @param N_Frequency_Clusters an integer indicating the number of Frequency Clusters to be computed.
#' @param N_Monetary_Clusters an integer indicating the number of Monetary Clusters to be computed.
#' @keywords RFM, kmeans++
#' @export
#' @examples
#' RFM_Segments()

RFM_Segments <- function(data,N_Recency_Clusters,N_Frequency_Clusters,N_Monetary_Clusters){
	if (!requireNamespace("LICORS", quietly = TRUE)) {
		stop("Please install the LICORS Package.",call. = FALSE)
	}

	data$Ranked_Recency_Cluster <- 0
	data$Ranked_Frequency_Cluster <- 0
	data$Ranked_Monetary_Cluster <- 0
	data$RF_Cluster <- 0
	data$RFM_Cluster <- 0

	km1 <- kmeanspp(data$Recency, N_Recency_Clusters)
	ByRCL <- split(data$User_ID, km1$cluster)
	data <- getRankedClusters(data,ByRCL,"Recency","Ranked_Recency_Cluster",min)
	ByR <- split(data$User_ID, data$Ranked_Recency_Cluster)

	for(j in 1:length(ByR)){
		km2 <- kmeanspp(data[ByR[[j]],"Frequency"],N_Frequency_Clusters)
		ByRF <- split(data[ByR[[j]],"User_ID"], km2$cluster)
		data[ByR[[j]],] <- getRankedClusters(data[ByR[[j]],],ByRF,"Frequency","Ranked_Frequency_Cluster",max)
		data[ByR[[j]],"RF_Cluster"] <- paste(data[ByR[[j]],"Ranked_Recency_Cluster"],data[ByR[[j]],"Ranked_Frequency_Cluster"],sep="-")

		for(k in 1:length(ByRF)){
			km3 <- kmeanspp(data[ByRF[[k]],"Monetary"],N_Monetary_Clusters)
			ByRFM <- split(data[ByRF[[k]],"User_ID"], km3$cluster)
			data[ByRF[[k]],] <- getRankedClusters(data[ByRF[[k]],],ByRFM,"Monetary","Ranked_Monetary_Cluster",max)
		}

	}
	data$RFM_Cluster <- paste(data$RF_Cluster,data$Ranked_Monetary_Cluster,sep="-")
	data$RF_Cluster <- NULL
	return(data)
}