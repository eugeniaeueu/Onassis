
#' \code{findHealthy}
#'
#' @rdname findHealthy
#' @name findHealthy
#' @param metadata_df a data frame where the first column corresponds to the identifier of a sample #' and the other columns the metadata relative to the sample
#' @description findHealthy annotates as 'Healthy' the samples whose metadata matches with one of the elements of a list of sentence used to describe the normal disease state or healthy.
#' @return a data frame with healthy samples annotated as 'Healthy'
#' @examples
#' metadatafile <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#'
#' healthy_gsms <- findHealthy(metadatafile)
#' @export

findHealthy <- function(metadata_df) {
    markers <- "disease state: normal|tissuetype: normal|no ad present|healthy|disease: healthy|disease: normal|disease: presumed normal|disease: none|disease: null|disease: na|disease status: normal|tumor: none"
    gsm_list <- c()
    metadata_df[, 1] <- as.character(as.vector(metadata_df[, 1]))
    for (i in 2:ncol(metadata_df)) {
        # metadata_df[,i] <- as.character(as.vector(gsub('ξ', '', metadata_df[,i])))
        gsms <- metadata_df[, 1][grep(markers, tolower(metadata_df[, i]))]
        if (length(gsms) > 0) 
            gsm_list <- c(gsm_list, gsms)
    }
    return(unique(gsm_list))
}


#' \code{filterTerms}
#'
#' @rdname filterTerms
#' @name filterTerms
#' @param annotated_df a data frame resulting from the annotation process of Onassis
#' @param termlist the list of terms to be filtered out from the annotations
#' @description filterTerms allows users to remove a set of defined terms from the results of the annotation provided by Onassis
#' @return a data frame with removed annotations
#' @examples
#' metadatafile <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#' healthy_gsms <- findHealthy(metadatafile)
#' @export

filterTerms <- function(annotated_df, termlist = c()) {
    if (length(termlist) > 0) {
        annotated_df <- annotated_df[which(!tolower(annotated_df$term_name) %in% 
            tolower(termlist)), ]
    }
    return(annotated_df)
}



