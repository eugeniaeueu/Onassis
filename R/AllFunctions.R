

#' \code{annotate}
#'
#' @rdname annotate
#' @return dataframe of annotations
#' @description this function runs the entityfinder on given input
#' @param inputFileorDf the file, directory, or data frame to annotate
#' @param dictionary the Conceptmapper dictionary file
#' @param options the options to run the finder
#' @param outDir the directory where to store the output files from onassis
#' @param multipleDocs TRUE if the input file is one but contains multiple documents
#' @importFrom rJava is.jnull
#' @importFrom tools file_ext
#' @importFrom methods is
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- dictionary(inputFileOrDb=obo, outputdir=getwd(), synonymType='ALL')
#' myopts <- CMoptions()
#' paramValueIndex(myopts) <- 40
#' entities <- Onassis::annotate(readRDS(system.file('extdata', 'vignette_data',
#'  'GEO_human_chip.rds', package='Onassis')), options = myopts,
#'  dictionary=sample_dict)
#'
#' @export
#'
annotate <- function(inputFileorDf, dictionary, options = NA,
    outDir = tempdir(), multipleDocs = FALSE) {
    ef <- EntityFinder()

    ## Creating the options object
    opts <- options


    if (is(options, "CMoptions")) {
        opts <- options
    } else {
        if (is.integer(options)) {
            opts <- CMoptions()
            paramValueIndex(opts) <- options
        } else {
            # setting to default values
            opts <- CMoptions()
        }
    }
    ## end else options
    results <- NA
    # Creating the dictionary object
    dict <- dictionary
    if (is(dict, "CMdictionary")) {
        if (is.jnull(dict@dictRef))
            stop("Invalid dictionary. Please provide a valid CMdictionary or create it from OBO ontologies")
    } else {
        if (file.exists(dictionary)) {
            if (file_ext(dictionary) != "xml") {
                dict <- CMdictionary()
                dict <- buildDictionary(dict, inputFileOrDb = dictionary)
            } else {
                if (file_ext(dictionary) == "xml") {
                  dict <- CMdictionary()
                  dict <- buildDictionary(dict, dictType = "CMDICT",
                    inputFileOrDb = dictionary)
                }
            }
        } else {
            stop("Invalid dictionary. Please build a correct dictionary")
        }  ##end else dictionary
    }
    if (is.data.frame(inputFileorDf))
        results <- annotateDF(ef, inputFileorDf, opts,
            dict, outDir = outDir) else results <- findEntities(ef, inputDirOrFile = inputFileorDf,
        multipleDocs = FALSE, outDir = outDir, configOpt = opts,
        cmDict = dict)
    return(results)


}





#' \code{Similarity}
#' @name similarity
#' @rdname Similarity-class
#' @return the similarity value based on the configured measures
#' @description this function computes the similarity between couple of terms, couple of samples, or group of terms
#' @param ontologyFile the file in OBO or RDF format to build the graph
#' @param termlist1 The URI or character vector of URIs belonging to the first set or the sample id of an annotated data frame
#' @param termlist2 The URI or character vector of URIs belonging to the second  set or the sample id of the annotated data frame
#' @param annotatedtab The data frame of annotations
#' @param pairConf a configuration for the pairwise meausres. Defaults to resnik and seco.
#' @param groupConf one of the allowed configurations for groupwise measures
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- dictionary(inputFileOrDb=obo, outputdir=getwd(), synonymType='ALL')
#' myopts <- CMoptions()
#' paramValueIndex(myopts) <- 40
#' term_list1 <- c('http://purl.obolibrary.org/obo/CL_0000000',
#'  'http://purl.obolibrary.org/obo/CL_0000236')
#' term_list2 <- c('http://purl.obolibrary.org/obo/CL_0000542')
#' sim <- similarity(obo, term_list1, term_list2)
#'
#' @export
similarity <- function(ontologyFile, termlist1, termlist2,
    annotatedtab = NA, pairConf = c("resnik", "seco"),
    groupConf = "ui") {
    sim <- Similarity()
    ontology(sim) <- ontologyFile

    pairwiseConfig(sim) <- pairConf
    groupConfig(sim) <- groupConf

    similarity_value <- 0
    if (!is.data.frame(annotatedtab)) {
        if (length(termlist1) == 1 & length(termlist2) ==
            1) {
            similarity_value <- pairsim(sim, as.character(termlist1),
                as.character(termlist2))
        } else {
            if (length(termlist1) > 1 | length(termlist2) >
                1) {
                similarity_value <- groupsim(sim, as.character(as.vector(termlist1)),
                  as.character(as.vector(termlist2)))
            }
        }
    } else {
        similarity_value <- samplesim(sim, as.character(termlist1),
            as.character(termlist2), annotatedtab)
    }
    return(similarity_value)
}




#' \code{showSimilarities}
#'
#' @rdname Similarity-methods
#' @name showSimilarities
#' @description This function shows the list of pairwise, information content and groupwise measures to compute the semantic similarities
#' @return the list of options
#' @examples
#' measures <- showSimilarities()
#'
#' @export

showSimilarities <- function() {
    sim <- Similarity()
    return(showOpts(sim))
}


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

findHealthy <- function(metadata_df){
  markers <-"disease state: normal|tissuetype: normal|no ad present|healthy|disease: healthy|disease: normal|disease: presumed normal|disease: none|disease: null|disease: na|disease status: normal|tumor: none"
  gsm_list <- c()
  metadata_df[,1] <- as.character(as.vector(metadata_df[,1]))
  for(i in 2:ncol(metadata_df)){
  #  metadata_df[,i] <-   as.character(as.vector(gsub('ξ', '', metadata_df[,i])))
    gsms <- metadata_df[,1][grep(markers, tolower(metadata_df[,i]))]
    if(length(gsms)>0)
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

filterTerms <- function(annotated_df, termlist=c()){
  if(length(termlist)>0){
    annotated_df <- annotated_df[which(!tolower(annotated_df$term_name) %in% tolower(termlist)),]
  }
  return( annotated_df)
}



