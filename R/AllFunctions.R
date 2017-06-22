#' \code{CMdictionary}
#'
#' @description This function builds a dictionary for Conceptmapper.
#' @param outputdir the directory where the XML conceptmapper dictionary will be stored. Defaults to the tmp system's directory
#' @return An object of type CMdictionary that can be used to annotate text with the \code{EntityFinder}.
#' @examples
#'
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- CMdictionary(inputFileOrDb=obo, outputdir=getwd(), synonymType='ALL')
#'
#' @name CMdictionary
#' @rdname CMdictionary-class
#' @export
#' @importFrom methods new validObject
#' @importFrom AnnotationDbi toTable

CMdictionary <- function(inputFileOrDb = NULL, dictType = "OBO", outputdir = getwd(), synonymType = "ALL", taxID = 0) {
    dictionary <- new("CMdictionary")
    cmdictionary <- buildDictionary(dictionary, inputFileOrDb =  inputFileOrDb, outputDir = outputdir, dictType = dictType, synonymType = synonymType,
        taxID = taxID)
    return(cmdictionary)
}


#' \code{dictTypes}
#'
#' @rdname dictionaryTypes
#' @return the list of dictionary types available
#' @description This function shows a lis of the pre-defined conceptmapper dictionary types
#' @examples
#' types <- dictionaryTypes()
#' @export dictionaryTypes
#' @export
dictionaryTypes <- function() {
    dictionary <- new("CMdictionary")
    types <- dictTypes(dictionary)
    return(types)
}


#' \code{dictionaryInfo}
#'
#' @rdname dictionaryInfo
#' @param CMdictionary CMdictionary object of the conceptmapper dictionary
#' @return list of details about the dictionary
#' @description This function shows the list of details of the conceptmapper dictionary
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- CMdictionary(inputFileOrDb=obo, outputdir=getwd(), synonymType='ALL')
#' dictionaryInfo(sample_dict)
#' @exportMethod dictInfo
#' @export
#'
dictionaryInfo <- function(CMdictionary) {
    if (!class(CMdictionary) == "CMdictionary")
        message("Invalid parameter tuype: Please provide a valid conceptmapper dictionary of type CMdictionary")
    info <- dictInfo(CMdictionary)
    return(info)
}


#' \code{showCMoptions}
#'
#' @rdname showCMoptions
#' @return list of possible options that one cas net to run the entity finder
#' @description This function shows the list of possible combinations of options to run the entity finder
#' @examples
#' showCMoptions()
#' @export
#'
showCMoptions <- function() {
    opts <- new("CMoptions")
    return(listCombinations(opts))
}



#' \code{CMoptions}
#'
#' @name CMoptions
#' @rdname CMoptions-class
#' @param options the list of parameters to set for conceptmapper
#' @return list of possible options that one has to run the entity finder
#' @description This function shows the list of possible combinations of options to run the entity finder
#' @examples
#' op <- CMoptions(c('CONTIGUOUS_MATCH', 'CASE_IGNORE',
#'  'BIOLEMMATIZER', 'NONE', 'OFF', 'YES', 'EXACT_ONLY'))
#' @export

CMoptions <- function(options = NA) {
    opts <- new("CMoptions")
    if(!is.na(options))
      CMargs(opts) <- as.list(options)
    else
      paramValueIndex(opts) <- 32
    return(opts)
}


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
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- CMdictionary(inputFileOrDb=obo, outputdir=getwd(), synonymType='ALL')
#' myopts <- new('CMoptions')
#' paramValueIndex(myopts) <- 40
#' entities <- annotate(system.file('extdata', 'vignette_data',
#'  'GEO_human_chip.rds', package='Onassis'), options = myopts,
#'  dictionary=sample_dict)
#'
#' @export
#'
annotate <- function(inputFileorDf, dictionary, options = NA, outDir = tempdir(), multipleDocs = FALSE) {
    ef <- new("EntityFinder")

    ## Creating the options object
    opts <- options


    if (class(options) == "CMoptions") {
        opts <- options
    } else {
        if (is.integer(options)) {
            opts <- new("CMoptions")
            paramValueIndex(opts) <- options
        } else {
            # setting to default values
            opts <- new("CMoptions")
        }
    }
    ## end else options
    results <- NA
    # Creating the dictionary object
    dict <- dictionary
    if (class(dictionary) == "CMdictionary") {
        if (is.jnull(dict@dictRef))
            stop("Invalid dictionary. Please provide a valid CMdictionary or create it from OBO ontologies")
    } else {
        if (file.exists(dictionary)) {
            if (file_ext(dictionary) != "xml") {
                dict <- new("CMdictionary")
                dict <- buildDictionary(dict, inputFileOrDb = dictionary)
            } else {
                if (file_ext(dictionary) == "xml") {
                  dict <- new("CMdictionary")
                  dict <- buildDictionary(dict, dictType = "CMDICT", inputFileOrDb =  dictionary)
                }
            }
        } else {
            stop("Invalid dictionary. Please build a correct dictionary")
        }  ##end else dictionary
    }
    if (class(inputFileorDf) == "data.frame")
        results <- annotateDF(ef, inputFileorDf, opts, dict, outDir = outDir) else results <- findEntities(ef, inputDirOrFile = inputFileorDf, multipleDocs = FALSE, outDir = outDir, configOpt = opts, cmDict = dict)
    return(results)


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
    sim <- new("Similarity")
    return(showOpts(sim))
}





#' \code{Similarity}
#'
#' @rdname Similarity-class
#' @return the similarity value based on the configured measures
#' @description this function computes the similarity between couple of terms, couple of samples, or group of terms
#' @param ontologyFile the file in OBO or RDF format to build the graph
#' @param termlist1 The URI or character vector of URIs belonging to the first set or the sample id of an annotated data frame
#' @param termlist2 The URI or character vector of URIs belonging to the second  set or the sample id of the annotated data frame
#' @param annotatedtab The data frame of
#' @param pairConf a configuration for the pairwise meausres. Defaults to resnik and seco.
#' @param groupConf one of the allowed configurations for groupwise measures
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- CMdictionary(inputFileOrDb=obo, outputdir=getwd(), synonymType='ALL')
#' myopts <- new('CMoptions')
#' paramValueIndex(myopts) <- 40
#' term_list1 <- c('http://purl.obolibrary.org/obo/CL_0000000', 'http://purl.obolibrary.org/obo/CL_0000236')
#' term_list2 <- c('http://purl.obolibrary.org/obo/CL_0000542')
#' sim <- similarity(obo, term_list1, term_list2)
#' @export
#'
similarity <- function(ontologyFile, termlist1, termlist2, annotatedtab = NA, pairConf = c("resnik", "seco"), groupConf = "ui") {

    sim <- new("Similarity")
    ontology(sim) <- ontologyFile

    pairwiseConfig(sim) <- pairConf
    groupwiseConfig(sim) <- groupConf

    similarity_value <- 0
    if (!class(annotatedtab) == "data.frame") {

        if (length(termlist1) == 1 & length(termlist2) == 1) {
            similarity_value <- sim(sim, as.character(termlist1), as.character(termlist2))
        } else {
            if (length(termlist1) > 1 | length(termlist2) > 1) {
                similarity_value <- groupsim(sim, as.character(termlist1), as.character(termlist2))
            }
        }
    } else {
        similarity_value <- samplesim(sim, as.character(termlist1), as.character(termlist2), annotatedtab)
    }
    return(similarity_value)
}





