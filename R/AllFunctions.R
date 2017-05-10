#' \code{CMdictionary}
#'
#' @description This function builds a dictionary for Conceptmapper.
#' @param outputdir the directory where the XML conceptmapper dictionary will be stored. Defaults to the tmp system's directory
#' @return An object of type CMdictionary that can be used to annotate text with the \code{EntityFinder}.
#' @examples
#' \dontrun{
#'
#' ##This might take some time to download the dictionary
#' dict_file <-  file.path(getwd(), 'BrendaTissue.obo')
#'  if(!file.exists(dict_file))
#'    download.file('https://sourceforge.net/projects/onassis/files/BrendaTissue.obo',
#'     destfile=file.path(getwd(), 'BrendaTissue.obo'))
#' dict <- CMdictionary(inputFile=file.path(getwd(), 'BrendaTissue.obo'
#' ), outputdir=getwd(), synonymType='ALL')
#' }
#'
#' @name CMdictionary
#' @rdname CMdictionary-class
#' @export
#' @importFrom methods new validObject

CMdictionary <- function(inputFile = NA_character_, dictType = "OBO", outputdir = getwd(), synonymType = "ALL", taxID = 0) {
    dictionary <- new("CMdictionary")
    cmdictionary <- buildDictionary(dictionary, inputFile = inputFile, outputDir = outputdir, dictType = dictType, synonymType = synonymType,
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
#' dict_file <-  file.path(getwd(), 'BrendaTissue.obo')
#'  if(!file.exists(dict_file))
#'    download.file('https://sourceforge.net/projects/onassis/files/BrendaTissue.obo',
#'     destfile=file.path(getwd(), 'BrendaTissue.obo'))
#' ontology_file <- file.path(getwd(), 'BrendaTissue.obo')
#' dict <- CMdictionary(inputFile=ontology_file, outputdir=getwd(), synonymType='ALL')
#' dictionaryInfo(dict)
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
#' @param index the paramValueIndex of the options
#' @return list of possible options that one cas net to run the entity finder
#' @description This function shows the list of possible combinations of options to run the entity finder
#' @examples
#' op <- CMoptions(40)
#' @export

CMoptions <- function(index = NA) {
    opts <- new("CMoptions")
    paramValueIndex(opts) <- index
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
#' dict_file <-  file.path(getwd(), 'cmDict-BrendaTissue.xml')
#'  if(!file.exists(dict_file))
#'    download.file('https://sourceforge.net/projects/onassis/files/cmDict-BrendaTissue.xml',
#'     destfile=file.path(getwd(), 'cmDict-BrendaTissue.xml'))
#' brenda_dict <- CMdictionary(inputFile=file.path(getwd(),
#' 'cmDict-BrendaTissue.xml'), dictType = 'CMDICT')
#' myopts <- CMoptions(40)
#' tissue_entities <- annotate(system.file('extdata',
#''testsamples', package='Onassis'), options = myopts,
#'  dictionary=brenda_dict)
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
                dict <- buildDictionary(dict, inputFile = dictionary)
            } else {
                if (file_ext(dictionary) == "xml") {
                  dict <- new("CMdictionary")
                  dict <- buildDictionary(dict, dictType = "CMDICT", inputFile = dictionary)
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
#' dict_file <-  file.path(getwd(), 'BrendaTissue.obo')
#'  if(!file.exists(dict_file))
#'    download.file('https://sourceforge.net/projects/onassis/files/BrendaTissue.obo',
#'     destfile=file.path(getwd(), 'BrendaTissue.obo'))
#' ontology_file <- file.path(getwd(), 'BrendaTissue.obo')
#' term_list1 <- c('http://purl.obolibrary.org/obo/BTO_0001546',
#' 'http://purl.obolibrary.org/obo/BTO_0000664')
#' term_list2 <- c('http://purl.obolibrary.org/obo/BTO_0000759',
#' 'http://purl.obolibrary.org/obo/BTO_0000759')
#' sim <- similarity(ontology_file, term_list1, term_list2)
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





