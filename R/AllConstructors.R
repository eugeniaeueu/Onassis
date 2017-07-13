#' \code{CMdictionary}
#'
#' @description This constructor instantiates a dictionary object for Conceptmapper.
#' @return An object of type CMdictionary that can be used to create a dictionary
#' @examples
#'
#' sample_dict <- CMdictionary()
#'
#' @name CMdictionary
#' @rdname CMdictionary-class
#' @param dict_location The path of the created dictionary file
#' @param dictRef Reference to the java object representing the dictionary
#' @param dictInfo Information about how dictionary has been created. It is a list with the following fields
#' @export
#' @importFrom methods new validObject
#' @importFrom AnnotationDbi toTable

CMdictionary <- function(dict_location = NA_character_,
    dictInfo = list(), dictRef = .jnull()) {
    d <- new("CMdictionary")
    return(d)
}

#' \code{CMoptions}
#'
#' @name CMoptions
#' @rdname CMoptions-class
#' @param arguments the list of parameters to set for conceptmapper
#' @return instance of the class CMoptions set to the default combination of values
#' @description This function shows the list of possible combinations of options to run the entity finder
#' @examples
#' op <- CMoptions()
#' @export

CMoptions <- function(arguments = list()) new("CMoptions")


#' \code{EntityFinder}
#'
#' @name EntityFinder
#' @rdname EntityFinder-class
#' @param typeSystemRef the reference to the ccp-nlp type system
#' @return instance of the class CMoptions set to the default combination of values
#' @description This function shows the list of possible combinations of options to run the entity finder
#' @examples
#' op <- CMoptions()
#' @export

EntityFinder <- function(typeSystemRef = .jnull()) new("EntityFinder")



#' \code{Similarity}
#'
#' @rdname Similarity-class
#' @name Similarity-constructor
#' @aliases Similarity-constructor
#' @return instance of the class Similarity to compute semantic similarities based on the configured measures
#' @param pairwiseConfig the pairwise configuration
#' @param icConfig the information content configuration
#' @param groupConfig the groupwise configuration
#' @description this constructr initializes the Similarity class to compute the similarity between couple of terms, couple of samples, or group of terms
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- dictionary(inputFileOrDb=obo, outputdir=getwd(), synonymType='ALL')
#' myopts <- new('CMoptions')
#' paramValueIndex(myopts) <- 40
#' term_list1 <- c('http://purl.obolibrary.org/obo/CL_0000000', 'http://purl.obolibrary.org/obo/CL_0000236')
#' term_list2 <- c('http://purl.obolibrary.org/obo/CL_0000542')
#' sim <- similarity(obo, term_list1, term_list2)
#'
#' @export
Similarity <- function(pairwiseConfig = NA_character_,
    icConfig = NA_character_, groupConfig = NA_character_) new("Similarity")


