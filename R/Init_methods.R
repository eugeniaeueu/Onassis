#' @importFrom utils download.file
#' @importFrom rJava .jinit
initJavaLibs <- function(libLoc, mem = "12G") {
    # Check if library directory is missing

    # Starting the java engine
    .jinit(force.init = TRUE)
    if (missing(libLoc)) {
        libLoc = system.file("extdata", "java", package = "OnassisJavaLibs")
    }

    path = Sys.glob(paste0(libLoc, "/*.jar"))
    available_local_files <- list.files(libLoc, full.names = FALSE,
        pattern = "\\.jar$")

    # Check if all the files are there
    if (!"conceptmapper-0.0.1-SNAPSHOT-jar-with-dependencies.jar" %in%
        available_local_files)
        stop("Conceptmapper jar not available")

    if (!"similarity-0.0.1-SNAPSHOT-jar-with-dependencies.jar" %in%
        available_local_files)
        stop("Similarity jar not available")


    if (length(path) > 0) {
        sapply(path, function(path_entry) rJava::.jaddClassPath(path_entry))
    }

    rJava::.jaddClassPath(dirname(path))
}




# Init method that is called when a new
# CMdictionary is created
setMethod("initialize", "CMdictionary", function(.Object) {
    .Object@dictInfo = list("Empty dictionary", "NA",
        "NA", "0")
    names(.Object@dictInfo) <- c("Dictionary_type",
        "Dictionary_source", "Synonym_type", "taxid")
    .Object@dict_location = tempdir()
    .Object@dictRef = .jnull("java/io/File")
    .Object
})






# This method initializes the Conceptmapper Options
# to the 31th parameter combination.
# SearchStrategy: CONTIGUOUS_MATCH CaseMatch:
# CASE_INSENSITIVE Stemmer: PORTER Stopwords: NONE
# OrderIndependentLookup: OFF FindAllMatches: NO
# SynonymType: EXACT_ONLY

setMethod("initialize", "CMoptions", function(.Object) {
    options_combinations <- readRDS(system.file("extdata",
        "Options_table.rds", package = "Onassis"))
    .Object@arguments <- options_combinations[32, ,
        drop = TRUE]
    .Object
})




setMethod("initialize", "EntityFinder", function(.Object) {

    # Creating a conceptmapper type system
    type_system_array_list <- .jnew("java/util/ArrayList")

    ccp_nlp_type_system <- .jfield("edu/ucdenver/ccp/nlp/uima/util/TypeSystemUtil",
        name = "CCP_TYPE_SYSTEM")

    sentence_detector_type_system_str <- "org.cleartk.token.type.Sentence"

    conceptmapper_type_system <- "edu.ucdenver.ccp.nlp.wrapper.conceptmapper.TypeSystem"

    dictTerm <- "analysis_engine.primitive.DictTerm"

    tokenizer <- "org.apache.uima.conceptMapper.support.tokenizer.TokenAnnotation"

    vector_of_ts <- c(ccp_nlp_type_system, sentence_detector_type_system_str,
        conceptmapper_type_system, dictTerm, tokenizer)


    type_system_description <- J("org/uimafit/factory/TypeSystemDescriptionFactory")$createTypeSystemDescription(vector_of_ts)

    .Object@typeSystemRef <- type_system_description
    return(.Object)
})






setMethod("initialize", "Similarity", function(.Object) {
    .Object@similarityInstance <- .jnew("iit/comp/epigen/nlp/similarity/Similarity")
    .Object@pairwiseConfig = NA_character_
    .Object@icConfig = NA_character_
    .Object@groupConfig = NA_character_
    .Object
})







