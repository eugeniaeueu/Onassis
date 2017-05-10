#' @importFrom utils download.file
# Init method that is called when a new CMdictionary is created
initJavaLibs <- function(libLoc, mem = "12G") {
    # Check if library directory is missing
    if (missing(libLoc)) {
        libLoc = system.file("java", package = "Onassis")
    }
    # Check if the provided directory is valid
    if (!file.exists(libLoc) || !file.info(libLoc)$isdir) {
      dir.create(libLoc)
    }
    # Adding the directory to the path
    path = Sys.glob(paste0(libLoc, "/*.jar"))
    # Check if access to the library directory is allowed
    if (file.access(libLoc, "6") == -1)
        stop("No read+write access to specified lib location")


    download_url1 = "https://sourceforge.net/projects/onassis/files/conceptmapping-0.0.1-SNAPSHOT-jar-with-dependencies.jar/download"
    download_url2 = "https://sourceforge.net/projects/onassis/files/similarity-0.0.1-SNAPSHOT-jar-with-dependencies.jar/download"
    # Listing files in the library directory
    available_local_files <- list.files(libLoc, full.names = FALSE, pattern = "\\.jar$")

    # Check if all the files are there
    if (length(available_local_files) == 0) {
        message("Onassis is attempting to download Java libraries. The process might take a while...")
        complete_remote_files <- download.file(download_url1, destfile = file.path(libLoc, "conceptmapper-0.0.1-SNAPSHOT-jar-with-dependencies.jar"))
        similarity_lib <- download.file(download_url2, destfile = file.path(libLoc, "similarity-0.0.1-SNAPSHOT-jar-with-dependencies.jar"))
    }


    # Start java engine, if not already done, and add to classpath options(java.parameters = paste0('-Xmx', mem)) rJava::.jinit()
    rJava::.jaddClassPath(path)

    # Determine if the jar files have been loaded correctly
    len = length(grep("*", basename(rJava::.jclassPath())))
    if (len == 0L)
        stop("The jar files not found in libLoc.")

    rJava::.jaddClassPath(dirname(path))
}






setMethod("initialize", "CMdictionary", function(.Object) {
    .Object@dictInfo = list("Empty dictionary", "NA", "NA", "0")
    names(.Object@dictInfo) <- c("Dictionary_type", "Dictionary_source", "Synonym_type", "taxid")
    .Object@dict_location = tempdir()
    .Object@dictRef = .jnull("java/io/File")
    .Object
})






# This method initializes the Conceptmapper Options to the 31th parameter combination.  SearchStrategy: CONTIGUOUS_MATCH
# CaseMatch: CASE_INSENSITIVE Stemmer: PORTER Stopwords: NONE OrderIndependentLookup: OFF FindAllMatches: NO SynonymType:
# EXACT_ONLY

setMethod("initialize", "CMoptions", function(.Object) {
    options_combinations <- readRDS(system.file("extdata", "Options_table.rds", package = "Onassis"))
    .Object@arguments <- options_combinations[32, , drop = TRUE]
    .Object
})




setMethod("initialize", "EntityFinder", function(.Object) {

    # Creating a conceptmapper type system
    type_system_array_list <- .jnew("java/util/ArrayList")

    ccp_nlp_type_system <- .jfield("edu/ucdenver/ccp/nlp/uima/util/TypeSystemUtil", name = "CCP_TYPE_SYSTEM")

    sentence_detector_type_system_str <- "org.cleartk.token.type.Sentence"

    conceptmapper_type_system <- "edu.ucdenver.ccp.nlp.wrapper.conceptmapper.TypeSystem"

    dictTerm <- "analysis_engine.primitive.DictTerm"

    tokenizer <- "org.apache.uima.conceptMapper.support.tokenizer.TokenAnnotation"

    vector_of_ts <- c(ccp_nlp_type_system, sentence_detector_type_system_str, conceptmapper_type_system, dictTerm, tokenizer)


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







