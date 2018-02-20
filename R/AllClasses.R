#' OnASSiS package
#' @description
#'OnASSiS (Ontology Annotations and Semantic Similarity software) is a package for the annotation of any given text with concepts from biomedical ontologies that also provides features to relate the concepts using semantic similarity metrics.

#' @details

#'OnASSiS (Ontology Annotations and Semantic Similarity software) is a package that uses Conceptmapper, an Apache UIMA (Unstructured Information Management Architecture) \url{https://uima.apache.org/downloads/sandbox/ConceptMapperAnnotatorUserGuide/ConceptMapperAnnotatorUserGuide.html} dictionary lookup tool to retrieve dictionary terms in a given text.
#'
#' In particular a Conceptmapper wrapper specific for the biomedical domain, ccp-nlp, (\url{https://github.com/UCDenver-ccp/ccp-nlp}) has been personalized to retrieve concepts from OBO ontologies in a given text with different options.
#'
#'The package also provides the possibility to annotate Gene Expression Omnibus (GEO) metadata for stored experiments and samples.
#'
#'Different annotated sets of text can be then compared using semantic similarity metrics based on the structure of the biomedical ontologies.
#'The semantic similarity module has been obtained using the Java slib  (\url{http://www.semantic-measures-library.org/sml/})

#' @name OnASSiS
#' @rdname Onassis
#' @title OnASSiS (Ontology Annotations and Semantic Similarity software)
#' @docType package
#' @aliases Onassis-package
#'
NULL



#' Onassis-class
#'
#' @description Onassis is a container class for annotating samples metadata with concepts from dictionaries/ontologies, creating semantic sets of unique annotations, computing the distances between different semantic sets and eventually comparing the different identified conditions.
#' @slot dictionary One or more input dictionaries to annotate samples metadata
#' @slot entities a data frame containing the result of the annotation of the input with ontology terms
#' @slot similarity A matrix of the similarities between the entries in the entities slot
#' @slot scores An optional score matrix containing genomic units on the rows (genes, regions) and on the columns the elements on the rows of the entities slot
#' @details The following methods can be applied to Onassis
#'
#'\code{\link{annotate}} \cr
#'\code{\link{collapse}} \cr
#'\code{\link{compare}} \cr
#' @exportClass Onassis
setClass(Class = "Onassis", representation(dictionary = "character", entities = "data.frame", 
    similarity = "matrix", scores = "matrix"), validity = function(object) {
    text <- character()
    
    if (length(text)) 
        text else TRUE
})





#' Class that stores a Conceptmapper dictionary
#'
#' @description CMdictionary is a class that wraps a Conceptmapper ccp-nlp Java dictionary. Its methods allow the creation of a dictionary from OBO ontologies in OBO or OWL format. Different options to build the dictionary are available.
#' @slot dict_location The path of the created dictionary file
#' @slot dictRef Reference to the java object representing the dictionary
#' @slot dictInfo Information about how dictionary has been created. It is a list with the following fields
#'\itemize{
#'\item{Dictionary Type:{The type of dictionary}
#'  \itemize{
#'       \item{OBO}{A dictionary that has been created by An OBO file}
#'       \item{ENTREZ}{Entrez genes dictionary}
#'       \item{TARGET}{Entrez genes dictionary, Histone marks and Histone modifications}
#'       \item{CMDICT}{A previously created dictionary file in the Conceptmapper XML format}
#'  }
#'}
#'
#'\item{SynonymType: {The type of synonyms to consider when building the dictionary for Conceptmapper. For further detail \url{http://owlcollab.github.io/oboformat/doc/obo-syntax.html}}
#'\itemize{
#'\item{EXACT}{}
#'\item{BROAD}{}
#'\item{NARROW}{}
#'\item{RELATED}{}
#'\item{ALL}{}
#'}
#'}
#'
#'\item{dictSource: {The OBO/OWL dictionary file to convert to a Conceptmapper dictionary in case the type is OBO. The XML file in case the type is CMDICT}}
#'
#'\item{taxID The NCBI taxon identifier for species to create the Entrez gene dictionary (e.g 9606 for Mus musculus)}
#'}
#' @details The following methods can be applied to CMdictionary instances
#'
#' \code{\link{dict_location}} \cr
#' \code{\link{dict_location<-}} \cr
#' \code{\link{dictInfo}} \cr
#' \code{\link{dictInfo<-}} \cr
#' \code{\link{dictRef}} \cr
#' \code{\link{dictRef<-}} \cr
#'
#' To show the available dictionary types use the function
#' \code{\link{dictTypes}} \cr
#' @examples
#'dict <- new('CMdictionary')
setClass(Class = "CMdictionary", representation(dict_location = "character", dictInfo = "list", 
    dictRef = "jobjRef"), validity = function(object) {
    text <- character()
    if (is.na(object@dict_location) | !file.exists(object@dict_location)) 
        text <- c("Invalid directory Path: You should provide a valid directory for the Conceptmapper dictionary")
    if (length(text)) 
        text else TRUE
})





#' Class to set the options to run the EntityFinder
# 
#' @description CMoptions is a class that represents Conceptmapper configurations. It allows users to set the possible combinations of different parameters for Conceptmapper running.
#' @slot paramValueIndex An integer value to index the 576 parameter combinations
#' @slot SearchStrategy The matching strategy for finding concepts in the input text
#'\itemize{
#'\item{CONTIGUOUS_MATCH}{Longets match of contiguous tokens within enclosing span}
#'\item{SKIP_ANY_MATCH}{Longest match of not-necessarily contiguous tokens}
#'\item{SKIP_ANY_MATCH_ALLOW_OVERLAP}{Longest match of not-necessarily contiguous tokens, overlapping matches are allowed}
#'}
#' @slot CaseMatch
#'\itemize{
#'\item{CASE_IGNORE}{Fold everything to lowercase for matching}
#'\item{CASE_INSENSITIVE}{Fold only tokens with initial caps to lowercase}
#'\item{CASE_FOLD_DIGITS}{Fold all (and only) tokens with a digit}
#'\item{CASE_SENSITIVE}{Perform no case folding}
#'}
#' @slot Stemmer
#'\itemize{
#'\item BIOLEMMATIZER {A stemmer specific for biomedical literature}
#'\item PORTER {A stemmer that removes the commoner morphological and inflexional endings from words in English}
#'\item NONE {No word stemming}
#'}
#' @slot StopWords
#'\itemize{
#'\item PUBMED {A list of stop words obtained analyzing Pubmed papers}
#'\item NONE {No stop words }
#'}
#'@slot OrderIndependentLookup
#'\itemize{
#'\item ON {Ordering within span is ignored (i.e. 'Breast cancer' would equal 'Cancer breast') }
#'\item OFF {Ordering is taken into consideration}
#'}
#' @slot FindAllMatches
#'\itemize{
#'\item YES {All the matches within the span are found }
#'\item NO {Only the longest match within the span will be returned}
#'}
#' @slot SynonymType
#'\itemize{
#'\item EXACT_ONLY {Only exact synonyms are considered }
#'\item ALL {All synonym types are included}
#'}
#' @details The following methods can be applied to CMoptions
#'
#' \code{\link{show}}  \cr
#' \code{\link{paramValueIndex}} \cr
#' \code{\link{paramValueIndex<-}} \cr
#' \code{\link{SearchStrategy}} \cr
#' \code{\link{SearchStrategy<-}} \cr
#' \code{\link{CaseMatch}} \cr
#' \code{\link{CaseMatch<-}} \cr
#' \code{\link{Stemmer}} \cr
#' \code{\link{Stemmer<-}} \cr
#' \code{\link{StopWords}} \cr
#' \code{\link{StopWords<-}} \cr
#' \code{\link{OrderIndependentLookup}} \cr
#' \code{\link{OrderIndependentLookup}} \cr
#' \code{\link{FindAllMatches}} \cr
#' \code{\link{FindAllMatches<-}} \cr
#' \code{\link{SynonymType}} \cr
#' \code{\link{SynonymType<-}} \cr
#' @examples
#' options <- new('CMoptions')
setClass(Class = "CMoptions", representation = representation(paramValueIndex = "character", 
    SearchStrategy = "character", CaseMatch = "character", Stemmer = "character", 
    StopWords = "character", OrderIndependentLookup = "character", FindAllMatches = "character", 
    SynonymType = "character"), validity = function(object) {
    text <- character()
    options_combinations <- readRDS(system.file("extdata", "Options_table.rds", package = "Onassis"))
    if (!object@paramValueIndex %in% unique(options_combinations$paramValueIndex)) 
        text <- c("Invalid paramValueIndex: should be a number in [0:575]")
    if (!object@SearchStrategy %in% unique(options_combinations$SearchStrategy)) 
        text <- c(text, "Invalid SearchStrategy argument")
    if (!object@CaseMatch %in% unique(options_combinations$CaseMatch)) 
        text <- c(text, "Invalid SearchStrategy argument")
    if (!object@Stemmer %in% unique(options_combinations$Stemmer)) 
        text <- c(text, "Invalid Stemmer argument")
    if (!object@StopWords %in% unique(options_combinations$Stopwords)) 
        text <- c(text, "Invalid StopWords argument")
    if (!object@OrderIndependentLookup %in% unique(options_combinations$OrderIndependentLookup)) 
        text <- c(text, "Invalid OrderIndependentLookup argument")
    if (!object@FindAllMatches %in% unique(options_combinations$FindAllMatches)) 
        text <- c(text, "Invalid FindAllMatches argument")
    if (!object@SynonymType %in% unique(options_combinations$SynonymType)) 
        text <- c(text, "Invalid SynonymType argument")
    
    
    if (length(text)) 
        text else TRUE
})





#' EntityFinder class to create a Conceptmapper instance
#' @description EntityFinder is a class that wraps a Conceptmapper pipeline using the CCP UIMA Type System \url{https://github.com/UCDenver-ccp/ccp-nlp}. The pipeline includes a sentence detector, offset tokenizer and retrieves concepts from dictionaries built from  OBO/OWL formatted ontology files.
#' @slot typeSystemRef The reference to the Java object representing the type system
#' @details The following methods can be applied to EntityFinder
#'
#'\code{\link{findEntities}} \cr
#'\code{\link{annotateDF}} \cr
#' The methods can be automatically called using the function
#' \code{\link{EntityFinder}} \cr
#' @examples
#' finder <- new('EntityFinder')
setClass(Class = "EntityFinder", representation = representation(typeSystemRef = "jobjRef"))


#' Similarity class to compute similarities between concepts in ontologies and samples annotated with different concepts
#'\code{Similarity-class}
#' @description Similarity is a class that wraps some methods of the Java library slib \url{http://www.semantic-measures-library.org/sml/}. Starting from OBO ontologies it is possible to build semantic graphs that allow the computation of different similarity measures between concepts belonging to the same ontology, group of concepts, samples annotated with different ontology concepts. Further details about the graph based semantic similarity measures are available at \url{http://www.semantic-measures-library.org/sml/index.php?q=doc_graph_based_advanced}
#' @slot similarityInstance The Java reference to the Java Similarity class.
#' @slot pairwiseConfig The list of measures used to compute the semantic similarity between two concpets in the same ontology.
#' @slot pairwiseConfigRef The reference to the Java object of type CMconf corresponding to the pairwise configuration
#' @slot groupConfig The groupwise configuration to compute the semantic similarity between groups of concepts.
#' @slot icConfig The information content measure
#'@slot groupwiseConfigRef The reference to the Java configuration object for the computation of semantic similarity between groups of concepts
#' @slot ontology The ontology to compute semantic similarities

#' @details The following methods can be applied to Similarity
#'
#'\code{\link{ontology<-}} \cr
#'\code{\link{pairwiseConfig}} \cr
#'\code{\link{groupConfig}} \cr
#'\code{\link{sim}} \cr
#'\code{\link{groupsim}} \cr
#'\code{\link{samplesim}} \cr
#' To see the available similarity measures run the function \code{\link{listSimilarities}}
#' @examples
#' sim <- new('Similarity')
#'
#' @exportClass Similarity
#' @export
setClass(Class = "Similarity", representation = representation(similarityInstance = "jobjRef", 
    pairwiseConfig = "character", pairwiseConfigRef = "jobjRef", icConfig = "character", 
    groupConfig = "character", groupwiseConfigRef = "jobjRef", ontology = "jobjRef"))




