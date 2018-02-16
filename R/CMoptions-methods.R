#' \code{show}
#'
#' @rdname CMoptions-class
#' @aliases show,CMoptions-method
#' @param object  CMoptions instance
#' @return the list of options
#' @description This method shows the list of options to run the Entity finder
#' @examples
#' opt <- CMoptions()
#' show(opt)
setMethod("show", "CMoptions", function(object) {
    message("CMoptions object to set ConceptMapper Options")
        message(
          'SearchStrategy: ', object@SearchStrategy, '\n',
          'CaseMatch: ' , object@CaseMatch, '\n',
          'Stemmer: ' , object@Stemmer, '\n',
          'StopWords: ' , object@StopWords, '\n',
          'OrderIndependentLookup: ' , object@OrderIndependentLookup, '\n',
          'FindAllMatches: ', object@FindAllMatches, '\n',
          'SynonymType: ' , object@SynonymType
        )
})

#' \code{paramValueIndex}
#'
#' @description Method to get and set the parameter combination index corresponding to a given parameter combination. The value of the paramValueIndex lays in the range [0:575]
#' @param x instance of class \code{\link{CMoptions}}
#' @return The paramValueIndex corresponding to the current options when used as get, the new CMoptions object with updated parameters when used to set.
#' @examples
#' opts <- CMoptions()
#' paramValueIndex(opts)
#'
setMethod("paramValueIndex", "CMoptions", function(x) {
    as.character(x@paramValueIndex)
})



#' \code{paramValueIndex<-}
#' @rdname paramValueIndex
#' @param value Index corresponding to a given parameter combination
#' @examples
#' opts <- CMoptions()
#' paramValueIndex(opts) <- 2
setReplaceMethod("paramValueIndex", "CMoptions", function(x,
    value) {
    options_combinations <- readRDS(system.file("extdata",
        "Options_table.rds", package = "Onassis"))
     x@paramValueIndex <- as.character(value)
     x@SearchStrategy = as.character(as.vector(unique(options_combinations$SearchStrategy[which(options_combinations$paramValueIndex==value)])))
     x@CaseMatch = as.character(as.vector(unique(options_combinations$CaseMatch[which(options_combinations$paramValueIndex==value)])))
     x@Stemmer = as.character(as.vector(unique(options_combinations$Stemmer[which(options_combinations$paramValueIndex==value)])))
     x@StopWords = as.character(as.vector(unique(options_combinations$Stopwords[which(options_combinations$paramValueIndex==value)])))
     x@OrderIndependentLookup = as.character(as.vector(unique(options_combinations$OrderIndependentLookup[which(options_combinations$paramValueIndex==value)])))
     x@FindAllMatches = as.character(as.vector(unique(options_combinations$FindAllMatches[which(options_combinations$paramValueIndex==value)])))
     x@SynonymType = as.character(as.vector(unique(options_combinations$SynonymType[which(options_combinations$paramValueIndex==value)])))
     validObject(x)
     x
})




#' \code{SearchStrategy}
#'
#' @description Method to get and set the searchStrategy parameter
#' @param x instance of class \code{\link{CMoptions}}
#' @return The searchStrategy corresponding to the current options when used as get, the new CMoptions object with updated parameters when used to set.
#' @examples
#' opts <- CMoptions()
#' SearchStrategy(opts)
#'
setMethod("SearchStrategy", "CMoptions", function(x) {
  as.character(x@searchStrategy)
})



#' \code{SearchStrategy<-}
#' @rdname SearchStrategy
#' @param value The matching strategy for finding concepts in the input text
#'\itemize{
#'\item{CONTIGUOUS_MATCH}{Longets match of contiguous tokens within enclosing span}
#'\item{SKIP_ANY_MATCH}{Longest match of not-necessarily contiguous tokens}
#'\item{SKIP_ANY_MATCH_ALLOW_OVERLAP}{Longest match of not-necessarily contiguous tokens, overlapping matches are allowed}
#'}
#' @examples
#' opts <- CMoptions()
#' SearchStrategy(opts) <- 'SKIP_ANY_MATCH_ALLOW_OVERLAP'
setReplaceMethod("SearchStrategy", "CMoptions", function(x,
                                                          value) {
  options_combinations <- readRDS(system.file("extdata",
                                              "Options_table.rds", package = "Onassis"))
  x@SearchStrategy <- as.character(value)
  x@paramValueIndex <- as.character(as.vector(unique(options_combinations$paramValueIndex[
    which(options_combinations$SearchStrategy==x@SearchStrategy &
            options_combinations$CaseMatch==x@CaseMatch &
            options_combinations$Stemmer==x@Stemmer &
            options_combinations$Stopwords==x@StopWords &
            options_combinations$OrderIndependentLookup==x@OrderIndependentLookup &
            options_combinations$FindAllMatches==x@FindAllMatches &
            options_combinations$SynonymType==x@SynonymType
    )])))
  validObject(x)
  x
})



#' \code{CaseMatch}
#' @rdname CaseMatch
#' @description Method to get and set the CaseMatch parameter
#' @param x instance of class \code{\link{CMoptions}}
#' @return The CaseMatch corresponding to the current options when used as get, the new CMoptions object with updated parameters when used to set.
#' @examples
#' opts <- CMoptions()
#' CaseMatch(opts)
#'
setMethod("CaseMatch", "CMoptions", function(x) {
  as.character(x@CaseMatch)
})



#' \code{CaseMatch<-}
#' @rdname CaseMatch
#' @param value One of the following :
#'\itemize{
#'\item{CASE_IGNORE}{ Fold everything to lowercase for matching}
#'\item{CASE_INSENSITIVE}{ Fold only tokens with initial caps to lowercase}
#'\item{CASE_FOLD_DIGITS}{ Fold all (and only) tokens with a digit}
#'\item{CASE_SENSITIVE}{ Perform no case folding}
#'}
#' @examples
#' opts <- CMoptions()
#' CaseMatch(opts) <- 'CASE_SENSITIVE'
setReplaceMethod("CaseMatch", "CMoptions", function(x,
                                                         value) {
  options_combinations <- readRDS(system.file("extdata",
                                              "Options_table.rds", package = "Onassis"))
  x@CaseMatch <- as.character(value)
  x@paramValueIndex <- as.character(as.vector(unique(options_combinations$paramValueIndex[
    which(options_combinations$SearchStrategy==x@SearchStrategy &
            options_combinations$CaseMatch==x@CaseMatch &
            options_combinations$Stemmer==x@Stemmer &
            options_combinations$Stopwords==x@StopWords &
            options_combinations$OrderIndependentLookup==x@OrderIndependentLookup &
            options_combinations$FindAllMatches==x@FindAllMatches &
            options_combinations$SynonymType==x@SynonymType
    )])))
  validObject(x)
  x
})






#' \code{Stemmer}
#'
#' @description Method to get and set the Stemmer parameter
#' @param x instance of class \code{\link{CMoptions}}
#' @return The Stemmer corresponding to the current options when used as get, the new CMoptions object with updated parameters when used to set.
#' @examples
#' opts <- CMoptions()
#' Stemmer(opts)
#'
setMethod("Stemmer", "CMoptions", function(x) {
  as.character(x@Stemmer)
})



#' \code{Stemmer<-}
#' @rdname Stemmer
#' @param value
#'\itemize{
#'\item BIOLEMMATIZER {A stemmer specific for biomedical literature}
#'\item PORTER {A stemmer that removes the commoner morphological and inflexional endings from words in English}
#'\item NONE {No word stemming}
#'}
#' @examples
#' opts <- CMoptions()
#' Stemmer(opts) <- 'PORTER'
setReplaceMethod("Stemmer", "CMoptions", function(x,
                                                    value) {
  options_combinations <- readRDS(system.file("extdata",
                                              "Options_table.rds", package = "Onassis"))
  x@Stemmer <- as.character(value)
  x@paramValueIndex <- as.character(as.vector(unique(options_combinations$paramValueIndex[
    which(options_combinations$SearchStrategy==x@SearchStrategy &
            options_combinations$CaseMatch==x@CaseMatch &
            options_combinations$Stemmer==x@Stemmer &
            options_combinations$Stopwords==x@StopWords &
            options_combinations$OrderIndependentLookup==x@OrderIndependentLookup &
            options_combinations$FindAllMatches==x@FindAllMatches &
            options_combinations$SynonymType==x@SynonymType
    )])))
  validObject(x)
  x
})






#' \code{StopWords}
#'
#' @description Method to get and set the StopWords parameter
#' @param x instance of class \code{\link{CMoptions}}
#' @return The StopWords corresponding to the current options when used as get, the new CMoptions object with updated parameters when used to set.
#' @examples
#' opts <- CMoptions()
#' StopWords(opts)
#'
setMethod("StopWords", "CMoptions", function(x) {
  as.character(x@StopWords)
})



#' \code{StopWords<-}
#' @rdname StopWords
#' @param value
#'\itemize{
#'\item PUBMED {A list of stop words obtained analyzing Pubmed papers}
#'\item NONE {No stop words }
#'}
#' @examples
#' opts <- CMoptions()
#' StopWords(opts) <- 'NONE'
setReplaceMethod("StopWords", "CMoptions", function(x,
                                                  value) {
  options_combinations <- readRDS(system.file("extdata",
                                              "Options_table.rds", package = "Onassis"))
  x@StopWords <- as.character(value)
  x@paramValueIndex <- as.character(as.vector(unique(options_combinations$paramValueIndex[
    which(options_combinations$SearchStrategy==x@SearchStrategy &
            options_combinations$CaseMatch==x@CaseMatch &
            options_combinations$Stemmer==x@Stemmer &
            options_combinations$Stopwords==x@StopWords &
            options_combinations$OrderIndependentLookup==x@OrderIndependentLookup &
            options_combinations$FindAllMatches==x@FindAllMatches &
            options_combinations$SynonymType==x@SynonymType
    )])))
  validObject(x)
  x
})




#' \code{OrderIndependentLookup}
#'
#' @description Method to get and set the OrderIndependentLookup parameter
#' @param x instance of class \code{\link{CMoptions}}
#' @return The OrderIndependentLookup corresponding to the current options when used as get, the new CMoptions object with updated parameters when used to set.
#' @examples
#' opts <- CMoptions()
#' OrderIndependentLookup(opts)
#'
setMethod("OrderIndependentLookup", "CMoptions", function(x) {
  as.character(x@OrderIndependentLookup)
})



#' \code{OrderIndependentLookup<-}
#' @rdname OrderIndependentLookup
#' @param value
#'\itemize{
#'\item ON {Ordering within span is ignored (i.e. 'Breast cancer' would equal 'Cancer breast') }
#'\item OFF {Ordering is taken into consideration}
#'}
#' @examples
#' opts <- CMoptions()
#' OrderIndependentLookup(opts) <- 'YES'
setReplaceMethod("OrderIndependentLookup", "CMoptions", function(x,
                                                    value) {
  options_combinations <- readRDS(system.file("extdata",
                                              "Options_table.rds", package = "Onassis"))
  x@OrderIndependentLookup <- as.character(value)
  x@paramValueIndex <- as.character(as.vector(unique(options_combinations$paramValueIndex[
    which(options_combinations$SearchStrategy==x@SearchStrategy &
            options_combinations$CaseMatch==x@CaseMatch &
            options_combinations$Stemmer==x@Stemmer &
            options_combinations$Stopwords==x@StopWords &
            options_combinations$OrderIndependentLookup==x@OrderIndependentLookup &
            options_combinations$FindAllMatches==x@FindAllMatches &
            options_combinations$SynonymType==x@SynonymType
    )])))
  validObject(x)
  x
})





#' \code{FindAllMatches}
#'
#' @description Method to get and set the FindAllMatches parameter
#' @param x instance of class \code{\link{CMoptions}}
#' @return The FindAllMatches corresponding to the current options when used as get, the new CMoptions object with updated parameters when used to set.
#' @examples
#' opts <- CMoptions()
#' FindAllMatches(opts)
#'
setMethod("FindAllMatches", "CMoptions", function(x) {
  as.character(x@FindAllMatches)
})



#' \code{FindAllMatches<-}
#' @rdname FindAllMatches
#' @param value
#'\itemize{
#'\item YES {All the matches within the span are found }
#'\item NO {Only the longest match within the span will be returned}
#'}
#' @examples
#' opts <- CMoptions()
#' FindAllMatches(opts) <- 'YES'
setReplaceMethod("FindAllMatches", "CMoptions", function(x,
                                                                 value) {
  options_combinations <- readRDS(system.file("extdata",
                                              "Options_table.rds", package = "Onassis"))
  x@FindAllMatches <- as.character(value)
  x@paramValueIndex <- as.character(as.vector(unique(options_combinations$paramValueIndex[
    which(options_combinations$SearchStrategy==x@SearchStrategy &
            options_combinations$CaseMatch==x@CaseMatch &
            options_combinations$Stemmer==x@Stemmer &
            options_combinations$Stopwords==x@StopWords &
            options_combinations$OrderIndependentLookup==x@OrderIndependentLookup &
            options_combinations$FindAllMatches==x@FindAllMatches &
            options_combinations$SynonymType==x@SynonymType
    )])))
  validObject(x)
  x
})






#' \code{SynonymType}
#'
#' @description Method to get and set the SynonymType parameter
#' @param x instance of class \code{\link{CMoptions}}
#' @return The SynonymType corresponding to the current options when used as get, the new CMoptions object with updated parameters when used to set.
#' @examples
#' opts <- CMoptions()
#' SynonymType(opts)
#'
setMethod("SynonymType", "CMoptions", function(x) {
  as.character(x@SynonymType)
})



#' \code{SynonymType<-}
#' @rdname SynonymType
#' @param value
#'\itemize{
#'\item EXACT_ONLY {Only exact synonyms are considered }
#'\item ALL {All synonym types are included}
#'}
#' @examples
#' opts <- CMoptions()
#' SynonymType(opts) <- 'ALL'
setReplaceMethod("SynonymType", "CMoptions", function(x,
                                                         value) {
  options_combinations <- readRDS(system.file("extdata",
                                              "Options_table.rds", package = "Onassis"))
  x@SynonymType <- as.character(value)
  x@paramValueIndex <- as.character(as.vector(unique(options_combinations$paramValueIndex[
    which(options_combinations$SearchStrategy==x@SearchStrategy &
            options_combinations$CaseMatch==x@CaseMatch &
            options_combinations$Stemmer==x@Stemmer &
            options_combinations$Stopwords==x@StopWords &
            options_combinations$OrderIndependentLookup==x@OrderIndependentLookup &
            options_combinations$FindAllMatches==x@FindAllMatches &
            options_combinations$SynonymType==x@SynonymType
    )])))
  validObject(x)
  x
})









#' \code{listCMOptions}
#'
#' @description This method retrieves all the possible parameters combinations for Conceptmapper.
#'\describe{
#'\item{paramValueIndex}{An integer value to index the 576 parameter combinations}
#'\item{SearchStrategy}{The matching strategy for finding concepts in the input text}
#'\itemize{
#'\item{CONTIGUOUS_MATCH}{Longets match of contiguous tokens within enclosing span}
#'\item{SKIP_ANY_MATCH}{Longest match of not-necessarily contiguous tokens}
#'\item{SKIP_ANY_MATCH_ALLOW_OVERLAP}{Longest match of not-necessarily contiguous tokens, overlapping matches are allowed}
#'}
#'\item{CaseMatch}{}
#'\itemize{
#'\item{CASE_IGNORE}{Fold everything to lowercase for matching}
#'\item{CASE_INSENSITIVE}{Fold only tokens with initial caps to lowercase}
#'\item{CASE_FOLD_DIGITS}{Fold all (and only) tokens with a digit}
#'\item{CASE_SENSITIVE}{Perform no case folding}
#'}
#'\item{Stemmer}{}
#'\itemize{
#'\item BIOLEMMATIZER {A stemmer specific for biomedical literature}
#'\item PORTER {A stemmer that removes the commoner morphological and inflexional endings from words in English}
#'\item NONE {No word stemming}
#'}
#'\item{StopWords}{}
#'\itemize{
#'\item PUBMED {A list of stop words obtained analyzing Pubmed papers}
#'\item NONE {No stop words }
#'}
#'\item{OrderIndependentLookup}{}
#'\itemize{
#'\item ON {Ordering within span is ignored (i.e. 'Breast cancer' would equal 'Cancer breast') }
#'\item OFF {Ordering is taken into consideration}
#'}
#'\item{FindAllMatches}{}
#'\itemize{
#'\item YES {All the matches within the span are found }
#'\item NO {Only the longest match within the span will be returned}
#'}
#'\item{SynonymType}{}
#'\itemize{
#'\item EXACT_ONLY {Only exact synonyms are considered }
#'\item ALL {All synonym types are included}
#'}
#'}

#' @rdname listCMOptions
#' @return The data frame with all the possible parameter combinations
#' @examples
#' o <- listCMOptions()


listCMOptions <- function(x) {
  options_combinations <- readRDS(system.file("extdata",
                                              "Options_table.rds", package = "Onassis"))
  options_combinations[, 1:ncol(options_combinations)]
}
