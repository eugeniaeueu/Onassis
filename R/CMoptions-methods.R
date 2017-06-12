#' \code{show}
#'
#' @rdname CMoptions-class
#' @aliases show,CMoptions-method
#' @param object  CMoptions instance
#' @return the list of options
#' @description This method shows the list of options to run the Entity finder
#' @examples
#' opt <- new('CMoptions')
#' show(opt)
setMethod("show", "CMoptions", function(object) {
    message("CMoptions object to set ConceptMapper Options")
    for (i in 2:length(object@arguments)) {
        message(paste0(names(object@arguments)[i], ": ", as.character(object@arguments[[i]])))

    }
})




#' \code{listCombinations}
#'
#' @description This method retrieves all the possible parameter combination for Conceptmapper.
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

#' @rdname CMoptions-class
#' @aliases listCombinations,CMoptions-method
#' @return The data frame with all the possible parameter combinations
#' @examples
#' opts <- new('CMoptions')
#' listCombinations(opts)


setMethod("listCombinations", "CMoptions", function(x) {
    options_combinations <- readRDS(system.file("extdata", "Options_table.rds", package = "Onassis"))
    options_combinations[, 2:ncol(options_combinations)]
})



#' \code{paramValueIndex}
#'
#' @description This method retrieves the parameter combination index corresponding to a given parameter combination. The value of the paramValueIndex lays in the range [0:575]
#' @rdname CMoptions-class
#' @aliases paramValueIndex,CMoptions-method
#' @param x CMoptions instance
#' @return The paramValueIndex corresponding to the current options. If user has not changed the options this is set to 31
#' @examples
#' opts <- new('CMoptions')
#' paramValueIndex(opts)
#'

setMethod("paramValueIndex", "CMoptions", function(x) {
    as.character(x@arguments$paramValueIndex)
})



#' \code{paramValueIndex<-}
#' @description This method sets the parameter combination for the given index. When the paramValueIndex is set to a given value the corresponding settings for the other parameters are automatically loaded.
#' @rdname CMoptions-class
#' @aliases paramValueIndex<-,CMoptions-method
#' @return The updated CMoptions S4 object
#' @examples
#' opts <- new('CMoptions')
#' list_opts <- listCombinations(opts)
#'
#' paramValueIndex(opts) <- 2

setReplaceMethod("paramValueIndex", "CMoptions", function(x, value) {
    options_combinations <- readRDS(system.file("extdata", "Options_table.rds", package = "Onassis"))

    if (!value %in% options_combinations$paramValueIndex)
        stop("Invalid param Value Index")
    value <- value + 1
    x@arguments <- as.list(options_combinations[(value), ])
    x
})


#' \code{CMargs}
#'
#' @description This method retrieves the list of parameters currently set to run Conceptmapper
#' @rdname CMoptions-class
#' @aliases CMargs,CMoptions-method
#' @return list of parameters currently set
#' @examples
#' opts <- new('CMoptions')
#' CMargs(opts)

setMethod("CMargs", "CMoptions", function(x) {
    x@arguments
})



#' \code{CMargs<-}
#' @aliases CMargs<-,CMoptions-method
#' @description This method sets a list of parameters and updates the paramValueIndex to the correct value if necessary.
#' @param value a list of arguments in the containing paramValueIndex (options), SearchStrategy, CaseMatch, Stemmer, Stopwords, OrderIndependentLookup, FindAllMatches and SynonymType parameters in the specified order.
#' @rdname CMoptions-class
#' @return The updated CMoptions S4 object
#' @examples
#' opts <- new('CMoptions')
#' list_opts <- listCombinations(opts)
#' CMargs(opts) <- as.list(c('CONTIGUOUS_MATCH', 'CASE_IGNORE',
#'  'BIOLEMMATIZER', 'NONE', 'OFF', 'YES', 'EXACT_ONLY'))

setReplaceMethod("CMargs", "CMoptions", function(x, value) {
    if (length(value) == (length(x@arguments)-1))
        value <- c("1", value)
    options_combinations <- readRDS(system.file("extdata", "Options_table.rds", package = "Onassis"))
    lev <- lapply(x@arguments, levels)
    nam <- names(x@arguments)
    x@arguments <- value
    names(x@arguments) <- nam

    index <- apply(options_combinations, 1, function(entire_row) {
        row <- as.character(entire_row[2:length(entire_row)])

        argument_row <- x@arguments[2:length(x@arguments)]
        argument_row <- unname(sapply(argument_row, as.character))
        if (identical(argument_row, row))
            return(TRUE) else return(FALSE)
    })
    paramValueIndex <- options_combinations[index == TRUE, 1]
    x@arguments$paramValueIndex <- paramValueIndex
    x@arguments <- lapply(1:length(x@arguments), function(z) factor(as.character(x@arguments[[z]]), levels = lev[[z]]))
    names(x@arguments) <- nam
    validObject(x)
    x
})





