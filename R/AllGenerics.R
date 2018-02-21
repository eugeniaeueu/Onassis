############################################### Generics for class dictBuilder ##################################

#' @rdname dict_location
setGeneric("dict_location", function(.Object) standardGeneric("dict_location"))

#' @rdname dict_location
setGeneric("dict_location<-", function(.Object, value) standardGeneric("dict_location<-"))


#' Get and Set the dicInfo slot for the \code{\link{CMdictionary}} class
setGeneric("dictInfo", function(.Object) standardGeneric("dictInfo"))


#' @rdname dictInfo
setGeneric("dictInfo<-", function(.Object, value) standardGeneric("dictInfo<-"))



#' @rdname dictRef
setGeneric("dictRef", function(.Object) standardGeneric("dictRef"))


#' @rdname dictRef
setGeneric("dictRef<-", function(.Object, value) standardGeneric("dictRef<-"))



# ############################################## Generics for class CMoptions
# ###################################


#' Method paramValueIndex
setGeneric("paramValueIndex", function(x) standardGeneric("paramValueIndex"))


#' Method paramValueIndex<-
#' @rdname paramValueIndex
setGeneric("paramValueIndex<-", function(x, value) standardGeneric("paramValueIndex<-"))


#' Method SearchStrategy
setGeneric("SearchStrategy", function(x) standardGeneric("SearchStrategy"))


#' Method SearchStrategy<-
#' @rdname SearchStrategy
setGeneric("SearchStrategy<-", function(x, value) standardGeneric("SearchStrategy<-"))

#' @rdname CaseMatch
setGeneric("CaseMatch", function(x) standardGeneric("CaseMatch"))


#' @rdname CaseMatch
setGeneric("CaseMatch<-", function(x, value) standardGeneric("CaseMatch<-"))

#' Method StopWords
setGeneric("StopWords", function(x) standardGeneric("StopWords"))


#' Method StopWords<-
#' @rdname StopWords
setGeneric("StopWords<-", function(x, value) standardGeneric("StopWords<-"))

#' @rdname OrderIndependentLookup
setGeneric("OrderIndependentLookup", function(x) standardGeneric("OrderIndependentLookup"))


#' @rdname OrderIndependentLookup
setGeneric("OrderIndependentLookup<-", function(x, value) standardGeneric("OrderIndependentLookup<-"))



#' Method FindAllMatches
setGeneric("FindAllMatches", function(x) standardGeneric("FindAllMatches"))


#' Method FindAllMatches<-
#' @rdname FindAllMatches
setGeneric("FindAllMatches<-", function(x, value) standardGeneric("FindAllMatches<-"))


#' Method SynonymType
setGeneric("SynonymType", function(x) standardGeneric("SynonymType"))


#' Method SynonymType<-
#' @rdname SynonymType
setGeneric("SynonymType<-", function(x, value) standardGeneric("SynonymType<-"))


#' Method Stemmer
setGeneric("Stemmer", function(x) standardGeneric("Stemmer"))


#' Method Stemmer<-
#' @rdname Stemmer
setGeneric("Stemmer<-", function(x, value) standardGeneric("Stemmer<-"))





# ########################################### Generics for class EntityFinder
# ################################

#' Method typeSystemRef
#' @name typeSystemRef
setGeneric(name = "typeSystemRef", def = function(x) standardGeneric("typeSystemRef"))

#' Method typeSystemRef<-
#' @rdname typeSystemRef
setGeneric(name = "typeSystemRef<-", def = function(x, value) standardGeneric("typeSystemRef<-"))

#' @name annotateDF
setGeneric(name = "annotateDF", def = function(object, descr_df, outDir = tempdir(), 
    configOpt, cmDict) {
    standardGeneric("annotateDF")
})


#' Method findEntities
#' @name findEntities
setGeneric(name = "findEntities", def = function(object, inputDirOrFile, multipleDocs = FALSE, 
    outDir = tempdir(), configOpt, cmDict) {
    standardGeneric("findEntities")
})

# ########################################### Generics for class Similarity
# #######################################


#' @rdname similarityInstance
setGeneric(name = "similarityInstance", def = function(object) standardGeneric("similarityInstance"))

#' @rdname similarityInstance
setGeneric(name = "similarityInstance<-", def = function(object, value) {
    standardGeneric("similarityInstance<-")
})

#' @rdname icConfig
setGeneric(name = "icConfig", def = function(object) standardGeneric("icConfig"))

#' @rdname icConfig
setGeneric(name = "icConfig<-", def = function(object, value) {
    standardGeneric("icConfig<-")
})

#' @rdname pairwiseConfig
setGeneric(name = "pairwiseConfig", def = function(object) standardGeneric("pairwiseConfig"))


#' @rdname pairwiseConfig
setGeneric(name = "pairwiseConfig<-", def = function(object, value) {
    standardGeneric("pairwiseConfig<-")
})

#' @rdname pairwiseConfigRef
setGeneric(name = "pairwiseConfigRef", def = function(object) standardGeneric("pairwiseConfigRef"))

#' @rdname pairwiseConfigRef
setGeneric(name = "pairwiseConfigRef<-", def = function(object, value) {
    standardGeneric("pairwiseConfigRef<-")
})

#' @rdname groupwiseConfigRef
setGeneric(name = "groupwiseConfigRef", def = function(object) standardGeneric("groupwiseConfigRef"))


#' @rdname groupwiseConfigRef
setGeneric(name = "groupwiseConfigRef<-", def = function(object, value) {
    standardGeneric("groupwiseConfigRef<-")
})

#' @rdname groupConfig
setGeneric(name = "groupConfig", def = function(object) {
    standardGeneric("groupConfig")
})

#' @rdname groupConfig
setGeneric(name = "groupConfig<-", def = function(object, value) {
    standardGeneric("groupConfig<-")
})



#' @rdname ontology
setGeneric(name = "ontology", def = function(object) {
    standardGeneric("ontology")
})


#' @rdname ontology
setGeneric(name = "ontology<-", def = function(object, value) {
    standardGeneric("ontology<-")
})



#' @rdname pairsim
setGeneric(name = "pairsim", def = function(object, term1, term2) {
    standardGeneric("pairsim")
})

#' @rdname groupsim
setGeneric(name = "groupsim", def = function(object, termList1, termList2) {
    standardGeneric("groupsim")
})

#' @rdname samplesim
setGeneric(name = "samplesim", def = function(object, sample1, sample2, annotated_df) {
    standardGeneric("samplesim")
})

#' @rdname multisim
setGeneric(name = "multisim", def = function(similarities, annotations, sample1, 
    sample2, aggregating_function) {
    standardGeneric("multisim")
})

### Generics for the class Onassis ####

#' @rdname dictionary
setGeneric(name = "dictionary", def = function(object) {
    standardGeneric("dictionary")
})


#' @rdname dictionary
setGeneric(name = "dictionary<-", def = function(object, value) {
    standardGeneric("dictionary<-")
})

#' @rdname simil
setGeneric(name = "simil", def = function(object) {
    standardGeneric("simil")
})


#' @rdname simil
setGeneric(name = "simil<-", def = function(object, value) {
    standardGeneric("simil<-")
})


#' @rdname entities
setGeneric(name = "entities", def = function(object) {
    standardGeneric("entities")
})


#' @rdname entities
setGeneric(name = "entities<-", def = function(object, value) {
    standardGeneric("entities<-")
})


#' @rdname scores
setGeneric(name = "scores", def = function(object) {
    standardGeneric("scores")
})


#' @rdname scores
setGeneric(name = "scores<-", def = function(object, value) {
    standardGeneric("scores<-")
})


#' @rdname annotate
#' @param ... Optional parameters
setGeneric(name = "annotate", signature = c("input", "dictType", "dictionary"), def = function(input = NA, 
    dictType = NA, dictionary = NA, ...) standardGeneric("annotate"))


#' @rdname sim
#' @param ... Optional parameters
setGeneric(name = "sim", signature = c("onassis"), def = function(onassis = NA, ...) standardGeneric("sim"))


#' @rdname collapse
setGeneric(name = "collapse", signature = c("onassis"), def = function(onassis = NA, 
    simil_thresh) standardGeneric("collapse"))



#' @rdname mergeonassis
setGeneric(name = "mergeonassis", signature = c("onassis1", "onassis2"), def = function(onassis1 = NA, 
    onassis2 = NA) standardGeneric("mergeonassis"))

#' @rdname compare
#' @param ... Optional parameters
setGeneric(name = "compare", signature = c("onassis"), def = function(onassis, ...) standardGeneric("compare"))




#' @rdname filterconcepts
setGeneric(name = "filterconcepts", signature = c("onassis"), def = function(onassis, 
    concepts_to_filter) standardGeneric("filterconcepts"))
