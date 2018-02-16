############################################### Generics for class dictBuilder
############################################### ##################################

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



# ##############################################
# Generics for class CMoptions
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


setGeneric("CaseMatch", function(x) standardGeneric("CaseMatch"))


#' @rdname CaseMatch
setGeneric("CaseMatch<-", function(x, value) standardGeneric("CaseMatch<-"))

#' Method StopWords
setGeneric("StopWords", function(x) standardGeneric("StopWords"))


#' Method StopWords<-
#' @rdname StopWords
setGeneric("StopWords<-", function(x, value) standardGeneric("StopWords<-"))


#' Method OrderIndependentLookup
setGeneric("OrderIndependentLookup", function(x) standardGeneric("OrderIndependentLookup"))


#' Method OrderIndependentLookup<-
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





# ###########################################
# Generics for class EntityFinder
# ################################

#' Method typeSystemRef
#' @name typeSystemRef
#' @rdname EntityFinder-class
#' @exportMethod typeSystemRef
setGeneric(name = "typeSystemRef", def = function(x) standardGeneric("typeSystemRef"))

#' Method typeSystemRef<-
#' @name typeSystemRef<-
#' @rdname EntityFinder-class
#' @exportMethod typeSystemRef<-
setGeneric(name = "typeSystemRef<-", def = function(x,
    value) standardGeneric("typeSystemRef<-"))

#' Method annotateDF
#' @name annotateDF
#' @rdname EntityFinder-class
#' @exportMethod  annotateDF
setGeneric(name = "annotateDF", def = function(object,
    descr_df, outDir = tempdir(), configOpt, cmDict) {
    standardGeneric("annotateDF")
})


#' Method findEntities
#' @name findEntities
#' @rdname EntityFinder-class
#' @exportMethod  findEntities
setGeneric(name = "findEntities", def = function(object,
    inputDirOrFile, multipleDocs = FALSE, outDir = tempdir(),
    configOpt, cmDict) {
    standardGeneric("findEntities")
})



# ###########################################
# GenericS for class Similarity
# #######################################


#' Method similarityInstance
#' @name similarityInstance
#' @rdname Similarity-class
#' @exportMethod similarityInstance
setGeneric(name = "similarityInstance", def = function(object) standardGeneric("similarityInstance"))

#' Method similarityInstance<-
#' @name similarityInstance<-
#' @rdname Similarity-class
#' @exportMethod similarityInstance<-
setGeneric(name = "similarityInstance<-", def = function(object,
    value) {
    standardGeneric("similarityInstance<-")
})

#' Method icConfig
#' @name icConfig
#' @rdname Similarity-class
#' @exportMethod icConfig
setGeneric(name = "icConfig", def = function(object) standardGeneric("icConfig"))

#' Method icConfig<-
#' @name icConfig<-
#' @rdname Similarity-class
#' @exportMethod icConfig<-
setGeneric(name = "icConfig<-", def = function(object,value) {
  standardGeneric("icConfig<-")
})

#' Method pairwiseConfig
#' @name pairwiseConfig
#' @rdname Similarity-class
#' @exportMethod pairwiseConfig
setGeneric(name = "pairwiseConfig", def = function(object) standardGeneric("pairwiseConfig"))


#' Method pairwiseConfig<-
#' @name pairwiseConfig<-
#' @rdname Similarity-class
#' @exportMethod pairwiseConfig<-
setGeneric(name = "pairwiseConfig<-", def = function(object,
                                                     value) {
  standardGeneric("pairwiseConfig<-")
})

#' Method pairwiseConfigRef
#' @name pairwiseConfigRef
#' @rdname Similarity-class
#' @exportMethod pairwiseConfigRef
setGeneric(name = "pairwiseConfigRef", def = function(object) standardGeneric("pairwiseConfigRef"))

#' Method pairwiseConfigRef<-
#' @name pairwiseConfigRef<-
#' @rdname Similarity-class
#' @exportMethod pairwiseConfigRef<-
setGeneric(name = "pairwiseConfigRef<-", def = function(object,
    value) {
    standardGeneric("pairwiseConfigRef<-")
})




#' Method groupwiseConfigRef
#' @name groupwiseConfigRef
#' @rdname Similarity-class
#' @exportMethod groupwiseConfigRef
setGeneric(name = "groupwiseConfigRef", def = function(object) standardGeneric("groupwiseConfigRef"))

#' Method groupwiseConfigRef<-
#' @name groupwiseConfigRef<-
#' @rdname Similarity-class
#' @exportMethod groupwiseConfigRef<-
setGeneric(name = "groupwiseConfigRef<-", def = function(object,
    value) {
    standardGeneric("groupwiseConfigRef<-")
})

#' Method groupConfig
#' @name groupConfig
#' @rdname Similarity-class
#' @exportMethod groupConfig
setGeneric(name = "groupConfig", def = function(object) {
    standardGeneric("groupConfig")
})

#' Method groupConfig<-
#' @name groupConfig<-
#' @rdname Similarity-class
#' @exportMethod groupConfig<-
setGeneric(name = "groupConfig<-", def = function(object,
    value) {
    standardGeneric("groupConfig<-")
})



#' Method ontology
#' @name ontology
#' @rdname Similarity-class
#' @exportMethod ontology
setGeneric(name = "ontology", def = function(object) {
    standardGeneric("ontology")
})


#' Method ontology<-
#' @name ontology<-
#' @rdname Similarity-class
#' @exportMethod ontology<-
setGeneric(name = "ontology<-", def = function(object,
    value) {
    standardGeneric("ontology<-")
})



#' Method showOpts
#' @name showOpts
#' @rdname Similarity-class
#' @exportMethod showOpts
setGeneric(name = "showOpts", def = function(object) {
    standardGeneric("showOpts")
})

#' Method sim
#' @name pairsim
#' @aliases pairsim, Similarity-method
#' @rdname pairsim
#' @exportMethod pairsim
setGeneric(name = "pairsim", def = function(object, term1,
    term2) {
    standardGeneric("pairsim")
})

#' Method groupsim
#' @name groupsim
#' @rdname Similarity-class
#' @exportMethod  groupsim
setGeneric(name = "groupsim", def = function(object,
    termList1, termList2) {
    standardGeneric("groupsim")
})

#' Method samplesim
#' @name samplesim
#' @rdname Similarity-class
#' @exportMethod samplesim
setGeneric(name = "samplesim", def = function(object,
    sample1, sample2, annotated_df) {
    standardGeneric("samplesim")
})

#' Method multisim
#' @name multisim
#' @rdname Similarity-class
#' @exportMethod multisim
setGeneric(name = "multisim", def = function(similarities,
    annotations, sample1, sample2, aggregating_function) {
    standardGeneric("multisim")
})

### Generics for the class Onassis ####

#' Method dictionary
#' @name dict
#' @rdname dict
#' @exportMethod dict
setGeneric(name = "dict", def = function(object) {
  standardGeneric("dict")
})


#' Method dict<-
#' @name dict<-
#' @rdname dict
#' @exportMethod dict<-
setGeneric(name = "dict<-", def = function(object,
                                               value) {
  standardGeneric("dict<-")
})

#' Method simil
#' @name simil
#' @rdname simil
#' @exportMethod simil
setGeneric(name = "simil", def = function(object) {
  standardGeneric("simil")
})


#' Method simil<-
#' @name simil<-
#' @rdname simil
#' @exportMethod simil<-
setGeneric(name = "simil<-", def = function(object,
                                                 value) {
  standardGeneric("simil<-")
})


#' Method entities
#' @name entities
#' @rdname entities
#' @exportMethod entities
setGeneric(name = "entities", def = function(object) {
  standardGeneric("entities")
})


#' Method entities<-
#' @name entities<-
#' @rdname entities
#' @exportMethod entities<-
setGeneric(name = "entities<-", def = function(object,
                                                 value) {
  standardGeneric("entities<-")
})


#' Method scores
#' @name scores
#' @rdname scores
#' @exportMethod scores
setGeneric(name = "scores", def = function(object) {
  standardGeneric("scores")
})


#' Method scores<-
#' @name scores<-
#' @rdname scores
#' @exportMethod scores<-
setGeneric(name = "scores<-", def = function(object,
                                               value) {
  standardGeneric("scores<-")
})

#' @name annot
#' @rdname annot
#' @exportMethod annot
setGeneric(name='annot', signature= c("input", "dictType", "dictionary"), def=function(input=NA, dictType=NA, dictionary=NA, ...) standardGeneric('annot')
)


#' @name sim
#' @rdname sim
#' @exportMethod sim
setGeneric(name='sim', signature= c("onassis"), def=function(onassis=NA, ...) standardGeneric('sim')
)


#' @name collapse
#' @rdname collapse
#' @exportMethod collapse
setGeneric(name='collapse', signature= c("onassis"), def=function(onassis=NA, simil_thresh) standardGeneric('collapse')
)



#' @name mergeonassis
#' @rdname mergeonassis
#' @exportMethod mergeonassis
setGeneric(name='mergeonassis', signature= c("onassis1", "onassis2"), def=function(onassis1=NA, onassis2=NA) standardGeneric('mergeonassis')
)

#' @name compare
#' @rdname compare
#' @exportMethod compare
setGeneric(name='compare', signature=c('onassis'), def=function(onassis, ...) standardGeneric('compare'))




#' @name filterconcepts
#' @rdname filterconcepts
#' @exportMethod filterconcepts
setGeneric(name='filterconcepts', signature=c('onassis'), def=function(onassis, concepts_to_filter) standardGeneric('filterconcepts'))
