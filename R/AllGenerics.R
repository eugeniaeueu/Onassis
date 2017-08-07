############################################### Generics for class dictBuilder
############################################### ##################################

#' Method dict_location
#' @name dict_location
#' @rdname CMdictionary-class
#' @exportMethod dict_location
setGeneric("dict_location", function(.Object) standardGeneric("dict_location"))


#' Method dict_location<-
#' @name dict_location<-
#' @rdname CMdictionary-class
#' @exportMethod dict_location<-
setGeneric("dict_location<-", function(.Object, value) standardGeneric("dict_location<-"))


#' Method dictInfo
#' @name dictInfo
#' @rdname CMdictionary-class
#' @exportMethod dictInfo
setGeneric("dictInfo", function(.Object) standardGeneric("dictInfo"))


#' Method dictInfo<-
#' @name dictInfo<-
#' @rdname CMdictionary-class
#' @exportMethod dictInfo<-
setGeneric("dictInfo<-", function(.Object, value) standardGeneric("dictInfo<-"))

#' Method dictRef
#' @name dictRef
#' @rdname CMdictionary-class
#' @exportMethod dictRef
setGeneric("dictRef", function(.Object) standardGeneric("dictRef"))


#' Method dictRef<-
#' @name dictRef<-
#' @rdname CMdictionary-class
#' @exportMethod dictRef<-
setGeneric("dictRef<-", function(.Object, value) standardGeneric("dictRef<-"))


#' Method dictTypes
#' @name dictTypes
#' @rdname CMdictionary-class
#' @exportMethod dictTypes
setGeneric("dictTypes", function(.Object) standardGeneric("dictTypes"))



#' Method buildDictionary
#' @name buildDictionary
#' @rdname CMdictionary-class
#' @exportMethod buildDictionary
setGeneric("buildDictionary", function(.Object, outputDir = tempdir(),
    dictType = "OBO", synonymType = "EXACT", inputFileOrDb = NULL,
    taxID = 0) standardGeneric("buildDictionary"))


# ##############################################
# Generics for class CMoptions
# ###################################


#' Method arguments
#' @name arguments
#' @rdname CMoptions-class
#' @exportMethod arguments
setGeneric("arguments", function(x) standardGeneric("arguments"))


#' Method arguments<-
#' @name arguments<-
#' @rdname CMoptions-class
#' @exportMethod arguments<-
setGeneric("arguments<-", function(x, value) standardGeneric("arguments<-"))

#' Method listCombinations
#' @name listCombinations
#' @rdname CMoptions-class
#' @exportMethod listCombinations
setGeneric("listCombinations", function(x) stanardGeneric("listCombinations"))


#' Method paramValueIndex
#' @name paramValueIndex
#' @rdname CMoptions-class
#' @exportMethod paramValueIndex
setGeneric("paramValueIndex", function(x) standardGeneric("paramValueIndex"))


#' Method paramValueIndex<-
#' @name paramValueIndex<-
#' @rdname CMoptions-class
#' @exportMethod paramValueIndex<-
setGeneric("paramValueIndex<-", function(x, value) standardGeneric("paramValueIndex<-"))



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
#' @name sim
#' @aliases sim, Similarity-method
#' @rdname Similarity-class
#' @exportMethod sim
setGeneric(name = "sim", def = function(object, term1,
    term2) {
    standardGeneric("sim")
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
