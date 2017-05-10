############################################### Generics for class dictBuilder ##################################

#' Method dictTypes
#' @name dictTypes
#' @rdname CMdictionary-class
#' @exportMethod dictTypes
setGeneric("dictTypes", function(.Object) standardGeneric("dictTypes"))


#' Method dictInfo
#' @name dictInfo
#' @rdname CMdictionary-class
#' @exportMethod dictInfo
setGeneric("dictInfo", function(.Object) standardGeneric("dictInfo"))

#' Method buildDictionary
#' @name buildDictionary
#' @rdname CMdictionary-class
#' @exportMethod dictInfo
setGeneric("buildDictionary", function(.Object, outputDir = tempdir(), dictType = "OBO", synonymType = "EXACT", inputFile = NA_character_,
    taxID = 0) standardGeneric("buildDictionary"))


# ############################################## Generics for class CMoptions ###################################

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

#' Method CMargs
#' @name CMargs
#' @rdname CMoptions-class
#' @exportMethod CMargs
setGeneric("CMargs", function(x) standardGeneric("CMargs"))


#' Method CMargs<-
#' @name CMargs<-
#' @rdname CMoptions-class
#' @exportMethod CMargs<-
setGeneric("CMargs<-", function(x, value) standardGeneric("CMargs<-"))


# ########################################### Generics for class EntityFinder ################################

#' Method findEntities
#' @name findEntities
#' @rdname EntityFinder-class
#' @exportMethod  findEntities
setGeneric(name = "findEntities", def = function(object, inputDirOrFile, multipleDocs = FALSE, outDir = tempdir(), configOpt,
    cmDict) {
    standardGeneric("findEntities")
})


#' Method annotateDF
#' @name annotateDF
#' @rdname EntityFinder-class
#' @exportMethod  annotateDF
setGeneric(name = "annotateDF", def = function(object, descr_df, outDir = tempdir(), configOpt, cmDict) {
    standardGeneric("annotateDF")
})
# ########################################### GenericS for class Similarity #######################################


#' Method ontology<-
#' @name ontology<-
#' @rdname ontology
#' @exportMethod ontology<-

setGeneric(name = "ontology<-", def = function(object, value) {
    standardGeneric("ontology<-")
})

#' Method pairwiseConfig
#' @name pairwiseConfig
#' @rdname pairwiseConfig
#' @exportMethod pairwiseConfig
setGeneric(name = "pairwiseConfig", def = function(object) standardGeneric("pairwiseConfig"))

#' Method pairwiseConfig<-
#' @name pairwiseConfig<-
#' @rdname pairwiseConfig
#' @exportMethod pairwiseConfig<-
setGeneric(name = "pairwiseConfig<-", def = function(object, value) {
    standardGeneric("pairwiseConfig<-")
})


#' Method groupwiseConfig
#' @name groupwiseConfig
#' @rdname groupwiseConfig
#' @exportMethod groupwiseConfig
setGeneric(name = "groupwiseConfig", def = function(object) {
    standardGeneric("groupwiseConfig")
})

#' Method groupwiseConfig<-
#' @name groupwiseConfig<-
#' @rdname groupwiseConfig
#' @exportMethod groupwiseConfig<-
setGeneric(name = "groupwiseConfig<-", def = function(object, value) {
    standardGeneric("groupwiseConfig<-")
})

#' Method showOpts
#' @name showOpts
#' @rdname showOpts
#' @exportMethod showOpts
setGeneric(name = "showOpts", def = function(object) {
    standardGeneric("showOpts")
})

#' Method sim
#' @name sim
#' @rdname sim
#' @exportMethod sim
setGeneric(name = "sim", def = function(object, term1, term2) {
    standardGeneric("sim")
})

#' Method groupsim
#' @name groupsim
#' @rdname groupsim
#' @exportMethod  groupsim
setGeneric(name = "groupsim", def = function(object, termList1, termList2) {
    standardGeneric("groupsim")
})

#' Method samplesim
#' @name samplesim
#' @rdname samplesim
#' @exportMethod samplesim
setGeneric(name = "samplesim", def = function(object, sample1, sample2, annotated_df) {
    standardGeneric("samplesim")
})

#' Method multisim
#' @name multisim
#' @rdname multisim
#' @exportMethod multisim
setGeneric(name = "multisim", def = function(similarities, annotations, sample1, sample2, aggregating_function) {
    standardGeneric("multisim")
})
