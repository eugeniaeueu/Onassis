#' \code{dict_location}
#' @rdname dict_location
#' @description A method to get and set the location of  \code{\link{CMdictionary}}
#' @param .Object An instance of class CMdictionary
#' @return the path of the dictionary in the file system
#' @examples
#' dictionary <- new('CMdictionary')
#' loc <- dict_location(dictionary)
#' @exportMethod dict_location
setMethod(f = "dict_location", signature = "CMdictionary", definition = function(.Object) {
    validObject(.Object)
    return(.Object@dict_location)
})


#' \code{dict_location<-}
#' @rdname dict_location
#' @param value the path of the new dictionary
#' @examples
#' dictionary <- new('CMdictionary')
#' dict_location(dictionary) <- getwd()
#' @exportMethod dict_location<-
setReplaceMethod("dict_location", "CMdictionary", function(.Object, value) {
    if (file.exists(value)) {
        .Object@dict_location <- value
        .Object
    } else stop("Invalid location parameter")
    
})




#' \code{dictInfo}
#' @rdname dictInfo
#' @param .Object An instance of class \code{\link{CMdictionary}}
#' @description Method to get and set a list of info about the dictionary
#' @return list of details about the dictionary
#' @examples
#' dictionary <- new('CMdictionary')
#' dictInfo(dictionary)
#' @exportMethod dictInfo
setMethod("dictInfo", signature = "CMdictionary", definition = function(.Object) {
    info <- .Object@dictInfo
    return(info)
})


#' \code{dictInfo<-}
#' @rdname dictInfo
#' @param value list of details about the dictionary
#' @return object of class \code{\link{CMdictionary}}
#' @examples
#' dictionary <- new('CMdictionary')
#' dictInfo(dictionary) <-
#' list(Dictionary_type =  'ENTREZ from OrgDb', Dictionary_source ='OrgDb')
#' @exportMethod dictInfo<-
setReplaceMethod("dictInfo", signature = "CMdictionary", definition = function(.Object, 
    value) {
    .Object@dictInfo <- value
    .Object
})



#' \code{dictRef}
#' @rdname dictRef
#' @return java reference to the Conceptmapper dictionary
#' @description This method retrieves and sets the java reference the conceptmapper dictionary
#' @param .Object An instance of class \code{\link{CMdictionary}}
#' @examples
#' dictionary <- new('CMdictionary')
#' dictRef(dictionary)
#' @exportMethod dictRef
setMethod("dictRef", signature = "CMdictionary", definition = function(.Object) {
    return(.Object@dictRef)
})


#' \code{dictRef<-}
#' @rdname dictRef
#' @param value the reference of a XML Conceptmapper dictionary file already created
#' @exportMethod dictRef<-
#' @examples
#' dictionary <- new('CMdictionary')
#' dict_file <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' dictRef(dictionary) <- .jnew('java/io/File', dict_file)
setReplaceMethod("dictRef", signature = "CMdictionary", definition = function(.Object, 
    value) {
    .Object@dictRef <- value
    .Object
})


#' \code{dictTypes}
#' @rdname dictTypes
#' @return list of types that can be used to create a Conceptmapper dictionary
#' @description A function to show the pre-defined Conceptmapper dictionary types wwith their descriptions
#' @examples
#' dictTypes()
#' @export
dictTypes <- function() {
    LABEL <- c("ENTREZ", "OBO", "TARGET", "CMDICT")
    DESCRIPTION <- c("Entrez genes dictionary", "OBO Ontologies in OBO or RFD format. It is the default dictionary type", 
        "Entrez genes dictionary with Histone marks and Histone modifications", "Dictionary in the Conceptmapper format")
    return(as.data.frame(cbind(LABEL, DESCRIPTION)))
}
