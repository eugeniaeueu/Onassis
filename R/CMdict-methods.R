#' \code{dict_location}
#'
#' @description This method returns and sets the location of the dictionary.
#' @examples
#' dictionary <- CMdictionary()
#' loc <- dict_location(dictionary)
#' @rdname CMdictionary-class
#' @aliases dict_location, CMdictionary-method

setMethod(f = "dict_location", signature = "CMdictionary",
          definition = function(.Object) {
            validObject(.Object)
            return(.Object@dict_location)
          })


#' \code{dict_location<-}
#' @rdname CMdictionary-class
#' @aliases dict_location<-, CMdictionary-method
#' @return The location of the dictionary
#' @examples
#' dictionary <- CMdictionary()
#' dict_location(dictionary) <- getwd()
#'
setReplaceMethod("dict_location", "CMdictionary", function(.Object,
                                                           value) {
  if (file.exists(value)) {
    .Object@dict_location <- value
    .Object
  } else stop("Invalid location parameter")

})




#' \code{dictInfo}
#'
#' @rdname CMdictionary-class
#' @aliases dictInfo, CMdictionary-method
#' @return list of details about the dictionary
#' @description This method shows the list of details of the conceptmapper dictionary
#' @examples
#' dictionary <- CMdictionary()
#' dictInfo(dictionary)
setMethod("dictInfo", signature = "CMdictionary", definition = function(.Object) {
  info <- .Object@dictInfo
  return(info)
})


#' \code{dictInfo<-}
#'
#' @rdname CMdictionary-class
#' @aliases dictInfo<-, CMdictionary-method
#' @return the dictionary with updated dictInfo field
#' @description This method sets the list of details of the conceptmapper dictionary.
#' @param value is the list of metadata associated with the dictionary, for example the Dictionary source OBO file, the dicitonary type (OBO, ENTREZ, TARGET) or any other information that the user needs to store.
#' @examples
#' dictionary <- CMdictionary()
#' dictInfo(dictionary) <-
#' list(Dictionary_type =  'ENTREZ from OrgDb', Dictionary_source ='OrgDb')
setReplaceMethod("dictInfo", signature = "CMdictionary",
                 definition = function(.Object, value) {
                   .Object@dictInfo <- value
                   .Object
                 })



#' \code{dictRef}
#'
#' @rdname CMdictionary-class
#' @aliases dictRef, CMdictionary-method
#' @return java reference to the Conceptmapper dictionary
#' @description This method retrieves the java reference the conceptmapper dictionary
#' @examples
#' dictionary <- CMdictionary()
#' dictRef(dictionary)
setMethod("dictRef", signature = "CMdictionary", definition = function(.Object) {
  return(.Object@dictRef)
})


#' \code{dictRef<-}
#'
#' @rdname CMdictionary-class
#' @aliases dictRef<-, CMdictionary-method
#' @return list of details about the dictionary
#' @description This method retrieves the java reference to the conceptmapper file
#' @examples
#' dictionary <- CMdictionary()
#' dict_file <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' dictRef(dictionary) <- .jnew('java/io/File', dict_file)
setReplaceMethod("dictRef", signature = "CMdictionary",
                 definition = function(.Object, value) {
                   .Object@dictRef <- value
                   .Object
                 })








#' \code{buildDictionary}
#'
#' @description This method builds a dictionary for Conceptmapper.
#' @param outputDir the directory where the XML conceptmapper dictionary will be stored. Defaults to the tmp system's directory
#' @param dictType the type of input dictionary
#'\describe{
#'\item{OBO}{A dictionary that has been created by An OBO file}
#'\item{ENTREZ}{Entrez genes dictionary}
#'\item{TARGET}{Entrez genes dictionary, Histone marks and Histone modifications}
#'\item{CMDICT}{A previously created dictionary file in the Conceptmapper XML format}
#'}
#' @param synonymType The type of synonyms to consider when building the dictionary for Conceptmapper. For further detail \url{http://owlcollab.github.io/oboformat/doc/obo-syntax.html}. Default: EXACT
#'\describe{
#'\item{EXACT}{}
#'\item{ALL}{}
#'}
#' @param inputFileOrDb The local OBO/OWL ontology to be converted into an XML Conceptmapper dictionary. If NA is passed and the \code{dicType} parameter is not the generic OBO then the method tries to download the corresponding dictionary from the available repositories. For ENTREZ and TARGET dictionary types a file named gene_info.gz is automatically downloaded from \url{ftp://ncbi.nlm.nih.gov/gene/data/gene_info.gz} if not provided by the user. Alternatively an annotation package of the type Org.xx.eg.db from Bioconductor can be used. In this case the gene ids and their alternative identifiers will be retrieved from the annotation database without the need of downloading a gene_info file.
#' @param taxID the taxonomy identifier of the organism when the dictionary type is ENTREZ or TARGET. If 0 all the taxonomies will be included in the new dictionary.
#' @param .Object instance of class CMdictionary
#' @return An object of type CMdictionary that can be used to annotate text with the \code{EntityFinder}.
#' @examples
#' dictionary <- CMdictionary()
#' \dontrun{
#'#' ##This might take some time to download the dictionary
#' dict <- buildDictionary(dictionary, dictType = 'TARGET', inputFileOrDb='org.Hs.eg.db')
#'
#' dict_file <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' dictionary <- buildDictionary(dictionary, dictType='OBO', inputFileOrDb=dict_file)
#'}
#' @rdname CMdictionary-class
#' @aliases buildDictionary, CMdictionary-method
#' @importFrom  rJava J .jnew .jcast .jevalArray .jfield .jnull .jarray
#' @importFrom AnnotationDbi toTable
#' @importFrom RCurl url.exists
#' @export
setMethod(f = "buildDictionary", signature(.Object = "CMdictionary",
                                           outputDir = "character", dictType = "character",
                                           synonymType = "character", inputFileOrDb = "character",
                                           taxID = "numeric"), definition = function(.Object,
                                                                                     outputDir = tmpdir(), dictType = "OBO", synonymType = "EXACT",
                                                                                     inputFileOrDb = NULL, taxID = 0) {
                                             x <- .Object

                                             dict_location(x) <- outputDir
                                             validObject(x)

                                             outputDirOp = TRUE
                                             if (outputDirOp == TRUE)
                                               cleandir <- J("edu.ucdenver.ccp.common.file.FileUtil")$CleanDirectory$YES else cleandir <- J("edu.ucdenver.ccp.common.file.FileUtil")$CleanDirectory$NO

                                             outputdir <- .jnew("java/io/File", dict_location(x))


                                             switch(synonymType, EXACT = {
                                               synonym <- J("edu.ucdenver.ccp.datasource.fileparsers.obo.OntologyUtil")$SynonymType$EXACT
                                             }, RELATED = {
                                               synonym <- J("edu.ucdenver.ccp.datasource.fileparsers.obo.OntologyUtil")$SynonymType$RELATED
                                             }, NARROW = {
                                               synonym <- J("edu.ucdenver.ccp.datasource.fileparsers.obo.OntologyUtil")$SynonymType$NARROW
                                             }, BROAD = {
                                               synonym <- J("edu.ucdenver.ccp.datasource.fileparsers.obo.OntologyUtil")$SynonymType$BROAD
                                             }, ALL = {
                                               synonym <- J("edu.ucdenver.ccp.datasource.fileparsers.obo.OntologyUtil")$SynonymType$ALL
                                             })

                                             switch(dictType, ENTREZ = {
                                               if (grepl("^org.*eg.db$", inputFileOrDb)) {
                                                 # Building the dictionary from OrgDb database
                                                 # Building the dictionary from OrgDb database
                                                 orgdb <- loadNamespace(inputFileOrDb)
                                                 gene_symbols_table <- sub("eg.db", "egSYMBOL",
                                                                           inputFileOrDb)
                                                 gene_symbols <- orgdb[[gene_symbols_table]]
                                                 gene_symbols <- AnnotationDbi::toTable(gene_symbols)
                                                 short_symbols <- sapply(gene_symbols[,
                                                                                      2], nchar)
                                                 gene_symbols <- gene_symbols[short_symbols >
                                                                                2, ]
                                                 gene_synonyms <- NA
                                                 dictInfo(x)["synonymtype"] <- "ENTREZ GENE IDENTIFIERS"
                                                 if (synonymType != "EXACT") {
                                                   gene_synonyms_table <- sub("eg.db",
                                                                              "egALIAS2EG", inputFileOrDb)
                                                   gene_synonyms <- toTable(orgdb[[gene_synonyms_table]])
                                                   short_synonyms <- sapply(gene_synonyms[,
                                                                                          2], nchar)
                                                   gene_synonyms <- gene_synonyms[short_symbols >
                                                                                    2, ]
                                                   dictInfo(x)["synonymtype"] <- "ENTREZ GENE ALTERNATIVE IDENTIFIERS"
                                                 }  #End if synonymtype != EXACT
                                                 dictionary <- prepareEntrezDictionary(gene_symbols,
                                                                                       gene_synonyms)
                                                 dictInfo(x)["Dictionary_type"] <- "ENTREZ from OrgDb"
                                                 dictInfo(x)["Dictionary_source"] <- "OrgDb"
                                                 dictInfo(x)["synonymtype"] <- synonymType
                                                 entrez_genes_dictionary_file <- file(file.path(outputDir,
                                                                                                "cmDict-EntrezGene.xml"))
                                                 writeLines(dictionary, entrez_genes_dictionary_file)
                                                 close(entrez_genes_dictionary_file)
                                                 dict_location(x) <- file.path(outputDir,
                                                                               "cmDict-EntrezGene.xml")
                                               }  #End If dictype is ENTREZ and inputFileOrDb is OrgDb
                                               else {
                                                 # class(inputFileOrDb) is not OrgDb
                                                 entrez_genes_dictionary_factory <- J("edu.ucdenver.ccp.nlp.wrapper.conceptmapper.dictionary.eg.EntrezGeneDictionaryFactory")
                                                 dictInfo(x)["synonymtype"] <- "GENE ALTERNATIVE IDENTIFIERS"
                                                 dictInfo(x)["Dictionary_type"] <- "ENTREZ"
                                                 if (is.na(inputFileOrDb)) {
                                                   if (taxID == 0) {
                                                     message("Downloading gene_info file and building the dictionary. This process might take a while")
                                                     entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory,
                                                                                       returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary",
                                                                                       outputdir, cleandir)
                                                     dictInfo(x)["Dictionary_source"] <- "Downloaded gene_info.gz"
                                                     message("Dictionary created in ",
                                                             outputDir)
                                                   } else {
                                                     dictInfo(x)["taxid"] <- taxID
                                                     taxIDinteger <- .jnew("java/lang/Integer",
                                                                           as.integer(taxID))
                                                     taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID",
                                                                             taxIDinteger)
                                                     tax_id_vector <- .jnew("java/util/Vector")
                                                     tax_id_vector$add(taxIDobjectRef)
                                                     tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)
                                                     message("Downloading gene_info file and building the dictionary. This process might take a while")
                                                     entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory,
                                                                                       returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary",
                                                                                       outputdir, cleandir, .jcast(tax_id_set,
                                                                                                                   "java/util/Set"))
                                                     message("Dictionary created in ",
                                                             outputDir)
                                                     dict_location(x) <- file.path(outputDir, 'cmDict-EntrezGene.xml')
                                                   }
                                                 }  #End if inputFileOrDb is na
                                                 else {
                                                   if (!file.exists(inputFileOrDb)) stop(message(inputFileOrDb,
                                                                                                 " not found"))
                                                   gene_info_file <- .jnew("java/io/File",
                                                                           inputFileOrDb)
                                                   dictInfo(x)["Dictionary_source"] <- "Already existing gene_info.gz file"
                                                   if (taxID == 0) {
                                                     message("Creating the gene dictionary. This process might take a while.")
                                                     entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory,
                                                                                       returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary",
                                                                                       gene_info_file, outputdir, .jcall("java/lang/Boolean",
                                                                                                                         "Z", "parseBoolean", tolower(outputDirOp)))
                                                     message("Dictionary created in ",
                                                             outputDir)
                                                   } else {
                                                     dictInfo(x)["taxid"] <- taxID
                                                     taxIDinteger <- .jnew("java/lang/Integer",
                                                                           as.integer(taxID))
                                                     taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID",
                                                                             taxIDinteger)
                                                     tax_id_vector <- .jnew("java/util/Vector")
                                                     tax_id_vector$add(taxIDobjectRef)
                                                     tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)
                                                     entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory,
                                                                                       returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary",
                                                                                       gene_info_file, outputdir, .jcast(tax_id_set,
                                                                                                                         "java/util/Set"), .jcall("java/lang/Boolean",
                                                                                                                                                  "Z", "parseBoolean", tolower(outputDirOp)))
                                                     message("Dictionary created in ",
                                                             outputDir)
                                                     dict_location(x) <- file.path(outputDir, 'cmDict-EntrezGene.xml')
                                                   }  #End else (taxID!=0)
                                                 }  #End else (inputFileOrDb is not na)
                                               }  #End else inputDirOrFile is orgDb
                                               dictRef(x) <- .jnew("java/io/File", file.path(outputDir,
                                                                                             "cmDict-EntrezGene.xml"))
                                               dict_location(x) <- dirname(file.path(file.path(outputDir,
                                                                                               "cmDict-EntrezGene.xml")))
                                             }, OBO = {
                                               dictInfo(x)$Dictionary_type <- "OBO"
                                               dict_location(x) <- outputDir

                                               if(url.exists(inputFileOrDb)){
                                                 destination <- basename(inputFileOrDb)
                                                 obofile = download.file(inputFileOrDb, destfile = file.path(outputDir, destination))
                                                 if(obofile==0)
                                                   oboInputFile <- file.path(outputDir, destination)
                                                 dictInfo(x)["Dictionary_source"] <- inputFileOrDb
                                                 oboInputFile <- .jnew("java/io/File", oboInputFile)
                                               } else {


                                               
                                               oboInputFile <- .jnew("java/io/File", inputFileOrDb)
                                               }

                                               dictInfo(x)["Synonym_type"] <- synonymType

                                               outputFile <- paste0("cmDict-", gsub("[.][^.]+$",
                                                                                    "", basename(inputFileOrDb)), ".xml")
                                               outputFile_ref <- .jnew("java/io/File", file.path(outputDir,
                                                                                                 outputFile))

                                               ontology_util_ref <- .jnew("edu/ucdenver/ccp/datasource/fileparsers/obo/OntologyUtil",
                                                                          oboInputFile)
                                               obo_cm_dictionary <- .jcall(J("edu/ucdenver/ccp/nlp/wrapper/conceptmapper/dictionary/obo/OboToDictionary"),
                                                                           returnSig = "V", method = "buildDictionary",
                                                                           outputFile_ref, ontology_util_ref, .jnull("java/util/Set"),
                                                                           synonym)
                                               dictRef(x) <- outputFile_ref
                                               dict_location(x) <- file.path(outputDir,
                                                                             outputFile)

                                             }, TARGET = {
                                               histones_f <- system.file("extdata", "HistonesAndVariants.txt",
                                                                         package = "Onassis")
                                               histones_file_content <- read.table(histones_f,
                                                                                   header = FALSE, sep = "\t")

                                               if (grepl("^org.*eg.db$", inputFileOrDb)) {
                                                 # Building the dictionary from OrgDb database
                                                 orgdb <- loadNamespace(inputFileOrDb)
                                                 gene_symbols_table <- sub("eg.db", "egSYMBOL",
                                                                           inputFileOrDb)
                                                 gene_symbols <- orgdb[[gene_symbols_table]]
                                                 gene_symbols <- AnnotationDbi::toTable(gene_symbols)
                                                 short_symbols <- sapply(gene_symbols[,
                                                                                      2], nchar)
                                                 gene_symbols <- gene_symbols[short_symbols >
                                                                                2, ]
                                                 gene_synonyms <- NA
                                                 dictInfo(x)["synonymtype"] <- "ENTREZ GENE IDENTIFIERS"
                                                 if (synonymType != "EXACT") {
                                                   gene_synonyms_table <- sub("eg.db",
                                                                              "egALIAS2EG", inputFileOrDb)
                                                   gene_synonyms <- toTable(orgdb[[gene_synonyms_table]])
                                                   short_synonyms <- sapply(gene_synonyms[,
                                                                                          2], nchar)
                                                   gene_synonyms <- gene_synonyms[short_symbols >
                                                                                    2, ]
                                                   dictInfo(x)["synonymtype"] <- "GENE ALTERNATIVE IDENTIFIERS AND HISTONE MODIFICATIONS"
                                                 }  #End if synonymtype != EXACT
                                                 dictionary <- prepareEntrezDictionary(gene_symbols,
                                                                                       gene_synonyms)
                                                 dictInfo(x)["Dictionary_type"] <- "TARGET: ENTREZ from OrgDb and Histone modifications"
                                                 entrez_genes_dictionary_file <- file(file.path(outputDir,
                                                                                                "cmDict-EntrezGene.xml"))
                                                 writeLines(dictionary, entrez_genes_dictionary_file)
                                                 c <- close(entrez_genes_dictionary_file)
                                               } else {
                                                 entrez_genes_dictionary_factory <- J("edu.ucdenver.ccp.nlp.wrapper.conceptmapper.dictionary.eg.EntrezGeneDictionaryFactory")
                                                 dictInfo(x)["synonymtype"] <- "GENE ALTERNATIVE IDENTIFIERS AND HISTONE MODIFICATIONS"
                                                 dictInfo(x)["Dictionary_type"] <- "TARGET"
                                                 if (is.na(inputFileOrDb)) {
                                                   if (taxID == 0) {
                                                     message("Downloading gene_info file and building the dictionary. This process might take a while")
                                                     entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory,
                                                                                       returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary",
                                                                                       outputdir, cleandir)
                                                     dictInfo(x)["Dictionary_source"] <- "Downloaded gene_info.gz and histone modifications list"
                                                   } else {
                                                     dictInfo(x)["taxid"] <- taxID
                                                     taxIDinteger <- .jnew("java/lang/Integer",
                                                                           as.integer(taxID))
                                                     taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID",
                                                                             taxIDinteger)
                                                     tax_id_vector <- .jnew("java/util/Vector")
                                                     tax_id_vector$add(taxIDobjectRef)
                                                     tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)
                                                     message("Downloading gene_info file and building the dictionary. This process might take a while")
                                                     entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory,
                                                                                       returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary",
                                                                                       outputdir, cleandir, .jcast(tax_id_set,
                                                                                                                   "java/util/Set"))
                                                   }
                                                 } else {
                                                   gene_info_file <- .jnew("java/io/File",
                                                                           inputFileOrDb)
                                                   dictInfo(x)["Dictionary_source"] <- "Already existing gene_info.gz file and histone modifications list"
                                                   if (taxID == 0) {
                                                     message("Creating the gene dictionary. This process might take a while.")
                                                     entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory,
                                                                                       returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary",
                                                                                       gene_info_file, outputdir, .jcall("java/lang/Boolean",
                                                                                                                         "Z", "parseBoolean", tolower(outputDirOp)))
                                                     message("Dictionary created in ",
                                                             outputDir)
                                                   } else {
                                                     dictInfo(x)["taxid"] <- taxID
                                                     taxIDinteger <- .jnew("java/lang/Integer",
                                                                           as.integer(taxID))
                                                     taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID",
                                                                             taxIDinteger)
                                                     tax_id_vector <- .jnew("java/util/Vector")
                                                     tax_id_vector$add(taxIDobjectRef)
                                                     tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)
                                                     entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory,
                                                                                       returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary",
                                                                                       gene_info_file, outputdir, .jcast(tax_id_set,
                                                                                                                         "java/util/Set"), .jcall("java/lang/Boolean",
                                                                                                                                                  "Z", "parseBoolean", tolower(outputDirOp)))
                                                   }  #end if taxID==0
                                                 }  #end if is.na(inputFileOrDb)
                                               }  # end if class(inputFileOrDb==OrgDB)
                                               # Once the dictionary file with the proper GENE ids
                                               # has been created we want to add to it the histone
                                               # modifications and histone marks
                                               entrez_to_modify <- list.files(outputDir, pattern = "cmDict-EntrezGene\\.xml$",
                                                                              full.names = TRUE)
                                               con <- file(entrez_to_modify)
                                               entrez_file_lines <- readLines(con, warn = FALSE)
                                               n <- length(entrez_file_lines) - 1
                                               entrez_file_lines <- entrez_file_lines[1:n]
                                               lines_to_add <- sapply(histones_file_content,
                                                                      function(histone) {
                                                                        modified_histone <- gsub("Histone",
                                                                                                 "", histone)
                                                                        token_to_append <- paste0("<token id=\"",
                                                                                                  modified_histone, "\"", " canonical=\"",
                                                                                                  modified_histone, "\">")
                                                                        variant_to_append <- paste0("<variant base=\"",
                                                                                                    histone, "\"", "/>")
                                                                        end_of_token <- "</token>\n"
                                                                        final_token <- paste(token_to_append,
                                                                                             variant_to_append, end_of_token,
                                                                                             sep = "\n")
                                                                      })
                                               final_elem <- "</synonym>"
                                               c <- close(con)
                                               fileConn <- file(file.path(outputDir, "cmDict-Target.xml"))
                                               all_file_lines <- c(entrez_file_lines, lines_to_add,
                                                                   final_elem)
                                               writeLines(all_file_lines, fileConn)
                                               c <- close(fileConn)
                                               if (file.exists(file.path(outputDir, "cmDict-EntrezGene.xml"))) file.remove(file.path(outputDir,
                                                                                                                                     "cmDict-EntrezGene.xml"))
                                               dictRef(x) <- .jnew("java/io/File", file.path(outputDir,
                                                                                             "cmDict-Target.xml"))
                                               dict_location(x) <- dirname(file.path(outputDir,
                                                                                     "cmDict-Target.xml"))
                                             }, CMDICT = {
                                               dictRef(x) <- .jnew("java/io/File", file.path(inputFileOrDb))
                                               dict_location(x) <- dirname(inputFileOrDb)

                                               dictInfo(x)["Dictionary_source"] <- "Available conceptmapper dictionary"
                                               dictInfo(x)["Dictionary_type"] <- "CMDICT"
                                             })
                                             jgc()

                                             message("Dictionary available at: ", dict_location(x))
                                             return(x)
                                           })



#' \code{dictTypes}
#'
#' @rdname CMdictionary-class
#' @aliases dictTypes, CMdictionary-method
#' @return the list of dictionary types available
#' @description This method shows a lis of the pre-defined conceptmapper dictionary types
#' @examples
#' dictionary <- CMdictionary()
#' dictTypes(dictionary)
setMethod("dictTypes", signature = "CMdictionary",
          definition = function(.Object) {
            LABEL <- c("ENTREZ", "OBO", "TARGET", "CMDICT")
            DESCRIPTION <- c("Entrez genes dictionary",
                             "OBO Ontologies in OBO or RFD format. It is the default dictionary type",
                             "Entrez genes dictionary with Histone marks and Histone modifications",
                             "Dictionary in the Conceptmapper format")
            return(as.data.frame(cbind(LABEL, DESCRIPTION)))
          })
