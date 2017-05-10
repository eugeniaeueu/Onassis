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
#' @param inputFile The local OBO/OWL ontology to be converted into an XML Conceptmapper dictionary. If NA is passed and the \code{dicType} parameter is not the generic OBO then the method tries to download the corresponding dictionary from the available repositories. For ENTREZ and TARGET dictionary types a file named gene_info.gz is automatically downloaded from \url{ftp://ncbi.nlm.nih.gov/gene/data/gene_info.gz} if not provided by the user.
#' @param taxID the taxonomy identifier of the organism when the dictionary type is ENTREZ or TARGET. If 0 all the taxonomies will be included in the new dictionary.
#' @param .Object instance of class CMdictionary
#' @return An object of type CMdictionary that can be used to annotate text with the \code{EntityFinder}.
#' @examples
#' dictionary <- new('CMdictionary')
#' \dontrun{
#'#' ##This might take some time to download the dictionary
#'
#' dict <- buildDictionary(dictionary, dictType = 'TARGET')
#'
#' dict_file <-  file.path(getwd(), 'BrendaTissue.obo')
#'  if(!file.exists(dict_file))
#'    download.file('https://sourceforge.net/projects/onassis/files/BrendaTissue.obo',
#'     destfile=file.path(getwd(), 'BrendaTissue.obo'))
#'
#' dictionary <- buildDictionary(dictionary, inputFile=dict_file)
#'}
#' @rdname CMdictionary-class
#' @aliases buildDictionary, CMdictionary-method
#' @importFrom  rJava J .jnew .jcast .jevalArray .jfield .jnull .jarray
setMethod(f = "buildDictionary", signature = "CMdictionary", definition = function(.Object, outputDir = tempdir(), dictType = "OBO",
    synonymType = "EXACT", inputFile = NA_character_, taxID = 0) {

    x <- .Object

    x@dict_location <- outputDir
    validObject(x)

    if (!is.na(inputFile) & !file.exists(inputFile))
        stop(paste0(inputFile, " not found"))
    outputDirOp = FALSE
    if (outputDirOp == FALSE)
        cleandir <- J("edu.ucdenver.ccp.common.file.FileUtil")$CleanDirectory$YES else cleandir <- J("edu.ucdenver.ccp.common.file.FileUtil")$CleanDirectory$NO

    outputdir <- .jnew("java/io/File", x@dict_location)




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

        entrez_genes_dictionary_factory <- J("edu.ucdenver.ccp.nlp.wrapper.conceptmapper.dictionary.eg.EntrezGeneDictionaryFactory")

        x@dictInfo[["synonymtype"]] <- "GENE ALTERNATIVE IDENTIFIERS"

        x@dictInfo[["Dictionary_type"]] <- "ENTREZ"

        if (is.na(inputFile)) {

            if (taxID == 0) {


                message("Downloading gene_info file and building the dictionary. This process might take a while")

                entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary",
                  outputdir, cleandir)

                x@dictInfo[["Dictionary_source"]] <- "Downloaded gene_info.gz"

                message(paste0("Dictionary created in ", outputDir))

            } else {
                x@dictInfo[["taxid"]] <- taxID

                taxIDinteger <- .jnew("java/lang/Integer", as.integer(taxID))

                taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID", taxIDinteger)

                tax_id_vector <- .jnew("java/util/Vector")

                tax_id_vector$add(taxIDobjectRef)

                tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)

                message("Downloading gene_info file and building the dictionary. This process might take a while")

                entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary",
                  outputdir, cleandir, .jcast(tax_id_set, "java/util/Set"))

                message(paste0("Dictionary created in ", outputDir))

            }

        } else {

            gene_info_file <- .jnew("java/io/File", inputFile)

            x@dictInfo[["Dictionary_source"]] <- "Already existing gene_info.gz file"

            if (taxID == 0) {

                message("Creating the gene dictionary. This process might take a while.")

                entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary",
                  gene_info_file, outputdir, .jcall("java/lang/Boolean", "Z", "parseBoolean", tolower(outputDirOp)))

                message(paste0("Dictionary created in ", outputDir))


            } else {

                x@dictInfo[["taxid"]] <- taxID

                taxIDinteger <- .jnew("java/lang/Integer", as.integer(taxID))

                taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID", taxIDinteger)

                tax_id_vector <- .jnew("java/util/Vector")

                tax_id_vector$add(taxIDobjectRef)

                tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)

                entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary",
                  gene_info_file, outputdir, .jcast(tax_id_set, "java/util/Set"), .jcall("java/lang/Boolean", "Z", "parseBoolean",
                    tolower(outputDirOp)))

                message(paste0("Dictionary created in ", outputDir))

            }
        }
        x@dictRef <- .jnew("java/io/File", file.path(outputdir, "cmDict-Entrez.xml"))
        x@dic_location <- file.path(file.path(outputdir, "cmDict-Entrez.xml"))
    }, OBO = {

        x@dictInfo[["Dictionary_type"]] <- "OBO"

        x@dict_location <- outputDir

        x@dictInfo[["Dictionary_source"]] <- inputFile

        x@dictInfo[["Synonym_type"]] <- synonymType

        oboInputFile <- .jnew("java/io/File", inputFile)

        outputFile <- paste0("cmDict-", gsub("[.][^.]+$", "", basename(inputFile)), ".xml")

        outputFile_ref <- .jnew("java/io/File", file.path(outputDir, outputFile))

        ontology_util_ref <- .jnew("edu/ucdenver/ccp/datasource/fileparsers/obo/OntologyUtil", oboInputFile)

        obo_cm_dictionary <- .jcall(J("edu/ucdenver/ccp/nlp/wrapper/conceptmapper/dictionary/obo/OboToDictionary"), returnSig = "V",
            method = "buildDictionary", outputFile_ref, ontology_util_ref, .jnull("java/util/Set"), synonym)
        x@dictRef <- outputFile_ref


    }, TARGET = {

        histones_f <- system.file("extdata", "HistonesAndVariants.txt", package = "Onassis")

        histones_file_content <- read.table(histones_f, header = FALSE, sep = "\t")

        entrez_genes_dictionary_factory <- J("edu.ucdenver.ccp.nlp.wrapper.conceptmapper.dictionary.eg.EntrezGeneDictionaryFactory")

        x@dictInfo[["synonymtype"]] <- "GENE ALTERNATIVE IDENTIFIERS AND HISTONE MODIFICATIONS"

        x@dictInfo[["Dictionary_type"]] <- "TARGET"

        if (is.na(inputFile)) {

            if (taxID == 0) {


                message("Downloading gene_info file and building the dictionary. This process might take a while")

                entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary",
                  outputdir, cleandir)

                x@dictInfo[["Dictionary_source"]] <- "Downloaded gene_info.gz and histone modifications list"


            } else {
                x@dictInfo[["taxid"]] <- taxID

                taxIDinteger <- .jnew("java/lang/Integer", as.integer(taxID))

                taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID", taxIDinteger)

                tax_id_vector <- .jnew("java/util/Vector")

                tax_id_vector$add(taxIDobjectRef)

                tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)

                message("Downloading gene_info file and building the dictionary. This process might take a while")

                entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary",
                  outputdir, cleandir, .jcast(tax_id_set, "java/util/Set"))

            }

        } else {

            gene_info_file <- .jnew("java/io/File", inputFile)

            x@dictInfo[["Dictionary_source"]] <- "Already existing gene_info.gz file and histone modifications list"

            if (taxID == 0) {

                message("Creating the gene dictionary. This process might take a while.")

                entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary",
                  gene_info_file, outputdir, .jcall("java/lang/Boolean", "Z", "parseBoolean", tolower(outputDirOp)))

                message(paste0("Dictionary created in ", outputDir))


            } else {

                x@dictInfo[["taxid"]] <- taxID

                taxIDinteger <- .jnew("java/lang/Integer", as.integer(taxID))

                taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID", taxIDinteger)

                tax_id_vector <- .jnew("java/util/Vector")

                tax_id_vector$add(taxIDobjectRef)

                tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)

                entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary",
                  gene_info_file, outputdir, .jcast(tax_id_set, "java/util/Set"), .jcall("java/lang/Boolean", "Z", "parseBoolean",
                    tolower(outputDirOp)))


            }
        }

        # Once the dictionary file with the proper GENE ids has been created we want to add to it the histone modifications and histone
        # marks

        entrez_to_modify <- list.files(outputDir, pattern = "cmDict-EntrezGene\\.xml$", full.names = TRUE)

        entrez_file_lines <- readLines(file(entrez_to_modify), warn = FALSE)

        n <- length(entrez_file_lines) - 1

        entrez_file_lines <- entrez_file_lines[1:n]

        lines_to_add <- sapply(histones_file_content, function(histone) {

            modified_histone <- gsub("Histone", "", histone)

            token_to_append <- paste0("<token id=\"", modified_histone, "\"", " canonical=\"", modified_histone, "\">")

            variant_to_append <- paste0("<variant base=\"", histone, "\"", "/>")

            end_of_token <- "</token>\n"

            final_token <- paste(token_to_append, variant_to_append, end_of_token, sep = "\n")

        })

        final_elem <- "</synonym>"

        fileConn <- file(file.path(outputDir, "cmDict-Target.xml"))

        all_file_lines <- c(entrez_file_lines, lines_to_add, final_elem)

        writeLines(all_file_lines, fileConn)

        close(fileConn)

        x@dictRef <- .jnew("java/io/File", file.path(outputDir, "cmDict-Target.xml"))




    }, CMDICT = {
        x@dictRef <- .jnew("java/io/File", file.path(inputFile))
        x@dict_location <- inputFile

        x@dictInfo[["Dictionary_source"]] <- "Available conceptmapper dictionary"
        x@dictInfo[["Dictionary_type"]] <- "CMDICT"
    })
    jgc()

    message(paste0("Dictionary available at: ", x@dict_location))
    return(x)
})



#' \code{dictTypes}
#'
#' @rdname CMdictionary-class
#' @aliases dictTypes, CMdictionary-method
#' @return the list of dictionary types available
#' @description This method shows a lis of the pre-defined conceptmapper dictionary types
#' @examples
#' dictionary <- new('CMdictionary')
#' dictTypes(dictionary)
setMethod("dictTypes", signature = "CMdictionary", definition = function(.Object) {
    LABEL <- c("ENTREZ", "OBO", "TARGET", "CMDICT")
    DESCRIPTION <- c("Entrez genes dictionary", "OBO Ontologies in OBO or RFD format. It is the default dictionary type", "Entrez genes dictionary with Histone marks and Histone modifications",
        "Dictionary in the Conceptmapper format")
    return(as.data.frame(cbind(LABEL, DESCRIPTION)))
})



#' \code{dictInfo}
#'
#' @rdname CMdictionary-class
#' @aliases dictInfo, CMdictionary-method
#' @return list of details about the dictionary
#' @description This method shows the list of details of the conceptmapper dictionary
#' @examples
#' dictionary <- new('CMdictionary')
#' dictInfo(dictionary)
setMethod("dictInfo", signature = "CMdictionary", definition = function(.Object) {
    return(.Object@dictInfo)
})

