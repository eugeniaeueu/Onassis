#' \code{Onassis}
#'
#'
#'
#'
#' @description This constructor instantiates an Onassis object.
#' @return An object of type Onassis that can be used to analyze metadata
#' @examples
#'
#' onassis <- Onassis()
#'
#' @name Onassis
#' @rdname Onassis
#' @param dictionary The path of the dictionary file
#' @param entities a data frame to store entities
#' @param similarity A matrix of the similarities between entities
#' @param scores The result of comparisons of the elements in the entities
#' @export
#' @importFrom methods new validObject

Onassis <- function(dictionary = NA_character_, entities = data.frame(), similarity = matrix(), 
    scores = matrix()) {
    o <- new("Onassis")
    if (!is.na(dictionary)) 
        dictionary(o) <- dictionary
    if (is.data.frame(entities) & nrow(entities) > 0) 
        entities(o) <- entities
    if (is.matrix(similarity)) 
        simil(o) <- similarity
    if (is.matrix(scores)) 
        scores(o) <- scores
    return(o)
}


#' \code{CMdictionary}
#' @description Constructor method for creating instances of class \code{\link{CMdictionary-class}}. The created Conceptmapper dictionary will be stored as an XML file in the file system.
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
#' @param inputFileOrDb The local OBO/OWL ontology to be converted into an XML Conceptmapper dictionary or the URL of a OBO/OWL file. If inputFileOrdb is NA and the \code{dicType} parameter is not the generic OBO then the method tries to download the corresponding dictionary from the available repositories. For ENTREZ and TARGET dictionary types a file named gene_info.gz will be automatically downloaded from \url{ftp://ncbi.nlm.nih.gov/gene/data/gene_info.gz} if a valid path is not provided by the user. Alternatively  the name of an annotation package of the type Org.xx.eg.db from Bioconductor can be used. In this case the gene unique identifiers and their alternative identifiers will be retrieved from the annotation database without the need of downloading a gene_info file.
#' @param taxID the taxonomy identifier of the organism when the dictionary type is ENTREZ or TARGET. If 0 all the taxonomies will be included in the new dictionary.
#' @param outputDirOp set to TRUE to clean the directory before creating the dictionary
#' @return An object of type \code{\link{CMdictionary-class}} that can be used to annotate text with the \code{\link{EntityFinder}}.
#' @examples
#' \dontrun{
#'#' ##This might take some time to download the dictionary
#' dict <- CMdictionary(dictType = 'TARGET', inputFileOrDb='org.Hs.eg.db')
#'
#' dict_file <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' dictionary <- CMdictionary(dictType='OBO', inputFileOrDb=dict_file)
#'}
#' @importFrom  rJava J .jnew .jcast .jevalArray .jfield .jnull .jarray
#' @importFrom AnnotationDbi toTable
#' @importFrom RCurl url.exists
#' @importFrom methods new validObject
#' @importFrom AnnotationDbi toTable
#' @export

CMdictionary <- function(inputFileOrDb = NULL, dictType = "OBO", outputDir = tempdir(), 
    synonymType = "EXACT", taxID = 0, outputDirOp = TRUE) {
    x <- new("CMdictionary")
    dict_location(x) <- outputDir
    validObject(x)
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
            # Building the dictionary from OrgDb database Building the dictionary from OrgDb
            # database
            orgdb <- loadNamespace(inputFileOrDb)
            gene_symbols_table <- sub("eg.db", "egSYMBOL", inputFileOrDb)
            gene_symbols <- orgdb[[gene_symbols_table]]
            gene_symbols <- AnnotationDbi::toTable(gene_symbols)
            short_symbols <- sapply(gene_symbols[, 2], nchar)
            gene_symbols <- gene_symbols[short_symbols > 2, ]
            gene_synonyms <- NA
            dictInfo(x)["synonymtype"] <- "ENTREZ GENE IDENTIFIERS"
            if (synonymType != "EXACT") {
                gene_synonyms_table <- sub("eg.db", "egALIAS2EG", inputFileOrDb)
                gene_synonyms <- toTable(orgdb[[gene_synonyms_table]])
                short_synonyms <- sapply(gene_synonyms[, 2], nchar)
                gene_synonyms <- gene_synonyms[short_symbols > 2, ]
                dictInfo(x)["synonymtype"] <- "ENTREZ GENE ALTERNATIVE IDENTIFIERS"
            }  #End if synonymtype != EXACT
            dictionary <- prepareEntrezDictionary(gene_symbols, gene_synonyms)
            dictInfo(x)["Dictionary_type"] <- "ENTREZ from OrgDb"
            dictInfo(x)["Dictionary_source"] <- "OrgDb"
            dictInfo(x)["synonymtype"] <- synonymType
            entrez_genes_dictionary_file <- file(file.path(outputDir, "cmDict-EntrezGene.xml"))
            writeLines(dictionary, entrez_genes_dictionary_file)
            close(entrez_genes_dictionary_file)
            dict_location(x) <- file.path(outputDir, "cmDict-EntrezGene.xml")
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
                  message("Dictionary created in ", outputDir)
                } else {
                  dictInfo(x)["taxid"] <- taxID
                  taxIDinteger <- .jnew("java/lang/Integer", as.integer(taxID))
                  taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID", 
                    taxIDinteger)
                  tax_id_vector <- .jnew("java/util/Vector")
                  tax_id_vector$add(taxIDobjectRef)
                  tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)
                  message("Downloading gene_info file and building the dictionary. This process might take a while")
                  entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, 
                    returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary", 
                    outputdir, cleandir, .jcast(tax_id_set, "java/util/Set"))
                  message("Dictionary created in ", outputDir)
                  dict_location(x) <- file.path(outputDir, "cmDict-EntrezGene.xml")
                }
            }  #End if inputFileOrDb is na
 else {
                if (!file.exists(inputFileOrDb)) stop(message(inputFileOrDb, " not found"))
                gene_info_file <- .jnew("java/io/File", inputFileOrDb)
                dictInfo(x)["Dictionary_source"] <- "Already existing gene_info.gz file"
                if (taxID == 0) {
                  message("Creating the gene dictionary. This process might take a while.")
                  entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, 
                    returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary", 
                    gene_info_file, outputdir, .jcall("java/lang/Boolean", "Z", "parseBoolean", 
                      tolower(outputDirOp)))
                  message("Dictionary created in ", outputDir)
                } else {
                  dictInfo(x)["taxid"] <- taxID
                  taxIDinteger <- .jnew("java/lang/Integer", as.integer(taxID))
                  taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID", 
                    taxIDinteger)
                  tax_id_vector <- .jnew("java/util/Vector")
                  tax_id_vector$add(taxIDobjectRef)
                  tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)
                  entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, 
                    returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary", 
                    gene_info_file, outputdir, .jcast(tax_id_set, "java/util/Set"), 
                    .jcall("java/lang/Boolean", "Z", "parseBoolean", tolower(outputDirOp)))
                  message("Dictionary created in ", outputDir)
                  dict_location(x) <- file.path(outputDir, "cmDict-EntrezGene.xml")
                }  #End else (taxID!=0)
            }  #End else (inputFileOrDb is not na)
        }  #End else inputDirOrFile is orgDb
        dictRef(x) <- .jnew("java/io/File", file.path(outputDir, "cmDict-EntrezGene.xml"))
        dict_location(x) <- dirname(file.path(file.path(outputDir, "cmDict-EntrezGene.xml")))
    }, OBO = {
        dictInfo(x)$Dictionary_type <- "OBO"
        dict_location(x) <- outputDir
        
        if (url.exists(inputFileOrDb)) {
            destination <- basename(inputFileOrDb)
            obofile = download.file(inputFileOrDb, destfile = file.path(outputDir, 
                destination))
            if (obofile == 0) oboInputFile <- file.path(outputDir, destination)
            dictInfo(x)["Dictionary_source"] <- inputFileOrDb
            oboInputFile <- .jnew("java/io/File", oboInputFile)
        } else {
            
            
            
            oboInputFile <- .jnew("java/io/File", inputFileOrDb)
        }
        
        dictInfo(x)["Synonym_type"] <- synonymType
        
        outputFile <- paste0("cmDict-", gsub("[.][^.]+$", "", basename(inputFileOrDb)), 
            ".xml")
        outputFile_ref <- .jnew("java/io/File", file.path(outputDir, outputFile))
        
        ontology_util_ref <- .jnew("edu/ucdenver/ccp/datasource/fileparsers/obo/OntologyUtil", 
            oboInputFile)
        obo_cm_dictionary <- .jcall(J("edu/ucdenver/ccp/nlp/wrapper/conceptmapper/dictionary/obo/OboToDictionary"), 
            returnSig = "V", method = "buildDictionary", outputFile_ref, ontology_util_ref, 
            .jnull("java/util/Set"), synonym)
        dictRef(x) <- outputFile_ref
        dict_location(x) <- file.path(outputDir, outputFile)
        
    }, TARGET = {
        histones_f <- system.file("extdata", "HistonesAndVariants.txt", package = "Onassis")
        histones_file_content <- read.table(histones_f, header = FALSE, sep = "\t")
        
        if (grepl("^org.*eg.db$", inputFileOrDb)) {
            # Building the dictionary from OrgDb database
            orgdb <- loadNamespace(inputFileOrDb)
            gene_symbols_table <- sub("eg.db", "egSYMBOL", inputFileOrDb)
            gene_symbols <- orgdb[[gene_symbols_table]]
            gene_symbols <- AnnotationDbi::toTable(gene_symbols)
            short_symbols <- sapply(gene_symbols[, 2], nchar)
            gene_symbols <- gene_symbols[short_symbols > 2, ]
            gene_synonyms <- NA
            dictInfo(x)["synonymtype"] <- "ENTREZ GENE IDENTIFIERS"
            if (synonymType != "EXACT") {
                gene_synonyms_table <- sub("eg.db", "egALIAS2EG", inputFileOrDb)
                gene_synonyms <- toTable(orgdb[[gene_synonyms_table]])
                short_synonyms <- sapply(gene_synonyms[, 2], nchar)
                gene_synonyms <- gene_synonyms[short_symbols > 2, ]
                dictInfo(x)["synonymtype"] <- "GENE ALTERNATIVE IDENTIFIERS AND HISTONE MODIFICATIONS"
            }  #End if synonymtype != EXACT
            dictionary <- prepareEntrezDictionary(gene_symbols, gene_synonyms)
            dictInfo(x)["Dictionary_type"] <- "TARGET: ENTREZ from OrgDb and Histone modifications"
            entrez_genes_dictionary_file <- file(file.path(outputDir, "cmDict-EntrezGene.xml"))
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
                  taxIDinteger <- .jnew("java/lang/Integer", as.integer(taxID))
                  taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID", 
                    taxIDinteger)
                  tax_id_vector <- .jnew("java/util/Vector")
                  tax_id_vector$add(taxIDobjectRef)
                  tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)
                  message("Downloading gene_info file and building the dictionary. This process might take a while")
                  entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, 
                    returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary", 
                    outputdir, cleandir, .jcast(tax_id_set, "java/util/Set"))
                }
            } else {
                gene_info_file <- .jnew("java/io/File", inputFileOrDb)
                dictInfo(x)["Dictionary_source"] <- "Already existing gene_info.gz file and histone modifications list"
                if (taxID == 0) {
                  message("Creating the gene dictionary. This process might take a while.")
                  entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, 
                    returnSig = "Ljava/io/File;", method = "buildModelOrganismConceptMapperDictionary", 
                    gene_info_file, outputdir, .jcall("java/lang/Boolean", "Z", "parseBoolean", 
                      tolower(outputDirOp)))
                  message("Dictionary created in ", outputDir)
                } else {
                  dictInfo(x)["taxid"] <- taxID
                  taxIDinteger <- .jnew("java/lang/Integer", as.integer(taxID))
                  taxIDobjectRef <- .jnew("edu/ucdenver/ccp/datasource/identifiers/ncbi/taxonomy/NcbiTaxonomyID", 
                    taxIDinteger)
                  tax_id_vector <- .jnew("java/util/Vector")
                  tax_id_vector$add(taxIDobjectRef)
                  tax_id_set <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createSet(tax_id_vector)
                  entrez_genes_dictionary <- .jcall(entrez_genes_dictionary_factory, 
                    returnSig = "Ljava/io/File;", method = "buildConceptMapperDictionary", 
                    gene_info_file, outputdir, .jcast(tax_id_set, "java/util/Set"), 
                    .jcall("java/lang/Boolean", "Z", "parseBoolean", tolower(outputDirOp)))
                }  #end if taxID==0
            }  #end if is.na(inputFileOrDb)
        }  # end if class(inputFileOrDb==OrgDB)
        # Once the dictionary file with the proper GENE ids has been created we want to
        # add to it the histone modifications and histone marks
        entrez_to_modify <- list.files(outputDir, pattern = "cmDict-EntrezGene\\.xml$", 
            full.names = TRUE)
        con <- file(entrez_to_modify)
        entrez_file_lines <- readLines(con, warn = FALSE)
        n <- length(entrez_file_lines) - 1
        entrez_file_lines <- entrez_file_lines[1:n]
        lines_to_add <- sapply(histones_file_content, function(histone) {
            modified_histone <- gsub("Histone", "", histone)
            token_to_append <- paste0("<token id=\"", modified_histone, "\"", " canonical=\"", 
                modified_histone, "\">")
            variant_to_append <- paste0("<variant base=\"", histone, "\"", "/>")
            end_of_token <- "</token>\n"
            final_token <- paste(token_to_append, variant_to_append, end_of_token, 
                sep = "\n")
        })
        final_elem <- "</synonym>"
        c <- close(con)
        fileConn <- file(file.path(outputDir, "cmDict-Target.xml"))
        all_file_lines <- c(entrez_file_lines, lines_to_add, final_elem)
        writeLines(all_file_lines, fileConn)
        c <- close(fileConn)
        if (file.exists(file.path(outputDir, "cmDict-EntrezGene.xml"))) file.remove(file.path(outputDir, 
            "cmDict-EntrezGene.xml"))
        dictRef(x) <- .jnew("java/io/File", file.path(outputDir, "cmDict-Target.xml"))
        dict_location(x) <- dirname(file.path(outputDir, "cmDict-Target.xml"))
    }, CMDICT = {
        dictRef(x) <- .jnew("java/io/File", file.path(inputFileOrDb))
        dict_location(x) <- dirname(inputFileOrDb)
        
        dictInfo(x)["Dictionary_source"] <- "Available conceptmapper dictionary"
        dictInfo(x)["Dictionary_type"] <- "CMDICT"
    })
    jgc()
    
    message("Dictionary available at: ", dict_location(x))
    return(x)
}



#' \code{CMoptions}
#' @param paramValueIndex An integer value to index the 576 parameter combinations
#' @param SearchStrategy The matching strategy for finding concepts in the input text
#'\itemize{
#'\item{CONTIGUOUS_MATCH}{Longets match of contiguous tokens within enclosing span}
#'\item{SKIP_ANY_MATCH}{Longest match of not-necessarily contiguous tokens}
#'\item{SKIP_ANY_MATCH_ALLOW_OVERLAP}{Longest match of not-necessarily contiguous tokens, overlapping matches are allowed}
#'}
#' @param CaseMatch
#'\itemize{
#'\item{CASE_IGNORE}{Fold everything to lowercase for matching}
#'\item{CASE_INSENSITIVE}{Fold only tokens with initial caps to lowercase}
#'\item{CASE_FOLD_DIGITS}{Fold all (and only) tokens with a digit}
#'\item{CASE_SENSITIVE}{Perform no case folding}
#'}
#' @param Stemmer
#'\itemize{
#'\item BIOLEMMATIZER {A stemmer specific for biomedical literature}
#'\item PORTER {A stemmer that removes the commoner morphological and inflexional endings from words in English}
#'\item NONE {No word stemming}
#'}
#' @param StopWords
#'\itemize{
#'\item PUBMED {A list of stop words obtained analyzing Pubmed papers}
#'\item NONE {No stop words }
#'}
#'@param OrderIndependentLookup
#'\itemize{
#'\item ON {Ordering within span is ignored (i.e. 'Breast cancer' would equal 'Cancer breast') }
#'\item OFF {Ordering is taken into consideration}
#'}
#' @param FindAllMatches
#'\itemize{
#'\item YES {All the matches within the span are found }
#'\item NO {Only the longest match within the span will be returned}
#'}
#' @param SynonymType
#'\itemize{
#'\item EXACT_ONLY {Only exact synonyms are considered }
#'\item ALL {All synonym types are included}
#'}
#' @rdname CMoptions
#' @return instance of the class CMoptions set to the default combination of parameters
#' @description This constructor creates an object of type CMoptions
#' @examples
#' op <- CMoptions()
#' @export

CMoptions <- function(SearchStrategy = "CONTIGUOUS_MATCH", CaseMatch = "CASE_INSENSITIVE", 
    Stemmer = "NONE", StopWords = "NONE", OrderIndependentLookup = "ON", FindAllMatches = "YES", 
    SynonymType = "ALL", paramValueIndex = NA) {
    opts <- new("CMoptions")
    opts@SearchStrategy = SearchStrategy
    opts@CaseMatch = CaseMatch
    opts@Stemmer = Stemmer
    opts@StopWords = StopWords
    opts@OrderIndependentLookup = OrderIndependentLookup
    opts@FindAllMatches = FindAllMatches
    opts@SynonymType = SynonymType
    options_combinations <- readRDS(system.file("extdata", "Options_table.rds", package = "Onassis"))
    if (is.na(paramValueIndex)) {
        opts@paramValueIndex <- as.character(as.vector(unique(options_combinations$paramValueIndex[which(options_combinations$SearchStrategy == 
            SearchStrategy & options_combinations$CaseMatch == CaseMatch & options_combinations$Stemmer == 
            Stemmer & options_combinations$Stopwords == StopWords & options_combinations$OrderIndependentLookup == 
            OrderIndependentLookup & options_combinations$FindAllMatches == FindAllMatches & 
            options_combinations$SynonymType == SynonymType)])))
    } else {
        opts@paramValueIndex <- as.character(as.vector(paramValueIndex))
        opts@SearchStrategy = as.character(as.vector(unique(options_combinations$SearchStrategy[which(options_combinations$paramValueIndex == 
            paramValueIndex)])))
        opts@CaseMatch = as.character(as.vector(unique(options_combinations$CaseMatch[which(options_combinations$paramValueIndex == 
            paramValueIndex)])))
        opts@Stemmer = as.character(as.vector(unique(options_combinations$Stemmer[which(options_combinations$paramValueIndex == 
            paramValueIndex)])))
        opts@StopWords = as.character(as.vector(unique(options_combinations$Stopwords[which(options_combinations$paramValueIndex == 
            paramValueIndex)])))
        opts@OrderIndependentLookup = as.character(as.vector(unique(options_combinations$OrderIndependentLookup[which(options_combinations$paramValueIndex == 
            paramValueIndex)])))
        opts@FindAllMatches = as.character(as.vector(unique(options_combinations$FindAllMatches[which(options_combinations$paramValueIndex == 
            paramValueIndex)])))
        opts@SynonymType = as.character(as.vector(unique(options_combinations$SynonymType[which(options_combinations$paramValueIndex == 
            paramValueIndex)])))
        
    }
    validObject(opts)
    return(opts)
}


#' \code{EntityFinder}
#' @return dataframe of annotations
#' @description this function creates instances of the class \code{\link{EntityFinder}}
#' @param input the file, directory, or data frame with the text to annotate
#' @param dictionary A dictionary of the type \code{\link{CMdictionary}} or the path to an already created Conceptmapper XML file
#' @param options an object of class \code{\link{CMoptions}}. If NA, the default configuration will be set.
#' @param outDir the directory where the annotated files will be stored
#' @param multipleDocs TRUE when multiple documents are loaded from a single file with each row representing a document. The file should have two columns. The first for the unique document identifier and the second for the textual descriptions
#' @importFrom rJava is.jnull
#' @importFrom tools file_ext
#' @importFrom methods is
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- CMdictionary(input=obo, outputDir=getwd(), synonymType='ALL')
#' myopts <- CMoptions()
#' paramValueIndex(myopts) <- 40
#' entities <- EntityFinder(input=readRDS(system.file('extdata', 'vignette_data', 'GEO_human_chip.rds', package='Onassis')), dictionary=sample_dict, options=myopts)
#' @export
EntityFinder <- function(input, dictionary, options = NA, outDir = tempdir(), multipleDocs = FALSE) {
    # Creating an instance of EntityFinder
    ef <- new("EntityFinder")
    inputFileorDf <- input
    ## Setting the options object
    opts <- options
    if (is(options, "CMoptions")) {
        opts <- options
    } else {
        if (is.integer(options)) {
            opts <- CMoptions()
            paramValueIndex(opts) <- options
        } else {
            # setting to default values
            message("Setting options to default values")
            opts <- CMoptions()
        }
    }
    ## end else options
    results <- NA
    # Creating the dictionary object
    dict <- dictionary
    if (is(dict, "CMdictionary")) {
        if (is.jnull(dict@dictRef)) 
            stop("Invalid dictionary. Please provide a valid CMdictionary or create it from OBO ontologies")
    } else {
        if (file.exists(dictionary)) {
            if (file_ext(dictionary) != "xml") {
                dict <- CMdictionary(inputFileOrDb = dictionary)
            } else {
                if (file_ext(dictionary) == "xml") {
                  dict <- CMdictionary(dictType = "CMDICT", inputFileOrDb = dictionary)
                }
            }
        } else {
            stop("Invalid dictionary. Please build a correct dictionary")
        }  ##end else dictionary
    }
    if (is.data.frame(inputFileorDf)) 
        results <- annotateDF(ef, inputFileorDf, opts, dict, outDir = outDir) else results <- findEntities(ef, inputDirOrFile = inputFileorDf, multipleDocs = FALSE, 
        outDir = outDir, configOpt = opts, cmDict = dict)
    
    return(results)
}



#' \code{Similarity}
#'
#' @return Measure of the similarity bewtween the concepts passed as input
#' @param ontology The ontology file to create the DAG to compute similarities
#' @param termlist1 The single concept or vector of concepts in the first set or the name of a sample if annotatedtab contains an annotated table
#' @param termlist2 The single concept or vector of concepts in the second set or the name of a sample if annotatedtab contains an annotated table
#' @param annotatedtab The table of annotation of samples or entities
#' @param pairConf the pairwise configuration
#' @param icConf the information content configuration
#' @param groupConf the groupwise configuration
#' @description this constructr initializes the Similarity class to compute the similarity between couple of terms, couple of samples, or group of terms
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- CMdictionary(input=obo, outputDir=getwd(), synonymType='ALL')
#' myopts <- CMoptions(paramValueIndex=40)
#' term_list1 <- c('http://purl.obolibrary.org/obo/CL_0000000', 'http://purl.obolibrary.org/obo/CL_0000236')
#' term_list2 <- c('http://purl.obolibrary.org/obo/CL_0000542')
#' sim <- Similarity(obo, term_list1, term_list2)
#'
#' @export
Similarity <- function(ontology, termlist1, termlist2, annotatedtab = NA, icConf = "seco", 
    pairConf = "resnik", groupConf = "ui") {
    sim <- new("Similarity")
    ontology(sim) <- ontology
    pairConf <- c(pairConf, icConf)
    pairwiseConfig(sim) <- pairConf
    groupConfig(sim) <- groupConf
    
    similarity_value <- 0
    if (!is.data.frame(annotatedtab)) {
        if (length(termlist1) == 1 & length(termlist2) == 1) {
            similarity_value <- pairsim(sim, as.character(termlist1), as.character(termlist2))
        } else {
            if (length(termlist1) > 1 | length(termlist2) > 1) {
                similarity_value <- groupsim(sim, as.character(as.vector(termlist1)), 
                  as.character(as.vector(termlist2)))
            }
        }
    } else {
        similarity_value <- samplesim(sim, as.character(termlist1), as.character(termlist2), 
            annotatedtab)
    }
    return(similarity_value)
    
}
