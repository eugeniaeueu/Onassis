#' \code{typeSystemRef}
#'
#' @description This method sets the type system to the ccp-nlp one to run the EntityFinder
#' @return the reference to the Java type system currently set
#' @rdname typeSystemRef
#' @param x instance of the class EntityFinder
#' @examples
#' ef <- new('EntityFinder')
#' typeSystemRef(ef)
#' @export
setMethod("typeSystemRef", "EntityFinder", function(x) {
    x@typeSystemRef
})



#' \code{typeSystemRef<-}
#' @description This method sets the type system to the ccp-nlp one to run the EntityFinder
#' @param value the java type system to detect concepts from ontologies.
#' @rdname typeSystemRef
#' @return The updated EntityFinder S4 object
#' @examples
#' ef <- new('EntityFinder')
#'type_system_array_list <- .jnew('java/util/ArrayList')
#' ccp_nlp_type_system <- .jfield('edu/ucdenver/ccp/nlp/uima/util/TypeSystemUtil',
#'  name = 'CCP_TYPE_SYSTEM')
#' sentence_detector_type_system_str <- 'org.cleartk.token.type.Sentence'
#' conceptmapper_type_system <-
#' 'edu.ucdenver.ccp.nlp.wrapper.conceptmapper.TypeSystem'
#' dictTerm <- 'analysis_engine.primitive.DictTerm'
#' tokenizer <- 'org.apache.uima.conceptMapper.support.tokenizer.TokenAnnotation'
#' vector_of_ts <- c(ccp_nlp_type_system, sentence_detector_type_system_str,
#'  conceptmapper_type_system, dictTerm, tokenizer)
#' type_system_description <-
#' J('org/uimafit/factory/TypeSystemDescriptionFactory')$createTypeSystemDescription(vector_of_ts)
#' typeSystemRef(ef) <- type_system_description
#' @export
setReplaceMethod("typeSystemRef", "EntityFinder", function(x, value) {
    ef@typeSystemRef <- value
})



#' \code{findEntities}
#'
#' @description This method finds concepts from a Conceptmapper Dictionary of type \code{\link{CMdictionary}} in a given directory or in a single pipe separated file containing a named document in each row, with a specified configuration of type \code{\link{CMoptions}}. This is a method of the \code{\link{EntityFinder-class}}
#' @param object instance of the class EntityFinder
#' @param inputDirOrFile the directory where the files to annotate are stored or the text file to annotate. A single file containing in each row sample names, the | symbol and the description of the sample is also allowed.
#' @param multipleDocs TRUE if a single file containing different text sources has been given as inputDirOrFile. FALSE if each text is in a separate file. Defaults to FALSE
#' @param outDir The directory where the Conceptmapper annotated files are stored. Default: the system tmp directory.
#' @param configOpt Object of type CMoptions in which the parameters to run Conceptmapper are stored
#' @param cmDict Instance of class \code{\link{CMdictionary}} or the file path of an already created CMdictionary
#' @return A data frame of annotations containing the sample name, the id of the OBO concept, the corresponding name, the part of the text containing the annotation
#' @rdname findEntities
#' @export
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' dict <- CMdictionary(inputFileOrDb=obo, outputDir=getwd(), synonymType='ALL')
#'
#' opts <- CMoptions()
#' ef <- new('EntityFinder')
#' annotations <- findEntities(ef,
#' system.file('extdata', 'test_samples', 'test_samples.txt', package='Onassis'), multipleDocs=TRUE, outDir=getwd(),
#'  configOpt=opts, cmDict=dict)
#'
setMethod(f = "findEntities", signature(object = "EntityFinder", inputDirOrFile = "character", 
    multipleDocs = "logical", outDir = "character", configOpt = "CMoptions", cmDict = "CMdictionary"), 
    definition = function(object, inputDirOrFile, multipleDocs = FALSE, outDir = tempdir(), 
        configOpt, cmDict) {
        if (!dir.exists(inputDirOrFile) & !file.exists(inputDirOrFile)) 
            stop("Invalid input directory or file")
        if (!dir.exists(outDir)) 
            dir.create(outDir)
        if (file.access(outDir, mode = 2) != 0) 
            stop("outDir not writable, please specify a valid output directory for the annotator output")
        do.call(file.remove, list(list.files(outDir, pattern = ".a1$", full.names = TRUE)))
        
        if (!isS4(cmDict)) {
            if (!file.exists(cmDict)) 
                stop("Invalid dictionary file path")
            cmDictobj <- CMdictionary(inputFileOrDb = cmDict, dictType = "CMDICT")
            cmDict <- cmDictobj
        }
        if (!isS4(configOpt)) {
            stop("Invalid option parameter. To create options use CMoptions.")
        }
        
        
        
        
        collection_reader <- NA
        
        if (multipleDocs) {
            
            inputFile <- .jnew("java/io/File", inputDirOrFile)
            
            collection_reader <- J("edu.ucdenver.ccp.nlp.uima.collections.line.DocumentPerLineCollectionReader")$createCollectionReader(.jcast(typeSystemRef(object), 
                "org.apache.uima.resource.metadata.TypeSystemDescription"), inputFile, 
                as.integer(0), as.integer(1e+08), J("edu.ucdenver.ccp.nlp.uima.collections.line.SimpleLinePipeDocumentExtractor")$class, 
                J("edu.ucdenver.ccp.nlp.uima.shims.document.impl.CcpDocumentMetadataHandler")$class)
            
        } else {
            
            inputDirOrFile <- .jnew("java/io/File", inputDirOrFile)
            
            collection_reader_description <- J("edu.ucdenver.ccp.nlp.uima.collections.file.FileSystemCollectionReader")$createDescription(.jcast(typeSystemRef(object), 
                "org.apache.uima.resource.metadata.TypeSystemDescription"), inputDirOrFile, 
                .jcall("java/lang/Boolean", "Z", "parseBoolean", "true"), J("edu.ucdenver.ccp.common.file.CharacterEncoding")$UTF_8, 
                .jnew("java/lang/String", "en"), .jcall("java/lang/Boolean", "Z", 
                  "parseBoolean", "false"), as.integer(1e+08), as.integer(0), .jnew("java/lang/String", 
                  J("edu.ucdenver.ccp.nlp.uima.util.View")$DEFAULT$viewName()), .jarray(c("txt")))
            
            collection_reader <- J("org.uimafit.factory.CollectionReaderFactory")$createCollectionReader(collection_reader_description, 
                .jnull())
            
        }
        
        sentence_detector_parameter_array <- .jarray(c(.jnew("java/lang/String", 
            J("org.cleartk.syntax.opennlp.SentenceAnnotator")$PARAM_SENTENCE_MODEL_PATH), 
            .jnew("java/lang/String", "/models/en-sent.bin"), .jnew("java/lang/String", 
                J("org.cleartk.syntax.opennlp.SentenceAnnotator")$PARAM_WINDOW_CLASS_NAMES), 
            .jnull(), .jnew("java/lang/String", J("org.cleartk.syntax.opennlp.SentenceAnnotator")$PARAM_SENTENCE_TYPE_NAME), 
            .jnew("java/lang/String", J("org.cleartk.token.type.Sentence")$class$getName())))
        
        # Analysis engine to detect sentences
        
        sentence_detector_engine_description <- J("org.uimafit.factory.AnalysisEngineFactory")$createPrimitiveDescription(J("org.cleartk.syntax.opennlp.SentenceAnnotator")$class, 
            sentence_detector_parameter_array)
        
        # Creating an aggregate analysis engine: sentence detector, offset tokenizer and
        # conceptmapper type system
        
        configuration_parameter <- as.integer(as.character(paramValueIndex(configOpt)))
        
        
        
        conceptMapperAggregateDesc <- J("edu.ucdenver.ccp.nlp.wrapper.conceptmapper.ConceptMapperPermutationFactory")$buildConceptMapperAggregatePermutation(configuration_parameter, 
            .jcast(typeSystemRef(object), "org.apache.uima.resource.metadata.TypeSystemDescription"), 
            cmDict@dictRef, J("org.cleartk.token.type.Sentence")$class)
        
        conceptMapperAggregateDesc$setAnnotatorImplementationName("aggregate descriptor")
        
        # Convert the CM OntologyTerm annotation class to CCPTextAnnotation classes The
        # true parameter tells the converter to keep slots (this is to retrieve the
        # canonical name of the ontology id)
        
        cmToCcpTypeSystemDesc <- J("edu.ucdenver.ccp.nlp.wrapper.conceptmapper.typesystem.ConceptMapper2CCPTypeSystemConverter_AE")$createAnalysisEngineDescription(.jcast(typeSystemRef(object), 
            "org.apache.uima.resource.metadata.TypeSystemDescription"), .jcall("java/lang/Boolean", 
            "Z", "parseBoolean", "true"))
        
        # Removes all token annotations as we don't want them to be output as RDF
        
        tokenRemoval <- J("edu.ucdenver.ccp.nlp.uima.annotators.filter.ClassMentionRemovalFilter_AE")$createAnalysisEngineDescription(.jcast(typeSystemRef(object), 
            "org.apache.uima.resource.metadata.TypeSystemDescription"), .jarray(c(J("edu.ucdenver.ccp.nlp.core.mention.ClassMentionType")$TOKEN$typeName())))
        
        
        cm_pipelineAeDescriptors <- J("edu.ucdenver.ccp.common.collections.CollectionsUtil")$createList(.jcast(conceptMapperAggregateDesc, 
            "java/lang/Object"), .jcast(cmToCcpTypeSystemDesc, "java/lang/Object"), 
            .jcast(tokenRemoval, "java/lang/Object"))
        
        duplicate_removal_filter <- J("edu.ucdenver.ccp.nlp.uima.annotators.filter.DuplicateAnnotationRemovalFilter_AE")$createAnalysisEngineDescription(.jcast(typeSystemRef(object), 
            "org.apache.uima.resource.metadata.TypeSystemDescription"))
        
        # Bionlp format printer in BRAT
        outputDir <- .jnew("java/io/File", outDir)
        
        bionlp_format_printer <- J("edu.ucdenver.ccp.nlp.uima.serialization.bionlp.BionlpFormatPrinter_AE")$createAnalysisEngineDescription(.jcast(typeSystemRef(object), 
            "org.apache.uima.resource.metadata.TypeSystemDescription"), outputDir, 
            .jcall("java/lang/Boolean", "Z", "parseBoolean", "true"))
        
        pipeline_ae_list <- .jarray(list(sentence_detector_engine_description, conceptMapperAggregateDesc, 
            cmToCcpTypeSystemDesc, tokenRemoval, duplicate_removal_filter, bionlp_format_printer), 
            contents.class = "org/apache/uima/analysis_engine/AnalysisEngineDescription")
        
        # Running the pipeline
        
        J("org.uimafit.pipeline.SimplePipeline")$runPipeline(collection_reader, pipeline_ae_list)
        
        print(outDir)
        outdf <- loadEntities(outDir, FALSE)
        
        if (is.null(outdf)) 
            message("No annotations available") else message("Conceptmapper annotations created in directory: ", outDir)
        return(outdf)
        
    })






#' \code{annotateDF}
#' @description Method to find concepts from a Conceptmapper Dictionary of type \code{\link{CMdictionary}} contained in a given data frame, with a specified configuration of type \code{\link{CMoptions}}. This is a method of the \code{\link{EntityFinder-class}}
#' @param object Instance of class \code{\link{EntityFinder-class}}
#' @param descr_df the table of text to annotate. The data frame should have identifiers in the first column and descriptions or text in the rest of the columns.
#' @param cmDict Object of type \code{\link{CMdictionary-class}} containing the reference to a previously created Conceptmapper dictionary. Alternatively the path to a Conceptmapper xml file can be passed.
#' @param outDir the output directory
#' @param configOpt instance of class \code{\link{CMoptions-class}}
#' @return A data frame of annotations containing the sample name, the id of the OBO concept, the corresponding name, the part of the text containing the annotation
#' @examples
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' dict <- CMdictionary(inputFileOrDb=obo, outputDir=getwd(), synonymType='ALL')
#' opts <- CMoptions()
#' ef <- new('EntityFinder')
#' methylation <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEOmethylation.rds', package='Onassis'))
#' annotations <- annotateDF(ef, methylation[1:10, ], getwd(), opts, dict)
#' @import data.table
#' @rdname annotateDF
#' @export
setMethod(f = "annotateDF", signature(object = "EntityFinder", descr_df = "data.frame", 
    outDir = "character", configOpt = "CMoptions"), definition = function(object, 
    descr_df, outDir = tempdir(), configOpt, cmDict) {
    if (!colnames(descr_df)[1] == "ID") 
        colnames(descr_df)[1] = "ID"
    descr_df[is.na(descr_df)] <- ""
    descr_df <- setDT(descr_df)
    descr_df <- descr_df[, lapply(.SD, function(x) toString(unique(x))), by = ID]
    descr_df[, `:=`(NEW, do.call(paste, c(.SD, sep = " "))), .SDcols = -1]
    descr_df <- descr_df[, c(1, ncol(descr_df)), with = FALSE]
    
    file_name <- tempfile(pattern = "df_annotations", tmpdir = tempdir(), fileext = ".txt")
    utils::write.table(descr_df, file = file_name, col.names = FALSE, row.names = FALSE, 
        sep = "|", quote = FALSE)
    out_df <- findEntities(object, file_name, TRUE, outDir = outDir, configOpt = configOpt, 
        cmDict = cmDict)
    return(out_df)
})
