#' \code{similarityInstance}
#' @rdname similarityInstance
#' @param object Instance of the class \code{\link{Similarity-class}}
#' @return The java reference to an object of class Similarity
#' @description This method retrieves the java object referencing the Similarity class
#' @examples
#' sim <- new('Similarity')
#' similarityInstance(sim)
#' @export
setMethod(f = "similarityInstance", signature = "Similarity", definition = function(object) {
    return(object@similarityInstance)
})



#' \code{similarityInstance<-}
#' @rdname similarityInstance
#' @param value the reference to a java Similarity object
#' @return The object of class Similarity with a new instance of the java Similarity class
#' @examples
#' sim <- new('Similarity')
#' similarityInstance(sim) <-.jnew('iit/comp/epigen/nlp/similarity/Similarity')
#' @export
setReplaceMethod(f = "similarityInstance", signature = "Similarity", definition = function(object, 
    value) {
    object@similarityInstance <- value
    return(object)
})


#' \code{icConfig}
#' @rdname icConfig
#' @param object instance of class \code{\link{Similarity-class}}
#' @description This method retrieves the configuration of the intrinsic information content measure
#' @return The measure used to compute concepts' information content
#' @examples
#' sim <- new('Similarity')
#' icConfig(sim)
#' @export
setMethod("icConfig", signature = "Similarity", definition = function(object) {
    object@icConfig
})

#' \code{icConfig<-}
#' @rdname icConfig
#' @param value the information content measure selected.
#' @description This method sets the configuration of the intrinsic information content measure by taking as parameter the short flag associated to the information measure. To have details about the available short flags see the \code{\link{pairwiseConfig}} help
#' @return The similarity object with the new information conten measure set
#' @examples
#' sim <- new('Similarity')
#' icConfig(sim) <- 'sanchez'
#' @export
setReplaceMethod("icConfig", "Similarity", function(object, value) {
    object@icConfig <- value
    return(object)
})



#' \code{pairwiseConfigRef}
#' @rdname pairwiseConfigRef
#' @description This method retrieves the reference to the Java configuration used to compute semantic similarities.
#' @param object instance of class \code{\link{Similarity-class}}
#' @return The reference to the pairwise configuration used to compute semantic similarity
#' @examples
#' sim <- new('Similarity')
#' pairwiseConfigRef(sim)
#' @export
setMethod("pairwiseConfigRef", "Similarity", function(object) {
    object@pairwiseConfigRef
})



#' \code{pairwiseConfigRef<-}
#' @rdname pairwiseConfigRef
#' @param value See details
#' @description configures the pairwise java object to compute semantic similarity between two concepts of a given ontology, by passing as input the java reference to one of the allowed pairwise semantic similarity measures. For a complete list check the details section of the function pairwiseConfig.
#' @examples
#' sim <- new('Similarity')
#' pairwiseConfigRef(sim) <- c('lin')
#' @export
setReplaceMethod(f = "pairwiseConfigRef", signature = "Similarity", definition = function(object, 
    value) {
    list_result <- as.list(similarityInstance(object)$showMeasures())
    names(list_result) <- c("pairwiseMeasures", "infoContentOption", "groupwiseMeasures")
    a <- lapply(list_result, function(x) {
        out <- .jevalArray(x$toArray())
        out2 <- sapply(out, function(y) y$toString())
        return(out2)
    })
    icShortFlag = ""
    if (length(value) == 1) {
        if (!value %in% a$pairwiseMeasures) 
            stop("Invalid pairwise measure. Please run listSimilarities method to visualize valid values")
        # This is set to the default even if we are not using an IC based measure
        icShortFlag = "sanchez"
        if (!icShortFlag == "NA" & !icShortFlag %in% a$infoContentOption) 
            stop("Invalid IC measure. Pleas run listSimilarities method to visualize valid IC options")
        
    } else {
        icShortFlag = value[2]
        
    }
    object@pairwiseConfigRef <- similarityInstance(object)$setPairwiseConfig(value[1], 
        icShortFlag)
    return(object)
})


#' \code{pairwiseConfig}
#' @rdname pairwiseConfig
#' @description This method shows the value of the pairwise configuration.
#' @return The pairwise measure
#' @param object instance of class \code{\link{Similarity-class}}
#' @examples
#' sim <- new('Similarity')
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' ontology(sim) <- obo
#' pairwiseConfig(sim)
#' @export
setMethod("pairwiseConfig", "Similarity", function(object) {
    object@pairwiseConfig
})



#' \code{pairwiseConfig<-}
#' @rdname pairwiseConfig
#' @param value See details
#' @description and configures the pairwise measure to compute semantic similarity between two concepts of a given ontology.To set the pairwise measure one of the available short flags described in details should be used.
#' @details The following measures can be used to compute semantic similarities between two concepts.
#' \itemize{
#' \item{'edge_rada_lca'} {: Computes the similarity of two concepts based on the shortest path linking the two concepts.
#'
#'\eqn{sim(u,v) = 1 /sp(u,v)}
#'
#'}
#'\item{'edge_wupalmer'}{: Computes the similarity of two concepts based on the depth of the concepts and the depth of their most specific common ancestor
#'
#'\eqn{sim(u,v) = depth(MSCA[u,v]) / (depth(u) + depth(v))}
#'
#'}
#'\item{'edge_resnik'}{: Computes the similarity of two concepts based on the shortest path between the concepts and the maximum depth of the taxonomy
#'
#'\eqn{ (2 * max_depth - min_sp(u,v)) / (2 * max_depth) }
#'
#'max_depth is the maximum depth in the ontology
#'
#'sp(u,v) is the shortest path legnth between u and v
#'}
#'\item{'edge_leachod'}{: Computes the similarity of two concepts based on the shortest path as Rada but also considering the depth of the ontology
#'
#'\eqn{sim(u,v) = -log( (sp(u,v) + 1) / 2 * max_depth )}
#'
#'}
#'\item{'edge_slimani'}{: Computes the similarity of two concepts based on the depth of the most specific common ancesto and the max depth of the concepts
#'
#'\eqn{sim(u,v) = 2 * depth(MCA) / ((depth(u) + depth(v) + 1) * pf ))
#'}
#'
#' depth(MCA) is the maximum depth of the most common ancestor of the concepts
#'
#'  pf is a penalization factor used when concepts belong to the same hierarchy
#'}
#'}
#'The following measures require the specification of an additional meausre to compute the information content of nodes.
#' \itemize{
#'\item{'lin'}{: Computes the similarity between two concepts based on the information content of the two concepts and the information content of the most informative common ancestor of the two concepts
#'
#'\eqn{ sim(u, v) = (2 * IC(MICA)) / ( IC(u) + IC(v) )}
#'
#'IC(MICA) is the information content of the most informative common ancestor of u and v. MICA is the concept in the ancestors of both u and v that maximizes the Information Content measure.
#'
#'}
#' \item{'resnik'}{: Computes the similarity between two concepts based on the information content of the most informative common ancestors of the compared concepts
#'
#'\eqn{ sim(u,v) = IC(MICA)}
#'}
#'
#'
#'\item{'schlicker'}{: Computes the similarity between two concepts based on the information concent of the most informative common ancestor of the compared concepts and its probability of occurrence
#'
#'\eqn{ sim(u,v) = (2 * IC(MICA)) / ( IC(u) + IC(v)) * (1 - Prob_MICA)}
#'
#' Prob_MICA is the probability of occurrence of the most informative common ancestor of the compared concepts
#'}
#'\item{'jaccard'}{: Computes the similarity between two concepts based on the information content of the most informative common ancestor.
#'
#'\eqn{ sim(u, v) = IC(MICA) / (IC(u) + IC(v) - IC(MICA)) } if the sum of the IC of the concepts is different from the IC of the MICA else sim(u, v) = 0.
#'}
#'
#'\item{'sim'}{: This measure is based on \code{lin} similarity
#'
#'\eqn{sim(u, v) = lin(u, v) - (1 - (1 / (1+ IC(MICA))))}
#'
#'}
#'\item{'jc_norm'}{: Computes the similarity between two concepts based on the IC of the most informative ancestor of the concpets
#'
#'\eqn{ sim(u,v) = 1 - (IC(u) + IC(v) - 2 * IC(MICA)) / 2}
#'
#'}
#' }
#' Information content based measures require the configuration parameter for estimating concept specificity. Intrinsic estimation uses the topological properties of the taxonomic backbone of the semantic graph. There are different options:
#' \itemize{
#'\item{'zhou'}{: Intrinsic estimation of the specificity of the concepts based on their depth in the ontology.
#'
#'\eqn{IC(c) = k( 1 - log(D(c))/log(|C|)) + (1 - k) (log(max(depth(x)))/ log(depth_max))}
#'
#' k is a factor to adjust the weight of the two items of the equation
#'
#' D(c) is the number of hyponims of concept c
#'
#' |C| is the number of concepts in the ontology
#'
#' depth(c) is the maximum depth of concept c
#'
#' depth_max is the maximum depth in the ontology
#'}
#'\item{'resnik_1995'}{: Intrinsic estimation of the specificity of concepts based on the number of ancestors of the concept.
#'
#'\eqn{IC(c) = |A(c)|}
#'}
#'\item{'seco'}{Intrinsic estimation of the specificity of the concepts based on the number of concepts they subsume.
#'
#'\eqn{IC(c) = 1 - ( log(D(c) / log(|C|) )}
#'
#'D(c) is the number of hyponims of concept c
#'
#' |C| is the number of concepts in the ontology
#' }
#' \item{'sanchez'}{: Intrinsic estimation of the specificity of the concepts based on the number of leaves and the number of subsumers of the concepts
#'
#' \eqn{IC(c) = -log(x / nb_leaves + 1) } with \eqn{x = |leaves(c)| / |A(c)|}
#'
#' nb_leaves is the represents the number of leaves corresponding to the root node of the hierarchy
#'
#' leaves(c) is the number of leaves corresponding to the concept c
#'
#' |A(c)| is the number of concepts that subsume c
#'
#' }
#' \item{'anc_norm'}{: Intrinsic estimation of the specificity of concepts based on the number of ancestors of a given concept normalized on the number of concepts in the ontology.}
#' \item{'depth_min_non_linear'}{: Intrinsic estimation of the specificity of concepts based on their minimum depth.}
#' \item{'depth_max_non_linear'}{: Intrinsic estimation of the specificity of concepts based on their maximum depth.}
#' }
#' @return instance of the Similarity class with the new pairwise option.
#' @examples
#' sim <- new('Similarity')
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' ontology(sim) <- obo
#' pairwiseConfig(sim) <- 'edge_resnik'
#'  #The following configuration uses an information content based measure
#'  pairwiseConfig(sim) <- c('resnik', 'seco')
#' @export
setReplaceMethod(f = "pairwiseConfig", signature = "Similarity", definition = function(object, 
    value) {
    list_result <- as.list(object@similarityInstance$showMeasures())
    names(list_result) <- c("pairwiseMeasures", "infoContentOption", "groupwiseMeasures")
    a <- lapply(list_result, function(x) {
        out <- .jevalArray(x$toArray())
        out2 <- sapply(out, function(y) y$toString())
        return(out2)
    })
    if (length(value) == 1) {
        if (!value %in% a$pairwiseMeasures) 
            stop("Invalid pairwise measure. Please run listSimilarities method to visualize valid values")
        # This is set to the default even if we are not using an IC based measure
        icShortFlag = "sanchez"
    } else if (length(value) == 2) {
        if (!value[1] %in% a$pairwiseMeasures) 
            stop("Invalid pairwise measure. Please run listSimilarities method to visualize valid values")
        icShortFlag <- value[2]
        if (!icShortFlag == "NA" & !icShortFlag %in% a$infoContentOption) 
            stop("Invalid IC measure. Pleas run listSimilarities method to visualize valid IC options")
        if (icShortFlag == "NA") 
            icShortFlag = "sanchez"
    } else {
        stop("Invalid number of arguments")
    }
    
    
    
    object@icConfig <- icShortFlag
    object@pairwiseConfigRef <- similarityInstance(object)$setPairwiseConfig(value[1], 
        icShortFlag)
    object@pairwiseConfig <- value[1]
    return(object)
})



#' \code{groupwiseConfigRef}
#' @description This method shows the value of the groupwise configuration used to compute semantic similarities between groups of concepts.
#' @return groupwise configured measure for the similarity object provided as input
#' @rdname groupwiseConfigRef
#' @param object instance of class \code{\link{Similarity-class}}
#' @examples
#' sim <- new('Similarity')
#' groupwiseConfigRef(sim)
#' @export
setMethod("groupwiseConfigRef", "Similarity", function(object) {
    object@groupwiseConfigRef
})


#' \code{grouprwiseConfigRef<-}
#' @rdname groupwiseConfigRef
#' @param value one of the groupwise measures. See \code{\link{groupConfig}}
#' @description Sets the groupwise measure to the reference of a groupwise measure to the semantic similarity between groups of concepts. For available meausres see the groupConfig function's details.
#' @return instance of the Similarity class with the new grouprwise option.
#' @examples
#' sim <- new('Similarity')
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' groupwiseConfigRef(sim) <- 'ui'
#' @export
setReplaceMethod(f = "groupwiseConfigRef", signature = "Similarity", definition = function(object, 
    value) {
    list_result <- as.list(similarityInstance(object)$showMeasures())
    names(list_result) <- c("pairwiseMeasures", "infoContentOption", "groupwiseMeasures")
    a <- lapply(list_result, function(x) {
        out <- .jevalArray(x$toArray())
        out2 <- sapply(out, function(y) y$toString())
        return(out2)
    })
    if (!value %in% a$groupwiseMeasures) 
        stop("Invalid groupwise measure")
    object@groupwiseConfigRef <- similarityInstance(object)$setGroupwiseConfig(value)
    return(object)
})



#' \code{groupConfig}
#' @rdname groupConfig
#' @param object instance of class \code{\link{Similarity-class}}
#' @description This method shows the value of the groupwise configuration used to compute semantic similarities between groups of concepts.
#' @return groupwise configured measure for the similarity object provided as input
#' @examples
#' sim <- new('Similarity')
#' groupConfig(sim)
#' @export
setMethod("groupConfig", "Similarity", function(object) {
    object@groupConfig
})



#' \code{groupConfig<-}
#' @rdname groupConfig
#' @param value See details
#' @description Sets the groupwise measure to compute the semantic similarity between groups of concepts. For available meausres use the method listSimilarities().
##' @details  The following measures are indirect groupwise measures, meaning that they are used to aggregate individual pairwise measures.
##' \itemize{
##'\item{'min'}{: Minimum of the pairwise similarities of the concepts in the two groups}
##'\item{'average'}{: Average of the pairwise similarities of the concepts in the two groups}
##'\item{'max'}{: Max of the pairwise similarities of the concepts in the two groups}
##'\item{'bma'}{: Best match average}
##'\item{'bmm'}{: Best match max}
##' }
##' Direct groupwise measures directly compare the sets of concepts considering the features of both sets.
##' \itemize{
##'\item{'ui'}{: Considers the intersection and the union of the set of ancestors of the two groups of concepts:
##'
#'\eqn{ sim(group_u, group_v) = | intersection( A(group_u), A(group_v)) | \code{/} | union( A(group_u), A(group_v)) | )}
#'}
##'\item{'nto_max'}{: Normalized max Term Overlap, computes the groupwise semantic similarity considering the inclusive set of ancestors of the two groups of concepts.
##'
#'\eqn{sim(group_u, group_v) = | intersection( A(group_u), A(group_v)) | \code{/} | max( |A(group_u)|, |A(group_v)| ) |}
##'
##'}
##'\item{'lee'}{: Computes the groupwise semantic similarity considering the inclusive set of ancestors of the two groups of conceps.
##'
#'\eqn{ sim(group_u, group_v) = | union( A(group_u), A(group_v)) |}
#'
#'}
#'
##'\item{'lp'}{: Computes the groupwise semantic similarity between two groups of concepts as the depth of the longest shared path from the root node}
##'
##'\item{'gic'}{: Computes the groupwise semantic similarity between two groups of concepts as the ration between the information content of the concepts in the intersection of the ancestors in the two groups and the information content of the concepts in the union of the ancestors in the two groups.
#'
#'\eqn{ sim(group_u, group_v) = IC_intersection / IC_union}
##'}
##'\item{'batet'}{: Computes the groupwise semantic similarity between two groups of concepts considering the union and intersection of ancestors normalized on the number of concepts in the ontology.
##'
#'\eqn{ sim(group_u, group_v) = |(union( A(group_u), A(group_v) ) - intersection(A(group_u), A(group_v)))| /  ( |(union( A(group_u), A(group_v) )| * tot_concepts )}
##'}
##'}
#' @return instance of the Similarity class with the new grouprwise option.
#' @examples
#' sim <- new('Similarity')
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' ontology(sim) <- obo
#' groupConfig(sim) <- 'ui'
#'
#' @export
setReplaceMethod(f = "groupConfig", signature = "Similarity", definition = function(object, 
    value) {
    list_result <- as.list(object@similarityInstance$showMeasures())
    names(list_result) <- c("pairwiseMeasures", "infoContentOption", "groupwiseMeasures")
    a <- lapply(list_result, function(x) {
        out <- .jevalArray(x$toArray())
        out2 <- sapply(out, function(y) y$toString())
        return(out2)
    })
    if (!value %in% a$groupwiseMeasures) 
        stop("Invalid groupwise measure")
    if (value %in% c("min", "max", "average", "bma", "bmm") & is.na(object@pairwiseConfig)) 
        stop("The selected groupwise measure requires that pairwiseConfig is set to the one of available pairwise measures")
    object@groupwiseConfigRef <- similarityInstance(object)$setGroupwiseConfig(value)
    object@groupConfig <- value
    return(object)
})








#' \code{ontology<-}
#' @rdname ontology
#' @param value The path of an ontology file
#' @return The Similarity object where 'ontology' slot refers to the Java graph created
#' @description This method creates a semantic graph to compute semantic similarity between concepts. It takes as input an OBO ontology in RDF, OWL or OBO format.
#' @examples
#' sim <- new('Similarity')
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' ontology(sim) <- obo
#' @export
setReplaceMethod(f = "ontology", signature = "Similarity", definition = function(object, 
    value) {
    if (!file.exists(value)) 
        stop("Invalid input ontology path")
    object@ontology = object@similarityInstance$loadOntology(value)
    return(object)
})




#' \code{ontology}
#' @rdname ontology
#' @param object instance of class \code{\link{Similarity-class}}
#' @return Ontology object
#' @description This method shows the ontology.
#' @examples
#' sim <- new('Similarity')
#' ontology(sim)
#' @export
setMethod(f = "ontology", signature = "Similarity", definition = function(object) {
    
    return(object@ontology)
})


#' \code{pairsim}
#' @rdname pairsim
#' @description This method computes the semantic similarity between two terms of a given ontology.
#' @param object instance of class \code{\link{Similarity-class}}
#' @param term1 The URI of the ontology term in the format http://purl.obolibrary.org/obo/Ontology_id (e.g 'http://purl.obolibrary.org/obo/CL_0000542')
#' @param term2 The URI of the ontology term
#' @return the semantic similarity of the two provided concepts
#' @examples
#' sim <- new('Similarity')
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' ontology(sim) <- obo
#' pairwiseConfig(sim) <- listSimilarities()$pairwiseMeasures[9]
#' similarity <- pairsim(sim, 'http://purl.obolibrary.org/obo/CL_0000542',
#' 'http://purl.obolibrary.org/obo/CL_0000236')
#' @export
setMethod(f = "pairsim", signature("Similarity", "character", "character"), definition = function(object, 
    term1, term2) {
    URI1 <- .jcast(object@similarityInstance$createURI(term1), new.class = "org.openrdf.model.URI", 
        check = FALSE, convert.array = FALSE)
    URI2 <- .jcast(object@similarityInstance$createURI(term2), new.class = "org.openrdf.model.URI", 
        check = FALSE, convert.array = FALSE)
    if (is.jnull(pairwiseConfigRef(object))) 
        stop("No similarity measure defined. Please run setPairwiseConfig method")
    if (is.jnull(ontology(object))) 
        stop("No graph loaded. To load a graph use the ontology method")
    similarity_score <- .jcall(similarityInstance(object), "D", "pair_similarity", 
        URI1, URI2, .jcast(ontology(object), new.class = "slib.graph.model.graph.G"), 
        pairwiseConfigRef(object))
    return(similarity_score)
})


#' Method groupsim
#' @description This method computes the semantic similarity between two groups of terms of a given ontology.
#' @param object instance of class \code{\link{Similarity-class}}
#' @param termList1  A vector of URIs of ontology terms in the format \url{http://purl.obolibrary.org/obo/Ontology_id} (e.g \url{http://purl.obolibrary.org/obo/BTO_0004732})
#' @param termList2 A vector of URIs of ontology terms
#' @return the semantic similarity of the two provided groups of concepts
#' @rdname groupsim
#' @examples
#'
#' sim <- new('Similarity')
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' ontology(sim) <- obo
#' pairwiseConfig(sim) <- listSimilarities()$pairwiseMeasures[9]
#' groupConfig(sim) <- listSimilarities()$groupwiseMeasures[3]
#' similarity <- groupsim(sim, c('http://purl.obolibrary.org/obo/CL_0000542',
#'  'http://purl.obolibrary.org/obo/CL_0000236'),
#' c('http://purl.obolibrary.org/obo/CL_0000000'))
#' similarity
#'
#' @export
setMethod(f = "groupsim", signature("Similarity", "character", "character"), definition = function(object, 
    termList1, termList2) {
    URIs1 <- similarityInstance(object)$createURIs(.jarray(termList1))
    URIs2 <- similarityInstance(object)$createURIs(.jarray(termList2))
    if (is.jnull(pairwiseConfigRef(object))) 
        stop("No similarity measure defined. Use pairwiseConfig(similarity_object) to set the pairwise configuration option")
    if (is.jnull(ontology(object))) 
        stop("No graph loaded. To load a graph use the loadOntology method")
    if (is.jnull(groupwiseConfigRef(object))) 
        stop("Non group similarity measure defined. Use groupConfig(similarity_object) to set the pairwise configuration option")
    similarity_score <- similarityInstance(object)$group_similarity(URIs1, URIs2, 
        ontology(object), groupwiseConfigRef(object), pairwiseConfigRef(object))
    return(similarity_score)
})



#' \code{samplesim}
#' @description This method computes the semantic similarity between two named samples annotated with a group of ontolgy terms belonging to the same ontology
#' @param sample1  A sample ID with its annotations available in a data frame
#' @param sample2 A sample ID with its annotations available in a data frame
#' @param annotated_df data frame with annotations obtained using entityFinder. The data frame should have at least a column named 'sample_id' with the sample identifier and a column named 'term_url' with the URL of the ontology terms annotating the sample. The ontology terms must belong to the ontology loaded in the Similarity class.
#' @param object instance of class \code{\link{Similarity-class}}
#' @return The semantic similarity between the samples sample1 and sample2
#' @rdname samplesim
#' @examples
#' sim <- new('Similarity')
#'
#' pairwiseConfig(sim) <- listSimilarities()$pairwiseMeasures[9]
#' groupConfig(sim) <- listSimilarities()$groupwiseMeasures[3]
#' ef <- new('EntityFinder')
#' opts <- CMoptions()
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' ontology(sim) <- obo
#' sample_dict <- CMdictionary(inputFileOrDb=obo, outputDir=getwd(), synonymType='ALL')
#' sra_chip_seq <- readRDS(system.file('extdata', 'vignette_data', 'GEO_human_chip.rds',
#'   package='Onassis'))
#' chipseq_dict_annot <- EntityFinder(sra_chip_seq[1:20,c('sample_accession', 'title',
#'  'experiment_attribute', 'sample_attribute', 'description')], dictionary=sample_dict,
#'   options=opts)
#' s <- samplesim(sim, as.character(as.vector(chipseq_dict_annot$sample_id[1])),
#' as.character(as.vector(chipseq_dict_annot$sample_id[7])) , chipseq_dict_annot)
#' @export
setMethod(f = "samplesim", signature("Similarity", "character", "character", "data.frame"), 
    definition = function(object, sample1, sample2, annotated_df) {
        if (!"term_url" %in% colnames(annotated_df)) 
            stop("Invalid annotation data frame")
        similarity_score <- groupsim(object, as.character(as.vector(annotated_df$term_url[which(annotated_df$sample_id == 
            sample1)])), as.character(as.vector(annotated_df$term_url[which(annotated_df$sample_id == 
            sample2)])))
        return(similarity_score)
    })





#' \code{multisim}
#' @rdname multisim
#' @description This method computes the semantic similarity between samples annotated with different ontology terms from different ontologies
#' @param sample1 the name of a sample in annotations
#' @param sample2 the name of a sample in annotations
#' @param similarities a list of Similarity instances, one for each ontology used to annotate the data
#' @param annotations a list of annotated data frames obtained using annotateDF or findEntities, one for each ontology
#' @param aggregating_function A function used to aggregate the single similarities obtained from each ontology annotation. The function should be applied to a numeric vector. The default value is 'mean'
#' @rdname multisim
#' @return The aggregate semantic similarity between the samples sample1 and sample2
#' @examples
#' ef <- new('EntityFinder')
#'
#' opts <- CMoptions()
#' cell_dict_file <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' sample_dict <- CMdictionary(cell_dict_file, outputDir=getwd(),
#' synonymType='ALL')
#' samples <- findEntities(ef, system.file('extdata', 'test_samples',
#' 'test_samples.txt',
#' package='Onassis'), outDir=getwd(), multipleDocs=TRUE, configOpt=opts,
#'  cmDict=sample_dict)
#' d_dict_file <-  system.file('extdata', 'sample.do.obo', package='OnassisJavaLibs')
#' disease_dict <- CMdictionary(d_dict_file, outputDir=getwd(), synonymType='ALL')
#' disease <- findEntities(ef, system.file('extdata', 'test_samples',
#' 'test_samples.txt', package='Onassis'),
#'  multipleDocs=TRUE, outDir=getwd(), configOpt=opts,
#'   cmDict=disease_dict)
#'
#'
#' cell_sim <- new('Similarity')
#' ontology(cell_sim) <- cell_dict_file
#'
#' disease_sim <- new('Similarity')
#' ontology(disease_sim) <- d_dict_file
#'
#' pairwiseConfig(cell_sim) <- listSimilarities()$pairwiseMeasures[9]
#' pairwiseConfig(disease_sim) <- listSimilarities()$pairwiseMeasures[9]
#' groupConfig(cell_sim) <- listSimilarities()$groupwiseMeasures[3]
#' groupConfig(disease_sim) <- listSimilarities()$groupwiseMeasures[3]
#' similarity <- multisim(list(cell_sim, disease_sim),
#' list(samples, disease),
#' as.character(as.vector(samples[1,1])),
#' as.character(as.vector(samples[5,1])), 'mean')
#' @export
setMethod(f = "multisim", signature(similarities = "list", annotations = "list", 
    sample1 = "character", sample2 = "character"), definition = function(similarities, 
    annotations, sample1, sample2, aggregating_function = "mean") {
    lapply(similarities, function(similarity) {
        if (!is(similarity, "Similarity")) 
            stop(paste0("Invalid Similarity object ", similarity))
    })
    
    lapply(annotations, function(annot_df) {
        if (!is.data.frame(annot_df)) 
            stop(paste0("Invalid annotation", annot_df))
        if (!"term_url" %in% colnames(annot_df)) 
            stop("Invalid annotation data frame")
    })
    if (!length(similarities) == length(annotations)) 
        stop("The number of Similarity classes is different from the number of annotation data frames provided")
    all_similarities <- c(rep(0, length(similarities)))
    for (i in 1:length(similarities)) {
        sim <- similarities[[i]]
        annot <- annotations[[i]]
        
        term_list1 <- as.character(as.vector(annot$term_url[which(annot$sample_id == 
            sample1)]))
        term_list2 <- as.character(as.vector(annot$term_url[which(annot$sample_id == 
            sample2)]))
        if (identical(term_list1, character(0)) | identical(term_list2, character(0))) 
            similarity_score <- 0 else similarity_score <- groupsim(sim, as.character(unlist(as.list(term_list1))), 
            as.character(unlist(as.list(term_list2))))
        all_similarities[i] <- similarity_score
    }
    aggr_f <- match.fun(aggregating_function)
    return(aggr_f(all_similarities))
})






#' \code{listSimilarities}
#' @return the list of pairwise, information content and groupwise measures to compute the semantic similarities
#' @rdname listSimilarities
#' @description This method shows a list of the possible measures to compute pairwise and groupwise semantic similarity between concepts
#' @examples
#' s <- listSimilarities()
#' @export
listSimilarities <- function() {
    object <- new("Similarity")
    list_result <- as.list(object@similarityInstance$showMeasures())
    names(list_result) <- c("pairwiseMeasures", "infoContentOption", "groupwiseMeasures")
    a <- lapply(list_result, function(x) {
        out <- .jevalArray(x$toArray())
        out2 <- sapply(out, function(y) y$toString())
        return(out2)
    })
    return(a)
    
}

