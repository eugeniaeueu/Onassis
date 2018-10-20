# This file contains all the methods of the class Onassis

#' \code{dictionary<-}
#' @rdname dictionary
#' @param value the path of an OBO file
#' @description Method to get and set the \code{dictionary} slot of the class \code{\link{Onassis-class}}
#' @examples
#' onassis <- Onassis()
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' dictionary(onassis) <- obo
#' @export
setReplaceMethod(f = "dictionary", signature = "Onassis", definition = function(object,
                                                                                value) {
  if (!file.exists(value))
    stop("Invalid input ontology path")
  object@dictionary = value
  return(object)
})

#' \code{dictionary}
#' @return The path of the dictionary in case of get, the instance of Onassis with new dictionary in case of set
#' @param object instance of class \code{\link{Onassis-class}}
#' @rdname dictionary
#' @examples
#' o <- Onassis()
#' dictionary(o)
#'
#' @export
setMethod(f = "dictionary", signature = "Onassis", definition = function(object) {
  
  return(object@dictionary)
})



#' \code{simil<-}
#' @return the similarity matrix of an object of type \code{\link{Onassis-class}} in case of get, the new \code{\link{Onassis-class}} instance in case of set
#' @rdname simil
#' @description Method to get and set the \code{similarity} slot of the class \code{\link{Onassis-class}}
#' @param value a matrix of similarities between the entities of an object of class \code{\link{Onassis-class}}
#' @examples
#' onassis <- Onassis()
#' simil(onassis) <- matrix()
#'
#' @export
setReplaceMethod(f = "simil", signature = "Onassis", definition = function(object,
                                                                           value) {
  object@similarity = value
  return(object)
})

#' \code{simil}
#' @rdname simil
#' @param object instance of class \code{\link{Onassis-class}}
#' @examples
#' o <- Onassis()
#' simil(o)
#'
#' @export
setMethod(f = "simil", signature = "Onassis", definition = function(object) {
  
  return(object@similarity)
})


#' \code{entities<-}
#' @rdname entities
#' @return the entities of the Onassis object in case of get and the Onassis object with new entities in case of set
#' @description Method to get and set the \code{entities} slot of the class \code{\link{Onassis-class}}
#' @param value a data frame with annotated entities
#' @examples
#' onassis <- Onassis()
#' entities(onassis) <- data.frame()
#'
#' @export
setReplaceMethod(f = "entities", signature = "Onassis", definition = function(object,
                                                                              value) {
  object@entities = value
  return(object)
})

#' \code{entities}
#' @param object instance of class \code{\link{Onassis-class}}
#' @rdname entities
#' @examples
#' o <- Onassis()
#' entities(o)
#'
#' @export
setMethod(f = "entities", signature = "Onassis", definition = function(object) {
  
  return(object@entities)
})

#' \code{scores<-}
#' @rdname scores
#' @param value a matrix of scores
#' @return the matrix of scores in case of get and the new \code{\link{Onassis-class}} object in case of set
#' @description This method gets and sets the \code{scores} slot of a class \code{\link{Onassis-class}}
#' @examples
#' onassis <- Onassis()
#' scores(onassis) <- matrix()
#'
#' @export
setReplaceMethod(f = "scores", signature = "Onassis", definition = function(object,
                                                                            value) {
  object@scores = value
  return(object)
})

#' \code{scores}
#' @rdname scores
#' @param object instance of class \code{\link{Onassis-class}}
#' @examples
#' o <- Onassis()
#' scores(o)
#'
#' @export
setMethod(f = "scores", signature = "Onassis", definition = function(object) {
  
  return(object@scores)
})


#' \code{annotate}
#'
#' @description This method annotates the entities contained in a data frame with the concepts from a specific dictionary.
#' @param input A data frame where the first column is the ID of the sample or document to annotate
#' @param dictType the type of input dictionary
#'\describe{
#'\item{OBO}{A dictionary that has been created by An OBO file}
#'\item{ENTREZ}{Entrez genes dictionary}
#'\item{TARGET}{Entrez genes dictionary, Histone marks and Histone modifications}
#'\item{CMDICT}{A previously created dictionary file in the Conceptmapper XML format}
#'}
#' @param dictionary The local OBO/OWL ontology to be converted into an XML Conceptmapper dictionary or the URL to download the file. If NA is passed and the \code{dicType} parameter is not the default OBO then the method tries to download the corresponding dictionary from the available repositories. For ENTREZ and TARGET dictionary types a file named gene_info.gz can be automatically downloaded from \url{ftp://ncbi.nlm.nih.gov/gene/data/gene_info.gz} if its path is not provided by the user in this parameter. Alternatively an annotation package of the type \code{Org.xx.eg.db} from Bioconductor can be used. In this case the gene identifiers and their alternative names will be retrieved from the annotation database without the need of downloading a gene_info file.
#' @param dictoutdir  Optional parameter to specify the location where the Conceptmapper dictionary file will be stored. Defaults to current working directory.
#' @param d_synonymtype Optional parameter to specify the type of synonyms to consider when building the dictionary for Conceptmapper. For further detail \url{http://owlcollab.github.io/oboformat/doc/obo-syntax.html}. Default: EXACT
#'\describe{
#'\item{EXACT}{}
#'\item{ALL}{}
#'}
#' @param taxID the taxonomy identifier of the organism when the \code{dictType} = 'ENTREZ' or 'TARGET' and the \code{dictionary} parameter refers to a gene_info.gz file. If 0 all the taxonomies will be included in the new dictionary.
#' @param disease A logical value set to TRUE if the annotation requires the 'Healthy' condition to be found.
#' @param annot_out The path of the output directory where Conceptmapper annotation files will be stored
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
#' @param e_synonymtype The type of synoyms for the EntityFinder
#'\itemize{
#'\item EXACT_ONLY {Only exact synonyms are considered }
#'\item ALL {All synonym types are included}
#'}
#' @param multipleDocs TRUE when multiple documents are loaded from a single file with each row representing a document. The file should have two columns. The first for the unique document identifier and the second for the textual descriptions
#' @examples
#' geo_chip <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#'
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' onassis_results <- annotate(geo_chip, 'OBO', dictionary=obo)
#' entities <- entities(onassis_results)
#' entities <- entities[sample(nrow(entities), 30),]
#' @rdname annotate
#' @import data.table
#' @return instance of class \code{\link{Onassis-class}} with annotated entities
#' @export
setMethod("annotate", c("data.frame", "character", "character"), function(input,
                                                                          dictType = "OBO", dictionary = NA, dictoutdir = getwd(), d_synonymtype = "EXACT",
                                                                          taxID = 0, annot_out = getwd(), paramValueIndex = NA, SearchStrategy = "CONTIGUOUS_MATCH",
                                                                          CaseMatch = "CASE_INSENSITIVE", Stemmer = "NONE", StopWords = "NONE", OrderIndependentLookup = "ON",
                                                                          FindAllMatches = "YES", e_synonymtype = "ALL", multipleDocs = FALSE, disease = FALSE) {
  
  # Building the dictionary
  dict <- CMdictionary(inputFileOrDb = dictionary, dictType = dictType, outputDir = dictoutdir,
                       synonymType = d_synonymtype, taxID = taxID)
  
  # Setting the annotator options for the entity finder to the default
  myopts <- CMoptions(SearchStrategy = SearchStrategy, CaseMatch = CaseMatch, Stemmer = Stemmer,
                      StopWords = StopWords, OrderIndependentLookup = OrderIndependentLookup, FindAllMatches = FindAllMatches,
                      SynonymType = e_synonymtype, paramValueIndex = paramValueIndex)
  # Creating an instance of the class Onassis
  onassis <- new("Onassis")
  
  # Annotating the entitites
  annotated_df <- EntityFinder(input = input, dictionary = dict, options = myopts,
                               outDir = annot_out, multipleDocs = multipleDocs)
  
  # Checking for 'Healthy' conditions
  if (disease) {
    healthy_samples <- findHealthy(input)
    annotated_df <- data.frame(annotated_df)
    annotated_df$term_name <- as.character(as.vector(annotated_df$term_name))
    annotated_df$term_id <- as.character(as.vector(annotated_df$term_id))
    annotated_df$term_url <- as.character(as.vector(annotated_df$term_url))
    
    annotated_df$term_name[which(annotated_df$sample_id %in% healthy_samples)] <- "Healthy"
    annotated_df$term_id[which(annotated_df$sample_id %in% healthy_samples)] <- "Healthy"
    annotated_df$term_url[which(annotated_df$sample_id %in% healthy_samples)] <- "Healthy"
    healthy_samples <- healthy_samples[which(!healthy_samples %in% annotated_df$sample_id)]
    if (length(healthy_samples) > 0)
      new_lines <- data.frame(cbind(healthy_samples, rep("Healthy", length(healthy_samples)),
                                    rep("Healthy", length(healthy_samples)), rep("Healthy", length(healthy_samples))),
                              rep("Healthy", length(healthy_samples)))
    colnames(new_lines) <- colnames(annotated_df)
    annotated_df <- rbind(annotated_df, new_lines)
  }
  # (Collapsing the entities)
  if (nrow(annotated_df) > 0) {
    annotated_df <- annotated_df[!duplicated(annotated_df[, c("sample_id", "term_id",
                                                              "term_url", "term_name")]), ]
    setDT(annotated_df)
    collapsed_annotations <- annotated_df[, lapply(.SD, function(x) toString(x)),
                                          by = sample_id]
    entities(onassis) <- collapsed_annotations
  }
  
  if (dictType == "OBO") {
    if (file.exists(dictionary))
      dictionary(onassis) <- dictionary else if (url.exists(dictionary)) {
        destination <- basename(dictionary)
        dictionary(onassis) <- file.path(dictoutdir, destination)
      }
  } else # Creating the Onassis class to store the entitites
    dictionary(onassis) <- dict_location(dict)
  return(onassis)
})



#' \code{sim}
#' @return an instance of \code{\link{Onassis-class}} with computed similarities
#' @param onassis instance of class \code{\link{Onassis-class}}
#' @param iconf the information content measure see \code{pairwiseConfig} help for details
#' @param pairconf the pairwse measure to compute semantic similarity between single concepts. See \code{pariwiseConfig} help for details
#' @param groupconf the groupwise measure to compute semantic similarity between groups of concepts. See \code{groupConfig} help for details
#' @description This method computes the similarities of the entities annotated in a object fo class \code{\link{Onassis-class}}.
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data', 'GEO_human_chip.rds', package='Onassis'))
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' onassis_results <- annotate(geo_chip, 'OBO', dictionary=obo)
#' entities <- entities(onassis_results)
#' entities(onassis_results) <- entities[sample(nrow(entities), 30),]
#' onassis_results <- sim(onassis_results)
#'
#' @rdname sim
#' @export
setMethod("sim", signature = c("Onassis"), def = function(onassis, iconf = "sanchez",
                                                          pairconf = "lin", groupconf = "bma") {
  
  if (!class(onassis) == "Onassis")
    return(NA) else {
      entities <- entities(onassis)
      entities <- entities[which(!entities$term_url == ""), ]
      if (!("term_id" %in% colnames(entities))) {
        term_id <- gsub("http://purl.obolibrary.org/obo/", "", entities$term_url)
        entities <- cbind(entities, term_id)
      }
      
      unique_sets <- data.frame(unique(entities[, c("term_url", "term_id", "term_name")]))
      
      semantic_net_matrix <- matrix(0, nrow(unique_sets), nrow(unique_sets))
      
      colnames(semantic_net_matrix) <- rownames(semantic_net_matrix) <- unique_sets[,
                                                                                    1]
      
      trim <- function(x) gsub("^\\s+|\\s+$", "", x)
      k = nrow(unique_sets) - 1
      sim_instance <- .jnew("iit/comp/epigen/nlp/similarity/Similarity")
      ontology_obo <- dictionary(onassis)
      graph <- sim_instance$loadOntology(ontology_obo)
      config_p <- sim_instance$setPairwiseConfig(pairconf, iconf)
      config_g <- sim_instance$setGroupwiseConfig(groupconf)
      sm_engine <- .jnew("slib/sml/sm/core/engine/SM_Engine", .jcast(graph, "slib/graph/model/graph/G"))
      for (i in 1:k) {
        gc()
        # Reinitialization of the JVM to avoid Out of memory esceptions
        .jinit(force.init = TRUE, parameters = "-Xmx20g")
        similarity <- 0
        term_list1 <- strsplit(rownames(semantic_net_matrix)[i], ",")[[1]]
        term_list1 <- trim(term_list1)
        URIs1 <- sim_instance$createURIs(.jarray(term_list1))
        
        massimo = nrow(semantic_net_matrix)
        minimo = i + 1
        for (j in minimo:massimo) {
          term_list2 <- strsplit(rownames(semantic_net_matrix)[j], ",")[[1]]
          term_list2 <- trim(term_list2)
          URIs2 <- sim_instance$createURIs(.jarray(term_list2))
          if (length(term_list1) > 0 & length(term_list2) > 0)
          {
            if ((length(term_list1) == 1 & term_list1[1] == "Healthy") |
                (length(term_list2) == 1 & term_list2[1] == "Healthy"))
              semantic_net_matrix[i, j] <- semantic_net_matrix[j, i] <- 0 else {
                
                similarity <- sm_engine$compare(config_g, config_p, URIs1,
                                                URIs2)
                
                semantic_net_matrix[i, j] <- semantic_net_matrix[j, i] <- similarity
              }  # end if healthy
          }  # end if
        }  # end inner for
      }  # end outer for for the computation of the semantic similairty
      
      # setting the names of the semantic matrix
      if ("short_label" %in% colnames(entities)) {
        unique_sets <- data.frame(unique(entities[, c("term_url", "term_id",
                                                      "term_name", "short_label")]), stringsAsFactors = FALSE)
        rownames(semantic_net_matrix) <- colnames(semantic_net_matrix) <- unique_sets[,
                                                                                      4][match(rownames(semantic_net_matrix), unique_sets[, 1])]
      } else rownames(semantic_net_matrix) <- colnames(semantic_net_matrix) <- unique_sets[,
                                                                                           3][match(rownames(semantic_net_matrix), unique_sets[, 1])]
      diag(semantic_net_matrix) <- 1
      simil(onassis) <- semantic_net_matrix
      return(onassis)
    }  # end else (class(onassis ) is onassis)
})





#' \code{collapse}
#' @param onassis instance of class \code{\link{Onassis-class}}
#' @param simil_thresh the semantic similarity threshold to use to merge similar semantic sets
#' @description This method collapses semantic states in an Onassis object.
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' onassis_results <- annotate(geo_chip, 'OBO', dictionary=obo)
#' entities <- entities(onassis_results)
#' entities(onassis_results) <- entities[sample(nrow(entities), 15),]
#' onassis_results <- sim(onassis_results)
#' collapsed_onassis <- collapse(onassis_results, 0.9)
#' @rdname collapse
#' @return a new object of class \code{\link{Onassis-class}} with collapsed annotations for the entities and a new similarity matrix of similarities between newly created semantic sets
#' @export
setMethod("collapse", signature = c("Onassis"), def = function(onassis, simil_thresh) {
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  if (!class(onassis) == "Onassis")
    return(NA) else {
      entities <- entities(onassis)
      
      # If this column is available in entities maybe we are collapsing otherwise user
      # has to run similarity before calling this method
      similarity <- simil(onassis)
      if ("short_label" %in% colnames(entities) & any(rownames(similarity) %in%
                                                      entities$short_label))
      {
        # Setting again the urls as rownames for the similarity matrix so that its easier
        # to compute new similarities
        temp_entities <- data.frame(unique(entities[, c("term_url", "short_label")]))
        rownames(similarity) <- colnames(similarity) <- temp_entities$term_url[match(rownames(similarity),
                                                                                     temp_entities$short_label)]
      }  # end if short_label
      else {
        rownames(similarity) <- colnames(similarity) <- entities$term_url[match(rownames(similarity),
                                                                                entities$term_name)]
        short_label <- as.character(as.vector(entities$term_name))
        entities <- data.frame(cbind(entities, short_label), stringsAsFactors = FALSE)
      }
      semantic_distance <- 1 - as.matrix(similarity)
      semantic_distance <- as.dist(semantic_distance)
      clusters <- hclust(d = semantic_distance)
      
      # Cutting the clustering tree at the similarity threshold
      merged_clusters <- cutree(clusters, h = 1 - simil_thresh)
      merged_clusters <- data.frame(cbind(names(merged_clusters), as.numeric(as.vector(merged_clusters))))
      colnames(merged_clusters) <- c("term_url", "cluster")
      
      
      new_entities <- merge(entities[which(!entities$term_name == ""), ], merged_clusters,
                            by = "term_url", all.x = TRUE)
      new_entities$term_url <- as.character(as.vector(new_entities$term_url))
      new_entities$term_name <- as.character(as.vector(new_entities$term_name))
      new_entities$term_id <- as.character(as.vector(new_entities$term_id))
      new_entities$short_label <- as.character(as.vector(new_entities$short_label))
      clusters <- unique(new_entities$cluster)
      modified_entities <- sapply(clusters, function(cluster_name) {
        subset_of_rows <- new_entities[which(new_entities$cluster == cluster_name),
                                       ]
        tot_samples <- nrow(subset_of_rows)
        term_urls <- toString(subset_of_rows$term_url[order(subset_of_rows$term_url)])
        term_urls_occurrences <- table(trim(strsplit(toString(subset_of_rows$term_url[order(subset_of_rows$term_url)]),
                                                     ",")[[1]]))
        term_urls_occurrences <- term_urls_occurrences[order(-term_urls_occurrences)]
        new_entities[which(new_entities$cluster == cluster_name), c("term_url")] <<- toString(names(term_urls_occurrences))
        term_names <- toString(subset_of_rows$term_name[order(subset_of_rows$term_name)])
        term_names_occurrences <- table(trim(strsplit(toString(subset_of_rows$term_name[order(subset_of_rows$term_name)]),
                                                      ",")[[1]]))
        term_names_occurrences <- term_names_occurrences[order(-term_names_occurrences)]
        new_entities[which(new_entities$cluster == cluster_name), c("term_name")] <<- toString(names(term_names_occurrences))
        term_ids <- toString(subset_of_rows$term_id[order(subset_of_rows$term_id)])
        term_ids_occurrences <- table(trim(strsplit(toString(subset_of_rows$term_id[order(subset_of_rows$term_id)]),
                                                    ",")[[1]]))
        term_ids_occurrences <- term_ids_occurrences[order(-term_ids_occurrences)]
        new_entities[which(new_entities$cluster == cluster_name), c("term_id")] <<- toString(names(term_ids_occurrences))
        short_names <- term_names_occurrences[1:min(3:length(term_names_occurrences))]
        short_names <- paste0(names(short_names), " [", as.character(short_names),
                              "]")
        short_names <- toString(paste0(toString(short_names), " (", tot_samples,
                                       ")"))
        new_entities[which(new_entities$cluster == cluster_name), c("short_label")] <<- short_names
      })
      entities(onassis) <- new_entities
      onassis <- sim(onassis)
      return(onassis)
    }
})











#' \code{mergeonassis}
#' @param onassis1 instance of class \code{\link{Onassis-class}}
#' @param onassis2 instance of class \code{\link{Onassis-class}}
#' @description This method unifies the entities of two Onassis objects
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#' geo_chip <- geo_chip[sample(nrow(geo_chip), 15) ,]
#' obo1 <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' obo2 <- system.file('extdata', 'sample.do.obo', package='OnassisJavaLibs')
#' onassis_results1 <- annotate(geo_chip, 'OBO', dictionary=obo1)
#' onassis_results2 <- annotate(geo_chip, 'OBO', dictionary=obo2)
#' onassis_results <- mergeonassis(onassis_results1, onassis_results2)
#'
#' @rdname mergeonassis
#' @return new object of type \code{\link{Onassis-class}} with merged entities
#' @export
setMethod("mergeonassis", signature = c("Onassis", "Onassis"), def = function(onassis1,
                                                                              onassis2) {
  if (!class(onassis1) == "Onassis" | !class(onassis2) == "Onassis")
    return(NA) else {
      entities1 <- entities(onassis1)
      if('short_label' %in% colnames(entities1))
        ordered_entities1 <- entities1[, c('sample_id', 'term_id', 'term_name', 'term_url', 'short_label', 'matched_sentence') ]
      else
        ordered_entities1 <- entities1[, c('sample_id', 'term_id', 'term_name', 'term_url', 'matched_sentence')]
      entities2 <- entities(onassis2)
      if('short_label' %in% colnames(entities2))
        ordered_entities2 <- entities2[, c('sample_id', 'term_id', 'term_name', 'term_url', 'short_label', 'matched_sentence') ]
      else
        ordered_entities2 <- entities2[, c('sample_id', 'term_id', 'term_name', 'term_url', 'matched_sentence')]
      colnames(ordered_entities1)[2:ncol(ordered_entities1)] <- paste0(colnames(ordered_entities1)[2:ncol(ordered_entities1)], "_1")
      colnames(ordered_entities2)[2:ncol(ordered_entities2)] <- paste0(colnames(ordered_entities2)[2:ncol(ordered_entities2)], "_2")
      entities <- merge(ordered_entities1, ordered_entities2, by = "sample_id", all.x = TRUE)
      if('short_label_1' %in% colnames(entities) & 'short_label_2' %in% colnames(entities) )
        ordered_entities <- entities[, c('sample_id', 'term_id_1', 'term_name_1', 'term_url_1', 'short_label_1', 'matched_sentence_1',  'term_id_2', 'term_name_2', 'term_url_2', 'short_label_2', 'matched_sentence_2')]
      else if('short_label_1' %in% colnames(entities))
        ordered_entities <- entities[, c('sample_id', 'term_id_1', 'term_name_1', 'term_url_1', 'short_label_1', 'matched_sentence_1',  'term_id_2', 'term_name_2', 'term_url_2', 'matched_sentence_2')]
      else if('short_label_2' %in% colnames(entities))
        ordered_entities <- entities[, c('sample_id', 'term_id_1', 'term_name_1', 'term_url_1', 'matched_sentence_1',  'term_id_2', 'term_name_2', 'term_url_2', 'short_label_2', 'matched_sentence_2')]
      else 
        ordered_entities <- entities[, c('sample_id', 'term_id_1', 'term_name_1', 'term_url_1', 'matched_sentence_1',  'term_id_2', 'term_name_2', 'term_url_2', 'matched_sentence_2')]
      
      
      entities(onassis1) <- ordered_entities
      return(onassis1)
    }
})



#' \code{compare}
#' @rdname compare
#' @return The results of the comparison between semantic classes. In case of one ontology a square matrix whose rows and columns contain the semantic sets. For each couple of semantic sets i and j if by is 'row' the element in the row i and column j is a data frame with biological entities (genes, regions..) and for each entity, the results of the statistics function and p.value. For multiple test functions if padj = TRUE then also the Bonferroni correction will be reported. For each couple of semantic sets i and j if by is 'col' the element at row i and column j is a couple of values with the result of the statistic and corresponding p.value
#' In case of two ontologies the result will be a list named with the first level semantic sets and each element of the list will be the result of the comparisons between second level semantic sets within first level semantic sets. Depending on the by parameter, the elements of the list will be matrices or vectors as for the one ontology case.  
#' @param onassis instance of class \code{\link{Onassis-class}}
#' @param score_matrix a matrix of scores containing on the rows genomic units and on the columns the samples annotated in the entities
#' @param by 'row' if the test refers to single genomic units in multiple conditions, 'col' if the test compares all the genomic units across different conditions. Defaults to row
#' @param fun_name name of the test to apply
#' @param fun_args list of arguments needed by the function
#' @param padj TRUE if multiple test correction is needed 
#' @description This method compares lists of scores (e.g. gene expression values, gene copy numbers, binding factors intensity) associated to the annotated entities (samples) of an Onassis object according to the semantic sets obtained from the annotation step. For Onassis objects annotated with a single ontology the method applies a test function to determine differences between subsets of the scores in one semantic set compared to subsets of scores in any other semantic set. For Onassis objects containing annotated entities with two ontologies, the first ensemble of semantic sets (e.g cell lines) will be used as the main container to orgnize samples and the comparisons will be carried out between the entities belonging to different semantic sets of the second ontology (e.g disease) within each semantic set defined from the first ontology.  
#' @details The entities slot of an Onassis object can contain annotations with concepts from one ontology or two ontologies. The function compare separates these two scenarios. 
#' 
#' ONE ONTOLOGY CASE:
#' The entities (samples) are assigned to their corresponding semantic set (e.g. cell lines)
#' A score matrix should have as rows genomic units (gene names, genomic regions, mutation identifiers...) and as columns the identifiers of all the entities annotated and belonging to semantic sets found in the sample_id field. The score for each entry of the matrix can be any biological measurement (gene expression RPKMs, peak intensity, copy numbers... )
#' Importantly, each semantic set can contain a different number of annotated entities (samples) and thus a single vector of scores (in case only 1 entity belongs to 1 semantic set) or a subset of the columns of the scores matrix
#' If by is "row" then the function provided as fun_name will be applied to compare, genomic unit by genomic unit (rows of the score matrix), all the possible couples of semantic sets.
#' The function can be any multiple statistical test function taking as parameters:
#'  - two numeric vectors of potentially different lengths (for genomic unit in row i, n samples in semantic set 1 and m samples in semantic set 2)
#'  - other optional arguments needed by the function passed as a list in fun_args
#'  The function should always provide as result a "statistic" and "p.value"  
#'  In case the padj is set to TRUE the bonferroni correction will be applied for multiple tests
#'  If by is "col" then the function provided as fun_name will be applied  to the couples of semantic sets considering all the possible values for the genomic units. In this case the test function should take as parameters 
#'  - two matrices with potentially different number of columns
#'  - other optional arguments needed by the function can be passed as fun_args
#' TWO ONTOLOGIES CASE:
#' The comparisons in this case will be carried out considering for each semantic set defined from the primary ontology, all the possible couples of semantic sets generated from the second ontology (e.g. for each cell line, different diseases) within the first ontology semantic sets  
#' @examples
#' #Loading ChIP-seq data
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data','GEO_human_chip.rds', package='Onassis'))
#' #Sampling 30 samples
#' geo_chip <- geo_chip[sample(nrow(geo_chip), 30) ,]
#' # Loading the obo ontology for cell lines
#' obo1 <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' # Loading the obo ontology for diseases 
#' obo2 <- system.file('extdata', 'sample.do.obo', package='OnassisJavaLibs')
#' #Annotating cell lines
#' onassis_results1 <- annotate(geo_chip, 'OBO', dictionary=obo1)
#' #Annotating diseases
#' onassis_results2 <- annotate(geo_chip, 'OBO', dictionary=obo2)
#' # Creating a score matrix
#' n <- length(unique(geo_chip$sample_accession))
#' m <- 50
#' score_matrix <-   matrix(sample(0:1, m * n, replace = TRUE), m, n)
#' colnames(score_matrix) <- unique(geo_chip$sample_accession)
#' rownames(score_matrix) <- paste0('gene_', seq(1, m, 1))
#' # Merging the annotations from the two ontologies in a single object
#' my_onassis <- mergeonassis(onassis1 = onassis_results1, onassis2 = onassis_results2)
#' scores(my_onassis) <- score_matrix
#' # Comparing the scores associated to samples belonging to cell line semantic sets
#' # By default the wilcox.test will be applied to each of the 50 genes 
#' scores(onassis_results1) <- score_matrix
#' gene_by_gene_cell_differences <- compare(onassis_results1)
#' head(gene_by_gene_cell_differences[1,2])
#' # Applying the same wilcox.test but obtaining a multiple test correction
#' gene_by_gene_cell_differences_ADJ <- compare(onassis_results1, padj=TRUE)
#' head(gene_by_gene_cell_differences_ADJ[1,2])
#' # Comparing disease genes across all the tissue semantic sets with wilcox.test and applying Bonferroni correction
#' gene_by_gene_disease_differences <- compare(my_onassis, by='row', padj=TRUE, fun_name='wilcox.test')
#' # Comparing diseases in cell line semantic sets
#' disease_differences <- compare(my_onassis, by='col', fun_name='wilcox.test')
#' # Using a personalized function
#' mykruskal <- function(x, y, params){
#' kruskal.test(as.list(c(x, y)))}
#' cell_differences_personalized <- compare(onassis_results1, by='col', fun_name='mykruskal')
#' @rdname compare
#' @export
setMethod("compare", signature = c("Onassis"), def = function(onassis, score_matrix = NA,
                                                              by = "row", fun_name = "wilcox.test", fun_args=list(), padj=FALSE) {
  scores_matrix <- NA
  # If a score matrix is passed as argument, this will be set in the onassis slot
  if (is.matrix(score_matrix)){
    scores(onassis) <- score_matrix
    scores_matrix <- score_matrix
  }
  else {
    if(is.na(score_matrix)){
      scores_matrix <- scores(onassis)
    }
    else
    {
      message('Please provide a valid matrix with scored entities to Onassis')
      return(NA)
    }
  }
   
   
  if (nrow(scores_matrix) == 0) {
    message("Unable to compare: empty score matrix, please provide a score matrix or set it")
    return(NA)
  }
  
  if (any(!colnames(scores_matrix) %in% entities(onassis)$sample_id)){
    message('Found score matrix column names that are not in entities annotations\n Onassis will try to subset the score matrix to obtain the same columns')
    scores(onassis) <- scores_matrix[, match(entities(onassis)$sample_id, colnames(scores_matrix))]
    scores_matrix <- scores(onassis)
    message('Done')
  }
  if(any(!entities(onassis)$sample_id %in% colnames(scores_matrix))){
    message('Found annotated entities not in score matrix, \n Onassis will try to subset the entities to obtain the same columns') 
    entities(onassis)$sample_id <- as.character(as.vector(entities(onassis)$sample_id))
     entities(onassis) <- entities(onassis)[match(colnames(scores_matrix), entities(onassis)$sample_id), ]
    message('Done')
  }
  if (nrow(scores_matrix) == 0) {
    message("Unable to compare: empty score matrix")
    return(NA)
  }
  
  # After checking that all the samples are represented both in the entities
  # and scores slot:
  fun_obj <- match.fun(fun_name)
  # Retrieving the entities
  entities <- entities(onassis)
  # Variable for the results of the tests
  comparison_result <- NA
  # Check if the entities table contains annotations from two ontologies
  #ONE ONTOLOGY CASE
  if (any(grepl("2", colnames(entities(onassis)))) == FALSE)
  {
    message('Running the ', fun_name, ' function to compare semantic sets')
    #Retrieving the unique semantic sets
    conditions <- unique(entities$term_url[which(!is.na(entities$term_url))])
    # Setting the names to the conditions to short_labels if these are available
    # or to term_names
    
    if ("short_label" %in% colnames(entities))
      condition_names <- unique(entities$short_label[which(!is.na(entities$term_url))])
    else
      condition_names <- unique(entities$term_name[which(!is.na(entities$term_url))])
    
    # Creating a square matrix for storing the results of the tests for each 
    # possible couple of semantic sets
    comparison_result <- matrix(list(), nrow = length(condition_names), ncol = length(condition_names))
    rownames(comparison_result) <- colnames(comparison_result) <- condition_names
    for (i in 1:(length(conditions) - 1)) {
      sample_ids_1 <- unique(entities$sample_id[which(entities$term_url == conditions[i])])
      j = i + 1
      for (k in j:length(conditions)) {
        sample_ids_2 <- unique(entities$sample_id[which(entities$term_url == conditions[k])])
        if (by == "row"){
          
          # Each genomic unit or element on the row will be tested for differences between
          # semantic sets in the columns
          
          test_result <- t(apply(scores_matrix, 1, function(row) {
            names(row) <- colnames(scores_matrix)
            #Creating function parameters
            arguments <- list()
            arguments[[1]] <- row[as.character(sample_ids_1)]
            arguments[[2]] <- row[as.character(sample_ids_2)]
            # Adding additional parameters
            if(length(fun_args)>0)
              arguments <- c(arguments, fun_args)
            # Calling the function with the options
            test_res <- do.call(fun_obj, arguments)
            # Getting the statistic and p.value
            test_res_col <- cbind(test_res$statistic, test_res$p.value)
          }))
          
          
          colnames(test_result) <- c("statistic", "p.value")
          
          # If correction for multiple testing is set to true
          # The Bonferroni correction will be applied
          if(padj==TRUE){
            adj.pvalue <- p.adjust(test_result[,2])
            test_result <- cbind(test_result, adj.pvalue)
          }
          comparison_result[condition_names[i], condition_names[k]][[1]] <- comparison_result[condition_names[k], condition_names[i]][[1]] <- test_result  
        }  # end if by row
        else {
          # We want to test by columns
          scores_matrix1 <- scores_matrix[, match(sample_ids_1, colnames(scores_matrix))]
          scores_matrix2 <- scores_matrix[, match(sample_ids_2, colnames(scores_matrix))]
          arguments <- list()
          arguments[[1]] <- scores_matrix1
          arguments[[2]] <- scores_matrix2
          # Adding additional parameters
          if(length(fun_args)>0)
            arguments <- c(arguments, fun_args)
          # Calling the function with the options
          test_res <- do.call(fun_obj, arguments)
          # Getting the statistic and p.value
          test_res_col <- cbind(test_res$statistic, test_res$p.value)
          comparison_result[condition_names[i], condition_names[k]][[1]] <- comparison_result[condition_names[k],
                                                                                              condition_names[i]][[1]] <- test_res_col
        }  #end else (if by is col)
      }  #end inner for
    }  # end outer for
    return(comparison_result)
    
  }  # end if ontology is only one
  else {
    message("TWO ONTOLOGIES FOUND")
    # Conditions in the level 1
    level1_conditions <- unique(entities$term_url_1[which(!is.na(entities$term_url_1))])
    if ("short_label_1" %in% colnames(entities))
      level1_condition_names <- unique(entities$short_label_1[which(!is.na(entities$term_url_1))]) 
    else 
      level1_condition_names <- unique(entities$term_name_1[which(!is.na(entities$term_url_1))])
    # For each condition in the level 1
    global_result_list <- list()
    outer_counter <- 1
    for (i in 1:length(level1_conditions)) {
      level1_cond <- level1_conditions[i]
      # Consider the subconditions in the level 2
      level2_conds <- unique(entities$term_url_2[which(entities$term_url_1 ==
                                                         level1_cond)])
      level2_conds <- level2_conds[which(!is.na(level2_conds))]
      if ("short_label_2" %in% colnames(entities)) {
        level2_cond_names <- unique(entities$short_label_2[which(entities$term_url_1 ==
                                                                   level1_cond)])
        level2_cond_names <- level2_cond_names[which(!is.na(level2_cond_names))]
      } else {
        level2_cond_names <- unique(entities$term_name_2[which(entities$term_url_1 ==
                                                                 level1_cond)])
        level2_cond_names <- level2_cond_names[which(!is.na(level2_cond_names))]
      }
      # if there are at least two sub conditions
      comparison_result <- NA
      if (length(level2_conds) > 1)
      {
        # Go across the couples of sub conditions and apply the tests
        comparison_result <- matrix(list(), nrow = length(level2_cond_names),
                                    ncol = length(level2_cond_names))
        rownames(comparison_result) <- colnames(comparison_result) <- level2_cond_names
        for (j in 1:(length(level2_conds) - 1)) {
          k = j + 1
          sample_ids_1 <- unique(entities$sample_id[which(entities$term_url_2 ==
                                                            level2_conds[j] & entities$term_url_1 == level1_cond)])
          for (l in k:length(level2_conds)) {
            sample_ids_2 <- unique(entities$sample_id[which(entities$term_url_2 ==
                                                              level2_conds[l] & entities$term_url_1 == level1_cond)])
            if (by == "row")
            {
              test_result <- t(apply(scores_matrix, 1, function(row) {
                names(row) <- colnames(scores_matrix)
                # Creating function parameters
                arguments <- list()
                arguments[[1]] <- row[as.character(sample_ids_1)]
                arguments[[2]] <- row[as.character(sample_ids_2)]
                # Adding additional parameters
                if(length(fun_args)>0)
                  arguments <- c(arguments, fun_args)
                # Calling the function with the options
                test_res <- do.call(fun_obj, arguments)
                # Getting the statistic and p.value
                test_res_col <- cbind(test_res$statistic, test_res$p.value)
              }))
              
              
              colnames(test_result) <- c("statistic", "p.value")
              
              # If correction for multiple testing is set to true
              # The Bonferroni correction will be applied
              if(padj==TRUE){
                adj.pvalue <- p.adjust(test_result[,2])
                test_result <- cbind(test_result, adj.pvalue)
              }
              
              comparison_result[level2_cond_names[j], level2_cond_names[l]][[1]] <- comparison_result[level2_cond_names[j],
                                                                                                      level2_cond_names[l]][[1]] <- test_result
              
            }  # end if by row
            else {
              # Comparing by columns 
              
              scores_matrix1 <- scores_matrix[, match(sample_ids_1, colnames(scores_matrix))]
              scores_matrix2 <- scores_matrix[, match(sample_ids_2, colnames(scores_matrix))]
              arguments <- list()
              arguments[[1]] <- scores_matrix1
              arguments[[2]] <- scores_matrix2
              # Adding additional parameters
              if(length(fun_args)>0)
                arguments <- c(arguments, fun_args)
              # Calling the function with the options
              test_res <- do.call(fun_obj, arguments)
              # Getting the statistic and p.value
              test_res_col <- cbind(test_res$statistic, test_res$p.value)
              colnames(test_res_col) <- c('statistics', 'p.vaue')
              comparison_result[level2_cond_names[j], level2_cond_names[l]][[1]] <- comparison_result[level2_cond_names[j],
                                                                                                      level2_cond_names[l]][[1]] <- test_res_col
            }  # end if by col
          }  # end inner for level 2 condition
        }  # end outer for level 2 conditions
        if (class(comparison_result) == "matrix") {
          global_result_list[[outer_counter]] <- comparison_result
          names(global_result_list)[outer_counter] <- level1_condition_names[i]
        }
      }  # end if length of conditions in level 2 > 1
    }  # end for level 1 conditions
    return(global_result_list)
  }  #end else [ontology is more than one]
  
})






#' \code{filterconcepts}
#' @rdname filterconcepts
#' @description This method filters unwanted concepts from the entities of an \code{\link{Onassis-class}} object
#' @param onassis An object of class \code{\link{Onassis-class}} with already annotated entities
#' @param concepts_to_filter A vector with unwanted concepts
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#' geo_chip <- geo_chip[sample(nrow(geo_chip), 15) ,]
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' onassis_results <- annotate(geo_chip, 'OBO', dictionary=obo)
#' filtered_onassis <- filterconcepts(onassis_results, c('cell'))
#'
#' @return the instance of \code{\link{Onassis-class}} with filtered entities
#' @export
setMethod("filterconcepts", signature = c("Onassis"), def = function(onassis, concepts_to_filter = c()) {
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  
  if (!class(onassis) == "Onassis")
    return(NA) else {
      entities <- data.frame(entities(onassis))
      for (i in 1:ncol(entities)) entities[, i] <- as.character(entities[, i])
      if (length(concepts_to_filter) > 0) {
        filtered_table <- data.frame(t(apply(entities, 1, function(entity_row) {
          entity_row <- as.character(entity_row)
          names(entity_row) <- colnames(entities)
          term_id <- entity_row["term_id"]
          term_name <- entity_row["term_name"]
          term_url <- entity_row["term_url"]
          splitted_term_names <- trim(strsplit(term_name, ",")[[1]])
          splitted_term_ids <- trim(strsplit(term_id, ",")[[1]])
          splitted_term_urls <- trim(strsplit(term_url, ",")[[1]])
          indexes_to_remove <- which(tolower(splitted_term_names) %in% tolower(concepts_to_filter))
          if (length(indexes_to_remove) > 0) {
            splitted_term_names <- splitted_term_names[-indexes_to_remove]
            splitted_term_ids <- splitted_term_ids[-indexes_to_remove]
            splitted_term_urls <- splitted_term_urls[-indexes_to_remove]
            if (length(splitted_term_names) > 0) {
              term_id <- paste(splitted_term_ids, collapse = ",")
              term_name <- paste(splitted_term_names, collapse = ",")
              term_url <- paste(splitted_term_urls, collapse = ",")
            } else {
              term_id <- NA
              term_name <- NA
              term_url <- NA
            }
          }
          new_row <- c(entity_row["sample_id"], term_id, term_name, term_url,
                       entity_row["matched_sentence"])
          return(new_row)
        })))
        colnames(filtered_table) <- colnames(entities)
        filtered_table <- filtered_table[which(!is.na(filtered_table$term_name)),
                                         ]
        entities(onassis) <- filtered_table
        
      }
      
      return(onassis)
    }
})
