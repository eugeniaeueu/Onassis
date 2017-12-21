#This file contains all the methods of the class Onassis

#' \code{dict<-}
#' @rdname dict
#' @aliases dict<-
#' @return The Onassis object
#' @description This method sets the dictionary of Onassis
#' @examples
#' onassis <- Onassis()
#' dict(onassis) <- file.path(getwd(), 'obo.obo')
#'
#' @export
setReplaceMethod(f = "dict", signature = "Onassis",
                 definition = function(object, value) {
                   if (!file.exists(value))
                     stop("Invalid input ontology path")
                   object@dictionary = value
                   return(object)
                 })

#' \code{dict}
#' @aliases dict
#' @return Ontology object
#' @rdname dict
#' @description This method shows the ontology.
#' @examples
#' o <- Onassis()
#' dict(o)
#'
#' @export
setMethod(f = "dict", signature = "Onassis",
          definition = function(object) {

            return(object@dictionary)
          })


#This file contains all the methods of the class Onassis

#' \code{simil<-}
#' @rdname simil
#' @aliases simil<-
#' @return The Onassis object
#' @description This method sets the similarity of Onassis
#' @examples
#' onassis <- Onassis()
#' simil(onassis) <- matrix()
#'
#' @export
setReplaceMethod(f = "simil", signature = "Onassis",
                 definition = function(object, value) {
                   object@similarity = value
                   return(object)
                 })

#' \code{simil}
#' @aliases simil
#' @return Ontology object
#' @rdname simil
#' @description This method shows the similarity matrix in Onassis.
#' @examples
#' o <- Onassis()
#' simil(o)
#'
#' @export
setMethod(f = "simil", signature = "Onassis",
          definition = function(object) {

            return(object@similarity)
          })


#' \code{entities<-}
#' @rdname entities
#' @aliases entities<-
#' @return The Onassis object
#' @description This method sets the entities of Onassis
#' @examples
#' onassis <- Onassis()
#' entities(onassis) <- matrix()
#'
#' @export
setReplaceMethod(f = "entities", signature = "Onassis",
                 definition = function(object, value) {
                   object@entities = value
                   return(object)
                 })

#' \code{entities}
#' @aliases entities
#' @return Ontology object
#' @rdname entities
#' @description This method shows the entities matrix in Onassis.
#' @examples
#' o <- Onassis()
#' entities(o)
#'
#' @export
setMethod(f = "entities", signature = "Onassis",
          definition = function(object) {

            return(object@entities)
          })

#' \code{scores<-}
#' @rdname scores
#' @aliases scores<-
#' @return The Onassis object
#' @description This method sets the score of Onassis
#' @examples
#' onassis <- Onassis()
#' scores(onassis) <- matrix()
#'
#' @export
setReplaceMethod(f = "scores", signature = "Onassis",
                 definition = function(object, value) {
                   object@scores = value
                   return(object)
                 })

#' \code{scores}
#' @aliases scores
#' @return Ontology object
#' @rdname scores
#' @description This method shows the score matrix in Onassis.
#' @examples
#' o <- Onassis()
#' scores(o)
#'
#' @export
setMethod(f = "scores", signature = "Onassis",
          definition = function(object) {

            return(object@scores)
          })


#' \code{annot}
#'
#' @description This method returns and sets the location of the dictionary.
#' @examples
#'
#' geo_chip <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#'
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' onassis_results <- annot(geo_chip, "OBO", dictionary=obo)
#' entities <- entities(onassis_results)
#' entities <- entities[sample(nrow(entities), 30),]
#' @rdname annot
#' @aliases annot
#' @importFrom data.table setDT
#' @export
setMethod('annot', c("data.frame", "character", "character"), function(input, dictType="OBO", dictionary=NA, dictoutdir=getwd(), d_synonymtype='EXACT', taxID=0, annot_out=getwd(), paramValueIndex=0, searchstrategy='CONTIGUOUS_MATCH', casematch='CASE_INSENSITIVE', stemmer='PORTER', stopwords='NONE', orderindependentlookup='OFF', findallmatches='NO', e_synonymtype='EXACT_ONLY', multipledocs=FALSE){

  #Building the dictionary

  dict <- dictionary(dictionary, dictType, dictoutdir, d_synonymtype, taxID)
  # Setting the annotator options for the entity finder to the default
  myopts <- CMoptions()
  #If user specified other arguments then use those
  if(paramValueIndex!=0){
    paramValueIndex(myopts) <- paramValueIndex
  }
  else{
    argomenti <- as.list(c( SearchStrategy=searchstrategy, CaseMatch=casematch, Stemmer=stemmer, Stopwords=stopwords, OrderIndependentLookup=orderindependentlookup, FindAllMatches=findallmatches, SynonymType=e_synonymtype))
    arguments(myopts) <- argomenti
  }
  onassis <- new('Onassis')

  #Annotating the entitites
  annotated_df <- annotate(inputFileorDf=input, dictionary=dict, options = myopts,
                           outDir = annot_out, multipleDocs = FALSE)
  #(Collapsing the entities)
  if(nrow(annotated_df)>0){
    annotated_df <- annotated_df[!duplicated(annotated_df[, c('sample_id', 'term_id', 'term_url', 'term_name')]),]
    setDT(annotated_df)
    collapsed_annotations <- annotated_df[, lapply(.SD, function(x) toString(x)), by=sample_id]
    entities(onassis) <- collapsed_annotations
  }

  if(dictType=='OBO'){
    if(file.exists(dictionary))
      dict(onassis) <- dictionary
    else if(url.exists(dictionary)){
      destination <- basename(dictionary)
      dict(onassis) <- file.path(dictoutdir, destination)
    } }
  else
    #Creating the Onassis class to store the entitites
    dict(onassis) <- dict_location(dict)
  return(onassis)
})




#' \code{sim}
#'
#' @description This method computes the similarities of the entities in an Onassis object.
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data', 'GEO_human_chip.rds', package='Onassis'))
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' onassis_results <- annot(geo_chip, "OBO", dictionary=obo)
#' entities <- entities(onassis_results)
#' entities(onassis_results) <- entities[sample(nrow(entities), 30),]
#' onassis_results <- sim(onassis_results)
#'
#' @rdname sim
#' @aliases sim
#' @export
setMethod('sim', signature=c("Onassis"), def= function(onassis, iconf='sanchez', pairconf='lin', groupconf='bma'){

  if(!class(onassis)=='Onassis')
    return(NA)
  else{
    entities <- entities(onassis)
    entities <- entities[which(!entities$term_url==''),]
    if(!('term_id' %in% colnames(entities))){
      term_id <- gsub('http://purl.obolibrary.org/obo/', '', entities$term_url)
      entities <- cbind(entities, term_id)
    }

    unique_sets <- data.frame(unique(entities[, c('term_url', 'term_id', 'term_name')]))

    semantic_net_matrix <- matrix(0, nrow(unique_sets), nrow(unique_sets))

    colnames(semantic_net_matrix) <- rownames(semantic_net_matrix) <- unique_sets[,1]

    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    k = nrow(unique_sets) - 1
    for(i in 1:k){
      gc()
      #Reinitialization of the JVM to avoid Out of memory esceptions
      .jinit(force.init = TRUE, parameters="-Xmx20g")
      similarity <- 0
      term_list1 <- strsplit(rownames(semantic_net_matrix)[i], ",")[[1]]
      term_list1 <- trim(term_list1)
      massimo = nrow(semantic_net_matrix)
      minimo = i + 1
      for(j in minimo:massimo){
        term_list2 <- strsplit(rownames(semantic_net_matrix)[j], ",")[[1]]
        term_list2 <- trim(term_list2)
        if(length(term_list1)>0 & length(term_list2)>0){
          sim <- Similarity()
          Onassis::ontology(sim) <- dict(onassis)
          icConfig(sim) <- iconf
          pairwiseConfig(sim) <- pairconf
          groupConfig(sim) <- groupconf
          similarity <- groupsim(sim, as.character(as.vector(term_list1)), as.character(as.vector(term_list2)))
          semantic_net_matrix[i, j] <- semantic_net_matrix[j, i] <- similarity
        } # end if
      } # end inner for
    } # end outer for for the computation of the semantic similairty

    #setting the names of the semantic matrix
    if('short_label' %in% colnames(entities)){
      unique_sets <- data.frame(unique(entities[, c('term_url', 'term_id', 'term_name', 'short_label')]), stringsAsFactors = FALSE)
      rownames(semantic_net_matrix) <- colnames(semantic_net_matrix) <- unique_sets[,4][match(rownames(semantic_net_matrix), unique_sets[,1])]
    }
    else
      rownames(semantic_net_matrix) <- colnames(semantic_net_matrix) <- unique_sets[,3][match(rownames(semantic_net_matrix), unique_sets[,1])]
    diag(semantic_net_matrix) <- 1
    simil(onassis) <- semantic_net_matrix
    return(onassis)
  }# end else (class(onassis ) is onassis)
})





#' \code{collapse}
#'
#' @description This method collapses semantic states in an Onassis object.
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' onassis_results <- annot(geo_chip, "OBO", dictionary=obo)
#' entities <- entities(onassis_results)
#' entities(onassis_results) <- entities[sample(nrow(entities), 15),]
#' onassis_results <- sim(onassis_results)
#' collapsed_onassis <- collapse(onassis_results, 0.9)
#' @rdname collapse
#' @aliases collapse
#' @export
setMethod('collapse', signature=c("Onassis"), def= function(onassis, simil_thresh){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  if(!class(onassis)=='Onassis')
    return(NA)
  else{
    entities <- entities(onassis)

    #If this column is available in entities maybe we are collapsing
    #otherwise user has to run similarity before calling this method
    similarity <- simil(onassis)
    if('short_label' %in% colnames(entities) & any(rownames(similarity) %in% entities$short_label)){
      #Setting again the urls as rownames for the similarity matrix so that its easier to compute new similarities
      temp_entities <- data.frame(unique(entities[, c('term_url','short_label')]))
      rownames(similarity) <- colnames(similarity) <- temp_entities$term_url[match(rownames(similarity), temp_entities$short_label)]
    } # end if short_label
    else{
      rownames(similarity) <- colnames(similarity) <- entities$term_url[match(rownames(similarity), entities$term_name)]
      short_label <- as.character(as.vector(entities$term_name))
      entities <- data.frame(cbind(entities, short_label), stringsAsFactors = FALSE)
    }
    semantic_distance <- 1 - as.matrix(similarity)
    semantic_distance <- as.dist(semantic_distance)
    clusters <- hclust(d=semantic_distance)

    #Cutting the clustering tree at the similarity threshold
    merged_clusters <- cutree(clusters, h=1-simil_thresh)
    merged_clusters <- data.frame(cbind(names(merged_clusters), as.numeric(as.vector(merged_clusters))))
    colnames(merged_clusters) <- c('term_url', 'cluster')


    new_entities <- merge(entities[which(!entities$term_name==""),], merged_clusters, by='term_url', all.x=TRUE)
    new_entities$term_url <- as.character(as.vector(new_entities$term_url))
    new_entities$term_name <- as.character(as.vector(new_entities$term_name))
    new_entities$term_id <- as.character(as.vector(new_entities$term_id))
    new_entities$short_label <- as.character(as.vector(new_entities$short_label))
    clusters <- unique(new_entities$cluster)
    modified_entities <- sapply(clusters, function(cluster_name){
      subset_of_rows <- new_entities[which(new_entities$cluster == cluster_name),]
      tot_samples <- nrow(subset_of_rows)
      term_urls <- toString(subset_of_rows$term_url[order(subset_of_rows$term_url)])
      term_urls_occurrences <- table(trim(strsplit(toString(subset_of_rows$term_url[order(subset_of_rows$term_url)]), ',')[[1]]))
      term_urls_occurrences <- term_urls_occurrences[order(-term_urls_occurrences)]
      new_entities[which(new_entities$cluster==cluster_name),c('term_url')] <<- toString(names(term_urls_occurrences))
      term_names <- toString(subset_of_rows$term_name[order(subset_of_rows$term_name)])
      term_names_occurrences <- table(trim(strsplit(toString(subset_of_rows$term_name[order(subset_of_rows$term_name)]), ',')[[1]]))
      term_names_occurrences <- term_names_occurrences[order(-term_names_occurrences)]
      new_entities[which(new_entities$cluster==cluster_name), c('term_name')] <<- toString(names(term_names_occurrences))
      term_ids <- toString(subset_of_rows$term_id[order(subset_of_rows$term_id)])
      term_ids_occurrences <- table(trim(strsplit(toString(subset_of_rows$term_id[order(subset_of_rows$term_id)]), ',')[[1]]))
      term_ids_occurrences <- term_ids_occurrences[order(-term_ids_occurrences)]
      new_entities[which(new_entities$cluster==cluster_name), c('term_id')] <<- toString(names(term_ids_occurrences))
      short_names <- term_names_occurrences[1:min(3: length(term_names_occurrences))]
      short_names <- paste0(names(short_names), ' [', as.character(short_names), ']')
      short_names <- toString(paste0(toString(short_names), ' (' ,tot_samples, ')'))
      new_entities[which(new_entities$cluster==cluster_name), c('short_label')] <<- short_names
    })
    entities(onassis) <- new_entities
    onassis <- sim(onassis)
    return(onassis)
  }
})











#' \code{mergeonassis}
#' @name mergeonassis
#' @rdname mergeonassis
#' @description This method unifies the entities of two Onassis objects
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#' geo_chip <- geo_chip[sample(nrow(geo_chip), 15) ,]
#' obo1 <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' obo2 <- system.file('extdata', 'sample.do.obo', package='OnassisJavaLibs')
#' onassis_results1 <- annot(geo_chip, "OBO", dictionary=obo1)
#' onassis_results2 <- annot(geo_chip, "OBO", dictionary=obo2)
#' onassis_results1 <- sim(onassis_results1)
#' onassis_results <- mergeonassis(onassis_results1, onassis_results2)
#'
#' @rdname mergeonassis
#' @aliases mergeonassis
#' @export
setMethod('mergeonassis', signature=c("Onassis", "Onassis"), def= function(onassis1, onassis2){
  if(!class(onassis1)=='Onassis' | !class(onassis2)=='Onassis')
    return(NA)
  else{
    entities2 <- entities(onassis2)
    colnames(entities2) <- paste0(colnames(entities2), '_2')
    entities <- entities(onassis1)
    entities <- merge(entities, entities2, by.x='sample_id', by.y='sample_id_2', all.x=TRUE)
    entities(onassis1) <- entities
    return(onassis1)
  }
})



#' \code{compare}
#' @name compare
#' @rdname compare
#'
#' @description This method compares a list of scored entities
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data','GEO_human_chip.rds', package='Onassis'))
#' geo_chip <- geo_chip[sample(nrow(geo_chip), 30) ,]
#' obo1 <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' obo2 <- system.file('extdata', 'sample.do.obo', package='OnassisJavaLibs')
#' onassis_results1 <- annot(geo_chip, "OBO", dictionary=obo1)
#' onassis_results2 <- annot(geo_chip, "OBO", dictionary=obo2)
#' n <- length(unique(geo_chip$sample_accession))
#' m <- 50
#' score_matrix <-   matrix(sample(0:1, m * n, replace = TRUE), m, n)
#' colnames(score_matrix) <- unique(geo_chip$sample_accession)
#' rownames(score_matrix) <- paste0('gene_', seq(1, m, 1))
#' my_onassis <- mergeonassis(onassis1 = onassis_results1, onassis2 = onassis_results2)
#' scores(my_onassis) <- score_matrix
#'
#' comparisons <- compare(my_onassis)
#' comparisons2 <- compare(my_onassis, by='col', fun_name='kruskal.test')
#' scores(onassis_results1) <- score_matrix
#' comparisons3 <- compare(onassis_results1)
#' comparisons4 <- compare(onassis_results1, by='col', fun_name='kruskal.test')
#' @rdname mergeonassis
#' @aliases mergeonassis
#' @export
setMethod('compare', signature=c("Onassis"), def= function(onassis, score_matrix=NA, by='row', fun_name='wilcox.test'  ){
  if(!is.na(score_matrix)){
    scores(onassis) <- score_matrix
  }
  scores_matrix <-  scores(onassis)
  if(nrow(scores_matrix)==0){
    message('Unable to compare because the score matrix is empty')
    return(NA)
  }
  if(any(!colnames(score_matrix) %in% entities(onassis)$sample_id))
    return(NA)
  fun_obj <- match.fun(fun_name)
  entities <- entities(onassis)
  comparison_result <- NA
  if(any(grepl('2', colnames(entities(onassis))))==FALSE){
    conditions <- unique(entities$term_url)
    if('short_names' %in% colnames(entities))
      condition_names <- unique(entities$short_name)
    else
      condition_names <- unique(entities$term_name)
    comparison_result <- matrix(list(), nrow=length(condition_names), ncol=length(condition_names))
    rownames(comparison_result) <- colnames(comparison_result) <- condition_names
    for(i in 1:(length(conditions)-1)){
      sample_ids_1 <- unique(entities$sample_id[which(entities$term_url==conditions[i])])
      j = i + 1
      for(k in j:length(conditions)){
        sample_ids_2 <- unique(entities$sample_id[which(entities$term_url==conditions[k])])
        if(by=='row'){
          test_result <- t(apply(scores_matrix, 1, function(row){
            names(row) <- colnames(scores_matrix)
            test_res <- fun_obj(row[sample_ids_1], row[sample_ids_2])
            test_res_col <- cbind(test_res$statistic, test_res$p.value)
          }))
          colnames(test_result) <- c('statistic', 'p.value')
          comparison_result[condition_names[i], condition_names[k]][[1]] <- comparison_result[condition_names[k], condition_names[i]][[1]] <- test_result
        }# end if by row
        else{
          scores_matrix1 <- scores_matrix[, sample_ids_1]
          scores_matrix2 <- scores_matrix[, sample_ids_2 ]
          test_res <- fun_obj(list(scores_matrix1, scores_matrix2))
          test_result <- c(test_res$statistic, test_res$p.value)
          comparison_result[condition_names[i], condition_names[k]][[1]] <- comparison_result[condition_names[k], condition_names[i]][[1]] <- test_result

        } #end else (if by is col)
      } #end inner for
    } # end outer for
    return(comparison_result)

  } # end if ontology is only one
  else{
    print('Multiple ontologies')
    #Conditions in the level 1
    level1_conditions <- unique(entities$term_url)
    if('short_names' %in% colnames(entities))
      level1_condition_names <- unique(entities$short_name)
    else
      level1_condition_names <- unique(entities$term_name)
    # For each condition in the level 1
    global_result_list <- list()
    outer_counter <- 1
    for(i in 1:length(level1_conditions)){
      level1_cond <- level1_conditions[i]
      #Consider the subconditions in the level 2
      level2_conds <- unique(entities$term_url_2[which(entities$term_url==level1_cond)])
      if('short_names_2' %in% colnames(entities))
        level2_cond_names <- unique(entities$short_name_2[which(entities$term_url==level1_cond)])
      else
        level2_cond_names <- unique(entities$term_name_2[which(entities$term_url==level1_cond)])
      #if there are at least two sub conditions
      comparison_result <- NA
      if(length(level2_conds)>1){
        #Go across the couples of sub conditions and apply the tests
        comparison_result <- matrix(list(), nrow=length(level2_cond_names), ncol=length(level2_cond_names))
        rownames(comparison_result) <- colnames(comparison_result) <- level2_cond_names
        for(j in 1:(length(level2_conds)-1)){
          k = j + 1
          sample_ids_1 <- unique(entities$sample_id[which(entities$term_url_2==level2_conds[j] & entities$term_url==level1_cond)])
          for(l in k:length(level2_conds)){
            sample_ids_2 <- unique(entities$sample_id[which(entities$term_url_2==level2_conds[l] & entities$term_url==level1_cond)])
            if(by=='row'){
              test_result <- t(apply(scores_matrix, 1, function(row){
                names(row) <- colnames(scores_matrix)
                test_res <- fun_obj(row[sample_ids_1], row[sample_ids_2])
                test_res_col <- cbind(test_res$statistic, test_res$p.value)
              }))
              colnames(test_result) <- c('statistic', 'p.value')
              comparison_result[level2_cond_names[j], level2_cond_names[l]][[1]] <- comparison_result[level2_cond_names[j], level2_cond_names[l]][[1]] <- test_result
            }# end if by row
            else{
              scores_matrix1 <- scores_matrix[, sample_ids_1]
              scores_matrix2 <- scores_matrix[, sample_ids_2 ]
              test_res <- fun_obj(list(scores_matrix1, scores_matrix2))
              test_result <- c(test_res$statistic, test_res$p.value)
              comparison_result[level2_cond_names[j], level2_cond_names[l]][[1]] <- comparison_result[level2_cond_names[j], level2_cond_names[l]][[1]] <- test_result
            } # end if by col
          } # end inner for level 2 condition
        } # end outer for level 2 conditions
        if(class(comparison_result)=='matrix'){
          global_result_list[[outer_counter]] <- comparison_result
          names(global_result_list)[outer_counter] <- level1_condition_names[i]
        }
      } # end if length of conditions in level 2 > 1
    }# end for level 1 conditions
    return(global_result_list)
  } #end else [ontology is more than one]

})






#' \code{filterconcepts}
#' @name filterconcepts
#' @rdname filterconcepts
#' @description This method filter unwanted concepts from the entities of an onassis object
#' @examples
#'geo_chip <- readRDS(system.file('extdata', 'vignette_data',
#' 'GEO_human_chip.rds', package='Onassis'))
#' geo_chip <- geo_chip[sample(nrow(geo_chip), 15) ,]
#' obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#' onassis_results <- annot(geo_chip, "OBO", dictionary=obo)
#'
#' @rdname filterconcepts
#' @aliases filterconcepts
#' @export
setMethod('filterconcepts', signature=c("Onassis"), def= function(onassis, concepts_to_filter=c()){
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  if(!class(onassis)=='Onassis')
    return(NA)
  else{
    entities <- data.frame(entities(onassis))
    for(i in 1:ncol(entities))
      entities[,i] <- as.character(entities[,i])
    if(length(concepts_to_filter)>0){
      filtered_table <- data.frame(t(apply(entities, 1, function(entity_row){
        entity_row <- as.character(entity_row)
        names(entity_row) <- colnames(entities)
        term_id <- entity_row['term_id']
        term_name <- entity_row['term_name']
        term_url <- entity_row['term_url']
        splitted_term_names <- trim(strsplit(term_name, ",")[[1]])
        splitted_term_ids <- trim(strsplit(term_id, ",")[[1]])
        splitted_term_urls <- trim(strsplit(term_url, ",")[[1]])
        indexes_to_remove <- which(tolower(splitted_term_names) %in% tolower(concepts_to_filter))
        if(length(indexes_to_remove) > 0){
          splitted_term_names <- splitted_term_names[-indexes_to_remove]
          splitted_term_ids <- splitted_term_ids[-indexes_to_remove]
          splitted_term_urls <- splitted_term_urls[-indexes_to_remove]
          if(length(splitted_term_names)>0){
            term_id <- paste(splitted_term_ids, collapse=',')
            term_name <- paste(splitted_term_names, collapse=',')
            term_url <- paste(splitted_term_urls, collapse=',')
          } else {
            term_id <- NA
            term_name <- NA
            term_url <- NA
          }
        }
        new_row <- c(entity_row['sample_id'], term_id, term_name, term_url, entity_row['matched_sentence'])
        return(new_row)
      })))
      colnames(filtered_table) <- colnames(entities)
      filtered_table <- filtered_table[which(!is.na(filtered_table$term_name)),]
      entities(onassis) <- filtered_table

    }

    return(onassis)
  }
})
