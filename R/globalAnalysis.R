#' \code{createScoreMatrix}
#' @rdname createScoreMatrix
#' @param ref_granges An object of type GRanges to be considered as the reference GRanges
#' @param granges_list A list of GRanges, one for each sample, to be mapped on the reference granges
#' @return A logical score matrix where the number of rows corresponds to the number of different genomic intervals in the ref_granges object and each column is associated to a Grange in the granges_list. Score(i, j) is set to 1 if the j-th sample has a interval overlapping the one in the i-th row of the reference GRanges.
#' @description This functions allows the creation of a score matrix for genomic regions overlapping the genomic regions provided as reference (e.g. promoter regions). Each entry of the matrix corresponds to 1 if the sample represented on the column overlaps the genomic interval represented on the row.
#' @examples
#'
#' granges <- readRDS(system.file('extdata', 'sample_granges.rds', package='Onassis'))
#' ref_granges <- granges[[1]]
#'
#' for(i in 2:length(granges)) {
#'  ref_granges <- GenomicRanges::union(ref_granges, granges[[i]])
#' }
#' score_mat <- create_score_matrix(ref_granges, granges)
#' @export

create_score_matrix <- function(ref_granges, granges_list) {
    semantic_set_matrix <- do.call("cbind", sapply(names(granges_list), function(grange_name) {
        matrix_column <- rep(0, length(ref_granges))
        granges_t <- granges_list[grange_name][[1]]
        overlap_of_gr_to_ref <- GenomicRanges::findOverlaps(ref_granges, granges_t)
        matrix_column[unique(overlap_of_gr_to_ref@from)] <- 1
        granges_t <- list(matrix_column)
    }))
    semantic_set_matrix
}



#' \code{annotateTissueDisease}
#'
#' @rdname annotateTissueDisease
#' @param geo_metadb_path The full path of the directory where the GEOmetadb.sqlite file is stored
#' @param gsm_list A list of GEO sample ids (GSM)s to annotate with tissue and disease concepts
#' @param tissue_obo The obo ontology containing concepts to identify tissues/cell lines
#' @param disease_obo The obo ontology containing concepts to identify diseases
#' @param outdir The directory where the results will be stored
#' @param height_threshold The percentage of clusters to merge based on the height of the dendrogram produced by the hclust method. Height_threshold is defined in the range [0, 1].
#' @param score_matrix A matrix where rows represent units (GRanges or genes) and columns represent GSMs.
#' @return A list of the tissue semantic sets defined by Onassis. For each tissue, a list of diseases and for each disease the columns of the \code{score_matrix} that were annotated with a given tissue and a given disease
#' @description annotateTissueDisease is a function to automatize the annotation process of tissues and diseases. It connects to the GEOmetadb through the \code{geo_metadb_path} parameter to retrieve the metadata of the samples provided in the \code{gsm_list} parameter.
#' A dictionary for tissues/cell lines is built from the \code{tissue_obo} file provided as parameter. All the samples' metadata are annotated with tissue concepts from the tissue_obo and samples are clustered based on the semantic similarity of the defined semantic annotation sets.
#' To reduce the number of tissue semantic sets, similar semantic sets are merged based on a givin semantic similarity threshold provided in the \code{height_threshold} parameter. Within each semantic set, samples are annotated with disease concepts from the dictionary obtained from \code{disease_obo} parameter. For each disease the function retrieves the columns of the \code{score_matrix} to organize them in a list with tissues, diseases and scores.
#'
#' @examples
#' if(!file.exists(file.path(getwd(), 'GEOmetadb.sqlite'))){
#'      message('To run this example please copy GEOmetadb.sqlite in your current working directory')
#'      } else{
#'      geo_metadb_path <- getwd()
#'      score_matrix <- readRDS(system.file('extdata', 'score_matrix.rds', package='Onassis'))
#'      gsm_list <- colnames(score_matrix)
#'      tissue_obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
#'      disease_obo <- system.file('extdata', 'sample.do.obo', package='OnassisJavaLibs')
#'      outdir = getwd()
#'      height_threshold <- 0.4
#'      result_list <- annotateTissueDisease(geo_metadb_path, gsm_list, tissue_obo, disease_obo, outdir, height_threshold, score_matrix)
#'      }
#' @importFrom stats as.dist hclust quantile cutree aggregate

annotateTissueDisease <- function(geo_metadb_path, gsm_list, tissue_obo, disease_obo, 
    outdir, height_threshold, score_matrix) {
    
    message("Connecting to the GEOmetadb")
    geo_con <- connectToGEODB(sqliteFileName = "GEOmetadb.sqlite", destdir = geo_metadb_path)
    
    message("Retrieving GSM metadata")
    gsm_list <- as.character(as.vector(gsm_list))
    gsm_list <- paste0("'", paste(gsm_list, collapse = "', '"), "'")
    query <- paste0("select * from gsm where gsm in (", gsm_list, ")")
    geo_metadata <- dbGetQuery(geo_con, query)
    geo_metadata <- geo_metadata[, c("gsm", "source_name_ch1", "characteristics_ch1")]
    
    message("Creating the tissue dictionary")
    outdir = tools:::file_path_as_absolute(outdir)
    tissue_dictionary <- CMdictionary(tissue_obo, dictType = "OBO", outdir)
    
    message("Annotatin the metadata with tissue/ cell lines")
    tissue_annotations <- EntityFinder(geo_metadata, dictionary = tissue_dictionary, 
        options = CMoptions(), outDir = outdir)
    
    message("Filtering generic tissue terms")
    tissue_annotations <- tissue_annotations[which(!tolower(tissue_annotations$term_name) %in% 
        c("cell")), ]
    
    message("Creating the disease dictionary")
    disease_dictionary <- CMdictionary(disease_obo, dictType = "OBO", outdir)
    
    message("Annotating diseases")
    disease_annotations <- EntityFinder(geo_metadata, dictionary = disease_dictionary, 
        options = CMoptions(), outDir = outdir)
    
    message("Filtering generic disease terms")
    disease_annotations <- disease_annotations[which(!tolower(disease_annotations$term_name) %in% 
        c("disease")), ]
    
    message("finding healthy samples")
    healthy_gsms <- geo_metadata$gsm[grep("healhty", tolower(geo_metadata$characteristic_ch1))]
    if (length(healthy_gsms) > 0) {
        new_lines <- as.data.frame(t(sapply(healthy_gsms[which(!healthy_gsms %in% 
            disease_annotations$sample_id)], function(gsm) {
            line <- as.character(as.vector(c(gsm, rep("healthy", 4))))
        })))
        colnames(new_lines) <- colnames(disease_annotations)
        disease_annotations <- rbind(disease_annotations, new_lines)
        disease_annotations[which(disease_annotations$sample_id %in% healthy_gsms)]$term_id = disease_annotations[which(disease_annotations$sample_id %in% 
            healthy_gsms)]$term_name = disease_annotations[which(disease_annotations$sample_id %in% 
            healthy_gsms)]$term_url = disease_annotations[which(disease_annotations$sample_id %in% 
            healthy_gsms)]$matched_sentence = "healthy"
    }
    
    message("Computing the semantic similarities between samples using the tissue/cell line semantic similarity")
    annotated_gsms <- unique(tissue_annotations$sample_id)
    x = length(annotated_gsms) - 1
    similarity_matrix <- matrix(0, nrow = length(annotated_gsms), ncol = length(annotated_gsms))
    rownames(similarity_matrix) <- colnames(similarity_matrix) <- annotated_gsms
    for (i in 1:x) {
        j = i + 1
        for (k in j:length(annotated_gsms)) similarity_matrix[i, k] <- similarity_matrix[k, 
            i] <- Similarity(tissue_obo, termlist1 = annotated_gsms[i], termlist2 = annotated_gsms[k], 
            annotatedtab = tissue_annotations)
    }
    diag(similarity_matrix) <- 1
    
    message("Creating the semantic distance matrix")
    semantic_distance <- 1 - as.matrix(similarity_matrix)
    semantic_distance <- as.dist(semantic_distance)
    clusters <- hclust(d = semantic_distance)
    
    message("Collapsing tissue semantic states")
    if (height_threshold < 1 & height_threshold > 0) {
        height <- quantile(clusters$height, height_threshold)
        merged_clusters <- cutree(clusters, h = height)
        merged_clusters <- as.data.frame(cbind(names(merged_clusters), merged_clusters))
        colnames(merged_clusters) <- c("annotation", "cluster")
        merged_clusters_names <- aggregate(annotation ~ cluster, data = merged_clusters, 
            paste, collapse = ", ")
        semantic_sets <- as.data.frame(t(apply(merged_clusters_names, 1, function(cluster) {
            term_url_list <- as.character(paste(unique(tissue_annotations$term_url[which(tissue_annotations$sample_id %in% 
                strsplit(cluster[2], ", ")[[1]])]), collapse = ", "))
            term_name_list <- as.character(paste(unique(tissue_annotations$term_name[which(tissue_annotations$sample_id %in% 
                strsplit(cluster[2], ", ")[[1]])]), collapse = ", "))
            semantic_set_row <- cbind(cluster[1], cluster[2], term_url_list, term_name_list)
            semantic_set_row
        })))
        colnames(semantic_sets) <- c("semantic_set_id", "list_of_gsms", "term_url_list", 
            "term_name_list")
    }
    semantic_sets$semantic_set_id <- paste0("Tissue", semantic_sets$semantic_set_id)
    
    
    
    message("Distributing the score matrix columns within tissue and disease semantic states list")
    
    tissue_disease_scored_list <- sapply(semantic_sets$semantic_set_id, function(semantic_set_id) {
        
        gsm_vector <- strsplit(semantic_sets$list_of_gsm[which(semantic_sets$semantic_set_id == 
            semantic_set_id)], ", ")[[1]]
        diseases_in_tissue <- sapply(gsm_vector, function(single_gsm) {
            d_vect <- disease_annotations$term_name[which(disease_annotations$sample_id == 
                single_gsm)]
            if (length(d_vect) == 0) 
                d_vect <- "Unknown"
            d_vect <- sort(unique(d_vect))
            d_string <- paste(d_vect, collapse = ", ")
        })
        unique_d <- unique(diseases_in_tissue)
        
        
        gsm_columns <- sapply(unique_d, function(disease_name) {
            gsms <- names(diseases_in_tissue)[which(diseases_in_tissue == disease_name)]
            gsm_columns <- as.matrix(score_matrix[, match(gsms, colnames(score_matrix))])
            colnames(gsm_columns) <- gsms
            gsm_columns
        }, simplify = FALSE)
        gsm_columns
    })
    return(tissue_disease_scored_list)
}


#' \code{semanticdifference}
#'
#' @rdname semanticdifference
#' @param score_matrix A matrix where rows represent units (GRanges or genes) and columns #' represent GSMs.
#' @param list_of_annotations A list of lists where the firt level elements represent a tissue/cell line semantic set, the second level elements represent disease semantic sets and each element of the list is a named score matrix with column names corresponding to sample (GSM) identifiers
#' @param fun_name The name of a testing function to measure the differences between semantic classes. For the test_type parameter =  'pair' the semanticdifference function applies the test function to the tissue semantic states including 'Healthy' and one or or more diseases. In this case the function (e.g. wilcoxon.test) should take as input a couple of vectors and return as output the value of the of the statistic '$statistic' field and a p-value '$p.value' field.  For the test_type = 'multiple' the function tests if there are differences between the semantic disease classes within the tissue class. The function (e.g. kruskal.test) takes as input a list of vectors (one for each disease semantic set) and returns the value of the statistic in the '$statistic' field and the corresponding p-value.
#' @param test_type This value can be 'pair' or 'multiple'. In case it is set to pair the function to provide in fun_name requires the tissue semantic class to include at least two disease semantic classes of which one has to be 'Healthy' to compare diseases against healthy. For example the 'wilcox.test'. When set to multiple the fun_name provided should take as input a list of input vectors corresponding to the different disease conditions. A minimum of three is required. For example the 'kruskal.test' can be used.
#' @return The semanticdifference function returns a list where each element corresponds to a tissue/cell line semantic state. The content of the list for 'multiple' test types a matrix of the test results and corresponding p.values.
#' @description semanticdifference function allows to test, within a given tissue class, the difference between i) the healthy samples and one or more disease states and ii) if there is a significant difference between the different disease states for each unit represented in each row of a scorematrix containing as rows genomic units and as columns GEO sample ids.
#'
#' @examples
#' granges <- readRDS(system.file('extdata', 'sample_granges.rds', package='Onassis'))
#' ref_granges <- granges[[1]]
#' for(i in 2:length(granges)) {
#'  ref_granges <- GenomicRanges::union(ref_granges, granges[[i]])
#' }
#' score_mat <- create_score_matrix(ref_granges, granges)
#' gsm_list <- names(granges)
#' list_of_annotations <- readRDS(system.file('extdata', 'list_of_annotations.rds', package='Onassis'))
#' fun_name = 'wilcox.test'
#' fun_type = 'pair'
#' sem_dif <- semanticdifference(score_mat, list_of_annotations, fun_name, fun_type)
#' @export
semanticdifference <- function(score_matrix, list_of_annotations, fun_name, test_type) {
    fun_obj <- match.fun(fun_name)
    comparative_results <- NULL
    # wilcoxon.test
    if (test_type == "pair") {
        message("Two sample testing with wilcox.test to test differences with healthy samples")
        comparative_results <- lapply(list_of_annotations, function(disease_list) {
            results <- NULL
            if (length(disease_list) > 1 & ("healthy" %in% tolower(names(disease_list)))) {
                healthy_matrix <- as.matrix(disease_list[[which(tolower(names(disease_list)) == 
                  "healthy")]])
                disease_list <- disease_list[which(tolower(names(disease_list)) != 
                  "healthy")]
                if (is.null(rownames(healthy_matrix))) 
                  rownames(healthy_matrix) <- paste0("row_", seq(1:nrow(healthy_matrix)))
                test_results <- lapply(disease_list, function(disease_matrix) {
                  if (is.null(rownames(disease_matrix))) 
                    rownames(disease_matrix) <- rownames(healthy_matrix)
                  results <- as.data.frame(t(sapply(rownames(disease_matrix), function(row_name) {
                    healthy_matrix_row <- as.numeric(as.vector(healthy_matrix[row_name, 
                      ]))
                    disease_matrix_row <- as.numeric(as.vector(disease_matrix[row_name, 
                      ]))
                    result <- fun_obj(healthy_matrix_row, disease_matrix_row)
                    cbind(result$statistic, result$p.value)
                  })))
                  colnames(results) <- c("statistic", "p.value")
                  results
                })
                results <- test_results
            }
            results
        })
        # kruskal.test
    } else if (test_type == "multiple") {
        message("Testing if there are differences between groups of diseases")
        comparative_results <- lapply(list_of_annotations, function(disease_list) {
            results <- NULL
            if (length(disease_list) > 2) {
                if (is.null(rownames(disease_list[[1]]))) 
                  disease_list_t <- lapply(disease_list, function(disease) {
                    disease <- as.matrix(disease)
                    rownames(disease) <- paste0("row_", seq(1, nrow(disease)))
                    disease
                  })
                
                test_result <- t(sapply(seq(1, nrow(disease_list_t[[1]])), function(i) {
                  i <- as.numeric(i)
                  row_vectors <- lapply(disease_list_t, "[", i, )
                  test_res <- fun_obj(row_vectors)
                  test_res_col <- c(test_res$statistic, test_res$p.value)
                  test_res_col
                }))
                print(test_result)
                colnames(test_result) <- c("statistic", "p.value")
                rownames(test_result) <- rownames(disease_list[[1]])
                results <- test_result
            }
            return(results)
        })
    }
    return(comparative_results)
}
