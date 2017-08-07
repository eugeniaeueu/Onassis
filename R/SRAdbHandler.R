#' \code{connectToSRADB}
#'
#' @rdname connectToSRADB
#' @param sqliteFileName optional SQLite file path of the SQLite database if already downloadd
#' @return A connection to the SRAdb
#' @description This method allows users to connect to the SRAdb downloaded. If no parameter is provided than the function retrieves the database in sqlite format and return a connection to query the database
#' @examples
#' if(file.exists('SRAdb.sqlite')){
#'  sra_con <- connectToSRADB('SRAdb.sqlite')
#' }else{
#'  print('You need to download SRAdb.sqlite to run this example')
#' }
#' @export
#' @importFrom SRAdb getSRAdbFile
#' @importFrom RSQLite dbConnect SQLite



connectToSRADB <- function(sqliteFileName = NULL) {
    
    if (is.null(sqliteFileName)) {
        # set the filename to default
        sqliteFileName <- system.file(getwd(), "SRAdb.sqlite")
    }
    if (!file.exists(sqliteFileName)) 
        sqlfile <- SRAdb::getSRAdbFile()
    # creation of the connection to the database
    sra_con <- dbConnect(SQLite(), sqliteFileName)
    return(sra_con)
}




#' \code{library_strategies}
#'
#' @rdname library_strategies
#' @param SRAcon connection to the SQLite SRAdb databse
#' @return A character vector with all the possible library strategy values
#' @description This method retrierves the library strategy types stored in SRAdb
#' @examples
#' if(file.exists('SRAdb.sqlite')){
#'    sra_con <- connectToSRADB('SRAdb.sqlite')
#'    strategies <- library_strategies(sra_con)
#'    head(strategies)
#'  } else{
#'    print('You need to download the SRAdb.sqlite to run this example')
#'  }
#' @export
#' @importFrom RSQLite dbGetQuery
library_strategies <- function(SRAcon) {
    library_types <- dbGetQuery(SRAcon, "select distinct library_strategy from experiment where library_strategy is not null")
    return(as.character(as.vector(library_types[, 1])))
}


#' \code{library_sources}
#'
#' @rdname library_sources
#' @param SRAcon connection to the SQLite SRAdb databse
#' @return A character vector with all the possible library sources
#' @description This method retrierves the library source types stored in SRAdb
#' @export
#' @examples
#' if(file.exists('SRAdb.sqlite')){
#'    sra_con <- connectToSRADB('SRAdb.sqlite')
#'    sources <- library_sources(sra_con)
#'    head(sources)
#'    }else{
#'    print('You need to download the SRAdb.sqlite to run this example')
#'    }
#' @importFrom RSQLite dbGetQuery
library_sources <- function(SRAcon) {
    library_s <- dbGetQuery(SRAcon, "select distinct library_source from experiment where library_source is not null")
    return(as.character(as.vector(library_s[, 1])))
}


#' \code{getSRAMetadata}
#'
#' @rdname getSRAMetadata
#' @param sra_con connection to the SQLite SRAdb databse
#' @param library_strategy corresponding to the  type of experiment. Allowed values can be obtained through the function \code{library_strategies}
#' @param library_source corresponding to the material from the sample (GENOMIC, METAGENOMIC, TRANSCRIPTOMIC...)
#' @param taxon_id Optional taxon id of the organism. If no taxon_id is passed as parameter the query will retrieve all the organisms
#' @param center_name Optional center name identifier. Possible values can be obtained querying the database e.g. dbGetQuery(sra_con, 'select distinct center_name from sample')
#' @return A data frame with the queried samples' metadata
#' @description This method retrierves the descriptive fields of experiments and samples in SRAdb for a given library_strategy (experiment type), of a given organism and center name.
#' @examples
#' if(file.exists('SRAdb.sqlite')){
#'    sra_con <- connectToSRADB('SRAdb.sqlite')
#'    geo_human_chip <- getSRAMetadata(sra_con, library_strategy='ChIP-Seq',
#'    library_source='GENOMIC', taxon_id=9606, center_name='GEO')
#'    bisulfite_seq <- getSRAMetadata(sra_con, library_strategy='Bisulfite-Seq',
#'     library_source='GENOMIC', taxon_id=9606, center_name='GEO')
#'  } else{
#'    print('You need to download the SRAdb.sqlite to run this example')
#'    }
#' @export

getSRAMetadata <- function(sra_con, library_strategy, 
    library_source = NA, taxon_id = NA, center_name = NA) {
    exp_types <- library_strategies(sra_con)
    samples <- NA
    samples_df <- NA
    if (!library_strategy %in% exp_types) 
        stop("Invalid library strategy. Please run library_strategies(SRAcon) to view valid values")
    if (!is.na(taxon_id)) {
        samples_query <- paste0("select sample_accession, description, sample_attribute, sample_url_link from sample where taxon_id='", 
            taxon_id, "' and sample_accession IS NOT NULL")
        if (!is.na(center_name)) 
            samples_query <- paste0(samples_query, 
                " and center_name='", center_name, 
                "'")
        samples_df <- dbGetQuery(sra_con, samples_query)
        samples <- unique(as.character(as.vector(samples_df[, 
            1])))
    }
    experiment_query <- paste0("select experiment_accession, center_name, title, sample_accession, sample_name, experiment_alias, library_strategy, library_layout, experiment_url_link, experiment_attribute from experiment where library_strategy='", 
        library_strategy, "'")
    if (!is.na(library_source)) {
        sources <- unique(dbGetQuery(sra_con, "select distinct library_strategy, library_source from experiment order by library_strategy")[, 
            2])
        sources <- sources[which(!is.na(sources))]
        if (!library_source %in% sources) 
            stop("Invalid library_source") else experiment_query <- paste0(experiment_query, 
            " and library_source ='", library_source, 
            "' ")
    }
    if (is.character(samples) & length(samples) > 0) 
        experiment_query <- paste0(experiment_query, 
            " and sample_accession in ('", paste(unique(samples), 
                collapse = "','"), "')")
    if (!is.na(center_name)) {
        experiment_query <- paste0(experiment_query, 
            " and center_name=='", center_name, "'")
    }
    message("Running the query")
    experiment_df <- dbGetQuery(sra_con, experiment_query)
    message("Done!")
    if (is.data.frame(samples_df) & nrow(samples_df) > 
        0) 
        experiment_df <- merge(experiment_df, samples_df, 
            by = "sample_accession")
    experiment_df$experiment_attribute <- sapply(experiment_df$experiment_attribute, 
        function(value) {
            gsub("||", "  ", value)
        })
    experiment_df$sample_attribute <- sapply(experiment_df$sample_attribute, 
        function(value) {
            gsub("||", "  ", value)
        })
    experiment_df$sample_name <- sapply(experiment_df$sample_name, 
        function(value) {
            gsub("_", " ", value)
        })
    experiment_df$experiment_alias <- sapply(experiment_df$experiment_alias, 
        function(value) {
            gsub("_", " ", value)
        })
    
    return(experiment_df)
}

