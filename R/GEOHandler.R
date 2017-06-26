#' \code{connectToGEODB}
#'
#' @rdname connectToGEODB
#' @param sqliteFileName optional SQLite file path of the SQLite database if already downloaded
#' @param download If TRUE allow the automatic downloading of the database file.
#' @param optional destination directory 
#' @return A connection to the GEOmetadb
#' @description This method allows users to connect to the GEOmetadb downloaded. If no parameter is provided than the function retrieves the database in sqlite format and returns a connection to query the database
#' @examples
#' \dontrun{
#'  geo_connection <- connectToGEODB(download=TRUE)
#'  }
#'  geo_con <- connectToGEODB()
#' @export
#' @importFrom GEOmetadb getSQLiteFile
#' @importFrom RSQLite dbConnect SQLite
connectToGEODB <- function(sqliteFileName=NULL, download=FALSE, destdir=getwd()){
  geo_con <- NA
  if(is.null(sqliteFileName)){
    #set the filename to default
    sqliteFileName <- "GEOmetadb.sqlite"
  }
  
  
  if(! file.exists(file.path(destdir, sqliteFileName)) & download==TRUE){
    sqlfile <- GEOmetadb::getSQLiteFile(destdir=destdir)
  } 
  
  if(file.exists(file.path(destdir, sqliteFileName)))
    geo_con <- RSQLite::dbConnect(SQLite(), sqliteFileName)
  else 
    stop('please provide a valid connection')
  return(geo_con)
}




#' \code{experiment_types}
#'
#' @rdname experiment_types
#' @param GEOcon connection to the SQLite GEOmetadb databse
#' @return A character vector with all the possible experiment values
#' @description This method retrierves the experiment types stored in GEOmetadb
#' @examples
#' if(file.exists('GEOmetadb.sqlite')){
#'  geo_con <- connectToGEODB('GEOmetadb.sqlite')
#'  experiments <- experiment_types(geo_con)
#' }else{
#'  print('You need to download GEOmetadb.sqlite to run this example')
#' }
#' @export
#' @importFrom RSQLite dbGetQuery
experiment_types <- function(GEOcon){
  gse_types <- dbGetQuery(GEOcon, "select distinct type from gse order by type")
  gse_types <- unlist(sapply(as.character(as.vector(gse_types[,1])), function(list_of_types) {
    if(grepl(";", list_of_types))
      NA
    else
      list_of_types
  }))
  names(gse_types) <- rep(NA, length(gse_types))
  gse_types <- unique(sapply(gse_types, function(type_string) gsub(";", "", type_string)))
  gse_types <- unique(gse_types[which(!is.na(gse_types))])
  return(gse_types)
}

#' \code{organism_types}
#'
#' @rdname organism_types
#' @param geo_con connection to the SQLite GEOmetadb databse
#' @return A character vector with all the possible organism values
#' @description This method retrierves the allowed organisms in GEOmetadb
#' @examples
#' if(file.exists('GEOmetadb.sqlite')){
#'  geo_con <- connectToGEODB('GEOmetadb.sqlite')
#'  species <- organism_types(geo_con)
#' }else{
#'  print('You need to download GEOmetadb.sqlite to run this example')
#' }
#' @export

organism_types <- function(geo_con){
  organisms <-dbGetQuery(geo_con, "select distinct organism_ch1 from gsm")
  organisms <- unlist(sapply(as.character(as.vector(organisms[,1])), function(list_of_types) {
    if(grepl(";", list_of_types))
      NA
    else
      list_of_types
  }))
  organisms <- unique(organisms[which(!is.na(organisms))])
  return(organisms)
}



#' \code{getGEOMetadata}
#'
#' @rdname getGEOMetadata
#' @param geo_con connection to the SQLite GEOmetadb databse
#' @param experiment_type The type of experiment. Allowed values can be obtained through the function \code{experiment_types}
#' @param organism Optional type of organism. Allowed species can be obtained using the function \code{organism_types}. If no organism is passed as parameter the query will retrieve all the organisms
#' @param gpl Optional platform identifier in case a platform based query has to be executed
#' @return A data frame with the queried samples' metadata
#' @description This method retrierves the descriptive fields of the samples in GEO for a given experiment_type, organism or platform.
#' @examples
#' if(file.exists('GEOmetadb.sqlite')){
#'  geo_con <- connectToGEODB('GEOmetadb.sqlite')
#' methilation <- getGEOMetadata(geo_con, 'Methylation profiling by high throughput sequencing', 'Homo sapiens')
#' expression <- getGEOMetadata(geo_con, 'Expression profiling by array', 'Homo sapiens', 'GPL570')
#' }else{
#'  print('You need to download GEOmetadb.sqlite to run this example')
#' }
#' @export

getGEOMetadata <- function(geo_con, experiment_type=NA, organism=NA, gpl=NA){
  experiment_acs <- NA
  experiment_metadata <- NA
  statement <- ' where '
  if(!is.na(experiment_type)){
    exp_types <- experiment_types(geo_con)
    if(!experiment_type %in% exp_types)
      stop('Invalid experiment type. Please run experiment_types(GEOcon) to view valid values')
    experiment_query <- paste0("select gse, title, summary from gse where type ='", experiment_type,"'")
    experiment_metadata <- dbGetQuery(geo_con, experiment_query)
    colnames(experiment_metadata) <- c("gse", "experiment_title", "experiment_summary")
    experiment_acs <- unique(experiment_metadata$gse)
  }
  sample_query <-"select series_id, gsm, title, gpl, source_name_ch1, organism_ch1, characteristics_ch1, description from gsm"

  if(!is.na(experiment_acs[1])){
    sample_query <- paste0(sample_query, statement, "series_id in ('", paste(unique(experiment_acs), collapse="','") , "')")
    statement <- ' and '
  }

  if(!is.na(organism)){
    organism_types <- organism_types(geo_con)
    if(!organism %in% organism_types)
      stop("Invalid organism. Please run organism_types to visualize valid values")
    else{
      sample_query <- paste0(sample_query, statement, " organism_ch1 ='", organism,"'")
      statement <- ' and '
    }
  }
  if(!is.na(gpl)){
    sample_query <- paste0(sample_query, statement,  " gpl = '", gpl, "'")
  }
  sample_metadata <- dbGetQuery(geo_con, sample_query)
  columns <- c(2, 1, 3:ncol(sample_metadata))
  if(!is.na(experiment_metadata)){
    geo_metadata <- merge(sample_metadata, experiment_metadata, by.x='series_id', by.y='gse')
    columns <-  c(2,3,5,7,8,9,10,1,4,6)
  }

  else{
    geo_metadata <- sample_metadata

  }
    geo_metadata$characteristics_ch1 <- sapply(geo_metadata$characteristics_ch1, function(value){ gsub(";\t", "  ", value)})
    geo_metadata$description <- sapply(geo_metadata$description, function(value){ gsub(";\t", "  ", value)})

    return(geo_metadata[, columns])
}
