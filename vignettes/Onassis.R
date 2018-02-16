## ----imports, echo=FALSE,eval=TRUE, message=FALSE------------------------
library(Onassis)
library(DT)
library(gplots)
library(org.Hs.eg.db)

## ----connectTodb, echo=TRUE,eval=FALSE-----------------------------------
#  ## Running this function might take long time if the database has to be downloaded.
#  geo_con <- connectToGEODB(system.file(getwd(), 'GEOmetadb.sqlite'))
#  
#  #Showing the experiment types available in GEO
#  experiments <- experiment_types(geo_con)
#  
#  #Showing the organism types available in GEO
#  species <- organism_types(geo_con)
#  
#  #Retrieving the metadata associated to experiment type "Methylation profiling by high througput sequencing"
#  meth_metadata <- getGEOMetadata(geo_con, experiment_type='Methylation profiling by high throughput sequencing', organism = 'Homo sapiens')
#  
#  #Retrieving Human gene expression metadata, knowing the GEO platform identifier, e.g. the Affymetrix Human Genome U133 Plus 2.0 Array
#  expression <- getGEOMetadata(geo_con, experiment_type='Expression profiling by array', gpl='GPL570')

## ----experimentTypesshow, echo=FALSE, eval=TRUE--------------------------
experiments <- readRDS(system.file('extdata', 'vignette_data', 'experiment_types.rds', package='Onassis'))
knitr::kable(experiments[1:10], rownames=FALSE, 
              caption = htmltools::tags$caption(
                style = 'caption-side: top-left; text-align: left;',
                'Table 1: ', htmltools::em('Experiments available in GEO')),
              options=list(
                pageLength =5,
                autoWidth = TRUE, 
                scrollX='300px',
                rownames=FALSE))



## ----speciesShow, echo=FALSE,eval=TRUE-----------------------------------
species <- readRDS(system.file('extdata', 'vignette_data', 'organisms.rds', package='Onassis'))
knitr::kable(species[1:10], rownames=FALSE, 
              caption = htmltools::tags$caption(
                style = 'caption-side: top-left; text-align: left;',
                'Table 1: ', htmltools::em('Species available in GEO')),
              options=list(
                pageLength =5,
                autoWidth = TRUE, 
                scrollX='300px',
                rownames=FALSE))

## ----loadgeoMetadata, echo=TRUE, eval=TRUE-------------------------------
meth_metadata <- readRDS(system.file('extdata', 'vignette_data', 'GEOmethylation.rds', package='Onassis'))

## ----printmeta, echo=FALSE,eval=TRUE-------------------------------------

methylation_tmp <- meth_metadata
methylation_tmp$experiment_summary <- sapply(methylation_tmp$experiment_summary, function(x) substr(x, 1, 50))
knitr::kable(methylation_tmp[1:10,], rownames=FALSE, 
              caption = htmltools::tags$caption(
                style = 'caption-side: top-left; text-align: left;',
                'Table 1: ', htmltools::em('Methylation profiling by high througput sequencing metadata from GEOmetadb.')),
              options=list(
                pageLength =5,
                autoWidth = TRUE, 
                scrollX='300px',
                rownames=FALSE))
              #  columnDefs = list(list(targets=10,
              #                         render = JS(
              #                           "function(data, type, row, meta) {",
              #                           "return type === 'display' && data.length > 50 ?",
              #                           "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
              #                           "}")
              #  )))), callback = JS('table.page("next").draw(false);'))


## ----connectSRA, echo=TRUE,eval=FALSE------------------------------------
#  # Connection to the SRAmetadb and potential download of the sqlite file
#  sqliteFileName <- './data/SRAdb.sqlite'
#  sra_con <- dbConnect(SQLite(), sqliteFileName)()
#  
#  # Query for the ChIP-Seq experiments contained in GEO for human samples
#  library_strategy <- 'ChIP-Seq'
#  library_source='GENOMIC'
#  taxon_id=9606
#  center_name='GEO'
#  
#  samples_query <- paste0("select sample_accession, description, sample_attribute, sample_url_link from sample where taxon_id='", taxon_id, "' and sample_accession IS NOT NULL", " and center_name='", center_name, "'",  )
#  
#  samples_df <- dbGetQuery(sra_con, samples_query)
#  samples <- unique(as.character(as.vector(samples_df[, 1])))
#  
#  
#  
#  experiment_query <- paste0("select experiment_accession, center_name, title, sample_accession, sample_name, experiment_alias, library_strategy, library_layout, experiment_url_link, experiment_attribute from experiment where library_strategy='",
#                             library_strategy, "'" , " and library_source ='", library_source,
#                             "' " )
#  
#  experiment_df <- dbGetQuery(sra_con, experiment_query)
#  
#  experiment_df <- merge(experiment_df, samples_df, by = "sample_accession")
#  experiment_df$experiment_attribute <- sapply(experiment_df$experiment_attribute,
#                                               function(value) {
#                                                 gsub("||", "  ", value)
#                                               })
#  experiment_df$sample_attribute <- sapply(experiment_df$sample_attribute,
#                                           function(value) {
#                                             gsub("||", "  ", value)
#                                           })
#  experiment_df$sample_name <- sapply(experiment_df$sample_name,
#                                      function(value) {
#                                        gsub("_", " ", value)
#                                      })
#  experiment_df$experiment_alias <- sapply(experiment_df$experiment_alias,
#                                           function(value) {
#                                             gsub("_", " ", value)
#                                           })
#  sra_chip_seq <- experiment_df
#  

## ----readCHIP, echo=TRUE, eval=TRUE--------------------------------------
sra_chip_seq <- readRDS(system.file('extdata', 'vignette_data', 'GEO_human_chip.rds',  package='Onassis'))

## ----printchromatinIP, echo=FALSE,eval=TRUE------------------------------

knitr::kable(head(sra_chip_seq, 10), rownames=FALSE, 
  caption = htmltools::tags$caption(
    style = 'caption-side: top-left; text-align: left;',
    'Table: ', htmltools::em('ChIP-Seq metadata obtained from SRAdb.')),
  options=list(
  pageLength =5,
  autoWidth = TRUE, 
  scrollX='300px',
  rownames=FALSE))#, 
#columnDefs = list(list(targets=9,
#           render = JS(
#    "function(data, type, row, meta) {",
#    "return type === 'display' && data.length > 50 ?",
#    "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
#    "}")
#))), callback = JS('table.page("next").draw(false);'))
                       

## ----createSampleAndTargetDict, echo=TRUE,eval=TRUE, message=FALSE-------
# If a Conceptmapper dictionary is already available the dictType CMDICT can be specified and the corresponding file loaded
sample_dict <- CMdictionary(inputFileOrDb=system.file('extdata', 'cmDict-sample.cs.xml', package = 'Onassis'), dictType = 'CMDICT')

#Creation of a dictionary from the file sample.cs.obo available in OnassisJavaLibs
obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')

sample_dict <- CMdictionary(inputFileOrDb=obo, outputDir=getwd(), synonymType='ALL')

# Creation of a dictionary for human genes/proteins
require(org.Hs.eg.db)
targets <- CMdictionary(dictType='TARGET', inputFileOrDb = 'org.Hs.eg.db')

## ----settingOptions, echo=TRUE,eval=TRUE---------------------------------

#Showing configuration permutations 
opts <- CMoptions()  
show(opts)
combinations <- listCMOptions()
a <- CMoptions(SynonymType = 'EXACT_ONLY')
a
#Changing the SearchStrategy parameter
SearchStrategy(a) <- 'SKIP_ANY_MATCH_ALLOW_OVERLAP'
a

