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
#  sra_con <- connectToSRADB()
#  
#  # Query for the ChIP-Seq experiments contained in GEO for human samples
#  sra_chip_seq <- getSRAMetadata(sra_con, library_strategy='ChIP-Seq', library_source='GENOMIC', taxon_id=9606, center_name='GEO')
#  
#  # The following example allows to retrieve Bisulfite sequencing samples' metadata.
#  bisulfite_seq <- getSRAMetadata(sra_con, library_strategy='Bisulfite-Seq', library_source='GENOMIC', taxon_id=9606, center_name='GEO' )
#  
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
sample_dict <- dictionary(inputFileOrDb=system.file('extdata', 'cmDict-sample.cs.xml', package = 'Onassis'), dictType = 'CMDICT')

#Creation of a dictionary from the file sample.cs.obo available in OnassisJavaLibs
obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')

sample_dict <- dictionary(inputFileOrDb=obo, outputdir=getwd(), synonymType='ALL')

# Creation of a dictionary for human genes/proteins
require(org.Hs.eg.db)
targets <- dictionary(dictType='TARGET', inputFileOrDb = 'org.Hs.eg.db')

## ----settingOptions, echo=TRUE,eval=TRUE---------------------------------

#Showing configuration permutations 
opts <- CMoptions()  
combinations <- listCombinations(opts)
#Setting the combination of parameters through the paramValueIndex value
myopts <- CMoptions(c("CONTIGUOUS_MATCH", "CASE_INSENSITIVE", 'BIOLEMMATIZER', 'PUBMED', 'ON', 'YES', 'ALL'))


## ----annotateDF, echo=TRUE, eval=TRUE------------------------------------
chipseq_dict_annot <- annotate(sra_chip_seq[1:20,c('sample_accession', 'title', 'experiment_attribute', 'sample_attribute', 'description')], dictionary=sample_dict, options=myopts)


## ----showchipresults, echo=FALSE, eval=TRUE, message=FALSE---------------

#methylation_brenda_annot <- readRDS(system.file('extdata', 'vignette_data', 'methylation_brenda_annot.rds', package='Onassis'))
#UPDATE con ChIP-seq
knitr::kable(head(chipseq_dict_annot, 20), rownames=FALSE, 
  caption = htmltools::tags$caption(
    style = 'caption-side: top-left; text-align: left;',
    'Table: ', htmltools::em('Annotations of the methylation profiling by high througput sequencing metadata obtained from GEO with BRENDA ontology concepts')),
  options=list(
  pageLength =10,
  autoWidth = TRUE, 
  scrollX='300px',
  rownames=FALSE))#, 
#columnDefs = list(list(targets=1,
#           render = JS(
#    "function(data, type, row, meta) {",
#    "return type === 'display' && data.length > 50 ?",
#    "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
#    "}")
#))), callback = JS('table.page("next").draw(false);'))
                 
    

## ----annotateGenes, echo=TRUE, eval=TRUE, message=FALSE------------------

#Finding the TARGET entities
target_entities <- annotate(inputFileorDf=sra_chip_seq[1:20,c('sample_accession', 'title', 'experiment_attribute', 'sample_attribute', 'description')], options = myopts, dictionary=targets) 

## ----printKable, echo=FALSE, eval=TRUE-----------------------------------
knitr::kable(target_entities, rownames=FALSE, 
  caption = htmltools::tags$caption(
    style = 'caption-side: top-left; text-align: left;',
    'Table: ', htmltools::em('Annotations of ChIP-seq test metadata obtained from SRAdb and stored into files with the TARGETs (genes and histone variants)')),
  options=list(
  pageLength =10,
  autoWidth = TRUE, 
  scrollX='100px',
  rownames=FALSE, 
columnDefs = list(list(targets= c(0,1,2,3,4),
           render = JS(
    "function(data, type, row, meta) {",
    "return type === 'display' && data.length > 50 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
    "}")
))), callback = JS('table.page("next").draw(false);'))


## ----similarity, echo=TRUE, eval=TRUE, message=FALSE---------------------
#Instantiating the Similarity
similarities <- showSimilarities()

## ----computing measures, echo=TRUE, eval=TRUE, message=FALSE-------------

found_terms <- unique(chipseq_dict_annot$term_url)
n <- length(found_terms)

ontologyfile <- obo
pairwise_results <- data.frame(term1 = character(0), term2= character(0), value = double(0L))
for(i in 1:(n-1)){
  term1 <- as.character(found_terms[i])
  j = i + 1 
  for(k in j:n){
    term2 <- as.character(found_terms[k])
    two_term_similarity <- similarity(ontologyfile,  term1, term2 )
    new_row <- cbind(term1, term2, two_term_similarity)
    pairwise_results <- rbind(pairwise_results, new_row )
  }
}
pairwise_results <- unique(pairwise_results)
pairwise_results <- merge(pairwise_results, chipseq_dict_annot[, c('term_url', 'term_name')], by.x='term2', by.y='term_url', all.x=TRUE)
colnames(pairwise_results)[length(colnames(pairwise_results))] <- 'term2_name'
pairwise_results <- merge(pairwise_results, chipseq_dict_annot[, c('term_url', 'term_name')], by.x='term1', by.y='term_url', all.x=TRUE)
colnames(pairwise_results)[length(colnames(pairwise_results))] <- 'term1_name'
pairwise_results <- unique(pairwise_results)


## ----showing_similarity1, echo=FALSE, eval=TRUE, message=FALSE-----------


knitr::kable(pairwise_results, rownames=FALSE, 
  caption = htmltools::tags$caption(
    style = 'caption-side: top-left; text-align: left;',
    'Table: ', htmltools::em('Pairwise similarities of cell line terms annotating the ChIP-seq metadata')),
  options=list(
  pageLength =10,
  autoWidth = TRUE, 
  scrollX='100px',
  rownames=FALSE))#, 
#columnDefs = list(list(targets= 1,
#           render = JS(
#    "function(data, type, row, meta) {",
#    "return type === 'display' && data.length > 50 ?",
#    "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
#    "}")
#))), callback = JS('table.page("next").draw(false);'))


## ----groupwise_measures, echo=TRUE, eval=TRUE, message=FALSE-------------

similarity(obo, found_terms[1:2], found_terms[3])


## ----samples_similarity, echo=TRUE, eval=TRUE, message=FALSE-------------

annotated_samples <- as.character(as.vector(unique(chipseq_dict_annot$sample_id)))
n <- length(annotated_samples)


samples_results <- data.frame(sample1 = character(0), sample2= character(0), value = double(0L))
samples_results <- matrix(0, nrow=n, ncol=n)
rownames(samples_results) <- colnames(samples_results) <- annotated_samples
for(i in 1:(n-1)){
  sample1 <- as.character(annotated_samples[i])
  j = i + 1 
  for(k in j:n){
    sample2 <- as.character(annotated_samples[k])
    two_samples_similarity <- similarity(ontologyfile, sample1, sample2, chipseq_dict_annot)
    samples_results[i, k] <- samples_results[k, i] <- two_samples_similarity
  }
}
diag(samples_results) <- 1
heatmap.2(samples_results, density.info = "none", trace="none", main='Semantic similarity of annotated samples', margins=c(5,5))

