% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllConstructors.R
\name{EntityFinder}
\alias{EntityFinder}
\title{\code{EntityFinder}}
\usage{
EntityFinder(input, dictionary, options = NA, outDir = tempdir(),
  multipleDocs = FALSE)
}
\arguments{
\item{input}{the file, directory, or data frame with the text to annotate}

\item{dictionary}{A dictionary of the type \code{\link{CMdictionary}} or the path to an already created Conceptmapper XML file}

\item{options}{an object of class \code{\link{CMoptions}}. If NA, the default configuration will be set.}

\item{outDir}{the directory where the annotated files will be stored}

\item{multipleDocs}{TRUE when multiple documents are loaded from a single file with each row representing a document. The file should have two columns. The first for the unique document identifier and the second for the textual descriptions}
}
\value{
dataframe of annotations
}
\description{
this function creates instances of the class \code{\link{EntityFinder}}
}
\examples{
obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
sample_dict <- CMdictionary(input=obo, outputDir=getwd(), synonymType='ALL')
myopts <- CMoptions()
paramValueIndex(myopts) <- 40
entities <- EntityFinder(input=readRDS(system.file('extdata', 'vignette_data', 'GEO_human_chip.rds', package='Onassis')), dictionary=sample_dict, options=myopts)
}
