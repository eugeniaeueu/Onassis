# Onassis
Ontology Annotations and Semantic Similarity Software

<b>OnASSiS</b> (Ontology Annotations and Semantic Similarity software) is a package that uses Conceptmapper, an Apache UIMA (Unstructured Information Management Architecture) dictionary lookup tool to retrieve dictionary terms in a given text. https://uima.apache.org/downloads/sandbox/ConceptMapperAnnotatorUserGuide/ConceptMapperAnnotatorUserGuide.html  
In particular, a Conceptmapper wrapper, specific for the biomedical domain, ccp-nlp, (https://github.com/UCDenver-ccp/ccp-nlp) has been personalized to retrieve concepts from OBO ontologies in a given text with different settable options.
The package also provides the possibility to annotate Gene Expression Omnibus (GEO) metadata retrieved using `r Biocpkg("GEOmetadb")` or hight througput sequencing metadata from `r Biocpkg("SRAdb")` with concepts from any given ontology. It is also possible to annotate genes, histone marks and histone modifications.

The semantic similarity module uses different semantic similarity measures to determine the semantic similarity of concepts in a given ontology, groups of concepts or experiments and samples annotated with ontology terms. 
This step is particularly useful for a better retrieval of data of interest from public repositories such as GEO http://www.ncbi.nlm.nih.gov/geo/ or SRA http://www.ncbi.nlm.nih.gov/sra. 
The semantic similarity module has been developed on the basis of the Java slib http://www.semantic-measures-library.org/sml.
To run Onassis Java (>= 1.8) is needed. 
