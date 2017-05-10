# Calling the java garbage collector
#' @importFrom rJava .jcall
jgc <- function() {
    .jcall("java/lang/System", method = "gc")
}


# This method reads the .a1 files obtained from conceptmapper execution and converts them into a data frame
#' @importFrom utils read.table tail
loadEntities <- function(entityDirectory, deleteDir = TRUE) {
    if (!dir.exists(entityDirectory))
        stop("Unvalid directory or file provided")
    entityfiles <- list.files(entityDirectory, pattern = ".a1$")
    entity_table <- lapply(entityfiles, function(entityfile) {
        if (file.info(file.path(entityDirectory, entityfile))$size != 0) {
            fromerror = FALSE
            tryCatch({
                x <- read.table(file.path(entityDirectory, entityfile), header = FALSE, sep = "\t", as.is = TRUE)
                even <- seq(1, nrow(x), 2)
                odd <- seq(2, nrow(x), 2)
                sample_id <- strsplit(entityfile, ".a1")[[1]][1]
                term_id <- sub(".*/", "", x[odd, 2])
                matched_sentence <- as.character(as.vector(x[even, 3]))
                term_name <- x[odd, 3]
                if (grepl("_", term_id[1])) {
                  splitted_id <- tail(strsplit(term_id, "_")[[1]], 2)
                  value <- splitted_id[2]
                  namespace <- splitted_id[1]
                  term_url <- paste0("http://purl.obolibrary.org/obo/", namespace, "_", value)
                } else {
                  term_url = "NA"
                }
                final <- unique(cbind(sample_id, term_id, term_name, term_url, matched_sentence))


            }, error = function(e) {
                final <- parseLineByLine(file.path(entityDirectory, entityfile), entityfile)
                final
            })
        }
    })
    if (!is.null(unlist(entity_table))) {
        entity_df <- do.call("rbind", entity_table)
        rownames(entity_df) <- 1:nrow(entity_df)
        if (deleteDir) {
            sapply(entityfiles, function(entity_file) file.remove(file.path(entityDirectory, entity_file)))
        }
        entity_df <- as.data.frame(entity_df)
        entity_df <- entity_df[which(sapply(entity_df$matched_sentence, nchar) >= 3), ]
        return(unique(as.data.frame(entity_df)))
    } else return(NULL)
}


# This method is called from the other method when it is impossible to read the table and the file has to be read linbe by line
parseLineByLine <- function(cmfile, entityfile) {
    conn <- file(cmfile, open = "r")
    lines <- readLines(conn)
    description_line <- ""
    splitted_line <- ""
    termi_id <- ""
    position <- ""
    ref_line <- ""
    options(stringsAsFactors = FALSE)
    sample_id <- strsplit(entityfile, ".a1")[[1]][1]
    entity_table <- data.frame()
    for (i in 1:length(lines)) {
        if (grepl("^T[0-9]", lines[i])) {
            splitted_line <- strsplit(lines[i], "\t")
            term_id <- splitted_line[[1]][1]
            position <- splitted_line[[1]][2]
            description_line <- paste0(splitted_line[[1]][3:length(splitted_line[[1]])])

        } else if (grepl("N[0-9]", lines[i])) {
            ref_line <- strsplit(lines[i], "\t")

            second_term_id <- ref_line[[1]][1]
            reference_sentence <- ref_line[[1]][2]
            term_url <- ""
            if (grepl("/", reference_sentence))
                term_id <- sub(".*/", "", reference_sentence)
            if (grepl("_", term_id)) {
                splitted_id <- tail(strsplit(term_id, "_")[[1]], 2)

                value <- splitted_id[2]
                namespace <- splitted_id[1]
                term_url <- paste0("http://purl.obolibrary.org/obo/", namespace, "_", value)
            } else {
                term_id <- tail(strsplit(reference_sentence, split = " ")[[1]], 1)
                if (grepl("_", term_id)) {
                  splitted_id <- tail(strsplit(term_id, "_")[[1]], 2)

                  value <- splitted_id[2]
                  namespace <- splitted_id[1]
                  term_url <- paste0("http://purl.obolibrary.org/obo/", namespace, "_", value)
                }
            }
            term_name <- ref_line[[1]][3]
            table_row <- cbind(sample_id, term_id, term_name, term_url, description_line)
            entity_table <- rbind(entity_table, table_row)


        } else {
            trimmed_line <- gsub("\\t", " ", lines[i])
            description_line <- paste0(description_line, " ", trimmed_line)
        }
    }
    close(conn)
    colnames(entity_table) <- c("sample_id", "term_id", "term_name", "term_url", "matched_sentence")
    return(entity_table)
}