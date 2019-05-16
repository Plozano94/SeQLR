
extract_query <- function(route, name) {
    content <- paste(readLines(route, warn = FALSE, skipNul = TRUE), collapse = "\n")
    queries <- paste0("SeQLR/", strsplit(content, "SeQLR/")[[1]])
    queries <- queries[2:length(queries)]
    extract <- function(querie) {
        q <- stringr::str_replace_all(strsplit(querie, "\\\\SeQLR")[[1]][2], "\n", "") %>% stringr::str_replace_all("-", "")
        metadata <- rjson::fromJSON(stringr::str_match_all(querie, "(?<=SeQLR/)(.*)(?=\\\\SeQLR)")[[1]][, 1])
        returned_list <-
            structure(
                list(
                    q[[1]],
                    metadata["Name"][[1]],
                    metadata$param
                ),
                names = c("query", "name", "param")
            )
        returned_list
    }
    sqldata <- lapply(queries, extract)

    # Selecting chunk
    cond <- lapply(sqldata, function(x) x$name == name)
    query_selected <- sqldata[unlist(cond)][[1]]
    return(query_selected)
}

format_query <- function(query_selected) {
    if (!is.null(query_selected$param)) {
        for (key in names(query_selected$param)) {
            query = stringr::str_replace_all(query_selected$query, paste0("\\$\\{", key, "\\}"), query_selected$param[[key]])
        }
    }
    return(query)
}

execute_query <- function(route, name, impala = FALSE, params = NULL) {
    query_selected <- extract_query(route, name)
    query <- format_query(query_selected)
    print(query)
    # con <- dbConnect(dbDriver('MySQL'), user = 'testuser', password='testpassword', dbname = 'seqlr')
    return(send_query(query))
}
