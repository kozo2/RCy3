# ==============================================================================
# Functions for working with FILTERS for the selection of nodes and edges in 
# networks, including operations to import and export filters. In the Cytoscape
# user interface, filters are managed in the Select tab of the Control Panel.
#
# ==============================================================================
#' @title Apply Filter
#'
#' @description Run an existing filter by supplying the filter name.
#' @param filter.name Name of filter to apply. Default is "Default filter".
#' @param hide Whether to hide filtered out nodes and edges. Default is FALSE.
#' Ignored if all nodes or edges are filtered out. This is an alternative to 
#' filtering for node and edge selection.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of selected nodes and edges.
#' @details Known bug: selection (or hiding) of edges using edge-based column 
#' filters does not work. As a workaround, simply repeat the createColumnFilter
#' operation to perform selection (or hiding) of edges.
#' @examples \donttest{
#' applyFilter('myFilter')
#' applyFilter('myFilter', hide = TRUE)
#' }
#' @seealso unhideAll
#' @importFrom RJSONIO toJSON
#' @export
applyFilter<-function(filter.name="Default filter", hide=FALSE, network=NULL, 
                      base.url = .defaultBaseUrl){
    
    if(!filter.name %in% getFilterList(base.url))
        stop (sprintf("Filter %s does not exist.",filter.name))
    
    net.SUID <- getNetworkSuid(network,base.url)
    
    cmd.container <- paste('container', 'filter', sep='=')
    cmd.name <- paste('name',filter.name,sep='=')
    cmd.network <- paste('network=SUID',net.SUID, sep=':')
    
    commandsPOST(paste('filter apply',
                       cmd.container,
                       cmd.name,
                       cmd.network, 
                       sep=' '), base.url)
    
    sel.nodes<-getSelectedNodes(network, base.url)
    sel.edges<-getSelectedEdges(network, base.url)
    
    if(hide) {
        unhideAll(network, base.url)
        if(!is.na(sel.nodes[1]))
            hideNodes(invertNodeSelection(network, base.url)$nodes)
        if(!is.na(sel.edges[1]))
            hideEdges(invertEdgeSelection(network, base.url)$edges)
    }
    
    return(list(nodes=sel.nodes, edges=sel.edges))
}

# ------------------------------------------------------------------------------
#' @title Create Column Filter
#'
#' @description Creates a filter to control node or edge selection. Works on
#' columns of boolean, string, numeric and lists. Note the unique restrictions
#' for criterion and predicate depending on the type of column being filtered.
#' @param filter.name Name for filter.
#' @param column Table column to base filter upon.
#' @param criterion  For boolean columns: TRUE or FALSE. For string columns: a 
#' string value, e.g., "hello". If the predicate is REGEX then this can be a 
#' regular expression as accepted by the Java Pattern class 
#' (https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html). For 
#' numeric columns: If the predicate is BETWEEN or IS_NOT_BETWEEN then this is 
#' a two-element vector of numbers, example: c(1,5), otherwise a single number.
#' @param predicate  For boolean columns: IS, IS_NOT. For string columns: IS, 
#' IS_NOT, CONTAINS, DOES_NOT_CONTAIN, REGEX. For numeric columns: IS, IS_NOT, 
#' GREATER_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN, LESS_THAN_OR_EQUAL, BETWEEN, 
#' IS_NOT_BETWEEN
#' @param caseSensitive (optional) If string matching should be case sensitive. 
#' Default is FALSE.
#' @param anyMatch (optional) Only applies to List columns. If true then at least 
#' one element in the list must pass the filter, if false then all the elements 
#' in the list must pass the filter. Default is TRUE.
#' @param type (optional) Apply filter to "nodes" (default) or "edges".
#' @param hide Whether to hide filtered out nodes and edges. Default is FALSE.
#' Ignored if all nodes or edges are filtered out. This is an alternative to 
#' filtering for node and edge selection.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of selected nodes and edges.
#' @examples \donttest{
#' createColumnFilter('myFilter', 'log2FC', c(-1,1), "IS_NOT_BETWEEN")
#' createColumnFilter('myFilter', 'pValue', 0.05, "LESS_THAN")
#' createColumnFilter('myFilter', 'function', "kinase", "CONTAINS", FALSE)
#' createColumnFilter('myFilter', 'name', "^Y.*C$", "REGEX")
#' createColumnFilter('myFilter', 'isTarget', TRUE , "IS")
#' createColumnFilter('myFilter', 'isTarget', TRUE , "IS", hide = TRUE)
#' }
#' @importFrom RJSONIO fromJSON
#' @export
createColumnFilter<-function(filter.name, column, criterion, predicate, 
                             caseSensitive=FALSE, anyMatch=TRUE, 
                             type="nodes", hide = FALSE, network = NULL,
                             base.url = .defaultBaseUrl){
    
    if(!column %in% getTableColumnNames(substr(type,1,4), base.url = base.url))
        stop (sprintf("Column %s does not exist in the %s table", column, substr(type,1,4)))
    
    if(predicate %in% c("BETWEEN","IS_NOT_BETWEEN")){
        if(!length(criterion)==2)
            stop ("criterion must be a list of two numeric values, e.g., c(0.5,2.0)")
    } else if (predicate %in% c("GREATER_THAN", "GREATER_THAN_OR_EQUAL")){
    # manually feed max bound so that UI is also correct    
        col.vals <- getTableColumns(substr(type,1,4), column, base.url = base.url)
        crit.max <- max(na.omit(col.vals))
        criterion <- c(criterion[1], crit.max)
    # same trick to fix UI does not work for LESS_THAN cases
    # } else if (predicate %in% c("LESS_THAN", "LESS_THAN_OR_EQUAL")){
    #     col.vals <- getTableColumns(substr(type,1,4), column, base.url = base.url)
    #     crit.max <- min(na.omit(col.vals))
    #     criterion <- c(crit.max,criterion[1])
    } else if (is.numeric(criterion[1]) & predicate == "IS"){
        criterion <- c(criterion[1],criterion[1])
        predicate <- "BETWEEN"
    } else if (is.numeric(criterion[1]) & predicate == "IS_NOT"){
        criterion <- c(criterion[1],criterion[1])
        predicate <- "IS_NOT_BETWEEN"
    }else if (is.logical(criterion[1]) & predicate == "IS_NOT"){
        criterion <- !criterion
    }
    
    cmd.name <- paste0('name="',filter.name,'"')
    cmd.json <- list(id="ColumnFilter", parameters=list(criterion=criterion, 
                                                        columnName=column,
                                                        predicate=predicate,
                                                        caseSensitive=caseSensitive,
                                                        anyMatch=anyMatch,
                                                        type=type))
    cmd.body <- toJSON(list(name=filter.name, json=cmd.json))
    
    .postCreateFilter(cmd.body, base.url)
 
    commandSleep(1) #Yikes! Have to wait a second for selection to settle!
    
    sel.nodes<-getSelectedNodes(network, base.url)
    sel.edges<-getSelectedEdges(network, base.url)
    
    if(hide) {
        unhideAll(network, base.url)
        if(!is.na(sel.nodes[1]))
            hideNodes(invertNodeSelection(network, base.url)$nodes)
        if(!is.na(sel.edges[1]))
            hideEdges(invertEdgeSelection(network, base.url)$edges)
    }
    
    return(list(nodes=sel.nodes, edges=sel.edges))
}
# ------------------------------------------------------------------------------
#' @title Create Composite Filter
#'
#' @description Combines filters to control node and edge selection based on
#' previously created filters.
#' @param filter.name Name for filter.
#' @param filter.list List of filters to combine.
#' @param type (optional) Type of composition, requiring ALL (default) or ANY
#' filters to pass for final node and edge selection.
#' @param hide Whether to hide filtered out nodes and edges. Default is FALSE.
#' Ignored if all nodes or edges are filtered out. This is an alternative to 
#' filtering for node and edge selection.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of selected nodes and edges.
#' @examples \donttest{
#' createCompositeFilter("comp1", c("filter1", "filter2"))
#' createCompositeFilter("comp2", c("filter1", "filter2"), "ANY")
#' createCompositeFilter("comp3", c("comp1", "filter3"))
#' }
#' @importFrom RJSONIO fromJSON
#' @export
createCompositeFilter<-function(filter.name, filter.list, type="ALL", 
                                hide = FALSE, network = NULL,
                                base.url = .defaultBaseUrl){
    
    if(!length(filter.list)>1)
        stop ('Must provide a list of two or more filter names, e.g., c("filter1", "filter2")')
    
    trans.list <- lapply(filter.list, function(x) .getFilterJson(x,base.url)[[1]]$transformers[[1]])
    
    #return(trans.list)
    cmd.json <- list(id="CompositeFilter", parameters=list(type=type), transformers=trans.list)
    cmd.body <- toJSON(list(name=filter.name, json=cmd.json))

    .postCreateFilter(cmd.body, base.url)

    sel.nodes<-getSelectedNodes(network, base.url)
    sel.edges<-getSelectedEdges(network, base.url)
    
    if(hide) {
        unhideAll(network, base.url)
        if(!is.na(sel.nodes[1]))
            hideNodes(invertNodeSelection(network, base.url)$nodes)
        if(!is.na(sel.edges[1]))
            hideEdges(invertEdgeSelection(network, base.url)$edges)
    }
    
    return(list(nodes=sel.nodes, edges=sel.edges))
}

# ------------------------------------------------------------------------------
#' @title Create Degree Filter
#'
#' @description Creates a filter to control node selection base on in/out degree.
#' @param filter.name Name for filter.
#' @param criterion  A two-element vector of numbers, example: c(1,5).
#' @param predicate BETWEEN (default) or IS_NOT_BETWEEN
#' @param edgeType (optional) Type of edges to consider in degree count: 
#' ANY (default), UNDIRECTED, INCOMING, OUTGOING, DIRECTED
#' @param hide Whether to hide filtered out nodes and edges. Default is FALSE.
#' Ignored if all nodes or edges are filtered out. This is an alternative to 
#' filtering for node and edge selection.
#' @param network (optional) Name or SUID of the network. Default is the 
#' "current" network active in Cytoscape.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of selected nodes and edges.
#' @examples \donttest{
#' createDegreeFilter('myFilter', c(2,5))
#' }
#' @importFrom RJSONIO fromJSON
#' @export
createDegreeFilter<-function(filter.name, criterion, predicate="BETWEEN", 
                             edgeType="ANY", hide = FALSE, network = NULL,
                             base.url = .defaultBaseUrl){
    
    if(!length(criterion)==2)
        stop ("criterion must be a list of two numeric values, e.g., c(2,5)")
    
    cmd.name <- paste0('name="',filter.name,'"')
    cmd.json <- list(id="DegreeFilter", parameters=list(criterion=criterion, 
                                                        predicate=predicate,
                                                        edgeType=edgeType))
    cmd.body <- toJSON(list(name=filter.name, json=cmd.json))
    
    .postCreateFilter(cmd.body, base.url)
    
    sel.nodes<-getSelectedNodes(network, base.url)
    sel.edges<-getSelectedEdges(network, base.url)
    
    if(hide) {
        unhideAll(network, base.url)
        if(!is.na(sel.nodes[1]))
            hideNodes(invertNodeSelection(network, base.url)$nodes)
        if(!is.na(sel.edges[1]))
            hideEdges(invertEdgeSelection(network, base.url)$edges)
    }
    
    return(list(nodes=sel.nodes, edges=sel.edges))
}

# ------------------------------------------------------------------------------
#' @title Get Filter List
#'
#' @description Retrieve list of named filters in current session.
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return List of filter names
#' @examples \donttest{
#' getFilterList()
#' }
#' @export
getFilterList<-function(base.url=.defaultBaseUrl){
    commandsPOST('filter list', base.url = base.url)
}

# ------------------------------------------------------------------------------
#' @title Export Filters
#'
#' @description Saves filters to file in JSON format.
#' @param filename (char) Path and name of the filters file to save. Default is 
#' "filters.json"
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Path to saved file
#' @examples \donttest{
#' exportFilters()
#' }
#' @export
exportFilters<-function(filename = "filters.json", base.url = .defaultBaseUrl){
    commandsGET(paste0('filter export file="',filename,'"'),base.url)
}

# ------------------------------------------------------------------------------
#' @title Import Filters
#'
#' @description Loads filters from a file in JSON format.
#' @param filename (char) Path and name of the filters file to load. 
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return Path to saved file
#' @examples \donttest{
#' importFilters()
#' }
#' @export
importFilters<-function(filename , base.url = .defaultBaseUrl){
    commandsGET(paste0('filter import file="',filename,'"'),base.url)
}

# ------------------------------------------------------------------------------
# Internal function to process special json list syntax with pesky single quotes. This
# is an alternative to commandsPOST.
#' @importFrom httr POST
#' @importFrom httr content_type_json
.postCreateFilter<-function(cmd.body, base.url){
    cmd.url <- paste0(base.url, '/commands/filter/create')
    cmd.body <- gsub("json\": {\n", "json\": \\'{\n", cmd.body, perl = TRUE)
    cmd.body <- gsub("\n} \n} \n}", "\n} \n}\\' \n}", cmd.body, perl = TRUE)
    cmd.body <- gsub("\n] \n} \n}", "\n] \n}\\' \n}", cmd.body, perl = TRUE) #for createCompositeFilter
    
    res=POST(url=cmd.url, body=cmd.body, encode="json", content_type_json())
    if(res$status_code > 299){
        write(sprintf("RCy3::.postCreateFilter, HTTP Error Code: %d\n url=%s\n body=%s", 
                      res$status_code, URLencode(cmd.url), cmd.body), stderr())
        stop(fromJSON(rawToChar(res$content))$errors[[1]]$message)
    } 
}

# ------------------------------------------------------------------------------
# Internal function to get filters as JSON for constructing composite filters.
.getFilterJson<-function(filter.name, base.url){
    commandsPOST(paste0('filter get name="',filter.name,'"'),
                 base.url = base.url)
}