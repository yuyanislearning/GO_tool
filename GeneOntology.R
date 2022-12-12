# for analysis on GO term
library(httr)
library(jsonlite)
library(xml2)

# get root node's children
get_root_children = function(){
  requestURL <- "https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/GO%3A0008150/children"
  r <- GET(requestURL, accept("application/json"))
  
  stop_for_status(r)
  
  json <- toJSON(content(r))
  root = fromJSON(json)
  childs = root$results$children[[1]]
  childs = childs %>% filter(relation=='is_a')
  return(childs)
}

# assign the second root BP
assign_large_bp = function(id, childs){
  requestURL <- paste0("https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/GO%3A", id,"/ancestors?relations=is_a")
  r <- GET(requestURL, accept("application/json"))
  
  stop_for_status(r)
  
  json <- toJSON(content(r))
  res = fromJSON(json)
  res = res$results$ancestors
  nid = intersect(res[[1]][,1], unlist(childs$id))
  return(unlist(childs$name)[which(unlist(childs$id)==nid)])
}

trace_ancesstry = function(id){
  # Get ancestry
  requestURL <- paste0("https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/GO%3A", id,"/ancestors?relations=is_a")
  r <- GET(requestURL, accept("application/json"))
  
  stop_for_status(r)
  json <- toJSON(content(r))
  res = fromJSON(json)
  res = res$results$ancestors[[1]][,1]
  
  # Get the name of ancestry
  res = paste(res, collapse='%2C')
  res = sub(':', '%3A', res)
  requestURL <- paste0("https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/", res)
  r <- GET(requestURL, accept("application/json"))
  
  stop_for_status(r)
  json <- toJSON(content(r))
  res = fromJSON(json)
  out = data.frame(id=unlist(res$results$id), 
                   name=unlist(res$results$name))
  return(out)
}
