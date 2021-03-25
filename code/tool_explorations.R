# code to keep track of tools I'm playing with and test them out

# install.packages("natserv")
library(natserv)
library(tidyverse)
# vignette(package ="natserv")
# View(ns_search_spp(text ="Triepeolus"))
ns_search_spp(text ="Triepeolus") # first result is 20x15
?ns_search_spp
View(ns_search_spp(text_adv = list(searchToken ="Andrena", matchAgainst ="scientificName", operator ="contains"), location = list(nation ="US", subnation ="MD")))


#play with the code and get what I want: a dataframe with 1 row per location
# ideally returns a row for national status when requested and a different row
# for any subnational status requested, with attendent status

# reminder about special fucntions from natserv: This seems like a clever way to deal with optional arguments, come back to this.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
`%|lst|%` <- function(x, y) if (length(unlist(x)) == 0) y else x

# how does natserv:::ns_POST work?
# looks like it is basically a way to re-write R into json syntax and make the
# NatureServe request

function (url, body, query = list(), ...)
{
  cli <- crul::HttpClient$new(url = url, opts = list(...),
                              headers = list(Accept = "application/json"))
  temp <- cli$post(body = body, query = query, encode = "json")
  temp$raise_for_status()
  x <- temp$parse("UTF-8")
  return(x)
}

# params and their arguments
text = NULL
text_adv = list(searchToken ="Andrena", matchAgainst ="scientificName", operator ="contains")
status = NULL
paramType = "nation"
nation = "US"
subnation = NULL
location = list(nation ="US", subnation ="MD")
species_taxonomy = NULL
record_subtype = NULL
modified_since = NULL
page = NULL
per_page = NULL

# make it all work some more
paging <- natserv:::handle_paging(page, per_page)
text <- natserv:::handle_text(text, text_adv)
status <-natserv:::handle_status(status)
location <- natserv:::handle_location(location)
species_taxonomy <- natserv:::handle_sptax(species_taxonomy)
record_subtype <- natserv:::handle_subtype(record_subtype)
natserv:::assert(modified_since, "character")

# handle_location<-function (location)
# {
#   assert(location, "list")
#   if (!is.null(location)) {
#     if (!all(names(location) %in% c("nation", "subnation"))) {
#       stop("`location` must be a list w/ 'nation' or 'nation' and 'subnation'",
#            call. = FALSE)
#     }
#     pt <- if ("subnation" %in% names(location))
#       "subnation"
#     else "nation"
#     location <- c(paramType = pt, location)
#   }
#   return(location)
# }

res <- natserv:::ns_POST(url = "https://explorer.natureserve.org/api/data/speciesSearch",
                         #file.path(natserv:::ns_base(), "api/data/speciesSearch"),
               body = list(
                 criteriaType = "species"
                 , textCriteria = list(text) %|lst|% NULL
                 , statusCriteria = list(status) %|lst|% NULL
                 # ,  if(paramType =="nation"){
                 #   locationCriteria =list(paramType = paramType, nation = nation)}
                 # , if(paramType =="subnation"){
                 #   locationCriteria =list(paramType = paramType, nation = nation
                 #              , subnation = subnation)}
                 , locationCriteria = list(location) %|lst|% NULL
                 , speciesTaxonomyCriteria = list(species_taxonomy) %|lst|%
                             NULL
                 , recordSubtypeCriteria = list(record_subtype) %|lst|%
                             NULL
                 , pagingOptions = paging
                 , modifiedSince = modified_since))
format_res<-natserv:::parse_search(res)

library(natserv)
library(tidyverse)
unnested <- ns_search_spp(text_adv = list(searchToken ="Andrena"
                                        , matchAgainst ="scientificName"
                                        , operator ="contains")
                        , location = list(nation ="US", subnation ="MD"))[[1]] %>%
  unnest(cols=nations) %>%
  unnest (cols = "subnations", names_repair ="unique") %>%
  filter(subnationCode =="MD")
unnested
