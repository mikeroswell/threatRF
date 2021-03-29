# code to keep track of tools I'm playing with and test them out

# install.packages("natserv")
library(natserv) #this is how we get naturserve status
library(rgbif) #how we get gbif observations
library(tidyverse)
# vignette(package ="natserv")
vignette(package ="rgbif")
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

#solution to issue of weird data structure from this package
unnested <- ns_search_spp(text_adv = list(searchToken ="Andrena"
                                        , matchAgainst ="scientificName"
                                        , operator ="contains")
                        , location = list(nation ="US", subnation ="MD"))[[1]] %>%
  unnest(cols=nations) %>%
  unnest (cols = "subnations", names_repair ="unique") %>%
  filter(subnationCode =="MD")
unnested

###
# see if I can figure out how to get occurrence records in MD
# not sure how it deals with string matching in `scientificName`
andrenaOcc<-occ_search(scientificName ="Andrena", stateProvince ="Maryland", limit = 1e4)
andData<-andrenaOcc$data
head(andData)

# e.g. for searching all plants (but how to download? Can rgbif do this?)
#https://www.gbif.org/occurrence/taxonomy?basis_of_record=OBSERVATION&basis_of_record=HUMAN_OBSERVATION&basis_of_record=PRESERVED_SPECIMEN&taxon_key=7707728&state_province=Maryland&advanced=1&occurrence_status=present

# get it so search returns <1e5 results
MD_vasc <- occ_search(taxonKey = 7707728
                    , stateProvince = "Maryland", year="1999, 2019"
                    , basisOfRecord = c("OBSERVATION", "HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")
                    , hasCoordinate = T
                    , limit = 1e5)

vasc_flat<-flatten(MD_vasc)

length(MD_vasc[[3]])
MD_vasc[[2]][[3]]

md_vasc_obs<-bind_rows(MD_vasc[[2]][[3]], MD_vasc[[3]][[3]])
str(md_vasc_obs)
md_vasc_obs %>% group_by(acceptedTaxonKey, year) %>% summarize(obs =n()) %>% ggplot(aes(obs))+geom_histogram()+facet_wrap(~year)+theme_classic()+labs(y="species", x="occurrences")+scale_y_log10()

vasc_UL<-unlist(MD_vasc)
head(vasc_UL)
MD_vasc
#ans: use occ_download()

try_vacsular<-occ_download(c(
  pred("basisOfRecord", "OBSERVATION")
  , pred("basisOfRecord", "HUMAN_OBSERVATION")
  , pred("basisOfRecord", "PRESERVED_SPECIMEN")
  , pred("taxonKey", "7707728")
  # , pred("country","US")
  # , pred("occurrenceStatus", "present")
  , pred("hasCoordinate", TRUE)
)
  , user = "mroswell"
  , pwd = "Epeoloides!1"
  , email = "mroswell@umd.edu"
)

occ_download_get(try_vacsular, path = "data", overwrite = T)
occ_download_meta(try_vacsular)
