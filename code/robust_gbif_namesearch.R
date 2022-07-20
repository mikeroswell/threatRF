# write a wrapper on rgbif::name_backbone to 1) check if matchType == "NONE" 
# means "too many options" and 2) look again if the best match is to a higher 
# taxon for good synonyms

#' Robust GBIF backbone taxonomy species name search
#' 
#' \link{\code{rgbif::name_backbone(verbose = FALSE)}} returns at most one match
#' per name, and can return none in situations where viable matches exists.
#' Setting \code{verbose = TRUE} tends to return multiple matches.
#' \code{nbRobust()} tries to thread the needle by providing a single best match
#' per name input, but when the first match provided by
#' \code{name_backbone(verbose = FALSE)} is unsatisfactory, it will run the
#' verbose search and make an educated guess about a probable species-level
#' match. In addition to the GBIF fields (e.g `status` and `rank`), the function
#' returns a `matchNote` indicating that the less-precise matching process
#' occurred, as these records may require additional scrutiny.
#'   
#'
#' @param name String, a species name (typically a Latin binomial)
#'
#' @return single-row data frame with results from GBIF API query and additional matchNotes 
#' @export
#'
#' @example 
#' schcklst<-c("Athyrium angustum"
#'      , "Viburnum cassinoides"
#'      , "Sagittaria australis"
#'      , "Sagittarius australis"
#'      , "Rudbeckia hirta"
#'      , "Pyrola elliptica"
#'      , "Rhus glabra"
#'      , "Chamaecrista nictitans")
#' test.df <- purrr::map_dfr(chcklst, function(taxName){
#'      nbRobust(taxName)})
#'
#' test.df


nbRobust <- function(name){
  naive <- rgbif::name_backbone(name = name)
  if(naive$matchType != "EXACT"){
    tooMany <- rgbif::name_backbone(name = name, verbose = TRUE)
    if(naive$matchType == "NONE"){
      firstAccept<-tooMany[match("EXACT", tooMany$matchType), ]
      if(is.null(firstAccept)){
        return(data.frame(naive, matchNote = "matching problem"))}
    else{
      return(data.frame(firstAccept, matchNote = "First matching accepted taxon returned but other exact matches may exist"))
      }
    }
    if(naive$matchType == "HIGHERRANK"){
      firstSpecies <- tooMany[match("SPECIES", tooMany$rank), ]
      return(data.frame(firstSpecies
                        , matchNote = "First species-level taxon (likely synonynymy or misspelling) but other matches may exist"))
    }
    else{
      return(data.frame(naive, matchNote = "possible synonymy or misspelling"))}
  }
  else{return(data.frame(naive, matchNote = "unambiguous"))}
}




