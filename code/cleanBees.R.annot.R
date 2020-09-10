## cleanBees.R - compiled by RoxygenReady, a package by @vertesy


#' cleannames 
#' 
#' cleannames <- function(project, binomial_column = "gs"){
#' @param project 
#' @param binomial_column 
#' @examples cleannames (project =  , binomial_column = gs)
#' @export 

cleannames <-function (project, binomial_column = "gs") {
	project = project %>% rename_(gs = binomial_column) %>% mutate(gs = str_to_sentence(gs)) %>% separate(col = "gs", 
		into = c("genus", "species"), sep = " ")
	project$species[project$genus == "Melissodes" & project$species == "bimaculata"] <- "bimaculatus"
	project$species[project$genus == "Melissodes" & project$species == "denticulata"] <- "denticulatus"
	project$species[project$genus == "Melissodes" & project$species == "druriella"] <- "druriellus"
	project$species[project$genus == "Melissodes" & project$species == "illata"] <- "illatus"
	project$species[project$genus == "Melissodes" & project$species == "subillata"] <- "subillatus"
	project$species[project$genus == "Melissodes" & project$species == "desponsa"] <- "desponsus"
	project$species[project$genus == "Melissodes" & project$species == "nivea"] <- "niveus"
	project$species[project$genus == "Lasioglossum" & project$species == "illinoensis"] <- "illinoense"
	project$species[project$genus == "Nomada" & project$species == "luteolodies"] <- "luteoloides"
	project$species[project$genus == "Dufouria" & project$species == "novaeangleae"] <- "novaeangliae"
	project$genus[project$genus == "Dufouria"] <- "Dufourea"
	project$species[project$genus == "Coelioxys" & project$species == "germana"] <- "germanus"
	project$species[project$genus == "Coelioxys" & project$species == "alternata"] <- "alternatus"
	project$species[project$genus == "Coelioxys" & project$species == "modesta"] <- "modestus"
	project$species[project$genus == "Coelioxys" & project$species == "octodentata"] <- "octodentatus"
	project$species[project$genus == "Coelioxys" & project$species == "moesta"] <- "moestus"
	project$species[project$genus == "Coelioxys" & project$species == "funeraria"] <- "funerarius"
	project$species[project$genus == "Coelioxys" & project$species == "immaculatus"] <- "immaculata"
	project$species[project$genus == "Sphecodes" & project$species == "carolinus"] <- "coronus"
	project$species[project$genus == "Sphecodes" & project$species == "antennaria"] <- "antennariae"
	project$species[project$genus == "Sphecodes" & project$species == "johnsoni"] <- "johnsonii"
	project$genus[project$genus == "Peponapis"] <- "Eucera"
	project$species[project$genus == "Megachile" & project$species == "rotunda"] <- "rotundata"
	project$species[project$genus == "Heriades" & project$species == "carinata"] <- "carinatus"
	project$species[project$genus == "Heriades" & project$species == "variolosa"] <- "variolosus"
	project$species[project$species == "nymphaearum"] <- "oceanicum"
	project$species[project$genus == "Lasioglossum" & project$species == "divergens"] <- "macoupinense"
	project$species[project$genus == "Pseudopanurgus" & project$species == "nebrascensis"] <- "aestivalis"
	project$species[project$genus == "Lasioglossum" & project$species == "perplexa"] <- "perplexans"
	project$species[project$genus == "Lasioglossum" & project$species == "hitchensi/weemsi"] <- "hitchensi_weemsi"
	project$species[project$genus == "Lasioglossum" & c(project$species == "hitchensi" | project$species == 
		"weemsi")] <- "hitchensi_weemsi"
	project$species[project$genus == "Lasioglossum" & c(project$species == "mitchelli" | project$species == 
		"weemsi")] <- "hitchensi_weemsi"
	project$species[project$genus == "Nomada" & project$species == "near_lehighensis"] <- "lehighensis"
	project$species[project$genus == "Hylaeus" & c(project$species == "annulatus_or_mod" | project$species == 
		"near_modestus" | project$species == "near_affinis" | project$species == "affinis" | project$species == 
		"modestus" | project$species == "affinis_like" | project$species == "affinis/modestus" | project$species == 
		"modestus/affinis" | project$species == "modestus-affinis" | project$species == "affinis-modestus" | 
		project$species == "annulatus_modestus" | project$species == "modestus_like") | project$species == 
		"modestus group sp. 1" | project$species == "modestus group sp. 2" | project$species == "modestus group sp. 3" | 
		project$species == "modestus group sp. 4" | project$species == "illinoisensis"] <- "affinis_modestus"
	project$species[project$genus == "Halictus" & c(project$species == "ligatus" | project$species == 
		"poeyi")] <- "ligatus_poeyi"
	project$species <- gsub("_sensu_lato", "", project$species)
	project$species[project$genus == "Nomada" & c(project$species == "sayi" | project$species == "illinoensis" | 
		project$species == "sayi_illinoensis")] <- "illinoensis_sayi"
	project <- project %>% filter(species != "asteris_asteroides")
	project <- project %>% filter(genus != "gen" & species != "sp_CHECK" & species != "sp" & species != 
		"lost_spec" & species != "sp1" & species != "sp2" & species != "sp3" & species != "bidentate_gr" & 
		species != "bidentate_group" & species != "interesting_seeTN" & species != "sp_seeTN" & species != 
		"sp_101" & species %in% grep("maybe", .$species, value = T, invert = T))
	project$species <- gsub("__", "_", project$species)
	project <- project[grep("calcarata_", project$species, invert = T), ]
	project <- project[grep("dupla_", project$species, invert = T), ]
	project <- project[grep("_seeTN", project$species, invert = TRUE), ]
	project <- project[grep("_cf", project$species, invert = TRUE), ]
	project$gs <- paste(project$genus, project$species, sep = " ")
	return(project)
}


