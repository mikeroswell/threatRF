# scratch pad to explore land use and land cover classes
# this is being written
# while crunch_GIS has just been run and the objects it creates in memory.

table(LC2013_points)
# 1 is open water MMU 25 m2
# 2 is emergent wetlands (saturated and next to water) MMU 225 m2
# 3 Trees MMU 9 m2
# 4 shrub MMU 25 m2 # note this one is hardly there, shoudl collapse
# 5 lawns and crops mmu 9m2
# 6 barren MMu 25 m2 # also a rare type
# 7 structures mmu 9m2
# 8 impervious <2m tall MMU 9 m2
# 9 impervious roads MMU 9 m2
# 10 trees over 7 MMU 9 m2
# 11 trees over 8 
# 12 Trees over 9
# 13 APG

# convert 4, 10-13 to 3, trees
# convert 8,9 to 7
# that leaves 1, 2, 3, 5, 6, 7 = six classes
# combine 1 and 2 to make a wet class.

LC_simple<-function(x){
  if_else(x %in% c(1,2), "wet"
          , if_else(x  %in% c(3, 4), "tree"
                  , if_else(x == 5, "low_veg"
                           , if_else( x == 6, "barren"
                                      , if_else(x %in% 7:9, "impervious"
                                                , if_else(x %in% 10:13, "tree_imperv", "NA"))
                                      )
                           )
          )
  )}

LC13SimplePoints<-LC_simple(LC2013_points)

table(LC13SimplePoints)

table(LU2013_points)

# 1 Road
# 2 other impervious
# 3 Tree over impervious
# 4 water
# 5 tidal wetlands # pretty small
# 6 floodplain wetlands
# 7 other wet # minor
# 8 forest
# 9 trees over turf
# 10 mixed open
# 11 small turf
# 12 medium turf # pretty small
# 13 large turf
# 14 fractional impervious # basically none, combine with 10
# 15 turf
# 16 crop

# combine all the turfs? This would be 11, 12, 13, 15 and optionally 9
# combine the wets? 4:7
# combine 14, 10

LU_simple<-<-function(x){
  if_else(x ==1, "road"
          , if_else( x ==2, )
          , if_else(x %in% c(4,7), "wet"
          , if_else(x  %in% c(3, 4), "tree"
                    , if_else(x == 5, "low_veg"
                              , if_else( x == 6, "barren"
                                         , if_else(x %in% 7:9, "impervious"
                                                   , if_else(x %in% 10:13, "tree_imperv", "NA"))
