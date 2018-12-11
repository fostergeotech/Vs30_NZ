#
# verify.R
#
# Generate summary table(s) plots and other outputs to help ensure everything works.
#
#
# I'm using this to manually examine how the unit sorting is done (for age, grain size and depositional unit).
#
#


library(gridExtra)
library(magrittr)


ageTable <- function(df) {
  ageNames <- c("MAIN_ROCK",
                "SIMPLE_NAME",
                "TEXT_CODE",
                "age1",
                "age2",
                "age3",
                "CODE",
                "UNIT_CODE",
                "SUB_ROCKS",
                "MAP_UNIT",
                "STRAT_UNIT",
                "STRAT_AGE",
                "ABS_MIN",
                "ABS_MAX",
                "CONFIDENCE",
                "KEY_GROUP_NAME",
                "DESCRIPTION",
                "ROCK_GROUP",
                "ROCK_CLASS")
  return(unique(df[,ageNames]))
}
ageTablePolys <- function(polygons) {
  return(unique(data.frame(
    polygons$MAIN_ROCK,    # input to classifyAge_v2
    polygons$SIMPLE_NAME,  # input to classifyAge_v2
    polygons$TEXT_CODE,    # input to classifyAge_v2
    polygons$age1,         # output
    polygons$age2,         # output
    polygons$age3,         # output
    polygons$CODE,         # other data (maybe) pertinent to age...
    polygons$UNIT_CODE,    # ...
    polygons$SUB_ROCKS,    # ..
    polygons$MAP_UNIT,     # .
    polygons$STRAT_UNIT,   # .
    polygons$STRAT_AGE,    # .
    polygons$ABS_MIN, 
    polygons$ABS_MAX,
    polygons$CONFIDENCE,
    polygons$KEY_GROUP_NAME,
    polygons$DESCRIPTION,
    polygons$ROCK_GROUP,
    polygons$ROCK_CLASS
  )))
}

# This function is same as above but only spits out the columns used for input and output.
# It results in a smaller number of unique rows for inspection of the sorting scheme.
ageTableSimple <- function(polygons) {
  return(unique(data.frame(
    polygons$MAIN_ROCK,    # input to classifyAge_v2
    polygons$SIMPLE_NAME,  # input to classifyAge_v2
    polygons$TEXT_CODE,    # input to classifyAge_v2
    polygons$age1,         # output
    polygons$age2,         # output
    polygons$age3          # output
  )))
}




grainSizeTable <- function(polygons) {
  return(unique(data.frame(
    polygons$MAIN_ROCK,    # this is the only input to classifyGS_v2
    polygons$gs,           # this is the only output of classifyGS_v2
    polygons$SUB_ROCKS,    # other data (maybe) pertinent to grain size...
    polygons$MAP_UNIT,     # ...
    polygons$DESCRIPTION,  # ..
    polygons$ROCK_GROUP,   # .
    polygons$ROCK_CLASS,   # .
    polygons$SIMPLE_NAME,
    polygons$KEY_NAME,
    polygons$KEY_GROUP_NAME
  )))
}

grainSizeSimple <- function(polygons) {
  return(unique(data.frame(
    polygons$MAIN_ROCK,    # this is the only input to classifyGS_v2
    polygons$gs           # this is the only output of classifyGS_v2
  )))
}


depTable <- function(polygons) {
  return(unique(data.frame(
    polygons$SIMPLE_NAME,  # input
    polygons$MAIN_ROCK,    # input
    polygons$UNIT_CODE,    # input
    polygons$dep,          # output
    polygons$SUB_ROCKS,    # other data (maybe) pertinent to deposition...
    polygons$MAP_UNIT,     # ...
    polygons$DESCRIPTION,  # ..
    polygons$ROCK_GROUP,   # .
    polygons$ROCK_CLASS,   # .
    polygons$SIMPLE_NAME,
    polygons$KEY_GROUP_NAME
  )))
}

depTableSimple <- function(polygons) {
  return(unique(data.frame(
    polygons$SIMPLE_NAME,  # input
    polygons$MAIN_ROCK,    # input
    polygons$UNIT_CODE,    # input
    polygons$dep           # output
  )))
}



AhdiAKtable <- function(polygons) {
  return(unique(data.frame(
    polygons$SIMPLE_NAME,
    polygons$MAIN_ROCK  ,
    polygons$UNIT_CODE  ,
    polygons$DESCRIPTION,
    polygons$MAP_UNIT   , 
    polygons$groupID_AhdiAK
  )))
}
