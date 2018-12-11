# For applying group classifications to either polygons (spatialPolygonsDataFrame) or points (spatialPointsDataFrame)
classifyThings <- function(inp){
  library(maptools) 
  source("R/models.R")

  data <- as.data.frame(inp)[,c(
    "SIMPLE_NAME", "MAIN_ROCK", "UNIT_CODE", "DESCRIPTION", "MAP_UNIT", "TEXT_CODE"
  )]
  
  
  
  ## no longer using these - all just for "testing" geology model. ########        # Add fields for grain size, age, depositional groups
  ## no longer using these - all just for "testing" geology model. ########        age           <- classifyAge_v3(data)
  ## no longer using these - all just for "testing" geology model. ########        age1 = age$age1
  ## no longer using these - all just for "testing" geology model. ########        age2 = age$age2
  ## no longer using these - all just for "testing" geology model. ########        age3 = age$age3
  ## no longer using these - all just for "testing" geology model. ########        gs            <- classifyGS_v2(data)
  ## no longer using these - all just for "testing" geology model. ########        dep           <- classifyDep_v2(data)
  ## no longer using these - all just for "testing" geology model. ########        newFields <- data.frame( age1, age2, age3, gs, dep)
  ## no longer using these - all just for "testing" geology model. ########        
  ## no longer using these - all just for "testing" geology model. ########        data1 <- cbind(data, newFields)
  data1 <- data
  
  
  
  
  ############
  ####################################
  #################################################################
  # The following needs to be updated MANUALLY when new models are produced.
  #################################################################
  ####################################
  ############
  
  groupID_testing              <-    testing_setGroupID(data1)
  groupID_AhdiAK               <-      # same geology group ID
  groupID_AhdiAK_KaiAll        <-      # same geology group ID
  groupID_AhdiAK_KaiNoQ3       <-      # same geology group ID
  groupID_AhdiAK_noQ3          <-      # same geology group ID - added 20171129
  groupID_AhdiAK_noQ3noMcGann  <-      # same geology group ID - added 20171129
  groupID_AhdiAK_KaiAll_hyb09c <-      # same geology group ID
  groupID_AhdiAK_noQ3_hyb09c   <-      # added 20171205
  # groupID_YongCA_noQ3          <-      # added 20171218  # MISTAKE?!
                                    AhdiAK_setGroupID(data1)
  
  newFields <- cbind(      newFields                    ,
                           groupID_testing              ,
                           groupID_AhdiAK               ,
                           groupID_AhdiAK_KaiAll        ,
                           groupID_AhdiAK_KaiNoQ3       ,
                           groupID_AhdiAK_noQ3          ,
                           groupID_AhdiAK_noQ3noMcGann  ,
                           groupID_AhdiAK_KaiAll_hyb09c ,
                           groupID_AhdiAK_noQ3_hyb09c   #, # MISTAKE?!
                           # groupID_YongCA_noQ3           # MISTAKE?!
                           )

  outp <- spCbind(inp, newFields)

  return(outp)
}
