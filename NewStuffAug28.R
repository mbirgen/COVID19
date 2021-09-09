#########################################
#### Practicing with Index-Match
#########################################

# Vaccine Information
CountyLookup = read.csv("CountyLookUp.csv", stringsAsFactors=F)
testing = read.csv("Vaccine Series Completion by County of Vaccine Provider.csv",
                   stringsAsFactors = F)

testing$County <- CountyLookup[
    match(testing$ADMIN_ADDRESS_COUNTY, 
          CountyLookup$ï..RECIP_ADDRESS_COUNTY),2]

#############################################
CreateNew = function(x){
    new = x - lag(x)
    return(new)
}

newstatedata = sapply(statedata[,-1], CreateNew)
colnames(newstatedata) = 
    paste("New",colnames(statedata[,-1]), sep=".")


rm(list =ls(pattern = "temp"))

