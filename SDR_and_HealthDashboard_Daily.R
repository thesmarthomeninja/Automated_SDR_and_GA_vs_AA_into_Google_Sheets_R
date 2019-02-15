# Capturing the start time -- the final console output will be how many minutes
# it took for the process to run.
monitorStartTime <- Sys.time()

# Packages needed - don't forget to install.packages() for each of these first before performing any automations for your first manual run.
library(RSiteCatalyst) # Get data from Adobe Analytics
library(jsonlite) # Needed for RSiteCatalyst (?)
library(devtools) # Needed for RSiteCatalyst (?)
library(httr) # Needed for RSiteCatalyst (?)
library(base64enc) # Needed for RSiteCatalyst (?)
library(dplyr) # Streamline some of the code
library(tidyr) # Clean up the data -- get it into "tidy" (flat) format
library(WriteXLS)
library(tools)
library(googlesheets)

gs_auth()
#Validate that underlying Perl modules for WriteXLS are installed correctly
#Will return "Perl found. All required Perl modules were found" if installed correctly
testPerl()

############################################

# Set the start date. This is set up to pull data for the last 30 days.
dateFrom <- Sys.Date()-30

# Set the end date to yesterday
dateTo <- Sys.Date()-1 

adobe_key <- Sys.getenv("ADD_YOUR_ADOBE_API_CLIENT_KEY_IN_QUOTES")
adobe_secret <- Sys.getenv("ADD_YOUR_ADOBE_API_SECRET_KEY_IN_QUOTES")

# Call SCAuth function to set credentials before usage
SCAuth(adobe_key, adobe_secret)

###########################################

# OPTION #3: Get a list of report suite IDs from a .csv. This file should have the 
# first line (header) of "rsid" and then a new line for each RSID to be
# included. To use this option, place that file in the working directory for
# this project, replace [filename] below with the name of the file, and 
# COMMENT OUT the rows in Option #1 and Option #2
RSIDs <- read.csv("rsid.csv")

#########################################

# This process pulls all reasonable metrics -- both standard metrics and all
# custom events that are available (and then removes ones that have no data).
# Some events can't be included or shouldn't be included. The vector below
# is the "start of string" for the ids for ones that we want to exclude. This
# list can be massaged to exclude additional metrics or include some of them.
c_excludeMetrics <- c("activity","average","bots","total","participation","experience","instances",
                      "customers","visitors","reloads","f:","cm300000")

########################################################
# FUNCTION: to get the values for the eVars or props
########################################################

rankedData <- function(masterData,valuelist,varType){
  
  # For eVars and sProps, we're just going to pull pageviews and instances.
  metric <- c("pageviews","instances")
  
  # Having this as a loop may be a not-ideal way to do it. But, I believe a separate API call
  # is needed for each eVar/sProp being evaluated, and there's just enough subsequent cleanup
  # that I couldn't get it working as a straight up lapply()
  for (i in 1:length(valuelist$id)){
    
    elements <- valuelist$id[i]
    valueName <- valuelist$name[match(elements,valuelist$id)]  # Get the plain English name for value
    
    # Pull a ranked list for the entire period. If you want to bring back more than the
    # top 5 values, just change the "top" value below.
    reportData <- QueueRanked(c_reportsuiteID, dateFrom, dateTo, metric, 
                              elements, top = 5)
    
    # Write out to the console what has been pulled.
    cat(c_reportsuiteID,"- Finished pulling",elements,"\n",sep=" ")
    
    # There are various ways to check to see if the variable has data or not. The
    # processing is different depending on which it is, so breaking out two functions to 
    # handle what to do in either case.
    
    hasData <- function(reportData){
      # Rename the columns so we can add to masterData easily
      names(reportData) <- c("name","type","Event_Value_or_Page_Views","Instances","id","daterange")
      
      reportData$type <- varType
      
      # We're going to put the name of the element for the "name" We want to keep
      # this to limited values (remember it's "metric" for ALL of the events/metrics)
      reportData$name <- paste(valueName,":",reportData$name, sep=" ")
      reportData$id <- elements
      reportData$Has_Data <- "Yes"
      reportData$daterange <- paste(dateFrom,"to",dateTo,sep=" ")
      
      # Tidy things up -- just keep the 5 columns we want to keep
      reportData <- reportData[,c("type","name","id","Has_Data","Event_Value_or_Page_Views","Instances","daterange")]
      
      return(reportData)
    }
    
    noData <- function(reportData){
      
      reportData <- data.frame(type = varType,
                               name = paste(valueName,": (No Data)", sep=" "),
                               id = elements,
                               Has_Data = "No",
                               daterange = paste(dateFrom,"to",dateTo,sep=" "),
                               Event_Value_or_Page_Views = NA,
                               Instances = NA)
      return (reportData)
      
    }
    
    # Check for the different conditions. This is a little messy because, if the results
    # are empty, we need to avoid a condition with an argument with [1].
    
    if(nrow(reportData) == 0){
      reportData <- noData(reportData)
    } else {
      if((nrow(reportData) == 1) & (reportData$name[1] == "::unspecified::")) {
        reportData <- noData(reportData)
      } else {
        reportData <- hasData(reportData)
      }
    }
    
    # Finally, take this purdy data set and tack it onto masterData!
    masterData <- rbind(masterData,reportData)
  }
  
  return(masterData)
}

########################################################
# END FUNCTION
########################################################

########################################################
# FUNCTION: Just for jumping through some hoops to get events sorting nicely
########################################################

# Trying to match the Admin Console ordering fairly closely: first listing
# all of the "standard" events, and then listing custom events in numeric
# order.

sortVal <- function(valToAdjust){
  if(substr(valToAdjust,1,5)=="event"){
    valToAdjust <- as.character(sprintf("%03d",as.numeric(substr(valToAdjust,6,nchar(valToAdjust)))))
  } else {
    valToAdjust <- paste("000",valToAdjust,sep="")
  }
  return(valToAdjust)
}


############################################
# Start of loop to go through all of the RSIDs
############################################

for (r in 1:nrow(RSIDs)){
  
  c_reportsuiteID <- as.character(RSIDs$rsid[r])
  
  #####################
  # First, work through all the events and get that data
  #####################
  
  # Get the list of events we're working with
  metricsList <- eventsConfig <- GetSuccessEvents(c_reportsuiteID)
  # Grab just the columns we want to keep...and then rename them
  eventsConfig <- eventsConfig[c("id","name","type","serialization","participation","description")]
  names(eventsConfig) <- c("ID","Name","Type","Serialization","Participation","Description")
  # We want events to be sorted a bit more cleanly
  eventsConfig <- eventsConfig[order(unlist(lapply(eventsConfig$ID,sortVal))),]
  # Add cleaner names to the Serialization column
  eventsConfig$Serialization[eventsConfig$Serialization=="always_record"] <- "Always Record Event"
  eventsConfig$Serialization[eventsConfig$Serialization=="record_once_per_unique_id"] <- "Use Event ID"
  eventsConfig$Serialization[eventsConfig$Serialization=="record_once_per_visit"] <- "Record Once Per Visit"
  eventsConfig$Serialization[eventsConfig$Serialization=="record_once_per_purchaseId"] <- "Use purchaseID" # Need to check this one
  # Clean up the Type column
  eventsConfig$Type <- toTitleCase(eventsConfig$Type)
  eventsConfig$Type[eventsConfig$Type=="Counter_no_subrelations"] <- "Counter (no subrelations)"
  eventsConfig$Type[eventsConfig$Type=="Numeric_no_subrelations"] <- "Numeric (no subrelations)"
  eventsConfig$Type[eventsConfig$Type=="Currency_no_subrelations"] <- "Currency (no subrelations)"
  # Clean up the Participation column
  eventsConfig$Participation <- toTitleCase(eventsConfig$Participation)
  
  
  # Remove the metrics that we don't want to include or that are not available.
  # See the config script to add more "start" patterns to exclude here if need be.
  for (i in 1:length(c_excludeMetrics)){
    metricsList <- metricsList[!grepl(c_excludeMetrics[i], metricsList$id),]
  }
  
  # Remove disabled events
  metricsList <- metricsList[which(metricsList$type!="disabled"),]
  
  # We can only pull 30 metrics at a time, so we're going to have to iterate through
  # the list.
  totalVars <- as.numeric(nrow(metricsList))
  firstVar <- 1
  lastVar <- 30
  
  while(firstVar<=totalVars){
    # Check that we're not exceeding the total number of variables that we're trying to pull.
    # If we are, set the last value to the last row
    if(lastVar > totalVars) { 
      lastVar = totalVars     
    }
    
    # Set the metrics to pull -- a subset of the full list
    metrics <- metricsList$id[firstVar:lastVar] 
    
    reportData <- QueueSummary(c_reportsuiteID, "", metrics, date.from = dateFrom, date.to = dateTo)
    
    # Add the results from the query to the master data set
    if(firstVar == 1){
      masterData <- reportData
    } else {
      masterData <- cbind(masterData,reportData[,4:ncol(reportData)])
    }
    
    cat(c_reportsuiteID,"- Finished pulling events through #",lastVar,"\n",sep=" ")
    
    
    firstVar <- lastVar + 1  # Reset the next start point
    lastVar <- lastVar + 30
    
  }
  
  ####################
  # Tidy up the date
  ####################
  
  # Initial tidying / flattening
  masterData <- masterData %>% gather(id,value,4:length(names(masterData)))
  
  # Add a daterange column
  masterData$daterange <- paste(dateFrom,"to",dateTo,sep=" ")
  
  # Tidy further -- just keep the 3 columns we want to keep
  masterData <- masterData[,c("daterange","id","value")]
  
  # Add a "type" column. These will all be "metric" for this data, but
  # we'll be adding "eVar" types later
  masterData$type <- "metric"
  
  # Now get the plain English name for each metric from metricsList
  masterData$name <- metricsList$name[match(masterData$id, metricsList$id)]
  
  names(masterData)[names(masterData) == "value"] <- "Event_Value_or_Page_Views"
  
  masterData$Has_Data <- lapply(masterData$Event_Value_or_Page_Views, function(x)
    if(x==0){"No"} else {"Yes"})
  
  # Add a column for Instances. This isn't needed for events, but we'll use it for
  # eVars and sProps
  masterData$Instances <- NA
  
  # And...let's rearrange columns to make a little more sense
  masterData <- masterData[c("type","name","id","Has_Data",
                             "Event_Value_or_Page_Views","Instances","daterange")]
  
  masterData$Instances <- NA
  
  # We want events to be sorted a bit more cleanly
  masterData <- masterData[order(unlist(lapply(masterData$id,sortVal))),]
  
  cat("Done with events. Getting tracking code.\n")
  
  #####################
  # Now, get the non-event data - eVars, s.props, and other
  #####################
  
  ##############
  # Tracking Code
  trackingCode <- list(name = "Tracking Code",
                       id = "trackingcode")
  # Pull the data
  masterData <- rankedData(masterData,trackingCode,"tracking code")
  
  
  ###############
  # eVars
  eVarsConfig <- GetEvars(c_reportsuiteID)
  eVarsEnabled <- subset(eVarsConfig, enabled==TRUE)
  
  # Pull the eVar data
  masterData <- rankedData(masterData,eVarsEnabled,"eVar")
  
  # Clean up the eVars configuration list so it's easier to read in the final output.
  
  # The query won't return merchandising columns if there is no merchandising, so check for
  # and add them.
  if(length(eVarsConfig$merchandising_syntax)==0){
    eVarsConfig$merchandising_syntax <- ""
  }
  if(length(eVarsConfig$binding_events)==0){
    eVarsConfig$binding_events <- ""
  }
  
  # Grab just the columns we want to keep...and then rename them
  eVarsConfig <- eVarsConfig[c("id","name","enabled","allocation_type","expiration_type","type",
                               "merchandising_syntax","binding_events","description")]
  names(eVarsConfig) <- c("ID","Name","Enabled","Allocation_Type","Expiration_Type","Type",
                          "Merchandising_Syntax","Binding_Events","Description")
  # Add cleaner names to various columns
  eVarsConfig$Type[eVarsConfig$Type=="text_string"] <- "Text String"
  eVarsConfig$Type[eVarsConfig$Type=="counter"] <- "Counter"
  eVarsConfig$Expiration_Type[eVarsConfig$Expiration_Type=="page_view"] <- "Page View"
  eVarsConfig$Expiration_Type <- toTitleCase(eVarsConfig$Expiration_Type)
  eVarsConfig$Allocation_Type[eVarsConfig$Allocation_Type=="merchandising_last"] <- "Merchandising (Last)"
  eVarsConfig$Allocation_Type[eVarsConfig$Allocation_Type=="most_recent_last"] <- "Most Recent (Last)"
  eVarsConfig$Allocation_Type[eVarsConfig$Allocation_Type=="original_value_first"] <- "Original Value (First)"
  eVarsConfig$Allocation_Type[eVarsConfig$Allocation_Type=="linear"] <- "Linear"
  eVarsConfig$Enabled[eVarsConfig$Enabled==TRUE] <- "Enabled"
  eVarsConfig$Enabled[eVarsConfig$Enabled==FALSE] <- "Disabled"
  eVarsConfig$Merchandising_Syntax[eVarsConfig$Merchandising_Syntax=="product"] <- "Product Syntax"
  eVarsConfig$Merchandising_Syntax[eVarsConfig$Merchandising_Syntax=="conversion_variable"] <- "Conversion Syntax"
  eVarsConfig$Binding_Events[eVarsConfig$Binding_Events=="NULL"] <- ""
  
  ##############
  # Core/standard
  coreElements <- list(name = c("Page","Server","Product","Site Section"),
                       id = c("page","server","product","sitesection"))
  # Pull the data
  masterData <- rankedData(masterData,coreElements,"core element")
  
  #############
  # sProps
  sPropsConfig <- GetProps(c_reportsuiteID)
  sPropsEnabled <- subset(sPropsConfig, enabled==TRUE)
  
  # Pull the data
  masterData <- rankedData(masterData,sPropsEnabled,"sProp")
  
  # Clean up the sProps configuration list so it's easier to read in the final output.
  
  # Grab just the columns we want to keep...and then rename them
  sPropsConfig <- sPropsConfig[c("id","name","enabled","list_enabled","participation_enabled",
                                 "pathing_enabled","description")]
  names(sPropsConfig) <- c("ID","Name","Enabled","List","Participation",
                           "Pathing","Description")
  # Add cleaner names to the various columns
  sPropsConfig$Enabled[sPropsConfig$Enabled==TRUE] <- "Enabled"
  sPropsConfig$Enabled[sPropsConfig$Enabled==FALSE] <- "Disabled"
  sPropsConfig$List[sPropsConfig$List==TRUE] <- "Enabled"
  sPropsConfig$List[sPropsConfig$List==FALSE] <- "Disabled"
  sPropsConfig$Participation[sPropsConfig$Participation==TRUE] <- "Enabled"
  sPropsConfig$Participation[sPropsConfig$Participation==FALSE] <- "Disabled"
  sPropsConfig$Pathing[sPropsConfig$Pathing==TRUE] <- "Enabled"
  sPropsConfig$Pathing[sPropsConfig$Pathing==FALSE] <- "Disabled"
  
  # A hack... really just want the worksheet tab to be "populatedData."
  # Should either change the masterData object throughout the code
  # or just rename the data frame.
  populatedData <- masterData
  
  # Rearrange the columns
  populatedData <- populatedData[c("type","id","name","Has_Data","Event_Value_or_Page_Views","Instances","daterange")]
  
  # And...rename the columns of that
  names(populatedData) <- c("Type","ID","Name","Has_Data","Event_Value_or_Page_Views","Instances","Date_Range")
  
  cat(c_reportsuiteID,"- Finished pulling available data.\n",sep=" ")
  
  ######################## 
  # Generate a single Excel file
  ########################
  
  # Create list of report suite objects, written as strings
  objlist <- c("eventsConfig","eVarsConfig","sPropsConfig", "populatedData")
  
  # And...we actually want to make the worksheet names a bit cleaner
  sheetNames <- c("Event Configuration","eVar Configuration","sProp Configuration",
                  "Sample Data")
  
  filename <- paste(c_reportsuiteID,".xlsx",sep="")
  
  # Write out Excel file with auto-width columns, a bolded header row and filters turned on
  WriteXLS(objlist, filename, SheetNames = sheetNames,
           AdjWidth = TRUE, BoldHeaderRow = TRUE, AutoFilter = TRUE)
  
  cat(c_reportsuiteID,"- Output file created.\n",sep=" ")
  
}


#Upload excel sheets into google sheet - you first need to run these two (remove # symbols for first run)
#gs_upload("rsid1.xlsx", sheet_title ="rsid1 sheet name", verbose = TRUE)
#gs_upload ("rsid2.xlsx", sheet_title ="rsid2 sheet name", verbose = TRUE)


#these are for when you automate - and you want it to update using overwrite = true
gs_upload("rsid1.xlsx", sheet_title ="rsid1", verbose = TRUE, overwrite = TRUE)
gs_upload("rsid2.xlsx", sheet_title ="rsid2", verbose = TRUE, overwrite = TRUE)


monitorEndTime <- Sys.time()

# Write out to the console how long it took for the entire process to run.
cat("This process took",monitorEndTime - monitorStartTime,"minutes to run.",sep=" ")
