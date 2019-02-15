#Credit initially goes to Tim Wilson of Search Discovery, formerly Analytics Demystified when I adapted it from here: https://github.com/gilliganondata/aa-ga-compare
#This is used as a complimentary extension of my adobe analytics SDR automated with R and Google Sheets
#FYI: Make sure to have all the developer access you need with your google project including Google Analytics, Google Drive
#Also - get your web services api key from adobe and the google project!  Enjoy!


#You need to install all the required packages below

#install.packages("googleAnalyticsR")
#install.packages("googleAuthR")
#install.packages("RSiteCatalyst")       
#install.packages(googleAnalyticsR)      
#install.packages("tidyverse")                    
#install.packages("lubridate")
#install.packages("googlesheets")


#load your packages and permissions before running the script
library("RSiteCatalyst")       # Package for getting the Adobe Analytics data
library("googleAnalyticsR")      # Package for getting the Google Analytics data You may need to pre-load your scopes and keys first or use googleauthr to store your token in file
library("tidyverse")           # Package(s) for manipulating
library("lubridate")
library("googlesheets")

#get your clientid and secret from your google developer api project and insert the keys here
options(googleAuthR.client_id = "INSERT_CLIENT_ID_HERE")
options(googleAuthR.client_secret = "INSERT_CLIENT_SECRET_HERE")
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/analytics")

     
library("googleAnalyticsR")
library("googleAuthR")

#to authenticate for GA
ga_auth()


ga_view_id <- "insert_view_id_for_google_here"

SCAuth('insert_adobe_api_client_here', 'insert_adobe_api_client_secret_here')
options(scipen=999)
aa_rsid <- Sys.getenv("insert_adobe_report_suite_here")
start_date <- Sys.Date()-90
end_date <- Sys.Date()-1
metrics <- data.frame(
  label = c("UVs","Visits","Pageviews","Orders","Revenue","Entrances","Bounces"),
  adobe_analytics = c("uniquevisitors","visits","pageviews","orders","revenue","entries","bounces"),
  google_analytics = c("users","sessions","pageviews","transactions","transactionRevenue","entrances","bounces"),
  stringsAsFactors = FALSE)



aa_data <- QueueOvertime(aa_rsid, start_date, end_date,
                         metrics$adobe_analytics,
                         date.granularity = "day")

aa_data <- select(aa_data, -name, -year, -month, -day,
                  -segment.id, -segment.name)

aa_data$datetime <- as.Date(aa_data$datetime)

colnames(aa_data) <- c("Date", metrics$label)

aa_data$Platform <- "Adobe Analytics"
ga_data <- google_analytics(ga_id,
                              c(start_date,end_date),
                              metrics = metrics$google_analytics,
                              dimensions = "date",
                              anti_sample = TRUE)
# Rename the columns to be the label value (this is so it will
# match when we do the same with the AA data)
colnames(ga_data) <- c("Date", metrics$label)
# Add a column that designates the data as being Google Analytics data
ga_data$Platform <- "Google Analytics"

master_data <- rbind(aa_data, ga_data)
master_diffs <- full_join(aa_data, ga_data, by = "Date")
colnames(master_diffs) <- gsub("\\.x$", ".aa", colnames(master_diffs)) %>%
  gsub("\\.y$", ".ga", .)
# Now, cycle through and add columns that calculate the % differences
# and total differences for each metric. I can't figure out how to do this
# without a loop, but maybe there's a way?
for (i in 1:length(metrics$label)){
  # Add a new column that calculates the total difference. This simply
  # takes the current metric value and adds "Total Diff" onto the end of
  # it to make the column name. Then, it grabs both the ".aa" and ".ga"
  # columns for that metric and subtracts the second column from the first.
  # This data doesn't currently get used anywhere, but it could be. I'd
  # initially had an additional chart that showed the total difference
  # in addition to the % difference, but that was getting to be overkill.
  master_diffs[,paste(metrics$label[i],"Total Diff")] <-
    select(master_diffs, contains(metrics$label[i]))[,1] -
    select(master_diffs, contains(metrics$label[i]))[,2]
  # Do the same thing, but calculate the % difference. This DOES currently
  # get used.
  master_diffs[,paste(metrics$label[i],"Percent Diff")] <-
    select(master_diffs, contains(metrics$label[i]))[,1] /
    select(master_diffs, contains(metrics$label[i]))[,2] - 1
}
default_theme <-   theme_bw() +
  theme(axis.text = element_text(size = 10, colour = "grey50"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_line(colour = "grey30"),
        axis.line.y = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor = element_blank()
  )

#write it into a csv
write.csv(master_diffs, file = "Master_Report_GA_vs_AA.csv")

#to authenticate with google sheets library
gs_auth()

#this will upload it to your google drive and refresh when running this script - you can setup a batch script with task scheduler in windows or crontab in linux to run this script with R on a schedule
gs_upload("PATH_TO_FILE_OF_YOUR_WORKING_DIRECTORY/Master_Report_GA_vs_AA.csv", sheet_title ="GA_vs_AA_Master_Dashboard", verbose = TRUE, overwrite = TRUE)



monitorEndTime <- Sys.time()

# Write out to the console how long it took for the entire process to run.
cat("This process took",monitorEndTime - monitorStartTime,"minutes to run.",sep=" ")
