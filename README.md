# Automated_SDR_to_Google_Sheets_R

Original credit for this work goes to Tim Wilson at Search Discovery.  His repository is here: https://github.com/gilliganondata.  I've adapted this to where you can automate your SDR into a daily refreshing google sheet for Adobe Analytics, and also have included a Google Analytics vs Adobe Analytics comparison of the last 90 days as a companion.  Batch scripts are in here as well.

Here's a video to help you out as well: https://youtu.be/-wUB0aplnik  
Note: This video only shows manual run of SDR.

Pre-requisites:
Just get the required keys, permissions with Adobe Web Service Keys and Google Developer Project API keys- for Google Analytics and google sheets, then install all the required packages and run the scripts manually for first run- then use Windows Task Scheduler to trigger the batch scripts that are also in this repo...  Look for the .ROUT files if you have any problems with running automation in Windows- it is a nice little debug log when things go wrong.  

Once you get these running fully- check to make sure the Google Sheets are in your drive, then automate, and lastly- use import ranges to some master google sheet with them all together and some nice little graphs to keep a close eye on your adobe and google analytics implementations.  Better to be safe than sorry!  Credit for the pivotal R packages goes to Randy Zwitch with RSiteCatalyst and Mark Edmundson for the Google Analytics and Google Auth Packages!  Boo-yah!
