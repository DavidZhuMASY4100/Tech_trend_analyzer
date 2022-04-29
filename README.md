# Tech_trend_analyzer

Technology Trend Analyzer is an R-based web application hosted on the shinyapp.io server. It can scrape 88 job titles in 11 technology categories that appeared in the O*NET Occupation database from Indeed.com and analyze the frequency of technologies appearences in the job descriptions (appear is 1, not is 0). It can show the bar graph of technology frequency grouped by a standard technology list from O'Reilly's compilation of technologies and also the trend of each technlogies by date. This website also allows to download the frequency tables in CSV format.


# how to use

There is 3 seperated files. 
  1. loading: the web scrap parts to collect first 10 job descriptions for each job titles and stored in a csv file
  2. transform: load the csv file from scraping into aggregate function preparing for display charts in shinyapp
  3. interface: display the bar graph and trend lines for each technology

Procedures of Use:
  Store all the files into same directory
  Run all the code in this section and confirm your file path is in a correct place
  Run the interface code with no need of runing transform.R file


The "track" button allows users to display all charts in the interface.
The "download" button allows users to store raw datasets to local directory. 
