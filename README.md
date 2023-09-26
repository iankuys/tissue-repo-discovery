# tissue-repo-cohort-discovery-app
This application allows our clients to dynamically query our tissue repository for records based on many different parameters. It can also generate graphs to help our clients visualize the statistics of the query results from our records. Moreover, this application also has a feature to download the results as a ".csv" file using the export button on the top of the side bar panel, the output will provide the "record_id"s of every result which is important in identifying which records fulfill the certain parameters chosen by the user.

Requires R >= 4.22

## How to run it locally
1. Store your RedCap API key in a file called "secrets.txt" in the project folder.
2. With pacman as our package manager, the application will automatically install all required libraries from running the script.
3. Uncomment these parts of the code,
```
# options(shiny.host = "0.0.0.0")
# options(shiny.port = 5000)
# shinyApp(ui = ui, server = server)
 ```
4. Run the app by running the script in the app folder by typing these commands in the terminal.
 ```
 cd app
 Rscript server.R 
 ```

## How to run it on R Shiny Server
1. Store your RedCap API key in a file called "secrets.txt" in the project folder.
2. Change user profile to shiny profile and install all these packages in R as shiny user.  
- To change profile,
```
sudo passwd shiny //to set password if not done yet
su - shiny

```
- In R terminal as shiny user,
```
install.packages("shiny")
install.packages("tidyverse")
install.packages("stringr")
install.packages("bslib")
install.packages("shiny")
install.packages("ggplot2")
install.packages("shinyWidgets")
install.packages("data.table")
```
3. Make sure this part of the code stays commented out,
 ```
# shinyApp(ui = ui, server = server)
 ```
4. Move the folder to R Shiny Server directory.
5. App can be accessed from the correct ip address and port of remote shiny server.
