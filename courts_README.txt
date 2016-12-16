Project Name: 
Champaign County Circuit Clerk Records Dashboard


Description: 
This dashboard takes data from several sources (Champaign County Circuit Clerk, Champaign County Inmate Lookup, and Champaign County Daily Jail Log) to create an interactive database and data visualization for misdemeanor and felony court cases in Champaign County, Illinois.
R is the scripting language for the data collection and creation of the shiny dashboard.
The dashboard can be found here: https://dijiachen.shinyapps.io/Circuit_Clerk-app/


Dashboard Usage:
================Data Table================
Description:
This is an interactive database of Champaign County Circuit Clerk misdemeanor and felony court cases. The defendants in this database also had to have records in either the Champaign County Inmate Lookup or Champaign County Daily Jail Log.

Interactive Filtering Functions:
File Date Range (data table will only show court cases within chosen date range)
Agency (data table will only show booking agency’s court cases selected)
Race (data table will only show court cases involving defendants of the selected race)
Type of Charge (data table will only show court cases with specified type of charge)

Columns and Description:
Case.Number (case number of court case)
File.Date (date that court case was filed)
Race (race of defendant)
Sex (sex of defendant)
Age (age of defendant)
Charge.Type (type of charge given to defendant)
Charge.Agency (booking agency)
Charge (description of charge given to defendant)
Disposition.Type (legal system’s resolution)
Defendant.Plea
Sentence.Date (date that sentence was given)
Sentence (sentence(s) given to defendant, if any)


================Race and Charges================
Description: 
This is a bar graph that is sorted by race, and it shows which type of charge people received. The key/explanation for the type of charge is below the visualization.

Interactive Function:
Hover over the bars to see exact number of defendants by race.
Click on races (Asian, Black, Hispanic, etc.) in the key to enable or disable different races on the bar graph.