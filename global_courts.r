library(rvest)
library(stringr)
library(RSQLite)
library(sqldf)
library(data.table)

#Create a SQLite database
sqldf("ATTACH 'Court Cases.sqlite' AS new")
con <- dbConnect(RSQLite::SQLite(), "Court Cases.sqlite")
#Create a table from scratch 
dbSendQuery(con,
            "CREATE TABLE Cases
            ('Case.Number' TEXT,
            'File.Date' TEXT,
            'Charge.Agency' TEXT,
            'Last.Name' TEXT,
            'First.Name' TEXT,
            'Middle.Initial' TEXT,
            'Charge.Type' TEXT,
            'Charge' TEXT,
            'Disposition.Type' TEXT,
            'Defendant.Plea' TEXT,
            'Sentence.Date' TEXT,
            'Sentence' TEXT)")
dbDisconnect(con)

#Access database
url <- "https://secure.jtsmith.com/clerk/clerkJ915DDqZ0.asp"
##Start html session
session <- html_session(url)
##Extract the html form from the website
form <- html_form(session)[[1]]
##specify "Submit" button to "Case Search"
submit_button <- list(name = "SUBMIT",
                      type = "SUBMIT",
                      value = "Case Search",
                      checked = NULL,
                      disabled = NULL,
                      readonly = NULL,
                      required = FALSE)
attr(submit_button, "class") <- "input"
form[["fields"]][["SUBMIT"]] <- submit_button
big.list <- seq(1, 9999, 1)
num_cases <- length(big.list)
count <- 0

#Felonies
for (f in 1:num_cases) {
felonies <- paste0(16, "CF", big.list[f])
##fill out form
filled_form <- set_values(form, CASENUMBER = felonies)
##submit form
result <- submit_form(session, filled_form, submit = submit_button)
##extract html of case page
case <- read_html(result)

#Get Case Number from html Headings
headings.info <- case %>%
  html_nodes("b") %>%
  html_text()
#defendent's name, file date, court room, near appearance, birth date
defend.info <- case %>%
  html_nodes("tr:nth-child(2) pre:nth-child(1)") %>%
  html_text()
#Check if case record exists. NEXT and BREAK code placed here.
if(identical(defend.info, character(0))){
  count <- count + 1
  if (count == 5){
    break
  } else
    next}

print(paste0("moving on after ", f)) #this line is not necessary -- good for testing
count <- 0
#docket information
d.string1 <- "pre:nth-child(9) , table:nth-child(8) b"
d.string2 <- "pre:nth-child(7) , table:nth-child(6) b"
d.string3 <- "pre:nth-child(5) , table:nth-child(4) b"
docket.info <- case %>%
  html_nodes(d.string1) %>%
  html_text()
if(length(docket.info) == 0) {
  docket.info <- case %>%
    html_nodes(d.string2) %>%
    html_text()}
if(length(docket.info) == 0) {
  docket.info <- case %>%
    html_nodes(d.string3) %>%
    html_text()}

#make information into neat lists
defend.info <- unlist(strsplit(defend.info, "*\r\n"))
docket.info <- unlist(strsplit(docket.info, "*\r\n"))
#Extract Information
##Case Number
case.number <- grep("CASE ", headings.info, value = TRUE)
case.number.out <- str_trim(substr(case.number[1], 32 , nchar(case.number[1])))
##File Date
file.date <- str_trim(grep("File Date: ", defend.info, value = TRUE))
file.date.out <- str_trim(substr(file.date[1], 11 , nchar(file.date[1])))
##Defendant
name <- str_trim(grep("DEFENDANT: ", defend.info, value = TRUE))
name1 <- unlist(strsplit(name, " "))
lastname <- name1[2]
firstname <- name1[4]
middlename <- name1[5]
##Agency
agency <- str_trim(grep("Agency: ", docket.info, value = TRUE))
##Sentencing
sentencing <- str_trim(grep("Sentence:", docket.info, value = TRUE))
sentencing1 <- str_trim(str_sub(sentencing, 0, -9))
###Sentencing Date
sentence.date <- str_trim(substr(sentencing[1], 10, nchar(sentencing[1])))
###Sentence
sentence0 <- paste(sentencing1, collapse= " ")
sentence1 <- unlist(strsplit(sentence0, "Sentence: ([0-9][0-9])"))
sentence <- str_trim(grep("Sentence:", sentence1, value = TRUE))
##Disposition
disposition0 <- str_trim(grep("Disposition Type: ", docket.info, value = TRUE))
disposition1 <- unlist(str_split(disposition0, "  "))
disposition <- grep("Disposition Type: ", disposition1, value = TRUE)
disposition <- str_trim(substr(disposition, 18, nchar(disposition)))
##Plea
plea <- str_trim(grep("Defendant Plea: ", disposition1, value = TRUE))
plea <- str_trim(substr(plea, 16, nchar(plea)))
##Charges
charges <- str_trim(grep("Charge [0-9]", docket.info, value = TRUE))
charges <- unlist(strsplit(charges, "Charge"))
charges <- str_trim(grep("Count", charges, value = TRUE))
##Charge Type
charge_class <- str_trim(grep("Statute", docket.info, value = TRUE))
#Number of Charges for looping
num_charges <- length(agency)
for (i in 1:num_charges) {
  agency_out <- str_trim(str_sub(agency[i], 8, -26))
  charges_out <- str_trim(substr(charges[i], 14, nchar(charges[i])))
  charge_class_out <- str_trim(str_sub(charge_class[i], -14, -7))
  charge_type <- paste0("Felony ", charge_class_out)
  ##Insert rows
  insert_row <- as.data.frame(cbind(case.number.out, file.date.out, agency_out, lastname, firstname, middlename, charge_type, charges_out, disposition[i], plea[i], sentence.date, sentence[i]),
                              stringsAsFactors=FALSE)
  colnames(insert_row) <- c("Case.Number", "File.Date", "Charge.Agency", "Last.Name", "First.Name", "Middle.Initial", "Charge.Type", "Charge", "Disposition.Type", "Defendant.Plea", "Sentence.Date", "Sentence")
  con <- dbConnect(RSQLite::SQLite(), "Court Cases.sqlite") 
  dbSendQuery(con, sqlAppendTable(ANSI(), "Cases", insert_row))
  dbDisconnect(con)
}
}


#Misdemeanors
count <- 0
for (m in 1:num_cases) {
misdemeanors <- paste0(16, "CM", big.list[m])
filled_form <- set_values(form, CASENUMBER = misdemeanors)
##submit form
result <- submit_form(session, filled_form, submit = submit_button)
##extract html of case page
case <- read_html(result)
#Get Case Number from html Headings
headings.info <- case %>%
  html_nodes("b") %>%
  html_text()
#defendent's name, file date, court room, near appearance, birth date
defend.info <- case %>%
  html_nodes("tr:nth-child(2) pre:nth-child(1)") %>%
  html_text()
#Check if case record exists
if(identical(defend.info, character(0))){
  count = count + 1
  if (count == 5){
    break
  }
  print(paste0(m, " got broken")) #this line is not necessary -- good for testing
  next
}
print(paste0("moving on after ", m)) #this line is not necessary -- good for testing
count <- 0
#docket information
d.string1 <- "pre:nth-child(9) , table:nth-child(8) b"
d.string2 <- "pre:nth-child(7) , table:nth-child(6) b"
d.string3 <- "pre:nth-child(5) , table:nth-child(4) b"
docket.info <- case %>%
  html_nodes(d.string1) %>%
  html_text()
if(length(docket.info) == 0) {
  docket.info <- case %>%
    html_nodes(d.string2) %>%
    html_text()}
if(length(docket.info) == 0) {
  docket.info <- case %>%
    html_nodes(d.string3) %>%
    html_text()}
#make information into neat lists
defend.info <- unlist(strsplit(defend.info, "*\r\n"))
docket.info <- unlist(strsplit(docket.info, "*\r\n"))

#Extract Information
##Case Number
case.number <- grep("CASE ", headings.info, value = TRUE)
case.number.out <- str_trim(substr(case.number[1], 32 , nchar(case.number[1])))
##File Date
file.date <- str_trim(grep("File Date: ", defend.info, value = TRUE))
file.date.out <- str_trim(substr(file.date[1], 11 , nchar(file.date[1])))
##Defendant
name <- str_trim(grep("DEFENDANT: ", defend.info, value = TRUE))
name1 <- unlist(strsplit(name, " "))
lastname <- name1[2]
firstname <- name1[4]
middlename <- name1[5]
##Agency
agency <- str_trim(grep("Agency: ", docket.info, value = TRUE))
##Sentencing
sentencing <- str_trim(grep("Sentence:", docket.info, value = TRUE))
sentencing1 <- str_trim(str_sub(sentencing, 0, -9))
###Sentencing Date
sentence.date <- str_trim(substr(sentencing[1], 10, nchar(sentencing[1])))
###Sentence
sentence0 <- paste(sentencing1, collapse= " ")
sentence1 <- unlist(strsplit(sentence0, "Sentence: ([0-9][0-9])"))
sentence <- str_trim(grep("Sentence:", sentence1, value = TRUE))
##Disposition
disposition0 <- str_trim(grep("Disposition Type: ", docket.info, value = TRUE))
disposition1 <- unlist(str_split(disposition0, "  "))
disposition <- grep("Disposition Type: ", disposition1, value = TRUE)
disposition <- str_trim(substr(disposition, 18, nchar(disposition)))
##Plea
plea <- str_trim(grep("Defendant Plea: ", disposition1, value = TRUE))
plea <- str_trim(substr(plea, 16, nchar(plea)))
##Charges
charges <- str_trim(grep("Charge [0-9]", docket.info, value = TRUE))
charges <- unlist(strsplit(charges, "Charge"))
charges <- str_trim(grep("Count", charges, value = TRUE))
##Charge Type
charge_class <- str_trim(grep("Statute", docket.info, value = TRUE))
#Number of Charges for looping
num_charges <- length(agency)
for (i in 1:num_charges) {
  agency_out <- str_trim(str_sub(agency[i], 8, -26))
  charges_out <- str_trim(substr(charges[i], 14, nchar(charges[i])))
  charge_class_out <- str_trim(str_sub(charge_class[i], -14, -7))
  charge_type <- paste0("Misdemeanor ", charge_class_out)
  ##Insert rows
  insert_row <- as.data.frame(cbind(case.number.out, file.date.out, agency_out, lastname, firstname, middlename, charge_type, charges_out, disposition[i], plea[i], sentence.date, sentence[i]),
                              stringsAsFactors=FALSE)
  colnames(insert_row) <- c("Case.Number", "File.Date", "Charge.Agency", "Last.Name", "First.Name", "Middle.Initial", "Charge.Type", "Charge", "Disposition.Type", "Defendant.Plea", "Sentence.Date", "Sentence")
  con <- dbConnect(RSQLite::SQLite(), "Court Cases.sqlite") 
  dbSendQuery(con, sqlAppendTable(ANSI(), "Cases", insert_row))
  dbDisconnect(con)
}
}


#DUIs
count <- 0
for (d in 1:num_cases) {
  dui <- paste0(16, "DT", big.list[d])
  ##fill out form
  filled_form <- set_values(form, CASENUMBER = dui)
  ##submit form
  result <- submit_form(session, filled_form, submit = submit_button)
  ##extract html of case page
  case <- read_html(result)
  
  #Get Case Number from html Headings
  headings.info <- case %>%
    html_nodes("b") %>%
    html_text()
  #defendent's name, file date, court room, near appearance, birth date
  defend.info <- case %>%
    html_nodes("tr pre:nth-child(1)") %>%
    html_text()
  #Check if case record exists. NEXT and BREAK code placed here.
  if(identical(defend.info, character(0))){
    count <- count + 1
    if (count == 5){
      break
    } else
      next}
  
  print(paste0("moving on after ", d)) #this line is not necessary -- good for testing
  count <- 0
  #docket information
  d.string1 <- "pre:nth-child(9) , table:nth-child(8) b"
  d.string2 <- "pre:nth-child(7) , table:nth-child(6) b"
  d.string3 <- "pre:nth-child(5) , table:nth-child(4) b"
  docket.info <- case %>%
    html_nodes(d.string1) %>%
    html_text()
  if(length(docket.info) == 0) {
    docket.info <- case %>%
      html_nodes(d.string2) %>%
      html_text()}
  if(length(docket.info) == 0) {
    docket.info <- case %>%
      html_nodes(d.string3) %>%
      html_text()}
  
  #make information into neat lists
  defend.info <- unlist(strsplit(defend.info, "*\r\n"))
  docket.info <- unlist(strsplit(docket.info, "*\r\n"))
  #Extract Information
  ##Case Number
  case.number <- grep("CASE ", headings.info, value = TRUE)
  case.number.out <- str_trim(substr(case.number[1], 31 , nchar(case.number[1])))
  ##File Date
  file.date <- str_trim(grep("File Date: ", defend.info, value = TRUE))
  file.date.out <- str_trim(substr(file.date[1], 11 , nchar(file.date[1])))
  ##Defendant
  name <- defend.info[1]
  name1 <- unlist(strsplit(name, " "))
  name1 <- name1[name1 != ""]
  lastname <- name1[1]
  firstname <- name1[2]
  middlename <- name1[3]
  ##Agency
  agency <- str_trim(grep("Agency: ", docket.info, value = TRUE))
  ##Sentencing
  sentencing <- str_trim(grep("Sentence:", docket.info, value = TRUE))
  sentencing1 <- str_trim(str_sub(sentencing, 0, -9))
  ###Sentencing Date
  sentence.date <- str_trim(substr(sentencing[1], 10, nchar(sentencing[1])))
  ###Sentence
  sentence0 <- paste(sentencing1, collapse= " ")
  sentence1 <- unlist(strsplit(sentence0, "Sentence: ([0-9][0-9])"))
  sentence <- str_trim(grep("Sentence:", sentence1, value = TRUE))
  ##Disposition
  disposition0 <- str_trim(grep("Disposition Type: ", docket.info, value = TRUE))
  disposition1 <- unlist(str_split(disposition0, "  "))
  disposition <- grep("Disposition Type: ", disposition1, value = TRUE)
  disposition <- str_trim(substr(disposition, 18, nchar(disposition)))
  ##Plea
  plea <- str_trim(grep("Defendant Plea: ", disposition1, value = TRUE))
  plea <- str_trim(substr(plea, 16, nchar(plea)))
  ##Charges
  charges <- str_trim(grep("Complaint [0-9]", docket.info, value = TRUE))
  charges <- unlist(strsplit(charges, "Complaint"))
  charges <- str_trim(grep("Count", charges, value = TRUE))
  ##Charge Type
  charge_class <- str_trim(grep("Statute", docket.info, value = TRUE))
  #Number of Charges for looping
  num_charges <- length(agency)
  for (i in 1:num_charges) {
    agency_out <- str_trim(str_sub(agency[i], 8, -16))
    charges_out <- str_trim(substr(charges[i], 14, nchar(charges[i])))
    charge_class_out <- str_trim(str_sub(charge_class[i], -14, -7))
    charge_type <- paste0("DUI ", charge_class_out)
    ##Insert rows
    insert_row <- as.data.frame(cbind(case.number.out, file.date.out, agency_out, lastname, firstname, middlename, charge_type, charges_out, disposition[i], plea[i], sentence.date, sentence[i]),
                                stringsAsFactors=FALSE)
    colnames(insert_row) <- c("Case.Number", "File.Date", "Charge.Agency", "Last.Name", "First.Name", "Middle.Initial", "Charge.Type", "Charge", "Disposition.Type", "Defendant.Plea", "Sentence.Date", "Sentence")
    con <- dbConnect(RSQLite::SQLite(), "Court Cases.sqlite") 
    dbSendQuery(con, sqlAppendTable(ANSI(), "Cases", insert_row))
    dbDisconnect(con)
  }
}

#Import to data frame
con <- dbConnect(RSQLite::SQLite(), "Court Cases.sqlite")
courts.table <- dbGetQuery(con,'select * from "Cases"')


##Incorporate Daily Jail Log
#line right below downloads the 'djl.all.csv' from Stuart Levy into working drive
#download.file("http://dart.ncsa.uiuc.edu/stuffed/bpnj/daily_jail_log/djl.all.csv", "djl.all.csv", quiet = TRUE)
data <-  read.csv('djl.all.csv',  header = TRUE)
data <- subset(data, select = c("Name", "Race", "Age", "Sex"))
#Split the name in order to match with courts data
name <- as.character(data$Name)
race <- as.character(data$Race)
age <- as.integer(data$Age)
sex <- as.character(data$Sex)
number <- length(name)
datalist <- list()
for (k in 1:number) {
  name1 <- unlist(strsplit(name[k], " "))
  lastname <- str_trim(str_sub(name1[1], 0, -2))
  firstname <- name1[2]
  middlename <- substring(name1[3], 1, 1)
  subset <- data.frame(lastname, firstname, middlename, race[k], age[k], sex[k])
  datalist[[k]] <- subset # add it to your list
}
jail_log <- do.call(rbind, datalist)
colnames(jail_log) <- c("Last.Name", "First.Name", "Middle.Initial", "Race", "Age", "Sex")
#Change column class to match with courts data
jail_log <- as.data.frame(sapply(jail_log, toupper))
jail_log$Last.Name <- as.character(jail_log$Last.Name)
jail_log$First.Name <- as.character(jail_log$First.Name)
jail_log$Middle.Initial <- as.character(jail_log$Middle.Initial)


##Incorporate Inmate Lookup
#Extract information from Inmate Lookup
jail <- read_html("Inmate Lookup.html")
inmate.records <- jail %>% 
  html_nodes("#accordion") %>%
  html_text()	   
#inmate.records
#Extract Names
inmate.names <- jail %>% 
  html_nodes("#accordion a") %>%
  html_text()	   
inmate.names.trim <- gsub("\\s+", " ", inmate.names)
inmate.names.trim <- str_trim(inmate.names.trim)
#inmate.names.trim
#Extract Race
inmate.race <- jail %>% 
  html_nodes(xpath='//*/div/div[1]/div[1]/p/strong[4]/following-sibling::text()[1]') %>%
  html_text()
inmate.race.trim <- gsub("\\s+", " ", inmate.race)
inmate.race.trim <- str_trim(inmate.race.trim)
#inmate.race.trim
#Extract Age
inmate.age <- jail %>% 
  html_nodes(xpath='//*/div/div[1]/div[1]/p/strong[3]/following-sibling::text()[1]') %>%
  html_text()
inmate.age.trim <- gsub("\\s+", " ", inmate.age)
inmate.age.trim <- str_trim(inmate.age.trim)
#inmate.age.trim
#Extract Gender
inmate.gender <- jail %>% 
  html_nodes(xpath='//*/div/div[1]/div[1]/p/strong[5]/following-sibling::text()[1]') %>%
  html_text()
inmate.gender.trim <- gsub("\\s+", " ", inmate.gender)
inmate.gender.trim <- str_trim(inmate.gender.trim)
#inmate.gender.trim
#Create table
con <- dbConnect(RSQLite::SQLite(), "Court Cases.sqlite")
dbSendQuery(con,
            "CREATE TABLE Inmates
            ('Last.Name' TEXT,
            'First.Name' TEXT,
            'Middle.Initial' TEXT,
            'Race' TEXT,
            'Age' TEXT,
            'Sex' TEXT)")
dbDisconnect(con)
#Get number of bookings
num_booking <- length(inmate.names.trim)
for (l in 1:num_booking) {
  name_split <- unlist(strsplit(inmate.names.trim[l], " "))
  firstname <- name_split[1]
  if (length(name_split) == 3) {
    lastname <- name_split[3]
    middlename <- substring(name_split[2], 1, 1)
  } else {lastname <- name_split[2]}
  #Insert rows
  insert_row <- as.data.frame(cbind(lastname, firstname, middlename, inmate.race.trim[l], inmate.age.trim[l], inmate.gender.trim[l]), stringsAsFactors=FALSE)
  colnames(insert_row) <- c("Last.Name", "First.Name", "Middle.Initial", "Race", "Age", "Sex")
  con <- dbConnect(RSQLite::SQLite(), "Court Cases.sqlite") 
  dbSendQuery(con,
              sqlAppendTable(ANSI(), "Inmates", insert_row))
  dbDisconnect(con)
}

#Import to data frame
con <- dbConnect(RSQLite::SQLite(), "Court Cases.sqlite")
inmates.table <- dbGetQuery(con,'select * from "Inmates"')
dbDisconnect(con)

#Combine tables
combined1 <- merge(courts.table, jail_log)
combined2 <- merge(courts.table, inmates.table)
combined <- rbind(combined1, combined2)
combined <- combined[!duplicated(combined), ]

combined$File.Date <- as.Date(combined$File.Date, "%m/%d/%y")
combined$Sentence.Date <- as.Date(combined$Sentence.Date, "%m/%d/%y")

circuit_clerk <- subset(combined, select = c("Case.Number", "File.Date", "Race", "Sex", "Age", "Charge.Type", "Charge.Agency", "Charge", "Disposition.Type", "Defendant.Plea", "Sentence.Date", "Sentence"))
#replacing variables in race column
circuit_clerk$Race <- as.character(circuit_clerk$Race)
circuit_clerk$Sex <- as.character(circuit_clerk$Sex)
circuit_clerk$Race[circuit_clerk$Race == "B"] <- "Black"
circuit_clerk$Race[circuit_clerk$Race == "W"] <- "White"
circuit_clerk$Race[circuit_clerk$Race == "A"] <- "Asian/Pacific Islander"
circuit_clerk$Race[circuit_clerk$Race == "H"] <- "Hispanic"
circuit_clerk$Race[circuit_clerk$Race == "U"] <- "Unknown"
circuit_clerk$Race[circuit_clerk$Race == "I"] <- "Native American/Alaskan"
circuit_clerk$Race[circuit_clerk$Race == "WH"] <- "White Hispanic"
#Replacing variables in Sex
circuit_clerk$Sex[circuit_clerk$Sex == "M"] <- "Male"
circuit_clerk$Sex[circuit_clerk$Sex == "F"] <- "Female"
circuit_clerk <-circuit_clerk[!(circuit_clerk$Race==""),]
write.csv(circuit_clerk, "circuit_clerk.csv")
