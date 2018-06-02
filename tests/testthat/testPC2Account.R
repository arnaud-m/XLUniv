context("Testing PC2Account")


test_that("All Account creation work", {
  
  # Entrée des données:
  teamdf <- data.frame(
    Team = head(letters, 5),
    Group = head(LETTERS, 5),
    Member1 = sample(letters, 5),
    Member2 = sample(LETTERS, 5)
  )
  path <- tempfile()
  namefilecsv <- paste(path, "/teamcsv", ".csv", sep = "")
  write.csv(teamdf, file = namefilecsv)
  teamcsv <- read.csv(namefilecsv, header = TRUE, sep= ",", encoding = "UTF-8") 
  CreatePC2AccountWorkbook(teamcsv, filename = "testPC2Account", nteams = 100, njudges = 4, teamcapa = NA, location = paste(path, "/", sep = ""))
  
  wb <- XLConnect::loadWorkbook(paste(location, filename, ".xlsx", sep = ""), create = TRUE)
  
 
 
 
 
 
 }
