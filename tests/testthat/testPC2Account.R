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
  namefilecsv <- paste(path, ".csv", sep = "")
  write.csv(teamdf, file = namefilecsv)
  loc = paste(path, "/", sep = "")
  CreatePC2AccountWorkbook(namefilecsv, filename = "testPC2Account", nteams = 100, njudges = 4, teamcapa = NA, location = loc)

  wb <- XLConnect::loadWorkbook(paste(loc, filename, ".xlsx", sep = ""), create = TRUE)
  location



 }
