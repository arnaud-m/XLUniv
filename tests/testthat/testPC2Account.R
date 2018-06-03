context("Testing PC2Account")


test_that("All Account creation work", {

  # Entrée des données:
  teamdf <- data.frame(
              Team = head(letters, 5),
              Group = head(LETTERS, 5),
              Member1 = sample(letters, 5),
              Member2 = sample(LETTERS, 5)
            )
  path <- tempdir()
  namefilecsv <- paste(tempfile(), ".csv", sep = "")
  write.csv(teamdf, file = namefilecsv)
  loc = paste(path, "/", sep = "")
  filename = "testPC2Account"
  CreatePC2AccountWorkbook(namefilecsv, filename = filename, nteams = 100, njudges = 4, teamcapa = NA, location = loc)

  wb <- XLConnect::loadWorkbook(paste(loc, filename, ".xlsx", sep = ""), create = TRUE)
  tsvfile <- read.table(paste(loc, filename, ".tsv", sep = ""), header = TRUE, sep = "\t") #Récupère le fichier tsv

  # Test for the tsv file:
  for (k in 1:5){
    expect_equal(paste( teamdf$Group[k]), paste(tsvfile$group[k+6]) )
    expect_equal(paste( teamdf$Team[k]), paste(tsvfile$displayname[k+6]) )
    expect_equal(paste( teamdf$Member1[k]), paste(tsvfile$Member1[k+6]) )
    expect_equal(paste( teamdf$Member2[k]), paste(tsvfile$Member2[k+6]) )
  }


  # Test for the readable data worksheet
  for (k in 1:5){
    expect_equal(paste( teamdf$Group[k]), paste(XLConnect::readWorksheet(wb, "readable data", 7, 4, 12, 4)[k, ]) )
    expect_equal(paste( teamdf$Team[k]), paste(XLConnect::readWorksheet(wb, "readable data", 7, 5, 12, 5)[k, ]) )
    expect_equal(paste( teamdf$Member1[k]), paste(XLConnect::readWorksheet(wb, "readable data", 7, 9, 12, 9)[k, ]) )
    expect_equal(paste( teamdf$Member2[k]), paste(XLConnect::readWorksheet(wb, "readable data", 7, 10, 12, 10)[k, ]) )
  }
  
  # Test for the compact table sort by student worksheet
  for(k in 1:10){
    expect_equal( sort( c(paste(teamdf$Member1),paste(teamdf$Member2)) )[k], paste(XLConnect::readWorksheet(wb, "compact table sort by student", 1, 1, 11, 1)[k, ]))

    condition = order( c(paste(teamdf$Member1), paste(teamdf$Member2)) )[k]

    expect_equal( if(condition > 5) {   #when > 5, the student is in Member2, else in Member1
                 paste(tsvfile$account[condition-5+6]) #+6 because there is 4 judges, 1 scoreboard and a team1 for test
                 } else { paste(tsvfile$account[condition+6]) }
                , paste(XLConnect::readWorksheet(wb, "compact table sort by student", 1, 2, 11, 2)[k, ]))

    expect_equal( if(condition > 5) {   #when > 5, the student is in Member2, else in Member1
                 paste(tsvfile$password[condition-5+6]) #+6 because there is 4 judges, 1 scoreboard and a team1 for test
                 } else { paste(tsvfile$password[condition+6]) }
                 , paste(XLConnect::readWorksheet(wb, "compact table sort by student", 1, 3, 12, 3)[k, ]))
  }
  
  # Test for the free teams worksheet
  for(k in 1:96){
    expect_equal(paste(tsvfile$account[k+6+5]), paste(XLConnect::readWorksheet(wb, "free teams", 1, 1, 96, 1)[k, ])) #+6+5 because there is  4 judges, 1 scoreboard, a team1 for test and 5 team
    expect_equal(paste(tsvfile$password[k+6+5]), paste(XLConnect::readWorksheet(wb, "free teams", 1, 2, 96, 2)[k, ])) #+6+5 because there is  4 judges, 1 scoreboard, a team1 for test and 5 team
  }
  
  expect_equal("logical(0)", paste(XLConnect::readWorksheet(wb, "free teams", 1, 3, 96, 3)))
  expect_equal("logical(0)", paste(XLConnect::readWorksheet(wb, "free teams", 1, 4, 96, 4)))

})
