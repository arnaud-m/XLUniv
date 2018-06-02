#' Create a tsv file and a xlsx workbook used for the registration process of a programming contest based on PC^2.
#'
#' @param teamfile a csv file that contains the team information
#' @param filename a character prefix for the generated files
#' @param teamcapa the capacity of a team
#' @param location where to save the created files (xls and tsv file)
#' @inheritParams GenerateAccountPC2
#'
#' the team information includes: Team, Group, Member1, ... , Member\code{teamfile}.
#'
#' The tsv file contains a data frame generated with \code{GenerateAccountPC2} with additional columns that give the members of the teams.
#'
#' The xlsx file contains three sheets:
#' \enumerate {
#' \item a human-readable version of the tsv file
#' \item a compact and sorted table of login/password by student
#' \item a listing of free teams for late registration:  logins/passwords ; enough empty columns to handwrite student IDs.
#' }
#'
#' @export
#' @examples
#'
CreatePC2AccountWorkbook <- function(teamfile, filename = "pc2account", nteams = 100, njudges = 4, teamcapa = NA, location = path.expand("~/")) {
  # On suppose que le fichier csv contient les colonnes suivantes.
  teamdata <- data.frame(
                Team = head(letters, 5),
                Group = head(LETTERS, 5),
                Member1 = sample(letters, 5),
                Member2 = sample(LETTERS, 5)
              )


  #teamdata <- data.frame(
  #              Team = teamfile$Team,
  #              Group = teamfile$Group,
  #              Member1 = teamfile$Member1,
  #              Member2 = teamfile$Member2
  #            )
  # Il faut juste vérifier avant que les noms des colonnes dans le fichier csv teamfile s'appel Team, Group, Member1, Member2
  # Cela résoudrait le problème de lecture des fichiers csv


  # Tsv file
  tsvfile <- GenerateAccountPC2(nteams = nteams, njudges = njudges, teams = teamdata[, 1], groups = teamdata[, 2])
  tsvfile <- data.frame(tsvfile, Member1 = NA, Member2 = NA)

  for(k in 1:length(teamdata$Group)) {
    tsvfile$Member1[tsvfile$group == paste(teamdata[k, 2])] = paste(teamdata$Member1[k])
    tsvfile$Member2[tsvfile$group == paste(teamdata[k, 2])] = paste(teamdata$Member2[k])
  }

  write.table(tsvfile, file = paste(location, filename, ".tsv", sep = ""), quote = FALSE , sep = "\t")

  # The xslx file
  wb <- XLConnect::loadWorkbook(paste(location, filename, ".xlsx", sep = ""), create = TRUE)

  # The human readable sheet
  XLConnect::createSheet(wb, "readable data")
  XLConnect::writeWorksheet(wb, tsvfile, sheet = "readable data", startRow = 1, startCol = 1)

  # Compact table sort by student sheet
  student1 <- !is.na(tsvfile$Member1)
  student2 <- !is.na(tsvfile$Member2)
  cstable <- data.frame(
               student = c(tsvfile$Member1[student1], tsvfile$Member2[student2]),
               login = c( paste(tsvfile$account[student1]), paste(tsvfile$account[student2]) ),
               password = c( paste(tsvfile$password[student1]), paste(tsvfile$password[student2]) )
             )
  cstable <- cstable[do.call(order, cstable), ]
  XLConnect::createSheet(wb, "compact table sort by student")
  XLConnect::writeWorksheet(wb, cstable, sheet = "compact table sort by student", startRow = 1, startCol = 1)

  # Free teams sheet
  notstudent <- is.na(tsvfile$Member1)
  notstudent[1:(njudges + 2)] <- FALSE
  ftable <- data.frame(
              login = paste(tsvfile$account[notstudent]),
              password = paste(tsvfile$password[notstudent]),
              member1 = NA, member2 = NA
            )
  XLConnect::createSheet(wb, "free teams")
  XLConnect::writeWorksheet(wb, ftable, sheet = "free teams", startRow = 1, startCol = 1)

  XLConnect::saveWorkbook(wb)

}
