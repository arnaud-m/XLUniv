#' Create a tsv file and a xlsx workbook used for the registration process of a programming contest based on PC^2.
#'
#' @param teamfile a csv file that contains the team information
#' @param filename a character prefix for the generated files
#' @param teamcapa the capacity of a team
#' @param location where to save the created files (xls and tsv file)
#' @param seed the seed for password generation
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
#' teamdata <- data.frame(Team = head(letters, 5),Group = head(LETTERS, 5),Member1 = sample(letters, 5),Member2 = sample(LETTERS, 5))
#' write.csv(teamdata, file = "~/example.csv")
#' CreatePC2AccountWorkbook("~/example.csv")
CreatePC2AccountWorkbook <- function(teamfile, filename = "pc2account", nteams = 100, njudges = 8, teamcapa = NA, location = path.expand("."), seed = NULL) {

  teamdata <- read.csv(teamfile, header = TRUE, sep= ",", stringsAsFactors = FALSE, encoding = "UTF-8")
  
  ## Clean members
  CleanMembers <- function(x) {
    x <- trimws(x)
    x [ nchar(x) == 0 ] <- NA
    return(x)
  }
  teamdata$Member1 <- CleanMembers(teamdata$Member1)
  teamdata$Member2 <- CleanMembers(teamdata$Member2)
  ## Remove duplicated members
  teamdata$Member2[teamdata$Member1 == teamdata$Member2] <- NA

  ## Set the random seed to generate the same passwords
  if(is.numeric(seed)) {
    set.seed(seed)
  }
  
  ## Generate account
  tsvfile <- GenerateAccountPC2(nteams = nteams, njudges = njudges, teams = teamdata$Team, groups = teamdata$Group)
  ## Add placeholders for the members
  tsvfile <- data.frame(tsvfile, member1 = NA, member2 = NA)
  ## Add the registered team members
  offset <- which(tsvfile$account == "team2")
  teamInd <- seq(offset, length.out = nrow(teamdata))
  tsvfile[ teamInd, paste0("member", 1:2)] <- teamdata[, paste0("Member", 1:2)]
  
  ## Write a csv file
  write.table(tsvfile, file = file.path(location, paste0(filename, ".tsv")), quote = FALSE , sep = "\t", row.names = FALSE)

  # The xslx file
  wb <- XLConnect::loadWorkbook(file.path(location, paste0(filename, ".xlsx")), create = TRUE)

  # The human readable sheet
  XLConnect::createSheet(wb, "Teams")
  XLConnect::writeWorksheet(wb, tsvfile[seq(offset, nrow(tsvfile)),], sheet = "Teams", startRow = 1, startCol = 1)

  # Lookup table sorted by student sheet
  student1 <- !is.na(tsvfile$member1)
  student2 <- !is.na(tsvfile$member2)
  cstable <- data.frame(
               student = c(tsvfile$member1[student1], tsvfile$member2[student2]),
               login = c(tsvfile$account[student1], tsvfile$account[student2] ),
               password = c(tsvfile$password[student1], tsvfile$password[student2])
             )
  cstable <- cstable[do.call(order, cstable), ]
  XLConnect::createSheet(wb, "Students")
  XLConnect::writeWorksheet(wb, cstable, sheet = "Students", startRow = 1, startCol = 1)

  # Free teams sheet
  notstudent <- is.na(tsvfile$member1)
  notstudent[1:offset-1] <- FALSE
  ftable <- data.frame(
              login = tsvfile$account[notstudent],
              password = tsvfile$password[notstudent],
              member1 = NA, member2 = NA
            )
  XLConnect::createSheet(wb, "Accounts")
  XLConnect::writeWorksheet(wb, ftable, sheet = "Accounts", startRow = 1, startCol = 1)

  XLConnect::saveWorkbook(wb)

}
