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
#' teamdata <- data.frame(Team = head(letters, 5),Group = head(LETTERS, 5),Member1 = sample(letters, 5),Member2 = sample(LETTERS, 5))
#' write.csv(teamdata, file = "~/example.csv")
#' CreatePC2AccountWorkbook("~/example.csv")
CreatePC2ResultWorkbook <- function(accountfile, boardfile, filename = "resultats-pc2.xlsx") {

  ## Read accounts
  accounts <- read.table(accountfile, header = TRUE, sep= "\t", stringsAsFactors = FALSE, fill = TRUE, quote="", encoding = "UTF-8")
  ## Keep only the teams
  accounts <- subset(accounts, grepl("team", accounts$account))
  accounts$id <- as.numeric(sub("team", "", accounts$account))
  accounts <- subset(accounts, accounts$id > 1)
  accounts <- accounts[c("id", "group", "displayname", "member1", "member2")]
  
  ## Read scoreboard
  scoreboard <- read.table(boardfile, header = FALSE, sep= "\t", stringsAsFactors = FALSE, encoding = "UTF-8", skip = 1)
  scoreboard <- cbind(
    1:nrow(scoreboard),
    scoreboard[c(2, 4, 5)],
    rowSums(scoreboard[seq(7, ncol(scoreboard), 2)])
    )
  colnames(scoreboard) <- c("rank", "id", "solved", "time", "runs")
  scoreboard <- subset(scoreboard, scoreboard$id != 1001)
  scoreboard$id <-   scoreboard$id - 1000
  
  ## Merge the two dataframes
  results <- merge(accounts, scoreboard, all = TRUE)
  results <- subset(results, !is.na(results$solved))
  results <- results [order(results$rank),]
  
  ind <- c("group", "rank", "solved", "time", "runs")
  x <- results[c("member1", ind)]
  colnames(x) <- c("member", ind)
  y <- results[c("member2", ind)]
  colnames(y) <- c("member", ind)
  studresults <- rbind(x, y)
  studresults <- subset(studresults, !is.na(studresults$member) & studresults$runs > 0)
  studresults <- studresults [order(studresults$member),]
  rownames(studresults) <- NULL
  
  ## The xslx file
  wb <- XLConnect::loadWorkbook(filename, create = TRUE)

  ## The human readable sheet
  XLConnect::createSheet(wb, "Ranking")
  XLConnect::writeWorksheet(wb, results, sheet = "Ranking", startRow = 1, startCol = 1)

  ## Lookup table sorted by student sheet
  XLConnect::createSheet(wb, "Students")
  XLConnect::writeWorksheet(wb, studresults, sheet = "Students", startRow = 1, startCol = 1)

  groups <- sort(unique(studresults$group))
  for(group in groups) {
    XLConnect::createSheet(wb, group)
    groupres <- studresults[ studresults$group == group, ]
    groupres <- groupres[order(groupres$rank),]
    XLConnect::writeWorksheet(wb, groupres, sheet = group, startRow = 1, startCol = 1)
  }
  XLConnect::saveWorkbook(wb)
}
