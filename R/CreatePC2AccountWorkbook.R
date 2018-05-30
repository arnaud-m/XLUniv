#' Create a tsv file and a xlsx workbook used for the registration process of a programming contest based on PC^2.
#'
#' @param teamfile a csv file that contains the team information
#' @param filename a character prefix for the generated files
#' @param teamcapa the capacity of a team
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
CreatePC2AccountWorkbook <- function(teamfile, filename = "pc2account", nteams = 100, njudges = 4, teamcapa = NA) {
  ## On suppose que le fichier csv contient les colonnes suivantes.
  teamdata <- data.frame(
    Team = head(letters,5),
    Group = head(LETTERS, 5),
    Member1 = sample(letters,5),
    Member2 = sample(LETTERS, 5)
  ) 
  
}
