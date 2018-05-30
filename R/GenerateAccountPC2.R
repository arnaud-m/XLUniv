#' Generate a data frame of PC^2 accounts 
#'
#'
#' @param nteams the total number of team accounts
#' @param njudges the number of judge accounts
#' @param teams the names of the registered teams displayed on the scoreboard 
#' @param groups the groups of the registered teams  
#'
#' @return a data frame of PC^2 accounts
#' 
#' The columns of the data frame are:
#'  \enumerate{
#'  \item site - site number
#'  \item account - team login name (ex. team1, judge4, scoreboard2)
#'  \item password - account password
#'  \item group - group name
#'  \item displayname - name to be displayed on scoreboard
#'  \item alias - an alias display name shown to judges to preserve team anonymity
#'  \item permdisplay - true or false, display on scoreboard
#'  \item permlogin - true or false, allowed to login
#' }
#' @export 
#' @examples
#' GenerateAccountPC2( nteams = 10, njudges = 5, teams = head(letters, 5), groups = head(LETTERS, 5))
GenerateAccountPC2 <- function(nteams = 100, njudges = 8, teams = character(0), groups = "nogroup") {
  GetTeamPasswords  <- function(n) {
    vowels <- c("a", "e", "i", "o", "u", "y")
    consonants <- subset(letters, ! letters %in% vowels)
    replicate(n, paste(
                   sample(consonants, 3, replace = TRUE),
                   sample(vowels, 3, replace = TRUE),
                   collapse = "", sep = "")
              )
  }
  
  CreateAccount <- function( account, password = chartr("aeiouy","AEIOUY",account), group = "nogroup", 
                                displayname = account, alias = account, permdisplay = "false", permlogin = "true") {
    data.frame(
      site = 1, 
      account = account,
      password = password,
      group = group,
      displayname = displayname,
      alias = alias,
      permdisplay = permdisplay,
      permlogin = permlogin
    )
  }
  
  
  GetAccountName <- function(prefix, n) paste(prefix, n, sep ="")

  ## TODO Replace tab character by space in the teams 
  nreg <- length(teams)
  nteams <- max(nteams, nreg, na.rm = TRUE)
  nadd <- nteams - length(teams)

  df  <- rbind(
    CreateAccount(GetAccountName("judge", seq(njudges))),
    CreateAccount("scoreboard1", displayname = "Scoreboard"),
    CreateAccount("team1", group = "test", displayname = "TestOnly", alias = "test", permdisplay = "false")
  )
  
  if(nreg > 0) {
    df <- rbind(
      df,
      CreateAccount(account = GetAccountName("team", 1 + seq(nreg)), password = GetTeamPasswords(nreg), displayname = teams, group = groups, permdisplay = "true")
    )
  }
  
  if(nadd > 0 ) {
    df <- rbind(
      df,
      CreateAccount(GetAccountName("team", nreg + 1 + seq(nadd)), password = GetTeamPasswords(nadd), permdisplay = "true")
    )
  }
  return(df)
}
