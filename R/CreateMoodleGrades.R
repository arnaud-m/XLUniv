#' Create two csv files with teams grades and results using sphere engine and moodle group plugin
#'
#' @param infile a csv file with runs that is exported from sphere engine
#' @param groupfile a csv file with teams that is exported from the moodle plugin (group auto-selection) 
#' @param outfile the filename prefix for the two generated files
#' @param binary binary or score assessment
#' @param verbose print information about processing 
#'
#' @export
CreateMoodleGrades <- function(infile, groupfile = NULL, outfile = 'moodle-SE', binary = FALSE, verbose = TRUE) {
  ## TODO Store the best submission number.
  results <- read.csv(infile, strip.white = TRUE, stringsAsFactors = FALSE)
  if(verbose) cat('Processing', nrow(results), 'runs\n')
  results$user.email <- tolower(results$user.email)
  results$user.email <- gsub("@etu\\.unice\\.fr$", "@etu\\.univ-cotedazur\\.fr", results$user.email)
  results <- subset(results, grepl('@etu\\.univ-cotedazur\\.fr$', results$user.email))
  if(verbose) cat('Found', nrow(results), 'student runs\n')

    ## Needed for statistics
  lastrun <- max(results$date)
  GetGroupStats <- function(group) {
    ## Find runs 
    x <- subset(results, results$user.email %in% group)
    ## Find AC runs
    accepted <- which(x$status == 15)
    ## Compute the score
    if(length(accepted) == 0) {
      score <- 0;
    } else if(binary) {
      score <- 100
    } else {
      sc <- x$score[accepted]
      if(is.character(sc)) {
        ## Remove percent sign
        sc <- as.numeric(gsub('%', '', sc))
      }
      score <- ceiling(max(sc))
    }
    ## Compute stats
    return (c(
      score = score,
      runs = nrow(x),
      AC = length(accepted),
      durH = ceiling(difftime(max(x$date), min(x$date), units = "hour")),
      sinceD = ceiling(difftime(lastrun, min(x$date), units = "day"))
    ))
  }

  if(is.character(groupfile)) {
  ## Read teams file
  groups <- read.csv(groupfile, skip = 1, strip.white = TRUE, stringsAsFactors = FALSE)
  ## Remove groups with less than 2 members
  groups <- subset(groups, groups$Group.Size > 1)
  if(verbose) cat('Processing', nrow(groups), 'teams\n')
  ## Keep only emails
  groups <- groups[ , grepl("^Member.*Email$", colnames(groups))]
  ## Remove groups without runs
  groups <- subset(groups, apply(groups, 1, function (x) any(x %in% results$user.email))) 
  ## Find students without team that have runs
  x <- unique(results$user.email)
  x <- subset(x, !(x %in% unlist(groups)))
  ## Add these students to the groups 
  groups[nrow(groups) + seq_along(x), 1] <- x
  if(verbose) cat('Found', nrow(groups), 'participating teams\n')

    ## Compute team statistics
    stats <- t(apply(groups, 1, GetGroupStats))
    ## Compute student grades
    grades <- data.frame( user.email = array(t(groups)), score = rep(stats[,"score"], each = ncol(groups)))
    ## Remove empy rows
    grades <- subset(grades, ! is.na(grades$user.email))
    ## Remove domains
    teams <- apply(groups, 2, gsub, pattern = "@etu\\.univ-cotedazur\\.fr$", replacement = "")

  } else {
    ## Find team of one students !
    teams <- unique(results$user.email)
    if(verbose) cat('Processing', length(teams),'students')
    ## Compute student statistics
    stats <- t(sapply(teams, GetGroupStats))
    ## Compute student grades
    grades <- data.frame(user.email = teams, score = stats[, 'score'])
    ## Remove domains
    teams <- gsub(teams, pattern = "@etu\\.univ-cotedazur\\.fr$", replacement = "")
  }
  
  ## Sort grades by names
  grades <- grades[order(grades$user.email), ]
  ## Export grades
  gname <- paste0(outfile, '-grades.csv')
  write.csv(grades, file = gname, quote = FALSE, row.names = FALSE)
  if(verbose) cat('Export', gname, '\n')

  ## Export groups/teams statistics
  teamstats <- data.frame(teams, stats)
  ## Sort so that suspicious behaviors appear first
  teamstats <- teamstats[ order( -teamstats$score, teamstats$runs, teamstats$AC, teamstats$durH, teamstats$sinceD), ]
  rname <- paste0(outfile, '-results.csv')
  write.csv(teamstats, file = rname, quote = FALSE, row.names = FALSE)
  if(verbose) cat('Export', rname, '\nProcessing done\n')
  return(c(gname, rname))
}
