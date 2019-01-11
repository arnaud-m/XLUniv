#' Send email with authentification information to members of the teams 
#'
#' @param accountfile a tsv file that contains the account information in the PC2 Format
#' @param from the email adress of the sender
#' @param smtp the smtp list of \code{send.mail}
#' @param dryrun only print the messages if \code{TRUE}, send them otherwise 
#'
#' the PC2 account information includes : displayname, password, member1 , member2.
#'
#' @export
# #' @examples
SendMailPC2Account <- function(accountfile, from, smtp, dryrun = TRUE) {
  bodyFormat <- "Cher participant,\n\nMerci de vous êtes inscrit au concours UCAnCODE !\n\nVous trouverez ci-dessous votre nom d'utilisateur et votre mot de passe pour vous connecter au serveur pendant le concours.\n\n - Équipe : %s\n - Composition : %s\n - Nom d'utilisateur : %s\n - Mot de passe : %s\n\nCordialement,\nLéquipe d'UCAnCODE.\n" 
  subject <- "Concours UCAnCODE : Login/Password"
   GetMsg <- function(x) {
     to <- x[c("member1", "member2")];
     to <- subset(to, !is.na(to))
     if(length(to) > 0) {
       body <- sprintf(bodyFormat, x["displayname"], paste(to, collapse = ", "), x["account"], x["password"])
       to <- paste0(to, "@etu.unice.fr")
       return(list(to = to, body = body))
     } else return (NULL)
   }
  
  PrintMail <- function(msg) {
    cat(msg$to, "\n")
    cat(msg$body, "\n\n")
  }
  
  SendMail <- function(msg) {
    mailR::send.mail(
             from = from,
##             cc = NULL,
             to = msg$to,
             subject = subject,
             body = msg$body,
             smtp = smtp,
             ## debug = TRUE,
             authenticate = TRUE,
             send = TRUE
           )
  }
  
  accounts <- read.csv(accountfile, header = TRUE, sep= "\t", stringsAsFactors = FALSE, encoding = "UTF-8")
  messages <- Filter( function(x) !is.null(x), apply(accounts, 1, GetMsg) )
  if(require(mailR) && !dryrun) {
    ## Send mails
    sapply(messages, SendMail)
  } else {
     ## Print mails (dry run)
    sapply(messages, PrintMail)
  }
  return(invisible(NULL))
}
