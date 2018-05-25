context("Testing Sesame")


test_that("All Calcul work", {
  
#entrée des notes:
df <- data.frame(nom=sample(LETTERS,20), prenom=sample(letters,20), numetu=seq(20))
listing <- c("nom", "prenom", "numetu")
notes <- data.frame(CC=sample(0:0.5:20,20,replace=TRUE), CP=sample(0:0.5:20,20,replace=TRUE), CT=sample(0:0.5:20,20,replace=TRUE))
CreateSesameWorkbook("test.xlsx", df, listing = listing, sort = TRUE)
wb <- loadWorkbook("test.xlsx", create = TRUE)
for(k in 4:6){
  XLConnect::setCellFormula(wb,sheet="NOTES",2:21,k,paste(notes[[k-3]]))
}
XLConnect::saveWorkbook(wb)
  
#vérification des formules:

  #pour CC, Proj et CT:
  for(k in 1:3){
    #moyenne de chaque note
    expect_equal(mean(notes[[k]]),readWorksheet(wb,"NOTES",22,4,23,7)[[k]])
    #ecart type
    expect_equal(sd(notes[[k]]),readWorksheet(wb,"NOTES",23,4,24,7)[[k]])
    #countIf
    expect_equal(sum(notes[[k]]>=10),readWorksheet(wb,"NOTES",24,4,25,7)[[k]])
    expect_equal(sum(notes[[k]]<=5),readWorksheet(wb,"NOTES",25,4,26,7)[[k]])
    expect_equal(sum(notes[[k]]<=1),readWorksheet(wb,"NOTES",26,4,27,7)[[k]])
  }
  
  
  #pour notes:
  #notes
  m=round((0.3*notes[[1]]+0.3*notes[[2]]+0.4*notes[[3]])*2)/2
  expect_equal(m,readWorksheet(wb,"NOTES",1,7,21,8)[[1]])
  #moyenne
  expect_equal(mean(m),readWorksheet(wb,"NOTES",22,4,23,8)[[4]])
  #ecart type
  expect_equal(sd(m),readWorksheet(wb,"NOTES",23,4,24,7)[[4]])
  #CountIf
  expect_equal(sum(m>=10),readWorksheet(wb,"NOTES",24,4,25,7)[[4]])
  expect_equal(sum(m<=5),readWorksheet(wb,"NOTES",25,4,26,7)[[4]])
  expect_equal(sum(m<=1),readWorksheet(wb,"NOTES",26,4,27,7)[[4]])
})
