context("Testing Sesame")


test_that("All Calcul work", {
  
# Entrée des notes:
df <- data.frame(nom = sample(LETTERS, 20), prenom = sample(letters, 20), numetu = seq(20))
listing <- c("nom", "prenom", "numetu")
notes <- data.frame(CC=sample(0:0.5:20, 20, replace = TRUE), CP = sample(0:0.5:20, 20, replace = TRUE), CT = sample(0:0.5:20, 20, replace = TRUE))
namefile<-paste(tempfile(),".xlsx")
CreateSesameWorkbook(namefile, df, listing = listing, sort = TRUE)
wb <- XLConnect::loadWorkbook(namefile, create = TRUE)
for(k in 4:6){
  XLConnect::setCellFormula(wb, sheet = "NOTES", 2:21, k, paste(notes[, k-3]))
}
XLConnect::saveWorkbook(wb)
  
# Vérification des formules:

  # Pour CC, Proj et CT:
  for(k in 1:3){
    # Moyenne de chaque note
    expect_equal(mean(notes[, k]), XLConnect::readWorksheet(wb, "NOTES", 22, 4, 23, 6)[, k])
    # Écart type
    expect_equal(sd(notes[, k]), XLConnect::readWorksheet(wb, "NOTES", 23, 4, 24, 6)[, k])
    # CountIf
    expect_equal(sum(notes[, k] >= 10), XLConnect::readWorksheet(wb, "NOTES", 24, 4, 25, 6)[, k])
    expect_equal(sum(notes[, k] <= 5), XLConnect::readWorksheet(wb, "NOTES", 25, 4, 26, 6)[, k])
    expect_equal(sum(notes[, k] <= 1), XLConnect::readWorksheet(wb, "NOTES", 26, 4, 27, 6)[, k])
  }
  
  
  # Pour notes:
  # Notes
  m = round((0.3*notes[, 1] + 0.3*notes[, 2] + 0.4*notes[, 3])*2)/2
  expect_equal(m, XLConnect::readWorksheet(wb, "NOTES", 1, 7, 21, 7)[, 1])
  # Moyenne
  expect_equal(mean(m), XLConnect::readWorksheet(wb, "NOTES", 22, 7, 23, 7)[, 1])
  # Écart type
  expect_equal(sd(m), XLConnect::readWorksheet(wb, "NOTES", 23, 7, 24, 7)[, 1])
  # CountIf
  expect_equal(sum(m >= 10), XLConnect::readWorksheet(wb, "NOTES", 24, 7, 25, 7)[, 1])
  expect_equal(sum(m <= 5), XLConnect::readWorksheet(wb, "NOTES", 25, 7, 26, 7)[, 1])
  expect_equal(sum(m <= 1), XLConnect::readWorksheet(wb, "NOTES", 26, 7, 27, 7)[, 1])
})
