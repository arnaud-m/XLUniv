context("Testing Grade")


test_that("the Grade works", {
  coefs <- c(CC = 0.2, CP = 0.2, CT = 0.6)
  expect_equal("MROUND(1*A1, 0.5)", GetGradeFormula(1, 1, 1))
  expect_equal("MROUND(0.2*C5 + 0.2*D5 + 0.3*E5 + 0.15*F5 + 0.15*G5, 0.5)", GetGradeFormula(5, 3, c(0.2, 0.2, 0.3, 0.15, 0.15)))
  expect_equal("MROUND(0.2*K5 + 0.2*L5 + 0.6*M5, 0.5)", GetGradeFormula(5, 11, coefs))
  expect_equal("MROUND(0.2*J5 + 0.2*K5 + 0.6*L5, 40)", GetGradeFormula(5, 10, coefs, round = 40))
  expect_equal("0.2*J5 + 0.2*K5 + 0.6*L5", GetGradeFormula(5, 10, coefs, round = NA))

  # Erreurs (argument manquant):
  expect_error(GetGradeFormula(n = 5, m = 11))
  expect_error(GetGradeFormula(n = 5, coefs = coefs))
  expect_error(GetGradeFormula(m = 11, coefs = coefs))

  expect_error(GetGradeFormula(-1, 11, coefs))
  expect_error(GetGradeFormula(0, 11, coefs))
  expect_error(GetGradeFormula(5, -4, coefs))
  expect_error(GetGradeFormula(5, 0, coefs))
  expect_error(GetGradeFormula(5, 10, coefs, round = -1))
  expect_error(GetGradeFormula(5, 10, coefs, round = 0))
  expect_error(GetGradeFormula(5, 10, 0))
  expect_error(GetGradeFormula(5, 10, -1))

  expect_error(GetGradeFormula(6, 11.87, coefs, round = NA))
  expect_error(GetGradeFormula(6.24, 11, coefs, round = NA))
  expect_error(GetGradeFormula(6, 2, coefs, max = 2.4))

  expect_error(GetGradeFormula(6, 11.87, coefs, max = -1))
  expect_error(GetGradeFormula(6, 11.87, coefs, max = 0))
  expect_error(GetGradeFormula(6, 11.87, coefs, max = 5)) ##avec length(coefs) = 3

})
