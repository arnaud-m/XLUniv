context("Testing Formula")


test_that("All Formulas work", {
  expect_equal(c("COUNTA(Z2:Z10)","AVERAGE(Z2:Z10)","STDEV(Z2:Z10)","COUNTIF(Z2:Z10, \">=10\")","COUNTIF(Z2:Z10, \"<=5\")","COUNTIF(Z2:Z10, \"<=1\")") , GetStatFormula(n = 10, m = 26,nmin=2))
  expect_equal(c("COUNTA(A4:A8)","AVERAGE(A4:A8)","STDEV(A4:A8)","COUNTIF(A4:A8, \">=10\")","COUNTIF(A4:A8, \"<=5\")","COUNTIF(A4:A8, \"<=1\")") , GetStatFormula(n = 8, m = 1,nmin=4))
  expect_equal(c("COUNTA(M45:M1)","AVERAGE(M45:M1)","STDEV(M45:M1)","COUNTIF(M45:M1, \">=10\")","COUNTIF(M45:M1, \"<=5\")","COUNTIF(M45:M1, \"<=1\")") , GetStatFormula(n = 1, m = 13,nmin=45))
  expect_equal(c("COUNTA(P:P)","AVERAGE(P:P)","STDEV(P:P)","COUNTIF(P:P, \">=10\")","COUNTIF(P:P, \"<=5\")","COUNTIF(P:P, \"<=1\")") , GetStatFormula(n = 0, m = 16,nmin=0)) # Toute la colonne P (revient à écrire par exemple "COUNTA(P)")
  expect_equal(c("COUNTA(P4:P4)","AVERAGE(P4:P4)","STDEV(P4:P4)","COUNTIF(P4:P4, \">=10\")","COUNTIF(P4:P4, \"<=5\")","COUNTIF(P4:P4, \"<=1\")") , GetStatFormula(n = 4, m = 16,nmin= 4 )) # Juste la case P4 (les fonctions comme "average" ne servent donc à rien dans ce cas là)
  
  expect_error(GetStatFormula(n = -1, m = 13,nmin=45)) #erreur
  expect_error(GetStatFormula(n = 1, m = -13,nmin=45)) #erreur
  expect_error(GetStatFormula(n = 1, m = 13,nmin=-45)) #erreur
  
  # Ce qui ne devrait pas fonctionner:
  #expect_error(GetStatFormula(n = 0, m = 16,nmin=7)) 
  # La réponse est c("COUNTA(P7:P)","AVERAGE(P7:P)","STDEV(P7:P)","COUNTIF(P7:P, \">=10\")","COUNTIF(P7:P, \"<=5\")","COUNTIF(P7:P, \"<=1\")")
  #expect_error(GetStatFormula(n = 8, m = 16,nmin=0)) 
  # La réponse est c("COUNTA(P:P8)","AVERAGE(P:P8)","STDEV(P:P8)","COUNTIF(P:P8, \">=10\")","COUNTIF(P:P8, \"<=5\")","COUNTIF(P:P8, \"<=1\")")
  #expect_error(GetStatFormula(n = 74, m = 0,nmin=2)) 
  # La réponse est c("COUNTA(2:74)","AVERAGE(2:74)","STDEV(2:74)","COUNTIF(2:74, \">=10\")","COUNTIF(2:74, \"<=5\")","COUNTIF(2:74, \"<=1\")")
  
  #expect_error(GetStatFormula(n = 8.4, m = 16,nmin=7)) 
  # La réponse est c("COUNTA(P7:P8)","AVERAGE(P7:P8)","STDEV(P7:P8)","COUNTIF(P7:P8, \">=10\")","COUNTIF(P7:P8, \"<=5\")","COUNTIF(P7:P8, \"<=1\")")
  # C'est à dire identique à "GetStatFormula(n = 8, m = 16,nmin=7)"
  #expect_error(GetStatFormula(n = 8, m = 16.2,nmin=7)) 
  # La réponse est c("COUNTA(P7:P8)","AVERAGE(P7:P8)","STDEV(P7:P8)","COUNTIF(P7:P8, \">=10\")","COUNTIF(P7:P8, \"<=5\")","COUNTIF(P7:P8, \"<=1\")")
  # C'est à dire identique à "GetStatFormula(n = 8, m = 16,nmin=7)"
  #expect_error(GetStatFormula(n = 8, m = 16,nmin=7.95)) 
  # La réponse est c("COUNTA(P7:P8)","AVERAGE(P7:P8)","STDEV(P7:P8)","COUNTIF(P7:P8, \">=10\")","COUNTIF(P7:P8, \"<=5\")","COUNTIF(P7:P8, \"<=1\")")
  # C'est à dire identique à "GetStatFormula(n = 8, m = 16,nmin=7)"
  
})
