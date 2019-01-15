context("Testing Number Encoding")


test_that("Integer Encoding succeeds", {
  expect_equal('0', EncodeNumber(0, base = 10))
  expect_equal('0', EncodeNumber(0.0, base = 10))
  expect_equal('0', EncodeNumber(0, base = 2))

  expect_equal('a', EncodeNumber(0, base = 2, digits = letters))
  expect_equal('ab', EncodeNumber(1, base = 2, digits = letters))
  expect_equal('ba', EncodeNumber(2, base = 2, digits = letters))
  expect_equal('bb', EncodeNumber(3, base = 2, digits = letters))


  expect_equal('10', EncodeNumber(16, base = 36))
  expect_equal('Z', EncodeNumber(45, base = 36))

  for(base in 2:36) {
    for(n in 0:1024) {
      expect_equal(n , strtoi( EncodeNumber(n, base = base), base))
    }
  }

})


test_that("Float Encoding succeeds", {
  expect_equal('1.25', EncodeNumber(1.25, base = 10))
  expect_equal('1,25', EncodeNumber(1.25, base = 10, dec = ','))

  expect_equal('a.ab', EncodeNumber(0.25, base = 2, digits = letters))
  expect_equal('a.b', EncodeNumber(0.5, base = 2, digits = letters))
  expect_equal('a.bb', EncodeNumber(0.75, base = 2, digits = letters))
})


test_that("Float Encoding warns", {
  expect_warning( EncodeNumber(n = 1/3, base = 10))
  expect_warning( EncodeNumber(n = 1/10, base = 2))

  expect_warning( expect_equal('0.12', EncodeNumber(n = 0.125, base = 10, precision = 2)))
  expect_warning( expect_equal('0', EncodeNumber(n = 0.125, base = 2, precision = 1)))
})


test_that("Number Encoding fails", {
  expect_error( EncodeNumber(n = 0, base = -1))
  expect_error( EncodeNumber(n = 0, base = 1))
  expect_error( EncodeNumber(n = 0, base = 100))
  expect_error( EncodeNumber(n = -1, base = 100))
})
