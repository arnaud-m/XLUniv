library(RGIFT)

DEBUG <- TRUE
n <- ifelse(DEBUG, 3, 100)

GiftConvertSAC <- function(category, n, minNum, maxNum, precision) {
  GIFTD(paste0('$CATEGORY: conversion/', category))
  GiftConvertSA(CreateConvertQuestions(n, minNum, maxNum, precision))
}

TestCodeQuestion <- function(pathname) {
  ## Read question code
  lines <- readLines(pathname)
  ## Add loop over params
  lines <- c(
    head(lines, 1),
    'for(n in params) {',
    tail(lines, -1),
    'cat(\'\\n\')',
    '}'
  )
  code <- paste(lines, collapse = '\n')
  cat(code)
  eval(parse(text = code))
}

sink('gift-questions.txt')
GiftConvertSAC('integer/easy', n, minNum = 0, maxNum = 256, precision = 0)
GiftConvertSAC('integer/medium', n, minNum = 256, maxNum = 2048, precision = 0)
GiftConvertSAC('integer/hard', n, minNum = 2048, maxNum = 2**14, precision = 0)

GiftConvertSAC('float/easy', n, minNum = 0, maxNum = 256, precision = 2)
GiftConvertSAC('float/medium', n, minNum = 256, maxNum = 2048, precision = 4)
GiftConvertSAC('float/hard', n, minNum = 2048, maxNum = 2**14, precision = 6)



for( category in c('easy', 'medium')) {
  GIFTD(paste0('$CATEGORY: code/', category))
  pathnames <- list.files(file.path('data-raw', category), full.names = TRUE)
  for(pathname in pathnames) {
    GiftCodeSA(pathname)
  }
}
sink()
