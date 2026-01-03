library(devtools)
devtools::load_all()
source('PrintLatexQCM.R')

exdata <- rbind(
    CreateConvertQuestions(2, min = 64, max = 128, precision = 0, each = 2),
    CreateConvertQuestions(2, min = 512, max = 1024, precision = 0, each = 2),
    CreateConvertQuestions(2, min = 2048, max = 16384, precision = 0, each = 2),
    CreateConvertQuestions(2, min = 128, max = 512, each = 2),
    CreateConvertQuestions(2, min = 1024, max = 16384, each = 2)
    )

PrintLaTexQCM(exdata)
