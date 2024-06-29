install.packages('tidyquant')
library(dplyr)
library(tidyquant)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

getSymbols("GMED", from = '2019-06-21',
           to = "2024-06-21",warnings = FALSE,
           auto.assign = TRUE)

getSymbols("MDT", from = '2019-06-21',
           to = "2024-06-21",warnings = FALSE,
           auto.assign = TRUE)

getSymbols("^IXIC", from = '2019-06-21',
           to = "2024-06-21",warnings = FALSE,
           auto.assign = TRUE)

getChangePct <- function(df) {
  pct.df <- data.frame(index=index(df), coredata(df[, 6])) %>%
    mutate(changeRate=((.[, 2]/first(.[, 2]) - 1)))
  
  pct.df$changeRate <- round(pct.df$changeRate, 2)
  colnames(pct.df) <- c('date', 'adjusted', deparse(substitute(df)))
  return (pct.df[, c(1, 3)])
}

GMED.df <- getChangePct(GMED)
MDT.df <- getChangePct(MDT)
IXIC.df <- getChangePct(IXIC)

result.df <- left_join(GMED.df, MDT.df, by='date') %>%
  left_join(., IXIC.df, by='date')

write.csv(result.df, fileEncoding = 'UTF-8', row.names = F,
          file='GMEDStockPerformance.csv')

