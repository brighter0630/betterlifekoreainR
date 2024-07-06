library(dplyr)
library(tidyquant)
library(stringr)
library(ggplot2)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

fromDate <- '2024-03-27'
toDate <- '2024-06-28'

symbol = "AVAV"
getSymbols(symbol, from = fromDate,
           to = toDate,warnings = FALSE,
           auto.assign = TRUE)

getPrice <- function(df, symbol) {
  newDf <- data.frame(index=index(df), coredata(df[, 6]))
  colnames(newDf) <- c('date', symbol)
  return(newDf)
}

getInterval <- function(diff) {
  if(diff > 300*10) {
    return (14)
  } else if (diff > 300*3) {
    return (7)
  } else if (diff > 300) {
    return (3)
  } else if (diff > 100) {
    return (2)
  } else {
    return (1)
  }
}

diff <- as.Date(toDate) - as.Date(fromDate)
interval = getInterval(diff)
result.df <- getPrice(get(symbol), symbol)

result.filtered.df <- result.df[seq(nrow(result.df)%%interval, nrow(result.df), interval), ]
result.filtered.df$date <- as.Date(result.filtered.df$date)

filename = paste0(symbol, "_stock_performance.csv")

if(file.exists(filename)) {
  print(paste0(filename, '파일이 존재하여 CSV를 생성하지 못했습니다.'))
} else {
  write.csv(result.filtered.df, fileEncoding = 'UTF-8', row.names = F,
            file=filename)
}

result.long <- melt(result.filtered.df, id='date', measure = symbol)

result.long %>%
  ggplot(aes(x=date, y=value, colour=variable)) + geom_line(size=1.2)

         