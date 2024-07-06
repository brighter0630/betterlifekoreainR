library(dplyr)
library(tidyquant)
library(stringr)
library(ggplot2)
library(extrafont)
library(showtext)

# font_add_google('Nanum Pen Script', 'nanumpen')

font_add(
  family = "MaruBuri1",
  regular='./font/MaruBuri-Regular.ttf',
  bold='./font/MaruBuri-Bold.ttf',
)
font_families()
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

fromDate <- '2023-03-27'
toDate <- '2024-07-06'

symbol = "RH"
getSymbols(symbol, from = fromDate,
           to = toDate,warnings = FALSE,
           auto.assign = TRUE)

getPrice <- function(df, symbol) {
  newDf <- data.frame(index=index(df), coredata(df[, 6]))
  colnames(newDf) <- c('date', symbol)
  return(newDf)
}

result.df <- getPrice(get(symbol), symbol)

filename = paste0(symbol, "_stock_performance.csv")

if(file.exists(filename)) {
  print(paste0(filename, '파일이 존재하여 CSV를 생성하지 못했습니다.'))
} else {
  write.csv(result.df, fileEncoding = 'UTF-8', row.names = F,
            file=filename)
}
labels <- data.frame(date=c("2024-01-02"),
                     text=c("중요한 날"))

labels$date <- as.Date(labels$date)
labelsY <- left_join(labels, result.df, by='date')
result.long <- result.df %>%  melt(., id='date')

title = paste0(symbol, "의 주가 흐름")

basicChart <- result.long %>%
  ggplot(aes(x=date, y=value)) + 
  geom_line(color="#69b3a2", size=2) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_point(size=.3, color="#69b3a2") +
  xlab("날짜") + ylab('주가') + 
  labs(title = title, colour="티커") + 
  theme_light() +
  theme(legend.position = "top",
        legend.justification = c(0, 0),
        text = element_text(family = 'Nanum Pen Script'))


makeAnnotate <- function(chart, labels) {
  annAdded <- chart
  for(i in nrow(labels)) {
    annAdded <- annAdded + 
      geom_label(label = labels[i, "text"],
                 x=date(labels[i, 'date']), y = labels[i, symbol],
                 vjust=1.5,
                 hjust=.5,
                 label.padding = unit(.5, "lines"), # Rectangle size around label
                 label.size = 0.35,
                 color = alpha("black", 0.1),
                 fill=alpha("gray", 0.01)) +
      geom_point(aes(x=date(labels[i, 'date']), y=labels[i, symbol]), size=3)
  }
  return(annAdded)
}

makeAnnotate(basicChart, labelsY)
  
