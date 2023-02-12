library(tidyverse)
library(rvest)
# install.packages("dplyr")
library(dplyr)
library(ggplot2)


# Read Bundesliga Home table as a function dependent on year

tmtab1 <- function(year) {
  link <- paste0("https://www.transfermarkt.de/bundesliga/heimtabelle/wettbewerb/L1/saison_id/", year)
  # read table
  tab <- 
    read_html(link) %>% 
    html_node(".box .box") %>% 
    html_table()
  # clean table
  names(tab)[4] <- "Spiele"
  tab %>% 
    select(-2) %>% 
    rename(Punkte = "Pkt.")

}
tmtab1(2018)
tmtab1(2019)
tmtab1(2020)
tmtab1(2021)

mean(tmtab1(2018)$Punkte)
mean(tmtab1(2019)$Punkte)
mean(tmtab1(2020)$Punkte)
mean(tmtab1(2021)$Punkte)

P <- c(mean(tmtab1(2018)$Punkte),mean(tmtab1(2019)$Punkte),mean(tmtab1(2020)$Punkte),mean(tmtab1(2021)$Punkte))
Y <- c(2018, 2019, 2020, 2021)

png(file = "barchart_points_bundesliga.png")

barplot(P,names.arg=Y,xlab="Years",ylab="Points",col="blue",
        main="Bundesliga Home Points chart",border="red")

#plot(years, points , main = "Time based plot")

# Read Premier League Home table as a function dependent on year


tmtab2 <- function(year) {
  link <- paste0("https://www.transfermarkt.de/premier-league/heimtabelle/wettbewerb/GB1/saison_id/", year)
  # read table
  tab <- 
    read_html(link) %>% 
    html_node(".box .box") %>% 
    html_table()
  # clean table
  names(tab)[4] <- "Spiele"
  tab %>% 
    select(-2) %>% 
    rename(Punkte = "Pkt.")
}
tmtab2(2018)
tmtab2(2019)
tmtab2(2020)
tmtab2(2021)

mean(tmtab2(2018)$Punkte)
mean(tmtab2(2019)$Punkte)
mean(tmtab2(2020)$Punkte)
mean(tmtab2(2021)$Punkte)

P <- c(mean(tmtab2(2018)$Punkte),mean(tmtab2(2019)$Punkte),mean(tmtab2(2020)$Punkte),mean(tmtab2(2021)$Punkte))
Y <- c(2018, 2019, 2020, 2021)

#png(file = "barchart_points_premierleague.png")

barplot(P,names.arg=Y,xlab="Years",ylab="Points",col="blue",
        main="Premier League Home Points chart",border="red")


# Read LaLiga Home table as a function dependent on year


tmtab3 <- function(year) {
  link <- paste0("https://www.transfermarkt.de/laliga/heimtabelle/wettbewerb/ES1/saison_id/", year)
  # read table
  tab <- 
    read_html(link) %>% 
    html_node(".box .box") %>% 
    html_table()
  # clean table
  names(tab)[4] <- "Spiele"
  tab %>% 
    select(-2) %>% 
    rename(Punkte = "Pkt.")
}
tmtab3(2018)
tmtab3(2019)
tmtab3(2020)
tmtab3(2021)

mean(tmtab3(2018)$Punkte)
mean(tmtab3(2019)$Punkte)
mean(tmtab3(2020)$Punkte)
mean(tmtab3(2021)$Punkte)

P <- c(mean(tmtab3(2018)$Punkte),mean(tmtab3(2019)$Punkte),mean(tmtab3(2020)$Punkte),mean(tmtab3(2021)$Punkte))
Y <- c(2018, 2019, 2020, 2021)

png(file = "barchart_points_laliga.png")

barplot(P,names.arg=Y,xlab="Years",ylab="Points",col="blue",
        main="Laliga Home Points chart",border="red")

# Read Serie A Home table as a function dependent on year


tmtab4 <- function(year) {
  link <- paste0("https://www.transfermarkt.de/serie-a/heimtabelle/wettbewerb/IT1/saison_id/", year)
  # read table
  tab <- 
    read_html(link) %>% 
    html_node(".box .box") %>% 
    html_table()
  # clean table
  names(tab)[4] <- "Spiele"
  tab %>% 
    select(-2) %>% 
    rename(Punkte = "Pkt.")
}
tmtab4(2018)
tmtab4(2019)
tmtab4(2020)
tmtab4(2021)

mean(tmtab4(2018)$Punkte)
mean(tmtab4(2019)$Punkte)
mean(tmtab4(2020)$Punkte)
mean(tmtab4(2021)$Punkte)

P <- c(mean(tmtab4(2018)$Punkte),mean(tmtab4(2019)$Punkte),mean(tmtab4(2020)$Punkte),mean(tmtab4(2021)$Punkte))
Y <- c(2018, 2019, 2020, 2021)

#png(file = "barchart_points_seriea.png")

barplot(P,names.arg=Y,xlab="Years",ylab="Points",col="blue",
        main="Seriea Home Points chart",border="red")


# Read Ligue 1 Home table as a function dependent on year


tmtab5 <- function(year) {
  link <- paste0("https://www.transfermarkt.de/ligue-1/heimtabelle/wettbewerb/FR1/saison_id/", year)
  # read table
  tab <- 
    read_html(link) %>% 
    html_node(".box .box") %>% 
    html_table()
  # clean table
  names(tab)[4] <- "Spiele"
  tab %>% 
    select(-2) %>% 
    rename(Punkte = "Pkt.")
}
tmtab5(2018)
tmtab5(2019)
tmtab5(2020)
tmtab5(2021)

mean(tmtab5(2018)$Punkte)
mean(tmtab5(2019)$Punkte)
mean(tmtab5(2020)$Punkte)
mean(tmtab5(2021)$Punkte)

P <- c(mean(tmtab5(2018)$Punkte),mean(tmtab5(2019)$Punkte),mean(tmtab5(2020)$Punkte),mean(tmtab5(2021)$Punkte))
Y <- c(2018, 2019, 2020, 2021)

#png(file = "barchart_points_seriea.png")

barplot(P,names.arg=Y,xlab="Years",ylab="Points",col="blue",
        main="League 1 Home Points chart",border="red")



#link <- "https://www.transfermarkt.de/bundesliga/heimtabelle/wettbewerb/L1/saison_id/2020"
# read table
#tab <- 
# read_html(link) %>% 
#html_node(".box .box") %>% 
#html_table()
# clean table
#names(tab)

#names(tab)[4] <- "Spiele"
#tab %>% 
# select(-2) %>% 
#rename(Punkte = "Pkt.")





