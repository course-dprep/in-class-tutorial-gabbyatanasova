#Install boxoffice
install.packages(c("data.table","readxl"))
library(data.table)
library(readxl)
getwd()
setwd("C:/Users/Gabriela Atanasova/Downloads")
boxofficemojo_com <- read_excel("boxofficemojo.com.xlsx")
setDT(boxofficemojo_com)
View(boxofficemojo_com)
summary(boxofficemojo_com)

#Install imdb
install.packages("readr")
library(readr)
imdb_com <- read_csv("imdb.com.csv")
setDT(imdb_com)
View(imdb_com)
summary(imdb_com)

#Converting character to numeric
imdb_com[, budget_num := as.numeric(imdb.com_budget)]

#Merging the datasets
movies <- merge(boxofficemojo_com,imdb_com, by.x = c("boxofficemojo.com_imdb.com_id"), by.y = c("imdb.com_id"), all.x = TRUE)


#Saving data
load("Data.RData")
write_csv(movies, "movies.csv")
install.packages("writexl")
library(writexl)
write_xlsx(movies, "movies.xlsx")

#Boxplot
boxplot(movies$boxofficemojo.com_openinggross)

#Descriptives and barplot for MPAA rating
table(movies$boxofficemojo.com_MPAArating)
barplot(table(movies$boxofficemojo.com_MPAArating))
barplot(table(movies$boxofficemojo.com_MPAArating)/sum(table(movies$boxofficemojo.com_MPAArating))*100)

#ggplot package
install.packages("ggplot2")
library(ggplot2)
ggplot(movies, aes(boxofficemojo.com_MPAArating)) + geom_bar()
ggplot(movies, aes(boxofficemojo.com_MPAArating)) + geom_bar(aes(y = after_stat(count)/sum(after_stat(count))*100)) + ylab("percentage")

#Dummy R-rated movies
movies[,boxofficemojo.com_MPAArating_R := ifelse(boxofficemojo.com_MPAArating == 'R', 1, 0)]
movies[is.na(boxofficemojo.com_MPAArating_R), boxofficemojo.com_MPAArating_R := 0]

#Boxplots
ggplot(movies, aes(x=as.factor(boxofficemojo.com_MPAArating_R), y=boxofficemojo.com_openinggross)) + geom_boxplot()
ggplot(movies, aes(x=as.factor(imdb.com_basedonbook), y=boxofficemojo.com_openinggross)) + geom_boxplot()
ggplot(movies[!is.na(imdb.com_basedonbook),], aes(x=as.factor(imdb.com_basedonbook), y=boxofficemojo.com_openinggross)) + geom_boxplot()

#Scatter plot
ggplot(movies, aes(x=budget_num, y=boxofficemojo.com_openinggross)) + geom_point()

#Aggregating, ordering
temp <- movies[, .(boxofficemojo.com_openinggross_mean = mean(boxofficemojo.com_openinggross)), by=c("imdb.com_year")]
temp <- movies[, .(boxofficemojo.com_openinggross_mean = mean(boxofficemojo.com_openinggross, na.rm=TRUE)), by=c("imdb.com_year")]
temp <- movies[!is.na(imdb.com_year), .(boxofficemojo.com_openinggross_mean = mean(boxofficemojo.com_openinggross, na.rm=TRUE)), by=c("imdb.com_year")]
setorderv(temp, c("imdb.com_year"))

#Lineplot
ggplot(temp, aes(x=imdb.com_year, y=boxofficemojo.com_openinggross_mean)) + geom_line()
rm(temp)

#Hypothesis testing: Levene test and independent sample t test
movies[!is.na(imdb.com_basedonbook), .(boxofficemojo.com_openinggross_mean = mean(boxofficemojo.com_openinggross, na.rm=TRUE)), by=c("imdb.com_basedonbook")]
install.packages("car")
library(car)
leveneTest(boxofficemojo.com_openinggross ~ as.factor(imdb.com_basedonbook), movies, center=mean)
t.test(boxofficemojo.com_openinggross ~ imdb.com_basedonbook, movies, var.equal=TRUE)
