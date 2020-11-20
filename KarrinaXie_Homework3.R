rm(list=ls())
install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")
install.packages("dplyr")
library(coronavirus)
library(dplyr)
library(ggplot2)




##Question 1
#Question 1. a
#imported the coronavirus package above
covid <- tbl_df(coronavirus) #changing into local dataframe 

#Question 1. b
head(coronavirus, 100)

#Question 1. c
#There are 7 columns in total 
#date - The date of the summary
#province - The province or state where the case occurred
#country - The country wherethe case occurred 
#lat - Latitude 
#long - Longitude 
#type - if the patient is confirmed, dead, recovered
#cases - the number of daily cases corresponding to the case type


##Question 2 
#Question 2.a      
#showing the Top 20 countries with the  most number of total confirmed cases 
t20 <-covid %>%
        select(country, type, cases) %>%
        group_by(country,type) %>%
        filter(type == "confirmed") %>%
        summarize(total_confirmed = sum(cases)) %>%
        arrange(desc(total_confirmed)) %>%
        head(n=20)

#Question 2.b
#creating a df of country and their corresponding total cases 
dat <- covid %>%
        select(country, type, cases) %>%
        group_by(country) %>%
        summarize(total_cases = sum(cases)) %>%
        arrange(desc(total_cases)) %>%
        head(n=5)

#plotting dat into bar graph 
bp <- ggplot(data=dat, aes(x=country, y=total_cases)) +
        geom_bar(stat="identity")

#Question 2.c 
#flipping the graph  
bp + coord_flip()

#Question 2.d 
bp + coord_flip() + ggtitle("Top 5 countries by total cases")


##Question 3 
#Question 3.a
recent_cases<- coronavirus %>%
                select(date, country,type, cases) %>%
                group_by(date,type) %>%
                filter(type == "confirmed") %>%
                summarize(confirmed = sum(cases)) %>%
                arrange(date) %>%
                head(n=20)

#Question 3.b
line <- ggplot(data=recent_cases, aes(x=date, y=confirmed)) +
        geom_line(stat="identity")
        


#Extra Credit 
#changing the width of bar to 0.5 
ggplot(data=dat, aes(x=country, y=total_cases)) +
        geom_bar(stat="identity", width = 0.5)

#changing the outline of bars 
ggplot(data=dat, aes(x=country, y=total_cases)) +
        geom_bar(stat="identity", color="black", width = 0.5)

#adding labels per bar, fitting the font, and positioning it above the bar
ggplot(data=dat, aes(x=country, y=total_cases)) +
        geom_bar(stat="identity", color="black", width = 0.5) +
        geom_text(aes(label=total_cases),vjust=-0.4, size=3.5)

#adding the fill colors 
ggplot(data=dat, aes(x=country, y=total_cases, fill = country)) +
        geom_bar(stat="identity", color="black", width = 0.5) + theme_minimal()+
        geom_text(aes(label=total_cases),vjust=-0.4, size=3.5)

#changing legend position
ggplot(data=dat, aes(x=country, y=total_cases, fill = country)) +
        geom_bar(stat="identity", color="black", width = 0.5) + theme_minimal()+
        geom_text(aes(label=total_cases),vjust=-0.4, size=3.5)+
        theme(legend.position="bottom")


#changing the fill color to custom color 
ggplot(data=dat, aes(x=country, y=total_cases, fill = country)) +
        geom_bar(stat="identity", color="black", width = 0.5) + theme_minimal()+
        geom_text(aes(label=total_cases),vjust=-0.4, size=3.5) +
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#96B3E5", "#F60F63"))+
        theme(legend.position="bottom")

#changing the font 
ggplot(data=dat, aes(x=country, y=total_cases, fill = country)) +
        geom_bar(stat="identity", color="black", width = 0.5) + theme_minimal()+
        geom_text(aes(label=total_cases),vjust=-0.4, size=3.5) +
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#96B3E5", "#F60F63"))+
        theme(legend.position="bottom") +
        theme(text=element_text(size=13, family="Comic Sans MS"))
        #lol comic sans 

#line plot 
#adding points 
ggplot(data=recent_cases, aes(x=date, y=confirmed)) +
        geom_line(stat="identity") +
        geom_point()

#changing line type
ggplot(data=recent_cases, aes(x=date, y=confirmed)) +
        geom_line(stat="identity", linetype = "dashed") +
        geom_point()
        
#changing of color of line 
ggplot(data=recent_cases, aes(x=date, y=confirmed)) +
        geom_line(stat="identity", linetype = "dashed", color = "blue") +
        geom_point()



