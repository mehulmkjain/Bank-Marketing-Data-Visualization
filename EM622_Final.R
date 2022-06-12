# importing necessary libraries
library(plyr)
library(dplyr)
library(tidyr)
library(tidyselect)
library(reshape2)
library(tidyverse)
library(alluvial)
library (vcd)

library(ggplot2)
library(googleVis)
library(treemap)
library(plotly)

# reading the data, understanding the data and performing data cleaning 
bank_data = read.csv("bank-additional-full.csv", header = TRUE)

# selecting the important variables to work with 
bank_data_imp = subset(bank_data, select = c("age", "job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "duration", "campaign", "poutcome", "y"))

# There are several missing values in some categorical attributes, all are coded with the "unknown" label
# These missing values are treated as a possible class label

sum(is.na(bank_data_imp)) # We can see that there are no missing values other than "unknownn" label in the data set 

# renaming the columns and manipulating the data
bank_data_imp = bank_data_imp %>% rename(Contacts_during_the_Campaign = campaign, Previous_Outcome = poutcome, Desired_Target = y)
bank_data_imp$Desired_Target[bank_data_imp$Desired_Target == "yes"] = "Subscribed"
bank_data_imp$Desired_Target[bank_data_imp$Desired_Target == "no"] = "Not Subscribed"

# correcting the month column
bank_data_imp$month[bank_data_imp$month == "may"] = "May"
bank_data_imp$month[bank_data_imp$month == "apr"] = "April"
bank_data_imp$month[bank_data_imp$month == "aug"] = "August"
bank_data_imp$month[bank_data_imp$month == "dec"] = "December"
bank_data_imp$month[bank_data_imp$month == "jul"] = "July"
bank_data_imp$month[bank_data_imp$month == "jun"] = "June"
bank_data_imp$month[bank_data_imp$month == "nov"] = "November"
bank_data_imp$month[bank_data_imp$month == "oct"] = "October"
bank_data_imp$month[bank_data_imp$month == "sep"] = "September"
bank_data_imp$month[bank_data_imp$month == "nov"] = "November"
bank_data_imp$month[bank_data_imp$month == "mar"] = "March"

# correcting day column
bank_data_imp$day_of_week[bank_data_imp$day_of_week == "mon"] = "Monday"
bank_data_imp$day_of_week[bank_data_imp$day_of_week == "tue"] = "Tuesday"
bank_data_imp$day_of_week[bank_data_imp$day_of_week == "wed"] = "Wednesday"
bank_data_imp$day_of_week[bank_data_imp$day_of_week == "thu"] = "Thursday"
bank_data_imp$day_of_week[bank_data_imp$day_of_week == "fri"] = "Friday"

str(bank_data_imp)
head(bank_data_imp)

# Also, we need to change to data type of a couple of categorical variables from "chr" to "factor"

bank_data_imp$job = as.factor(bank_data_imp$job)
bank_data_imp$marital = as.factor(bank_data_imp$marital)
bank_data_imp$education = as.factor(bank_data_imp$education)
bank_data_imp$contact = as.factor(bank_data_imp$contact)
# bank_data_imp$month = as.factor(bank_data_imp$month)
# bank_data_imp$day_of_week = as.factor(bank_data_imp$day_of_week)
str(bank_data_imp)



# Finding the relationship between Desired Target and different categorical variables 
# Desired Target and Job Title
plot1 = ggplot(data = bank_data_imp, aes(x = job, fill = Desired_Target)) + 
  geom_bar(stat = 'count', position = 'dodge') + 
  xlab("Job Title")+
  ylab("Count") + ggtitle("Job Title by Desired Target") + theme_bw() + theme(strip.text.x = element_text(face = "bold"),
                                                                              strip.background = element_rect(
                                                                                colour = "black", size = 1, fill = "green")) + theme(legend.key = element_blank(),
                                                                                                                                     legend.background = element_blank(),
                                                                                                                                     legend.title = element_text(face ="bold"))  + theme(panel.grid.major = element_line(size = 0.5),
                                                                                                                                                                                         panel.grid.minor =
                                                                                                                                                                                           element_line(colour = "green", 
                                                                                                                                                                                                        linetype = "dotted")) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_text(face="bold")) + guides(fill=guide_legend(title="Desired Target"))
plot1

# Desired Target and Education
plot2 = ggplot(data = bank_data_imp, aes(x = education, fill = Desired_Target)) + 
  geom_bar(stat = 'count', position = 'dodge') + 
  xlab("Education")+
  ylab("Count") + ggtitle("Education Background by Desired Target") + theme_bw() + theme(strip.text.x = element_text(face = "bold"),
                                                                              strip.background = element_rect(
                                                                                colour = "black", size = 1, fill = "green")) + theme(legend.key = element_blank(),
                                                                                                                                     legend.background = element_blank(),
                                                                                                                                     legend.title = element_text(face ="bold"))  + theme(panel.grid.major = element_line(size = 0.5),
                                                                                                                                                                                         panel.grid.minor =
                                                                                                                                                                                           element_line(colour = "green", 
                                                                                                                                                                                                        linetype = "dotted")) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_text(face="bold")) + guides(fill=guide_legend(title="Desired Target"))
plot2

# Desired Target and Marital Status
plot3 = ggplot(data = bank_data_imp, aes(x = marital, fill = Desired_Target)) + 
  geom_bar(stat = 'count', position = 'dodge') + 
  xlab("Marital Status")+
  ylab("Count") + ggtitle("Marital Status by Desired Target") + theme_bw() + theme(strip.text.x = element_text(face = "bold"),
                                                                              strip.background = element_rect(
                                                                                colour = "black", size = 1, fill = "green")) + theme(legend.key = element_blank(),
                                                                                                                                     legend.background = element_blank(),
                                                                                                                                     legend.title = element_text(face ="bold"))  + theme(panel.grid.major = element_line(size = 0.5),
                                                                                                                                                                                         panel.grid.minor =
                                                                                                                                                                                           element_line(colour = "green", 
                                                                                                                                                                                                        linetype = "dotted")) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_text(face="bold")) + guides(fill=guide_legend(title="Desired Target"))
plot3

# We can see that most of the subscribers are married people with "admin" as job title and people 
#with university degree, so let us analyze more on that 
subset1 = filter(bank_data_imp, job == "admin." & marital == "married" & education == "university.degree" & Desired_Target == "Subscribed")  

# Scatter Plot
plot4 = ggplot(data = subset1) + geom_jitter(aes(x = age, y = duration, colour = contact)) + facet_wrap(~Previous_Outcome)  + 
  xlab("Age")+
  ylab("Duration of the call (s)") + ggtitle("Analysis of the highest targeted audience who subscribed to the service based on the previous campaign outcome") + theme_bw() + theme(strip.text.x = element_text(face = "bold"),
                                                                              strip.background = element_rect(
                                                                                colour = "black", size = 1, fill = "green")) + theme(legend.key = element_blank(),
                                                                                                                                     legend.background = element_blank(),
                                                                                                                                     legend.title = element_text(face ="bold"))  + theme(panel.grid.major = element_line(size = 0.5),
                                                                                                                                                                                         panel.grid.minor =
                                                                                                                                                                                           element_line(colour = "green", 
                                                                                                                                                                                                        linetype = "dotted")) + theme(plot.title = element_text(face="bold")) + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_text(face="bold")) + scale_colour_discrete(name= "Contact Type")
plot4

# Tree map of Bank Campaign Subscriptions by month and Day of the Week
subset2 = subset(bank_data_imp, select = c("month", "day_of_week", "Desired_Target"))

x = table(subset2$month, subset2$day_of_week, subset2$Desired_Target)
y = as.data.frame(x)
names(y) = c("Month", "Day of the Week", "Desired_Target", "Count")
y = filter(y, Desired_Target == "Subscribed")
plot5 = treemap(y,
        index= c("Month", "Day of the Week"),
        vSize="Count", vColor = "Count",
        type="value",
        title="Bank Campaign Subscriptions by Month and Days of the Week",
        fontsize.title = 14
)

# Interactive Line Chart of month and loan
data_p6 = subset(bank_data_imp,select=c(month, loan, Desired_Target))
data_p6 = filter(data_p6, Desired_Target == "Subscribed")
noloan = data_p6[which(data_p6$loan %in% "no"),]
freq_noloan = as.data.frame(table(noloan$month))
yesloan = data_p6[which(data_p6$loan %in% "yes"),]
freq_yesloan = as.data.frame(table(yesloan$month))
freq_month = cbind(freq_noloan,freq_yesloan$Freq)
names(freq_month) = c("Month", "Loan - No", "Loan - Yes")
plot6 = gvisLineChart(freq_month, options = list(legend = "top right", title="Line Chart of Subscribed People by Month and Personal Loan", 
                                                 hAxes="[{title:'Month'}]",
                                                 vAxes="[{title:'Count'}]",
                                                 width = 900, height = 900))

plot(plot6)

# Final Results of the campaign
subset3 = subset(bank_data_imp, select = "Desired_Target")
a = table(subset3$Desired_Target)
b = as.data.frame(a)
names(b) = c("Desired_Target", "Count")

colors <- c('rgb(211,94,96)', 'rgb(114,147,203)')

plot7 = plot_ly(b, labels = ~Desired_Target, values = ~Count, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('Number of People: ', Count),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
plot7 = plot7 %>% layout(title = '<b>Results of the Campaign<b>',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

plot7


# # plot 3 - Scatter plot of job, age, marital and duration
# data_p3<-subset(bank_data,select=c(age,job,marital,duration))
# p3 <- ggplot(data =data_p3 )+geom_point(aes(x = age, y = duration,colour =job))+ facet_wrap(~marital)+labs(title ="Scatter plot of job, age, marital and duration")
# p3


