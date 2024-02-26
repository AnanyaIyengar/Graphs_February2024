####################################
### Graphs for CCT Policy Report ###
####################################

# First Updated: 19 January 2024
# Latest Updated: 23 January 2024
# Updated by: Ananya Iyengar 

#Setting Working Directory 

setwd("C:/Users/LENOVO/Desktop/Ananya/CCT Rewrite/graphing")

################################################################################

#Loading Packages 

library(readxl) #to import data
library(dplyr) #for data manipulation
library(ggplot2) #for visualisations
library(viridis) #colour palette 
library(tidyr) #for reshaping data

################################################################################

### Sex Ratio for Census Years ###

#Importing Data 

sexratio <- read_excel("data/sexratio.xlsx")

#Graphing

srplot <- sexratio %>% 
  ggplot(aes(x = Year)) + #setting x axis variable
  geom_line(aes(y = `Child Sex Ratio`, colour = "`Child Sex Ratio`"), linewidth = 1.15, alpha = 0.6) + #setting csr as y axis variable
  geom_line(aes(y = `Total Sex Ratio`, colour = "`Total Sex Ratio`"), linewidth = 1.15, alpha = 0.6) + #setting tsr as y axis variable
  geom_point(aes(y = `Child Sex Ratio`), colour = "orange4", shape = 16, size = 2.5) + #adding circles to csr line
  geom_point(aes(y = `Total Sex Ratio`), colour = "turquoise4", shape = 17, size = 2.5) + #adding triangles to tsr line
  theme_minimal() +  #keeping minimal gridlines in background
  theme(axis.line = element_line(colour = 'black', size = 0.6)) + #changing thickness of axis lines
  theme(axis.ticks = element_line(colour = "black", size = 0.6)) + #changing thickness of axis ticks
  theme(axis.text = element_text(colour = "black", size = 8)) + #changing size of axis text
  xlab("Year") + ylab("Sex Ratio") + #setting labels for x and y axis 
  labs(colour = "Legend") + theme(legend.title.align=0.5) + #setting legend name and alignment 
  scale_color_manual(labels = c("Child Sex Ratio", "Total Sex Ratio"), values = c("orange3", "turquoise3")) + #setting legend contents 
  theme( legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) + #making a box around the legend
  scale_x_continuous(breaks = c(1961, 1971, 1981, 1991, 2001, 2011)) + #setting x axis ticks to correspond to census years
  scale_y_continuous(breaks = seq(910, 1020, 10)) +  #setting y axis ticks 
  ggtitle("Evolution of Sex Ratios in India: 1961-2011") + #adding title to graph
  theme(plot.title = element_text(size = 12.7)) + #setting text size of title 
  theme(plot.title.position = "panel") + #setting position of title 
  theme(axis.line = element_line(arrow = arrow(type="closed", length = unit(0.17, "cm")))) + #adding arrows to axis ends
  theme(plot.caption = element_text(hjust = 0)) + #setting position of caption 
  annotate("text", x = 1961, y = 944, label = "941", size = 2.8, colour = "turquoise4") + 
  annotate("text", x = 2011, y = 945.7, label = "943", size = 2.8, colour = "turquoise4") +
  annotate("text", x = 1961, y = 978.5, label = "976", size = 2.8, colour = "orange4") +
  annotate("text", x = 2011, y = 921.7, label = "919", size = 2.8, colour = "orange4")
  
srplot #generate visualisation

###############################################################################################################

### Overall Literacy Rates by Gender for Census Years ###

#Importing Data 

literacy <- read_excel("data/literacy.xlsx")

#Reshaping Data 

literacy2 <- literacy %>% 
  pivot_longer(cols = c("Male", "Female"), 
               names_to = "Gender", 
               values_to = "Literacy")

literacy2 <- as.data.frame(literacy2)

#Graphing

litplot <- 
  ### BAR PLOT ###
  ggplot(data = literacy2, aes(x = as.factor(Year), y = Literacy, fill = Gender)) +  #defining aesthetics
  geom_bar(position = "dodge", stat = "identity", alpha = 0.75, width = 0.6, colour = "grey34") + #creating base bar plot
  theme_minimal() +  #setting theme of plot
  scale_fill_manual(values = c("orange3", "turquoise3")) + #setting colours for groups
  theme(axis.text = element_text(colour = "black", size = 8)) + #optimising axis text
  xlab("Year") + ylab("Literacy Rate (%)") + #labelling the x and y axes 
  labs(colour = "Gender") + theme(legend.title.align=0.5) + #creating and aligning the legend title 
  theme( legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) + #creating a box around the legend 
  theme(plot.title = element_text(size = 12.7)) + #setting text size of title 
  theme(plot.title.position = "panel") + #making a box around the legend
  ggtitle("Literacy Rates by Gender: 1991-2011") + #plot title 
  theme(plot.title = element_text(size = 12)) + #adjusting size of plot title
  theme(axis.title = element_text(size = 10)) + #adjusting size of axis title 
  labs(caption = "Source: Census of India") + #adding caption 
  theme(plot.caption = element_text(hjust = 0)) + #moving caption to left most part of the plot 
  ### LINE PLOT ###
  geom_segment(x = 1.3, y = 39.4, xend = 1.7, yend = 53.67, alpha = 0.75, colour = "black") + 
  geom_segment(x = 2.3, y = 53.67, xend = 2.7, yend = 65.46, alpha = 0.75, colour = "black") +
  geom_segment(x = 1.3, y = 63.9, xend = 1.7, yend = 75.26, alpha = 0.75, colour = "black") +
  geom_segment(x = 2.3, y = 75.26, xend = 2.7, yend = 82.14, alpha = 0.75, colour = "black") + 
  geom_segment(x = 0.7, y = 39.4, xend = 0.7, yend = 63.9, linetype = 2) + 
  geom_segment(x = 0.7, y = 63.9, xend = 1, yend = 63.9, linetype = 2) +
  geom_segment(x = 1.7, y = 53.67, xend = 1.7, yend = 75.26, linetype = 2) +
  geom_segment(x = 1.7, y = 75.26, xend = 2, yend = 75.26, linetype = 2) + 
  geom_segment(x = 2.7, y = 65.46, xend = 2.7, yend = 82.14, linetype = 2) +
  geom_segment(x = 2.7, y = 82.14, xend = 3, yend = 82.14, linetype = 2) +
  ### ADDING GAP NUMBERS 
  annotate("text", x = 0.86, y = 50, label = "24.50", size = 2.8) + 
  annotate("text", x = 1.86, y = 65, label = "21.59", size = 2.8) +
  annotate("text", x = 2.86, y = 74, label = "16.68", size = 2.8)
  
litplot #generate visualisation 

########################################################################################################

### Literacy by Gender and Group for Census Years ###


casteliteracy <- read_excel("data/casteliteracy.xlsx")

#Graphing 

casteplot <- casteliteracy %>% 
  ggplot(aes(x = as.factor(Year), y = Literacy)) + #setting aesthetics 
  geom_segment(x = 1, y = 0, xend = 1, yend = 77, alpha = 0.05, linetype = 1) + #vertical line at 2001
  geom_segment(x =2, y = 0, xend = 2, yend = 77, alpha = 0.05, linetype = 1) + #vertical line at 2011
  geom_point(aes(colour = Gender, shape = Group), size = 3.5, alpha = 0.8) + #adding points 
  theme_classic() + #setting theme 
  scale_color_manual(values = c("orange4", "turquoise4")) + #setting colours according to gender 
  theme(axis.line = element_line(colour = 'black', size = 0.5)) + #changing thickness of axis lines
  theme(axis.ticks = element_line(colour = "black", size = 0.6)) + #changing thickness of axis ticks
  theme(axis.text = element_text(colour = "black", size = 8)) + #changing size of axis text
  theme(legend.title.align=0.5) + #aligning the title of the graph
  xlab("Year") + ylab("Literacy Rate (%)") +  #adding x and y axis names 
  theme( legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) +
  scale_shape(guide = "none") + #removing the legend 
  theme(plot.title = element_text(size = 12.7)) + #setting text size of title 
  theme(plot.title.position = "panel") + #setting position of title 
  theme(axis.line = element_line(arrow = arrow(type="closed", length = unit(0.17, "cm")))) + #adding arrows to axis ends
  theme(plot.caption = element_text(hjust = 0)) + #setting position of caption 
  ggtitle("Literacy Rate by Gender & Caste") + #setting title 
  labs(caption = "Source: Census of India") + #adding caption
  #LINE SEGMENTS#
  geom_segment(x = 1, y = 34.8, xend = 2, yend = 49.4, alpha = 0.8, colour = "turquoise4", linetype = 3, linewidth = 0.7) +
  geom_segment(x = 1, y = 42.0, xend = 2, yend = 56.5, alpha = 0.8, colour = "turquoise4", linetype = 3, linewidth = 0.7) +
  geom_segment(x = 1, y = 59.2, xend = 2, yend = 68.5, alpha = 0.8, colour = "orange4", linetype = 3, linewidth = 0.7) +
  geom_segment(x = 1, y = 66.6, xend = 2, yend = 75.2, alpha = 0.8, colour = "orange4", linetype = 3, linewidth = 0.7) +
  theme(legend.position = "none") + #removing all legends 
  #LABELS#
  annotate("text", x = 0.85, y = 35.25, label = "ST Women", size = 2.8) + 
  annotate("text", x = 0.85, y = 42.2, label = "SC Women", size = 2.8) + 
  annotate("text", x = 0.875, y = 59.5, label = "ST Men", size = 2.8) + 
  annotate("text", x = 0.875, y = 66.8, label = "SC Men", size = 2.8)
 
  
casteplot






