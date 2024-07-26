
#Download & Load in Package(s)
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("patchwork")
install.packages("mrgsolve")
library(ggplot2)
library(gridExtra) 
library(dplyr)
library(patchwork)
library(mrgsolve)

--------------------------------------------------------------------------------
#BILLYS PROPERTY

#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
Graph2 = read.csv("Gud 2.csv", header = TRUE)   
Graph3 = read.csv("Gud 3.csv", header = TRUE)
Graph4 = read.csv("Gud 4.csv", header = TRUE)
Graph5 = read.csv("Gud 5.csv", header = TRUE)
Graph6 = read.csv("Gud 6.csv", header = TRUE)
Graph7 = read.csv("Gud 7.csv", header = TRUE)
Graph8 = read.csv("Gud 8.csv", header = TRUE)
Graph9 = read.csv("Gud 9.csv", header = TRUE)
Graph10 = read.csv("Gud 10.csv", header = TRUE)
Graph11 = read.csv("Gud 11.csv", header = TRUE)

Gud2 <- ggplot(Graph2, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + 
  scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Gud 5") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

Gud3 <- ggplot(Graph3, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + 
  scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Gud 4") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

#Use this one for final Graph (includes legend)
Gud4 <- ggplot(Graph4, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Gud 9") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.3, .18),
    legend.box.background = element_rect(color = "black", linewidth = 1), axis.text.y = element_blank()) + 
  guides(color = guide_legend(title = "Legend"))

Gud5 <- ggplot(Graph5, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Gud 8") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

Gud6 <- ggplot(Graph6, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Gud 6") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

Gud7 <- ggplot(Graph7, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Gud 7") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

Gud8 <- ggplot(Graph8, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "") + theme(legend.position = "none", axis.text.y = element_blank()) +
  ggtitle("Gud 10") +  theme(plot.title = element_text(hjust = 0.5))

Gud9 <- ggplot(Graph9, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Gud 3") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

Gud10 <- ggplot(Graph10, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(200,0)) + 
  ggtitle("Gud 1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

Gud11 <- ggplot(Graph11, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Gud 2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

grid.arrange(Gud10, Gud11, Gud9, Gud3, Gud2, Gud6, Gud7, Gud5, Gud4, Gud8, ncol = 10)

--------------------------------------------------------------------------------

#DEER CREEK SITE A
  #DEER A core 1 & 2

#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
DeerACore1 = read.csv("DeerACore1.csv", header = TRUE)   
DeerACore2 = read.csv("DeerACore2.csv", header = TRUE)
  
  #Use this one for final Graph(includes legend)
DeerACore1 <- ggplot(DeerACore1, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(120,0)) + 
  ggtitle("Deer A1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


DeerACore2 <- ggplot(DeerACore2, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(120,0)) + 
  ggtitle("Deer A2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.2), axis.text.y = element_blank(),
        legend.box.background = element_rect(color = "black", linewidth = 1)) + 
  guides(color = guide_legend(title = "Legend"))

grid.arrange(DeerACore1, DeerACore2, ncol = 2)

--------------------------------------------------------------------------------
  
#DEER CREEK SITE B
  #Deer 4 Core and Deer B core 1 & 2
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
Deer4Core = read.csv("Deer4Core.csv", header = TRUE)   
DeerBCore1 = read.csv("DeerBCore1.csv", header = TRUE)
DeerBCore2 = read.csv("DeerBCore2.csv", header = TRUE)

#Use this one for final Graph(includes legend)
Deer4Core <- ggplot(Deer4Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(250,0)) + 
  ggtitle("Deer B3") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

DeerBCore1 <- ggplot(DeerBCore1, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(250,0)) + 
  ggtitle("Deer B1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

DeerBCore2 <- ggplot(DeerBCore2, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(250,0)) + 
  ggtitle("Deer B2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.2), axis.text.y = element_blank(),
        legend.box.background = element_rect(color = "black", linewidth = 1)) + 
  guides(color = guide_legend(title = "Legend"))

grid.arrange(DeerBCore1, DeerBCore2, Deer4Core, ncol = 3)
  
--------------------------------------------------------------------------------

#DEER CREEK SITE C
  #Deer C core 1, 2, & 3
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
Deer1Core = read.csv("Deer1Core.csv", header = TRUE)   
Deer2Core = read.csv("Deer2Core.csv", header = TRUE)
Deer3Core = read.csv("Deer3Core.csv", header = TRUE)

#Use this one for final Graph(includes legend)
Deer1Core <- ggplot(Deer1Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(70,0)) + 
  ggtitle("Deer C1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

Deer2Core <- ggplot(Deer2Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(70,0)) + 
  ggtitle("Deer C2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

Deer3Core <- ggplot(Deer3Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(70,0)) + 
  ggtitle("Deer C3") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.1), axis.text.y = element_blank(),
        legend.box.background = element_rect(color = "black", linewidth = 1)) + 
  guides(color = guide_legend(title = "Legend"))

grid.arrange(Deer1Core, Deer2Core, Deer3Core, ncol = 3)

--------------------------------------------------------------------------------

#Fawn SITE A
  #Fawn A core 1 & 2
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
FawnACore1 = read.csv("FawnACore1.csv", header = TRUE)   
FawnACore2 = read.csv("FawnACore2.csv", header = TRUE)

FawnACore1 <- ggplot(FawnACore1, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(40,0)) + 
  ggtitle("Fawn A1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

FawnACore2 <- ggplot(FawnACore2, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(40,0)) + 
  ggtitle("Fawn A2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.6,.1), axis.text.y = element_blank(),
        legend.box.background = element_rect(color = "black", linewidth = 1)) + 
  guides(color = guide_legend(title = "Legend"))

grid.arrange(FawnACore1, FawnACore2, ncol = 2)

--------------------------------------------------------------------------------
  
#Fawn SITE E
  #Fawn E core 1 & 2
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
Fawn1Core = read.csv("Fawn1Core.csv", header = TRUE)   
Fawn2Core = read.csv("Fawn2Core.csv", header = TRUE)

Fawn1Core <- ggplot(Fawn1Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(200,0)) + 
  ggtitle("Fawn E1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.1),
        legend.box.background = element_rect(color = "black", linewidth = 1)) + 
  guides(color = guide_legend(title = "Legend"))

Fawn2Core <- ggplot(Fawn2Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Fawn E2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

grid.arrange(Fawn1Core, Fawn2Core, ncol = 2)

--------------------------------------------------------------------------------

#Fawn SITE F
  #Fawn Cores 4, 5, 6, & 7
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
Fawn4Core = read.csv("Fawn4Core.csv", header = TRUE)   
Fawn5Core = read.csv("Fawn5Core.csv", header = TRUE)
Fawn6Core = read.csv("Fawn6Core.csv", header = TRUE)   
Fawn7Core = read.csv("Fawn7Core.csv", header = TRUE)

#Use this one for final Graph(includes legend)
Fawn4Core <- ggplot(Fawn4Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(50,0)) + 
  ggtitle("Fawn F3") + theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.1),
                             legend.box.background = element_rect(color = "black", linewidth = 1), axis.text.y = element_blank()) + 
  guides(color = guide_legend(title = "Legend"))
  
Fawn5Core <- ggplot(Fawn5Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(50,0)) + 
  ggtitle("Fawn F2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

Fawn6Core <- ggplot(Fawn6Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(50,0)) + 
  ggtitle("Fawn F1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", )


Fawn7Core <- ggplot(Fawn7Core, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(50,0)) + 
  ggtitle("Fawn F4") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

grid.arrange(Fawn6Core, Fawn5Core, Fawn4Core, Fawn7Core, ncol = 4)
  
--------------------------------------------------------------------------------
  
#Steverson Lake
  #Steve Core 1, 2, & 3
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
SteveCore1 = read.csv("SteveCore1.csv", header = TRUE)   
SteveCore2 = read.csv("SteveCore2.csv", header = TRUE)
SteveCore3 = read.csv("SteveCore3.csv", header = TRUE)

#Use this one for final Graph(includes legend)
SteveCore1 <- ggplot(SteveCore1, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(200,0)) + 
  ggtitle("Steve 1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

SteveCore2 <- ggplot(SteveCore2, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Steve 2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.text.y = element_blank())

SteveCore3 <- ggplot(SteveCore3, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(200,0)) + 
  ggtitle("Steve 3") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.1),
        legend.box.background = element_rect(color = "black", linewidth = 1), axis.text.y = element_blank()) + 
  guides(color = guide_legend(title = "Legend"))

grid.arrange(SteveCore1, SteveCore2, SteveCore3, ncol = 3)
  
--------------------------------------------------------------------------------
  
#Spike Box Site B
  #Spike Box Site B core 1 & 2
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
SpikeBCore1 = read.csv("SpikeBCore1.csv", header = TRUE)   
SpikeBCore2 = read.csv("SpikeBCore2.csv", header = TRUE)

SpikeBCore1 <- ggplot(SpikeBCore1, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(120,0)) + 
  ggtitle("Spike B1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

SpikeBCore2 <- ggplot(SpikeBCore2, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(120,0)) + 
  ggtitle("Spike B2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.1),
        legend.box.background = element_rect(color = "black", linewidth = 1), axis.text.y = element_blank()) + 
  guides(color = guide_legend(title = "Legend"))

grid.arrange(SpikeBCore1, SpikeBCore2, ncol = 2)

--------------------------------------------------------------------------------  
  
#Spike Box Site G
  #Spike Box Site G core 1 & 2
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
SpikeGCore1 = read.csv("SpikeGCore1.csv", header = TRUE)   
SpikeGCore2 = read.csv("SpikeGCore2.csv", header = TRUE)

SpikeGCore1 <- ggplot(SpikeGCore1, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(100,0)) + 
  ggtitle("Spike G1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

SpikeGCore2 <- ggplot(SpikeGCore2, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(100,0)) + 
  ggtitle("Spike G2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.1),
        legend.box.background = element_rect(color = "black", linewidth = 1), axis.text.y = element_blank()) + 
  guides(color = guide_legend(title = "Legend"))

grid.arrange(SpikeGCore1, SpikeGCore2, ncol = 2)

#--------------------------------------------------------------------------------

#Mcginlley Site B
  #Mcginlley Site B core 1 & 2
  
#Load in CSV file from Core Data CSV Folder on Desktop
#Loads CSV file into name "Graph"
McginCore1 = read.csv("McginCore1.csv", header = TRUE)   
McginCore2 = read.csv("McginCore2.csv", header = TRUE)

McginCore1 <- ggplot(McginCore1, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "Depth (cm)", limits=c(205,0)) + 
  ggtitle("Mcginlley Core 1") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

McginCore2 <- ggplot(McginCore2, aes(x = Depth, y = BD)) + 
  geom_line(aes(colour = "Bulk Density"), size = 1) + 
  geom_line(aes(y = C/25, colour = "C (%)"), size = 1) + 
  geom_point(aes(Depth, C/25)) + 
  geom_point(aes(Depth, BD)) +
  scale_y_continuous(limits=c(0,2), sec.axis = sec_axis(~.*25, name="C (%)")) + 
  scale_color_manual(values = c("orange2", "gray30")) +
  coord_flip() + scale_x_reverse(name = "", limits=c(205,0)) + 
  ggtitle("Mcginlley Core 2") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(.5,.1),
        legend.box.background = element_rect(color = "black", linewidth = 1), axis.text.y = element_blank()) + 
  guides(color = guide_legend(title = "Legend"))

grid.arrange(McginCore1, McginCore2, ncol = 2)

#--------------------------------------------------------------------------------





  
  
  