library(tmap) 
library(rgdal)
sheffieldShape<-readOGR(dsn="./BoundaryData", layer="england_lsoa_2011") #load LSOA file
qtm(sheffieldShape, fill=NULL)
tmap_mode("view")
tmap_mode("plot")
library(tidyverse) #for read_tsv() function
deprivation2019<-read_csv("File_7.csv") #load deprivation file
View(deprivation2019)
colnames(deprivation2019)
deprivation2019data<-deprivation2019 %>% #select the variables used 
  select("LSOA code (2011)", 
         "LSOA name (2011)", 
         "Income Score (rate)", 
         "Employment Score (rate)", 
         "Education, Skills and Training Score", 
         "Health Deprivation and Disability Score",
         "Crime Score",
         "Barriers to Housing and Services Score",
         "Living Environment Score",
         "Total population: mid 2015 (excluding prisoners)",
         "Working age population 18-59/64: for use with Employment Deprivation Domain (excluding prisoners)")
View(deprivation2019data)
sheffieldShape@data<-
  left_join(sheffieldShape@data, 
            deprivation2019data, 
            by=c('code'='LSOA code (2011)')) # combine shape file with deprivation file
qtm(sheffieldShape, fill="Income Score (rate)")
#plot maps1
tm_shape(sheffieldShape) +
  tm_fill(c("Income Score (rate)",
            "Employment Score (rate)",
            "Education, Skills and Training Score",
            "Crime Score"), 
          style="kmeans", border.col = "black", alpha=0.8) + 
  tm_borders(alpha=0)
#plot maps2
tm_shape(sheffieldShape) +
  tm_fill(c("Barriers to Housing and Services Score",
            "Living Environment Score",
            "Total population: mid 2015 (excluding prisoners)",
            "Working age population 18-59/64: for use with Employment Deprivation Domain (excluding prisoners)"), 
          style="kmeans", border.col = "black", alpha=0.8) + 
  tm_borders(alpha=0)
#########################################deal with street file
View(street19_01_data)
street19_01 <- read.csv("2019-01-south-yorkshire-street.csv") #01
street19_01_data <- street19_01[c(2,8,9,10)]
View(street19_01_data)
street19_02 <- read.csv("2019-02-south-yorkshire-street.csv") #02
street19_02_data <- street19_02[c(2,8,9,10)]
View(street19_02_data)
street19_03 <- read.csv("2019-03-south-yorkshire-street.csv") #03
street19_03_data <- street19_03[c(2,8,9,10)]
View(street19_03_data)
street19_04 <- read.csv("2019-04-south-yorkshire-street.csv") #04
street19_04_data <- street19_04[c(2,8,9,10)]
View(street19_04_data)
street19_05 <- read.csv("2019-05-south-yorkshire-street.csv") #05
street19_05_data <- street19_05[c(2,8,9,10)]
View(street19_05_data)
street19_06 <- read.csv("2019-06-south-yorkshire-street.csv") #06
street19_06_data <- street19_06[c(2,8,9,10)]
View(street19_06_data)
street19_07 <- read.csv("2019-07-south-yorkshire-street.csv") #07
street19_07_data <- street19_07[c(2,8,9,10)]
View(street19_07_data)
street19_08 <- read.csv("2019-08-south-yorkshire-street.csv") #08
street19_08_data <- street19_08[c(2,8,9,10)]
View(street19_08_data)
street19_09 <- read.csv("2019-09-south-yorkshire-street.csv") #09
street19_09_data <- street19_09[c(2,8,9,10)]
View(street19_09_data)
street19_10 <- read.csv("2019-10-south-yorkshire-street.csv") #10
street19_10_data <- street19_10[c(2,8,9,10)]
View(street19_10_data)
street19_11 <- read.csv("2019-11-south-yorkshire-street.csv") #11
street19_11_data <- street19_11[c(2,8,9,10)]
View(street19_11_data)
street19_12 <- read.csv("2019-12-south-yorkshire-street.csv") #12
street19_12_data <- street19_12[c(2,8,9,10)]
View(street19_12_data)
total_street_19_data <- rbind(street19_01_data, 
                              street19_02_data,
                              street19_03_data,
                              street19_04_data,
                              street19_05_data,
                              street19_06_data,
                              street19_07_data,
                              street19_08_data,
                              street19_09_data,
                              street19_10_data,
                              street19_11_data,
                              street19_12_data) # combine data from all months
View(total_street_19_data)
#remove the rows with LSOA value missing
total_street_19_data_year <- total_street_19_data[-1]
table <- table(total_street_19_data_year$LSOA.code,total_street_19_data_year$Crime.type)
View(table)
matrix <- as.data.frame.matrix(table)
dataframe <- as.data.frame(matrix)
View(dataframe)
dataframe <- mutate(dataframe, LSOA.code=rownames(dataframe))
dataframe <- dataframe %>% dplyr::select(LSOA.code, everything())
dataframe_subset <- subset(dataframe, !duplicated(subset(dataframe, select=c(LSOA.code))))
View(dataframe_sorted)
dataframe_sorted <- left_join(dataframe_subset,deprivation2019data,by = c("LSOA.code" = "LSOA code (2011)"))
dataframe_sorted <- dataframe_sorted %>% dplyr::select(LSOA.name, everything())
dataframe_sorted <- dataframe_sorted[-1,]
colnames(dataframe_sorted)
dataframe_sorted_total <- dataframe_sorted %>% 
  mutate(Total=dataframe_sorted[2]+
           dataframe_sorted[3]+
           dataframe_sorted[4]+
           dataframe_sorted[5]+
           dataframe_sorted[6]+
           dataframe_sorted[7]+
           dataframe_sorted[8]+
           dataframe_sorted[9]+
           dataframe_sorted[10]+
           dataframe_sorted[11]+
           dataframe_sorted[12]+
           dataframe_sorted[13]+
           dataframe_sorted[14]+
           dataframe_sorted[15])
View(dataframe_sorted_total)
dataframe_sorted_total[dataframe_sorted_total==0] <- NA
dataframe_sorted_total <- na.omit(dataframe_sorted_total)
as.data.frame()

####################Plotting
colnames(dataframe_sorted_total)
names(dataframe_sorted_total)[2] <- "Anti_social_behaviour"
names(dataframe_sorted_total)[3] <- "Bicycle"
names(dataframe_sorted_total)[5] <- "Criminal_damage_and_arson"
names(dataframe_sorted_total)[7] <- "Other_crime"
names(dataframe_sorted_total)[8] <- "Other_theft"
names(dataframe_sorted_total)[9] <- "Possession_of_weapons"
names(dataframe_sorted_total)[10] <- "Public_order"
names(dataframe_sorted_total)[13] <- "Theft_from_the_person"
names(dataframe_sorted_total)[14] <- "Vehicle_crime"
names(dataframe_sorted_total)[15] <- "Violence_and_sexual_offence"
names(dataframe_sorted_total)[16] <- "LSOA_name"
names(dataframe_sorted_total)[17] <- "Income_Score_rate"
names(dataframe_sorted_total)[18] <- "Employment_Score_rate"
names(dataframe_sorted_total)[19] <- "Education_Skills_and_Training_Score"
names(dataframe_sorted_total)[20] <- "Health_Deprivation_and_Disability_Score"
names(dataframe_sorted_total)[21] <- "Crime_Score"
names(dataframe_sorted_total)[22] <- "Barriers_to_Housing_and_Services_Score"
names(dataframe_sorted_total)[23] <- "Living_Environment_Score"
names(dataframe_sorted_total)[24] <- "Total_population"
names(dataframe_sorted_total)[25] <- "Working_age_population"
ggplot(data = dataframe_sorted_total, aes(x = Anti_social_behaviour, y = Crime_Score)) + 
  geom_point()+mytheme
# set mytheme
library(gridExtra)
library(extrafont)
mytheme <-theme_bw()+
  theme(text = element_text(family = "Avenir"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 1),
        panel.grid.major = element_line(size=0.18,linetype="1212",colour = "grey75"),
        panel.border = element_blank(),
        panel.grid.minor = element_line(size=0.1,linetype="1212",colour = "grey85"),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1,1,1,1),"cm"),
        axis.line = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.title.x = element_text(vjust = -1.5,face="bold"),
        axis.title.y.left = element_text(vjust = 1.5,face="bold"),
        legend.position = "top",
        legend.text = element_text(size=9),
        plot.caption=element_text(hjust=0.5,vjust=-5,size=9,colour="grey70"),
        plot.subtitle=element_text(hjust=0.5,vjust=4.5,face="italic",size=9,colour="grey40"),
        plot.title=element_text(size=16,face="bold",hjust=0.5,vjust=4))

#########################correlation
#antisocial-income
cor.test(dataframe_sorted_total$Total_population,dataframe_sorted_total$Anti_social_behaviour)
Anti_social_Model<-lm(formula=Anti_social_behaviour~Income_Score_rate+
                        Employment_Score_rate+
                        Education_Skills_and_Training_Score+
                        Health_Deprivation_and_Disability_Score+
                        Barriers_to_Housing_and_Services_Score+
                        Living_Environment_Score+
                        Total_population+
                        Working_age_population, 
                      data=dataframe_sorted_total)
plot(Anti_social_Model, which=1)
Anti_social_Predictions<-data.frame(anti_social=dataframe_sorted_total$Anti_social_behaviour, pred=Anti_social_Model$fitted.values)
ggplot(data = Anti_social_Predictions, aes(x = anti_social, y = pred)) + 
  geom_point() + 
  geom_abline(mapping=aes(slope=1,intercept=0), color='red')+mytheme+
  labs(title="Antisocial behaviour: predictions and original data")
cor.test(dataframe_sorted_total$Anti_social_behaviour,dataframe_sorted_total$Living_Environment_Score)
antisocial_living_model <- lm(formula=Anti_social_behaviour~Living_Environment_Score,
                              data=dataframe_sorted_total)
plot(antisocial_living_model)

#bicycle-indicators
bicycle_Model<-lm(formula=Bicycle~Income_Score_rate+
                        Employment_Score_rate+
                        Education_Skills_and_Training_Score+
                        Health_Deprivation_and_Disability_Score+
                        Barriers_to_Housing_and_Services_Score+
                        Living_Environment_Score+
                        Total_population+
                        Working_age_population, 
                      data=dataframe_sorted_total)
plot(bicycle_Model, which=1)
summary(bicycle_Model)

bicycle_Predictions<-data.frame(bicycle=dataframe_sorted_total$Bicycle, pred=bicycle_Model$fitted.values)
ggplot(data = bicycle_Predictions, aes(x = bicycle, y = pred)) + 
  geom_point() + 
  geom_abline(mapping=aes(slope=1,intercept=0), color='red')+mytheme+
  labs(title="Bicycle thefts: predictions and original data")
cor.test(dataframe_sorted_total$Bicycle,dataframe_sorted_total$Barriers_to_Housing_and_Services_Score)
antisocial_living_model <- lm(formula=Anti_social_behaviour~Barriers_to_Housing_and_Services_Score,
                              data=dataframe_sorted_total)
plot(antisocial_living_model)
#burglary-indicators
burglary_Model<-lm(formula=Burglary~Income_Score_rate+
                    Employment_Score_rate+
                    Education_Skills_and_Training_Score+
                    Health_Deprivation_and_Disability_Score+
                    Barriers_to_Housing_and_Services_Score+
                    Living_Environment_Score+
                    Total_population+
                    Working_age_population, 
                  data=dataframe_sorted_total)
plot(burglary_Model, which=1)
summary(burglary_Model)
bicycle_Predictions<-data.frame(bicycle=dataframe_sorted_total$Bicycle, pred=bicycle_Model$fitted.values)
ggplot(data = bicycle_Predictions, aes(x = bicycle, y = pred)) + 
  geom_point() + 
  geom_abline(mapping=aes(slope=1,intercept=0), color='red')+mytheme+
  labs(title="Bicycle thefts: predictions and original data")
cor.test(dataframe_sorted_total$Burglary,dataframe_sorted_total$Living_Environment_Score)
#criminal-indicators
colnames(dataframe_sorted_total)
criminal_Model<-lm(formula=Criminal_damage_and_arson~Income_Score_rate+
                     Employment_Score_rate+
                     Education_Skills_and_Training_Score+
                     Health_Deprivation_and_Disability_Score+
                     Barriers_to_Housing_and_Services_Score+
                     Living_Environment_Score+
                     Total_population+
                     Working_age_population, 
                   data=dataframe_sorted_total)
plot(criminal_Model, which=1)
summary(criminal_Model)
bicycle_Predictions<-data.frame(bicycle=dataframe_sorted_total$Bicycle, pred=bicycle_Model$fitted.values)
ggplot(data = bicycle_Predictions, aes(x = bicycle, y = pred)) + 
  geom_point() + 
  geom_abline(mapping=aes(slope=1,intercept=0), color='red')+mytheme+
  labs(title="Bicycle thefts: predictions and original data")
cor.test(dataframe_sorted_total$Burglary,dataframe_sorted_total$Living_Environment_Score)
#drugs-indicators
colnames(dataframe_sorted_total)
drugs_Model<-lm(formula=Drugs~Income_Score_rate+
                     Employment_Score_rate+
                     Education_Skills_and_Training_Score+
                     Health_Deprivation_and_Disability_Score+
                     Barriers_to_Housing_and_Services_Score+
                     Living_Environment_Score+
                     Total_population+
                     Working_age_population, 
                   data=dataframe_sorted_total)
plot(drugs_Model, which=1)
summary(drugs_Model)
bicycle_Predictions<-data.frame(bicycle=dataframe_sorted_total$Bicycle, pred=bicycle_Model$fitted.values)
ggplot(data = bicycle_Predictions, aes(x = bicycle, y = pred)) + 
  geom_point() + 
  geom_abline(mapping=aes(slope=1,intercept=0), color='red')+mytheme+
  labs(title="Bicycle thefts: predictions and original data")
cor.test(dataframe_sorted_total$Burglary,dataframe_sorted_total$Living_Environment_Score)
#Possession_of_weapons-indicators
colnames(dataframe_sorted_total)
Possession_of_weapons_Model<-lm(formula=Possession_of_weapons~Income_Score_rate+
                  Employment_Score_rate+
                  Education_Skills_and_Training_Score+
                  Health_Deprivation_and_Disability_Score+
                  Barriers_to_Housing_and_Services_Score+
                  Living_Environment_Score+
                  Total_population+
                  Working_age_population, 
                data=dataframe_sorted_total)
plot(Possession_of_weapons_Model, which=1)
summary(Possession_of_weapons_Model)
Possession_of_weapons_Predictions<-data.frame(bicycle=dataframe_sorted_total$Bicycle, pred=bicycle_Model$fitted.values)
ggplot(data = bicycle_Predictions, aes(x = bicycle, y = pred)) + 
  geom_point() + 
  geom_abline(mapping=aes(slope=1,intercept=0), color='red')+mytheme+
  labs(title="Bicycle thefts: predictions and original data")
cor.test(dataframe_sorted_total$Possession_of_weapons,dataframe_sorted_total$Living_Environment_Score)
#public_order-indicators
colnames(dataframe_sorted_total)
public_order_Model<-lm(formula=Public_order~Income_Score_rate+
                                  Employment_Score_rate+
                                  Education_Skills_and_Training_Score+
                                  Health_Deprivation_and_Disability_Score+
                                  Barriers_to_Housing_and_Services_Score+
                                  Living_Environment_Score+
                                  Total_population+
                                  Working_age_population, 
                                data=dataframe_sorted_total)
plot(public_order_Model, which=1)
summary(public_order_Model)
Possession_of_weapons_Predictions<-data.frame(bicycle=dataframe_sorted_total$Bicycle, pred=bicycle_Model$fitted.values)
ggplot(data = bicycle_Predictions, aes(x = bicycle, y = pred)) + 
  geom_point() + 
  geom_abline(mapping=aes(slope=1,intercept=0), color='red')+mytheme+
  labs(title="Bicycle thefts: predictions and original data")
cor.test(dataframe_sorted_total$Possession_of_weapons,dataframe_sorted_total$Living_Environment_Score)
#Robbery-indicators
colnames(dataframe_sorted_total)
Robbery_Model<-lm(formula=Robbery~Income_Score_rate+
                         Employment_Score_rate+
                         Education_Skills_and_Training_Score+
                         Health_Deprivation_and_Disability_Score+
                         Barriers_to_Housing_and_Services_Score+
                         Living_Environment_Score+
                         Total_population+
                         Working_age_population, 
                       data=dataframe_sorted_total)
plot(Robbery_Model, which=1)
summary(Robbery_Model)
Possession_of_weapons_Predictions<-data.frame(bicycle=dataframe_sorted_total$Bicycle, pred=bicycle_Model$fitted.values)
ggplot(data = bicycle_Predictions, aes(x = bicycle, y = pred)) + 
  geom_point() + 
  geom_abline(mapping=aes(slope=1,intercept=0), color='red')+mytheme+
  labs(title="Bicycle thefts: predictions and original data")
cor.test(dataframe_sorted_total$Robbery,dataframe_sorted_total$Living_Environment_Score)






