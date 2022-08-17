# Fentanyl policy project

#install.packages("markovchain")
#install.packages("diagram")
library(markovchain)
library(diagram)
library(ggplot2)
library(tidyverse)
library(kableExtra)

# Alexandra Savinkina
# 6/7/22

#Inputs:

N_Pop_Incidental_high <- 0
N_Pop_Regular_high <- 100
N_Pop_Incidental_med <- 50
N_Pop_Regular_med <- 200
N_Pop_Incidental_low <- 200
N_Pop_Regular_low <- 450

N_Pop_Incidental <- N_Pop_Incidental_high + N_Pop_Incidental_med + N_Pop_Incidental_low
N_Pop_Regular <- N_Pop_Regular_high + N_Pop_Regular_med + N_Pop_Regular_low

N_Pop <- N_Pop_Incidental_high + N_Pop_Regular_high + N_Pop_Incidental_med + N_Pop_Regular_med +
         N_Pop_Incidental_low + N_Pop_Regular_low

N_Pop_At_Risk_Incidental <- N_Pop_Incidental_high
N_Pop_At_Risk_Regular <- N_Pop_Regular_high

N_Pop_At_Risk <- N_Pop_At_Risk_Incidental  + N_Pop_At_Risk_Regular 
  
#Prop_At_Risk_Incidental <- N_Pop_Incidental_high/ N_Pop_At_Risk
#Prop_At_Risk_Regular <- N_Pop_Regular_high/ N_Pop_At_Risk

Prop_AtRisk_Incid <- N_Pop_At_Risk_Incidental/N_Pop
Prop_AtRisk_Reg <- N_Pop_At_Risk_Regular/N_Pop

Prop_Incid <- N_Pop_Incidental/N_Pop
Prop_Reg <- N_Pop_Regular/N_Pop

Death_Rate_Incid <- 0.0005
Death_Rate_Reg <- 0.005

Death_Rate_Jail <- 0.00001

Arrest_Rate_Incid <- 0.1
Arrest_Rate_Reg <- 0.1

Charge_Rate_Incid <- 0.2
Charge_Rate_Reg <- 0.5

Jail_Rate_Incid <- Arrest_Rate_Incid*Charge_Rate_Incid
Jail_Rate_Reg <- Arrest_Rate_Reg*Charge_Rate_Reg

Arrest_Mult_Arrest <- 2
Arrest_Mult_Death <- 2

Cost_Jail <- 39000
Cost_Death <- 1500000

# Model parameters

# Rate of first felony:

Felony_1 <-Jail_Rate_Incid*Prop_AtRisk_Incid+Jail_Rate_Reg*Prop_AtRisk_Reg
Felony_2 <-Jail_Rate_Incid*Prop_AtRisk_Incid*Arrest_Mult_Arrest+Jail_Rate_Reg*Prop_AtRisk_Reg*Arrest_Mult_Arrest
Death_1 <- Death_Rate_Incid*Prop_Incid+Death_Rate_Reg*Prop_Reg
Death_2 <- Death_Rate_Incid*Arrest_Mult_Death*Prop_Incid+Death_Rate_Reg*Arrest_Mult_Death*Prop_Reg

# Model

trans_mat <- matrix(c(1-Felony_1-Death_1,Felony_1,0,Death_1,# Not Jailed
                    0,0,(1-Death_Rate_Jail),Death_Rate_Jail, # Charged, first time
                    0, Felony_2, 1-Felony_2-Death_2, Death_2,# Not Jailed, after first release
                    0,0,0,1
                    ),nrow = 4, byrow = TRUE)
trans_mat
  
disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=c("Not arrested","Arrested","Released","Dead"), name="MC 1") 
disc_trans
plot(disc_trans)

Current_state<-c(1,0,0,0)
steps<-10

Data <- data.frame(matrix(0,nrow=steps,ncol=5))
colnames(Data) <- c("year","Not arrested","Arrested","Released","Dead")
Data$year <- c(1:steps)

for (i in 1:steps) {

finalState<-Current_state*disc_trans^i #using power operator
Data[i,2:5] <- finalState

}

Data_People <- Data*N_Pop
Data_People$year <- Data_People$year/1000

Data_People$ChargeCost <- Data_People$Arrested*Cost_Jail
Data_People$DeathCost <- Data_People$Dead*Cost_Death

Data_People2 <- Data_People[,c(1,3:5)]
Data_Long <- gather(Data_People2, key="observation", value="value",-year)
Data_Long2 <- gather(Data_People, key="observation", value="value",-year)
  
Data_Cost_Long <- Data_Long2[which((Data_Long2$observation=="ChargeCost"|Data_Long2$observation=="DeathCost") & Data_Long2$year==10),]

# Graphs

plot <- ggplot(data=Data_Long) + geom_line(aes(x=Data_Long$year, y=Data_Long$value, color=Data_Long$observation)) +
  theme_classic() + xlab("Year") + ylab("Count") + theme(legend.title = element_blank())


plot_cost <- ggplot(data=Data_Cost_Long, aes(y=Data_Cost_Long$value, x=Data_Cost_Long$observation)) + geom_bar(stat="identity")

Data_People_Table <- kable(Data_People)


