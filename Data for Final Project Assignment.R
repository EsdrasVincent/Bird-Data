bird<-read.csv("Bird_and_route_data.csv",header=TRUE)
View(bird)
bird$CPR <-bird$Count/bird$Routes
View(bird)

#### CODE TO LOOK AT CPR DATA OF All 4 SPECIES ACROSS TIME, FOR ALL STATES

spp <-c("Osprey","Peregrine Falcon","Cooper's Hawk","Bald Eagle")
spp_dat <-subset(bird,Species==spp[1]|Species==spp[2]|Species==spp[3]|Species==spp[4])

library(ggplot2)
p <- ggplot(spp_dat, aes(x=Year, y=CPR,color=State))+ 
  #geom_point()+
  geom_line()+ 
  ggtitle("CPR over time")+
  facet_wrap(~Species,scales="free_y")  # this makes a multi panel plot based on the species
print(p)

#CORRELATION TEST BETWEEN CPR DATA FOR ALL 4 SPECIES AND TIME

cordat$CPR <- cordat$Count/cordat$Routes
View(cordat)
CTcor <-cor(cordat$CPR,cordat$Year)
CTcor
summary(CTcor)

# CPR CORRELATIONS FOR OSPREY ACROSS NORTH CAROLINA AND MARYLAND

library(dplyr)
cordat <- subset(bird,State=="NC"|State=="MD")  # filter by states
cordat <-subset(cordat,Species=="Osprey") # filter by species
cprdat <- cordat[,c(2,3,7)] # only keep state, Year, and CPR columns
# convert to wide data
cpr_wide <- reshape(cprdat, idvar = "Year", timevar = "State", direction = "wide")

#make pairwise plots of the columns with CPR data
pairs(cpr_wide[,-1]) # -1 means exclude the first column

# Calculate the correlation
spp_cor <- cor(cpr_wide[,-1], use="complete.obs", method="spearman")
spp_cor