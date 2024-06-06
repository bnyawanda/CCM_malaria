Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL", "English")
options(scipen=999) #removing scientific notations  

rm(list=ls())
gc()

# install.packages("npreg")
# install.packages("OpenStreetMap",type="binary")
pacman::p_load(dplyr,tidyverse,tidyr,ggplot2,rEDM,nonlinearTseries,scatterplot3d,npreg,haven,Hmisc,sqldf,tmap,tmaptools,sf,sp,st,tidyverse,png,grid,readxl) ###stats and npreg, for seasonality smoothing###


# path=("C:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/Data")
path=("D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024")
setwd(path)
########-------------------------------------- Forecasting 


dat4 = read_dta("dat4.dta") 

dat5=dat4 %>%
  dplyr::select(time, everything())%>% #rearrange taking time to be the first 
  arrange(time)
# dplyr::select(year,month,incidencePyrs_u5,Rain_2)

dat6=dat4 %>%
  dplyr::select(year,month,incidencePyrs_u5) 

openxlsx::write.xlsx(dat6, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/incd_u5.xlsx")

###----Six years prediction
lib_x=c(1,107)
lib_y=c(max(lib_x)+1,180)

simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

par(mfrow=c(4,1))
x_to_y_2022_1 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
openxlsx::write.xlsx(x_to_y_2022_1, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/pred_u5.xlsx")

###----three years prediction
lib_x=c(1,143)
lib_y=c(max(lib_x)+1,180)

simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

x_to_y_2022_2 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
openxlsx::write.xlsx(x_to_y_2022_2, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/pred_u5.xlsx")

###----one year prediction
lib_x=c(1,167)
lib_y=c(max(lib_x)+1,180)

simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

x_to_y_2022_3 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
openxlsx::write.xlsx(x_to_y_2022_3, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/pred_u5.xlsx")

###----six months prediction
lib_x=c(1,173)
lib_y=c(max(lib_x)+1,180)

simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

x_to_y_2022_4 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
openxlsx::write.xlsx(x_to_y_2022_4, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/pred_u5.xlsx")

###2022
par(mfrow=c(3,1))
x_to_y_2022_1_err = ComputeError(x_to_y_2022_1$Observations, x_to_y_2022_1$Predictions);x_to_y_2022_1_err
x_to_y_2022_1=x_to_y_2022_1 %>%
  filter(time<181)
plot(x_to_y_2022_1$time,x_to_y_2022_1$Observations,type="b",col="blue",ylim=c(0.0,1.2),lwd = 2,main="A. Six years prediction",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2022_1$time,x_to_y_2022_1$Predictions,col="red",lwd = 2) 
text(119, 1.15, paste("rho=",round(x_to_y_2022_1_err$rho,2),"    MAE=",round(x_to_y_2022_1_err$MAE,2),"    RMSE=",round(x_to_y_2022_1_err$RMSE,2)))          

x_to_y_2022_2_err = ComputeError(x_to_y_2022_2$Observations, x_to_y_2022_2$Predictions);x_to_y_2022_2_err
x_to_y_2022_2=x_to_y_2022_2 %>%
  filter(time<181)
plot(x_to_y_2022_2$time,x_to_y_2022_2$Observations,type="b",col="blue",ylim=c(0.0,1.2),lwd = 2,main="B. Three years prediction",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2022_2$time,x_to_y_2022_2$Predictions,col="red",lwd = 2) 
text(153, 1.15, paste("rho=",round(x_to_y_2022_2_err$rho,2),"    MAE=",round(x_to_y_2022_2_err$MAE,2),"    RMSE=",round(x_to_y_2022_2_err$RMSE,2)))          


x_to_y_2022_3_err = ComputeError(x_to_y_2022_3$Observations, x_to_y_2022_3$Predictions);x_to_y_2022_3_err
x_to_y_2022_3=x_to_y_2022_3 %>%
  filter(time<181)
plot(x_to_y_2022_3$time,x_to_y_2022_3$Observations,type="b",col="blue",ylim=c(0.0,1.2),lwd = 2,main="C. One year prediction",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2022_3$time,x_to_y_2022_3$Predictions,col="red",lwd = 2) 
text(171, 1.15, paste("rho=",round(x_to_y_2022_3_err$rho,2),"    MAE=",round(x_to_y_2022_3_err$MAE,2),"    RMSE=",round(x_to_y_2022_3_err$RMSE,2)))          

###From ggplot

###-----------------------6 years
incd=read_excel("incidence_u5 - 2022.xlsx",sheet=1)

incd$t2<-incd$month/12
incd$time<-incd$year+incd$t2
plot_1=ggplot() + theme_classic() +  xlab("Year") + ylab("Malaria incidence") + 
  geom_rect(aes(xmin=2008,xmax=2023, ymin=0, ymax=1.4), fill = "black", alpha = 0.05) + 
  geom_line(data = incd, aes(x = time, y = obs, group = 1, color="Observed"), size = 0.9)  +
  geom_line(data = incd, aes(x = time, y = pred, group = 1, color="Predicted"),linetype=1, linewidth = 0.9) + 
  geom_line(data=data.frame(x=2017,y=0:1.2),aes(x=x,y=y),linetype="dotted")+
  geom_segment(aes(x=2017.0,y=1.2,xend=2022.95,yend=1.2),arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(aes(x=2022.3,1.15,label="Forecast"),size=4,fontface="bold")+
  geom_text(aes(x=2008.9,1.36,label="A. 6 years"),size=4,fontface="bold")+
  scale_color_manual(values = c(Predicted = "red2", Observed = "dodgerblue4"))+
  scale_x_continuous(breaks = seq(from = 2008, to = 2023, by = 1), expand = c(0.02,0.02)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1.2, by = 0.2), expand = c(0.05,0.02), limits=c(0,1.4))+
  theme(strip.background = element_blank(), legend.justification = c(0,1), 
        legend.title= element_blank(),
        legend.text = element_text(size = 9), 
        legend.background = element_rect(fill=NA),
        axis.text=element_text(size=12, color = "black"), 
        axis.title = element_text(size=13),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.line.x=element_line(),axis.line.y=element_line())
plot_1

###-----------------------3 years
incd=read_excel("incidence_u5 - 2022.xlsx",sheet=2)

incd$t2<-incd$month/12
incd$time<-incd$year+incd$t2
plot_2=ggplot() + theme_classic() +  xlab("Year") + ylab("Malaria incidence") + 
  geom_rect(aes(xmin=2008,xmax=2023, ymin=0, ymax=1.4), fill = "black", alpha = 0.05) + 
  geom_line(data = incd, aes(x = time, y = obs, group = 1, color="Observed"), size = 0.9)  +
  geom_line(data = incd, aes(x = time, y = pred, group = 1, color="Predicted"),linetype=1, linewidth = 0.9) + 
  geom_line(data=data.frame(x=2020,y=0:1.4),aes(x=x,y=y),linetype="dotted")+
  geom_segment(aes(x=2020.0,y=1.2,xend=2022.95,yend=1.2),arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(aes(x=2022.3,1.15,label="Forecast"),size=4,fontface="bold")+
  geom_text(aes(x=2008.9,1.36,label="B. 3 years"),size=4,fontface="bold")+
  scale_color_manual(values = c(Predicted = "red2", Observed = "dodgerblue4"))+
  scale_x_continuous(breaks = seq(from = 2008, to = 2023, by = 1), expand = c(0.02,0.02)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1.2, by = 0.2), expand = c(0.05,0.02), limits=c(0,1.4))+
  theme(strip.background = element_blank(), legend.justification = c(0,1), 
        legend.title= element_blank(),
        legend.text = element_text(size = 9), 
        legend.background = element_rect(fill=NA),
        axis.text=element_text(size=12, color = "black"), 
        axis.title = element_text(size=13),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.line.x=element_line(),axis.line.y=element_line())
plot_2

###-----------------------1 year
incd=read_excel("incidence_u5 - 2022.xlsx",sheet=3)

incd$t2<-incd$month/12
incd$time<-incd$year+incd$t2
plot_3=ggplot() + theme_classic() +  xlab("Year") + ylab("Malaria incidence") + 
  geom_rect(aes(xmin=2008,xmax=2023, ymin=0, ymax=1.4), fill = "black", alpha = 0.05) + 
  geom_line(data = incd, aes(x = time, y = obs, group = 1, color="Observed"), size = 0.9)  +
  geom_line(data = incd, aes(x = time, y = pred, group = 1, color="Predicted"),linetype=1, linewidth = 0.9) + 
  geom_line(data=data.frame(x=2022,y=0:1.2),aes(x=x,y=y),linetype="dotted")+
  geom_segment(aes(x=2022.0,y=1.2,xend=2022.95,yend=1.2),arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(aes(x=2022.5,1.15,label="Forecast"),size=4,fontface="bold")+
  geom_text(aes(x=2008.9,1.36,label="C. 1 year"),size=4,fontface="bold")+
  scale_color_manual(values = c(Predicted = "red2", Observed = "dodgerblue4"))+
  scale_x_continuous(breaks = seq(from = 2008, to = 2023, by = 1), expand = c(0.02,0.02)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1.2, by = 0.2), expand = c(0.05,0.02), limits=c(0,1.4))+
  theme(strip.background = element_blank(), legend.justification = c(0,1), 
        legend.title= element_blank(),
        legend.text = element_text(size = 9), 
        legend.background = element_rect(fill=NA),
        axis.text=element_text(size=12, color = "black"), 
        axis.title = element_text(size=13),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.line.x=element_line(),axis.line.y=element_line())
plot_3

gridExtra::grid.arrange(plot_1, plot_2,  plot_3,  nrow = 3)

# x_to_y_2022_4_err = ComputeError(x_to_y_2022_4$Observations, x_to_y_2022_4$Predictions);x_to_y_2022_4_err
# x_to_y_2022_4=x_to_y_2022_4 %>%
#   filter(time<181)
# plot(x_to_y_2022_4$time,x_to_y_2022_4$Observations,type="b",col="blue",ylim=c(0.0,0.9),lwd = 2,main="D. Six months prediction",xlab="Time: Months",ylab="Incidence")
# lines(x_to_y_2022_4$time,x_to_y_2022_4$Predictions,col="red",lwd = 2) 
# text(176, 0.85, paste("rho=",round(x_to_y_2022_4_err$rho,2),"    MAE=",round(x_to_y_2022_4_err$MAE,2),"    RMSE=",round(x_to_y_2022_4_err$RMSE,2)))          


###----------------------------------------------------------------#####
########-------------------------------------- Forecasting with data upto 2019
###---six years prediction
lib_x=c(1,71)
lib_y=c(max(lib_x)+1,144)
simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

par(mfrow=c(4,1))
x_to_y_2019_1 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
openxlsx::write.xlsx(x_to_y_2019_1, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/pred_u5.xlsx")

###---Three years prediction
lib_x=c(1,107)
lib_y=c(max(lib_x)+1,144)
simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

x_to_y_2019_2 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
openxlsx::write.xlsx(x_to_y_2019_2, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/pred_u5.xlsx")

###---one years prediction
lib_x=c(1,131)
lib_y=c(max(lib_x)+1,144)
simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

x_to_y_2019_3 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
openxlsx::write.xlsx(x_to_y_2019_3, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/pred_u5.xlsx")

###---six months prediction
lib_x=c(1,137)
lib_y=c(max(lib_x)+1,144)
simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

x_to_y_2019_4 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
openxlsx::write.xlsx(x_to_y_2019_4, file = "D:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/2024/pred_u5.xlsx")

########-------------------------------------- Forecasting plots - One year
###2019

par(mfrow=c(3,1))
x_to_y_2019_1_err = ComputeError(x_to_y_2019_1$Observations, x_to_y_2019_1$Predictions);x_to_y_2019_1_err
x_to_y_2019_1=x_to_y_2019_1 %>%
  filter(time<145)
plot(x_to_y_2019_1$time,x_to_y_2019_1$Observations,type="b",col="blue",ylim=c(0.0,1.2),lwd = 2,main="A. Six years prediction",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2019_1$time,x_to_y_2019_1$Predictions,col="red",lwd = 2) 
text(85, 1.15, paste("rho=",round(x_to_y_2019_1_err$rho,2),"    MAE=",round(x_to_y_2019_1_err$MAE,2),"    RMSE=",round(x_to_y_2019_1_err$RMSE,2)))          

x_to_y_2019_2_err = ComputeError(x_to_y_2019_2$Observations, x_to_y_2019_2$Predictions);x_to_y_2019_2_err
x_to_y_2019_2=x_to_y_2019_2 %>%
  filter(time<145)
plot(x_to_y_2019_2$time,x_to_y_2019_2$Observations,type="b",col="blue",ylim=c(0.0,1.2),lwd = 2,main="B. Three years prediction",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2019_2$time,x_to_y_2019_2$Predictions,col="red",lwd = 2) 
text(115, 1.17, paste("rho=",round(x_to_y_2019_2_err$rho,2),"    MAE=",round(x_to_y_2019_2_err$MAE,2),"    RMSE=",round(x_to_y_2019_2_err$RMSE,2)))          


x_to_y_2019_3_err = ComputeError(x_to_y_2019_3$Observations, x_to_y_2019_3$Predictions);x_to_y_2019_3_err
x_to_y_2019_3=x_to_y_2019_3 %>%
  filter(time<145)
plot(x_to_y_2019_3$time,x_to_y_2019_3$Observations,type="b",col="blue",ylim=c(0.0,1.2),lwd = 2,main="C. One year prediction",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2019_3$time,x_to_y_2019_3$Predictions,col="red",lwd = 2) 
text(135, 1.15, paste("rho=",round(x_to_y_2019_3_err$rho,2),"    MAE=",round(x_to_y_2019_3_err$MAE,2),"    RMSE=",round(x_to_y_2019_3_err$RMSE,2)))          


###From ggplot

###-----------------------6 years
incd=read_excel("incidence_u5 - 2019.xlsx",sheet=1)

incd$t2<-incd$month/12
incd$time<-incd$year+incd$t2
plot_1=ggplot() + theme_classic() +  xlab("Year") + ylab("Malaria incidence") + 
  geom_rect(aes(xmin=2008,xmax=2020, ymin=0, ymax=1.4), fill = "black", alpha = 0.05) + 
  geom_line(data = incd, aes(x = time, y = obs, group = 1, color="Observed"), size = 0.9)  +
  geom_line(data = incd, aes(x = time, y = pred, group = 1, color="Predicted"),linetype=1, linewidth = 0.9) + 
  geom_line(data=data.frame(x=2014,y=0:1.2),aes(x=x,y=y),linetype="dotted")+
  geom_segment(aes(x=2014.0,y=1.3,xend=2019.95,yend=1.3),arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(aes(x=2019.3,1.25,label="Forecast"),size=4,fontface="bold")+
  geom_text(aes(x=2008.9,1.36,label="A. 6 years"),size=4,fontface="bold")+
  scale_color_manual(values = c(Predicted = "red2", Observed = "dodgerblue4"))+
  scale_x_continuous(breaks = seq(from = 2008, to = 2020, by = 1), expand = c(0.02,0.02)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1.2, by = 0.2), expand = c(0.05,0.02), limits=c(0,1.4))+
  theme(strip.background = element_blank(), legend.justification = c(0,1), 
        legend.title= element_blank(),
        legend.text = element_text(size = 9), 
        legend.background = element_rect(fill=NA),
        axis.text=element_text(size=12, color = "black"), 
        axis.title = element_text(size=13),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.line.x=element_line(),axis.line.y=element_line())
plot_1

###-----------------------3 years
incd=read_excel("incidence_u5 - 2019.xlsx",sheet=2)

incd$t2<-incd$month/12
incd$time<-incd$year+incd$t2
plot_2=ggplot() + theme_classic() +  xlab("Year") + ylab("Malaria incidence") + 
  geom_rect(aes(xmin=2008,xmax=2020, ymin=0, ymax=1.4), fill = "black", alpha = 0.05) + 
  geom_line(data = incd, aes(x = time, y = obs, group = 1, color="Observed"), size = 0.9)  +
  geom_line(data = incd, aes(x = time, y = pred, group = 1, color="Predicted"),linetype=1, linewidth = 0.9) + 
  geom_line(data=data.frame(x=2017,y=0:1.2),aes(x=x,y=y),linetype="dotted")+
  geom_segment(aes(x=2017.0,y=1.3,xend=2019.95,yend=1.3),arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(aes(x=2019.3,1.25,label="Forecast"),size=4,fontface="bold")+
  geom_text(aes(x=2008.9,1.36,label="B. 3 years"),size=4,fontface="bold")+
  scale_color_manual(values = c(Predicted = "red2", Observed = "dodgerblue4"))+
  scale_x_continuous(breaks = seq(from = 2008, to = 2020, by = 1), expand = c(0.02,0.02)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1.2, by = 0.2), expand = c(0.05,0.02), limits=c(0,1.4))+
  theme(strip.background = element_blank(), legend.justification = c(0,1), 
        legend.title= element_blank(),
        legend.text = element_text(size = 9), 
        legend.background = element_rect(fill=NA),
        axis.text=element_text(size=12, color = "black"), 
        axis.title = element_text(size=13),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.line.x=element_line(),axis.line.y=element_line())
plot_2

###-----------------------1 year
incd=read_excel("incidence_u5 - 2019.xlsx",sheet=3)

incd$t2<-incd$month/12
incd$time<-incd$year+incd$t2
plot_3=ggplot() + theme_classic() +  xlab("Year") + ylab("Malaria incidence") + 
  geom_rect(aes(xmin=2008,xmax=2020, ymin=0, ymax=1.4), fill = "black", alpha = 0.05) + 
  geom_line(data = incd, aes(x = time, y = obs, group = 1, color="Observed"), size = 0.9)  +
  geom_line(data = incd, aes(x = time, y = pred, group = 1, color="Predicted"),linetype=1, linewidth = 0.9) + 
  geom_line(data=data.frame(x=2019,y=0:1.2),aes(x=x,y=y),linetype="dotted")+
  geom_segment(aes(x=2019.0,y=1.3,xend=2019.95,yend=1.3),arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(aes(x=2019.5,1.25,label="Forecast"),size=4,fontface="bold")+
  geom_text(aes(x=2008.9,1.36,label="C. 1 year"),size=4,fontface="bold")+
  scale_color_manual(values = c(Predicted = "red2", Observed = "dodgerblue4"))+
  scale_x_continuous(breaks = seq(from = 2008, to = 2020, by = 1), expand = c(0.02,0.02)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1.2, by = 0.2), expand = c(0.05,0.02), limits=c(0,1.4))+
  theme(strip.background = element_blank(), legend.justification = c(0,1), 
        legend.title= element_blank(),
        legend.text = element_text(size = 9), 
        legend.background = element_rect(fill=NA),
        axis.text=element_text(size=12, color = "black"), 
        axis.title = element_text(size=13),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.line.x=element_line(),axis.line.y=element_line())
plot_3

gridExtra::grid.arrange(plot_1, plot_2,  plot_3,  nrow = 3)

# x_to_y_2019_4_err = ComputeError(x_to_y_2019_4$Observations, x_to_y_2019_4$Predictions);x_to_y_2019_4_err
# x_to_y_2019_4=x_to_y_2019_4 %>%
#   filter(time<145)
# plot(x_to_y_2019_4$time,x_to_y_2019_4$Observations,type="b",col="blue",ylim=c(0.2,1.2),lwd = 2,main="D. Six months prediction",xlab="Time: Months",ylab="Incidence")
# lines(x_to_y_2019_4$time,x_to_y_2019_4$Predictions,col="red",lwd = 2) 
# text(140, 1.15, paste("rho=",round(x_to_y_2019_4_err$rho,2),"    MAE=",round(x_to_y_2019_4_err$MAE,2),"    RMSE=",round(x_to_y_2019_4_err$RMSE,2)))          






##The forecasting from here is for univariate predictors


# ########-------------------------------------- Forecasting --scatter plot-not necessary
# par(mfrow=c(1,1))
# copred_x_to_y <- Simplex(dataFrame=df, lib = lib_x, pred = lib_y, columns = "Rain_0", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE,embedded = T)
# 
# Details = ComputeError(copred_x_to_y$Observations, copred_x_to_y$Predictions)
# plot(copred_x_to_y$Observations, copred_x_to_y$Predictions, pch = 19, cex = 0.5,
#      xlab = "Observations", ylab = "Predictions", main = "")
# abline(a = 0, b = 1, lty = 2, col = "blue")
# text(-0.6, 1, paste(capture.output(cbind(Details)), collapse = "\n"))

lib_x=c(1,168)
lib_y=c(max(lib_x)+1,180)

simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

par(mfrow=c(5,1))
x_to_y_2022_1 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "lstD_1", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
x_to_y_2022_2 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
x_to_y_2022_3 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "ERA5_RH_2", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
x_to_y_2022_4 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
x_to_y_2022_5 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)


########-------------------------------------- Forecasting with data upto 2019
lib_x=c(1,132)
lib_y=c(max(lib_x)+1,144)
simplex_out_x <- simplex(dat5$Rain_2, lib = lib_x, pred = lib_y,E = c(2:10),silent=TRUE)
best_E_x <- simplex_out_x$E[which.max(simplex_out_x$rho)]

simplex_out_y <- simplex(dat5$incidencePyrs_u5, lib = lib_x, pred = lib_y, E = c(2:10),silent = TRUE)
best_E_y <- simplex_out_y$E[which.max(simplex_out_y$rho)]

par(mfrow=c(5,1))
x_to_y_2019_1 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "lstD_1", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
x_to_y_2019_2 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
x_to_y_2019_3 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "ERA5_RH_2", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
x_to_y_2019_4 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)
x_to_y_2019_5 <- Simplex(dataFrame=dat5, lib = lib_x, pred = lib_y, columns = "Rain_2 lstD_1 ERA5_RH_2 bednets_u5", target = "incidencePyrs_u5", E = best_E_x,showPlot = TRUE)

########-------------------------------------- Forecasting plots - One year
###2022
par(mfrow=c(3,1))
x_to_y_2022_1_err = ComputeError(x_to_y_2022_1$Observations, x_to_y_2022_1$Predictions);x_to_y_2022_1_err
x_to_y_2022_1=x_to_y_2022_1 %>%
  filter(time<181)
plot(x_to_y_2022_1$time,x_to_y_2022_1$Observations,type="b",col="blue",ylim=c(0.2,0.9),lwd = 2,main="A. Daytime land surface temperature",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2022_1$time,x_to_y_2022_1$Predictions,col="red",lwd = 2) 
text(171, 0.85, paste("rho=",round(x_to_y_2022_1_err$rho,2),"    MAE=",round(x_to_y_2022_1_err$MAE,2),"    RMSE=",round(x_to_y_2022_1_err$RMSE,2)))          

x_to_y_2022_2_err = ComputeError(x_to_y_2022_2$Observations, x_to_y_2022_2$Predictions);x_to_y_2022_2_err
x_to_y_2022_2=x_to_y_2022_2 %>%
  filter(time<181)
plot(x_to_y_2022_2$time,x_to_y_2022_2$Observations,type="b",col="blue",ylim=c(0.2,1.0),lwd = 2,main="B. Rainfall",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2022_2$time,x_to_y_2022_2$Predictions,col="red",lwd = 2) 
text(171, 0.95, paste("rho=",round(x_to_y_2022_2_err$rho,2),"    MAE=",round(x_to_y_2022_2_err$MAE,2),"    RMSE=",round(x_to_y_2022_2_err$RMSE,2)))          


x_to_y_2022_3_err = ComputeError(x_to_y_2022_3$Observations, x_to_y_2022_3$Predictions);x_to_y_2022_3_err
x_to_y_2022_3=x_to_y_2022_3 %>%
  filter(time<181)
plot(x_to_y_2022_3$time,x_to_y_2022_3$Observations,type="b",col="blue",ylim=c(0.2,0.9),lwd = 2,main="C. Relative humidity",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2022_3$time,x_to_y_2022_3$Predictions,col="red",lwd = 2) 
text(171, 0.85, paste("rho=",round(x_to_y_2022_3_err$rho,2),"    MAE=",round(x_to_y_2022_3_err$MAE,2),"    RMSE=",round(x_to_y_2022_3_err$RMSE,2)))          


x_to_y_2022_4_err = ComputeError(x_to_y_2022_4$Observations, x_to_y_2022_4$Predictions);x_to_y_2022_4_err
x_to_y_2022_4=x_to_y_2022_4 %>%
  filter(time<181)
plot(x_to_y_2022_4$time,x_to_y_2022_4$Observations,type="b",col="blue",ylim=c(0.0,0.8),lwd = 2,main="D. Bed net use",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2022_4$time,x_to_y_2022_4$Predictions,col="red",lwd = 2) 
text(171, 0.75, paste("rho=",round(x_to_y_2022_4_err$rho,2),"    MAE=",round(x_to_y_2022_4_err$MAE,2),"    RMSE=",round(x_to_y_2022_4_err$RMSE,2)))          


x_to_y_2022_5_err = ComputeError(x_to_y_2022_5$Observations, x_to_y_2022_5$Predictions);x_to_y_2022_5_err
x_to_y_2022_5=x_to_y_2022_5 %>%
  filter(time<181)
plot(x_to_y_2022_5$time,x_to_y_2022_5$Observations,type="b",col="blue",ylim=c(0.2,1.0),lwd = 2,main="E. Multivariable",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2022_5$time,x_to_y_2022_5$Predictions,col="red",lwd = 2) 
text(171, 0.95, paste("rho=",round(x_to_y_2022_5_err$rho,2),"    MAE=",round(x_to_y_2022_5_err$MAE,2),"    RMSE=",round(x_to_y_2022_5_err$RMSE,2)))   


###2019

par(mfrow=c(3,1))
x_to_y_2019_1_err = ComputeError(x_to_y_2019_1$Observations, x_to_y_2019_1$Predictions);x_to_y_2019_1_err
x_to_y_2019_1=x_to_y_2019_1 %>%
  filter(time<145)
plot(x_to_y_2019_1$time,x_to_y_2019_1$Observations,type="b",col="blue",ylim=c(0.2,1.2),lwd = 2,main="A. Daytime land surface temperature",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2019_1$time,x_to_y_2019_1$Predictions,col="red",lwd = 2) 
text(135, 1.1, paste("rho=",round(x_to_y_2019_1_err$rho,2),"    MAE=",round(x_to_y_2019_1_err$MAE,2),"    RMSE=",round(x_to_y_2019_1_err$RMSE,2)))          

x_to_y_2019_2_err = ComputeError(x_to_y_2019_2$Observations, x_to_y_2019_2$Predictions);x_to_y_2019_2_err
x_to_y_2019_2=x_to_y_2019_2 %>%
  filter(time<145)
plot(x_to_y_2019_2$time,x_to_y_2019_2$Observations,type="b",col="blue",ylim=c(0.2,1.2),lwd = 2,main="B. Rainfall",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2019_2$time,x_to_y_2019_2$Predictions,col="red",lwd = 2) 
text(135, 1.1, paste("rho=",round(x_to_y_2019_2_err$rho,2),"    MAE=",round(x_to_y_2019_2_err$MAE,2),"    RMSE=",round(x_to_y_2019_2_err$RMSE,2)))          


x_to_y_2019_3_err = ComputeError(x_to_y_2019_3$Observations, x_to_y_2019_3$Predictions);x_to_y_2019_3_err
x_to_y_2019_3=x_to_y_2019_3 %>%
  filter(time<145)
plot(x_to_y_2019_3$time,x_to_y_2019_3$Observations,type="b",col="blue",ylim=c(0.2,1.2),lwd = 2,main="C. Relative humidity",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2019_3$time,x_to_y_2019_3$Predictions,col="red",lwd = 2) 
text(135, 1.1, paste("rho=",round(x_to_y_2019_3_err$rho,2),"    MAE=",round(x_to_y_2019_3_err$MAE,2),"    RMSE=",round(x_to_y_2019_3_err$RMSE,2)))          


x_to_y_2019_4_err = ComputeError(x_to_y_2019_4$Observations, x_to_y_2019_4$Predictions);x_to_y_2019_4_err
x_to_y_2019_4=x_to_y_2019_4 %>%
  filter(time<145)
plot(x_to_y_2019_4$time,x_to_y_2019_4$Observations,type="b",col="blue",ylim=c(0.2,1.2),lwd = 2,main="D. Bed net use",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2019_4$time,x_to_y_2019_4$Predictions,col="red",lwd = 2) 
text(135, 1.1, paste("rho=",round(x_to_y_2019_4_err$rho,2),"    MAE=",round(x_to_y_2019_4_err$MAE,2),"    RMSE=",round(x_to_y_2019_4_err$RMSE,2)))          


x_to_y_2019_5_err = ComputeError(x_to_y_2019_5$Observations, x_to_y_2019_5$Predictions);x_to_y_2019_5_err
x_to_y_2019_5=x_to_y_2019_5 %>%
  filter(time<145)
plot(x_to_y_2019_5$time,x_to_y_2019_5$Observations,type="b",col="blue",ylim=c(0.2,1.2),lwd = 2,main="E. Multivariable",xlab="Time: Months",ylab="Incidence")
lines(x_to_y_2019_5$time,x_to_y_2019_5$Predictions,col="red",lwd = 2) 
text(135, 1.1, paste("rho=",round(x_to_y_2019_5_err$rho,2),"    MAE=",round(x_to_y_2019_5_err$MAE,2),"    RMSE=",round(x_to_y_2019_5_err$RMSE,2)))   
