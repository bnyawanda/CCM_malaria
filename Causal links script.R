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

###Creating a map of the 33 IEIP villages
df_pc <- st_read(dsn = "C:/Swiss TPH/Swiss TPH - Write up/Chapter 1- WK Incidence/Data/Shape files - Maps/IEIP_Villages.shp") %>%  
  as_tibble() %>% 
  # filter(str_detect(plz, "^60")) %>% 
  st_as_sf
# generate 1) edge polygon unioning all postalcode geometries
# and 2) corresponding bounding box
df_pols <- st_union(df_pc) %>%  as_tibble() %>% 
  mutate(bbox = st_as_sfc(st_bbox(geometry))) %>% st_as_sf()

# compute difference between bounding box and edge polygon
diff_pol <- st_difference(df_pols$bbox, df_pols$geometry)

# read openstreetmap tile
osm <- read_osm(df_pc) 

tmap_mode("plot")
# first plot osm content
Lwak=tm_shape(osm) +  tm_rgb() +
  # overlay difference polygon to mask content outside
  tm_shape(df_pc) +
  tm_borders(lwd = 2.0, col = "darkblue") +
  tm_layout(frame = T,main.title = "F. PBIDS Area",title.bg.color = "white",main.title.position = c("center", "top"))+
  tm_credits("Lake Victoria",position = c(0.80,0.015),size = 0.9)+
  tm_scale_bar(position = c(0.05,0.05), width = 0.15,text.size=0.7)+
  tm_compass(position = c(0.1,0.15))


#####call datasets 
dat0 = read_dta("Merged_20_22.dta") 
dat1 = read_dta("C:/Swiss TPH/Swiss TPH - Write up/Chapter 1- WK Incidence/Code/JAGS/Pen/Incidence_data_Aggregated_3.dta") 
dat1=dat1 %>%
  dplyr::select(c(year,month,Malaria_Positive1,agec1yrs,propos1)) %>%
  rename(cases_u5=Malaria_Positive1,personyrs_u5=agec1yrs,bednets_u5=propos1)

dat2=read.table("dat1.csv",sep=",");head(dat1);dim(dat1)
dat2=dat2 %>%
  mutate(bednets=replace(bednets,time==144,0.96))%>%
  dplyr::select(c(year,month,cases,personyrs,bednets))

dat3 <- sqldf("SELECT *
              FROM dat1
              INNER JOIN dat2 USING(year,month)")


dat4=rbind(dat3,dat0)

dat4=dat4%>%
  mutate(incidencePyrs_u5=cases_u5/personyrs_u5)%>%
  mutate(incidencePyrs=cases/personyrs)

clim=read.table("dat_21112023.csv",sep=",") #;head(clim);dim(clim)

dat5 <- sqldf("SELECT *
              FROM dat4
              INNER JOIN clim USING(year,month)")

dat4= dat5%>%
  arrange(year,month)%>%
  mutate(time=row_number())%>%
  mutate(yrmon=year+(month/12))




cor1<-dat4[c("incidencePyrs_u5",
             "lstD_0","lstD_1","lstD_2","lstD_3","lstD_4",
             "Rain_0","Rain_1","Rain_2","Rain_3","Rain_4",
             "FLDAS_RH_0","FLDAS_RH_1","FLDAS_RH_2","FLDAS_RH_3","FLDAS_RH_4",
             "ERA5_RH_0","ERA5_RH_1","ERA5_RH_2","ERA5_RH_3","ERA5_RH_4")]
rc<-rcorr(as.matrix(cor1))
print(rc$r[1,], digits = 1)
format.pval(rc$P[1,],  digits = 2)

###################################################################################
# Fit a seasonal spline to data using 2 approaches  i.e. ss and smooth.spline
# http://users.stat.umn.edu/~helwig/notes/smooth-spline-notes.html
###################################################################################
# For study area plots
adm0= sf::st_read("C:/Swiss TPH/Swiss TPH - Write up/Chapter 1- WK Incidence/Data/Shape files - Maps/Kenya Shapefiles/ken_adm_iebc_20191031_shp/ken_admbnda_adm0_iebc_20191031.shp") #Kenyan admin 0
ieip= sf::st_read("C:/Swiss TPH/Swiss TPH - Write up/Chapter 1- WK Incidence/Data/Shape files - Maps/IEIP_Villages.shp")%>% 
  mutate(ID=1)
Siaya=sf::st_read("C:/Swiss TPH/Swiss TPH - Write up/Chapter 1- WK Incidence/Data/Shape files - Maps/Kenya Shapefiles/KEN_adm/KEN_adm2.shp")%>%
  dplyr::filter(NAME_2==c("Siaya"))
ieip_Dslvd <- ieip %>%
  group_by(ID) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()
plot(ieip_Dslvd)

cities <- data.frame(
  City = c("Kisumu", "Nairobi"),
  Longitude = c(34.7680, 36.8219),
  Latitude = c(-0.0917, -1.2921))

cities1 <- SpatialPointsDataFrame(cities[,2:3],
                                  cities,    #the R object to convert
                                  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
Lwak_Map=
  tm_shape(adm0) + tm_borders(col="black",lwd = 2)+
  tm_shape(Siaya) + tm_borders(col="blue",lwd = 3)+  #fffdd0-cream color
  tm_shape(ieip_Dslvd) + tm_borders(col="red",lwd = 5)+
  tm_shape(cities1)+tm_symbols(col = "black", shape = 1,border.lwd = 3,size=0.3)+
  tm_text(text = "City", size = 2,clustering = FALSE , auto.placement = 0.05 )+
  tm_credits("KENYA",position = c(0.425,0.5),size = 2.7)+
  tm_credits("F. Location",position = c(0.425,0.90),bg.color="white",size=2,fontface = "bold")

Lwak_Map #save as png

#preparing the number of tiles
summary(dat4$incidencePyrs_u5)
summary(dat4$lstD_0)
summary(dat4$Rain_0)
summary(dat4$ERA5_RH_0)
summary(dat4$bednets_u5)

par(mfrow=c(3,2))
par(las = 2) #x labels as usual
# par(las = 2) #Rotates x labels to 90 degrees - par() resets this
par(mar = c(5,4,5,2)+0.1) #margins
par(mgp = c(3, 0.7, 0)) #distance from tick marks to x-label

### incidence
mod.ss=npreg::ss(dat4$yrmon, dat4$incidencePyrs_u5, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$incidencePyrs_u5 - mod.ss$y )^2))
# plot(dat4$yrmon, dat4$incidencePyrs_u5, type = 'b',xlab="Year",ylab="Incidence",main="A. Malaria Incidence",axes = c(FALSE, TRUE),frame= T) #Axes option supresses the x-axis only
plot(dat4$yrmon, dat4$incidencePyrs_u5, type = 'b',xlab="",ylab="Incidence",main="A. Malaria Incidence",frame= T, cex.main = 1.7,cex.lab = 1.5,cex.axis = 1.5) #Axes option supresses the x-axis only
axis(side = 1, at = seq(2008, 2023, by = 1),labels = seq(2008, 2023, by = 1),cex.axis = 1.5)
# axis(side = 2,cex.axis = 1.5)  # Y-axis
plot(mod.ss,lty = 2, col = 2, lwd = 2,add=T)
dat4$incidence_season=mod.ss$y

### lstD
mod.ss=npreg::ss(dat4$yrmon, dat4$lstD_0, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$lstD_0- mod.ss$y )^2))
# plot(dat4$yrmon, dat4$lstD_0, type = 'b',xlab="Year",ylab="Degrees Celcius (\u00B0C)",main="B. Day land surface temprerature",axes = c(FALSE, TRUE),frame= T) #Axes option supresses the x-axis only
plot(dat4$yrmon, dat4$lstD_0, type = 'b',xlab="",ylab="Degrees Celcius (\u00B0C)",main="B. Day land surface temprerature",frame= T, cex.main = 1.7,cex.lab = 1.5,cex.axis = 1.5 ) #Axes option supresses the x-axis only
axis(side = 1, at = seq(2008, 2023, by = 1),labels = seq(2008, 2023, by = 1),cex.axis = 1.5)
# axis(side = 2,cex.axis = 1.2)  # Y-axis
plot(mod.ss,lty = 2, col = 2, lwd = 2,add=T)
dat4$lstD_0_season=mod.ss$y

### Rainfall
mod.ss=npreg::ss(dat4$yrmon, dat4$Rain_0, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$Rain_0- mod.ss$y )^2))
# plot(dat4$yrmon, dat4$Rain_0, type = 'b',xlab="Year",ylab="mm",main="C. Rainfall",axes = c(FALSE, TRUE),frame= T) #Axes option supresses the x-axis only
plot(dat4$yrmon, dat4$Rain_0, type = 'b',xlab="",ylab="mm",main="C. Rainfall",frame= T, cex.main = 1.7,cex.lab = 1.5,cex.axis = 1.5 ) #Axes option supresses the x-axis only
axis(side = 1, at = seq(2008, 2023, by = 1),labels = seq(2008, 2023, by = 1),cex.axis = 1.5)
# axis(side = 2,cex.axis = 1.2)  # Y-axis
plot(mod.ss,lty = 2, col = 2, lwd = 2,add=T)
dat4$Rain_0_season=mod.ss$y

### Humidity
mod.ss=npreg::ss(dat4$yrmon, dat4$ERA5_RH_0, nknots = 10);mod.ss; names(mod.ss)
# plot(dat4$yrmon, dat4$ERA5_RH_0, type = 'b',xlab="Year",ylab="Relative humidity (%)",main="D. Humidity",axes = c(FALSE, TRUE),frame= T) #Axes option supresses the x-axis only
plot(dat4$yrmon, dat4$ERA5_RH_0, type = 'b',xlab="",ylab="Percent (%)",main="D. Relative Humidity",frame= T, cex.main = 1.7,cex.lab = 1.5,cex.axis = 1.5) #Axes option supresses the x-axis only
axis(side = 1, at = seq(2008, 2023, by = 1),labels = seq(2008, 2023, by = 1),cex.axis = 1.5)
# axis(side = 2,cex.axis = 1.2)  # Y-axis
plot(mod.ss,lty = 2, col = 2, lwd = 2,add=T)
dat4$ERA5_RH_0_season=mod.ss$y

### Bednet
mod.ss=npreg::ss(dat4$yrmon, dat4$bednets_u5, nknots = 10);mod.ss; names(mod.ss)
# plot(dat4$yrmon, dat4$bednets_u5, type = 'b',xlab="Year",ylab="Proportion",main="E. Bed net use",axes = c(FALSE, TRUE),frame= T) #Axes option supresses the x-axis only
plot(dat4$yrmon, dat4$bednets_u5, type = 'b',xlab="",ylab="Proportion",main="E. Bed net use",frame= T, cex.main = 1.7,cex.lab = 1.5,cex.axis = 1.5 ) #Axes option supresses the x-axis only
axis(side = 1, at = seq(2008, 2023, by = 1),labels = seq(2008, 2023, by = 1),cex.axis = 1.5)
# axis(side = 2,cex.axis = 1.2)  # Y-axis
plot(mod.ss,lty = 2, col = 2, lwd = 2,add=T)
dat4$Bedn_0_season=mod.ss$y

PBIDS <- readPNG("C:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/Plots/2024/PBIDS3.png")

grid.rect(x = 0.75, y = 0.185, width = 0.26, height = 0.28, gp = gpar(lwd = 1, col = "black"))
grid.raster(PBIDS,x = 0.75, y = 0.185, width = 0.258, height = 0.278) #try severayy till iy plots

grid.raster(PBIDS,width = 0.26, height = 0.28,vp = viewport(x = 0.75, y = 0.185, width = 0.8, height = 0.8)) #try severayy till iy plots

# ##Import and overlay the 2 PNG files 
# image1 <- readPNG("C:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/Plots/Descriptives.png")
# image2 <- readPNG("C:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/Plots/PBIDS.png")
# 
# # Create a grid viewport
# grid.newpage()
# 
# # Draw the first image
# grid.raster(image1)
# 
# # Draw the second image with some transparency
# grid.raster(image2, width = 0.8, height = 0.8, vp = viewport(x = 0.5, y = 0.5, width = 0.8, height = 0.8))
# grid.raster(image2, width = 0.6, height = 0.5, vp = viewport(x = 0.75, y = 0.2, width = 0.7, height = 0.7))


##Additional seasonal vars
mod.ss=npreg::ss(dat4$yrmon, dat4$lstD_0, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$lstD_0- mod.ss$y )^2))
dat4$lstD_0_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$lstD_1, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$lstD_1- mod.ss$y )^2))
dat4$lstD_1_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$lstD_2, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$lstD_2- mod.ss$y )^2))
dat4$lstD_2_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$lstD_3, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$lstD_3- mod.ss$y )^2))
dat4$lstD_3_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$lstD_4, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$lstD_4- mod.ss$y )^2))
dat4$lstD_4_season=mod.ss$y

#Rain
mod.ss=npreg::ss(dat4$yrmon, dat4$Rain_0, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$Rain_0- mod.ss$y )^2))
dat4$Rain_0_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$Rain_1, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$Rain_1- mod.ss$y )^2))
dat4$Rain_1_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$Rain_2, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$Rain_2- mod.ss$y )^2))
dat4$Rain_2_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$Rain_3, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$Rain_3- mod.ss$y )^2))
dat4$Rain_3_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$Rain_4, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$Rain_4- mod.ss$y )^2))
dat4$Rain_4_season=mod.ss$y

# Humidity
mod.ss=npreg::ss(dat4$yrmon, dat4$ERA5_RH_0, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$ERA5_RH_0- mod.ss$y )^2))
dat4$ERA5_RH_0_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$ERA5_RH_1, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$ERA5_RH_1- mod.ss$y )^2))
dat4$ERA5_RH_1_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$ERA5_RH_2, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$ERA5_RH_2- mod.ss$y )^2))
dat4$ERA5_RH_2_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$ERA5_RH_3, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$ERA5_RH_3- mod.ss$y )^2))
dat4$ERA5_RH_3_season=mod.ss$y

mod.ss=npreg::ss(dat4$yrmon, dat4$ERA5_RH_4, nknots = 10);mod.ss; names(mod.ss)
sqrt(mean(( dat4$ERA5_RH_4- mod.ss$y )^2))
dat4$ERA5_RH_4_season=mod.ss$y

write_dta(dat4,"dat4.dta")
############################################################################
# Create surrogate time series for each climatic factor by
# 1. Remove the seasonal pattern obtain with the splines i.e. X_s(t)
# 2. Calculate the residuals from the seasonal pattern R(t)=X(t)-X_s(t)
# 3. Add the residuals back to the spline randomly 

######################################################################
##Check CCMSplines from the Github example


#########################################################################

dat4 = read_dta("dat4.dta") 
Original=ClimFact=dat4 %>%
  dplyr::select(cases_u5,Rain_0,Rain_1,Rain_2,Rain_3,Rain_4,
                lstD_0,lstD_1,lstD_2,lstD_3,lstD_4,
                # FLDAS_RH_0,FLDAS_RH_1,FLDAS_RH_2,FLDAS_RH_3,FLDAS_RH_4,
                ERA5_RH_0,ERA5_RH_1,ERA5_RH_2,ERA5_RH_3,ERA5_RH_4)%>%
  dplyr::mutate_at(c("lstD_0","lstD_1","lstD_2","lstD_3","lstD_4",
                     "Rain_0","Rain_1","Rain_2","Rain_3","Rain_4",
                     # "FLDAS_RH_0","FLDAS_RH_1","FLDAS_RH_2","FLDAS_RH_3","FLDAS_RH_4",
                     "ERA5_RH_0","ERA5_RH_1","ERA5_RH_2","ERA5_RH_3","ERA5_RH_4"),~(scale(.) %>% as.vector)) #standardise the variables


Seasonality=dat4 %>%
  dplyr::select(lstD_0_season, lstD_1_season,lstD_2_season,lstD_3_season,lstD_4_season,
                Rain_0_season, Rain_1_season,Rain_2_season,Rain_3_season,Rain_4_season,
                ERA5_RH_0_season, ERA5_RH_1_season,ERA5_RH_2_season,ERA5_RH_3_season,ERA5_RH_4_season)



rho_E <- EmbedDimension(dataFrame = dat4, columns = "cases_u5", target = "cases_u5",
                        lib = "1 170", pred = "1 170", showPlot = TRUE)
max(rho_E$rho)
# E = 7

E = 5
rho_theta_e3 = PredictNonlinear(dataFrame = dat4, columns = "cases_u5",
                                target = "cases_u5", lib = "1 170", pred = "1 170", E = E)
max(rho_theta_e3$rho)
# E = 2
E = 2
vars = colnames(Original)
var_pairs = combn(vars, 2) # Combinations of vars, 2 at a time
libSize = paste(NROW(dat4) - E, NROW(dat4) - E, 10, collapse = " ")
ccm_matrix = array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,vars))

for (i in 1:ncol(var_pairs)) {
  ccm_out = CCM(dataFrame = dat4, 
                columns = var_pairs[1, i], 
                target = var_pairs[2,i], 
                libSizes = libSize, 
                Tp = 0, 
                E = E, 
                sample = 100)
  outVars = names(ccm_out)
  var_out = unlist(strsplit(outVars[2], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 2]
  var_out = unlist(strsplit(outVars[3], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 3]
}

# Seasonal Surrogate Test

############################################################################
# Create surrogate time series for each climatic factor by
# 1. Remove the seasonal pattern obtain with the splines i.e. X_s(t)
# 2. Calculate the residuals from the seasonal pattern R(t)=X(t)-X_s(t)
# 3. Add the residuals back to the spline randomly 

######################################################################
##Check CCMSplines from the Github example
Original=Original%>%
  select(-cases_u5)

make_pred_nozero <- function(time_series,E){
  I_zero_strings <- which(time_series==0)
  I_zero_strings <- Reduce(intersect, lapply((0:E),function(offset) I_zero_strings-offset))
  I_zero_strings <- c(0,I_zero_strings,NROW(time_series)) # for convenience in next step
  N_zero_strings <- length(I_zero_strings)
  lib_nozeroes <- cbind(I_zero_strings[1:(N_zero_strings-1)]+1,I_zero_strings[2:(N_zero_strings)]-1)
  lib_out <- lib_nozeroes[which(lib_nozeroes[,2] > lib_nozeroes[,1]),]
  return(lib_out)
}



NSurr=1000
TimeForPred=0
MaxE=10 #Dimensional spaces
alpha=0.05
Drivers=dim(Original)[2]; Drivers
Residuals<-as.data.frame(matrix(NA,NROW(Original),Drivers))
RhoSurr<-matrix(NA,(NSurr+1),Drivers);RhoSurr
signf<-matrix(NA,1,Drivers)
Target<-dat4$cases_u5 ##Should we use incidence instead?

count<-1; j=1;i=1
for(j in 1:Drivers){
  Residuals[,count]<-(Original[,j]-Seasonality[,j])
  block_temp<-as.data.frame(matrix(NA,NROW(Original),2))
  E_star<-(matrix(NA,1,NSurr))
  lib_ccm<-c(1,NROW(Original))
  
  for (i in 1:NSurr){
    block_temp[,1]<-Target
    block_temp[,2]<-sample(Residuals[,count])+Seasonality[,j]
    out.temp <- do.call(rbind,lapply(2:MaxE, function(E_i){
      pred_ccm <- make_pred_nozero(Target,E_i)
      rEDM::ccm(block=block_temp,
                E=E_i,
                lib=lib_ccm,
                pred=pred_ccm,
                lib_sizes = NROW(block_temp)-MaxE,
                exclusion_radius=0,
                random_libs = FALSE,
                num_sample=1,
                tp = (TimeForPred-1),
                lib_column = 1,
                target_column = 2,
                stats_only=FALSE)  # target column of the block
    }))
    res=do.call(rbind, lapply(out.temp[,2], data.frame, stringsAsFactors=FALSE))
    E_star[i] <- res$E[which.max(res$rho)] # here it was out.temp$E[which.max(out.temp[,2])+1]
    pred_ccm <- make_pred_nozero(Target,E_star[i])
    df.out.ccm <- ccm(block=block_temp,
                      E=E_star[i],
                      lib=lib_ccm,
                      pred = pred_ccm,
                      lib_sizes = NROW(block_temp)-MaxE,
                      exclusion_radius=0,
                      random_libs = FALSE,
                      num_sample=1,
                      tp = TimeForPred,
                      stats_only=FALSE)
    RhoSurr[(i+1),count]<-df.out.ccm$CCM1_PredictStat$rho # it was df.out.ccm$rho
  } # end loop surrogates
  
  ## Original
  block_temp[,2]<-Original[,j]
  # optimal E for original
  out.temp <- do.call(rbind,lapply(1:MaxE, function(E_i){
    pred_ccm <- make_pred_nozero(Target,E_i)
    ccm(block=block_temp,
        E=E_i,
        lib=lib_ccm,
        pred=pred_ccm, 
        lib_sizes = NROW(block_temp)-MaxE,
        exclusion_radius=0,
        random_libs = FALSE,
        num_sample=1,
        tp = (TimeForPred-1),
        lib_column = 1,
        target_column = 2)
  }))
  E_starOriginal<- out.temp$E[which.max(out.temp[,2])]   # it was out.temp$E[which.max(out.temp$rho[2:MaxE])+1]
  pred_ccm <- make_pred_nozero(Target,E_starOriginal)
  df.out.ccm <- ccm(block=block_temp,
                    E=E_starOriginal,
                    lib=lib_ccm,
                    pred = pred_ccm,
                    lib_sizes = NROW(block_temp)-MaxE,
                    exclusion_radius=0,
                    random_libs = FALSE,
                    num_sample=1,
                    tp = TimeForPred)
  RhoSurr[1,count]<- df.out.ccm[,2]     # it was: df.out.ccm$rho
  count<-count+1
} #end loop drivers

# check significance
for (k in 1:Drivers){
  signf[k]<-(sum(RhoSurr[1,k]>RhoSurr[,k])/NSurr)>(1-alpha)
}
RhoSurr<-as.data.frame(RhoSurr,row.names = F)
colnames(RhoSurr)<-names(Original);head(RhoSurr)
# write.csv(RhoSurr,paste('Boxplot_tp=',TimeForPred,'.csv',sep=''),row.names = F)

par(mfrow=c(1,1))
boxplot(RhoSurr)
summary(RhoSurr)
title(main =paste('tp= ',TimeForPred,sep=''),ylab='rho')
points(seq(1,Drivers),RhoSurr[1,],pch=16,cex=1.25,lwd=1.25)
points(seq(1,Drivers)[signf],RhoSurr[1,signf],pch=8,col='red',cex=2,lwd=2)


# After identifying the prediction skills using CCM and obtaining significance from the surrogates we plot
df <- read_excel("C:/Swiss TPH/Swiss TPH - Write up/Chapter 2- Causal inference/Results/CCM Prediction skills_u5.xlsx",3)


df_lstd=df%>%
  filter(var==c("lstD_4","lstD_3","lstD_2","lstD_1","lstD_0"))%>%
  mutate(Time=-4:0)

df_rain=df%>%
  filter(var==c("Rain_4","Rain_3","Rain_2","Rain_1","Rain_0"))%>%
  mutate(Time=-4:0)

df_rh=df%>%
  filter(var==c("ERA5_RH_4","ERA5_RH_3","ERA5_RH_2","ERA5_RH_1","ERA5_RH_0"))%>%
  mutate(Time=-4:0)

par(mfrow=c(3,1))
par(las = 1)
plot(df_lstd$Time,df_lstd$rho,type = 'b',xlab="Lags",ylab="\u03C1",main="A. Daytime land surface temperature (\u00B0C)",ylim=c(-0.05,0.4),cex.main = 1.7,cex.lab = 1.5,cex.axis = 1.5 )
points(df_lstd$Time,df_lstd$signf,pch=8,cex=1.5,lwd=2,col='red')

plot(df_rain$Time,df_rain$rho,type = 'b',xlab="Lags",ylab="\u03C1",main="B. Rainfall (mm)",ylim=c(-0.05,0.25),cex.main = 1.7,cex.lab = 1.5,cex.axis = 1.5 )
points(df_rain$Time,df_rain$signf,pch=8,cex=1.5,lwd=2,col='red')

plot(df_rh$Time,df_rh$rho,type = 'b',xlab="Lags",ylab="\u03C1",main="C. Relative humidity (%)",ylim=c(-0.1,0.45),cex.main = 1.7,cex.lab = 1.5,cex.axis = 1.5 )
points(df_rh$Time,df_rh$signf,pch=8,cex=1.5,lwd=2,col='red')



##########################################################################################################
# Obtain the coefficients (interaction strenghts) for each 
# pre-defined causal variable with respective lag.
# The series to be analyzed are manually cut (according to the lag) and named in the beginning, 
# the rest just needs to use the same column names.
###########################################################################################################
##Check CCM Coefficient code from Github for the series cut section

##With humidity lag 3
series_cut=dat4 %>%
  dplyr::select(incidencePyrs_u5,AIRT_1,lstD_1,Rain_2,ERA5_RH_2,bednets_u5) %>%
  mutate(Rain_2=replace(Rain_2,Rain_2>350,mean(Rain_2)))

series_cut_N=series_cut %>%
  dplyr::mutate_at(c("incidencePyrs_u5","lstD_1","Rain_2","ERA5_RH_2","bednets_u5"),~(scale(.) %>% as.vector)) #standardise the variables

aux2<-block_lnlp(series_cut_N,theta=1.,
                 columns = c("lstD_1","Rain_2","ERA5_RH_2","bednets_u5"),
                 target_column = 'incidencePyrs_u5',
                 method = 's-map',
                 tp = 0,
                 save_smap_coefficients = T);#str(aux2)

C_coeff=aux2$smap_coefficients[[1]]
colnames(C_coeff)=c("Index","C0","C1_lstD_1","C2_Rain_2","C3_ERA5_RH_2","C4_bednets_u5");head(C_coeff)

par(mfrow=c(4,1))
plot(series_cut$lstD_1, C_coeff$C1_lstD_1,xlab='Daytime land surface temperature (\u00B0C)',ylab='interaction strength [lstD lag 1]',axes = c(FALSE, TRUE),frame= T) #,main='A.'
abline(0,0,lty=2,lwd=2)
axis(side = 1, at = seq(25, 45, by = 3),labels = seq(25, 45, by = 3))
axis(side = 2)  # Y-axis
title("A.", line = 1, adj = 0.01)

plot(series_cut$Rain_2, C_coeff$C2_Rain_2,xlab='Rainfall (mm)',ylab='interaction strength [Rain lag 2]',axes = c(FALSE, TRUE),frame= T)
abline(0,0,lty=2,lwd=2)
axis(side = 1, at = seq(0, 300, by = 30),labels = seq(0, 300, by = 30))
axis(side = 2)  # Y-axis
title("B.", line = 1, adj = 0.01)

plot(series_cut$ERA5_RH_2, C_coeff$C3_ERA5_RH_2,xlab='Relative humidity (%)',ylab='interaction strength [Humidity lag 2]',axes = c(FALSE, TRUE),frame= T)
abline(0,0,lty=2,lwd=2)
axis(side = 1, at = seq(50, 85, by = 5),labels = seq(50, 85, by = 5))
axis(side = 2)  # Y-axis
title("C.", line = 1, adj = 0.01)

plot(series_cut$bednets_u5, C_coeff$C4_bednets_u5,xlab='Bed net use (proportion)',ylab='interaction strength [Bed nets]',axes = c(FALSE, TRUE),frame= T)
abline(0,0,lty=2,lwd=2)
axis(side = 1, at = seq(0.7, 1, by = 0.05),labels = seq(0.7, 1, by = 0.05))
axis(side = 2)  # Y-axis
title("D.", line = 1, adj = 0.01)
