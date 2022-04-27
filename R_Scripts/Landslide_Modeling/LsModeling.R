
####################################################
################ Data preparations #################
####################################################

# libraries
library(dplyr)
library(raster)
library(rgdal)
library(writexl)

##### read all data #####

# directory for landslide pixel counts
wd <- setwd("O:/GEO403/GIS/LSModel/Tables/Combine")
# store file names
temp = list.files(pattern = "*.txt")
# create list for pixel counts of landslide areas
counts_ls <- list() 
# fill the list with data
for (i in 1:length(temp)){
  counts_ls[[i]] <- read.csv2(temp[i])
}

# directory for pixel counts of viewshed
wd <- setwd("O:/GEO403/GIS/LSModel/Tables/View")
# store file names
temp = list.files(pattern = "*.txt") 
# create list for pixel counts of viewshed
counts <- list()
# fill the list with data
for (i in 1:length(temp)){
  counts[[i]] <- read.csv2(temp[i])
}



##### clean the data #####

# list of all parameters (same order as in counts/ counts_ls!!! alphabetical!!!)
name <- c("curv", "dem", "dist_roads", "dist_water", 
          "landcover", "litho", "slope", "soil", "TWetI")
columns = c("curv_view", "dem_view", "dist_roads_view", "dist_water_view", 
            "landcover_view", "litho_view", "slope_view", "soil_view", "TWI_view")


# loop through parameter tables
for (d in 1:length(counts_ls)) {
  
  # cleaning landslide tables
  
  # create  dataframe with sequence of all original classes
  id <- seq(1:nrow(counts[[d]]))
  df <- data.frame(matrix(nrow = length(id), ncol = 1))
  colnames(df) = columns[d]
  df[,1] <- id
  
  # combine new dataframe with landslide table and do some cleaning
  com <- left_join(df, counts_ls[[d]], by = columns[d])
  com <- subset(com, select = -c(OBJECTID, Value, slides))
  com$Value <- com[, 1]
  
  # now combine with corresponding viewshed table
  
  # combine clean landslide table with viewshed table and do some cleaning
  final <- left_join(counts[[d]], com, by = "Value")
  final <- subset(final, select = -c(OID_, Value))
  
  # define new variable names and sort variables
  cols <- c("npix_all", "class", "npix_ls")
  colnames(final) <- cols
  final <- final[, c(2, 1, 3)]
  
  # assign a new clean dataframe 
  assign(paste0(name[d]), final)
  
}



# define new list of clean data
data <- list(curv, dem, dist_roads, dist_water, 
             landcover, litho, slope, soil, TWetI)
name <- c("curv", "dem", "dist_roads", "dist_water", 
          "landcover", "litho", "slope", "soil", "TWetI")




##########################################################
################ Statistical Index #######################
###### calculating Wi큦 for each parameter class #########
##########################################################


# formula for calculating Wi큦
# Wi = ln(Landslide density in spec. class / Landslide density in the study area)
# = ln((npix of ls of spec. class/ npix of spec. class) / (npix of ls / npix total))

# define a function that executes the formula for Wi calculation
fo <- function(sum_ls_class, sum_class, sum_ls, sum_all) 
{log((sum_ls_class/sum_class)/(sum_ls/sum_all))}


#### Calculate the Wi values for each class of the parameters (litho, soil,...) ####

# loop through list of all parameters
for (d in 1:length(data)) {
  
  # create/empty temp storage for Wi values
  Wi <- c()
  
  # loop through each class
  for (i in unique(data[[d]]$class)) {
    
    ## calculate variables necessary for Wi calculation ##
    # number of landslide pixels in specific class
    sum_ls_class <- as.numeric(data[[d]]$npix_ls[data[[d]]$class == i])
    # number of pixels in specific class
    sum_class <- as.numeric(data[[d]]$npix_all[data[[d]]$class == i])
    # total number of landslide pixels 
    sum_ls <- as.numeric(sum(data[[d]]$npix_ls, na.rm = TRUE))
    # total number of pixels in the study area (viewshed)
    sum_all <- as.numeric(sum(data[[d]]$npix_all, na.rm = TRUE))
    
    # Calculate Wi with function
    Wi[i] <- fo(sum_ls_class, sum_class, sum_ls, sum_all)
    print(data[[d]]$Wi)
  }
  
  # save Wi큦 ...
  data[[d]]$Wi <- Wi
  # ... and store them the original dataframe
  assign(paste0(name[d]), data[[d]])
  
}

# replace Wi values in classes without landslide pixel by smallest Wi value 
# according to Meinhard et al. (2015)
# only for soil and dem
soil$Wi[is.na(soil$Wi)] <- min(soil$Wi, na.rm = TRUE)
dem$Wi[is.na(dem$Wi)] <- min(dem$Wi, na.rm = TRUE)

# define new list of data including Wi
data <- list(curv, dem, dist_roads, dist_water, 
             landcover, litho, slope, soil, TWetI)




##########################################################
################ Weighting Factor ########################
######## calculating Wf큦 for each parameter #############
##########################################################


### calculate Wf by multipliing Wis with landslide pixel counts of each class ###

# created data frame for Wf
columns = c("parameter","TWI", "Wf")
df_Wf = data.frame(matrix(nrow = length(name), ncol = length(columns)))
colnames(df_Wf) = columns

# create empty lists for parameter names and TWI results
variables <- c()
TWI_values <- c()


# calculate TWI for every Parameter and store the values and names in the empty lists
for (d in 1:length(data)) {
  data[[d]]$multipl <- data[[d]]$npix_ls * data[[d]]$Wi  
  
  TWI <- sum(data[[d]]$multipl, na.rm = TRUE)
  
  variables <- append(variables, name[d])
  TWI_values <- append(TWI_values, TWI)
  
}


# transfer values from lists to Wf dataframe
df_Wf$parameter <- variables
df_Wf$TWI <- TWI_values


# formula for calculating Wf values by stretching TWIs
Wf_fo <- function(TWI){
  ((TWI - min(TWI))/(max(TWI) - min(TWI))) * 99 + 1
}

# apply Wf formula and calculate Wf values for each parameter 
df_Wf$Wf <- Wf_fo(df_Wf$TWI)





##########################################################
################ Statistical Index #######################
######### Apply Wi큦 to each parameter raster ############
####### and overlay rasters to susceptibility map ########
##########################################################


### read all TIF files ###

# directory for tif files (extent of study area)
wd <- setwd("O:/GEO403/GIS/LSModel/StudyArea")

# store file names
temp <- list.files(pattern = "*.tif")

# create list for tif files
tif_list <- list() 

# fill the list with data
for (i in 1:length(temp)){
  tif_list[[i]] <- raster(temp[i])
}


# list for new rasters
recl_list <- list()


### loop for application of Wi to tifs ###

for (d in 1:length(data)) {
  
  # assign Wi values to each class
  recl <- c(1, data[[d]]$Wi[1],
            2, data[[d]]$Wi[2],
            3, data[[d]]$Wi[3],
            4, data[[d]]$Wi[4],
            5, data[[d]]$Wi[5],
            6, data[[d]]$Wi[6],
            7, data[[d]]$Wi[7],
            8, data[[d]]$Wi[8],
            9, data[[d]]$Wi[9],
            10, data[[d]]$Wi[10])
  
  # create reclass matrix
  recl_matrix <- matrix(recl, ncol = 2, byrow = TRUE)
  
  # reclassify via matrix
  recl_list[[d]] <- reclassify(tif_list[[d]], recl_matrix)
  
  # plotting of resulting tif (optional)
  #plot(recl_list[[d]])
  
  # save result into new tif with appropriate name
  writeRaster(recl_list[[d]], 
              paste("O:/GEO403/GIS/LSModel/Wi/", name[d], "_Wi.tif",sep=""))
  
}


### creating susceptibility map by overlaying all Wi raster ###

# read shape to set TWI to right extent
shape <- readOGR("O:/GEO403/GIS/StudyArea_Mask/study_area_mask.shp")

# crop TWI 
TWI_crop <- crop(recl_list[[9]], shape)

# new recl_list 
recl_list <- list(recl_list[[1]], recl_list[[2]], recl_list[[3]], recl_list[[4]],
                 recl_list[[5]],recl_list[[6]],recl_list[[7]],recl_list[[8]], TWI_crop)


# raster stack of single Wi rasters
Wi_stack <- stack(recl_list)

# final susceptibility raster by summing up Wi큦 of each pixel
Wi_raster <- overlay(Wi_stack, na.rm = FALSE, fun = function(a,b,c,d,e,f,x,y,z){return(a+b+c+d+e+f+x+y+z)})
#Wi_raster <- sum(Wi_stack)


# plot the final susceptibility raster
plot(Wi_raster)

# save final susceptibility raster as tif file
writeRaster(Wi_raster, 
            paste("O:/GEO403/GIS/LSModel/Wi/", "Susc_map", "_Wi.tif",sep=""))




##########################################################
################ Weighting Factor ########################
####### Apply Wi*Wf큦 to each parameter raster ###########
####### and overlay rasters to susceptibility map ########
##########################################################


#### preparations for susceptibility map ####

# calculating multiples of Wi and Wf

for (d in 1:length(data)) {
  
  # multiplying Wi and Wf
  data[[d]]$multipl <- data[[d]]$Wi * df_Wf$Wf[df_Wf$parameter == name[d]]
  
  # store results
  assign(paste0(name[d]), data[[d]])
  
}


# list for new rasters
recl_list <- list()


### loop for application of Wf큦 to tifs ###

for (d in 1:length(data)) {
  
  # assign multiples of Wf and Wi큦 to each class
  recl <- c(1, data[[d]]$multipl[1],
            2, data[[d]]$multipl[2],
            3, data[[d]]$multipl[3],
            4, data[[d]]$multipl[4],
            5, data[[d]]$multipl[5],
            6, data[[d]]$multipl[6],
            7, data[[d]]$multipl[7],
            8, data[[d]]$multipl[8],
            9, data[[d]]$multipl[9],
            10, data[[d]]$multipl[10],
            11, data[[d]]$multipl[11])
  
  # create reclass matrix
  recl_matrix <- matrix(recl, ncol = 2, byrow = TRUE)
  
  # reclassify via matrix
  recl_list[[d]] <- reclassify(tif_list[[d]], recl_matrix)
  
  # plotting of resulting tif (optional)
  #plot(recl_list[[d]])
  
  # save result into new tif with appropriate name
  writeRaster(recl_list[[d]], 
              paste("O:/GEO403/GIS/LSModel/Wf/", name[d], "_Wf.tif",sep=""))
  
}


### creating susceptibility map by overlaying all Wf raster ###

# crop TWI 
TWI_crop <- crop(recl_list[[9]], shape)

# new recl_list 
recl_list <- list(recl_list[[1]], recl_list[[2]], recl_list[[3]], recl_list[[4]],
                  recl_list[[5]],recl_list[[6]],recl_list[[7]],recl_list[[8]], TWI_crop)


# raster stack of single Wf rasters
Wf_stack <- stack(recl_list)

# final susceptibility raster by summing up Wf큦 of each pixel
Wf_raster <- overlay(Wf_stack, na.rm = FALSE, fun = function(a,b,c,d,e,f,x,y,z){return(a+b+c+d+e+f+x+y+z)})


# plot the final susceptibility raster
plot(Wf_raster)

# save final susceptibility raster as tif file
writeRaster(Wf_raster, 
            paste("O:/GEO403/GIS/LSModel/Wf/", "Susc_map", "_Wf.tif",sep=""))





### exporting all values ###

# list all data frames for the export
excel_list <- list("curv" = curv, "dem" = dem, "dist_roads" = dist_roads, "dist_water" = dist_water, 
                   "landcover" = landcover, "litho" = litho, "slope" = slope, "soil" = soil, "TWetI" = TWetI, "df_Wf" = df_Wf)

# export list of data frames
write_xlsx(excel_list, "O:/GEO403/GIS/LSModel/Tables/alldata.xlsx")


