
#################################################################
# data exploration landslides
#################################################################

# libraries
library(tidyverse)
library(writexl)
library(dplyr)
library(lattice)
library(ggplot2)

# data
ls <- read.csv("C:/Users/Joel Hauser/Documents/UZH/SEMP/GEO403/ls_sampled.csv", 
               header = TRUE, sep = ";", dec = ",")
nonls <- read.csv("C:/Users/Joel Hauser/Documents/UZH/SEMP/GEO403/non_ls_sampled.csv", 
               header = TRUE, sep = ";", dec = ",")

#### tidying up ####

# remove unnecessary columns
ls <- select(ls, -c(2:25, 35))
nonls <- select(nonls, -c(2, 13))
# alternatively
#ls <- ls[ -c(2:25, 35)]
#nonls <- nonls[ -c(2, 13)]

# rename columns
ls <- ls %>% rename(curvature = curvature,
              plan_curvature = curv_pln,
              profile_curvature = curv_profi,
              slope = dgm_slope1,
              aspect = aspect,
              elevation = fill_dgmq3,
              TWI = Topographi,
              landcover = VGTB_LandC,
              soil = soil1,
              litho = litho1,
              distclass_water = waterriver,
              dist_road = NEAR_DIST)
nonls <- nonls %>% rename(curvature = curvature1,
                    plan_curvature = curv_pln,
                    profile_curvature = curv_profi,
                    slope = dgm_slope1,
                    aspect = aspect,
                    elevation = fill_dgmq3,
                    TWI = Topographi,
                    landcover = VGTB_LandC,
                    soil = soil1,
                    litho = litho1,
                    distclass_water = waterriver,
                    dist_road = NEAR_DIST)

# change column order
ls <- select(ls, FID, is_ls, longitude, latitude, elevation,
            curvature, plan_curvature, profile_curvature,
            slope, aspect, TWI, landcover, soil, litho, 
            distclass_water, dist_road)
nonls <- select(nonls, FID, is_ls, longitude, latitude, elevation,
             curvature, plan_curvature, profile_curvature,
             slope, aspect, TWI, landcover, soil, litho, 
             distclass_water, dist_road)

# combine non-ls and ls dataframe
df <- bind_rows(ls, nonls)

## renaming for plotting

# renaming litho
df$litho2[df$litho == 1] <- "extrusive rock"
df$litho2[df$litho == 2] <- "sedimentary rock"
df$litho2[df$litho == 3] <- "metamorphic rock"
df$litho2[df$litho == 4] <- "intrusive igneous rock"
df$litho2[df$litho == 5] <- "unconsolidated sediment"
df$litho2[df$litho == 6] <- "undefined"

# soil
df$soil2 <- df$soil
df$soil2[df$soil == 1] <- "Undefined"
df$soil2[df$soil == 2] <- "Waterbody"
df$soil2[df$soil == 3] <- "Ferralsol"
df$soil2[df$soil == 4] <- "Fluvisol"
df$soil2[df$soil == 5] <- "Gleysol"
df$soil2[df$soil == 6] <- "Leptosol"
df$soil2[df$soil == 7] <- "Luvisol"
df$soil2[df$soil == 8] <- "Nitisol"
df$soil2[df$soil == 9] <- "Arenosol"
df$soil2[df$soil == 10] <- "Acrisol"
df$soil2[df$soil == 11] <- "Solonchaks"

# landcover
df$landcover2 <- df$landcover
df$landcover2[df$landcover == 1] <- "forest"
df$landcover2[df$landcover == 2] <- "cropland"
df$landcover2[df$landcover == 3] <- "grassland"
df$landcover2[df$landcover == 4] <- "water"
df$landcover2[df$landcover == 5] <- "settlement"
df$landcover2[df$landcover == 6] <- "undefined"
df$landcover2[df$landcover == 9] <- "no data"


# distance to water bodies
df$distclass_water2 <- df$distclass_water
df$distclass_water2[df$distclass_water == 0] <- "is waterbody"
df$distclass_water2[df$distclass_water == 30] <- "< 30 m"
df$distclass_water2[df$distclass_water == 60] <- "< 60 m"
df$distclass_water2[df$distclass_water == 90] <- "< 90 m"
df$distclass_water2[df$distclass_water == 99] <- "> 90 m"

#landslides
df$is_ls2 <- df$is_ls
df$is_ls2[df$is_ls == 1] <- "landslides"
df$is_ls2[df$is_ls == 0] <- "non-landslides"


# export dataframe
write_xlsx(df, "~/Geoinformatik_MSc/WiSe_21_22/GEO_403/Daten/all_ls.xlsx", col_names = TRUE)
write.csv(df, "~/Geoinformatik_MSc/WiSe_21_22/GEO_403/Daten/all_ls.csv", row.names = FALSE)


#### exploratory analysis ####

# boxplot for quantitative variables
par(mfrow = c(1,1))

boxplot(data = df, slope ~ is_ls, names = c("non-landslides", "landslides"),
        main = "", xlab = "", ylab = "slope angle [°]")

boxplot(data = df, aspect ~ is_ls, names = c("non-landslides", "landslides"),
        main = "", xlab = "", ylab = "aspect [°]")
title("aspect [degree]", line = 0.5, adj = 0)

boxplot(data = df, elevation ~ is_ls, names = c("non-landslides", "landslides"),
        main = "", xlab = "", ylab = "elevation [m] ")
title("elevation [m]", line = 0.5, adj = 0)

boxplot(data = df, plan_curvature ~ is_ls, names = c("non-landslides", "landslides"),
        main = "", xlab = "", ylab = "plan curvature")
title("plan curvature", line = 0.5, adj = 0)

boxplot(data = df, profile_curvature ~ is_ls, names = c("non-landslides", "landslides"),
        main = "", xlab = "", ylab = "profile curvature")
title("profile curvature", line = 0.5, adj = 0)

boxplot(data = df, TWI ~ is_ls, names = c("non-landslides", "landslides"),
        main = "", xlab = "", ylab = "SAGA Wetness Index")
title("TWI", line = 0.5, adj = 0)

boxplot(data = df, dist_road ~ is_ls, names = c("non-landslides", "landslides"),
        main = "", xlab = "", ylab = "distance to roads [m]")
title("distance to roads [m]", line = 0.5, adj = 0)


# t-test
t.test(df$slope ~ df$is_ls, alternative = "two.sided", conf.level = 0.95)
t.test(df$aspect ~ df$is_ls, alternative = "two.sided", conf.level = 0.95)
t.test(df$elevation ~ df$is_ls, alternative = "two.sided", conf.level = 0.95)
t.test(df$plan_curvature ~ df$is_ls, alternative = "two.sided", conf.level = 0.95)
t.test(df$profile_curvature ~ df$is_ls, alternative = "two.sided", conf.level = 0.95)
t.test(df$TWI ~ df$is_ls, alternative = "two.sided", conf.level = 0.95)
t.test(df$dist_road ~ df$is_ls, alternative = "two.sided", conf.level = 0.95)


## Barplots

# landcover vs ls

counts <- table(df$is_ls, df$landcover2)

barplot(counts, col = c("skyblue3", "skyblue1"),
        legend = FALSE, beside = TRUE)

legend("topright",  c("non-landslides", "landslides"), fill = c("skyblue3", "skyblue1"),
       bty = "n", inset = c(0.0, 0))

# lithography vs ls

counts <- table(df$is_ls, df$litho2)

barplot(counts, col = c("skyblue3", "skyblue1"),
        legend = FALSE, beside = TRUE)

legend("topright",  c("non-landslides", "landslides"), fill = c("skyblue3", "skyblue1"),
       bty = "n", inset = c(0.0, 0))

# soil vs ls

counts <- table(df$is_ls, df$soil2)

barplot(counts, col = c("skyblue3", "skyblue1"),
        legend = FALSE, beside = TRUE)

legend("topright",  c("non-landslides", "landslides"), fill = c("skyblue3", "skyblue1"),
       bty = "n", inset = c(0.0, 0))

# distance to waterbody vs ls

counts <- table(df$is_ls, df$distclass_water2)

barplot(counts, col = c("skyblue3", "skyblue1"),
        legend = FALSE, beside = TRUE)
legend("topright",  c("non-landslides", "landslides"), fill = c("skyblue3", "skyblue1"),
       bty = "n", inset = c(0.0, 0))


barplot(counts, col = c("skyblue3", "skyblue1"),
        legend = TRUE, beside = TRUE)




