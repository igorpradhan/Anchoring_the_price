library(missForest)
# import data files
df_full <- read.csv("finalDFwithNaN.csv", sep = ";")
# replace all true and false values with 0 and 1 in columns 19 to 39
cols <- sapply(df_full, is.character)
df_full[,cols] <- lapply(df_full[,cols], function(x) as.numeric(as.logical(x)))


# replace all NA values in engine hours with 0 if total horse power is also 0 or NA
df$Engine.Hours[is.na(df$Engine.Hours) & (df$Total.Horsepower == 0 | is.na(df$Total.Horsepower))] <- 0

# use missForest to impute engine hours based on age and location
df_to_impute <- df[, c("Age", "Engine.Hours", "Latitude", "Longitude")]
imputed_eh <- missForest(xmis = df_to_impute, maxiter = 10, ntree = 100)
imputed_eh_results <- imputed_eh$ximp
df$Engine.Hours <- imputed_eh_results$Engine.Hours

summary(df)

#impute condition based on age and location
df_to_impute <- df[, c("Age", "Condition", "Latitude", "Longitude")]
imputed <- missForest(xmis = df_to_impute, maxiter = 10, ntree = 100)$ximp
df$Condition <- imputed$Condition

#impute simultaneously draught, displacement, total horsepower taking into
#account length and beam, sailing vessel, motorized vessel, ship area
df_to_impute <- df[, c("Draught", "Displacement", "Total.Horsepower",
                            "Length", "Beam", "Sailing.Vessel",
                            "Motorized.Vessel", "Ship.Area")]
imputed <- missForest(xmis = df_to_impute, maxiter = 10, ntree = 100)$ximp
df$Draught <- imputed$Draught
df$Displacement <- imputed$Displacement
df$Total.Horsepower <- imputed$Total.Horsepower

#set missing image brightness values to 0
df$image.brightness[is.na(df$image.brightness)] <- 0

#set missing values for mainsail.area, jib.area, genoa.area, spinnaker.area,
#gennaker.area to zero if the vessel is not a sailing vessel
df$Mainsail.Area[df$Sailing.Vessel == 0 & is.na(df$Mainsail.Area)] <- 0
df$Jib.Area[df$Sailing.Vessel == 0 & is.na(df$Jib.Area)] <- 0
df$Genoa.Area[df$Sailing.Vessel == 0 & is.na(df$Genoa.Area)] <- 0
df$Spinnaker.Area[df$Sailing.Vessel == 0 & is.na(df$Spinnaker.Area)] <- 0
df$Gennaker.Area[df$Sailing.Vessel == 0 & is.na(df$Gennaker.Area)] <- 0

#impute the rest of the sail area variable based on boat size
df_to_impute <- df[, c("Mainsail.Area", "Jib.Area", "Genoa.Area",
                            "Spinnaker.Area", "Gennaker.Area", "Length", "Beam", "Ship.Area", "Draught")]
imputed <- missForest(xmis = df_to_impute, maxiter = 10, ntree = 100)$ximp
df$Mainsail.Area <- imputed$Mainsail.Area
df$Jib.Area <- imputed$Jib.Area
df$Genoa.Area <- imputed$Genoa.Area
df$Spinnaker.Area <- imputed$Spinnaker.Area
df$Gennaker.Area <- imputed$Gennaker.Area

#set Fuel.Capacity to 0 if the vessel is not a motorized vessel
df$Fuel.Capacity[df$Motorized.Vessel == 0 & is.na(df$Fuel.Capacity)] <- 0

#impute Fuel.Capacity based on Total.Horsepower and Displacement
df_to_impute <- df[, c("Fuel.Capacity", "Total.Horsepower", "Displacement")]
imputed <- missForest(xmis = df_to_impute, maxiter = 10, ntree = 100)$ximp
df$Fuel.Capacity <- imputed$Fuel.Capacity

# impute missing values for all other variables
df <- missForest(xmis = df, maxiter = 10, ntree = 100)$ximp
write.csv(df, file = "DFimputedMan.csv", row.names = FALSE)


