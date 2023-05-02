library(tidyverse)
library(sf)

building_data = read.csv("wui2019.csv")

parking_lots = read_sf("datasets/Parking_Lots/Parking_Lots.shp")
buildings = read_sf("datasets/Building_Footprints/Building_Footprints.shp")

# in "wui2019.csv", I have already manually edited the dataset to assign parking lot names
# to south campus residence halls that they serve.

# Now, I want to compare south campus residence halls to their corresponding parking lot capacity.
# Clean your data to exclude any unnecessary columns and data that are outside south campus.
building_data = filter(building_data, PROPERTY_TYPE %in% c("RESIDENCE HALLS"))

building_data <- building_data[!(building_data$LOT == ""), ]

# If a parking lot serves multiple buildings, then sum the corresponding areas of the buildings.
shared_lots = group_by(building_data, LOT) %>%
  summarize(building_area=sum(SQUAREFEET))

colnames(shared_lots)[1] <- "LOT_NAME"

# Join the parking lot data to the building data.
shared_lots = left_join(shared_lots, parking_lots, by="LOT_NAME")

colnames(shared_lots)[9] <- "lot_area"

# Run a linear regression model to look for any correlation between building area and lot area.
single_variable_model1 = lm(building_area~lot_area, shared_lots)
summary(single_variable_model1)

# The regression summary results in a p value of 0.2, which is higher than 0.05. 
# The R^2 value is low at 0.4604.
# There is no meaningful correlation between residence bulding area and parking lot area. 
# Therefore, there is currently poor allocation of parking spaces, and should be 
# corrected so that the residence hall capacity matches the nearby parking lot capacity.

# Plot the relationship between building area and lot area.
ggplot(shared_lots, aes(x=building_area, y=lot_area, group=1)) +
  geom_point()

# Note: Data limitations
# There was no downloadable data found on the building person capacities or
# the parking lot capacities that specified which were allotted for student use. 
# The building area is just a square footage of the building, and it only
# roughly represents the person capacity of that building. 
# Similarly, the lot area is only a rough representation of the available parking
# capacity.
