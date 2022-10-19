################################################################################
#
#  This code analyzes:
# 1) Percent of reclassed Key Habitats within NV
# 2) SClass (succession class) departures for Landfire
#  biophysical settings (BpS) reference condition vs. present
# 3)  % of each Key Habitat converted to other than natural veg
# 4) sclass differences within class at BpS level
#
# REQUIRED:
# 1)spatially joined tabular data for bps and sclass rasters including
# cell count by class and bps code
# 2)sclass reference table from LandFire
# 3)sclss labels from LandFire
#
################################################################################

library(tidyverse)
library(ggplot2)
library(gt) #for creating nice tables
library(here)

#get working drive/relative project path
here()
options(scipen=999) #disable scientific notation

################################################################################
# Read data
################################################################################

#joined sclass and bps tabular data from spatial join of rasters
sclassDat <-read.csv(here("data","bps_by_sclass_raw.csv"), fileEncoding="UTF-8-BOM")

#sclass labels
labelDat <- read.csv(here("data","LF16_SCla_200.csv"), fileEncoding="UTF-8-BOM")

#slcass reference conditions
refDat <- read.csv(here("data","LANDFIRE_CONUS_Reference_Condition_Table_August_2020.csv"), fileEncoding="UTF-8-BOM")



################################################################################
# Munging
################################################################################
#<-replace missing bps_models with bps_code for perennial ice/snow, barren-rock/sand/clay, open water
sclassDat$BPS_MODEL <- ifelse(is.na(sclassDat$BPS_MODEL), sclassDat$BPS_CODE, sclassDat$BPS_MODEL)
#sclassDat$ZONE <- ifelse(is.na(sclassDat$ZONE), "1" , sclassDat$ZONE) #<-replace NAs with dummy '1'
head(sclassDat)

#Step 1: Remove columns not needed for analysis
sclassDat <- sclassDat %>%
  select(!c(OID_, Value, lf_bps_nv, ZONE))
#view(sclassDat)

#Format reference condition data
#Remove fire return interval columns
refDat <- refDat%>%
  select(!(FRI_ReplacementFire:FRG_New))

#Add unnatural and converted class columns with zeros
# convert to long format
refDatTall<-  refDat%>%
  add_column(UN=0, UE=0, Water=0, "Snow/Ice" =0, Urban=0, "Barren or Sparse" = 0, Agriculture = 0) %>%
  gather(ClassA_ReferencePercent:Agriculture, key=SClass, value=sClassPercent) %>%
  arrange(Model_Code)
head(refDatTall, n=12)

#Rename SClass row labels to match LF16_SCla_200.csv (class labels) format for later joins
refDatTall$SClass <- recode(refDatTall$SClass, ClassA_ReferencePercent="A", ClassB_ReferencePercent="B", ClassC_ReferencePercent="C",
                            ClassD_ReferencePercent="D", ClassE_ReferencePercent="E")
head(refDatTall, n=12)

#Replace NAs in sClassPercent with zeroes
refDatTall$sClassPercent[is.na(refDatTall$sClassPercent)] = 0
head(refDatTall, n=12)


################################################################################
# Calculate percent of NV represented by each Key Habitat
################################################################################
#Calculate the total cell count for each key habitat, push to new df; use later
#for (bps count)/(key hab count) = % bps in key hab
totalKeyCountDat <- sclassDat %>%
  group_by(KEYHABCLASS_2022)%>%
  summarise(totalKeyCount=sum(Count))
view(totalKeyCountDat)

totalKeyCountDat <- mutate(totalKeyCountDat, Percent=(totalKeyCount/sum(totalKeyCount)*100))

#Total area of NV from BpS counts
#900 m^2 x 0.222395acres * 318168938pixels = 70759181
totalKeyCountDat <- mutate(totalKeyCountDat, Acres=((Percent/100)*70759181)) #<-little 'p' percent column!
sum(totalKeyCountDat$Acres)
head(totalKeyCountDat)

#Create graphic
ggplot(totalKeyCountDat, aes(x = reorder(KEYHABCLASS_2022, Percent), y = Percent)) +
  geom_bar(stat = "identity", fill = "#8da69e") +
  coord_flip() +
  labs(
    title = "Key Habitats of Nevada",
    #subtitle = "Grouped, and names shortened.  See table below.",
    caption = "Data from the LandFire Program",
    x = "Key Habitat",
    y = "Percentage" ) +
  theme_minimal(base_size = 16) +
  theme(panel.grid = element_blank())

#Save graphic
ggsave(here("output","percentNVKeyHab.png"),width=10,height=8,units='in',dpi=300)

#Create table
totalKeyCountDat$Percent <- round(totalKeyCountDat$Percent, digits=2)
tableKeyHab <- totalKeyCountDat %>%
  gt() %>%
  tab_header (
    title="Percent of Nevada Represented by Key Habitats"
  )  %>%
  cols_label(
    KEYHABCLASS_2022 = "Key Habitat"
  )
tableKeyHab %>%
  gtsave(here("output","tableKeyHab.rtf"))

################################################################################
# Calculate the percent of each key habitat converted to non-natural vegetation,
# agricultural use, urban, and barren or sparse
################################################################################

#Step 1: assign A-E to natural vegetation, UN&UE to non-natural vegetation
convDat <-sclassFull
convDat$Now <- recode(convDat$SClass, A="NaturalVeg", B="NaturalVeg", C="NaturalVeg",
                      D="NaturalVeg", E="NaturalVeg", UN="NonNaturalVeg", UE="NonNaturalVeg")
head(convDat, n=12)
#view(convDat)

# Step 2: sum percent of each key habitat in new classes, push to new df
#sclassGrouped <- subset(sclassFull,SClass!="Urban" &SClass!="Agriculture")
convDatGrouped <- group_by(convDat, Model_Code, KEYHABCLASS_2022, Now)
converted <- summarise(convDatGrouped, percent = sum(sClassPercentCurrent))

#view(converted)

write.csv(converted, "output/convertedKeyHabitat.csv")

################################################################################
# Calculate sclass % for bps model, bps, and key habitat; total cell count at bps level
################################################################################
#Step 1: Calculate count of cells for each s_class and total cells by BPS wi/in each key habitat
#Sum cell count across zones on s_class w/in BPS_MODEL w/in KEYHABCLASS
sclassCountDat <- sclassDat %>%
  group_by(s_class, KEYHABCLASS_2022, BPS_MODEL, BPS_NAME, BPS_CODE)%>%
  summarize(Count=sum(Count)) %>%
  ungroup()
#view(sclassCountDat)

#Complete df by adding zeros in for s_class w/in BPS_MODEL with missing values
sclassCountDat <- complete(sclassCountDat, nesting(KEYHABCLASS_2022,BPS_MODEL, BPS_NAME, BPS_CODE) ,s_class, fill = list(Count=0))
#view(sclassCountDat)

#Calculate total cell count for each BpS w/in each key habitat
totalBpsCountDat <- sclassDat %>%
  group_by(KEYHABCLASS_2022, BPS_MODEL)%>%
  summarise(totalBpsCount=sum(Count))
head(totalBpsCountDat)

#Join total cell count for each BpS model w/in each key hab from totalBpsCountDat
sclassCountDat <- left_join(sclassCountDat, select(totalBpsCountDat,c(KEYHABCLASS_2022, BPS_MODEL, totalBpsCount)) , by=c("KEYHABCLASS_2022", "BPS_MODEL"))
#view(sclassCountDat)

#drop any BpS without a stsm
sclassCountDat <- subset(sclassCountDat, !BPS_MODEL %in% c(31,12,11, 10010,10020,10040,10060))

#Step 2: Calculate the current % of each sclass for each BpS Model w/in each KeyHab
sclassCountDat$sClassPercentCurrent <- (sclassCountDat$Count / sclassCountDat$totalBpsCount) *100
#view(sclassCountDat)

#Step 3: Calculate % current and % historic sclass for each BpS
#Join sclass label to current sclass
sclassCountDat$SClass <- labelDat$LABEL[match(sclassCountDat$s_class, labelDat$VALUE)]
#view(sclassCountDat)

#change BPS_MODEL to Model_Code in sclassCurrent to match reference data
sclassCountDat <- sclassCountDat %>%
  dplyr::rename(Model_Code = BPS_MODEL)

#CREATE sclassFull DF with current & historic sclass percents by BpS Model_Code
#join historic sclass to current to create df sclassFull
sclassFull <- left_join(sclassCountDat, select(refDatTall,c("Model_Code", "SClass", "sClassPercent")) , by=c("Model_Code", "SClass"))
#view(sclassFull)
names(sclassFull)

#Re-order and rename sclass columns to clarify historic and current
sclassFull <- sclassFull %>%
  select(Model_Code, BPS_NAME, KEYHABCLASS_2022, s_class, SClass, Count, totalBpsCount,
         sClassPercent, sClassPercentCurrent)
sclassFull <- sclassFull %>%
  dplyr::rename(sClassHistPercent = sClassPercent)




