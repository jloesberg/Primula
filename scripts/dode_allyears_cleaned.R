#####Putting Demography data into one file with pflower, psurvival...dealing with problem tags and dormancy)###

library(tidyverse)
################################################################################
#Eventually to do

##### - dormany problem - make reproducible and add 2022 plants


################################################################################
#### 2016: ####
dode2016 <- read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/2016_Dodecatheon_Demography_Data.csv", header = T)

#changed tags:
dode2016$tag[dode2016$tag==346]<-551 #new tag in 2017
dode2016$tag[dode2016$tag==202]<-1057 #new tag in 2019
dode2016$tag[dode2016$tag==542]<-1054 #new tag in 2019

#changing formats:
dode2016 <- dode2016 %>% 
  mutate(tag = as.character(tag),
        plot = as.factor(plot),
        YrTag = as.numeric(YrTag),
        Xcoor = as.character(Xcoor))
dode2016 <- dode2016 %>% 
  mutate(psurvival = 1,
         pflower = 1)

############################################################################
####2017:####
dode2017<-read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/2017_Dodecatheon_Demography_Data.csv", header=T)

dode2017$pflower<-ifelse(dode2017$no.flowers > 0, 1, 0)
dode2017$psurvival <- ifelse(dode2017$leaves > 0, 1, 0) #no NP/NT's, so no NA's for no. leaves
dode2017[dode2017$tag == 920, "pflower"] <- 1 #tag 920 flowered but was eaten...need to change to pflower = 1

dode2017 <- dode2017 %>% 
  mutate(plot = as.factor(plot),
         tag = as.character(tag))

#changed tags
dode2017$tag[dode2017$tag==202]<-1057 #new tag in 2019
dode2017$tag[dode2017$tag==542]<-1054 #new tag in 2019
dode2017$tag[dode2017$tag==70]<-1734 #new tag in 2019
#965 was recorded twice in 2017, but only found in plot 6 in sudsequent years. Taking out the plot 5 tag 965, even though it has measurements:
dode2017 <- subset(dode2017, plot != "5" | tag != "965")

#correct format
dode2017 <- dode2017 %>% 
  mutate(plot = as.factor(plot),
         tag = as.character(tag),
         Xcoor = as.character(Xcoor))

################################################################################
####2018:####
dode2018<-read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/2018_Dodecatheon_Demography_Data.csv", header=T)

dode2018$pflower<-ifelse(dode2018$no.flowers > 0, 1, 0)
#more plants that were eaten and not showing up as pflower = 1: must be a better way to do it, but this is what I came up with:
dode2018[dode2018$tag == 928, "pflower"] <- 1
dode2018[dode2018$tag == 210, "pflower"] <- 1
dode2018[dode2018$tag == 927, "pflower"] <- 1
dode2018[dode2018$tag == 577, "pflower"] <- 1
dode2018[dode2018$tag == 703, "pflower"] <- 1
dode2018[dode2018$tag == 798, "pflower"] <- 1
dode2018[dode2018$tag == 951, "pflower"] <- 1
dode2018$psurvival <- ifelse(dode2018$leaves > 0, 1, 0) #NP/NT are NA's

dode2018 <- dode2018 %>% 
  mutate(plot = as.factor(plot),
         tag = as.character(tag))

#replaced tags
dode2018$tag[dode2018$tag==202]<-1057 #new tag in 2019
dode2018$tag[dode2018$tag==542]<-1054 #new tag in 2019
dode2018$tag[dode2018$tag==70]<-1734 #new tag in 2019

# 147 was found to be 1477 in 2019:
dode2018$tag[dode2018$tag=="147"]<-"1477"
# 1742 in 2018 was found as 1724 in 2019 (but neither found since)
dode2018$tag[dode2018$tag=="1742"]<-"1724"

#965 was recorded twice in 2017, but only found in plot 6 in sudsequent years. Taking out the plot 5 tag 965
dode2018 <- subset(dode2018, plot != "5" | tag != "965")

#correct format
dode2018 <- dode2018 %>% 
  mutate(plot = as.factor(plot),
         tag = as.character(tag),
         Xcoor = as.character(Xcoor))

################################################################################
#### 2019:####
dode2019<-read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/2019_Dodecatheon_Demography_Data.csv", header=T)

#we collected pflower in the field - great!
dode2019$psurvival <- ifelse(dode2019$leaves > 0, 1, 0)
#Tag 551: no # leaves but has LxW, so changing to psurvival = 1
dode2019[dode2019$tag == 551, "psurvival"] <- 1


#fixing trouble tags
###1294 is in plot 1 not plot 6
dode2019$plot[dode2019$tag == "1294"] <- "1"
###1283 is in plot 3 not 2
dode2019$plot[dode2019$tag == "1283"] <- "3"

#1728 is in plot 5 not 3
dode2019$plot[dode2019$tag == "1728"] <- "5"
#1645 is in plot 5 not 3
dode2019$plot[dode2019$tag == "1645"] <- "5"


#1270 and 1946:  1946 tagged in 2018 in plot 11 16J, given a new tag (1270) in 2019. 1946 found in 2020, but entered incorrectly in plot 9 (is in plot 11), but was in 5J. 1270 NS (fixed above)
#1946 not found in 2021, but 1270 was found, in 16J
#2022: 1270 found in 16J, 1946 found in 5J
# seems like these are separate plants and 1270 should have been new in 2019
dode2019$YrTag[dode2019$tag == "1270"] <- "2019"
dode2019$notes[dode2019$tag == "1270"] <- "not a replacement for 1946"
#adding 1946 as a NPNT
dode2019 <- dode2019 %>% add_row(year = 2019, plot = "11", Xcoor = 5, Ycoor = "J", tag = 1946, notes = "NPNT, different than 1270", YrTag = "2018")


### tag 35 in 2019 didnt have an entry for pflower or no.flowers, I'm not certain whether it flowered or not. Leave it in?
#dode2019 <- dode2019 %>% filter(tag != "35") 

dode2019 <- dode2019 %>% 
  mutate(tag = as.character(tag),
         plot = as.factor(plot))

#965 was recorded twice in 2017, but only found in plot 6 in sudsequent years. Taking out the plot 5 tag 965
dode2019 <- subset(dode2019, plot != "5" | tag != "965")

#tag 35 this year had pflower adn no flowers black - taking it out for now - note that this might mean something for dormancy at some point! it was alive!
dode2019 <- subset(dode2019, tag != "35")

dode2019 <- dode2019 %>% mutate(YrTag = as.numeric(YrTag),
                                Xcoor = as.character(Xcoor))

################################################################################
### #2020:####
dode2020<-read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/2020_Dodecatheon_Demography_Data.csv", header=T)

#changing trouble tags
###1294 is in plot 1 not plot 6
dode2020 <- subset(dode2020, plot != "6" | tag != "1294")
#965 was recorded twice in 2017, but only found in plot 6 in sudsequent years. Taking out the plot 5 tag 965
dode2020 <- subset(dode2020, plot != "5" | tag != "965")
###1283 is in plot 3 not 2
dode2020 <- subset(dode2020, plot != "2" | tag != "1283")
dode2020$YrTag[dode2020$tag == "1283"] <- "2019" #was tagged in 2019
#1728 is in plot 5 not 3
dode2020$plot[dode2020$tag == "1728"] <- "5"
#1645 is in plot 5 not 3
dode2020$plot[dode2020$tag == "1645"] <- "5"

#1946 was written down in plot 9 but it is in plot 11. Was tagged in 2018
dode2020$plot[dode2020$tag == "1946"] <- "11"
dode2020$YrTag[dode2020$tag == "1946"] <- "2018"


#putting into correct format
dode2020 <- dode2020 %>% 
  mutate(tag = as.character(tag),
         plot = as.factor(plot),
         YrTag = as.numeric(YrTag),
         Xcoor = as.character(Xcoor),
         psurvival = ifelse(leaves == 0 & is.na(rosetteL), 0, 1))


###################################################################################
#2021 data
dode2021<-read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/2021_Dodecatheon_Demography_Data.csv", header=T)


#dealing with problem tags
###1294 is in plot 1, not 6
dode2021 <- subset(dode2021, plot != "6" | tag != "1294")
#965 was recorded twice in 2017, but only found in plot 6 in sudsequent years. Taking out the plot 5 tag 965
dode2021 <- subset(dode2021, plot != "5" | tag != "965")


###1283 is in plot 3 not 2
dode2021 <- subset(dode2021, plot != "2" | tag != "1283")
 
# 1728 is in plot 5 not 3 (just take out NPNT one in plot 3) and change year to to 2019
dode2021 <- subset(dode2021, plot != "3" | tag != "1728")
dode2021$YrTag[dode2021$tag == "1728"] <- "2019"

# 852 and 1471: 1471 was a replacement for 852 in 2021, but found both in 2022 and they are separate plants
# changing notes: 
dode2021$notes[dode2021$tag == "1471"] <- "NA"
dode2021$notes[dode2021$tag == "852"] <- "NPNT"

# 1754 was replaced and was recorded under 8307 this year
dode2021 <- subset(dode2021, tag != "1754")
#change 8307 to tagged in 2018
dode2021$YrTag[dode2021$tag == "8307"] <- "2018"
# 805 was replaced and was recorded under 8315 this year
dode2021 <- subset(dode2021, tag != "805")
#change 8315 to tagged in 2017
dode2021$YrTag[dode2021$tag == "8315"] <- "2017"

# 864 was replaced and was recorded under 8312 this year
dode2021 <- subset(dode2021, tag != "864")
#change 8315 to tagged in 2017
dode2021$YrTag[dode2021$tag == "8312"] <- "2017"

# #81 replaced by 1881
dode2021 <- subset(dode2021, tag != "81")
#change 1881 to tagged in 2016
dode2021$YrTag[dode2021$tag == "1881"] <- "2016"

# 562 replaced by 1183
dode2021 <- subset(dode2021, tag != "562")
#change 1183 to tagged in 2016
dode2021$YrTag[dode2021$tag == "1183"] <- "2016"

#214 replaced by 8317
dode2021 <- subset(dode2021, tag != "214")
#change 8317 to tagged in 2017 (tag found from last year in 2018)
dode2021$YrTag[dode2021$tag == "8317"] <- "2017"

#49 repleaced by 8319
dode2021 <- subset(dode2021, tag != "49")
#change 8319 to tagged in 2017
dode2021$YrTag[dode2021$tag == "8319"] <- "2017"

#215 replaced by 8311
dode2021 <- subset(dode2021, tag != "215")
#change 8311 to tagged in 2017
dode2021$YrTag[dode2021$tag == "8311"] <- "2017"


#1645 (incorrectly entered as in plot 3 - it is in plot 5) was "replaced" by 1579 in plot 5
# 1645 remove the plot 3 one (NPNT)
dode2021 <- subset(dode2021, plot != "3" | tag != "1645")
# 1645 was found in 2019 in plot:
dode2021$YrTag[dode2021$tag == "1645"] <- "2019" #was tagged in 2019
### 1645 is not in plot 3, so 1579 is just new in 2021
#change 1579 note bc not new tag for 1645
dode2021$notes[dode2021$tag == "1579"] <- "NA"

#1946 was written down in plot 9 but it is in plot 11 in 2020. Was tagged in 2018
dode2021$plot[dode2021$tag == "1946"] <- "11"
dode2021$YrTag[dode2021$tag == "1946"] <- "2018"

# there's a problem with 955 and 995, and its not clear in the data sheets which goes where. We definitely found 995 in plot 5 though, not plot 3 in 2021. NPNT for 955 in 2021. In 2019 something happened (one "was wrong last year") but there was demography for both tags
# 995 was new in 2019 - it was incorrectly written as plot 3, but it is in plot 5 (found in 2021, also happened for 1728 and 1645)
# 955 was new in 2017, has always been in plot 5
dode2021 <- subset(dode2021, plot != "3" | tag != "995") #taking out the plot 3 995 for 2021
dode2021$notes[dode2021$tag == "995"] <- "tag in plot 5"

#1498 replaced by 1452 ###
### 2022: found the original tag 1498, and pulled 1452 - were pointing at same plant
dode2021 <- subset(dode2021, tag != "1498")
#change 1498 to tagged in 2019
dode2021$tag[dode2021$tag == "1452"] <- "1498"
dode2021$notes[dode2021$tag == "1498"] <- "tag found in 2022, not replaced by 1452"

#1076 is definitely seperate from 1210 (2022), and is in 2B not 12B
dode2021$notes[dode2021$tag == "1076"] <- "in 2B. seperate from 1210"
dode2021$notes[dode2021$tag == "1210"] <- "NP/NT"
dode2021$Xcoor[dode2021$tag == "1076"] <- 2

dode2021 <- dode2021 %>% 
  mutate(plot = as.factor(plot),
         tag = as.character(tag),
         YrTag = as.numeric(YrTag),
         Xcoor = as.character(Xcoor),
         psurvival = ifelse(leaves == 0 & is.na(rosetteL), 0, 1))


###################################################################################
#2022 data
dode2022<-read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/2022_Dodecatheon_Demography_Data.csv", header=T)


#dealing with problem tags

#	Found original tag (1498), replacement tag (1452) pulled. Changed them in 2021, in 2022 csv: take out
### 1452
dode2022 <- subset(dode2022, tag != "1452")
dode2022$problem.tag[dode2022$tag == "1498"] <- 0

# 1283 is in plot 3 not 2
dode2022 <- subset(dode2022, plot != "2" | tag != "1283")

# tag 995 and 955 are both in plot 5 (see note above in 2021 section)
dode2022 <- subset(dode2022, plot != "3" | tag != "995") #taking out the plot 3 995 for 2022
dode2022$notes[dode2022$tag == "995"] <- "tag in plot 5"
dode2022$problem.tag[dode2022$tag == "995"] <- 0

# 1728 is in plot 5 not 3 (just take out NPNT one in plot 3) and change year tg to 2019
dode2022 <- subset(dode2022, plot != "3" | tag != "1728")
dode2022$YrTag[dode2022$tag == "1728"] <- "2019"
#1579 was "new" tag for 1728 in plot 3 (but 1728 is not in plot 3, so its just a new tag)

#1645 (incorrectly entered as in plot 3 in 2019 - it is in plot 5) was "replaced" by 1579 in plot 5
# 1645 remove the plot 3 one (NPNT)
dode2022 <- subset(dode2022, plot != "3" | tag != "1645")
# 1645 was found in 2019 in plot:
dode2022$YrTag[dode2022$tag == "1645"] <- "2019" #was tagged in 2019
### 1645 is not in plot 3, so 1579 is just new in 2021
#change 1579 note bc not new tag for 1645
dode2022$notes[dode2022$tag == "1579"] <- ""
dode2022$problem.tag[dode2022$tag == "1579"] <- 0

# 716 is in 8G not 9B - changed below
dode2022$problem.tag[dode2022$tag == "716"] <- 0

#965 was recorded twice in 2017, but only found in plot 6 in subsequent years. Taking out the plot 5 tag 965
dode2022 <- subset(dode2022, plot != "5" | tag != "965")

# 852 and 1471: 1471 was a replacement for 852 in 2021, but found both in 2022 and they are separate plants
# changed them in 2019
dode2022$problem.tag[dode2022$tag == "852"] <- 0

# 1210 and 1076: these are definitely separate plants. But, 1076 is 2B not 12B
dode2022$Xcoor[dode2022$tag == "1076"] <- 2
dode2022$problem.tag[dode2022$tag == "1076"] <- 0
dode2022$notes[dode2022$tag == "1076"] <- "seperate from 1210"
dode2022$problem.tag[dode2022$tag == "1210"] <- 0

#1937 is actually 1397
dode2022$tag[dode2022$tag == "1937"] <- "1397"
dode2022$problem.tag[dode2022$tag == "1397"] <- 0

###1294 is in plot 1, not 6
dode2022 <- subset(dode2022, plot != "6" | tag != "1294")

#1754 and 8307 - 8307 was new tag for it in 1754
dode2022 <- subset(dode2022, tag != "1754")
#change 8307 to tagged in 2018
dode2022$YrTag[dode2022$tag == "8307"] <- "2018"
dode2022$notes[dode2022$tag == "8307"] <- "old tag was 1754"
dode2022$problem.tag[dode2022$tag == "8307"] <- 0

# 864 was replaced and was recorded under 8312 this year
dode2022 <- subset(dode2022, tag != "864")
#change 8312 to tagged in 2017
dode2022$YrTag[dode2022$tag == "8312"] <- "2017"
dode2022$problem.tag[dode2022$tag == "8312"] <- 0

#552 coordinates are 15D - solved below
dode2022$problem.tag[dode2022$tag == "552"] <- 0

#1270 and 1946:  1946 tagged in 2018 in plot 11 16J, given a new tag (1270) in 2019. 1946 found in 2020, but entered incorrectly in plot 9 (is in plot 11), but was in 5J. 1270 NS (fixed above)
#1946 not found in 2021, but 1270 was found, in 16J
#2022: 1270 found in 16J, 1946 found in 5J
# seems like these are separate plants and 1270 should have been new in 2019  (done above)
# 1946 remove the plot 9 one (NPNT)
dode2022 <- subset(dode2022, plot != "9" | tag != "1946")
dode2022$notes[dode2022$tag == "1946"] <- ""
dode2022$problem.tag[dode2022$tag == "1946"] <- 0
dode2022$problem.tag[dode2022$tag == "1270"] <- 0
#1946 was tagged in 2018
dode2022$YrTag[dode2022$tag == "1946"] <- "2018"

#making a clean df for next year's datasheet
### taking out pulled tags
dode2022clean <- dode2022 %>% filter(tag.pulled != "1") %>% 
  select(-c(tag.pulled, problem.tag, toothpick))
#write.csv(dode2022clean, "/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData/Dodecatheon/Cleaned (use for future datasheets)/2022_Dodecatheon_Demography_Data_CLEANED.csv",row.names = F)
remove(dode2022clean)


dode2022 <- dode2022 %>% 
  mutate(plot = as.factor(plot),
         tag = as.character(tag),
         YrTag = as.numeric(YrTag),
         Xcoor = as.character(Xcoor),
         psurvival = ifelse(leaves == 0 & is.na(rosetteL), 0, 1))


################################################################################
#Putting it all together:
Dodecatheon<-bind_rows(dode2016, dode2017, dode2018, dode2019, dode2020, dode2021, dode2022) 
# it gives warnings, but looks like everything is there!

# in 2021, replaced some tags. Doing this in the big df:
# 625 was replaced with 1585
Dodecatheon$tag[Dodecatheon$tag == "625"] <- "1585"

# 1754 was replaced by 8307
Dodecatheon$tag[Dodecatheon$tag == "1754"] <- "8307"

# 805 was replaced by 8315
Dodecatheon$tag[Dodecatheon$tag == "805"] <- "8315"

# 864 was replaced by 8312
Dodecatheon$tag[Dodecatheon$tag == "864"] <- "8312"

#81 replaced by 1881
Dodecatheon$tag[Dodecatheon$tag == "81"] <- "1881"

# 562 replaced by 1183
Dodecatheon$tag[Dodecatheon$tag == "562"] <- "1183"

#214 replaced by 8317
Dodecatheon$tag[Dodecatheon$tag == "214"] <- "8317"

#49 repleaced by 8319
Dodecatheon$tag[Dodecatheon$tag == "49"] <- "8319"

#215 replaced by 8311
Dodecatheon$tag[Dodecatheon$tag == "215"] <- "8311"

#852 replaced by 1471
Dodecatheon$tag[Dodecatheon$tag == "852"] <- "1471"

#1937 is actually 1397
Dodecatheon$tag[Dodecatheon$tag == "1937"] <- "1397"

#715 was tagged in 2017 not 2018
Dodecatheon$YrTag[Dodecatheon$tag == "715"] <- "2017"

#523 was found in 2016
Dodecatheon$YrTag[Dodecatheon$tag == "523"] <- "2016"

#1270 was new in 2019
Dodecatheon$YrTag[Dodecatheon$tag == "1270"] <- "2019"
#1946 was tagged in 2018
Dodecatheon$YrTag[Dodecatheon$tag == "1946"] <- "2018"


# 716 is in 8G not 9B
Dodecatheon$Xcoor[Dodecatheon$tag == "716"] <- 8
Dodecatheon$Ycoor[Dodecatheon$tag == "716"] <- "G"

#coordinates for 552 are 15D (not written when tagged)
Dodecatheon$Xcoor[Dodecatheon$tag == "552"] <- 15
Dodecatheon$Ycoor[Dodecatheon$tag == "552"] <- "D"
 

#adding ros.area...radius*radius*pi
Dodecatheon$ros.area<-Dodecatheon$rosetteL/2*Dodecatheon$rosetteW/2*(pi)
################################################################################
#adding treatment
#read in data that assign treatment to plot
plots<-read.csv("/Users/Jenna/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot Info/IDE_plotinfo.csv", header=T)

plots <- plots %>% 
  mutate(plot = as.character(plot))

Dodecatheon<-left_join(Dodecatheon, plots, by="plot") #join so that every record also belongs to a plot

##################################################################################
# other problem tags:


################################################################################
#To check for duplicates:
which(duplicated(dode2019$tag))
which(duplicated(dode2018$tag)) 
which(duplicated(dode2020$tag))
which(duplicated(dode2017$tag))
which(duplicated(dode2021$tag))
which(duplicated(dode2022$tag))

################################################################################
################################################################################
#dont need these anymore:
remove(dode2016, dode2017, dode2018, dode2019, dode2020, dode2021, plots)

# 2020 problems:
#146 = solved
#715 = solved

#1283 = duplicate maybe - check in 2021
#1271 = solved
#1800 = solved
#799 = solved
#569 = solved
#965 = duplicate maybe - check in 2021
#855 = solved
#738 = solved 
#523 = solved 
#183 = solved
#1998 = solved
#1734 = solved
#934 = solved
#372/976 = solved 

###############################################################################
########################### DORMANT PLANTS ####################################
###############################################################################

# ## need to redo what I did in 2020 to make 2021 plants dormant. This will be a chore with the NS plants in 2020. I  dont want to use the excel method...let's do it in this script so it's reproducible moving forward:
# this makes a wide table like the one I made in excel before
# Yr Tag does ultimately matter for the dormant table - for this can't include it!
dormant <- Dodecatheon %>%
  mutate(tag = as.character(tag),
         year = as.factor(year)) %>%
  select(tag, year, psurvival) %>%
  dplyr::group_by(tag) %>%
 pivot_wider(names_from = "year", values_from = "psurvival") #%>% 
  #mutate(years.above = rowSums(across(where(is.numeric)), na.rm = T))

# this is pretty atrocious, but can't think of another way!
# First, open that csv in excel. Then, go through by hand and assign plants stages in each year - future Jenna will have
# to do it over again in next years
## there is a problem with this though - can really only tell survival for plants up to 2019 - can only assign plants as dead
## when they have been underground for two consecutive years. With 2020's plants mostly as NA's this is quite hard

#write.csv(dormant, "./data/dormant.table.csv")

dtable <- read.csv("./data/dormant.table.as.stages.csv")
dtable <- dtable %>% select(c(tag, X2016, X2017, X2018, X2019, X2020, X2021)) %>% # now pivot longer:
  pivot_longer(!tag, names_to = "year", values_to = "state", names_prefix = "X")


#adding life stages
#V = vegetative, F = flowering, U = Underground/Dormant, D = dead..but for now leaving it as NA. Havent decided when a plant is dead vs when its still dormant AND what to do with unsurveyed plants. Hmm...
Dodecatheon <- Dodecatheon %>% 
  mutate(life_st = if_else(pflower == "1", "F", #if it flowered, its flowering
                           if_else(psurvival == "1" & pflower == "0", "V", #if its above ground but not flowering = vegetative
                                   if_else(psurvival == "0" & pflower == "0", "U", "D"))),
         year = as.character(year)) 
  
dor_fates <- left_join(Dodecatheon, dtable, by = c("tag", "year"))
dor_fates$life_st[dor_fates$state == "dormant"] <- "dormant"
dor_fates$life_st[dor_fates$state == "underground"] <- "underground"
dor_fates$life_st[dor_fates$state == "dead"] <- "dead"

#855 is having a problem!

dor_fates$psurvival[dor_fates$life_st == "dormant"] <- "1"
dor_fates$psurvival[dor_fates$life_st == "dead"] <- "0"
dor_fates$psurvival[dor_fates$life_st == "underground"] <- "1"
dor_fates$ros.area[dor_fates$life_st == "dormant"] <- NA
dor_fates$pflower[dor_fates$life_st == "dormant"] <- "0"
# can change more things for the dead ones (pflower, etc), but models should be ok for now

# #Putting these onto the main data file:
Dodecatheon <- dor_fates %>% 
  select(-state) %>% 
  filter(tag !="855") #fix this at some point!

remove(dormant, dor_fates, dtable)
# ################################################################################
# #Adding in eaten data from 2019!
# eaten_original <- read.csv("/Users/Jenna/Dropbox/Jenna/fall_dode/dode/Dodecatheon_data/2019 Prelim/2019_Dodecatheon_Demography_Data_Resurveys_TS3.csv",  fileEncoding="UTF-8-BOM")
# # get table for all flowering plants in 2019, and if they were eaten (p(eaten) = 1 or 0)
# #what to do: get 4 time steps into eaten data
# eaten_original <- eaten_original %>% 
#   mutate(plot = as.character(plot),
#          tag = as.character(tag))
# 
#  flowering19<- Docecatheon_wdor %>% 
#   filter(year == "2019") %>% 
#   filter(pflower == 1| !is.na(no.flowers)) %>%
#   select(year, plot, tag, no.flowers, no.capsules, no.eaten, no.aborted) %>%
#   mutate(time.surveyed = 0) 
# 
# semi_eaten <- semi_join(flowering19, eaten_original, by = "tag") 
# all_eaten <- rbind(semi_eaten, eaten_original) %>% 
#   arrange(plot, tag)
# all_eaten[is.na(all_eaten)] <- 0
# 
# all_eaten <- 
#   all_eaten %>% 
#   group_by(tag) %>% 
#   mutate(csum = cumsum(no.eaten)) %>% 
#   mutate(repro = ifelse(no.flowers > 0 | no.capsules > 0, 1, 0)) %>% 
#   filter(time.surveyed == "3")
# eaten <- all_eaten %>% 
#   mutate(peaten = if_else(repro == 0, 1, 0))
# #eaten = has peaten, which says if plant got eaten or not. If 0, then plant made seeds!
# 
# # there should be 26 plants that set seed, rest (105) were eaten
# 
# # Add the peaten column to Docecatheon_wdor
# 
# eaten <- eaten %>% 
#   select(c(year, tag, peaten))
# 
# star <- Docecatheon_wdor
# star <- star %>% 
#   left_join(eaten, star, by = c("year", "tag"))


##################################################################################

#tidyverse methods: lead() and lag()


# using tidyverse methods to get status next year

Dodecatheon <- Dodecatheon %>% 
  group_by(tag) %>% 
  mutate(ros.areaT1 = lead(ros.area),
         no.capsulesT1 = lead(no.capsules),
         pflowerT1 = lead(pflower),
         pflower = as.numeric(pflower),
         pflowerT1 = as.numeric(pflowerT1),
         psurvivalT1 = lead(psurvival),
         psurvivalT1 = as.numeric(psurvivalT1),
         life_stT1 = lead(life_st),
         ros.areaTminus1 = lag(ros.area),
         pflowerTminus1 = lag(pflower))
Dodecatheon <- Dodecatheon %>% 
  mutate(plot = as.character(plot),
         log.ros.areaT1 = log(ros.areaT1),
         log.ros.area = log(ros.area))
# 
# ## Adding fate next year, using mostly jenn's code:
# #star2 <- star %>% 
#   select(-c(ht, leaves, lv.length, rosetteL, rosetteW, notes, no.eaten, no.aborted)) %>% 
#   no.flowersT0 = 
# 
# Tzero<-star
# Tzero<-subset(Tzero, select = -c(ht, leaves, lv.length, rosetteL, rosetteW, notes, no.eaten, no.aborted))
# Tone<-star
# Tone<-subset(Tone, select = -c(ht, plot, Xcoor, Ycoor, YrTag, lv.length, leaves, rosetteL, rosetteW, notes, trt, no.capsules, no.eaten, no.aborted))
# 
# names(Tzero) <- sub("no.flowers", "no.flowersT0", names(Tzero))
# names(Tzero) <- sub("capsules", "capsulesT0", names(Tzero))
# names(Tzero) <- sub("pflower", "pflowerT0", names(Tzero)) #probability of flowering
# names(Tzero) <- sub("ros.area", "ros.areaT0", names(Tzero))
# names(Tzero) <- sub("psurvival", "psurvivalT0", names(Tzero))
# names(Tzero) <- sub("life_st", "life_stT0", names(Tzero))
# names(Tzero) <- sub("peaten", "peatenT0", names(Tzero))
# 
# names(Tone) <- sub("no.flowers", "no.flowersT1", names(Tone))
# names(Tone) <- sub("ros.area", "ros.areaT1", names(Tone))
# names(Tone) <- sub("pflower", "pflowerT1", names(Tone)) #probability of flowering
# names(Tone) <- sub("psurvival", "psurvivalT1", names(Tone))
# names(Tone) <- sub("life_st", "life_stT1", names(Tone))
# names(Tone) <- sub("peaten", "peatenT1", names(Tone))
# 
# Tone$year <- Tone$year - 1
# 
# star2 <- merge(Tzero, Tone, by=c("tag","year"), all.x=TRUE) 

#6/25/2020 There are some Lifestage = U's still in there for NP/NT plants, right now they're NA's for everything

#write.csv(star2, "C:/Users/Jenna/Dropbox/Jenna/fall_dode/dode/Dodecatheon_data/AllYears_Dodecatheon_Demography_fates_Oct6.csv",  row.names=F)
#write.csv(star2, "C:/Users/Jenna/Dropbox/Jenna/fall_dode/dode/Dodecatheon_data/AllYears_Dodecatheon_Demography_fates.csv",  row.names=F)


##########Tiny Plants###################################################################
#who are the tiny plants?

tiny <- Dodecatheon %>% filter(log.ros.area<2)
extiny <- Dodecatheon %>% filter(log.ros.area<1)
#913 in 2018: flowered when tiny - had 2 flowers, no notes
#extiny plants - what to do here - they are probably clones or new things popping up. But they can definitely get smaller from being big after being underground!

##########Making repro structures metric###################################################################################
Dodecatheon <- Dodecatheon %>% 
  mutate(no.flowers = as.numeric(no.flowers),
         no.capsules = as.numeric(no.capsules),
         no.aborted = as.numeric(no.aborted),
         no.eaten = as.numeric(no.eaten)) %>% 
  replace_na(list(no.flowers = 0, no.capsules = 0, no.aborted = 0, no.eaten = 0)) %>% 
  dplyr::mutate(flow.sum = no.capsules+ no.flowers,
                flow.sum = flow.sum+no.aborted,
                 flow.sum = flow.sum+no.eaten)
                
#here, Sum = total number of reproductive parts the plant made, regardless of if those made seeds
# Next step: add total number of flowers that made seeds
   # tot.flowers = no.aborted + no.capsules, .na.rm = T)

       #  tot.flowers2 = sum(tot.flowers, no.flowers, na.rm = T))
# some thign weird is going on with no flowers...not sure what



##########determine density for pot experiment:###############################################################
# #
# 
# test <- Dodecatheon %>% 
#   filter(year == "2020") %>% 
#   unite("coords", Xcoor:Ycoor) %>% 
#   group_by(plot, coords) %>% 
#   summarize( n = n())
# test2 <- test %>% separate(coords, c("Xcoor", "Ycoor"))
# test2$Ycoor <- as.integer(as.factor(test2$Ycoor))
# test2$Xcoor <- as.numeric(test2$Xcoor)
# test2$n <- as.character(test2$n)
# density <- test2 %>% ggplot(aes(Xcoor, Ycoor))+
#   geom_point(aes(shape = n, color = n))+
#   theme_classic()+
#   facet_wrap(~plot)
# 
# #ggsave("density in plots.png")

remove(tiny, extiny)

