#set working directory
setwd("C:/Users/raadt/OneDrive - University of Nebraska at Omaha/Datasets/Sex_Ratio")

#read file
df <- read.csv("NC_netting.csv")

####cleanup####
class(df$SEX)
df$SEX<-as.factor(df$SEX)
df$REPRO_STAT<-as.factor(df$REPRO_STAT)
df2$REPRO_STAT<-as.factor(df2$REPRO_STAT)

df1$X..GRTS.Cell.Id<-as.numeric(df1$X..GRTS.Cell.Id)

summary(df$SEX)
summary(df$REPRO_STAT)
summary(df2$REPRO_STAT)

df[,c(1,3)]
df1<-df[,c(1,7)]

###Data meeting revisions###
df <- df[, c(1, 5, 6, 7, 8, 14, 30)]
#df[is.na(df),] <- -1

#turn "DATE" into month/day/year
df <- separate(df, col = "DATE", c("month", "day", "year"), sep = "/")

#Remove excel descriptor row
df <- subset(df, df$AGE != "Age")

#to see how many recaptures in df 
df$BAND <- as.factor(df$BAND)
summary(df$BAND)

#SEX term cleanup
summary(df$SEX)
df_male <- subset (df, df$SEX=="male" | df$SEX=="Male" | df$SEX=="M")
df_female <- subset (df, df$SEX=="Female")
df_male$SEX <- "male"
df_female$SEX <- "female"

#generates dataframe with missing, unknown, etc.
df_new <- rbind(df_male, df_female)

colnames(df_new[1]) <- "cell"

#gets rid of awkward GRID cell title, makes new column "cell" within df_new
df_new$cell <- df_new$X..GRTS.Cell.Id
df_new <- df_new[, c(2:10)]
df_new$cell <- as.factor(df_new$cell)

#makes year column numeric
df_new$year <- as.numeric(df_new$year)

#creates a column where year is a character
df_new$year_char <- as.factor(df_new$year)

#summarizes BAT_SP, relabels BAT_SP entries as factors
summary(df_new$BAT_SP)
df_new$BAT_SP <- as.factor(df_new$BAT_SP)

#creates new column "species", sets all to 0
df_new$species <- 0

#labels unknowns to "noid"(omitting NA cells), and single na to "pesu" within species column
df_new[!is.na(df_new$BAT_SP) & (df_new$BAT_SP == "unknown" | df_new$BAT_SP == "Unknown"), "species"] <- "noid"
df_new[is.na(df_new$BAT_SP),]$species <- "pesu"

#adds species tags in "species" column
df_new$species[df_new$BAT_SP=="Corynorhinus townsendii virginianus"] <- "coto"
df_new$species[df_new$BAT_SP=="Corynorhinus rafinesquii rafinesquii"] <- "cora"
df_new$species[df_new$BAT_SP=="Corynorhinus rafinesquii macrotis"] <- "cora"
df_new$species[df_new$BAT_SP=="Eptesicus fuscus"] <- "epfu"
df_new$species[df_new$BAT_SP=="Lasionycteris noctivagans"] <- "lano"
df_new$species[df_new$BAT_SP=="Lasiurus borealis"] <- "labo"
df_new$species[df_new$BAT_SP=="Lasiurus cinereus"] <- "laci"
df_new$species[df_new$BAT_SP=="Lasiurus intermedius floridanus"] <- "lain"
df_new$species[df_new$BAT_SP=="Lasiurus seminolus"] <- "lase"
df_new$species[df_new$BAT_SP=="Myotis austroriparius"] <- "myau"
df_new$species[df_new$BAT_SP=="Myotis grisescens"] <- "mygr"
df_new$species[df_new$BAT_SP=="Myotis leibii leibii"] <- "myle"
df_new$species[df_new$BAT_SP=="Myotis lucifugus"] <- "mylu"
df_new$species[df_new$BAT_SP=="Myotis septentrionalis"] <- "myse"
df_new$species[df_new$BAT_SP=="Myotis sodalis"] <- "myso"
df_new$species[df_new$BAT_SP=="Nycticeius humeralis"] <- "nyhu"
df_new$species[df_new$BAT_SP=="Perimyotis subflavus"] <- "pesu"
df_new$species[df_new$BAT_SP=="Tadarida brasiliensis"] <- "tabr"

df_new$species[df_new$BAT_SP=="Lasionycteris notivagans"] <- "lano"

#creates df2, a "Female"-only dataframe
df2<-subset(df,df$SEX=="Female")

df_new$species <- as.factor(df_new$species)
summary(df_new$species)

####Eastern Red#####
df_labo <- subset(df_new, species=="labo")

summary(df_new$cell)

#creates df for labo using agg function and renaming columns
df_labo_agg <- aggregate(df_labo$SEX, by=list(df_labo$SEX, df_labo$month, df_labo$day, df_labo$year_char, df_labo$cell), FUN = length)
names(df_labo_agg)[1]<-"sex"
names(df_labo_agg)[2]<-"month"
names(df_labo_agg)[3]<-"day"
names(df_labo_agg)[4]<-"year"
names(df_labo_agg)[5]<-"cell"
names(df_labo_agg)[6]<-"count"

#creates df3, making male/female count columns
df3 <- spread(df_labo_agg, sex, count)
df3[is.na(df3)]<- 0

#makes new column "total", which totals all bats caught (male+female)
df3$total <- df3$male + df3$female

#making wide instead of long; looking at totals by year(df4), or cell(df5)
df4 <- xtabs(count ~ sex + year, data=df_labo_agg)
df5 <- xtabs(count ~ sex + cell, data=df_labo_agg)

#read file
df_table <- read.csv("table.csv")

#makes df6 from df_table with only needed columns
df6 <- df_table[, c(2, 14, 15)]
df7 <- merge(df3, df6, by.x = "cell", by.y = "GRTS_ID")
df7$male_prop <- df7$male / df7$total

#convert df7 to csv
write.csv(df7, "NC_labo.csv", row.names = FALSE)


#fit <- glm(data = dataframe you want to test, dep_var~indep~var, family = data distribution)
fit <- glm(data=df7, male_prop~centroid_x, family = "quasibinomial")
fit2 <- glm(data=df7, male_prop~centroid_y, family = "quasibinomial")
summary(fit)
summary(fit2)
df7$year1 <- as.integer(levels(df7$year))[df7$year]
fit <- glm(data=df7, male_prop~year1, family = "quasibinomial")
summary(fit)

#X and Y are SIGNIFICANT -- Females more abundant south-east corner of NC

####Big Brown#####

df_epfu <- subset(df_new, species=="epfu")

#creates df for epfu using agg function and renaming columns
df_epfu_agg <- aggregate(df_epfu$SEX, by=list(df_epfu$SEX, df_epfu$month, df_epfu$day, df_epfu$year_char, df_epfu$cell), FUN = length)
names(df_epfu_agg)[1]<-"sex"
names(df_epfu_agg)[2]<-"month"
names(df_epfu_agg)[3]<-"day"
names(df_epfu_agg)[4]<-"year"
names(df_epfu_agg)[5]<-"cell"
names(df_epfu_agg)[6]<-"count"

#creates df8, making male/female count columns for epfu
df8 <- spread(df_epfu_agg, sex, count)
df8[is.na(df8)]<- 0

#makes new column "total", which totals all bats caught (male+female)
df8$total <- df8$male + df8$female

#making wide instead of long; looking at totals by year(df9), or cell(df10)
df9 <- xtabs(count ~ sex + year, data=df_epfu_agg)
df10 <- xtabs(count ~ sex + cell, data=df_epfu_agg)

#makes df11 from df_table with only needed columns
df11 <- df_table[, c(2, 14, 15)]
df12 <- merge(df8, df11, by.x = "cell", by.y = "GRTS_ID")
df12$male_prop <- df8$male / df8$total

write.csv(df12, "NC_epfu.csv", row.names = FALSE)

#fit <- glm(data = dataframe you want to test, dep_var~indep~var, family = data distribution)
fit <- glm(data=df12, male_prop~centroid_x, family = "quasibinomial")
fit2 <- glm(data=df12, male_prop~centroid_y, family = "quasibinomial")
summary(fit)
summary(fit2)
df12$year1 <- as.integer(levels(df12$year))[df12$year]
fit <- glm(data=df12, male_prop~year1, family = "quasibinomial")
summary(fit)

#Y-dimension is NOT significant for epfu -- more male skewed towards east

####Little Brown#####

df_mylu <- subset(df_new, species=="mylu")

#creates df for mylu using agg function and renaming columns
df_mylu_agg <- aggregate(df_mylu$SEX, by=list(df_mylu$SEX, df_mylu$year_char, df_mylu$cell), FUN = length)
names(df_mylu_agg)[1]<-"sex"
names(df_mylu_agg)[2]<-"year"
names(df_mylu_agg)[3]<-"cell"
names(df_mylu_agg)[4]<-"count"

#creates df13, making male/female count columns for mylu
df13 <- spread(df_mylu_agg, sex, count)
df13[is.na(df13)]<- 0

#makes new column "total", which totals all bats caught (male+female)
df13$total <- df13$male + df13$female

#making wide instead of long; looking at totals by year(df14), or cell(df15)
df14 <- xtabs(count ~ sex + year, data=df_mylu_agg)
df15 <- xtabs(count ~ sex + cell, data=df_mylu_agg)

#makes df16 from df_table with only needed columns
df16 <- df_table[, c(2, 14, 15)]
df17 <- merge(df13, df16, by.x = "cell", by.y = "GRTS_ID")
df17$male_prop <- df13$male / df13$total

#fixing issue with above line
nrow(df13)  # Should return 165
nrow(df17)  # Should return 164
df13 <- df13[-161, ]

#fit <- glm(data = dataframe you want to test, dep_var~indep~var, family = data distribution)
fit <- glm(data=df17, male_prop~centroid_x, family = "quasibinomial")
fit2 <- glm(data=df17, male_prop~centroid_y, family = "quasibinomial")
summary(fit)
summary(fit2)
df17$year1 <- as.integer(levels(df17$year))[df17$year]
fit <- glm(data=df17, male_prop~year1, family = "quasibinomial")
summary(fit)

#no significance found

####Northern Long-eared####

df_myse <- subset(df_new, species == "myse")

# creates df for myse using agg function and renaming columns
df_myse_agg <- aggregate(df_myse$SEX, by = list(df_myse$SEX, df_myse$year_char, df_myse$cell), FUN = length)
names(df_myse_agg)[1] <- "sex"
names(df_myse_agg)[2] <- "year"
names(df_myse_agg)[3] <- "cell"
names(df_myse_agg)[4] <- "count"

# creates df18, making male/female count columns for myse
df18 <- spread(df_myse_agg, sex, count)
df18[is.na(df18)] <- 0

# makes new column "total", which totals all bats caught (male + female)
df18$total <- df18$male + df18$female

# making wide instead of long; looking at totals by year (df19), or cell (df20)
df19 <- xtabs(count ~ sex + year, data = df_myse_agg)
df20 <- xtabs(count ~ sex + cell, data = df_myse_agg)

# makes df21 from df_table with only needed columns
df21 <- df_table[, c(2, 14, 15)]
df22 <- merge(df18, df21, by.x = "cell", by.y = "GRTS_ID")
df22$male_prop <- df18$male / df18$total

# fit models for male proportion using GLM with quasibinomial distribution
fit <- glm(data = df22, male_prop ~ centroid_x, family = "quasibinomial")
fit2 <- glm(data = df22, male_prop ~ centroid_y, family = "quasibinomial")
summary(fit)
summary(fit2)

df22$year1 <- as.integer(levels(df22$year))[df22$year]
fit <- glm(data = df22, male_prop ~ year1, family = "quasibinomial")
summary(fit)

#no signifiance found

####summary table LABO####

library(dplyr)

df7 %>%
  group_by(centroid_x, centroid_y) %>%
  summarise(mean(male_prop, na.rm=TRUE), sd(male_prop, na.rm=TRUE), n=371)

####playing with ggplot####
library(ggplot2)

ggplot(data=df7, aes(x=centroid_x, y=male_prop, fill=male_prop))
geom_point(size = 3)
theme_minimal()

