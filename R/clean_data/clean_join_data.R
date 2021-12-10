#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.08.28: Max Lindmark
#
# Code to read and join raw back calculated length-at-age data for each area
# 
# A. Load libraries
# 
# B. Read data sets
# 
# C. Join data 
#
# D. Explore data
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
rm(list = ls()) # Clear console history (restart r for complete fresh start)

# Load libraries (install first if needed)
library(tidyverse)
library(tidylog)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggsidekick) # devtools::install_github("seananderson/ggsidekick")
library(RColorBrewer)
library(forcats)
library(viridis)

# Set a nice and clean theme
theme_set(ggsidekick::theme_sleek())


# B. READ DATA =====================================================================
#** Biotest ========================================================================
#**** Read in the .txt files =======================================================
#**** Read in the .xls files =======================================================

#** Brunskär =======================================================================

#**** Read in the .txt files =======================================================
BSAB91 <- read_delim("data/Brunskär/BSTAB91.txt", delim = "\t", col_names = FALSE)  
BSAB92 <- read_delim("data/Brunskär/BSTAB92.txt", delim = "\t", col_names = FALSE)
BSAB93 <- read_delim("data/Brunskär/BSTAB93.txt", delim = "\t", col_names = FALSE)
BSAB94 <- read_delim("data/Brunskär/BSTAB94.txt", delim = "\t", col_names = FALSE)
BSAB95 <- read_delim("data/Brunskär/BSTAB95.txt", delim = "\t", col_names = FALSE) 
BSAB96 <- read_delim("data/Brunskär/BSTAB96.txt", delim = "\t", col_names = FALSE) 
BSAB97 <- read_delim("data/Brunskär/BSTAB97.txt", delim = "\t", col_names = FALSE) 
BSAB98 <- read_delim("data/Brunskär/BSTAB98.txt", delim = "\t", col_names = FALSE) 
BSAB99 <- read_delim("data/Brunskär/BSTAB99.txt", delim = "\t", col_names = FALSE) 
BSAB00 <- read_delim("data/Brunskär/BSTAB00.txt", delim = "\t", col_names = FALSE)
BSAB01 <- read_delim("data/Brunskär/Bstab01.txt", delim = "\t", col_names = FALSE)

BSAB_91_01 <- bind_rows(BSAB91, BSAB92, BSAB93, BSAB94, BSAB95, BSAB96,
                        BSAB97, BSAB98, BSAB99, BSAB00, BSAB01)

BSAB_91_01 <- BSAB_91_01 %>% 
  separate(X1, 
           sep = c(2,4,8,10,13,15,17,18,21,22,23,26,29,32,35,38,41,44,47,50,53,56,59,
                   62,65,68,71,74,77), 
           into = c("bl","area","species","red","stn_nr","catch_year","cohort","sex",
                    "sample_nr","number_rows","growth_catch_year","final_length","X1",
                    "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
                    "X14","X15","X16","X17")) %>% 
  mutate_at(c("red","stn_nr","catch_year","cohort","sex",
            "sample_nr","number_rows","final_length","X1",
            "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
            "X14","X15","X16","X17"), as.numeric) 

# Hmm..
BSAB_91_01 %>% distinct(bl)
BSAB_91_01 %>% filter(bl == "\u001a") %>% as.data.frame()


# Now create a long data frame and use the col names that are in the .xls data
colnames(BSAB_91_01)

BSAB_91_01 <- BSAB_91_01 %>%
  pivot_longer(col = c(13:29), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 1) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm) %>% 
  mutate(catch_year = ifelse(catch_year > 10, paste(19, catch_year, sep = ""), paste(200, catch_year, sep = ""))) %>%
  mutate_at("catch_year", as.numeric) %>% 
  mutate(cohort = ifelse(cohort > 10, paste(19, cohort, sep = ""), paste(200, cohort, sep = ""))) %>% 
  mutate_at("cohort", as.numeric) %>% 
  mutate(age = catch_year - cohort)

# Create ID column
BSAB_91_01 <- BSAB_91_01 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "_")) # add date + gear here as well


#**** Read in the .xls files =======================================================
# This is not a t-file! (growth)
BSabboa2002 <- readxl::read_xls("data/Brunskär/BSabboa2002.xls")

# These are t-files
BSabboa2003 <- readxl::read_xls("data/Brunskär/BSabbot2003p.xls") # specify which tab here... 
BSabboa2004 <- readxl::read_xls("data/Brunskär/BSabbot2004p.xls")
BSabboa2005 <- readxl::read_xls("data/Brunskär/BSabbot2005p.xls")
BSabboa2006 <- readxl::read_xls("data/Brunskär/BSabbot2006p.xls")

colnames(BSabboa2003)
colnames(BSabboa2004)
colnames(BSabboa2005)
colnames(BSabboa2006)

# Find names of columns that are not in both data sets
colnames(BSabboa2005)[!colnames(BSabboa2005) %in% colnames(BSabboa2003)]
colnames(BSabboa2003)[!colnames(BSabboa2003) %in% colnames(BSabboa2005)]

# Need to change the name of this column
colnames(BSabboa2003)[41] <- "Text som anger var kopplade data finns"
colnames(BSabboa2004)[41]
colnames(BSabboa2005)[41]
colnames(BSabboa2006)[41] <- "Text som anger var kopplade data finns"

# Need to change "Analysdatum" to numeric for 2005 & 2006
BSabboa2005$Analysdatum <- as.numeric(BSabboa2005$Analysdatum)
BSabboa2006$Analysdatum <- as.numeric(BSabboa2006$Analysdatum)

# Need to change "Totallängd mm" to numeric for 2006
BSabboa2006$"Totallängd mm" <- as.numeric(BSabboa2006$"Totallängd mm")

BSAB_03_06 <- bind_rows(BSabboa2003,
                        BSabboa2004,
                        BSabboa2005,
                        BSabboa2006)

# Go from wide to long data
str(BSAB_03_06)

# First make sure all lengths at age columns are numeric
BSAB_03_06 <- BSAB_03_06 %>% 
  mutate_at(c("År1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
            "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) 

# Columns 52-71 should be gathered
glimpse(BSAB_03_06)
colnames(BSAB_03_06)

BSAB_03_06 <- BSAB_03_06 %>%
  pivot_longer(col = c(52:71), names_to = "age_temp", values_to = "op_length") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(op_length)

# Rename variables
BSAB_03_06 <- BSAB_03_06 %>% 
  rename("catch_year" = "Fångstår",
         "age" = "Ålder",
         "area" = "Areakod",
         "species" = "Art",
         "gear" = "Redskap",
         "stn_nr" = "Station",
         "cohort" = "Födelseår",
         "sex" = "Kön",
         "sample_nr" = "Löpnummer",
         "final_length" = "Totallängd mm")

# Create ID column
BSAB_03_06 <- BSAB_03_06 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))

# Convert from operculum to length
colnames(BSAB_03_06)

# If there are more readings than the age, it means plus growth was measured. This means
# that the final length is actually the radius, which we need when converting from operculum
# length to fish length
# Verify this:
BSAB_03_06 %>%
  group_by(ID) %>% 
  mutate(n = n(),
         plus_growth = ifelse(n > age, "Y", "N")) %>% 
  ungroup() %>% 
  distinct(plus_growth)

BSAB_03_06 <- BSAB_03_06 %>%
  group_by(ID) %>% 
  mutate(max_op_length = max(op_length)) %>% 
  ungroup() %>% 
  mutate(length_mm = (15.56 + 18.485*op_length*Magnifikation - 0.1004*(op_length*Magnifikation)^2)*final_length /
           ((15.56 + 18.485*max_op_length*Magnifikation - 0.1004*(max_op_length*Magnifikation)^2)))



#**** Bind rows ====================================================================
BSAB_03_06$source <- "xls"
BSAB_91_01$source <- "txt" # for any other area we might want to xls KUL or xls G
BS_91_06 <- bind_rows(BSAB_03_06, BSAB_91_01)

# Change variable type so that we can join all data later
BS_91_06$gear <- as.character(BS_91_06$gear)
BS_91_06$sex <- as.character(BS_91_06$sex)
BS_91_06$Analysdatum <- as.character(BS_91_06$Analysdatum)

# Add in column that indicates if the length is from a + reading or not
BS_91_06 <- BS_91_06 %>% 
  mutate(age_ring = ifelse(reading_no > age, "N", "Y"))

BS_91_06 %>% 
  filter(ID == "2003.4.BS") %>% 
  as.data.frame()


#**** Plot =========================================================================
# Plot # of samples per catch year
BS_91_06 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year, source) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n, fill = source)) + geom_bar(stat = "identity") 

# Plot length-at-age over catch_year
BS_91_06 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm, color = source)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_wrap(~factor(reading_no), scales = "free_y")

# Plot length-at-age
BS_91_06 <- BS_91_06 %>% filter(length_mm < 600)
BS_91_06 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


#** Finbo ==========================================================================
#**** Read in the .txt files =======================================================
#**** Read in the .xls files =======================================================


#** Forsmark =======================================================================
#**** Read in the .txt files =======================================================
FMAB70 <- read_delim("data/Forsmark/FMTAB70.TXT", delim = "\t", col_names = FALSE)
FMAB71 <- read_delim("data/Forsmark/FMTAB71.TXT", delim = "\t", col_names = FALSE)
FMAB72 <- read_delim("data/Forsmark/FMTAB72.TXT", delim = "\t", col_names = FALSE)
FMAB73 <- read_delim("data/Forsmark/FMTAB73.TXT", delim = "\t", col_names = FALSE)
FMAB74 <- read_delim("data/Forsmark/FMTAB74.TXT", delim = "\t", col_names = FALSE)
FMAB75 <- read_delim("data/Forsmark/FMTAB75.TXT", delim = "\t", col_names = FALSE)
FMAB76 <- read_delim("data/Forsmark/FMTAB76.TXT", delim = "\t", col_names = FALSE)
FMAB77 <- read_delim("data/Forsmark/FMTAB77.TXT", delim = "\t", col_names = FALSE)
FMAB78 <- read_delim("data/Forsmark/FMTAB78.TXT", delim = "\t", col_names = FALSE)
FMAB79 <- read_delim("data/Forsmark/FMTAB79.TXT", delim = "\t", col_names = FALSE)
FMAB80 <- read_delim("data/Forsmark/FMTAB80.TXT", delim = "\t", col_names = FALSE)
FMAB81 <- read_delim("data/Forsmark/Fmtab81.TXT", delim = "\t", col_names = FALSE)
FMAB82 <- read_delim("data/Forsmark/FMTAB82.TXT", delim = "\t", col_names = FALSE)
FMAB83 <- read_delim("data/Forsmark/FMTAB83.TXT", delim = "\t", col_names = FALSE)
FMAB84 <- read_delim("data/Forsmark/FMTAB84A.TXT", delim = "\t", col_names = FALSE)
FMAB85 <- read_delim("data/Forsmark/FMTAB85.TXT", delim = "\t", col_names = FALSE)
FMAB86 <- read_delim("data/Forsmark/FMTAB86A.TXT", delim = "\t", col_names = FALSE)
FMAB87 <- read_delim("data/Forsmark/FMTAB87.TXT", delim = "\t", col_names = FALSE)
FMAB88 <- read_delim("data/Forsmark/FMTAB88.TXT", delim = "\t", col_names = FALSE)
FMAB89 <- read_delim("data/Forsmark/FMTAB89.TXT", delim = "\t", col_names = FALSE)
FMAB90 <- read_delim("data/Forsmark/FMTAB90.TXT", delim = "\t", col_names = FALSE)
FMAB91 <- read_delim("data/Forsmark/FMTAB91.TXT", delim = "\t", col_names = FALSE)
FMAB92 <- read_delim("data/Forsmark/FMTAB92.TXT", delim = "\t", col_names = FALSE)
FMAB93 <- read_delim("data/Forsmark/FMTAB93.TXT", delim = "\t", col_names = FALSE)
FMAB94 <- read_delim("data/Forsmark/FMTAB94.TXT", delim = "\t", col_names = FALSE)
FMAB95 <- read_delim("data/Forsmark/FMTAB95.TXT", delim = "\t", col_names = FALSE)
FMAB96 <- read_delim("data/Forsmark/FMTAB96.TXT", delim = "\t", col_names = FALSE)
FMAB97 <- read_delim("data/Forsmark/FMTAB97.TXT", delim = "\t", col_names = FALSE)
FMAB98 <- read_delim("data/Forsmark/Fmtab98.TXT", delim = "\t", col_names = FALSE)
FMAB99 <- read_delim("data/Forsmark/FMTAB99.TXT", delim = "\t", col_names = FALSE)
FMAB00 <- read_delim("data/Forsmark/FMTAB00.TXT", delim = "\t", col_names = FALSE)
FMAB01 <- read_delim("data/Forsmark/FMTAB01.TXT", delim = "\t", col_names = FALSE)

FM_71_01 <- bind_rows(FMAB70, FMAB71, FMAB72, FMAB73, FMAB74, FMAB75, FMAB76, FMAB77, 
                      FMAB78, FMAB79, FMAB80, FMAB81, FMAB82, FMAB83, FMAB84, FMAB85, 
                      FMAB86, FMAB87, FMAB88, FMAB89, FMAB90, FMAB91, FMAB92, FMAB93, 
                      FMAB94, FMAB95, FMAB96, FMAB97, FMAB98, FMAB99, FMAB00, FMAB01)


FM_71_01 <- FM_71_01 %>% 
  separate(X1, 
           sep = c(2,4,8,10,13,15,17,18,21,22,23,26,29,32,35,38,41,44,47,50,53,56,59,
                   62,65,68,71,74,77), 
           into = c("bl","area","species","red","stn_nr","catch_year","cohort","sex",
                    "sample_nr","number_rows","growth_catch_year","final_length","X1",
                    "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
                    "X14","X15","X16","X17")) %>% 
  mutate_at(c("red","stn_nr","catch_year","cohort","sex",
              "sample_nr","number_rows","final_length","X1",
              "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
              "X14","X15","X16","X17"), as.numeric) 

# Now create a long data frame and use the col names that are in the .xls data
colnames(FM_71_01)

FM_71_01 <- FM_71_01 %>%
  pivot_longer(col = c(13:29), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 1) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm) %>% 
  mutate(catch_year = ifelse(catch_year > 10, paste(19, catch_year, sep = ""), paste(200, catch_year, sep = ""))) %>%
  mutate_at("catch_year", as.numeric) %>% 
  mutate(cohort = ifelse(cohort > 10, paste(19, cohort, sep = ""), paste(200, cohort, sep = ""))) %>% 
  mutate_at("cohort", as.numeric) %>% 
  mutate(age = catch_year - cohort)

# Create ID column
FM_71_01 <- FM_71_01 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "_")) # add date + gear here as well


#**** Read in the .xls files =======================================================
FMabboa2004 <- readxl::read_xls("data/Forsmark/FMabbot2004p.xls") %>%
  rename("Fiskeår" = "Fångstår", "Redskap.kod" = "Redskap", "Total.längd.mm" = "Totallängd mm")

FMabboa2005 <- readxl::read_xls("data/Forsmark/FMabbot2005p.xls") %>%
  rename("Fiskeår" = "Fångstår", "Redskap.kod" = "Redskap", "Total.längd.mm" = "Totallängd mm")

FMabboa2006 <- readxl::read_xls("data/Forsmark/FMabbot2006p.xls") %>%
  rename("Fiskeår" = "Fångstår", "Redskap.kod" = "Redskap", "Total.längd.mm" = "Totallängd mm")

FMabbo_02_and_17_20_gear09_aug <- read.csv("data/Forsmark/FMabbo_2002and2017to2020_gear09aug.csv", header = TRUE, sep = ";") %>% 
  rename("År1" = "Tillväxt..mm.år.1")

FMabbo_02_03_and_07_20_gear64_aug <- read.csv("data/Forsmark/FMabbo_2002to2003and2007to2020_gear64aug.csv", header = TRUE, sep = ";") %>% 
  rename("År1" = "Tillväxt..mm.år.1")

FMabbo_09_20_gear09_oct <- read.csv("data/Forsmark/FMabbo_2009to2020_gear09okt.csv", header = TRUE, sep = ";") %>% 
  rename("År1" = "Tillväxt..mm.år.1")

FMabboa2004$Redskap.kod <- as.character(FMabboa2004$Redskap.kod)
FMabboa2005$Redskap.kod <- as.character(FMabboa2005$Redskap.kod)

FMabboa2004$År12 <- as.numeric(FMabboa2004$År12)
FMabboa2005$År12 <- as.numeric(FMabboa2005$År12)
FMabboa2006$År12 <- as.numeric(FMabboa2006$År12)

FMabboa2004$Kön <- as.character(FMabboa2004$Kön)
FMabboa2005$Kön <- as.character(FMabboa2005$Kön)
FMabboa2006$Kön <- as.character(FMabboa2006$Kön)

FM_02_20 <- bind_rows(FMabboa2004,
                      FMabboa2005,
                      FMabboa2006,
                      FMabbo_02_and_17_20_gear09_aug,
                      FMabbo_02_03_and_07_20_gear64_aug,
                      FMabbo_09_20_gear09_oct)

# First make sure all lengths at age columns are numeric
FM_02_20 <- FM_02_20 %>% 
  mutate_at(c("År1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
              "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) 

# Columns 52-71 should be gathered
glimpse(FM_02_20)
colnames(FM_02_20)

FM_02_20 <- FM_02_20 %>%
  pivot_longer(col = c(52:71), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm)

# Rename variables
FM_02_20 <- FM_02_20 %>% 
  rename("catch_year" = "Fiskeår",
         "age" = "Ålder",
         "species" = "Art",
         "gear" = "Redskap.kod",
         "sex" = "Kön",
         "sample_nr" = "Löpnr",
         "final_length" = "Total.längd.mm") %>% 
  mutate(area = "JM",
         cohort = catch_year - age)

# Create ID column
FM_02_20 <- FM_02_20 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))

# Now, if the years are 04, 05, 06, length_mm is in fact not the actual length!
# It's the operculum length. I need to correct that for those years
# Convert from operculum to length
colnames(FM_02_20)

# If there are more readings than the age, it means plus growth was measured. This means
# that the final length is actually the radius, which we need when converting from operculum
# length to fish length
# Verify this:
FM_02_20 %>%
  group_by(ID) %>% 
  mutate(n = n(),
         plus_growth = ifelse(n > age, "Y", "N")) %>% 
  ungroup() %>% 
  filter(catch_year %in% c(2004, 2005, 2006)) %>% 
  count(plus_growth)

FM_02_20 %>% group_by(catch_year) %>% distinct(Magnifikation)
FM_02_20 %>% group_by(catch_year) %>% distinct(is.na(length_mm))

FM_02_20 <- FM_02_20 %>%
  group_by(ID) %>% 
  mutate(n = n(),
         plus_growth = ifelse(n > age, "Y", "N")) %>% 
  ungroup() %>% 
  filter(plus_growth == "Y") %>% # All other years have plus growth, so this only removes the individuals can't can't get a length-measurement
  group_by(ID) %>% 
  mutate(max_op_length = max(length_mm)) %>% # In contrast to the xls files where ALL lengths had to be converted, this is only for certain years. Hence I didn't bother to call lengths "op_length"
  ungroup() %>% 
  mutate(length_mm = ifelse(catch_year %in% c(2004, 2005, 2006),
                            (15.56 + 18.485*length_mm*Magnifikation - 0.1004*(length_mm*Magnifikation)^2)*final_length /
                              ((15.56 + 18.485*max_op_length*Magnifikation - 0.1004*(max_op_length*Magnifikation)^2)),
                            length_mm))

FM_02_20 %>% group_by(catch_year) %>% distinct(is.na(length_mm))


#**** Bind rows ====================================================================
FM_02_20$source <- "xls"
FM_71_01$source <- "txt"

FM_71_01$sex <- as.character(FM_71_01$sex)

FM_71_20 <- bind_rows(FM_71_01, FM_02_20)

# So that I can merge them later
FM_71_20$Analysdatum <- as.character(FM_71_20$Analysdatum)

# Add in column that indicates if the length is from a + reading or not
FM_71_20 <- FM_71_20 %>% 
  mutate(age_ring = ifelse(reading_no > age, "N", "Y"))

#**** Plot =========================================================================
# Plot # of samples per catch year
FM_71_20 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year, source) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n, fill = source)) + geom_bar(stat = "identity") 

# Plot length-at-age over catch_year 
FM_71_20 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm, color = source)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_wrap(~factor(reading_no), scales = "free_y")

# Plot length-at-age
FM_71_20 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


#** Holmön =========================================================================
#**** Read in the .xls files =======================================================
HOabboa88 <- read.csv("data/Holmön/HOabbo_1988_pilot.csv", header = TRUE, sep = ";")
HOabboa89_01 <- read.csv("data/Holmön/HOabbo_1989to2001.csv", header = TRUE, sep = ";")
HOabboa03_20 <- read.csv("data/Holmön/HOabbo_2003to2020.csv", header = TRUE, sep = ";")

HOabboa88$Somatisk.vikt <- as.character(HOabboa88$Somatisk.vikt)
HOabboa89_01$Somatisk.vikt <- as.character(HOabboa89_01$Somatisk.vikt)
HOabboa03_20$Somatisk.vikt <- as.character(HOabboa03_20$Somatisk.vikt)

HO_88_20 <- bind_rows(HOabboa88,
                      HOabboa89_01,
                      HOabboa03_20)

# Go from wide to long data
str(HO_88_20)

# First make sure all lengths at age columns are numeric
HO_88_20 <- HO_88_20 %>% 
  rename("År1" = "Tillväxt..mm.år.1") %>% 
  mutate_at(c("År1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
              "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) 

# Columns 19-38 should be gathered
glimpse(HO_88_20)
colnames(HO_88_20)

HO_88_20 <- HO_88_20 %>%
  pivot_longer(col = c(19:38), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm)

# Rename variables
HO_88_20 <- HO_88_20 %>% 
  rename("catch_year" = "Fiskeår",
         "age" = "Ålder",
         "species" = "Art",
         "gear" = "Redskap.kod",
         "sex" = "Kön",
         "sample_nr" = "Löpnr",
         "final_length" = "Total.längd.mm") %>% 
  mutate(area = "HO",
         cohort = catch_year - age)

# Create ID column
HO_88_20 <- HO_88_20 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))

# Add in column that indicates if the length is from a + reading or not
HO_88_20 <- HO_88_20 %>% 
  mutate(age_ring = ifelse(reading_no > age, "N", "Y"))

# Remove somatisk vikt...
HO_88_20 <- HO_88_20 %>% dplyr::select(-Somatisk.vikt)


#**** Plot =========================================================================
# Plot # of samples per catch year
HO_88_20 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n)) + geom_bar(stat = "identity") 

# Plot length-at-age over catch_year
HO_88_20 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "tomato", method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_wrap(~factor(reading_no), scales = "free_y")

# Plot length-at-age over catch_year
HO_88_20 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "tomato", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


#** Kvädöfjärden ===================================================================
#**** Read in the .txt files =======================================================
JMAB84 <- read_delim("data/Kvädöfjärden/JMTAB84.TXT", delim = "\t", col_names = FALSE)  

JMAB84 <- JMAB84 %>% 
  separate(X1, 
           sep = c(2,4,8,10,13,15,17,18,21,22,23,26,29,32,35,38,41,44,47,50,53,56,59,
                   62,65,68,71,74,77), 
           into = c("bl","area","species","red","stn_nr","catch_year","cohort","sex",
                    "sample_nr","number_rows","growth_catch_year","final_length","X1",
                    "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
                    "X14","X15","X16","X17")) %>% 
  mutate_at(c("red","stn_nr","catch_year","cohort","sex",
              "sample_nr","number_rows","final_length","X1",
              "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
              "X14","X15","X16","X17"), as.numeric) 

# Now create a long data frame and use the col names that are in the .xls data
colnames(JMAB84)

JMAB84 <- JMAB84 %>%
  pivot_longer(col = c(13:29), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 1) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm) %>% 
  mutate(catch_year = ifelse(catch_year > 10, paste(19, catch_year, sep = ""), paste(200, catch_year, sep = ""))) %>%
  mutate_at("catch_year", as.numeric) %>% 
  mutate(cohort = ifelse(cohort > 10, paste(19, cohort, sep = ""), paste(200, cohort, sep = ""))) %>% 
  mutate_at("cohort", as.numeric) %>% 
  mutate(age = catch_year - cohort)

# Create ID column
JMAB84 <- JMAB84 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "_")) # add date + gear here as well


#**** Read in the .xls files =======================================================
# And these files that are in csv
JMAB_63_81_and_85_88 <- read.csv("data/Kvädöfjärden/JMabbo_1963to1981and1985to1988.csv", header = TRUE, sep = ";")
JMAB_82_83 <- read.csv("data/Kvädöfjärden/JMabbo_1982to1983.csv", header = TRUE, sep = ";")
JMAB_86 <- read.csv("data/Kvädöfjärden/JMabbo_1986.csv", header = TRUE, sep = ";") 
JMAB_87_and_89_19 <- read.csv("data/Kvädöfjärden/JMabbo_1987and1989to2019.csv", header = TRUE, sep = ";")
JMabbo_19_20_gear10 <- read.csv("data/Kvädöfjärden/JMabbo_2019to2020_gear10.csv", header = TRUE, sep = ";")
JMabbo_19_20_gear52 <- read.csv("data/Kvädöfjärden/JMabbo_2019to2020_gear52.csv", header = TRUE, sep = ";")
JMabbo_19_20_gear64_aug <- read.csv("data/Kvädöfjärden/JMabbo_2019to2020_gear64_aug.csv", header = TRUE, sep = ";")
JMabbo_19_20_gear64_okt <- read.csv("data/Kvädöfjärden/JMabbo_2019to2020_gear64_okt.csv", header = TRUE, sep = ";")

# Remove some problematic columns we any don't use, and standardize names
JMAB_63_81_and_85_88 <- JMAB_63_81_and_85_88 %>% dplyr::select(-Total.vikt, -Somatisk.vikt)
JMAB_82_83 <- JMAB_82_83 %>% dplyr::select(-Total.vikt, -Somatisk.vikt)
JMAB_86 <- JMAB_86 %>% dplyr::select(-Total.vikt, -Somatisk.vikt)
JMAB_87_and_89_19 <- JMAB_87_and_89_19 %>% dplyr::select(-Total.vikt, -Somatisk.vikt)
JMabbo_19_20_gear10 <- JMabbo_19_20_gear10 %>% dplyr::select(-Total.vikt, -Somatisk.vikt)
JMabbo_19_20_gear52 <- JMabbo_19_20_gear52 %>% dplyr::select(-Total.vikt, -Somatisk.vikt)
JMabbo_19_20_gear64_aug <- JMabbo_19_20_gear64_aug %>% dplyr::select(-Total.vikt, -Somatisk.vikt)
JMabbo_19_20_gear64_okt <- JMabbo_19_20_gear64_okt %>% dplyr::select(-Total.vikt, -Somatisk.vikt)

JM_63_20 <- bind_rows(JMAB_63_81_and_85_88,
                      JMAB_82_83,
                      JMAB_86,
                      JMAB_87_and_89_19,
                      JMabbo_19_20_gear10,
                      JMabbo_19_20_gear52,
                      JMabbo_19_20_gear64_aug,
                      JMabbo_19_20_gear64_okt)

# First make sure all lengths at age columns are numeric
JM_63_20 <- JM_63_20 %>% 
  rename("År1" = "Tillväxt..mm.år.1") %>% 
  mutate_at(c("År1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
              "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) 

# Columns 17-36 should be gathered
glimpse(JM_63_20)
colnames(JM_63_20)

JM_63_20 <- JM_63_20 %>%
  pivot_longer(col = c(17:36), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm)

# Rename variables
JM_63_20 <- JM_63_20 %>% 
  rename("catch_year" = "Fiskeår",
         "age" = "Ålder",
         "species" = "Art",
         "gear" = "Redskap.kod",
         "sex" = "Kön",
         "sample_nr" = "Löpnr",
         "final_length" = "Total.längd.mm") %>% 
  mutate(area = "JM",
         cohort = catch_year - age)

# Create ID column
JM_63_20 <- JM_63_20 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))

unique(is.na(JM_63_20$length_mm))
unique(is.na(JM_63_20$final_length))
unique(is.na(JM_63_20$age))
unique(is.na(JM_63_20$catch_year))

#**** Bind rows ====================================================================
JM_63_20$source <- "xls"
JMAB84$source <- "txt" # for any other area we might want to xls KUL or xls G

JMAB84$sex <- as.character(JMAB84$sex)

JM_63_20 <- bind_rows(JM_63_20, JMAB84)

# Add in column that indicates if the length is from a + reading or not
JM_63_20 <- JM_63_20 %>% 
  mutate(age_ring = ifelse(reading_no > age, "N", "Y"))


#**** Plot =========================================================================
# Plot # of samples per catch year
JM_63_20 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year, source) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n, fill = source)) + geom_bar(stat = "identity") 

# Plot length-at-age over catch_year
JM_63_20 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm, color = source)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_wrap(~factor(reading_no), scales = "free_y")

# Plot length-at-age over catch_year
JM_63_20 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


#** Muskö ==========================================================================
#**** Read in the .txt files =======================================================
MUAB91 <- read_delim("data/Muskö/MUTAB91.txt", delim = "\t", col_names = FALSE)  
MUAB92 <- read_delim("data/Muskö/MUTAB92.txt", delim = "\t", col_names = FALSE)  
MUAB93 <- read_delim("data/Muskö/MUTAB93.txt", delim = "\t", col_names = FALSE)  
MUAB94 <- read_delim("data/Muskö/MUTAB94.txt", delim = "\t", col_names = FALSE)
MUAB95 <- read_delim("data/Muskö/MUTAB95.txt", delim = "\t", col_names = FALSE)  
MUAB96 <- read_delim("data/Muskö/MUTAB96.txt", delim = "\t", col_names = FALSE)  
MUAB97 <- read_delim("data/Muskö/MUTAB97.txt", delim = "\t", col_names = FALSE)  
MUAB98 <- read_delim("data/Muskö/MUTAB98.txt", delim = "\t", col_names = FALSE)  
MUAB99 <- read_delim("data/Muskö/MUTAB99.txt", delim = "\t", col_names = FALSE)  
MUAB00 <- read_delim("data/Muskö/MUTAB00.txt", delim = "\t", col_names = FALSE)  
MUAB01 <- read_delim("data/Muskö/MUTAB01.txt", delim = "\t", col_names = FALSE)  

MUAB_91_01 <- bind_rows(MUAB91, MUAB92, MUAB93, MUAB94, MUAB95, MUAB96,
                        MUAB97, MUAB98, MUAB99, MUAB00, MUAB01)

MUAB_91_01 <- MUAB_91_01 %>% 
  separate(X1, 
           sep = c(2,4,8,10,13,15,17,18,21,22,23,26,29,32,35,38,41,44,47,50,53,56,59,
                   62,65,68,71,74,77), 
           into = c("bl","area","species","red","stn_nr","catch_year","cohort","sex",
                    "sample_nr","number_rows","growth_catch_year","final_length","X1",
                    "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
                    "X14","X15","X16","X17")) %>% 
  mutate_at(c("red","stn_nr","catch_year","cohort","sex",
              "sample_nr","number_rows","final_length","X1",
              "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
              "X14","X15","X16","X17"), as.numeric) 

# Now create a long data frame and use the col names that are in the .xls data
colnames(MUAB_91_01)

MUAB_91_01 <- MUAB_91_01 %>%
  pivot_longer(col = c(13:29), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 1) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm) %>% 
  mutate(catch_year = ifelse(catch_year > 10, paste(19, catch_year, sep = ""), paste(200, catch_year, sep = ""))) %>%
  mutate_at("catch_year", as.numeric) %>% 
  mutate(cohort = ifelse(cohort > 10, paste(19, cohort, sep = ""), paste(200, cohort, sep = ""))) %>% 
  mutate_at("cohort", as.numeric) %>% 
  mutate(age = catch_year - cohort)

# Create ID column
MUAB_91_01 <- BSAB_91_01 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "_")) # add date + gear here as well


#**** Read in the .xls files =======================================================
MUabboa2002 <- readxl::read_xls("data/Muskö/MUabbot2002.xls")
MUabboa2003 <- readxl::read_xls("data/Muskö/MUabbot2003p.xls")
MUabboa2004 <- readxl::read_xls("data/Muskö/MUabbot2004p.xls")

sort(colnames(MUabboa2003))
sort(colnames(MUabboa2003))
sort(colnames(MUabboa2004))

MUabboa2002$Analysdatum <- as.character(MUabboa2002$Analysdatum)
MUabboa2003$Analysdatum <- as.character(MUabboa2003$Analysdatum)
MUabboa2004$Analysdatum <- as.character(MUabboa2004$Analysdatum)

# They look the same actually
MUAB_02_04 <- bind_rows(MUabboa2002,
                        MUabboa2003,
                        MUabboa2004)

# Make sure all lengths at age columns are numeric
MUAB_02_04 <- MUAB_02_04 %>% 
  mutate_at(c("År1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
              "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) 

# Columns 49-68 should be gathered
glimpse(MUAB_02_04)
colnames(MUAB_02_04)

MUAB_02_04 <- MUAB_02_04 %>%
  pivot_longer(col = c(49:68), names_to = "age_temp", values_to = "op_length") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(op_length)

# Rename variables
MUAB_02_04 <- MUAB_02_04 %>% 
  rename("catch_year" = "Fångstår",
         "age" = "Ålder",
         "area" = "Areakod",
         "species" = "Art",
         "gear" = "Redskap",
         "stn_nr" = "Station",
         "cohort" = "Födelseår",
         "sex" = "Kön",
         "sample_nr" = "Löpnummer",
         "final_length" = "Totallängd mm")

# Create ID column
MUAB_02_04 <- MUAB_02_04 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))

# Convert from operculum to length
colnames(MUAB_02_04)

# If there are more readings than the age, it means plus growth was measured. This means
# that the final length is actually the radius, which we need when converting from operculum
# length to fish length
# Verify this:
MUAB_02_04 %>%
  group_by(ID) %>% 
  mutate(n = n(),
         plus_growth = ifelse(n > age, "Y", "N")) %>% 
  ungroup() %>% 
  distinct(plus_growth)

MUAB_02_04 <- MUAB_02_04 %>%
  group_by(ID) %>% 
  mutate(max_op_length = max(op_length)) %>% 
  ungroup() %>% 
  mutate(length_mm = (15.56 + 18.485*op_length*Magnifikation - 0.1004*(op_length*Magnifikation)^2)*final_length /
           ((15.56 + 18.485*max_op_length*Magnifikation - 0.1004*(max_op_length*Magnifikation)^2)))


#**** Bind rows ====================================================================
MUAB_02_04$source <- "xls"
MUAB_91_01$source <- "txt"
MU_91_04 <- bind_rows(MUAB_02_04, MUAB_91_01)

# For merging later
MU_91_04$gear <- as.character(MU_91_04$gear)
MU_91_04$sex <- as.character(MU_91_04$sex)
MU_91_04$Analysdatum <- as.character(MU_91_04$Analysdatum)

# Add in column that indicates if the length is from a + reading or not
MU_91_04 <- MU_91_04 %>% 
  mutate(age_ring = ifelse(reading_no > age, "N", "Y"))


#**** Plot =========================================================================
# Plot # of samples per catch year
MU_91_04 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year, source) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n, fill = source)) + geom_bar(stat = "identity") 

# Plot length-at-age over catch_year
MU_91_04 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm, color = source)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_wrap(~factor(reading_no), scales = "free_y")

# Plot length-at-age over catch_year
MU_91_04 <- MU_91_04 %>% filter(length_mm < 600)

MU_91_04 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


#** Råneå ==========================================================================
# These text files have encoding issues.. I have changed that manually. Originals are saved


#**** Read in the .txt files =======================================================
# These are bl 57
# NVm doesn't matter! can use the same script for both 57 and 67
# RAB85 <- read_delim("data/Råneå/RÅTAB85.TXT", delim = "\t", col_names = FALSE)  
# RAB86 <- read_delim("data/Råneå/RÅTAB86.TXT", delim = "\t", col_names = FALSE)  

# Not using these since it's a different gear!

# These are bl 67
RAB90 <- read_delim("data/Råneå/RÅTAB90.TXT", delim = "\t", col_names = FALSE)
RAB96 <- read_delim("data/Råneå/RÅTAB96.TXT", delim = "\t", col_names = FALSE)
RAB97 <- read_delim("data/Råneå/RÅTAB97.TXT", delim = "\t", col_names = FALSE) 
RAB98 <- read_delim("data/Råneå/RÅTAB98.TXT", delim = "\t", col_names = FALSE) 
RAB99 <- read_delim("data/Råneå/RÅTAB99.TXT", delim = "\t", col_names = FALSE)
RAB00 <- read_delim("data/Råneå/RÅTAB00.TXT", delim = "\t", col_names = FALSE)
RAB01 <- read_delim("data/Råneå/RÅTAB01.TXT", delim = "\t", col_names = FALSE) 

# They have different sheets (i.e., bl, the first two values of the string), so I can't match them all at once
#RA_85_86 <- bind_rows(RAB85, RAB86) # bl 57

RA_90_01 <- bind_rows(RAB90, RAB96, RAB97, RAB98, RAB99, RAB00, RAB01)

# Start with bl 67
RA_90_01 <- RA_90_01 %>% 
  separate(X1, 
           sep = c(2,4,8,10,13,15,17,18,21,22,23,26,29,32,35,38,41,44,47,50,53,56,59,
                   62,65,68,71,74,77,80,83,86), 
           into = c("bl","area","species","gear","stn_nr","catch_year","cohort","sex",
                    "sample_nr","number_rows","growth_catch_year","final_length","X1",
                    "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
                    "X14","X15","X16","X17","X18","X19","X20")) %>% 
  mutate_at(c("gear","stn_nr","catch_year","cohort","sex",
              "sample_nr","number_rows","final_length","X1",
              "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
              "X14","X15","X16","X17","X18","X19","X20"), as.numeric) 

glimpse(RA_90_01)

# Now to bl 57 (old)
# RA_85_86 <- RA_85_86 %>% 
#   separate(X1, 
#            sep = c(2,4,8,10,11,13,15,17,18,21,
#                    22,23,26,29,32,35,38,41,44,47,50,53,57,60,63,67,70,73,74,77,80,83), 
#            into = c("bl","area","species","red","sektion","lokal","catch_year","cohort","sex","sample_nr",
#                     "extra_rows","plus","final_length","X1","X2","X3","X4","X5","X6","X7",
#                     "X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")) %>%
#   mutate_at(c("red","catch_year","cohort","sex",
#               "sample_nr","final_length","X1","X2","X3","X4","X5","X6","X7",
#               "X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20"), as.numeric) 

# Bind rows
#RA_85_01 <- bind_rows(RA_90_01, RA_85_86)

# Now create a long data frame and use the col names that are in the .xls data
colnames(RA_90_01)

RA_90_01 <- RA_90_01 %>%
  pivot_longer(col = c(13:32), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 1) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm) %>% 
  mutate(catch_year = ifelse(catch_year > 10, paste(19, catch_year, sep = ""), paste(200, catch_year, sep = ""))) %>%
  mutate_at("catch_year", as.numeric) %>% 
  mutate(cohort = ifelse(cohort > 10, paste(19, cohort, sep = ""), paste(200, cohort, sep = ""))) %>% 
  mutate_at("cohort", as.numeric) %>% 
  mutate(age = catch_year - cohort)

# Create ID column
RA_90_01 <- RA_90_01 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "_")) # add date + gear here as well


#**** Read in the .xls files =======================================================
RAabboa2003 <- readxl::read_xls("data/Råneå/RAabbot2003p.xls")
RAabboa2004 <- readxl::read_xls("data/Råneå/RAabbot2004p.xls")
RAabboa2005 <- readxl::read_xls("data/Råneå/RAabbot2005p.xls")

RAabboa_08_10 <- read.csv("data/Råneå/RAabbo_2008and2010.csv", header = TRUE, sep = ";") 

RAabboa2003$Kön <- as.character(RAabboa2003$Analysdatum)
RAabboa2004$Kön <- as.character(RAabboa2004$Analysdatum)
RAabboa2005$Kön <- as.character(RAabboa2005$Analysdatum)

# Totallängdmm is different in the excel files, use a common name
colnames(RAabboa2004)
RAabboa2004[1, 25]
RAabboa2004$Totallängdmm <- data.frame(RAabboa2004)[, 25]

colnames(RAabboa2003)
RAabboa2003[1, 25]
RAabboa2003$Totallängdmm <- data.frame(RAabboa2003)[, 25]

RAabboa2004$Analysdatum <- as.numeric(RAabboa2004$Analysdatum)
RAabboa2005$Analysdatum <- as.numeric(RAabboa2005$Analysdatum)

RAabboa_08_10 <- RAabboa_08_10 %>% rename("Fångstår" = "Fiskeår")

RAabboa2005$Åldersstruktur <- as.numeric(RAabboa2005$Åldersstruktur)

# Check important colnames (they need to match before binding, else they get NA)
sort(colnames(RAabboa2003))
sort(colnames(RAabboa2004))
sort(colnames(RAabboa2005))
sort(colnames(RAabboa_08_10))

RAabboa_08_10$Areakod <- "RÅ"
RAabboa_08_10$Födelseår <- RAabboa_08_10$Fångstår - RAabboa_08_10$Ålder
RAabboa_08_10$Totallängdmm <- RAabboa_08_10$Total.längd.mm

RA_03_10 <- bind_rows(RAabboa2003,
                      RAabboa2004,
                      RAabboa2005,
                      RAabboa_08_10)

# Go from wide to long data
str(RA_03_10)

# First make sure all lengths at age columns are numeric
RA_03_10 <- RA_03_10 %>% 
  mutate_at(c("År1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
              "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) 

# Columns 52-71 should be gathered
glimpse(RA_03_10)
colnames(RA_03_10)

RA_03_10 <- RA_03_10 %>%
  pivot_longer(col = c(52:71), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm)

# Rename variables
RA_03_10 <- RA_03_10 %>% 
  rename("catch_year" = "Fångstår",
         "age" = "Ålder",
         "area" = "Areakod",
         "species" = "Art",
         "gear" = "Redskap",
         "stn_nr" = "Station",
         "cohort" = "Födelseår",
         "sex" = "Kön",
         "sample_nr" = "Löpnummer",
         "final_length" = "Totallängdmm")

# Create ID column
RA_03_10 <- RA_03_10 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))

# Now, if the years are 03, 04, 05, length_mm is in fact not the actual length!
# It's the operculum length. I need to correct that for those years
# Convert from operculum to length
colnames(RA_03_10)

# If there are more readings than the age, it means plus growth was measured. This means
# that the final length is actually the radius, which we need when converting from operculum
# length to fish length
# Verify this:
RA_03_10 %>%
  group_by(ID) %>% 
  mutate(n = n(),
         plus_growth = ifelse(n > age, "Y", "N")) %>% 
  ungroup() %>% 
  filter(plus_growth == "N") %>% 
  as.data.frame()

RA_03_10 %>% group_by(catch_year) %>% distinct(Magnifikation)
RA_03_10 %>% group_by(catch_year) %>% distinct(is.na(length_mm))

RA_03_10 <- RA_03_10 %>%
  group_by(ID) %>% 
  mutate(n = n(),
         plus_growth = ifelse(n > age, "Y", "N")) %>% 
  ungroup() %>% 
  filter(plus_growth == "Y") %>% # All other years have plus growth, so this only removes the indivudals can't can't get a length-measurement
  group_by(ID) %>% 
  mutate(max_op_length = max(length_mm)) %>% # In contrast to the xls files where ALL lengths had to be converted, this is only for certain years. Hence I didn't bother to call lengths "op_length"
  ungroup() %>% 
  mutate(length_mm = ifelse(catch_year %in% c(2003, 2004, 2005),
                            (15.56 + 18.485*length_mm*Magnifikation - 0.1004*(length_mm*Magnifikation)^2)*final_length /
                              ((15.56 + 18.485*max_op_length*Magnifikation - 0.1004*(max_op_length*Magnifikation)^2)),
                            length_mm))

#**** Bind rows ====================================================================
RA_03_10$source <- "xls"
RA_90_01$source <- "txt"

# Change variable type so that we can join all data later
RA_90_01$sex <- as.character(RA_90_01$sex)
RA_03_10$gear <- as.character(RA_03_10$gear)
RA_90_01$gear <- as.character(RA_90_01$gear)

RA_90_10 <- bind_rows(RA_03_10, RA_90_01)

RA_90_10$Analysdatum <- as.character(RA_90_10$Analysdatum)

ggplot(RA_90_10, aes(catch_year, length_mm)) + 
  geom_point()

# Add in column that indicates if the length is from a + reading or not
RA_90_10 <- RA_90_10 %>% 
  mutate(age_ring = ifelse(reading_no > age, "N", "Y"))

#**** Plot =========================================================================
# Plot # of samples per catch year
RA_90_10 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year, source) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n, fill = source)) + geom_bar(stat = "identity") 

# Plot length-at-age over catch_year 
RA_90_10 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm, color = source)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_wrap(~factor(reading_no), scales = "free_y")

# Plot length-at-age
RA_90_10 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


#** Simpevarp ======================================================================
#**** Read in the .xls files =======================================================
#**** Old file from Janne ==========================================================
SIAB_63_08 <- read.csv("data/Simpevarp/SI Individdata 1963-2008.csv", header = TRUE, sep = ";") 

# Here we assume that section 1 and 5 are two distinct areas
unique(SIAB_63_08$SEK)

SIAB_63_08 <- SIAB_63_08 %>% mutate(area = ifelse(SEK == 1, "SI_EK", SEK),
                                    area = ifelse(SEK == 5, "SI_HA", area))

# Add ID before making the data long
SIAB_63_08 <- SIAB_63_08 %>% mutate(ind_id = 1:n())

SIAB_63_08

# Go from wide to long data
glimpse(SIAB_63_08)

# First make sure all lengths at age columns are numeric
SIAB_63_08 <- SIAB_63_08 %>% 
  mutate_at(c("Y1","Y2","Y3","Y4","Y5","Y6","Y7","Y8","Y9","Y10","Y11","Y12",
              "Y13","Y14","Y15","Y16","Y17","Y18"), as.numeric)

# Columns 19-38 should be gathered
colnames(SIAB_63_08)
SIAB_63_08[1, c(15:32)]

SIAB_63_08 <- SIAB_63_08 %>%
  pivot_longer(col = c(15:32), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 1) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm)

# Rename variables
SIAB_63_08 <- SIAB_63_08 %>% 
  rename("bl" = "BL", 
         "section" = "SEK", 
         "catch_year" = "CY",
         "age" = "AGE",
         "species" = "ART",
         "gear" = "RED",
         "sex" = "KÖN",
         "sample_nr" = "ind_id",
         "final_length" = "LÄNGD") %>% 
  mutate(cohort = catch_year - age,
         age_ring = ifelse(reading_no > age, "N", "Y")) %>% 
  filter(area %in% c("SI_EK", "SI_HA"))

# Create ID column
SIAB_63_08 <- SIAB_63_08 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))


#**** Newer data (KUL) =============================================================
SIAB_19 <- read.csv("data/Simpevarp/SIabbo_Hamne2019.csv", header = TRUE, sep = ";") %>% 
  mutate(area = "SI_HA") 

SIAB_07_18 <- read.csv("data/Simpevarp/SIabbo_2007to2018_Hamne.csv", header = TRUE, sep = ";") %>% 
  mutate(area = "SI_HA") %>% 
  mutate_at(c("Total.vikt", "Somatisk.vikt"), as.numeric)

SIAB_07_19 <- read.csv("data/Simpevarp/SIabbo_2007to2019_Ek.csv", header = TRUE, sep = ";") %>% 
  mutate(area = "SI_EK")  

SIAB_07_19_tot <- bind_rows(SIAB_19, SIAB_07_18, SIAB_07_19)

# Go from wide to long data
glimpse(SIAB_07_19_tot)
colnames(SIAB_07_19_tot)

# First make sure all lengths at age columns are numeric
SIAB_07_19_tot <- SIAB_07_19_tot %>% 
  mutate_at(c("Tillväxt..mm.år.1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
              "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) %>% 
  rename("År1" = "Tillväxt..mm.år.1")

# Columns 19-38 should be gathered
colnames(SIAB_07_19_tot)
SIAB_07_19_tot[1, c(19:38)]

SIAB_07_19_tot <- SIAB_07_19_tot %>%
  pivot_longer(col = c(19:38), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm)

# Rename variables
SIAB_07_19_tot <- SIAB_07_19_tot %>% 
  rename("catch_year" = "Fiskeår",
         "age" = "Ålder",
         "species" = "Art",
         "gear" = "Redskap.kod",
         "sex" = "Kön",
         "sample_nr" = "Löpnr",
         "final_length" = "Total.längd.mm") %>% 
  mutate(cohort = catch_year - age,
         age_ring = ifelse(reading_no > age, "N", "Y"))

# Create ID column
SIAB_07_19_tot <- SIAB_07_19_tot %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))


#**** Bind rows ====================================================================
SIAB_07_19_tot$source <- "KUL"
SIAB_63_08$source <- "Janne"

SIAB_63_08 <- SIAB_63_08 %>% mutate(gear = as.character(gear),
                                    sex = as.character(sex))

SI_63_19 <- bind_rows(SIAB_07_19_tot, SIAB_63_08)

#**** Plot =========================================================================
# Plot # of samples per catch year
SI_63_19 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year, source) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n, fill = source)) + geom_bar(stat = "identity") 

# Plot length-at-age over birth_year
SI_63_19 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm, color = source)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_grid(area~factor(reading_no), scales = "free_y")

# Plot length-at-age
SI_63_19 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


#** Torhamn ========================================================================

#**** Read in the .xls files =======================================================
THAB_03_20 <- read.csv("data/Torhamn/THabbo.csv", header = TRUE, sep = ";")
head(THAB_03_20)

# Go from wide to long data
glimpse(THAB_03_20)

# First make sure all lengths at age columns are numeric
THAB_03_20 <- THAB_03_20 %>% 
  mutate_at(c("Tillväxt..mm.år.1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
              "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) %>% 
  rename("År1" = "Tillväxt..mm.år.1")

# Columns 19-38 should be gathered
colnames(THAB_03_20)
THAB_03_20[1, c(19:38)]

THAB_03_20 <- THAB_03_20 %>%
  pivot_longer(col = c(19:38), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm)

# Rename variables
THAB_03_20 <- THAB_03_20 %>% 
  rename("catch_year" = "Fiskeår",
         "age" = "Ålder",
         "species" = "Art",
         "gear" = "Redskap.kod",
         "sex" = "Kön",
         "sample_nr" = "Löpnr",
         "final_length" = "Total.längd.mm") %>% 
  mutate(cohort = catch_year - age,
         area = "TH",
         station = NA,
         age_ring = ifelse(reading_no > age, "N", "Y"))

# Create ID column
TH_03_20 <- THAB_03_20 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))

#**** Plot =========================================================================
# Plot # of samples per catch year
TH_03_20 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n)) + geom_bar(stat = "identity") 

# Plot length-at-age over birth_year
TH_03_20 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_wrap(~factor(reading_no), scales = "free_y")

# Plot length-at-age
TH_03_20 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


#** Vinö ==========================================================================
#**** Read in the .txt files =======================================================
VIAB95 <- read_delim("data/Vinö/VNTAB95.TXT", delim = "\t", col_names = FALSE)  
VIAB96 <- read_delim("data/Vinö/VNTAB96.TXT", delim = "\t", col_names = FALSE)
VIAB97 <- read_delim("data/Vinö/VNTAB97.TXT", delim = "\t", col_names = FALSE)
VIAB98 <- read_delim("data/Vinö/VNTAB98.TXT", delim = "\t", col_names = FALSE)
VIAB99 <- read_delim("data/Vinö/VNTAB99.TXT", delim = "\t", col_names = FALSE) 
VIAB00 <- read_delim("data/Vinö/Vntab00.TXT", delim = "\t", col_names = FALSE) 
VIAB01 <- read_delim("data/Vinö/VNTAB01.TXT", delim = "\t", col_names = FALSE) 

VIAB_95_01 <- bind_rows(VIAB95, VIAB96, VIAB97, VIAB98, VIAB99, VIAB00, VIAB01)

VIAB_95_01 <- VIAB_95_01 %>% 
  separate(X1, 
           sep = c(2,4,8,10,13,15,17,18,21,22,23,26,29,32,35,38,41,44,47,50,53,56,59,
                   62,65,68,71,74,77), 
           into = c("bl","area","species","red","stn_nr","catch_year","cohort","sex",
                    "sample_nr","number_rows","growth_catch_year","final_length","X1",
                    "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
                    "X14","X15","X16","X17")) %>% 
  mutate_at(c("red","stn_nr","catch_year","cohort","sex",
              "sample_nr","number_rows","final_length","X1",
              "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
              "X14","X15","X16","X17"), as.numeric) 

VIAB_95_01 %>% distinct(bl)

glimpse(VIAB_95_01)

# Now create a long data frame and use the col names that are in the .xls data
colnames(VIAB_95_01)

VIAB_95_01 <- VIAB_95_01 %>%
  pivot_longer(col = c(13:29), names_to = "age_temp", values_to = "length_mm") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 1) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(length_mm) %>% 
  mutate(catch_year = ifelse(catch_year > 10, paste(19, catch_year, sep = ""), paste(200, catch_year, sep = ""))) %>%
  mutate_at("catch_year", as.numeric) %>% 
  mutate(cohort = ifelse(cohort > 10, paste(19, cohort, sep = ""), paste(200, cohort, sep = ""))) %>% 
  mutate_at("cohort", as.numeric) %>% 
  mutate(age = catch_year - cohort)

# Create ID column
VIAB_95_01 <- VIAB_95_01 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "_")) # add date + gear here as well


#**** Read in the .xls files =======================================================
VNAB_02 <- readxl::read_xls("data/Vinö/VNabbot2002.xls", sheet = 1)
# The warning is due to a date

# Go from wide to long data
str(VNAB_02)

# First make sure all lengths at age columns are numeric
VNAB_02 <- VNAB_02 %>% 
  mutate_at(c("År1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
              "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) 

# Columns 52-71 should be gathered
glimpse(VNAB_02)
colnames(VNAB_02)

VNAB_02 <- VNAB_02 %>%
  pivot_longer(col = c(52:71), names_to = "age_temp", values_to = "op_length") %>% 
  separate("age_temp", c("NA", "reading_no"), sep = 2) %>% 
  mutate_at("reading_no", as.numeric) %>% 
  drop_na(op_length)

# Rename variables
VNAB_02 <- VNAB_02 %>% 
  rename("catch_year" = "Fångstår",
         "age" = "Ålder",
         "area" = "Areakod",
         "species" = "Art",
         "gear" = "Redskap",
         "stn_nr" = "Station",
         "cohort" = "Födelseår",
         "sex" = "Kön",
         "sample_nr" = "Löpnummer",
         "final_length" = "Totallängd mm")

# Create ID column
VNAB_02 <- VNAB_02 %>% mutate(ID = paste(catch_year, sample_nr, area, sep = "."))

# Convert from operculum to length
colnames(VNAB_02)

# If there are more readings than the age, it means plus growth was measured. This means
# that the final length is actually the radius, which we need when converting from operculum
# length to fish length
# Verify this:
VNAB_02 %>%
  group_by(ID) %>% 
  mutate(n = n(),
         plus_growth = ifelse(n > age, "Y", "N")) %>% 
  ungroup() %>% 
  distinct(plus_growth)

VNAB_02 <- VNAB_02 %>%
  group_by(ID) %>% 
  mutate(max_op_length = max(op_length)) %>% 
  ungroup() %>% 
  mutate(length_mm = (15.56 + 18.485*op_length*Magnifikation - 0.1004*(op_length*Magnifikation)^2)*final_length /
           ((15.56 + 18.485*max_op_length*Magnifikation - 0.1004*(max_op_length*Magnifikation)^2)))

#**** Bind rows ====================================================================
VNAB_02$source <- "xls"
VIAB_95_01$source <- "txt" # for any other area we might want to xls KUL or xls G
VI_95_02 <- bind_rows(VNAB_02, VIAB_95_01)

# Add in column that indicates if the length is from a + reading or not
VI_95_02 <- VI_95_02 %>% 
  mutate(age_ring = ifelse(reading_no > age, "N", "Y"))

# Change variable type so that we can join all data later
VI_95_02$gear <- as.character(VI_95_02$gear)
VI_95_02$sex <- as.character(VI_95_02$sex)
VI_95_02$Analysdatum <- as.character(VI_95_02$Analysdatum)


#**** Plot =========================================================================
# Plot # of samples per catch year
VI_95_02 %>% 
  filter(age_ring == "Y") %>% 
  group_by(catch_year, source) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(catch_year, n, fill = source)) + geom_bar(stat = "identity") 

# Plot length-at-age over birth_year
VI_95_02 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(catch_year, length_mm, color = source)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  facet_wrap(~factor(reading_no), scales = "free_y")

# Plot length-at-age
VI_95_02 <- VI_95_02 %>% mutate(keep = ifelse(reading_no < 6 & length_mm > 400, "N", "Y")) %>% 
  filter(keep == "Y")

VI_95_02 %>% 
  filter(age_ring == "Y") %>% 
  ggplot(., aes(factor(reading_no), length_mm, color = catch_year)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(color = "black", method = "gam", formula = y ~ s(x, k = 3)) + 
  scale_color_viridis()


# C. JOIN DATA =====================================================================
d_full <- bind_rows(BS_91_06,
                    SI_63_19,
                    TH_03_20,
                    VI_95_02,
                    RA_90_10,
                    MU_91_04,
                    JM_63_20,
                    HO_88_20,
                    FM_71_20)

d_full %>% group_by(area) %>% distinct(is.na(length_mm))
d_full %>% group_by(area) %>% distinct(is.na(area))
d_full %>% group_by(area) %>% distinct(is.na(catch_year))

# Make sure non of these columns below have NA due to name mismatch when joining! They shouldn't have any NA
# See Kvädöfjärden for how I can check that after re-naming and joining
# This includes also gear!

#ALSO
# WHen i mix old and new csv-files, rename after length at age 1 to ÅR1, like I do in forsmark. Else they will become NA when I bind_rows
# FMabbo_02_and_17_20_gear09_aug <- read.csv("data/Forsmark/FMabbo_2002and2017to2020_gear09aug.csv", header = TRUE, sep = ";") %>% 
#   rename("År1" = "Tillväxt..mm.år.1")

d <- d_full %>%
  dplyr::select(length_mm, reading_no, age, age_ring, area, catch_year,
                cohort, final_length, gear, ID, sex) %>% 
  rename(age_bc = reading_no,
         age_catch = age)


# D. EXPLORE DATA ==================================================================

# MOVE THIS TO ANOTHER SCRIPT!
# FIRST PLOT MAP WITH POINTS; LABELLED WITH UNIQUE YEARS AND n
# FOR EACH AREA PLOT MEAN SIZE AT AGE
# "VBGE" CURVES
# ETC ETC

# Plot sample size over cohort
d %>% 
  group_by(cohort, area) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(cohort, n, fill = area)) + geom_bar(stat = "identity") + 
  NULL

# Calculate time-slopes by age and area
d %>%
  group_by(age_bc, area) %>% 
  mutate(length_mm_ct = length_mm - mean(length_mm)) %>% # center length at age for comparison across ages
  ggplot(., aes(catch_year, length_mm_ct, color = area)) + 
  geom_point(size = 0.1, alpha = 0.5) + 
  facet_wrap(~age_bc) +
  #stat_smooth(inherit.aes = FALSE, aes(catch_year, length_mm_ct)) +
  NULL

time_slopes_by_year_area <- d %>%
  group_by(age_bc, area) %>% # center length at age for comparison across ages
  mutate(length_mm_ct = length_mm / mean(length_mm)) %>% 
  ungroup() %>% 
  mutate(id = paste(age_bc, area, sep = ";")) %>%
  split(.$id) %>%
  purrr::map(~lm(length_mm_ct ~ catch_year, data = .x)) %>%
  purrr::map_df(broom::tidy, .id = 'id') %>%
  filter(term == 'catch_year') %>% 
  separate(id, into = c("age_bc", "area"), sep = ";") %>% 
  mutate(upr = estimate + std.error*2, lwr = estimate - std.error*2) %>% 
  mutate(id = paste(age_bc, area, sep = ";"))

time_slopes_by_year_area

# Add sample size so that we can filter on that
sample_size <- d %>% 
  group_by(age_bc, area) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(id = paste(age_bc, area, sep = ";")) %>% 
  dplyr::select(n, id)

# Join sample size
time_slopes_by_year_area <- left_join(time_slopes_by_year_area, sample_size)

# Plot effect sizes
time_slopes_by_year_area %>%
  filter(n > 30) %>% 
  ggplot(., aes(reorder(age_bc, as.numeric(age_bc)), estimate, color = factor(area))) + 
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2,
                position = position_dodge(width = 0.4)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~factor(area), scales = "free") + 
  NULL

