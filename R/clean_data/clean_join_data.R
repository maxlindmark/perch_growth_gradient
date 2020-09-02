#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.08.28: Max Lindmark
#
# Code to read and join raw backcalculated data for each area
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

# Set a nice and clean theme
theme_set(ggsidekick::theme_sleek())


# B. READ DATA =====================================================================
#** Biotest ========================================================================


#** Brunskär =======================================================================
#**** Read in the .txt files =======================================================
BSTAB91 <- read_delim("data/Brunskär/BSTAB91.txt", delim = "\t", col_names = FALSE)  
BSTAB92 <- read_delim("data/Brunskär/BSTAB92.txt", delim = "\t", col_names = FALSE)
BSTAB93 <- read_delim("data/Brunskär/BSTAB93.txt", delim = "\t", col_names = FALSE)
BSTAB94 <- read_delim("data/Brunskär/BSTAB94.txt", delim = "\t", col_names = FALSE)
BSTAB95 <- read_delim("data/Brunskär/BSTAB95.txt", delim = "\t", col_names = FALSE) 
BSTAB96 <- read_delim("data/Brunskär/BSTAB96.txt", delim = "\t", col_names = FALSE) 
BSTAB97 <- read_delim("data/Brunskär/BSTAB97.txt", delim = "\t", col_names = FALSE) 
BSTAB98 <- read_delim("data/Brunskär/BSTAB98.txt", delim = "\t", col_names = FALSE) 
BSTAB99 <- read_delim("data/Brunskär/BSTAB99.txt", delim = "\t", col_names = FALSE) 
BSTAB00 <- read_delim("data/Brunskär/BSTAB00.txt", delim = "\t", col_names = FALSE)
BSTAB01 <- read_delim("data/Brunskär/Bstab01.txt", delim = "\t", col_names = FALSE)

BSTAB_91_01 <- bind_rows(BSTAB91, BSTAB92, BSTAB93, BSTAB94, BSTAB95, BSTAB96,
                         BSTAB97, BSTAB98, BSTAB99, BSTAB00, BSTAB01)

BSTAB_91_01 <- BSTAB_91_01 %>% 
  separate(X1, 
           sep = c(2,4,8,10,13,15,17,18,21,22,23,26,29,32,35,38,41,44,47,50,53,56,59,
                   62,65,68,71,74,77), 
           into = c("bl","area","art","red","stn.nr","fångstår","födelseår","kön",
                    "provets.nr","antal.rader","tillväxt.fångstår","slutlängd","X1",
                    "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
                    "X14","X15","X16","X17")) %>% 
  mutate_at(c("red","stn.nr","fångstår","födelseår","kön",
            "provets.nr","antal.rader","slutlängd","X1",
            "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
            "X14","X15","X16","X17"), as.numeric) 

# Now create a long data frame and use the col names that are in the .xls data
BSTAB_91_01 <- BSTAB_91_01 %>%
  pivot_longer(col = c(13:29), names_to = "age_temp", values_to = "Length") %>% 
  separate("age_temp", c("NA", "Reading_no"), sep = 1) %>% 
  mutate_at("Reading_no", as.numeric) %>% 
  drop_na(Length) %>% # remove NA lengths, cannot not choose that anymore (possible with gather)
  mutate(catch_year = ifelse(fångstår > 10, paste(19, fångstår, sep = ""), paste(200, fångstår, sep = ""))) %>%
  mutate_at("catch_year", as.numeric) %>% 
  mutate(birth_year = ifelse(födelseår > 10, paste(19, födelseår, sep = ""), paste(200, födelseår, sep = ""))) %>% 
  mutate_at("birth_year", as.numeric) %>% 
  select(-c(fångstår, bl)) %>% 
  rename("Fångstår" = "catch_year",
         "Areakod" = "area",
         "Art" = "art",
         "Redskap" = "red",
         "stn.nr" = "stn.nr",
         "Födelseår" = "birth_year",
         "Kön" = "kön",
         "Löpnummer" = "provets.nr" ,
         "Totallängd mm" = "slutlängd") %>%
  mutate(Ålder = Fångstår - Födelseår)


# Create ID column
BSTAB_91_01 <- BSTAB_91_01 %>% mutate(ID = paste(Fångstår, Löpnummer, Areakod, sep = "."))


#**** Read in the .xls files =======================================================
# This is not a t-file! (growth)
BSabboa2002 <- readxl::read_xls("data/Brunskär/BSabboa2002.xls")
#data.frame(BSabboa2002) # DON'T FORGET THIS ONE!!!

# These are t-files
BSabboa2003 <- readxl::read_xls("data/Brunskär/BSabbot2003p.xls")
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

BSTAB_03_06 <- bind_rows(BSabboa2003,
                         BSabboa2004,
                         BSabboa2005,
                         BSabboa2006)

# Go from wide to long data
str(BSTAB_03_06)

# First make sure all lengths at age columns are numeric
BSTAB_03_06 <- BSTAB_03_06 %>% 
  mutate_at(c("År1","År2","År3","År4","År5","År6","År7","År8","År9","År10","År11","År12",
            "År13","År14","År15","År16","År17","År18","År19","År20"), as.numeric) 

# Columns 52-71 are gathered
BSTAB_03_06 <- BSTAB_03_06 %>%
  pivot_longer(col = c(52:71), names_to = "age_temp", values_to = "Length") %>% 
  separate("age_temp", c("NA", "Reading_no"), sep = 2) %>% 
  mutate_at("Reading_no", as.numeric) %>% 
  drop_na(Length) # remove NA lengths, cannot not choose that anymore (possible with gather)

# Create ID column
BSTAB_03_06 <- BSTAB_03_06 %>% mutate(ID = paste(Fångstår, Löpnummer, Areakod, sep = "."))

  
#**** Bind rows ====================================================================
BSTAB_03_06$source <- "txt"
BSTAB_91_01$source <- "xls"
BSTAB_91_06 <- bind_rows(BSTAB_03_06, BSTAB_91_01)

# Add in column that indicates if the length is from a + reading or not
BSTAB_91_06 <- BSTAB_91_06 %>% 
  group_by(ID) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(Age_ring = ifelse(Reading_no > Ålder, "N", "Y"))

BSTAB_91_06 %>% 
  filter(ID == "2003.4.BS") %>% 
  as.data.frame()


#**** Plot =========================================================================
# Plot # of ages per individual per birth year
BSTAB_91_06 %>% 
  filter(Age_ring == "Y") %>% 
  group_by(ID) %>% 
  mutate(n = n()) %>% 
  arrange(desc(n)) %>% 
  ggplot(., aes(n)) + 
  geom_bar() + 
  facet_wrap(~Födelseår)

# Plot # of ages per individual per catch year
BSTAB_91_06 %>% 
  filter(Age_ring == "Y") %>% 
  group_by(ID) %>% 
  mutate(n = n()) %>% 
  arrange(desc(n)) %>% 
  ggplot(., aes(n)) + 
  geom_bar() + 
  facet_wrap(~Fångstår)

# Plot length-at-age over birth_year, use only back-calculated age
BSTAB_91_06 %>% 
  filter(Age_ring == "Y") %>% 
  ggplot(., aes(Födelseår, Length, color = source)) + 
  geom_point() + 
  facet_wrap(~factor(Reading_no), scales = "free_y")

# Plot length-at-age over catch year, use only back-calculated age
BSTAB_91_06 %>% 
  filter(Age_ring == "Y") %>% 
  ggplot(., aes(Fångstår, Length)) + 
  geom_point() + 
  facet_wrap(~factor(Reading_no), scales = "free_y")

# The issue here is that it's not length in the new data



#** Finbo ==========================================================================


#** Forsmark =======================================================================


#** Holmön =========================================================================


#** Muskö ==========================================================================


#** Simpevarp ======================================================================


#** Torhamn ========================================================================








