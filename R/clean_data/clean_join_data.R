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


#** Brunsk�r =======================================================================
#**** Read in the .txt files =======================================================
BSTAB91 <- read_delim("data/Brunsk�r/BSTAB91.txt", delim = "\t", col_names = FALSE)  
BSTAB92 <- read_delim("data/Brunsk�r/BSTAB92.txt", delim = "\t", col_names = FALSE)
BSTAB93 <- read_delim("data/Brunsk�r/BSTAB93.txt", delim = "\t", col_names = FALSE)
BSTAB94 <- read_delim("data/Brunsk�r/BSTAB94.txt", delim = "\t", col_names = FALSE)
BSTAB95 <- read_delim("data/Brunsk�r/BSTAB95.txt", delim = "\t", col_names = FALSE) 
BSTAB96 <- read_delim("data/Brunsk�r/BSTAB96.txt", delim = "\t", col_names = FALSE) 
BSTAB97 <- read_delim("data/Brunsk�r/BSTAB97.txt", delim = "\t", col_names = FALSE) 
BSTAB98 <- read_delim("data/Brunsk�r/BSTAB98.txt", delim = "\t", col_names = FALSE) 
BSTAB99 <- read_delim("data/Brunsk�r/BSTAB99.txt", delim = "\t", col_names = FALSE) 
BSTAB00 <- read_delim("data/Brunsk�r/BSTAB00.txt", delim = "\t", col_names = FALSE)
BSTAB01 <- read_delim("data/Brunsk�r/Bstab01.txt", delim = "\t", col_names = FALSE)

BSTAB_91_01 <- bind_rows(BSTAB91, BSTAB92, BSTAB93, BSTAB94, BSTAB95, BSTAB96,
                         BSTAB97, BSTAB98, BSTAB99, BSTAB00, BSTAB01)

BSTAB_91_01 <- BSTAB_91_01 %>% 
  separate(X1, 
           sep = c(2,4,8,10,13,15,17,18,21,22,23,26,29,32,35,38,41,44,47,50,53,56,59,
                   62,65,68,71,74,77), 
           into = c("bl","area","art","red","stn.nr","f�ngst�r","f�delse�r","k�n",
                    "provets.nr","antal.rader","tillv�xt.f�ngst�r","slutl�ngd","X1",
                    "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
                    "X14","X15","X16","X17")) %>% 
  mutate_at(c("red","stn.nr","f�ngst�r","f�delse�r","k�n",
            "provets.nr","antal.rader","slutl�ngd","X1",
            "X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13",
            "X14","X15","X16","X17"), as.numeric) 

# Now create a long data frame and use the col names that are in the .xls data
BSTAB_91_01 <- BSTAB_91_01 %>%
  pivot_longer(col = c(13:29), names_to = "age_temp", values_to = "Length") %>% 
  separate("age_temp", c("NA", "Reading_no"), sep = 1) %>% 
  mutate_at("Reading_no", as.numeric) %>% 
  drop_na(Length) %>% # remove NA lengths, cannot not choose that anymore (possible with gather)
  mutate(catch_year = ifelse(f�ngst�r > 10, paste(19, f�ngst�r, sep = ""), paste(200, f�ngst�r, sep = ""))) %>%
  mutate_at("catch_year", as.numeric) %>% 
  mutate(birth_year = ifelse(f�delse�r > 10, paste(19, f�delse�r, sep = ""), paste(200, f�delse�r, sep = ""))) %>% 
  mutate_at("birth_year", as.numeric) %>% 
  select(-c(f�ngst�r, bl)) %>% 
  rename("F�ngst�r" = "catch_year",
         "Areakod" = "area",
         "Art" = "art",
         "Redskap" = "red",
         "stn.nr" = "stn.nr",
         "F�delse�r" = "birth_year",
         "K�n" = "k�n",
         "L�pnummer" = "provets.nr" ,
         "Totall�ngd mm" = "slutl�ngd") %>%
  mutate(�lder = F�ngst�r - F�delse�r)


# Create ID column
BSTAB_91_01 <- BSTAB_91_01 %>% mutate(ID = paste(F�ngst�r, L�pnummer, Areakod, sep = "."))


#**** Read in the .xls files =======================================================
# This is not a t-file! (growth)
BSabboa2002 <- readxl::read_xls("data/Brunsk�r/BSabboa2002.xls")
#data.frame(BSabboa2002) # DON'T FORGET THIS ONE!!!

# These are t-files
BSabboa2003 <- readxl::read_xls("data/Brunsk�r/BSabbot2003p.xls")
BSabboa2004 <- readxl::read_xls("data/Brunsk�r/BSabbot2004p.xls")
BSabboa2005 <- readxl::read_xls("data/Brunsk�r/BSabbot2005p.xls")
BSabboa2006 <- readxl::read_xls("data/Brunsk�r/BSabbot2006p.xls")

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

# Need to change "Totall�ngd mm" to numeric for 2006
BSabboa2006$"Totall�ngd mm" <- as.numeric(BSabboa2006$"Totall�ngd mm")

BSTAB_03_06 <- bind_rows(BSabboa2003,
                         BSabboa2004,
                         BSabboa2005,
                         BSabboa2006)

# Go from wide to long data
str(BSTAB_03_06)

# First make sure all lengths at age columns are numeric
BSTAB_03_06 <- BSTAB_03_06 %>% 
  mutate_at(c("�r1","�r2","�r3","�r4","�r5","�r6","�r7","�r8","�r9","�r10","�r11","�r12",
            "�r13","�r14","�r15","�r16","�r17","�r18","�r19","�r20"), as.numeric) 

# Columns 52-71 are gathered
BSTAB_03_06 <- BSTAB_03_06 %>%
  pivot_longer(col = c(52:71), names_to = "age_temp", values_to = "Length") %>% 
  separate("age_temp", c("NA", "Reading_no"), sep = 2) %>% 
  mutate_at("Reading_no", as.numeric) %>% 
  drop_na(Length) # remove NA lengths, cannot not choose that anymore (possible with gather)

# Create ID column
BSTAB_03_06 <- BSTAB_03_06 %>% mutate(ID = paste(F�ngst�r, L�pnummer, Areakod, sep = "."))

  
#**** Bind rows ====================================================================
BSTAB_03_06$source <- "txt"
BSTAB_91_01$source <- "xls"
BSTAB_91_06 <- bind_rows(BSTAB_03_06, BSTAB_91_01)

# Add in column that indicates if the length is from a + reading or not
BSTAB_91_06 <- BSTAB_91_06 %>% 
  group_by(ID) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(Age_ring = ifelse(Reading_no > �lder, "N", "Y"))

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
  facet_wrap(~F�delse�r)

# Plot # of ages per individual per catch year
BSTAB_91_06 %>% 
  filter(Age_ring == "Y") %>% 
  group_by(ID) %>% 
  mutate(n = n()) %>% 
  arrange(desc(n)) %>% 
  ggplot(., aes(n)) + 
  geom_bar() + 
  facet_wrap(~F�ngst�r)

# Plot length-at-age over birth_year, use only back-calculated age
BSTAB_91_06 %>% 
  filter(Age_ring == "Y") %>% 
  ggplot(., aes(F�delse�r, Length, color = source)) + 
  geom_point() + 
  facet_wrap(~factor(Reading_no), scales = "free_y")

# Plot length-at-age over catch year, use only back-calculated age
BSTAB_91_06 %>% 
  filter(Age_ring == "Y") %>% 
  ggplot(., aes(F�ngst�r, Length)) + 
  geom_point() + 
  facet_wrap(~factor(Reading_no), scales = "free_y")

# The issue here is that it's not length in the new data



#** Finbo ==========================================================================


#** Forsmark =======================================================================


#** Holm�n =========================================================================


#** Musk� ==========================================================================


#** Simpevarp ======================================================================


#** Torhamn ========================================================================








