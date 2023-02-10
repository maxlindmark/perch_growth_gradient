
##-------------------------------------------------------------------## packages 
pkgs<-c("here","tidyverse","readxl","stringr","MARSS","ggplot2","RColorBrewer", "ggsidekick", "lubridate", "ISOweek", "viridis") 
if(length(setdiff(pkgs,rownames(installed.packages())))>0){ install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T)}
invisible(lapply(pkgs,library,character.only=T))

theme_set(theme_sleek())

##-------------------------------------------------------------------## get data
home <- here::here()
filenames <- list.files(paste0(home,"/data/temp_data"))
raw_data<-NULL
for(i in 1:length(filenames)){
  dat <- read_excel(paste0(home,"/data/temp_data/",filenames[i]))
  if(i==1) names <- colnames(dat)
  if(i!=1) names(dat) <- names
  area <- substr(filenames[i],1,2)
  if(area=="SI") area <- substr(filenames[i],1,5)
  if(area=="JM") area <- paste0(substr(filenames[i],1,2),substr(filenames[i],9,10))
  if(area=="JMT0") area <- substr(filenames[i],1,2)
  dat$area <- area
  raw_data <- data.frame(rbind(raw_data,dat))
} ## JMT0 is really JM_T10 and taken as the main temperature time series for JM

##------------------------------------------------------------## manipulate data
## depth measurements
data <- raw_data %>%
  mutate(Depth=gsub(",",".",gsub('m','',Depth))) %>%
  filter(Depth!="Yta") %>%
  filter(Depth!="Siktdjup") %>%
  mutate_at('Depth',~str_replace(.,"0.3-0.5","0.5")) %>%
  mutate_at('Depth',~str_replace(.,"ca 1.5","1.5")) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  drop_na(Depth)

## filter other columns
data <- data %>%
  select(area,Station_Code,Year,Month,Day,Depth,Mean) %>%
  filter(Depth>=0.5 & Depth<=1.5) %>% ## restrict depth range
  filter(Year %in% seq(1963,2019,1)) %>% ## complete years only
  filter(!is.na(Station_Code)) %>%
  filter(Month %in% seq(12)) %>%
  filter(Day %in% seq(31)) %>%
  mutate(Mean=as.numeric(Mean)) %>%
  select(-Station_Code) %>%
  mutate(date=as.Date(paste(Year,Month,Day,sep="-")))

##-----------------------------------------------------------------## plot data
data %>%
  ggplot(.,aes(x=date,y=Mean,color=factor(area))) +
  geom_line(size=0.25) +
  #scale_color_manual(values=colors,name="Area") +
  theme_sleek() +
  theme(plot.title=element_text(size=15,face="bold")) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=15)) +
  scale_x_date(breaks="10 years",date_labels="%Y") +
  theme(axis.text.x=element_text(angle=90)) +
  guides(color=guide_legend(override.aes=list(size=1))) +
  labs(x="Year",y="Temperature") +
  # facet_grid(cols=vars(area)) +
  facet_wrap(~area) +
  NULL

##-----------------------------------------------------------------## plot summarized data and compare to fishing

kul_dat <- read_excel("data/temp_data_fishing/temp_at_fishing.xlsx", skip = 4) %>% 
  mutate(Lokal = ifelse(Lokal == "Simpevarp" & Fångstområde == "Ekö", "SI_EK", Lokal),
         Lokal = ifelse(Lokal == "Simpevarp" & Fångstområde == "Hamnefjärden", "SI_HA", Lokal)) %>% 
  mutate(area = recode_factor(Lokal,
                              "Kvädöfjärden" = "JM", 
                              "Muskö" = "MU",
                              "Holmön" = "HO",
                              "Forsmark" = "FM",
                              "Finbo, Åland" = "FB",
                              "Råneå" = "RA",
                              "Torhamn, Karlskrona Ö skärgård" = "TH",
                              "Biotestsjön, Forsmark" = "BT",
                              "Vinö" = "VN")) %>% 
  rename(temp = Värde,
         year = År, 
         date = `Vittjnings\ndatum`) %>% 
  mutate(db = "KUL",
         yday = yday(date),
         month = month(date),
         id = paste(year, area, sep = "_")) %>% 
  mutate(temp = ifelse(area == "SI_HA" & temp == 267, 26.7, temp)) %>% 
  filter(!area == "Simpevarp") %>% 
  #filter(month >= 5 & month <= 9) %>% 
  filter(temp < 100) %>% 
  filter(Vertikalstyp == "Yta") %>% 
  dplyr::select(year, yday, date, temp, area, id, db)


# Plot
ggplot(kul_dat,aes(x=year,y=temp,color=area)) +
  geom_point(size=1) +
  guides(color = "none") +
  theme_sleek() +
  labs(x="Year",y="Temperature") +
  facet_wrap(~area) +
  NULL


## Now we need to add in the rest of the data from Firre database
home <- here::here()
filenames <- list.files(paste0(home,"/data/temp_data_fishing_firre"))
raw_data<-NULL
for(i in 1:length(filenames)){
  dat <- read_excel(paste0(home,"/data/temp_data_fishing_firre/",filenames[i]))
  if(i==1) names <- colnames(dat)
  if(i!=1) names(dat) <- names
  area <- substr(filenames[i],1,2)
  if(area=="SI") area <- substr(filenames[i],1,5)
  #if(area=="JM") area <- paste0(substr(filenames[i],1,2),substr(filenames[i],9,10))
  if(area=="JMT0") area <- substr(filenames[i],1,2)
  dat$area <- area
  raw_data <- data.frame(rbind(raw_data,dat))
} ## JMT0 is really JM_T10 and taken as the main temperature time series for JM

# https://stackoverflow.com/questions/45549449/transform-year-week-to-date-object
# https://www.r-bloggers.com/2013/08/date-formats-in-r/
firre_dat <- raw_data %>% 
  mutate(group = ifelse(VkDag > 100, "2", "1")) %>% 
  mutate(VkDag2 = ifelse(group == "1", paste0(0, VkDag), VkDag)) %>%  # add zero before strings with length 2...
  separate(VkDag2, sep = 2, into = c("week", 'day'), extra = 'drop', remove = FALSE) %>% 
  mutate(week = as.numeric(week), # to get rid of the 0
         day = as.numeric(day)) %>% 
  mutate(date = as.Date(paste(Årtal, week, day, sep = "-"), "%Y-%U-%u")) %>% 
  # mutate(week2 = paste0("W", week),
  #        weekdate = paste(Årtal, week2, day, sep = "-")) #%>% 
  mutate(#date = ISOweek2date(weekdate),
         yday = yday(date),
         month = month(date)) %>% 
  rename(year = Årtal,
         stn_nr = Station,
         section_nr = Sektion) %>% 
  filter(!if_all(c(MedelFörTemperatur_i, MedelFörTemperatur_u), is.na)) %>% 
  mutate(MedelFörTemperatur_i = ifelse(is.na(MedelFörTemperatur_i), MedelFörTemperatur_u, MedelFörTemperatur_i),
         MedelFörTemperatur_u = ifelse(is.na(MedelFörTemperatur_u), MedelFörTemperatur_i, MedelFörTemperatur_u)) %>% 
  mutate(temp = (MedelFörTemperatur_i + MedelFörTemperatur_u) / 2,
         db = "FIRRE",
         id = paste(year, area, sep = "_")) %>% 
  dplyr::select(year, yday, month, day, date, VkDag, temp, area, stn_nr, section_nr, db, id) %>% 
  drop_na(date) %>% 
  mutate(date = as.character(date))

# Now we need to merge it with fish_dat

fish_dat <- bind_rows(firre_dat %>% filter(!id %in%kul_dat$id),
                      kul_dat)

# Plot!
fish_dat %>% 
  ggplot(aes(yday, temp, color = factor(year))) + 
  scale_color_viridis(discrete = TRUE) +
  geom_point(size = 0.1, alpha = 0.5) + 
  geom_line(size = 0.1, alpha = 0.5) +
  facet_wrap(~area)

fish_dat %>% 
  ggplot(aes(yday, temp, color = factor(db))) + 
  scale_color_viridis(discrete = TRUE) +
  geom_point(size = 0.1, alpha = 0.5) + 
  geom_line(size = 0.1, alpha = 0.5) +
  facet_wrap(~area)

# Inspect FM a bit more closely...
# fish_dat %>% 
#   filter(area == "FM") %>% 
#   ggplot(aes(yday, temp, color = factor(year))) + 
#   scale_color_viridis(discrete = TRUE) +
#   geom_point(size = 0.5, alpha = 0.5) + 
#   geom_line(size = 0.5, alpha = 0.5) +
#   facet_wrap(section_nr~area)
# 
# # Looks like we should only keep section 4 in FM
# fish_dat <- fish_dat %>% 
#   mutate(keep = ifelse(area == "FM" & !section_nr == 4, "N", "Y")) %>% 
#   filter(keep == "Y") %>% dplyr::select(-keep)

fish_dat %>% 
  ggplot(aes(yday, temp, color = factor(year))) + 
  scale_color_viridis(discrete = TRUE) +
  geom_point(size = 0.1, alpha = 0.5) + 
  geom_line(size = 0.1, alpha = 0.5) +
  facet_wrap(~area)

# Plot over time
fish_dat %>% 
  ggplot(aes(year, temp)) + 
  scale_color_viridis(discrete = TRUE) +
  geom_point(size = 0.1, alpha = 0.5) + 
  geom_line(size = 0.1, alpha = 0.5) +
  stat_smooth(se = FALSE) +
  facet_wrap(~area)

# We need to account for day of the year somehow, because we loose data if we trim certain months
library(mgcv)

fish_dat2 <- fish_dat %>% 
  mutate(area = as.factor(area),
         yday_ct = yday - mean(yday)) 

m1 <- gam(temp ~ area + s(year, by = area) + s(yday), data = fish_dat2)
 
qq.gam(m1)

fish_dat2$id <- paste(fish_dat2$year, fish_dat2$area, sep = "_")

nd <- data.frame(expand.grid(yday = seq(min(fish_dat2$yday), max(fish_dat2$yday), by = 1),
                             area = unique(fish_dat2$area),
                             year = unique(fish_dat2$year))) %>%
  mutate(id = paste(year, area, sep = "_"),
         yday_ct = yday - mean(fish_dat2$yday)) %>%
  filter(id %in% unique(fish_dat2$id))

nd$pred <- predict(m1, newdata = nd)

# plot over yday
fish_dat2 %>% 
  ggplot(aes(yday, temp, color = factor(year))) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  facet_wrap(~area) + 
  geom_line(data = nd, aes(yday, pred), size = 1) +
  NULL

# plot over year
fish_dat2 %>% 
  ggplot(aes(year, temp, color = yday)) + 
  scale_color_gradient2(midpoint = mean(firre_dat$yday)) +
  geom_point(size = 0.5, alpha = 0.5) + 
  facet_wrap(~area) + 
  geom_line(data = nd %>% filter(yday == round(mean(firre_dat$yday))), aes(year, pred), color = "tomato", size = 1, inherit.aes = FALSE) +
  NULL

# Only predictions in mean yearday
nd %>% 
  filter(yday == round(mean(fish_dat$yday))) %>% 
  ggplot(aes(year, pred, color = area)) + 
  geom_line() +
  NULL

# Compare specific time window with logger data
ggplot(fish_dat2, aes(yday)) + 
  geom_histogram() + 
  facet_wrap(~area, scales = "free_y") + 
  geom_vline(xintercept = c(210, 230), color = "red")

nd2 <- nd %>%
  filter(yday >= 210 & yday <= 230) %>% 
  group_by(year, area) %>% 
  summarise(mean_temp = mean(pred)) %>% 
  mutate(source = "fishing")

nd2 %>% 
  ggplot(aes(year, mean_temp, color = area)) + 
  geom_line() +
  NULL

head(nd2)
head(data)

data2 <- data %>% 
  mutate(yday = yday(date)) %>% 
  filter(yday >= 210 & yday <= 230) %>% 
  rename(year = Year) %>% 
  group_by(year, area) %>% 
  summarise(mean_temp = mean(Mean)) %>% 
  dplyr::select(year, area, mean_temp) %>% 
  mutate(source = "logger")

dd <- bind_rows(nd2, data2)

dd %>% 
  ggplot(aes(year, mean_temp, color = source)) + 
  geom_line() +
  facet_wrap(~ area) +
  NULL

filter(nd2, area == "FM")


# Why don't we use logger data also and use source that a fixed effect?
data_logger <- data %>% 
  mutate(yday = yday(date)) %>% 
  rename(year = Year,
         temp = Mean) %>% 
  group_by(year, area) %>% 
  dplyr::select(year, area, temp, yday) %>% 
  mutate(source = "logger")

# Merge with fish_dat2

all_temp <- fish_dat2 %>% 
  mutate(source = "fishing") %>% 
  bind_rows(data_logger)

all_temp %>% 
  ggplot(aes(yday, temp, color = factor(source))) + 
  scale_color_viridis(discrete = TRUE) +
  geom_point(size = 0.1, alpha = 0.5) + 
  geom_line(size = 0.1, alpha = 0.5) +
  facet_wrap(~area)

all_temp <- all_temp %>% 
  filter(area %in% unique(fish_dat2$area)) %>% 
  mutate(area = as.factor(area),
         source = as.factor(source)) %>% 
  filter(temp > 9) %>% 
  filter(yday > yday("2020-06-01") & yday < yday("2020-09-01")) %>% # filter also some yeardays to make it easier to fit (avoid cyclic smooth)
  mutate(id = paste(year, area, sep = "_"))

ggplot(all_temp, aes(yday, temp, color = year)) + 
  geom_point(size = 0.2) + 
  facet_wrap(~area)

# Refit model
#m2 <- gam(temp ~ area + source + s(year, by = area) + s(yday), data = all_temp)
m2 <- gam(temp ~ area + source + s(year, by = area) + s(yday, by = area), 
          data = all_temp)

qq.gam(m2)

fish_dat2$id <- paste(fish_dat2$year, fish_dat2$area, sep = "_")

nd2 <- data.frame(expand.grid(yday = seq(min(all_temp$yday), max(all_temp$yday), by = 1),
                              area = unique(all_temp$area),
                              year = unique(all_temp$year))) %>%
  mutate(id = paste(year, area, sep = "_"),
         source = "logger") %>% 
  filter(id %in% unique(all_temp$id))

nd2$pred <- predict(m2, newdata = nd2)

# plot over yday
all_temp %>% 
  ggplot(aes(yday, temp, color = factor(year))) + 
  geom_point(size = 0.5, alpha = 0.5) + 
  facet_wrap(~area) + 
  geom_line(data = nd2, aes(yday, pred), size = 1) +
  NULL

# plot over year
all_temp %>% 
  ggplot(aes(year, temp, color = yday)) + 
  scale_color_gradient2(midpoint = mean(all_temp$yday)) +
  geom_point(size = 0.5, alpha = 0.5) + 
  facet_wrap(~area) + 
  geom_line(data = nd2 %>% filter(yday == round(mean(fish_dat2$yday))), aes(year, pred), color = "tomato", size = 1, inherit.aes = FALSE) +
  NULL

# Only predictions in mean yearday
nd2 %>% 
  filter(yday == round(mean(all_temp$yday))) %>% 
  ggplot(aes(year, pred, color = area)) + 
  geom_line() +
  NULL

nd3 <- nd2 %>%
  filter(yday > yday("2020-06-01") & yday < yday("2020-09-01")) %>% 
  group_by(year, area) %>% 
  summarise(mean_temp = mean(pred))

# Save as csv... 
write.csv(nd3, "data/predicted_temp/predicted_temp.csv")
