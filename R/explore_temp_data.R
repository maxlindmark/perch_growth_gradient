
##-------------------------------------------------------------------## packages 
pkgs<-c("here","tidyverse","readxl","stringr","MARSS","ggplot2","RColorBrewer", "ggsidekick") 
if(length(setdiff(pkgs,rownames(installed.packages())))>0){ install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T)}
invisible(lapply(pkgs,library,character.only=T))

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
  scale_color_manual(values=colors,name="Area") +
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
