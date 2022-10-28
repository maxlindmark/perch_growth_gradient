## function to download ERSST data 
get_ersst_data<-function(years,data.dir,latrange,lonrange){
  df<-data.frame(matrix(NA,ncol=5))
  colnames(df)<-c("lat","lon","sst","year","month")
  for(i in 1:length(years)){
    for(j in 1:12){
      tdat<-NULL
      try(tdat<-ersst(year=years[i],month=j,version="v5"),silent=T)
      if(!is.null(tdat)){
        df<-df%>%
          bind_rows(data.frame(
            ncdf4::ncvar_get(tdat,"sst")) %>%
              rename_with(~ncdf4::ncvar_get(tdat,"lat")) %>%
              mutate(lon=ncdf4::ncvar_get(tdat,"lon")) %>%
              pivot_longer(cols=-c("lon"),names_to="lat",values_to="sst") %>%
              mutate(lon=ifelse(lon>180,-360+lon,lon),lat=as.numeric(lat)) %>%
              filter(lat>min(latrange)&lat<max(latrange)) %>%
              filter(lon>min(lonrange)&lon<max(lonrange)) %>%
              mutate(year=years[i],month=j)
          )
      }
    }
  }
  df<-df %>% 
    filter(!is.na(year))
  write.csv(df,paste(data.dir,"/ersst.csv",sep=""),row.names=F)
  dat<-read_csv(paste(data.dir,"/ersst.csv",sep="")) %>%
    group_by(year,month) %>%
    summarize(meanSST=mean(sst,na.rm=T))
  return(dat)
}
