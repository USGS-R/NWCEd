library(ggplot2)
library(NWCEd)
# Super basic plot of precip and et.
NWCdata<-getNWCData(huc="031601030306")
NWCdata$et$group<-'et'
NWCdata$prcp$group<-'prcp'
plotData<-rbind(NWCdata$et, NWCdata$prcp)
ggplot(plotData, aes(x=date, y=data, group=group,colour = group)) +  geom_line() +
  scale_x_date(date_labels = "%Y",date_breaks = "5 years",name = "") + ylab("mm") +
  labs(title = "Plot of All Data", colour = "Explanation") +
  theme(legend.position = c(0.1, 0.87)) + scale_color_manual(values=c("#990066", "#339966"))

# annualize and take a difference.
annual_dataet<-annualize(NWCdata$et, method=sum)
annual_dataet$group<-'et'
annual_dataprcp<-annualize(NWCdata$prcp, method=sum)
annual_dataprcp$group<-'prcp'
annual_union<-merge(annual_dataet,annual_dataprcp,by.x="year",by.y="year")
annual_diff<-as.data.frame(annual_union$data.y - annual_union$data.x)
names(annual_diff) = c('data')
annual_diff$group<-'diff'
annual_diff$year<-annual_union$year
plotData<-rbind(annual_dataet,annual_dataprcp,annual_diff)

ggplot(plotData, aes(x=year, y=data, group=group,colour = group)) +  geom_line() +
  scale_x_discrete(name = "year") + ylab("mm") +
  labs(title = "Basic Diff Plot", colour = "Explanation") +
  theme(legend.position = c(0.1, 0.87)) + scale_color_manual(values=c("#990066", "#339966", "#FF0000"))

# Lets look at some modeled streamflow data.
# From: http://cida.usgs.gov/nwc/#!streamflow-stats/huc/031601090601
NWCdata<-getNWCData(huc="031601030306", local = FALSE)
# Note that we need to normalize the streamflow to watershed area!
NWCwatershed<-getNWCWatershed(huc="160202030505",local=FALSE)
areasqft<-NWCwatershed$features[[1]]$properties$areaacres*43560
annual_dataet<-annualize(NWCdata$et, method=sum)
annual_dataet$group<-'ET'
annual_dataprcp<-annualize(NWCdata$prcp, method=sum)
annual_dataprcp$group<-'P'
NWCdata$streamflow$data<-NWCdata$streamflow$data_00060_00003*60*60*24*304.8/areasqft # convert from cfs to mm
annual_datastrf<-annualize(NWCdata$streamflow$data,method=sum)
annual_datastrf$group<-'Q'
annual_union<-merge(annual_dataet,annual_dataprcp,by.x="year",by.y="year")
annual_union<-merge(annual_union,annual_datastrf,by.x="year",by.y="year")
names(annual_union)<-c("year","ET","groupET","P","groupP","Q","groupQ")
annual_diff<-as.data.frame(annual_union$P - annual_union$ET - annual_union$Q)
names(annual_diff) = c('data')
annual_diff$group<-'diff'
annual_diff$year<-annual_union$year
plotData<-rbind(annual_dataet,annual_dataprcp,annual_datastrf,annual_diff)

ggplot(plotData, aes(x=year, y=data, group=group,colour = group)) +  geom_line() +
  scale_x_discrete(name = "year") + ylab("mm") +
  labs(title = "Basic Diff Plot", colour = "Explanation") +
  scale_color_manual(values=c("#990066", "#339966", "#FF0000", "#3300FF"))

# Get watershed geometry and put it on a map.
NWCwatershed<-getNWCWatershed(huc="031601030306",local=FALSE)
coords<-NWCwatershed$features[[1]]$geometry$coordinates[[1]][[1]]
coords<-as.data.frame(matrix(unlist(coords),nrow=length(coords),byrow = T))
names(coords)<-c('lon','lat')

# We can use leaflet, probably a way to use ggmap with the coords as well.
leaflet() %>%
  setView(lng = mean(coords$lon), lat = mean(coords$lat), zoom = 9) %>%
  addTiles() %>%
  addGeoJSON(NWCwatershed, weight = 2, color = "#444444", fill = FALSE)

