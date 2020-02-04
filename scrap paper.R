ntwin<-droplevels(subset(df, Lake == "N Twin"))
str(ntwin)


unique(ntwin$Date[ntwin$Gear != "COMMERCIAL HARVEST"])
ntwin.tag<-droplevels(subset(ntwin, Gear != "COMMERCIAL HARVEST"))
ntwin.tag<-subset(ntwin.tag, Date != "2019-05-06") # fish not tagged May 6 2019
ntwin.tag<-subset(ntwin.tag, Date != "2019-05-17") # fish not tagged May 17 2019

unique(ntwin$Date[ntwin$Gear == "COMMERCIAL HARVEST"])
ntwin.har<-droplevels(subset(ntwin, Gear == "COMMERCIAL HARVEST"))

ntwin<-rbind(ntwin.tag,ntwin.har)
ntwin<-ntwin[order(as.Date(ntwin$Date, format = "%Y-%m-%d")),]


ntwin.c<-dcast(ntwin, Date+Gear ~ Species + detect)


# harvest date breaks at:
### 2019-04-04; 2019-04-05; 2019-04-07; 2019-04-08; 2019-04-09
### 2019-5-16; 2019-5-17


# remember M_i is the number of fish marked at the start of the period (all tags - harvest)

M_1<-data.frame(t(colSums(ntwin.c[c(1:31),c(3,4)]))) # marked in period 1 = pre 2019 harvest
M_2<- M_1+
      data.frame(t(colSums(ntwin.c[c(39:55),c(3,4)])))-
      data.frame(t(colSums(ntwin.c[c(32:38),c(3,4)])))# marked in period 2 = 2019 field season
M_3<- M_2+
      data.frame(ntwin.c[57,c(3,4)])-
      data.frame(ntwin.c[56,c(3,4)])# marked in one seine tag event before another late harvest 2019



# F_i., the number of fish marked in period i caught by fishers in all periods


F_1.<-data.frame(t(c(with(merge(subset(ntwin, Date <= "2019-04-03" & 
                                         ntwin$Species == "BIB"),  
                                ntwin.har, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(ntwin, Date <= "2019-04-03" & 
                                         ntwin$Species == "COC"),  
                                ntwin.har, by = "TAG.NUMBER"), length(Gear.y)))))

F_2.<-data.frame(t(c(with(merge(subset(ntwin, Date >= "2019-05-18" &
                                              Date <= "2019-10-04" & 
                                              ntwin$Species == "BIB"),  
                                ntwin.har, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(ntwin, Date >= "2019-05-18" &
                                              Date <= "2019-10-04" & 
                                              ntwin$Species == "COC"),  
                                ntwin.har, by = "TAG.NUMBER"), length(Gear.y)))))

F_3.<-data.frame(t(c(with(merge(subset(ntwin, Date == "2019-11-10" & 
                                              ntwin$Species == "BIB"),  
                                ntwin.har, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(ntwin, Date == "2019-11-10" & 
                                              ntwin$Species == "COC"),  
                                ntwin.har, by = "TAG.NUMBER"), length(Gear.y)))))


# F_.i, the number of fish caught in period i from all marking occasions

F_.1<-data.frame(t(c(with(merge(subset(ntwin, Date >= "2019-04-03" & 
                                              Date <= "2019-05-17" &
                                              ntwin$Species == "BIB"),  
                                ntwin.tag, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(ntwin, Date >= "2019-04-03" & 
                                              Date <= "2019-05-17" &
                                              ntwin$Species == "COC"),  
                                ntwin.tag, by = "TAG.NUMBER"), length(Gear.y)))))


F_.2<-data.frame(t(c(with(merge(subset(ntwin, Date == "2019-11-06" &
                                         ntwin$Species == "BIB"),  
                                ntwin.tag, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(ntwin, Date == "2019-11-06" &
                                         ntwin$Species == "COC"),  
                                ntwin.tag, by = "TAG.NUMBER"), length(Gear.y)))))


F_.3<-data.frame(t(c(with(merge(subset(ntwin, Date >= "2019-11-11" &
                                         ntwin$Species == "BIB"),  
                                ntwin.tag, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(ntwin, Date >= "2019-11-11" &
                                         ntwin$Species == "COC"),  
                                ntwin.tag, by = "TAG.NUMBER"), length(Gear.y)))))                    

# n_i, the number of marked fish caught after period i of fish marked before period i

n_1<-data.frame(c(0,0))

n_2<-data.frame(t(c(with(merge(subset(ntwin, Date <= "2019-04-03" & 
                                            ntwin$Species == "BIB"),  
                                   ntwin.har, by = "TAG.NUMBER"), length(Gear.y)),
                        with(merge(subset(ntwin, Date <= "2019-04-03" & 
                                            ntwin$Species == "COC"),  
                                   ntwin.har, by = "TAG.NUMBER"), length(Gear.y)))))