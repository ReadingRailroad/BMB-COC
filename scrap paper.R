silver<-droplevels(subset(df, Lake == "Silver"))
str(silver)


unique(silver$Date[silver$Gear != "COMMERCIAL HARVEST"])
silver.tag<-droplevels(subset(silver, Gear != "COMMERCIAL HARVEST"))

unique(silver$Date[silver$Gear == "COMMERCIAL HARVEST"])
silver.har<-droplevels(subset(silver, Gear == "COMMERCIAL HARVEST"))

silver<-rbind(silver.tag,silver.har)
silver<-silver[order(as.Date(silver$Date, format = "%Y-%m-%d")),]






##
silver.c<-dcast(silver, Date+Gear ~ Species + detect) # need to ID harvest periods, after checking for harvest dates.
##







# harvest date breaks at:
### 

#  M_i is the number of fish marked at the start of the period (all tags - harvest)

M_1<-data.frame(t(colSums(center.c[c(1:31),c(3,4)]))) # marked in period 1 = pre 2019 harvest
M_2<- M_1+
      data.frame(t(colSums(center.c[c(39:55),c(3,4)])))-
      data.frame(t(colSums(center.c[c(32:38),c(3,4)])))# marked in period 2 = 2019 field season
M_3<- M_2+
      data.frame(center.c[57,c(3,4)])-
      data.frame(center.c[56,c(3,4)])# marked in one seine tag event before another late harvest 2019



# F_i., the number of fish marked in period i caught by fishers in all periods


F_1.<-data.frame(t(c(with(merge(subset(center, Date <= "2019-04-03" & 
                                         center$Species == "BIB"),  
                                center.har, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(center, Date <= "2019-04-03" & 
                                         center$Species == "COC"),  
                                center.har, by = "TAG.NUMBER"), length(Gear.y)))))

F_2.<-data.frame(t(c(with(merge(subset(center, Date >= "2019-05-18" &
                                              Date <= "2019-10-04" & 
                                              center$Species == "BIB"),  
                                center.har, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(center, Date >= "2019-05-18" &
                                              Date <= "2019-10-04" & 
                                              center$Species == "COC"),  
                                center.har, by = "TAG.NUMBER"), length(Gear.y)))))

F_3.<-data.frame(t(c(with(merge(subset(center, Date == "2019-11-10" & 
                                              center$Species == "BIB"),  
                                center.har, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(center, Date == "2019-11-10" & 
                                              center$Species == "COC"),  
                                center.har, by = "TAG.NUMBER"), length(Gear.y)))))


# F_.i, the number of fish caught in period i from all marking occasions

F_.1<-data.frame(t(c(with(merge(subset(center, Date >= "2019-04-03" & 
                                              Date <= "2019-05-17" &
                                              center$Species == "BIB"),  
                                center.tag, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(center, Date >= "2019-04-03" & 
                                              Date <= "2019-05-17" &
                                              center$Species == "COC"),  
                                center.tag, by = "TAG.NUMBER"), length(Gear.y)))))


F_.2<-data.frame(t(c(with(merge(subset(center, Date == "2019-11-06" &
                                         center$Species == "BIB"),  
                                center.tag, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(center, Date == "2019-11-06" &
                                         center$Species == "COC"),  
                                center.tag, by = "TAG.NUMBER"), length(Gear.y)))))


F_.3<-data.frame(t(c(with(merge(subset(center, Date >= "2019-11-11" &
                                         center$Species == "BIB"),  
                                center.tag, by = "TAG.NUMBER"), length(Gear.y)),
                     with(merge(subset(center, Date >= "2019-11-11" &
                                         center$Species == "COC"),  
                                center.tag, by = "TAG.NUMBER"), length(Gear.y)))))                    

# n_i, the number of marked fish caught after period i of fish marked before period i

n_1<-data.frame(t(c(with(merge(subset(center, Date <= "2018-11-11" &  
                                        center$Species == "BIB"),  
                               subset(center.har, Date >= "2019-05-20"),
                               by = "TAG.NUMBER"), length(Gear.y)),
                    with(merge(subset(center, Date <= "2018-11-11" & 
                                        center$Species == "COC"),  
                               subset(center.har, Date >= "2019-05-20"),
                               by = "TAG.NUMBER"), length(Gear.y))))) 


n_2<-data.frame(t(c(with(merge(subset(center, Date <= "2019-10-05" &  
                                            center$Species == "BIB"),
                               subset(center.har, Date >= "2019-11-06"),
                               by = "TAG.NUMBER"), length(Gear.y)),
                        with(merge(subset(center, Date <= "2019-10-05" & 
                                            center$Species == "COC"),  
                                   subset(center.har, Date >= "2019-11-06"),
                                   by = "TAG.NUMBER"), length(Gear.y)))))

n_3<-data.frame(t(c(with(merge(subset(center, Date <= "2019-11-08" &  
                                        center$Species == "BIB"),
                               subset(center.har, Date >= "2019-11-21"),
                               by = "TAG.NUMBER"), length(Gear.y)),
                    with(merge(subset(center, Date <= "2019-11-08" & 
                                        center$Species == "COC"),  
                               subset(center.har, Date >= "2019-11-21"),
                               by = "TAG.NUMBER"), length(Gear.y)))))

# now to aggregate this all together.
# data frame that will be: Lake, Species, Period (i), F_i., F_.i, M_i, and n_i. Use those to compute mu_i

Lake<-factor(rep("Center",6))
Species<-factor(rep(c("Bigmouth Buffalo","Common Carp"),3))

center<-data.frame(Lake,Species)
center$Period<-factor(c("1","1","2","2","3","3"))
center$F_i.<-as.numeric(as.character(c(F_1.,F_2.,F_3.)))
center$F_.i<-as.numeric(as.character(c(F_.1,F_.2,F_.3)))
center$M_i<-as.numeric(as.character(c(M_1,M_2,M_3)))
center$n_i<-as.numeric(as.character(c(n_1,n_2,n_3)))

# applying formula for mu_i
center<-data.frame(center)
str(center)
center$mu_i<-(center$F_i. * center$F_.i) / (center$M_i * center$n_i) # provides some biased estimates.
# DOES NOT ACCOUNT FOR TAG LOSS

center$fishing.mortality<-round(center$mu_i * 100, 1)


