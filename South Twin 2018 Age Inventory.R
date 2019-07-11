df<-read.csv("2018 and 2017 Compiled Data.csv")


# trying to figure out what north twin age structures are. There are envelope numbers and tags...

stwin<-droplevels(subset(df, Lake == "S. Twin" &
                  Year == "2018"))

summary(stwin)

levels(stwin$Notes)

stwin.ages<-droplevels(subset(stwin,
                              
                              
                              
                                          
                              Notes == " spine taken env# 13605"                    |
                              Notes == " spine taken env# 5934"                     |
                              Notes == " spine taken env# 6819"|
                              Notes == "euthanized"             |                    
                              Notes == "four digit tag 2018 spine taken env# 1851"  |
                              Notes == "four digit tag 2018 spine taken env# 1852"  |
                              Notes == "four digit tag 2018 spine taken env# 1853"  |
                              Notes == "four digit tag 2018 spine taken env# 1854"  |
                              Notes == "four digit tag 2018 spine taken env# 1855"  |
                              Notes == "four digit tag 2018 spine taken env# 1856"  |
                              Notes == "four digit tag 2018 spine taken env# 1857"  |
                              Notes == "four digit tag 2018 spine taken env# 1858"  |
                              Notes == "four digit tag 2018 spine taken env# 1859"  |
                              Notes == "four digit tag 2018 spine taken env# 1860"  |
                              Notes == "four digit tag 2018 spine taken env# 1861"  |
                              Notes == "four digit tag 2018 spine taken env# 1862"  |
                              Notes == "four digit tag 2018 spine taken env# 1863"  |
                              Notes == "four digit tag 2018 spine taken env# 1864"  |
                              Notes == "four digit tag 2018 spine taken env# 1865"  |
                              Notes == "four digit tag 2018 spine taken env# 1866"  |
                              Notes == "four digit tag 2018 spine taken env# 1867"  |
                              Notes == "four digit tag 2018 spine taken env# 1868"  |
                              Notes == "four digit tag 2018 spine taken env# 1869"  |
                              Notes == "four digit tag 2018 spine taken env# 1870"  |
                              Notes == "four digit tag 2018 spine taken env# 1871"  |
                              Notes == "four digit tag 2018 spine taken env# 1872"  |
                              Notes == "four digit tag 2018 spine taken env# 1899"  |
                              Notes == "four digit tag 2018 spine taken env# 1900"  |
                              Notes == "spine envelope 14375"                       |
                              Notes == "Spine taken  env # 11717"                   |
                              Notes == "Spine taken  env # 11773"                   |
                              Notes == "Spine taken  env # 11774"                   |
                              Notes == "Spine taken  env # 11782"                   |
                              Notes == "Spine taken  env # 11783"                   |
                              Notes == "Spine taken  env # 11784"                   |
                              Notes == "Spine taken  env # 14738"                   |
                              Notes == "Spine taken  env # 14739"                   |
                              Notes == "spine taken env # 13977"                    |
                              Notes == "spine taken env # 14322"                    |
                              Notes == "spine taken env # 301"                      |
                              Notes == "spine taken env # 302"                      |
                              Notes == "spine taken env # 303"                      |
                              Notes == "spine taken env # 304"                      |
                              Notes == "spine taken env # 305"                      |
                              Notes == "spine taken env # 307"                      |
                              Notes == "spine taken env # 308"                      |
                              Notes == "spine taken env # 309"                      |
                              Notes == "spine taken env # 310"                      |
                              Notes == "spine taken env # 311"                      |
                              Notes == "spine taken env # 312"                      |
                              Notes == "spine taken env # 313"                      |
                              Notes == "spine taken env # 314"                      |
                              Notes == "spine taken env # 315"                      |
                              Notes == "spine taken env # 316"                      |
                              Notes == "spine taken env # 317"                      |
                              Notes == "SPINE TAKEN ENV #14320"                     |
                              Notes == "spine taken env 14321"                      |
                              Notes == "spine taken env 5694"                       |
                              Notes == "spine taken env# 16878"                     |
                              Notes == "spine taken env# 300"                       |
                              Notes == "spine taken env# 306"                       |
                              Notes == "spine taken env#1"                          |
                              Notes == "spine taken env#10"                         |
                              Notes == "spine taken env#11"                         |
                              Notes == "spine taken env#12"                         |
                              Notes == "spine taken env#13"                         |
                              Notes == "spine taken env#13973"                      |
                              Notes == "spine taken env#14"                         |
                              Notes == "spine taken env#15"                         |
                              Notes == "spine taken env#16"                         |
                              Notes == "spine taken env#17"                         |
                              Notes == "spine taken env#18"                         |
                              Notes == "spine taken env#19"                         |
                              Notes == "spine taken env#2"                          |
                              Notes == "spine taken env#20"                         |
                              Notes == "spine taken env#21"                         |
                              Notes == "spine taken env#22"                         |
                              Notes == "spine taken env#23"                         |
                              Notes == "spine taken env#24"                         |
                              Notes == "spine taken env#25"                         |
                              Notes == "spine taken env#26"                         |
                              Notes == "spine taken env#27"                         |
                              Notes == "spine taken env#28"                         |
                              Notes == "spine taken env#29"                         |
                              Notes == "spine taken env#3"                          |
                              Notes == "spine taken env#30"                         |
                              Notes == "spine taken env#31"                         |
                              Notes == "spine taken env#32"                         |
                              Notes == "spine taken env#33"                         |
                              Notes == "spine taken env#34"                         |
                              Notes == "spine taken env#35"                         |
                              Notes == "spine taken env#36"                         |
                              Notes == "spine taken env#37"                         |
                              Notes == "spine taken env#38"                         |
                              Notes == "spine taken env#39"                         |
                              Notes == "spine taken env#4"                          |
                              Notes == "spine taken env#40"                         |
                              Notes == "spine taken env#400"                        |
                              Notes == "spine taken env#401"                        |
                              Notes == "spine taken env#402"                        |
                              Notes == "spine taken env#403"                        |
                              Notes == "spine taken env#404"                        |
                              Notes == "spine taken env#405"                        |
                              Notes == "spine taken env#406"                        |
                              Notes == "spine taken env#407"                        |
                              Notes == "spine taken env#408"                        |
                              Notes == "spine taken env#409"                        |
                              Notes == "spine taken env#41"                         |
                              Notes == "spine taken env#410"                        |
                              Notes == "spine taken env#411"                        |
                              Notes == "spine taken env#412"                        |
                              Notes == "spine taken env#413"                        |
                              Notes == "spine taken env#414"                        |
                              Notes == "spine taken env#415"                        |
                              Notes == "spine taken env#416"                        |
                              Notes == "spine taken env#417"                        |
                              Notes == "spine taken env#418"                        |
                              Notes == "spine taken env#419"                        |
                              Notes == "spine taken env#42"                         |
                              Notes == "spine taken env#420"                        |
                              Notes == "spine taken env#421"                        |
                              Notes == "spine taken env#422"                        |
                              Notes == "spine taken env#423"                        |
                              Notes == "spine taken env#424"                        |
                              Notes == "spine taken env#425"                        |
                              Notes == "spine taken env#426"                        |
                              Notes == "spine taken env#427"                        |
                              Notes == "spine taken env#428"                        |
                              Notes == "spine taken env#429"                        |
                              Notes == "spine taken env#43"                         |
                              Notes == "spine taken env#430"                        |
                              Notes == "spine taken env#431"                        |
                              Notes == "spine taken env#432"                        |
                              Notes == "spine taken env#433"                        |
                              Notes == "spine taken env#44"                         |
                              Notes == "spine taken env#45"                         |
                              Notes == "spine taken env#46"                         |
                              Notes == "spine taken env#47"                         |
                              Notes == "spine taken env#48"                         |
                              Notes == "spine taken env#49"                         |
                              Notes == "spine taken env#5"                          |
                              Notes == "spine taken env#50"                         |
                              Notes == "spine taken env#51"                         |
                              Notes == "spine taken env#52"                         |
                              Notes == "spine taken env#53"                         |
                              Notes == "spine taken env#54"                         |
                              Notes == "spine taken env#55"                         |
                              Notes == "spine taken env#56"                         |
                              Notes == "spine taken env#57"                         |
                              Notes == "spine taken env#58"                         |
                              Notes == "spine taken env#59"                         |
                              Notes == "spine taken env#6"                          |
                              Notes == "spine taken env#60"                         |
                              Notes == "spine taken env#61"                         |
                              Notes == "spine taken env#62"                         |
                              Notes == "spine taken env#63"                         |
                              Notes == "spine taken env#64"                         |
                              Notes == "spine taken env#65"                         |
                              Notes == "spine taken env#66"                         |
                              Notes == "spine taken env#67"                         |
                              Notes == "spine taken env#68"                         |
                              Notes == "spine taken env#69"                         |
                              Notes == "spine taken env#7"                          |
                              Notes == "spine taken env#70"                         |
                              Notes == "spine taken env#71"                         |
                              Notes == "spine taken env#72"                         |
                              Notes == "spine taken env#73"                         |
                              Notes == "spine taken env#74"                         |
                              Notes == "spine taken env#75"                         |
                              Notes == "spine taken env#76"                         |
                              Notes == "spine taken env#77"                         |
                              Notes == "spine taken env#78"                         |
                              Notes == "spine taken env#79"                         |
                              Notes == "spine taken env#8"                          |
                              Notes == "spine taken env#80"                         |
                              Notes == "spine taken env#81"                         |
                              Notes == "spine taken env#82"                         |
                              Notes == "spine taken env#83"                         |
                              Notes == "spine taken env#84"                         |
                              Notes == "spine taken env#85"                         |
                              Notes == "spine taken env#86"                         |
                              Notes == "spine taken env#87"                         |
                              Notes == "spine taken env#88"                         |
                              Notes == "spine taken env#89"                         |
                              Notes == "spine taken env#9"                          |
                              Notes == "spine taken env#90"                         |
                              Notes == "spine taken env#91"                         |
                              Notes == "spine taken env#92"                         |
                              Notes == "spine taken env#93"                         |
                              Notes == "spine taken env#94"                         |
                              Notes == "spine taken env#95"                         |
                              Notes == "spine taken env#96"                         |
                              Notes == "spine taken env#97"  ))

# trim columns not relevant to age structure stuff:

head(stwin.ages)

stwin.ages<-stwin.ages[,-c(7,8,11:20)]

summary(stwin.ages)

write.csv(stwin.ages, "South Twin 2018 Age Inventory.csv")
