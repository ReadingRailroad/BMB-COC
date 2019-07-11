df<-read.csv("2018 and 2017 Compiled Data.csv")


# trying to figure out what north twin age structures are. There are envelope numbers and tags...

ntwin<-droplevels(subset(df, Lake == "N. Twin" &
                  Year == "2018"))

summary(ntwin)

levels(ntwin$Notes)

ntwin.ages<-droplevels(subset(ntwin, 
                              Notes == "euthanized" |
                              Notes == "Kept for dissection" |
                              Notes == "Spine Taken 1" |
                              Notes == "Spine Taken 1" |                                                                                                     
                              Notes == "Spine Taken 10" |                                                                                                   
                              Notes == "Spine Taken 100" |                                                                                                  
                              Notes == "Spine Taken 101"  |                                                                                                 
                              Notes == "Spine Taken 102"   |                                                                                                
                              Notes == "Spine Taken 103"    |                                                                                               
                              Notes == "Spine Taken 104"    |                                                                                               
                              Notes == "Spine Taken 105"     |                                                                                              
                              Notes == "Spine Taken 106"|                                                                                                   
                              Notes == "Spine Taken 107" |                                                                                                  
                              Notes == "Spine Taken 108"  |                                                                                                 
                              Notes == "Spine Taken 109"   |                                                                                                
                              Notes == "Spine Taken 11"     |                                                                                               
                              Notes == "Spine Taken 110"     |                                                                                              
                              Notes == "Spine Taken 111"      |                                                                                             
                              Notes == "Spine Taken 112"|                                                                                                   
                              Notes == "Spine Taken 113"|                                                                                                   
                              Notes == "Spine Taken 114"|                                                                                                   
                              Notes == "Spine Taken 115"|                                                                                                   
                              Notes == "Spine Taken 116"|                                                                                                   
                              Notes == "Spine Taken 117"|                                                                                                   
                              Notes == "Spine Taken 118"|                                                                                                   
                              Notes == "Spine Taken 119"|                                                                                                   
                              Notes == "Spine Taken 12" |                                                                                                   
                              Notes == "Spine Taken 120"|                                                                                                   
                              Notes == "Spine Taken 121"|                                                                                                   
                              Notes == "Spine Taken 122"|                                                                                                   
                              Notes == "Spine Taken 13"  |                                                                                                  
                              Notes == "Spine Taken 14"   |                                                                                                 
                              Notes == "Spine Taken 15"    |                                                                                                
                              Notes == "Spine Taken 16"     |                                                                                               
                              Notes == "Spine Taken 17"      |                                                                                              
                              Notes == "Spine Taken 18"       |                                                                                             
                              Notes == "Spine Taken 19"        |                                                                                            
                              Notes == "Spine Taken 2"          |                                                                                           
                              Notes == "Spine Taken 20"          |                                                                                          
                              Notes ==  "Spine Taken 21"          |                                                                                          
                              Notes ==  "Spine Taken 22"           |                                                                                         
                              Notes ==  "Spine Taken 23"            |                                                                                        
                              Notes ==  "Spine Taken 24"             |                                                                                       
                              Notes ==  "Spine Taken 25"              |                                                                                      
                              Notes ==  "Spine Taken 26"               |                                                                                     
                              Notes ==  "Spine Taken 27"                |                                                                                    
                              Notes ==  "Spine Taken 28"                 |                                                                                   
                              Notes ==  "Spine Taken 29"                |                                                                                    
                              Notes ==  "Spine Taken 3"                  |                                                                                   
                              Notes ==  "Spine Taken 30"                  |                                                                                  
                              Notes ==  "Spine Taken 31"                   |                                                                                 
                              Notes ==  "Spine Taken 32"                    |                                                                                
                              Notes ==  "Spine Taken 33"  |                                                                                                  
                              Notes ==  "Spine Taken 34"   |                                                                                                 
                              Notes ==  "Spine Taken 35"    |                                                                                                
                              Notes ==  "Spine Taken 36"     |                                                                                               
                              Notes ==  "Spine Taken 37"      |                                                                                              
                              Notes ==  "Spine Taken 38"       |                                                                                             
                              Notes ==  "Spine Taken 39"        |                                                                                            
                              Notes ==  "Spine Taken 4"          |                                                                                           
                              Notes ==  "Spine Taken 40"          |                                                                                          
                              Notes ==  "Spine Taken 41"           |                                                                                         
                              Notes ==  "Spine Taken 42"            |                                                                                        
                              Notes ==  "Spine Taken 43"             |                                                                                       
                              Notes ==  "Spine Taken 44"              |                                                                                      
                              Notes ==  "Spine Taken 45"  |                                                                                                  
                              Notes ==  "Spine Taken 46"   |                                                                                                 
                              Notes ==  "Spine Taken 47"    |                                                                                                
                              Notes ==  "Spine Taken 48"     |                                                                                               
                              Notes ==  "Spine Taken 49"      |                                                                                              
                              Notes ==  "Spine Taken 5"        |                                                                                             
                              Notes ==  "Spine Taken 50"        |                                                                                            
                              Notes ==  "Spine Taken 51"         |                                                                                           
                              Notes ==  "Spine Taken 52"          |                                                                                          
                              Notes ==  "Spine Taken 53"           |                                                                                         
                              Notes ==  "Spine Taken 54"            |                                                                                        
                              Notes ==  "Spine Taken 55"             |                                                                                       
                              Notes ==  "Spine Taken 56"              |                                                                                      
                              Notes ==  "Spine Taken 57"               |                                                                                     
                              Notes ==  "Spine Taken 58"|                                                                                                    
                              Notes ==  "Spine Taken 59" |                                                                                                   
                              Notes ==  "Spine Taken 6"   |                                                                                                  
                              Notes ==  "Spine Taken 60"   |                                                                                                 
                              Notes ==  "Spine Taken 61"    |                                                                                                
                              Notes ==  "Spine Taken 62"     |                                                                                               
                              Notes ==  "Spine Taken 63"    |                                                                                                
                              Notes ==  "Spine Taken 64"    |                                                                                                
                              Notes ==  "Spine Taken 65"    |                                                                                                
                              Notes ==  "Spine Taken 66"    |                                                                                                
                              Notes ==  "Spine Taken 67"    |                                                                                                
                              Notes ==  "Spine Taken 68"    |                                                                                                
                              Notes ==  "Spine Taken 69"    |                                                                                                
                              Notes ==  "Spine Taken 7"     |                                                                                                
                              Notes ==  "Spine Taken 70"    |                                                                                                
                              Notes ==  "Spine Taken 71"    |                                                                                                
                              Notes ==  "Spine Taken 72"    |                                                                                                
                              Notes ==  "Spine Taken 73"    |                                                                                                
                              Notes ==  "Spine Taken 74"    |                                                                                                
                              Notes ==  "Spine Taken 75"    |                                                                                                
                              Notes ==  "Spine Taken 76"    |                                                                                                
                              Notes ==  "Spine Taken 77"    |                                                                                                
                              Notes ==  "Spine Taken 78"    |                                                                                                
                              Notes ==  "Spine Taken 79"    |                                                                                                
                              Notes ==  "Spine Taken 8"     |                                                                                                
                              Notes ==  "Spine Taken 80"    |                                                                                                
                              Notes ==  "Spine Taken 81"    |                                                                                                
                              Notes ==  "Spine Taken 82"    |                                                                                                
                              Notes ==  "Spine Taken 83"    |                                                                                                
                              Notes ==  "Spine Taken 84"    |                                                                                                
                              Notes ==  "Spine Taken 85"    |                                                                                                
                              Notes ==  "Spine Taken 86"    |                                                                                                
                              Notes ==  "Spine Taken 87"    |                                                                                                
                              Notes ==  "Spine Taken 88"    |                                                                                                
                              Notes ==  "Spine Taken 89"    |                                                                                                
                              Notes ==  "Spine Taken 9"     |                                                                                                
                              Notes ==  "Spine Taken 90"    |                                                                                                
                              Notes ==  "Spine Taken 91"    |                                                                                                
                              Notes ==  "Spine Taken 92"    |                                                                                                
                              Notes ==  "Spine Taken 93"    |                                                                                                
                              Notes ==  "Spine Taken 94"    |                                                                                                
                              Notes ==  "Spine Taken 95"    |                                                                                                
                              Notes ==  "Spine Taken 96"    |                                                                                                
                              Notes ==  "Spine Taken 97"    |                                                                                                
                              Notes ==  "Spine Taken 98"    |                                                                                                
                              Notes ==  "Spine Taken 99"    |                                                                                                
                              Notes ==  "spine taken env # 150"   |                                                                                          
                              Notes ==  "spine taken env # 151"   |                                                                                          
                              Notes ==  "spine taken env # 152"   |                                                                                          
                              Notes ==  "spine taken env # 156"   |                                                                                          
                              Notes ==  "spine taken env # 157"   |                                                                                          
                              Notes ==  "spine taken env # 158"   |                                                                                          
                              Notes ==  "spine taken env # 159"   |                                                                                          
                              Notes ==  "spine taken env # 160"   |                                                                                          
                              Notes ==  "spine taken env # 161"   |                                                                                          
                              Notes ==  "spine taken env # 163"   |                                                                                          
                              Notes ==  "spine taken env #155"    |                                                                                          
                              Notes ==  "spine taken env# 14352"  |                                                                                          
                              Notes ==  "spine taken env# 14353"  |                                                                                          
                              Notes ==  "spine taken env# 14354"  |                                                                                          
                              Notes ==  "spine taken env# 14368"  |                                                                                          
                              Notes ==  "spine taken env# 14369"  |                                                                                          
                              Notes ==  "spine taken env# 14370"  |                                                                                          
                              Notes ==  "spine taken env# 14371"  |                                                                                          
                              Notes ==  "spine taken env# 14372"  |                                                                                          
                              Notes ==  "spine taken env# 14374"  |                                                                                          
                              Notes ==  "spine taken env# 14485"  |                                                                                          
                              Notes ==  "Spine taken env# 153"    |                                                                                          
                              Notes ==  "Spine taken env# 154"    |                                                                                          
                              Notes ==  "spine taken env# 162"    |                                                                                          
                              Notes ==  "spine taken env# 1645"   |                                                                                          
                              Notes ==  "spine taken env# 1679"   |                                                                                          
                              Notes ==  "spine taken env# 16865"  |                                                                                          
                              Notes ==  "spine taken env# 16866"  |                                                                                          
                              Notes ==  "spine taken env# 16867"  |                                                                                          
                              Notes ==  "spine taken env# 16868"  |                                                                                          
                              Notes ==  "spine taken env# 16869"  |                                                                                          
                              Notes ==  "spine taken env# 1687"   |                                                                                          
                              Notes ==  "spine taken env# 16870"  |                                                                                          
                              Notes ==  "spine taken env# 16871"  |                                                                                          
                              Notes ==  "spine taken env# 16872"  |                                                                                          
                              Notes ==  "spine taken env# 16873"  |                                                                                          
                              Notes ==  "spine taken env# 16874"  |                                                                                          
                              Notes ==  "spine taken env# 16875"  |                                                                                          
                              Notes ==  "spine taken env#175"     |                                                                                          
                              Notes ==  "spine taken env#176"     |                                                                                          
                              Notes ==  "spine taken env#177"     |                                                                                          
                              Notes ==  "spine taken env#178"     |                                                                                          
                              Notes ==  "spine taken env#179"     |                                                                                          
                              Notes ==  "spine taken env#180"     |                                                                                          
                              Notes ==  "spine taken env#181"     |                                                                                          
                              Notes ==  "spine taken env#182"     |                                                                                          
                              Notes ==  "spine taken env#183"     |                                                                                          
                              Notes ==  "spine taken env#184"     |                                                                                          
                              Notes ==  "spine taken env#185"     |                                                                                          
                              Notes ==  "spine taken env#186"     |                                                                                          
                              Notes ==  "spine taken env#187"     |                                                                                          
                              Notes ==  "spine taken env#188"     |                                                                                          
                              Notes ==  "spine taken env#189"     |                                                                                          
                              Notes ==  "spine taken env#190"     |                                                                                          
                              Notes ==  "spine taken env#191"     |                                                                                          
                              Notes ==  "spine taken env#192"     |                                                                                          
                              Notes ==  "spine taken env#193"     |                                                                                          
                              Notes ==  "spine taken env#194"     |                                                                                          
                              Notes ==  "spine taken env#195"     |                                                                                          
                              Notes ==  "spine taken env#196"     |                                                                                          
                              Notes ==  "spine taken env#197"     |                                                                                          
                              Notes ==  "spine taken env#198"     |                                                                                          
                              Notes ==  "spine taken env#199"     |                                                                                          
                              Notes ==  "spine taken env#200"     |                                                                                          
                              Notes ==  "spine taken env#300"     |                                                                                          
                              Notes ==  "spine taken env#301"     |                                                                                          
                              Notes ==  "spine taken env#302"     |                                                                                          
                              Notes ==  "spine taken env#303"     |                                                                                          
                              Notes ==  "spine taken env#304"     |                                                                                          
                              Notes ==  "spine taken env#305"     |                                                                                          
                              Notes ==  "spine taken env#306"     |                                                                                          
                              Notes ==  "spine taken env#307"     |                                                                                          
                              Notes ==  "spine taken env#308"     |                                                                                          
                              Notes ==  "spine taken env#309"     |                                                                                          
                              Notes ==  "spine taken env#310"     |                                                                                          
                              Notes ==  "spine taken env#311"     |                                                                                          
                              Notes ==  "spine taken env#312"     |                                                                                          
                              Notes ==  "spine taken env#313"     |                                                                                          
                              Notes ==  "spine taken env#314"     |                                                                                          
                              Notes ==  "spine taken env#315"     |                                                                                          
                              Notes ==  "spine taken env#316"     |                                                                                          
                              Notes ==  "spine taken env#317"     |                                                                                          
                              Notes ==  "spine taken env#318"     |                                                                                          
                              Notes ==  "spine taken env#319"     |                                                                                          
                              Notes ==  "spine taken env#320"     |                                                                                          
                              Notes ==  "spine taken env#500"     |                                                                                          
                              Notes ==  "spine taken env#501"     |                                                                                          
                              Notes ==  "spine taken env#502"     |                                                                                          
                              Notes ==  "spine taken env#503"     |                                                                                          
                              Notes ==  "spine taken env#504"     |                                                                                          
                              Notes ==  "spine taken env#505"     |                                                                                          
                              Notes ==  "spine taken env#506"     |                                                                                          
                              Notes ==  "spine taken env#507"     |                                                                                          
                              Notes ==  "spine taken env#508"     |                                                                                          
                              Notes ==  "spine taken env#509"     |                                                                                          
                              Notes ==  "spine taken env#510"     |                                                                                          
                              Notes ==  "spine taken env#511"     |                                                                                          
                              Notes ==  "spine taken env#512"     |                                                                                          
                              Notes ==  "spine taken env#513"     |                                                                                          
                              Notes ==  "spine taken env#514"     |                                                                                          
                              Notes ==  "spine taken env#515"     |                                                                                          
                              Notes ==  "spine taken env#516"     |                                                                                          
                              Notes ==  "spine taken env#517"     |                                                                                          
                              Notes ==  "spine taken env#518"     |                                                                                          
                              Notes ==  "spine taken env#519"     |                                                                                          
                              Notes ==  "spine taken env#520"     |                                                                                          
                              Notes ==  "spine taken env#521"     |                                                                                          
                              Notes ==  "spine taken env#522"     |                                                                                          
                              Notes ==  "spine taken env#523"     |                                                                                          
                              Notes ==  "spine taken env#524"     |                                                                                          
                              Notes ==  "spine taken env#525"     |                                                                                          
                              Notes ==  "spine taken env#526"     |                                                                                          
                              Notes ==  "spine taken env#527"     |                                                                                          
                              Notes ==  "spine taken env#528"     |                                                                                          
                              Notes ==  "spine taken env#529"     |                                                                                          
                              Notes ==  "spine taken env#530"     |                                                                                          
                              Notes ==  "spine taken env#531"     |                                                                                          
                              Notes ==  "spine taken env#532"))
#


# trim columns not relevant to age structure stuff:

head(ntwin.ages)

ntwin.ages<-ntwin.ages[,-c(7,8,11:20)]

summary(ntwin.ages)

write.csv(ntwin.ages, "North Twin 2018 Age Inventory.csv")
