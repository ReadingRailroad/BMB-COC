setwd("~/Stat 534/Homework 3")
library(RMark)
fish <- import.chdata("fishdaily.txt", header = T)

#3C
fish_proc <- process.data(fish, model = 'CJS')
fish_ddl <- make.design.data(fish_proc)

ft <- list(formula=~time)
Phit_pt <- mark(data = fish_proc, 
              ddl = fish_ddl,
              model.parameters= list (Phi=ft, p=ft),
              invisible = FALSE,
              model.name = 'CJS')
Phit_pt$results$real

#3D r= daily survival probability
  #this tells RMark that the time intervals are different between occasions. Use the null model so
  #you only get one estimate of survival. if you used a time model you'd get time specific esitmates

fish_proc_int <- process.data(fish, model = 'CJS', time.intervals = c(5,14,10))

fish_ddl_int <- make.design.data(fish_proc_int)
fish_ddl_int$Phi
f0 <- list(formula=~1)
DAY <- mark(data = fish_proc_int, 
                    ddl = fish_ddl_int,
                    model.parameters= list (Phi=f0, p=ft),
                    invisible = FALSE,
                    model.name = 'CJS')
DAY$results$real #daily survival= 0.964 SE= 0.018


#3E
fish_proc_day <- process.data(fish, model = 'CJS') #5,14,10 are the differences in julian days between occasions
fish_ddl_day <- make.design.data(fish_proc_day)
fish_ddl_day$Phi
fish_ddl_day$Phi$day <- c(5,14,10,14,10,10)
fday <- list(formula=~-1 + day, link='log')

Phiday_pt <- mark(data = fish_proc_day, 
                    ddl = fish_ddl_day,
                    model.parameters= list (Phi=fday, p=ft), #fday tells the model to look at the difference in days for a survival estimate
                    invisible = FALSE,
                    model.name = 'CJS',
                    time.intervals = c(5,14,10))
Phiday_pt$results$real

#make model table
fish_results <- collect.models(lx = NULL,
                               type = 'CJS',
                               adjust = TRUE)
fish_model_table <- model.table(fish_results, model.name=F)
fish_model_table
