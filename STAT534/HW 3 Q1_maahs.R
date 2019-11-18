setwd("~/Stat 534/Homework 3")
library(RMark)
cormorant <- import.chdata("cormorantLD.txt", header = T, field.types=c('n'))

#1B
#process data and specify model
cormorant_proc <- process.data(cormorant, model='Recovery')

#create design matrix
cormorant_ddl <- make.design.data(cormorant_proc)
cormorant_ddl$S
cormorant_ddl$r

f0 <- list(formula=~1)
ft <- list(formula=~time)

S0_rt <- mark(data = cormorant_proc, 
                   ddl = cormorant_ddl,
                   model.parameters= list (S=f0, r=ft),
                   invisible = FALSE,
                   model.name = 'Recovery')
S0_rt$results$real #survival = 0.51 (0.44, 0.58)

#1C
St_rt <- mark(data = cormorant_proc, 
              ddl = cormorant_ddl,
              model.parameters= list (S=ft, r=ft),
              invisible = FALSE,
              model.name = 'Recovery')
St_rt$results$real #survival = the first estimate is unestimatable and last S is confounded

#1D
fT <- list(formula=~Time)

ST_rt <- mark(data = cormorant_proc, 
              ddl = cormorant_ddl,
              model.parameters= list (S=fT, r=ft),
              model.name = 'Recovery')
ST_rt$results$real #

#1E
cormorant_results <- collect.models(lx = NULL,
                                    type = 'Recovery',
                                    table = TRUE,
                                    adjust = TRUE)
cormorant_model_table <- model.table(cormorant_results, model.name = F)
cormorant_model_table #St_rt has the lowest AIC and all the weight
