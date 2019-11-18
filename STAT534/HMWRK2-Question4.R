library(RMark)

####data to import####

newt<-import.chdata('newtB.txt', field.types = c('n','f'))

####no group#####

run.newt<-function(){
  f0<-list(formula= ~1)
  f0s<-list(formula=~1, share=TRUE)
  ft<- list(formula=~time, share=TRUE)
  fT<- list(formula=~Time, share=TRUE)
  
  m0<-mark(newt, model = "Closed", model.parameters = list(p=f0s,f0=f0), output = F)
  mt<-mark(newt, model='Closed',  model.parameters= list(p=ft, f0=f0), output = F)
  mT<-mark(newt, model = 'Closed', model.parameters =list(p=fT, f0=f0), output=F)
  
  return(collect.models())
}

newt.model<-run.newt()

model.table(newt.model, model.name=F)

newt.model$mt$results$derived

######groups#######

run.newtb<-function(){
  f0<-list(formula= ~1)
  fs<-list(formula=~sex)
  f0s<-list(formula=~1, share=TRUE)
  ft<- list(formula=~time, share=TRUE)
  fT<- list(formula=~Time, share=TRUE)
 
  m0<-mark(newt, model = "Closed", model.parameters = list(p=f0s,f0=fs), groups=c('sex'), output = F)
  mt<-mark(newt, model='Closed',  model.parameters= list(p=ft, f0=fs), groups=c('sex'), output = F)
  mT<-mark(newt, model = 'Closed', model.parameters =list(p=fT, f0=fs), groups=c('sex'), output=F)

  return(collect.models())
}

newtb.model<-run.newtb()

model.table(newtb.model, model.name=F)

newtb.model$mt$results$derived