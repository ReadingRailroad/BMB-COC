# 'Simulation of joint live and dead encounter histories';
##SIMULATES JOINT LIVE AND DEAD DATA STRUCTURE AND OUTPUTS A LIST WITH 3 DATA FRAMES
# JOINT LIVE-DEAD DATA IN LDLD FORMAT
#DEAD ONLY IN LDLD FORMAT
#LIVE ONLY IN STANDARD CAPTURE HISTORY FORMAT

rm(list=ls())

setwd("~/BMB-COC")

n.occas=5
S<-rep(0.8,n.occas)
p<-c(1,rep(0.3,n.occas-1))
r<-rep(0.8,n.occas)
F<-rep(.7,n.occas)
n.marked<-10

joint.sim<-function(n.occas=5,S=S,p=p,r=r,F=F,n.marked=10)
{
joint.history<-live.history<-dead.history<-NULL
   for (icap in 1:n.occas)
        {         
    #   cat("icap=",icap,"\n")
      for( indiv in 1:n.marked)
          {      
        
    #  cat("indiv=",indiv,"\n")
		joint.caphist<-dead.caphist<-""
			for(i in 1:(2*n.occas))
			{
			joint.caphist<-paste0(joint.caphist,"0",collapse="")
                  dead.caphist<-paste0(dead.caphist,"0",collapse="")
			}
               j<-icap*2-1
         substr(joint.caphist,j,j)='1'
         substr(dead.caphist,j,j)='1'

		live.caphist<-""
			for(i in 1:n.occas)
			{
			live.caphist<-paste0(live.caphist,"0",collapse="")
                  }
         substr(live.caphist,icap,icap)='1'


         z<-a<-captured<-recovered<-array(0,dim=n.occas)
         z[icap]=a[icap]=captured[icap]=1
        # recovered[icap]<-rbinom(1,1,(1-z[icap])*r[icap])
     i=icap
	#for (i in icap:(n.occas-1)
       while (i<n.occas)
        {
        #alive next period
        z[i+1]<-rbinom(1,1,z[i]*S[i])
        #available for capture
        a[i+1]<-rbinom(1,1,z[i+1]*a[i]*F[i])
        #captured 
        captured[i+1]<-rbinom(1,1,a[i+1]*p[i+1])

	recovered[i]<-rbinom(1,1,z[i]*(1-z[i+1])*r[i])
       i<-i+1
	  }
      #last occas
      recovered[n.occas]<-rbinom(1,1,z[n.occas]*(1-S[n.occas])*r[n.occas])

     for (i in icap:n.occas)
       {
     j<-i*2-1
     substr(joint.caphist,j,j)=paste(captured[i])
     substr(joint.caphist,j+1,j+1)=paste(recovered[i])
     substr(dead.caphist,j+1,j+1)=paste(recovered[i])
    substr(live.caphist,i,i)=paste(captured[i])

       }


        joint.history<-append(joint.history,joint.caphist)
       dead.history<-append(dead.history,dead.caphist)
    live.history<-append(live.history,live.caphist)
      #   cat("caphist",caphist,"\n")
         
     } #indiv
   }  #icap
CH.joint<-data.frame(ch=joint.history)
CH.dead<-data.frame(ch=dead.history)
CH.live<-data.frame(ch=live.history)
out=list(joint=CH.joint,dead=CH.dead,live=CH.live)
}



sim.data<-joint.sim(n.occas=5,S=S,p=p,r=r,F=F,n.marked=1000)

require(RMark)

##RUN SIMULATED DATA FOR EACH DATA STRUCTURE THROUGH CORRESPONDING MODEL

#JOINT DATA -- BURNHAM MODEL
joint.data<-sim.data$joint
joint.data$ch<-as.character(joint.data$ch)
joint.processed=process.data(data=joint.data,model="Burnham")
joint.ddl=make.design.data(joint.processed)
joint.mod1<-mark(joint.processed,joint.ddl)
export.MARK(joint.processed, "joint",
replace = TRUE, chat = 1, title = "Joint simulated data",
ind.covariates = "all")

#LIVE ONLY DATA - CJS MODEL
live.data<-sim.data$live
live.data$ch<-as.character(live.data$ch)
live.processed=process.data(data=live.data,model="CJS")
live.ddl=make.design.data(live.processed)
live.mod1<-mark(live.processed,live.ddl)
export.MARK(live.processed, "live",
replace = TRUE, chat = 1, title = "Live simulated data",
ind.covariates = "all")

#DEAD ONLY -SEBER RECOVERY MODEL
dead.data<-sim.data$dead
dead.data$ch<-as.character(dead.data$ch)
dead.processed=process.data(data=dead.data,model="Recovery")
dead.ddl=make.design.data(dead.processed)
dead.mod1<-mark(dead.processed,dead.ddl)
export.MARK(dead.processed, "dead",
replace = TRUE, chat = 1, title = "Dead recovery simulated data",
ind.covariates = "all")

