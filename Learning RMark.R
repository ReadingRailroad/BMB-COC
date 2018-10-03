#### Top Level
# This file is a follow-along of the RMark Appendix and Examples (AECL 611 folder on Desktop PC)

library(RMark)

example(dipper) # runs all examples at once. very cumbersome, hundreds of lines of output
rm(list=ls(all=T)) # removes objects from global environment
data(dipper) # attach dipper data
ls() # confirm it is attached and the only file


# Couple ways to examine the loaded-in df
str(dipper)
head(dipper)


# first model: Phi. and P.
myexample<-mark(dipper)
class(myexample)
summary(myexample)

########################################################################### 
#
#         Description of the output:
#
#   Real Parameters (Phi and P) are shown in PIM format
#   These would be shown by group if we added the group label to model
#   for Phi, the survival from time 1 to 2 is labeled as 1
#   for P, the recapture probability at time i is labeled with i
# 
###########################################################################


# work through if I forget to assign model to object
mark(dipper)
myexample2<-mark(dipper,filename="mark002")

# cleanup input and output files
rm(myexample2)
list.files()
cleanup(ask=F)
list.files()


# how to examine the PIMs 
PIMS(myexample,"Phi",simplified=F)
PIMS(myexample,"p",simplified =F)



# playing with the model.matrix command that creates design matrices for PIMS
### Note that the label names for Beta parameters are the column names for these

                 # Lowercase 'time' denotes seperate and discrete time parameters
model.matrix(~time,myexample$design.data$p[1:10])

                 # Uppercase 'Time" denotes continuous trend with intercept and slope
model.matrix(~Time,myexample$design.data$p[1:10])

                 # Continuous 'Time' and discrete 'age'
model.matrix(~Time+age,myexample$design.data$p[1:10])


# Trick number 3: simplifying the design matrix
PIMS(myexample,"Phi")
PIMS(myexample, "p")

##########################################################################
#
#
# Section c.4: the four functions of mark()
#
#
#########################################################################

# process.data()
      # takes input data frame and creates list for models, including
        # type of model
        # time of first capture/release
        # time intervals
        # groups
        # initial ages
        # nocc: number of capture/encounter occasions which is determined from the contents of "ch" field

  # Example
data(dipper)
dipper.process <- process.data(dipper,model="CJS",begin.time=1980,
                               time.intervals=c(1,.5,1,.75,.25,1), groups = "sex")
names(dipper.process) 
      # each name can be called using $
dipper.process$freq[1:10,]

############ If changing model, NEED to change process.data()
           # good to create new objects:
dipper.cjs<-process.data(dipper,model="CJS")
dipper.popan<-process.data(dipper,model="POPAN")


# make.design.data
    # PIM structure based on model, number of occasions, grouping variables, etc.
    # default design are cohort, age, time (lowercase: discrete) and any grouping variables
    # option to bin factor variables, ex: "age.bins" but...
        # default method is best and then use "add.design.data" for binning factors
    # also able to restrict parameters to use "time" or "constant" PIMS

  # EXAMPLE:
    # reprocess data to use annual time intervals rather than fictitious one above
dipper.process<- process.data(dipper,model="CJS",begin.time=1980,groups="sex")
dipper.ddl<-make.design.data(dipper.process) # design data list = ddl
myexample2<-mark(dipper.process,dipper.ddl)

  # non-simplified PIMS for phi and compare them to design data
PIMS(myexample2,"Phi",simplified=F)
names(dipper.ddl) # two parameters for CJS (Phi and p) and then a listy of PIM types
dipper.ddl$Phi
        # Row 1 and 22 have same design data, except for 1=Female and 22=male

######################################################################################
#
#
#     More examples (section C.5)
#
#
######################################################################################

# formula notation
Phi.dot<-list(formula=~1)
p.dot<-list(formula=~1)
myexample2<-mark(dipper.process,dipper.ddl,model.parameters=list(Phi=phi.dot,p=p.dot))

# create more parameter specifications (examples may not make sense biologically)
Phi.time<-list(formula=~time)
Phi.sex<-list(formula=~sex)
Phi.sexplusage<-list(formula=~sex+age)
p.time<-list(formula=~time)
p.Time<-list(formula=~Time)
p.Timeplussex<-list(formula=~Time+sex)

# Can incorporate model specifications in model object name
dipper.phi.dot.p.dot<-
  mark(dipper.process,dipper.ddl,model.parameters=list(Phi=Phi.dot,p=p.dot))

dipper.phi.time.p.dot<-
  mark(dipper.process,dipper.ddl,model.parameters=list(Phi=Phi.time,p=p.dot))

dipper.phi.sex.p.dot=
  mark(dipper.process,dipper.ddl,model.parameters=list(Phi=Phi.sex,p=p.dot))

dipper.phi.sex.p.Timeplussex=
  mark(dipper.process,dipper.ddl,model.parameters=list(Phi=Phi.sex,p=p.Timeplussex))

dipper.phi.time.p.time=
  mark(dipper.process,dipper.ddl,model.parameters=list(Phi=Phi.time,p=p.time))

dipper.phi.sexplusage.p.dot=
  mark(dipper.process,dipper.ddl,model.parameters=list(Phi=Phi.sexplusage,p=p.dot))

# recall output through "summary"
summary(dipper.phi.sex.p.Timeplussex)


# examine output object more:
names(dipper.phi.sex.p.Timeplussex)
        # output is the link to input and output files
dipper.phi.sex.p.Timeplussex$output
        # pims is the all-different pims for the model but PIMS function produces clearer output
dipper.phi.sex.p.Timeplussex$pims
PIMS(dipper.phi.sex.p.Timeplussex,"Phi",simplified=T)
PIMS(dipper.phi.sex.p.Timeplussex,"Phi",simplified=F)
        # design matrix for simplified model structure is contained in the results
dipper.phi.sex.p.Timeplussex$design.matrix
        # RESULTS
names(dipper.phi.sex.p.Timeplussex$results)
              # lnl: -2logL likelihood value
              # deviance: difference between null deviance and model deviance
              # npar: number of parameters (always number of columns in design matrix above)
              # n: effective sample size
              # AICc: Small sample corrected AIC using npar
              # beta: data frame of B parameters with estimate, standard error (se), lower & upper CI
              # real: data frame of unique (simplified) real parameters with estimate, se, lower & upper CI
              # beta.vcv: variance-covariance matrix for B
              # derived: dataframe of derived parameters, if any
              # derived.vcv: variance-covariance matrix for derived parameters, if any
              # covariate.values: datagrame with fields Variable and Value which are covariate names and value used for real parameter estimates
              # singular: indices of B parameters that are non-estimable or at a boundary

# all individual elements can be extracted using list notation, for example beta values:
dipper.phi.sex.p.Timeplussex$results$beta
          # or unique (simplified) real parameters
dipper.phi.sex.p.Timeplussex$results$real
                ########################## labels for real parameters in simplified model can be misleading
                                         # due to simplification process, therefore:
#view all real parameters with summary
summary(dipper.phi.sex.p.Timeplussex,se=T)
          # par.index field is index within simplified real parameters (recorded parameter index)
          # g is group
          # c is cohort
          # a for age
          # t for time
      # after each letter is the value of the variable.


################################
#
# Design Covariates in Mark
#
################################

# design sata (design covariate) already outlined: linked to parameters in model (time, cohort, sex)
# second type of covariates specify differences in individual animals
      # must be factor variable (typically small number of unique variables: M or F, Mirror or Leather)
      # value cannot change over time

# Individual covariates often used for numeric variables (mass, length) or when the value changes over time
  # need to create k-1 dummy variables for factor variables with k levels
  # design covariates are stored in design data (ddl) 