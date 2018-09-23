population<-function(N=3, years=2015:2025) {
  
  PopData<-NULL
  CondData<-NULL
  student.ID<-0
  run.id<-0
  conditions<-c("Factual (no tx)","CF (tx at month 13)", "CF (tx at month 25)", "CF (tx at month 37)")
  
  for (Enrollment in years) {     # loop through each incoming class
    for (i in 1:N) {             # for each class, loop through each individual 
      
      
      # demographic distributions
      Gender <- rbinom(1, 1, .5)    # 0 = male, 1 = female 
      Age <- 17 + 53*rbeta(1, 2, 10)
      
      # increment and assign student.ID
      student.ID <- student.ID +1
      
      # draw from random distributions to construct the conditions for the individual student
      cond.fact<-list(
        initfgr=runif(1,.16,4),                       # initial fractioanl growth rate of individual
        init.well=0.8,                                # initial level of wellness
        micro.ES=0.05*rbinom(1,1,.6),                 # probability of individual experiencing primary insults of 5% wellness
        micro.period=1,                              # assumption is that if primary insults happen, they are monthly
        shock.ES=0.5*rbinom(1,1,0.08*(1+Gender*9)),    # gender based risk of experiencing secondary insult                              s
        shock.t = runif(1,1,45),                          # time of secondary insult if it happen
        frcs=runif(1,0,.5),                           # fractional rate of gaining coping skills
        frdr=runif(1,0,.5),                           # fractional rate of developing resilience
        tx.ES=1.5,                                    # treatment effect size
        tx.start=2,                                   # time of start of treatment
        tx.length=3)                                  # length of treatment
      
      # Simulate the factual condition -- no tx with micro and shock
      run.id<-run.id+1
      condition<-conditions[1]
      cond<-cond.fact
      cond["tx.ES"] <- 1
      tmp<-do.call(person, cond)                     # simulate  trajectories for individual
      tmp<-tmp[tmp[,"time"] %in% c(0,12,24,36,45),]     # save the matrix for given times for
      # 0, 12, 24, 36, and 45 months
      CondData<-rbind(CondData,                      # Add conditions to conditiona data frame
                      data.frame(c(run.id=run.id, condition=condition, cond)))     
      
      indData<-cbind(
        data.frame(
          student.ID=rep(student.ID,5),              # student.ID (for future use)
          Enrollment.Year=rep(Enrollment,5),         # Year that the student enrolled
          Gender=rep(Gender,5),                      # Gender of individual
          Condition=rep(condition,5),                # Condition
          Run=rep(run.id,5),                         # Simulation run id
          Year=rep(Enrollment,5) + 0:4,              # Year that data is collected
          Age=Age + 0:4),                            # Age of individual for that year
        as.data.frame(tmp)
      )
      
      # Simulate the counterfactual -- tx at month 13
      run.id<-run.id+1
      condition<-conditions[2]
      cond<-cond.fact
      cond["tx.start"]<- 13                              # tx starts at month 13
      CondData<-rbind(CondData,                      # Add conditions to conditiona data frame
                      data.frame(c(run.id=run.id, condition=condition, cond)))     
      
      tmp<-do.call(person, cond)                     # simulate  trajectories for individual
      tmp<-tmp[tmp[,"time"] %in% c(0,12,24,36,45),]     # save the matrix for given times for
      # 0, 12, 24, 36, 45 months
      indData<-rbind(indData,
                     cbind(
                       data.frame( 
                         student.ID=rep(student.ID,5),              # student.ID (for future use)
                         Enrollment.Year=rep(Enrollment,5),         # Year that the student enrolled
                         Gender=rep(Gender,5),                      # Gender of individual
                         Condition=rep(condition,5),                # Condition
                         Run=rep(run.id,5),                         # Simulation run id
                         Year=rep(Enrollment,5) + 0:4,              # Year that data is collected
                         Age=Age + 0:4),                            # Age of individual for that year
                       as.data.frame(tmp)
                     )
      )
      
      # Simulate the counterfactual -- tx at month 25
      run.id<-run.id+1
      condition<-conditions[3]
      cond<-cond.fact
      cond["tx.start"]<-25                               
      CondData<-rbind(CondData,                      # Add conditions to conditiona data frame
                      data.frame(c(run.id=run.id, condition=condition, cond)))     
      
      tmp<-do.call(person, cond)                     # simulate  trajectories for individual
      tmp<-tmp[tmp[,"time"] %in% c(0,12,24,36,45),]     # save the matrix for given times for
      # 0, 12, 24, 36, 45 months
      indData<-rbind(indData,
                     cbind(
                       data.frame( 
                         student.ID=rep(student.ID,5),              # student.ID (for future use)
                         Enrollment.Year=rep(Enrollment,5),         # Year that the student enrolled
                         Gender=rep(Gender,5),                      # Gender of individual
                         Condition=rep(condition,5),                # Condition
                         Run=rep(run.id,5),                         # Simulation run id
                         Year=rep(Enrollment,5) + 0:4,              # Year that data is collected
                         Age=Age + 0:4),                            # Age of individual for that year
                       as.data.frame(tmp)
                     )
      )
      
      # Simulate the counterfactual -- tx at month 37
      run.id<-run.id+1
      condition<-conditions[4]
      cond<-cond.fact
      cond["tx.start"]<-37                               
      CondData<-rbind(CondData,                      # Add conditions to conditiona data frame
                      data.frame(c(run.id=run.id, condition=condition, cond)))     
      
      tmp<-do.call(person, cond)                     # simulate  trajectories for individual
      tmp<-tmp[tmp[,"time"] %in% c(0,12,24,36,45),]     # save the matrix for given times for
      # 0, 12, 24, 36, and 45 months
      indData<-rbind(indData,
                     cbind(
                       data.frame( 
                         student.ID=rep(student.ID,5),              # student.ID (for future use)
                         Enrollment.Year=rep(Enrollment,5),         # Year that the student enrolled
                         Gender=rep(Gender,5),                      # Gender of individual
                         Condition=rep(condition,5),                # Condition
                         Run=rep(run.id,5),                         # Simulation run id
                         Year=rep(Enrollment,5) + 0:4,              # Year that data is collected
                         Age=Age + 0:4),                            # Age of individual for that year
                       as.data.frame(tmp)
                     )
      )
      
      
      # add inidivudal data to population data
      PopData<-rbind(PopData,indData)
    }
  }
  
  # Clean up and save complete years
  sel.vec<-as.numeric(names(table(PopData$Year)))[table(PopData$Year) == 5*N*length(conditions)] 
  PopData<-PopData[PopData$Year %in% sel.vec,]
  write.csv(PopData, file="PopData_92218.csv", row.names=FALSE)
  write.csv(CondData, file="CondData_92218.csv", row.names=FALSE)
}
