require(deSolve)
person <- function (frcs=0, frdr=0, initfgr=0.14, init.well=0.8,
                    micro.ES=0, micro.period=1, shock.ES = 0,
                    shock.t=0, tx.ES=1, tx.start=2, tx.length=3, 
                    DT=0.25) 
{
  # setup results
  
  rslt<-NULL
  
  # set parameters
  pars <- c(FRCS = frcs,              # fractional rate of developing coping skills
            FRDR = frdr,              # fractional rate of developing resilience
            a = 10,                   # parameter that controls shape of effect of 
            # wellness on fractional growth rate
            b = 0.25,                 # parameter that controls shape of effect of
            # wellness on fractional growth rate
            INITFGR = initfgr,        # initial fractional growth rate
            GL = 2,                   # goal for growth
            Insult1 = micro.ES,       # magnitude of primary insult 
            Period1 = micro.period,   # period of primary insult (months)
            Insult2 = shock.ES,       # magnitude of secondary insult 
            Time2 = shock.t,          # timing of secondary insult (months)
            
            ES = tx.ES,               # effect size of treatment as a harzard rate ratio
            t.start = tx.start,       # start of treatment (month)
            t.length = tx.length)     # legnth of treatment (months)
  
  
  # define model
  SD.model <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      Ef <- 1/(1+exp(-a*(WEL-b)))
      FGR <- R * Ef * INITFGR * Tx(Time, ES, t.start, t.length)
      GP <- GL-WEL
      GWTH <- FGR * GP
      
      dCS <- 0
      dR  <- 0
      dWEL <- GWTH
      dINSLT <- 0
      
      return(list(c(dCS, dR, dWEL, dINSLT)))
    })
  }
  
  # define treatment function
  Tx <- function(t, ES, t.start, t.length) {
    rslt<-NULL
    if (t> t.start & t<=(t.start+t.length)) {
      rslt<-1+1
      rslt<-ES}
    else rslt<-1
    return(rslt)
  }
  
  yinit <- c(CS=1, R=1, WEL=init.well, INSLT=0)
  times <- seq(-2,48,DT)
  
  if (micro.ES >0 | shock.ES > 0 ) {
    EVENTS<-TRUE
    insults <- function(t, y, pars, ...) {
      with (as.list(c(y,pars)), {
        insult <- (if (t %% micro.period == 0) micro.ES else 0) +
          (if (t > shock.t & t <= (shock.t+DT)) shock.ES else 0) 
        INSLT <- INSLT + insult
        CS <- CS + CS*FRCS*insult
        R <- R + R*FRDR*insult*(2-R)/2
        WEL <- WEL - WEL*insult/CS
        return(c(CS, R, WEL, INSLT))
      })
    }
  }
  else EVENTS<-FALSE
  
  return(
    if (EVENTS) 
      ode(yinit, times, SD.model, pars, method="rk4", 
          events=list(func=insults,time=times))
    else 
      ode(yinit, times, SD.model, pars, method="rk4")
  )
}

