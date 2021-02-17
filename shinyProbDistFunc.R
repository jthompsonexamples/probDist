
font <- list(
  family = "Roboto Condensed",
  size = 15,
  color = "000000"
)
label <- list(
  bgcolor = "#FFFFFF",
  bordercolor = "#000000",
  font = font
)

fDist <- function(n1, n2){
  # Debug values
  # n1 <- 100
  # n2 <- 100
  # Try to convert the inputs to numeric
  n1 <- as.numeric(n1); n2 <- as.numeric(n2)
  # If either input is not numeric return an error plot
  if (any(is.na(c(n1, n2)))){
    p <- ggplot()+
      geom_text(aes(x=0, y=0, label='INPUT ERROR'), size=10)
  }
  else{
    # change Inf to very large numbers
    n1 <- ifelse(n1==Inf, 100000, n1)
    n2 <- ifelse(n2==Inf, 100000, n2)
    # create a data frame for the density
    # df <- data.frame(x=seq(qf(0.0001, n1, n2),qf(0.9999, n1, n2), by=0.0001),
    #                  y=df(seq(qf(0.0001, n1, n2),qf(0.9999, n1, n2), by=0.0001), n1, n2),
    #                  group=1)
    df <- data.frame(x=seq(qf(0.0001, n1, n2),qf(0.9999, n1, n2), length.out=10000),
                     y=df(seq(qf(0.0001, n1, n2),qf(0.9999, n1, n2), length.out=10000), n1, n2),
                     group=1)
    # make the hovertext info
    # remember that lower.tail=T gives the left side filled in
    df$text <- paste0('P(X<',round(df$x,5),') = ',round(pf(round(df$x,5), n1, n2),5),'<br>',
                      'P(X>',round(df$x,5),') = ',round(pf(round(df$x,5), n1, n2, lower.tail = F),5))
    # make the line plot
    p <- ggplot(df, aes(x=x, y=y, group=1), color='black')+
      geom_path(aes(text=text), color='black')+
      #xlim(0,round(qf(0.999, n1, n2),1))+
      labs(y= "Density",
           x= "x")
  }
  # return whichever plot
  p
}

normDist <- function(mu, sig){
  # Debug values
  # mu <- 0
  # sig <- 1
  # Try to convert the inputs to numeric
  mu <- as.numeric(mu); sig <- as.numeric(sig)
  # If either input is not numeric return an error plot
  if (any(is.na(c(mu, sig)))){
    p <- ggplot()+
      geom_text(aes(x=0, y=0, label='INPUT ERROR'), size=10)
  }
  else{
    # change Inf to very large numbers
    mu <- ifelse(mu==Inf, 100000, mu)
    sig <- ifelse(sig==Inf, 100000, sig)
    # create a data frame for the density
    
    df <- data.frame(x=seq(qnorm(0.0001, mu, sig),qnorm(0.9999, mu, sig), length.out=10000),
                     y=dnorm(seq(qnorm(0.0001, mu, sig),qnorm(0.9999,mu, sig), length.out=10000), mu, sig),
                     group=1)
    # make the hovertext info
    # remember that lower.tail=T gives the left side filled in
    df$text <- paste0('P(X<',round(df$x,5),') = ',round(pnorm(round(df$x,5), mu, sig),5),'<br>',
                      'P(X>',round(df$x,5),') = ',round(pnorm(round(df$x,5), mu, sig, lower.tail = F),5))
    # make the line plot
    p <- ggplot(df, aes(x=x, y=y, group=1), color='black')+
      geom_path(aes(text=text), color='black')+
      #xlim(0,round(qf(0.999, n1, n2),1))+
      labs(y= "Density",
           x= "x")
  }
  # return whichever plot
  p
}

tDist <- function(mu, nu, norm=F){
  # Debug values
  # mu <- 3
  # nu <- 10
  # Try to convert the inputs to numeric
  mu <- as.numeric(mu); nu <- as.numeric(nu)
  # If either input is not numeric return an error plot
  if (any(is.na(c(mu, nu)))){
    p <- ggplot()+
      geom_text(aes(x=0, y=0, label='INPUT ERROR'), size=10)
  }
  else{
    # change Inf to very large numbers
    mu <- ifelse(mu==Inf, 100000, mu)
    nu <- ifelse(nu==Inf, 100000, nu)
    # create a data frame for the density
    if(!norm){
    df <- data.frame(x=seq(qt(0.0001, ncp=mu, df=nu),qt(0.9999, ncp=mu, df=nu), length.out=10000),
                     y=dt(seq(qt(0.0001, ncp=mu, df=nu),qt(0.9999, ncp=mu, df=nu), length.out=10000), ncp=mu, df=nu),
                     group=1)
    # make the hovertext info
    # remember that lower.tail=T gives the left side filled in
    df$text <- paste0('P(X<',round(df$x,5),') = ',round(pt(round(df$x,5), ncp=mu, df=nu),5),'<br>',
                      'P(X>',round(df$x,5),') = ',round(pt(round(df$x,5), ncp=mu, df=nu, lower.tail = F),5))
    
    # make the line plot
    p <- ggplot(df, aes(x=x, y=y, group=1), color='black')+
      geom_path(aes(text=text), color='black')+
      #xlim(0,round(qf(0.999, n1, n2),1))+
      labs(y= "Density",
           x= "x")
    }else{
      df <- data.frame(x=seq(qt(0.0001, ncp=mu, df=nu),qt(0.9999, ncp=mu, df=nu), length.out=10000),
                       studentT=dt(seq(qt(0.0001, ncp=mu, df=nu),qt(0.9999, ncp=mu, df=nu), length.out=10000), ncp=mu, df=nu))
      df$Normal <- dnorm(df$x, mu, 1)
      # make the hovertext info
      # remember that lower.tail=T gives the left side filled in
      
      dfText <- data.frame(studentT=paste0('P(X<',round(df$x,5),') = ',round(pt(round(df$x,5), ncp=mu, df=nu),5),'<br>',
                                           'P(X>',round(df$x,5),') = ',round(pt(round(df$x,5), ncp=mu, df=nu, lower.tail = F),5)),
                           Normal=paste0('P(X<',round(df$x,5),') = ',round(pnorm(round(df$x,5), mu, 1),5),'<br>',
                                         'P(X>',round(df$x,5),') = ',round(pnorm(round(df$x,5), mu, 1, lower.tail = F),5)))
      
      df <- pivot_longer(df, studentT:Normal, names_to = 'Distribution', values_to = 'y')
      
      df <- bind_rows(df %>% filter(Distribution=='Normal') %>% mutate(text=dfText$Normal),
                      df %>% filter(Distribution=='studentT') %>% mutate(text=dfText$studentT))
      
      cols <- c("#3591d1","#000000")
      p <- p <- ggplot(df,aes(x=x, y=y, color=Distribution))+
        geom_path()+
        geom_path(aes(text=text))+
        scale_color_manual(name='', values=cols)+
        labs(y= "Density",
             x= "x")
    }
  }
  # return whichever plot
  p
}

chiDist <- function(nu, mu){
  # Debug values
  # mu <- 0
  # nu <- 10
  # Try to convert the inputs to numeric
  mu <- as.numeric(mu); nu <- as.numeric(nu)
  # If either input is not numeric return an error plot
  if (any(is.na(c(mu, nu)))){
    p <- ggplot()+
      geom_text(aes(x=0, y=0, label='INPUT ERROR'), size=10)
  }
  else{
    # change Inf to very large numbers
    mu <- ifelse(mu==Inf, 100000, mu)
    nu <- ifelse(nu==Inf, 100000, nu)
    # create a data frame for the density
    
    df <- data.frame(x=seq(qchisq(0.0001, nu, mu),qchisq(0.9999, nu, mu), length.out=10000),
                     y=dchisq(seq(qchisq(0.0001, nu, mu),qchisq(0.9999, nu, mu), length.out=10000), nu, mu))
    # make the hovertext info
    # remember that lower.tail=T gives the left side filled in
    df$text <- paste0('P(X<',round(df$x,5),') = ',round(pchisq(round(df$x,5), nu, mu),5),'<br>',
                      'P(X>',round(df$x,5),') = ',round(pchisq(round(df$x,5), nu, mu, lower.tail = F),5))
    # make the line plot
    p <- ggplot(df, aes(x=x, y=y, group=1), color='black')+
      geom_path(aes(text=text), color='black')+
      #xlim(0,round(qf(0.999, n1, n2),1))+
      labs(y= "Density",
           x= "x")
  }
  # return whichever plot
  p
}

expDist <- function(lambda){
  # Debug values
  # mu <- 0
  # nu <- 10
  # Try to convert the inputs to numeric
  lambda <- as.numeric(lambda)
  # If either input is not numeric return an error plot
  if (any(is.na(c(lambda)))){
    p <- ggplot()+
      geom_text(aes(x=0, y=0, label='INPUT ERROR'), size=10)
  }
  else{
    # change Inf to very large numbers
    lambda <- ifelse(lambda==Inf, 100000, lambda)
    # create a data frame for the density
    
    df <- data.frame(x=seq(qexp(0.0001, lambda),qexp(0.9999, lambda), length.out=10000),
                     y=dexp(seq(qexp(0.0001, lambda),qexp(0.9999, lambda), length.out=10000), lambda))
    # make the hovertext info
    # remember that lower.tail=T gives the left side filled in
    df$text <- paste0('P(X<',round(df$x,5),') = ',round(pexp(round(df$x,5), lambda),5),'<br>',
                      'P(X>',round(df$x,5),') = ',round(pexp(round(df$x,5), lambda, lower.tail = F),5))
    # make the line plot
    p <- ggplot(df, aes(x=x, y=y, group=1), color='black')+
      geom_path(aes(text=text), color='black')+
      #xlim(0,round(qf(0.999, n1, n2),1))+
      labs(y= "Density",
           x= "x")
  }
  # return whichever plot
  p
}

binomDist <- function(n, pSuc, normApprox=F){
  # Debug values
  # n <- 10
  # pSuc <- 0.25
  # Try to convert the inputs to numeric
  n <- as.numeric(n); pSuc <- as.numeric(pSuc)
  # If either input is not numeric return an error plot
  if (any(is.na(c(n, pSuc)))){
    p <- ggplot()+
      geom_text(aes(x=0, y=0, label='INPUT ERROR'), size=10)
  }
  else{
    # change Inf to very large numbers
    n <- ifelse(n==Inf, 100000, n)
    # create a data frame for the density
    
    df <- data.frame(x=seq(qbinom(0, n, pSuc),qbinom(1, n, pSuc), by=1),
                     y=dbinom(seq(qbinom(0, n, pSuc),qbinom(1, n, pSuc), by=1), n, pSuc))
    # make the hovertext info
    # remember that lower.tail=T gives the left side filled in
    df$text <- paste0('P(X<',round(df$x,5),') = ',round(pbinom(round(df$x,5), n, pSuc),5),'<br>',
                      'P(X>',round(df$x,5),') = ',round(pbinom(round(df$x,5), n, pSuc, lower.tail = F),5))
    # make the bar plot
    p <- ggplot(df, aes(x=x, y=y, group=1), color='black')+
      geom_col(aes(text=text), color='black')+
      #geom_path(data=dfNorm, aes(x=x, y=y, text=text), color='black')+
      labs(y= "Density",
           x= "x")
  }
  if (normApprox){
    mu <- n*pSuc; sig <- sqrt(n*pSuc*(1-pSuc))
    
    dfNorm <- data.frame(x=seq(qnorm(0.001, mu, sig),qnorm(0.999, mu, sig), length.out=10000),
                         y=dnorm(seq(qnorm(0.001, mu, sig),qnorm(0.999,mu, sig), length.out=10000), mu, sig))
    # make the hovertext info
    # remember that lower.tail=T gives the left side filled in
    dfNorm$text <- paste0('P(X<',round(dfNorm$x,5),') = ',round(pnorm(round(dfNorm$x,5), mu, sig),5),'<br>',
                          'P(X>',round(dfNorm$x,5),') = ',round(pnorm(round(dfNorm$x,5), mu, sig, lower.tail = F),5))
    p <- p + geom_path(data=dfNorm, aes(x=x, y=y, text=text), color='royalblue')
  }
  # return whichever plot
  p
}

discreteNormApprox <- function(mu, sig, digits){
  # Debug values
  # mu <- 0
  # sig <- 1
  # Try to convert the inputs to numeric
  mu <- as.numeric(mu); sig <- as.numeric(sig)
  # If either input is not numeric return an error plot
  if (any(is.na(c(mu, sig)))){
    p <- ggplot()+
      geom_text(aes(x=0, y=0, label='INPUT ERROR'), size=10)
  }
  else{
    # change Inf to very large numbers
    mu <- ifelse(mu==Inf, 100000, mu)
    sig <- ifelse(sig==Inf, 100000, sig)
    # create a data frame for the density
    
    df <- data.frame(x=seq(qnorm(0.0001, mu, sig),qnorm(0.9999, mu, sig), length.out=10000),
                     y=dnorm(seq(qnorm(0.0001, mu, sig),qnorm(0.9999,mu, sig), length.out=10000), mu, sig))
    # make the hovertext info
    # remember that lower.tail=T gives the left side filled in
    df$text <- paste0('P(X<',round(df$x,5),') = ',round(pnorm(round(df$x,5), mu, sig),5),'<br>',
                      'P(X>',round(df$x,5),') = ',round(pnorm(round(df$x,5), mu, sig, lower.tail = F),5))
    
    dfApprox <- data.frame(x=seq(round(qnorm(0.0001, mu, sig),digits),round(qnorm(0.9999, mu, sig),digits), by=1*10**-digits),
                           y=dnorm(seq(round(qnorm(0.0001, mu, sig),digits),round(qnorm(0.9999, mu, sig),digits), by=1*10**-digits), mu, sig))
    
    dfApprox$text <- paste0('P(X<',round(dfApprox$x,5),') = ',round(pnorm(round(dfApprox$x,5), mu, sig),5),'<br>',
                            'P(X>',round(dfApprox$x,5),') = ',round(pnorm(round(dfApprox$x,5), mu, sig, lower.tail = F),5))
    
    # make the line plot
    p <- ggplot(df, aes(x=x, y=y, group=1), color='black')+
      geom_path(aes(text=text), color='black')+
      geom_col(data=dfApprox, aes(x=x, y=y, text=text), color='red')+
      labs(y= "Density",
           x= "x")
  }
  # return whichever plot
  p
}

getCriticalF <- function(a, n1, n2){
  # Debug values
  # a=0.05
  # n1 <- 10
  # n2 <- 15
  # Try to convert the inputs to numeric
  a <- as.numeric(a); n1 <- as.numeric(n1); n2 <- as.numeric(n2)
  # If either input is not numeric return an error plot
  if (any(is.na(c(a, n1, n2)))){
    tt <- "<b>Input Error</b>"
  }
  else{
    tt <- paste0('Right Tail: ',
                 round(qf(a, n1, n2, lower.tail=F),3),
                 '<br>Left Tail: ',
                 round(qf(a, n1, n2, lower.tail=T),3)
    )
  }
  return(tt)
}

getCriticalNorm <- function(a, mu, sig){
  # Debug values
  # a=0.05
  # n1 <- 10
  # n2 <- 15
  # Try to convert the inputs to numeric
  a <- as.numeric(a); mu <- as.numeric(mu); sig <- as.numeric(sig)
  # If either input is not numeric return an error plot
  if (any(is.na(c(a, mu, sig)))){
    tt <- "<b>Input Error</b>"
  }
  else{
    tt <- paste0('Right Tail: ',
                 round(qnorm(a, mu, sig, lower.tail=F),3),
                 '<br>Left Tail: ',
                 round(qnorm(a, mu, sig, lower.tail=T),3)
    )
  }
  return(tt)
}

getCriticalT <- function(a, mu, nu){
  # Debug values
  # a <- 0.05
  # mu <- 3
  # nu <- 10
  # Try to convert the inputs to numeric
  a <- as.numeric(a); mu <- as.numeric(mu); nu <- as.numeric(nu)
  # If either input is not numeric return an error plot
  if (any(is.na(c(a, mu, nu)))){
    tt <- "<b>Input Error</b>"
  }
  else{
    tt <- paste0('Right Tail: ',
                 round(qt(a, ncp=mu, df=nu, lower.tail=F),3),
                 '<br>Left Tail: ',
                 round(qt(a, ncp=mu, df=nu, lower.tail=T),3)
    )
  }
  return(tt)
}

getCriticalChi <- function(a, nu, mu){
  # Debug values
  # a=0.05
  # n1 <- 10
  # n2 <- 15
  # Try to convert the inputs to numeric
  a <- as.numeric(a); nu <- as.numeric(nu); mu <- as.numeric(mu)
  # If either input is not numeric return an error plot
  if (any(is.na(c(a, nu, mu)))){
    tt <- "<b>Input Error</b>"
  }
  else{
    tt <- paste0('Right Tail: ',
                 round(qchisq(a, nu, mu, lower.tail=F),3),
                 '<br>Left Tail: ',
                 round(qchisq(a, nu, mu, lower.tail=T),3)
    )
  }
  return(tt)
}