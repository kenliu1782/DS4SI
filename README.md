---
title: "Final Project UKIP Media"
author: "Ken Liu, Zeyu Ding"
date: "11/28/2020"
output: pdf_document
---

```{r setup, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE, echo=FALSE, fig.height=4, fig.width=8}
if(!requireNamespace("here"))
  install.packages("here", repos = "https://cloud.r-project.org")
library("here")
dir.create("data")
if(!requireNamespace("renv"))
  install.packages("renv", repos = "https://cloud.r-project.org")
library("renv")
renv::consent(provided = TRUE)
renv::init(force=TRUE, restart = FALSE)

```

# Hypothesises & reproducibility
> H1: *Increases in media coverage of UKIP will be associated with future increases in public support for UKIP, controlling for previous changes in public support.*

> H2: *Increases in public support for UKIP will be associated with future increases in media coverage of UKIP, controlling for previous changes in media coverage.*

One of the problems we encountered as we tried to reproduce what the researchers did is that all of the datasets were in .tab formats instead of .csv formats. That creates some troubles for us to load data and do analyses. Therefore, we converted these datasets through google drive and upload tjem to our clouds, enabling us to share with each other conveniently as well as to reproduce the models. 


```{r, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE}
require(lubridate)
require(ggplot2)
require(timeSeries)
require(tseries)
require(reshape2)
require(scales)
require(vars)
require(car)
require(dynlm)
require(stargazer)
require(sandwich)
require(forecast)
require(zoo)
require(xts)
require(lubridate)
require(plyr)
require(grid)

options(scipen=999)
options(digits=2)
set.seed(345)
options(xtable.comment = FALSE)

df<-read.csv("https://osf.io/f5yg4/Download")
df$Date<-mdy(df$Date)

df$UKIP.Vote<-interpNA(df$UKIP.Vote, method = "linear")

df$Salience.Imm<-interpNA(df$Salience.Imm, method = "linear")

df$General.Elections<-ifelse(df$General.Election2005==1 | df$General.Election2010==1 |
                        df$General.Election2015==1, 1, 0)

df$EU.Elections<-ifelse(df$Euro.Election2004==1 | 
                  df$Euro.Election2009==1 |
                  df$Euro.Election2014==1,
                  1, 0)

unscaled<-df[c("Date", "UKIP.Vote", "UKIP.Articles", "Unemployment", "Salience.Imm", "General.Elections", "EU.Elections", "EU.Ref",
               "Worker.Rights", "Cameron.Fruitcakes", 
               "Leader.Articles", "Farage.Articles",  "European.Share.Step",
               "European.Share.Pulse", "National.Share.Step", "National.Share.Pulse")]

cbbPalette <- c("#D55E00", "#0072B2","#009E73","#999999", "#56B4E9","#CC79A7")

plot1 <- ggplot(df, aes(x=Date, y=UKIP.Vote)) +
            geom_line(colour="#0072B2", linetype=2) +
            scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year",
               limits = as.Date(c('2004-01-01','2017-04-01'))) +
            labs(y="Vote Intention (%)", x="", title="Dynamics of UKIP Support and Media Coverage") +
            theme_bw() +
            theme(axis.text.y = element_text(size=12.45),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                plot.title = element_text(hjust = 0.5))
                
```

```{r, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE}
plot2 <- ggplot(df, aes(x=Date, y=UKIP.Articles)) +
          geom_line(colour="#D55E00", linetype=1) +
          scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year",
               limits = as.Date(c('2004-01-01','2017-04-01'))) +
          labs(y="Articles", x="") +
          theme_bw() +
          theme(axis.text.y = element_text(size=7.25),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
```

```{r, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE}

df$UKIP.Articles<-scale(df$UKIP.Articles)[,]
df$UKIP.Vote<-scale(df$UKIP.Vote)[,]

# Both series in one plot

graph.df<-subset(df, select=c("Date", "UKIP.Articles", "UKIP.Vote"))
molten<-melt(graph.df, id.vars=c("Date"))

plot3 <- ggplot(molten, aes(x=as.Date(Date))) +
            geom_line(aes(y=value, colour=variable, linetype=variable)) +
            scale_colour_manual(values=cbbPalette) +
            scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year",
               limits = as.Date(c('2004-01-01','2017-04-01'))) +
            labs(x="", y="Standardized Values") +
            theme_bw() +
            theme(legend.position="none",
                  legend.title=element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1, size=12),
                  axis.text.y = element_text(size=12.45))
    
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), ggplotGrob(plot3), size = "last"))
```

# vector autoregression (VAR)

\begin{equation}
 \label{eq:VAR}
   y_t = A_1 y_{t-1} + \dots + A_p y_{t-p} + D_t + u_t
\end{equation}



```{r party-models, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE, echo=FALSE}

# adf.test(df$UKIP.Articles)  # non-stationary
# adf.test(df$UKIP.Vote) # non-stationary

modelvars1<-unscaled

modelvars1$UKIP.Articles.d<-diff(zoo(scale(modelvars1$UKIP.Articles)), na.pad=T)
modelvars1$UKIP.Articles.d.l<-lag(modelvars1$UKIP.Articles.d, k=-1, na.pad=T)
modelvars1$UKIP.Articles.d<-as.numeric(modelvars1$UKIP.Articles.d)
modelvars1$UKIP.Articles.d.l<-as.numeric(modelvars1$UKIP.Articles.d.l)
modelvars1$UKIP.Vote.d<-diff(zoo(scale(modelvars1$UKIP.Vote)), na.pad=T)
modelvars1$UKIP.Vote.d.l<-lag(modelvars1$UKIP.Vote.d,k=-1, na.pad=T)
modelvars1$UKIP.Vote.d<-as.numeric(modelvars1$UKIP.Vote.d)
modelvars1$UKIP.Vote.d.l<-as.numeric(modelvars1$UKIP.Vote.d.l)

modelvars1$UKIP.Articles.notlog<-modelvars1$UKIP.Articles
modelvars1$UKIP.Vote.notlog<-modelvars1$UKIP.Vote

modelvars1$UKIP.Articles<-log(modelvars1$UKIP.Articles)
modelvars1$Leader.Articles<-log(modelvars1$Leader.Articles+1)
modelvars1$Farage.Articles<-log(modelvars1$Farage.Articles+1)
modelvars1$UKIP.Vote<-log(modelvars1$UKIP.Vote)

diffvars<-data.frame()  # difference variables and lag dates/elections to match
diffvars<-as.data.frame(cbind(modelvars1$Date[2:length(modelvars1$Date)],
                              diff(modelvars1$UKIP.Vote),
                              diff(modelvars1$UKIP.Articles),
                              diff(modelvars1$Unemployment),
                              diff(modelvars1$Salience.Imm),
                              modelvars1[2:length(modelvars1$Date), 6:16]))
names(diffvars)<-c("Date", "UKIP.Vote", "UKIP.Articles","Unemployment",
                   "Immigration", "General.Elections", "EU.Elections",
                   "EU.Ref", "Worker.Rights", "Cameron", "Leader.Articles",
                   "Farage.Articles", "European.Share.Step", "European.Share.Pulse",
                   "National.Share.Step", "National.Share.Pulse")
row.names(diffvars)<-NULL

# adf.test(diffvars$UKIP.Articles)  # stationary after differencing
# adf.test(diffvars$UKIP.Vote)   # stationary after differencing

# Main model, dummies for general elections and EU elections
varmodel<-VAR(diffvars[c(3,2)],
              lag.max=10,
              ic="AIC",
              type="both",
              exogen=diffvars[c(6,7)])

# serial.test(varmodel) 
# arch.test(varmodel)
# stab<-stability(varmodel)
# plot(stab)
# summary(varmodel)
# causality(varmodel, cause="UKIP.Articles", vcov.=vcovHC(varmodel))

# Auxiliary models/robustness checks

# Election controls included as step vars
varmodel.steps<-VAR(diffvars[c(3,2)],
                    lag.max=10,
                    ic="AIC",
                    type="both",
                    exogen=diffvars[c(13,15)])

# Election controls included as pulse vars
varmodel.pulses<-VAR(diffvars[c(3,2)],
                     lag.max=10,
                     ic="AIC",
                     type="both",
                     exogen=diffvars[c(14,16)])

# Adding unemployment and immigration
diffvars.unemp <-diffvars[complete.cases(diffvars),]

varmodel2<-VAR(diffvars.unemp[c(3,2)],
              lag.max=10,
              ic="AIC",
              type="both",
              exogen=diffvars.unemp[c(6:7,4:5)])
# serial.test(varmodel2)
# arch.test(varmodel2)
# stab2<-stability(varmodel2)
# plot(stab2)
# summary(varmodel2)
# causality(varmodel2, cause="UKIP.Articles", vcov.=vcovHC(varmodel2))

# Adding referendum, work-restriction lifting, Cameron's comments
varmodel3<-VAR(diffvars[c(3,2)],
              lag.max=10,
              ic="AIC",
              type="both",
              exogen=diffvars[c(8:10)])
# serial.test(varmodel3)  # big p = good
# arch.test(varmodel3)    # big p = good
# stab3<-stability(varmodel3)
# plot(stab3)
# summary(varmodel3)

```

```{r leader-models, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE, echo=FALSE}

diffvars<-data.frame()  # difference variables and lag dates/elections to match
diffvars<-as.data.frame(cbind(modelvars1$Date[2:length(modelvars1$Date)],
                              diff(modelvars1$UKIP.Vote),
                              diff(modelvars1$Leader.Articles),
                              diff(modelvars1$Farage.Articles),
                              diff(modelvars1$Unemployment),
                              diff(modelvars1$Salience.Imm),
                              modelvars1[2:length(modelvars1$Date), 6:16]))
names(diffvars)<-c("Date", "UKIP.Vote", "Leader.Articles", "Farage.Articles","Unemployment",
                   "Immigration", "General.Elections", "EU.Elections", "EU.Ref",
                   "Worker.Rights", "Cameron", "Leader", "Farage", "European.Share.Step",
                   "European.Share.Pulse", "National.Share.Step", "National.Share.Pulse")
row.names(diffvars)<-NULL


# adf.test(diffvars$UKIP.Articles)  # stationary after differencing
# adf.test(diffvars$UKIP.Vote)   # stationary after differencing

# Main leader model, w/ dummies for Gen and EU elections, same as main model

diffvars.unemp2 <- diffvars[complete.cases(diffvars),]

varmodel.leader<-VAR(diffvars.unemp2[c(3,2)],
                     lag.max=10,
                     ic="AIC",
                     type="both",
                     exogen=diffvars.unemp2[c(14,16, 5:6, 4)]) # election steps

varmodel.leader2<-VAR(diffvars[c(3,2)],
                      lag.max=10,
                      ic="AIC",
                      type="both",
                      exogen=diffvars[c(14,16, 9:11,4)]) # events

varmodel.leader3<-VAR(diffvars.unemp2[c(3,2)],
                      p=4,
                      type="both",
                      exogen=diffvars.unemp2[c(14,16, 5:6, 9:11, 4)]) # Farage

# summary(varmodel.leader2)
# serial.test(varmodel.leader3)
# arch.test(varmodel.leader3)
# stab<-stability(varmodel.leader)
# plot(stab)
# summary(varmodel.leader)
# causality(varmodel.leader2, cause="Leader.Articles", vcov.=vcovHC(varmodel.leader2, type="HC"))

```

```{r, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE, echo=FALSE, results='asis'}

stargazer(varmodel$varresult$UKIP.Vote,
          varmodel$varresult$UKIP.Articles,
          varmodel2$varresult$UKIP.Vote,
          varmodel2$varresult$UKIP.Articles,
          title="Vector Autoregressions, UKIP Media Coverage and Public Support",
          font.size="small",
          column.sep.width = "0pt",
          omit.stat=c("ser", "f"),
          initial.zero=F,
          column.labels=c("$\\Delta Support$", "$\\Delta Articles$",
                          "$\\Delta Support$", "$\\Delta Articles$"),
          dep.var.labels=c(""),
          covariate.labels=c("$\\Delta Articles_{t-1}$",
                             "$\\Delta Support_{t-1}$",
                             "$\\Delta Articles_{t-2}$",
                             "$\\Delta Support_{t-2}$",
                             "$\\Delta Articles_{t-3}$",
                             "$\\Delta Support_{t-3}$",
                             "Constant",
                             "Trend",
                             "General Elections",
                             "EU Elections",
                             "$\\Delta Unemployment_{t-1}$",
                             "$\\Delta Immigration_{t-1}$"),
          header=FALSE)
```

# Additional Analysis 1 

The researchers from their analysis found two time intervals where the increases in media coverage were followed by some historically crucial increases in public support for UKIP. The two time intervals are the middle of 2012 (from July to September) and the second half of 2014 (from August to November). The paper concludes that the media coverages drive public support. 
My first analysis is based on these two interesting time intervals. I want to know how the p-value might change if deleting the data for these two intervals. If these two intervals are outliers that significantly increase the media coverage's influence on public support, deleting them might show new causality between media coverage and public support. 

```{r, analysis one}
# removing two intervals from data set
modelvars1<-unscaled[-c(103:107,128:132),]
# doing the same analysis
modelvars1$UKIP.Articles.d<-diff(zoo(scale(modelvars1$UKIP.Articles)), na.pad=T)
modelvars1$UKIP.Articles.d.l<-lag(modelvars1$UKIP.Articles.d, k=-1, na.pad=T)
modelvars1$UKIP.Articles.d<-as.numeric(modelvars1$UKIP.Articles.d)
modelvars1$UKIP.Articles.d.l<-as.numeric(modelvars1$UKIP.Articles.d.l)
modelvars1$UKIP.Vote.d<-diff(zoo(scale(modelvars1$UKIP.Vote)), na.pad=T)
modelvars1$UKIP.Vote.d.l<-lag(modelvars1$UKIP.Vote.d,k=-1, na.pad=T)
modelvars1$UKIP.Vote.d<-as.numeric(modelvars1$UKIP.Vote.d)
modelvars1$UKIP.Vote.d.l<-as.numeric(modelvars1$UKIP.Vote.d.l)

modelvars1$UKIP.Articles.notlog<-modelvars1$UKIP.Articles
modelvars1$UKIP.Vote.notlog<-modelvars1$UKIP.Vote

modelvars1$UKIP.Articles<-log(modelvars1$UKIP.Articles)
modelvars1$Leader.Articles<-log(modelvars1$Leader.Articles+1)
modelvars1$Farage.Articles<-log(modelvars1$Farage.Articles+1)
modelvars1$UKIP.Vote<-log(modelvars1$UKIP.Vote)

diffvars<-data.frame()  # difference variables and lag dates/elections to match
diffvars<-as.data.frame(cbind(modelvars1$Date[2:length(modelvars1$Date)],
                              diff(modelvars1$UKIP.Vote),
                              diff(modelvars1$UKIP.Articles),
                              diff(modelvars1$Unemployment),
                              diff(modelvars1$Salience.Imm),
                              modelvars1[2:length(modelvars1$Date), 6:16]))
names(diffvars)<-c("Date", "UKIP.Vote", "UKIP.Articles","Unemployment",
                   "Immigration", "General.Elections", "EU.Elections",
                   "EU.Ref", "Worker.Rights", "Cameron", "Leader.Articles",
                   "Farage.Articles", "European.Share.Step", "European.Share.Pulse",
                   "National.Share.Step", "National.Share.Pulse")
row.names(diffvars)<-NULL

diffvars.unemp <-diffvars[complete.cases(diffvars),]
varmodel1<-VAR(diffvars.unemp[c(3,2)],
              lag.max=10,
              ic="AIC",
              type="both",
              exogen=diffvars.unemp[c(6:7,4:5)])

gc.articles<-causality(varmodel1,
                       cause="UKIP.Vote",
                       vcov.=vcovHC(varmodel1, type="HC"))

gc.vote<-causality(varmodel1,
                   cause="UKIP.Articles",
                   vcov.=vcovHC(varmodel1, type="HC"))

gc.articles.result<-as.data.frame(unlist(gc.articles$Granger))
gc.vote.result<-as.data.frame(unlist(gc.vote$Granger))

results1<-merge(gc.vote.result,
                gc.articles.result,
                by="row.names")
results1

#0.0389869862339822 compare to 	0.0557604875584676
#very significant
```

# Additional Analysis 2

From multiple reliable sources, UKIP reached its greatest level of success in the mid of 2010 where it gained two members of Parliament. My second analysis is to figure out the role of media coverage in the downgrade of public support since then and check whether the causality between media coverage and public support has changed. I am focusing on the data between the second half of 2010 to the latest data. 

```{r analysis two}
modelvars2<-unscaled[(79:160),]

modelvars2$UKIP.Articles.d<-diff(zoo(scale(modelvars2$UKIP.Articles)), na.pad=T)
modelvars2$UKIP.Articles.d.l<-lag(modelvars2$UKIP.Articles.d, k=-1, na.pad=T)
modelvars2$UKIP.Articles.d<-as.numeric(modelvars2$UKIP.Articles.d)
modelvars2$UKIP.Articles.d.l<-as.numeric(modelvars2$UKIP.Articles.d.l)
modelvars2$UKIP.Vote.d<-diff(zoo(scale(modelvars2$UKIP.Vote)), na.pad=T)
modelvars2$UKIP.Vote.d.l<-lag(modelvars2$UKIP.Vote.d,k=-1, na.pad=T)
modelvars2$UKIP.Vote.d<-as.numeric(modelvars2$UKIP.Vote.d)
modelvars2$UKIP.Vote.d.l<-as.numeric(modelvars2$UKIP.Vote.d.l)

modelvars2$UKIP.Articles.notlog<-modelvars2$UKIP.Articles
modelvars2$UKIP.Vote.notlog<-modelvars2$UKIP.Vote

modelvars2$UKIP.Articles<-log(modelvars2$UKIP.Articles)
modelvars2$Leader.Articles<-log(modelvars2$Leader.Articles+1)
modelvars2$Farage.Articles<-log(modelvars2$Farage.Articles+1)
modelvars2$UKIP.Vote<-log(modelvars2$UKIP.Vote)

diffvars<-data.frame()  # difference variables and lag dates/elections to match
diffvars<-as.data.frame(cbind(modelvars2$Date[2:length(modelvars2$Date)],
                              diff(modelvars2$UKIP.Vote),
                              diff(modelvars2$UKIP.Articles),
                              diff(modelvars2$Unemployment),
                              diff(modelvars2$Salience.Imm),
                              modelvars2[2:length(modelvars2$Date), 6:16]))
names(diffvars)<-c("Date", "UKIP.Vote", "UKIP.Articles","Unemployment",
                   "Immigration", "General.Elections", "EU.Elections",
                   "EU.Ref", "Worker.Rights", "Cameron", "Leader.Articles",
                   "Farage.Articles", "European.Share.Step", "European.Share.Pulse",
                   "National.Share.Step", "National.Share.Pulse")
row.names(diffvars)<-NULL

diffvars.unemp <-diffvars[complete.cases(diffvars),]
varmodel2<-VAR(diffvars.unemp[c(3,2)],
              lag.max=10,
              ic="AIC",
              type="both",
              exogen=diffvars.unemp[c(6:7,4:5)])

gc.articles2<-causality(varmodel2,
                        cause="UKIP.Vote",
                        vcov.=vcovHC(varmodel2, type="HC"))

gc.vote2<-causality(varmodel2,
                    cause="UKIP.Articles",
                    vcov.=vcovHC(varmodel2, type="HC"))

gc.articles.result2<-as.data.frame(unlist(gc.articles2$Granger))
gc.vote.result2<-as.data.frame(unlist(gc.vote2$Granger))

results2<-merge(gc.vote.result2,
                gc.articles.result2,
                by="row.names")
results2
# before 2010
sum(unscaled$UKIP.Vote[1:78])/sum(unscaled$UKIP.Articles[1:78])
# after 2010
sum(unscaled$UKIP.Vote[79:160])/sum(unscaled$UKIP.Articles[79:160])
#The result still proves that media converage drive support. As more articles published, the lower the support. There might need more analysis to answer why. 
```

# Additional Analysis 3

In the dataset, media coverage can be separated into two subgroups: the number of articles and the number of leaders' articles. From the researchers' analysis and my second analysis, increased numbers of articles drive public support for UKIP. In addition, I want to be more specific in knowing the relationship between the leader's articles and public support. 

```{r analysis three}
modelvars3<-unscaled[c(1:78),]

modelvars3$UKIP.Articles.d<-diff(zoo(scale(modelvars3$UKIP.Articles)), na.pad=T)
modelvars3$UKIP.Articles.d.l<-lag(modelvars3$UKIP.Articles.d, k=-1, na.pad=T)
modelvars3$UKIP.Articles.d<-as.numeric(modelvars3$UKIP.Articles.d)
modelvars3$UKIP.Articles.d.l<-as.numeric(modelvars3$UKIP.Articles.d.l)
modelvars3$UKIP.Vote.d<-diff(zoo(scale(modelvars3$UKIP.Vote)), na.pad=T)
modelvars3$UKIP.Vote.d.l<-lag(modelvars3$UKIP.Vote.d,k=-1, na.pad=T)
modelvars3$UKIP.Vote.d<-as.numeric(modelvars3$UKIP.Vote.d)
modelvars3$UKIP.Vote.d.l<-as.numeric(modelvars3$UKIP.Vote.d.l)

modelvars3$UKIP.Articles.notlog<-modelvars3$UKIP.Articles
modelvars3$UKIP.Vote.notlog<-modelvars3$UKIP.Vote

modelvars3$UKIP.Articles<-log(modelvars3$UKIP.Articles)
modelvars3$Leader.Articles<-log(modelvars3$Leader.Articles+1)
modelvars3$Farage.Articles<-log(modelvars3$Farage.Articles+1)
modelvars3$UKIP.Vote<-log(modelvars3$UKIP.Vote)

diffvars_3<-data.frame()  # difference variables and lag dates/elections to match
diffvars_3<-as.data.frame(cbind(modelvars3$Date[2:length(modelvars3$Date)],
                              diff(modelvars3$UKIP.Vote),
                              diff(modelvars3$Leader.Articles),
                              diff(modelvars3$Farage.Articles),
                              diff(modelvars3$Unemployment),
                              diff(modelvars3$Salience.Imm),
                              modelvars3[2:length(modelvars3$Date), 6:16]))
names(diffvars_3)<-c("Date", "UKIP.Vote", "Leader.Articles", "Farage.Articles","Unemployment",
                   "Immigration", "General.Elections", "EU.Elections", "EU.Ref",
                   "Worker.Rights", "Cameron", "Leader", "Farage", "European.Share.Step",
                   "European.Share.Pulse", "National.Share.Step", "National.Share.Pulse")
row.names(diffvars_3)<-NULL


diffvars.unemp2 <- diffvars_3[complete.cases(diffvars_3),]

varmodel3<-VAR(diffvars.unemp2[c(3,2)],
                     lag.max=10,
                     ic="AIC",
                     type="both",
                     exogen=diffvars.unemp2[c(14,16, 5:6, 4)])

gc.articles3<-causality(varmodel3,
                        cause="UKIP.Vote",
                        vcov.=vcovHC(varmodel3, type="HC"))

gc.vote3<-causality(varmodel3,
                    cause="Leader.Articles",
                    vcov.=vcovHC(varmodel3, type="HC"))

gc.articles.result3<-as.data.frame(unlist(gc.articles3$Granger))
gc.vote.result3<-as.data.frame(unlist(gc.vote3$Granger))

results3<-merge(gc.vote.result3,
                gc.articles.result3,
                by="row.names")
results3
```


# Additional Analysis 4: Time-series analysis and Forecasting

```{r libraries, echo=FALSE}
library("forecast")
library("tseries")

data <- read.csv("https://osf.io/f5yg4/Download")
dt1 <- mdy(data$Date)
time <- 1: length(dt1)
status <- data$UKIP.Articles + 1 
log.status <- log(status)
diff.log.status <- c(NA, diff(log.status))
diff2.log.status <- c(NA, diff(diff.log.status))

```

### Plotting timeseries of UKIP Articles
```{r fig-2, fig.width=12, fig.height=8}
par(mfrow=c(2,2))
plot(dt1, status, type="l",
     xlab="Date", ylab="UKIP Articles")
plot(dt1, log.status, type="l",
     xlab="Date", ylab="UKIP Articles")
plot(dt1, diff.log.status, type="l",
     xlab="Date", ylab="Differenced Log UKIP Articles")
plot(dt1, diff2.log.status, type="l",
     xlab="Date", ylab="Differenced2 Log UKIP Articles")
```
### Figures 2b, 3a, 3b. Timeseries of Log UKIP Articles and ACF/PACF
```{r fig.width=12, fig.height=8}
# Time series plot
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(dt1, log.status, type="l",
     xlab="Date", ylab="UKIP Articles")
# ACF and PACF
Acf(log.status, na.action = na.pass)
log.status.acf <- Acf(log.status, na.action = na.pass, plot = FALSE)
pacf(log.status, na.action = na.pass)
log.status.acf.table <- data.frame(log.status.acf$lag,  log.status.acf$acf)[-1,]
```

### Figures 2c, 3c, 3d. Timeseries of Differenced Log UKIP Article and ACF/PACF
```{r fig.width=12, fig.height=8}
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
# Time series plot
plot(dt1, diff.log.status, type="l",
     xlab="Date", ylab="Differenced Log UKIP Article")
# ACF and PACF
Acf(diff.log.status, na.action = na.pass)
pacf(diff.log.status, na.action = na.pass)

```

### Figure 4. Computing the AICc values for ARIMA Candidate Models
```{r }
d <- 1
# choose p, q with AICc
for (include.constant in c(FALSE, TRUE)) {
    for (p in 0:4) {
        for (q in 0:4) {
            # work-around bug in R by manually differencing
            fit <- Arima(diff(log.status), c(p,0,q),
                         include.constant=include.constant, method="ML")
             cat("ARIMA",
                "(", p, ",", d, ",", q, ")",
                "(constant=", include.constant, ")",
                " : ", fit$aicc, "\n", sep="")
             #cat( p, ":", d, ":", q, ":",
            #     ":", include.constant, "",
             #    " : ", fit$aicc, "\n", sep="")
        }
    }
}
```

### Figure 5. Final Estimates of ARIMA Parameters

Here is code to fit the ARIMA model, then compute residuals and the fitted values:

```{r}
fit.mean <- Arima(log.status, c(1, 1, 3), include.constant=FALSE)
summary(fit.mean)
```

### Figure 6. ARIMA Residual Plots

Here are the residuals, with the last 10 residuals printed out:
```{r}
resid <- residuals(fit.mean)
tail(resid, n=10)
```


Here are the fitted values, with the last 10 fitted values printed out:
```{r}
f <- fitted.values(fit.mean)
tail(f, n=10)
```

### Figure 7. Forecast Intervals for ARIMA Model
Here is the one step ahead forecast and 95% forecast interval:

```{r}
forecast(fit.mean, h=1, level = 95)
```

Here is a plot of the residuals:

```{r fig-7a}
plot(dt1, resid, type="l",
     xlab="Date", ylab="UKIP Articles Residuals")
```

Here are the ACF and PACF of the residuals:

```{r fig.width=12, fig.height=6}
# ACF and PACF
par(mfrow=c(1,2))
Acf(resid, na.action = na.pass)
Pacf(resid, na.action = na.pass)
```

Here are the ACF and PACF of the squared-residuals:

```{r fig.width=12, fig.height=6}
# Add ACF, PACF of squared residuals.
par(mfrow=c(1,2))
Acf(resid^2, na.action = na.pass)
Pacf(resid^2, na.action = na.pass)
```

We found the lowest AICc by fitting ARIMA(1, 1, 3). Finally we used these parameters to do the forecasting and related analysis. Overall, the analysis reinforced what the original researchers found.


# Additional Analysis 5: Robust regression

```{r robust regression, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE, echo=FALSE}
data2 <- data
ggplot(data2, aes(x = UKIP.Vote, y = UKIP.Articles, na.rm = TRUE)) + 
  geom_point() + stat_smooth(method = "lm", col = "red") + theme_minimal() +
  ggtitle("UKIP.Articles vs. UKIP.Vote")
```

```{r}
install.packages("olsrr")
library(olsrr)
fitLS <- lm(UKIP.Articles ~ UKIP.Vote, data = data2)
ols_plot_cooksd_bar(fitLS)
```

```{r}
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(fitLS, las = 1)

```



```{r}
# Huber loss
fitH <- rlm(UKIP.Articles ~ UKIP.Vote, data = df) 
coef(fitH)

```

```{r}
hweights <- data.frame(articles = df$Date, resid = fitH$resid, weight = fitH$w)
hweights2 <- hweights[order(fitH$w), ]
hweights2[1:15, ]


```

Last but not least, our p value is 0.0054, which is significant enough to reject our null hypothesis. Overall, the number of outliers exceeds what we initially expected and our p value rejects the null hypothesis because the original model is not robust enough. 


renv::snapshot(prompt = FALSE)
