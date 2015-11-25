# DoD Fixed-Price Study: Contract Duration Classification
Greg Sanders  
Tuesday, January 13, 2015  


```
## Loading required package: ggplot2
## Loading required package: stringr
## Loading required package: plyr
## Loading required package: Hmisc
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
## 
## Loading required package: lubridate
## 
## Attaching package: 'lubridate'
## 
## The following object is masked from 'package:plyr':
## 
##     here
## 
## Loading required package: scales
## Loading required package: pander
## Loading required package: xtable
## 
## Attaching package: 'xtable'
## 
## The following objects are masked from 'package:Hmisc':
## 
##     label, label<-
```


```r
SiliconTopVendor  <- read.csv(
    paste("data\\Overall_Location_SP_SiliconValleyTopVendorHistoryPlatformSubCustomer.csv", sep = ""),
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
    )

#These will probably be moved into apply_lookups at some point
SiliconTopVendor<-apply_lookups(Path,SiliconTopVendor)
```

```
## Joining by: Customer, SubCustomer
## Joining by: PlatformPortfolio
## Joining by: Fiscal.Year
```

```
## Warning in apply_lookups(Path, SiliconTopVendor): NaNs produced
```

```r
# 
# as.numeric(as.duration(
#     ymd(ContractSample$SignedMonth)-ContractSample$StartFiscalYear)
#     /dyears(1)
#     )

SiliconTopVendor<-subset(SiliconTopVendor,Customer=="Defense")
SiliconTopVendor<-ddply(SiliconTopVendor,
                        .(ParentID),
                        transform,
                        ParentConsolidated=ifelse(sum(Obligation.2014,na.rm=TRUE)>=0.25,
                                                  as.character(ParentID),"Other Major Silicon Valley Vendors")
)

SiliconTopVendor$ParentConsolidated<-as.character(SiliconTopVendor$ParentConsolidated)

SiliconTopVendor$ParentConsolidated[SiliconTopVendor$ParentID %in%
                                        c('VARIAN SEMICONDUCTOR EQUIPMENT','VARIAN ASSOCIATES','VARIAN MEDICAL SYSTEMS')]<-
    "Varian Associates & Successors"

ParentOrderDF<-ddply(SiliconTopVendor,
      .(ParentConsolidated),
      summarise,
      Obligation.2014=sum(Obligation.2014,na.rm=TRUE)
)
ParentOrderDF<-ParentOrderDF[order(-ParentOrderDF$Obligation.2014),]

ParentOrderList<-ParentOrderDF$ParentConsolidated
ParentOrderList<-c(unlist(as.character(ParentOrderList[ParentOrderList!="Other Major Silicon Valley Vendors"])),
                   "Other Major Silicon Valley Vendors")

SiliconTopVendor$ParentConsolidated<-factor(SiliconTopVendor$ParentConsolidated,ParentOrderList)


OverallSummary<-
    ddply(SiliconTopVendor,
                        .(),
                        summarise,
                        TotalObligation=sum(Obligation.2014,na.rm=TRUE),
          Avg.1999.1999=sum(ifelse(year(Fiscal.Year)>=1990 & year(Fiscal.Year)<=1999, Obligation.2014,0),na.rm=TRUE)/10,
          Avg.2000.2009=sum(ifelse(year(Fiscal.Year)>=2000 & year(Fiscal.Year)<=2009, Obligation.2014,0),na.rm=TRUE)/10,
          Avg.2010.2012=sum(ifelse(year(Fiscal.Year)>=2010 & year(Fiscal.Year)<=2012, Obligation.2014,0),na.rm=TRUE)/3,
          Avg.2013.2014=sum(ifelse(year(Fiscal.Year)>=2013 & year(Fiscal.Year)<=2014, Obligation.2014,0),na.rm=TRUE)/2
)
```




```r
SiliconTopVendor<-
    ddply(SiliconTopVendor,
                        .(PlatformPortfolio),
                        transform,
                        PlatformPortfolioSC=ifelse(sum(Obligation.2014,na.rm=TRUE)>=0.25,
                                                  as.character(PlatformPortfolio),"Remaining Platform Categories")
)


PlatformSummary<-
    ddply(SiliconTopVendor,
                        .(PlatformPortfolio),
                        summarise,
                        TotalObligation=sum(Obligation.2014,na.rm=TRUE),
          Avg.1999.1999=sum(ifelse(year(Fiscal.Year)>=1990 & year(Fiscal.Year)<=1999, Obligation.2014,0),na.rm=TRUE)/10,
          Avg.2000.2009=sum(ifelse(year(Fiscal.Year)>=2000 & year(Fiscal.Year)<=2009, Obligation.2014,0),na.rm=TRUE)/10,
          Avg.2010.2012=sum(ifelse(year(Fiscal.Year)>=2010 & year(Fiscal.Year)<=2012, Obligation.2014,0),na.rm=TRUE)/3,
          Avg.2013.2014=sum(ifelse(year(Fiscal.Year)>=2013 & year(Fiscal.Year)<=2014, Obligation.2014,0),na.rm=TRUE)/2
)



ggplot(data = subset(SiliconTopVendor[order(SiliconTopVendor$PlatformPortfolioSC),],
                     Customer=="Defense" & year(Fiscal.Year)<=2014),# subset(ContractSurvival,StartFiscalYear>=2007 & StartFiscalYear<=2013),
       aes(x=Fiscal.Year,
           y=Obligation.2014,
           fill=PlatformPortfolioSC
           )
       )+ 
    geom_bar(stat="identity") + 
    facet_wrap( ~ ParentConsolidated)+
#                 scales="free_y", #The scales actually do stay fixed
#                 , space="free_y"#But only because the space is free)+
    scale_x_date("Fiscal Year",
                 labels=date_format("'%y"),
                 # breaks="2 years",
                 minor_breaks="1 year",
                 breaks=c(as.Date("1990-01-01"),
                          as.Date("1992-01-01"),
                          as.Date("1994-01-01"),
                          as.Date("1996-01-01"),
                          as.Date("1998-01-01"),
                          as.Date("2000-01-01"),
                          as.Date("2002-01-01"),
                          as.Date("2004-01-01"),
                          as.Date("2006-01-01"),
                          as.Date("2008-01-01"),
                          as.Date("2010-01-01"),
                          as.Date("2012-01-01"),
                          as.Date("2014-01-01"))
                 # breaks=date_breaks("year")
                 # minor_breaks = "1 year"
                 # breaks=date_breaks("year"),
                 # breaks=c(as.Date("1990-01-01"),as.Date("2014-12-31"))
                 )+
    theme(axis.text.x=element_text(angle = 90))+
    scale_y_continuous("Obligations (2014 Dollars Billions)",labels=comma)
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

![](silicon_valley_graphs_files/figure-html/PlatformPortfolio-1.png) 



```r
SubCustomerSummary<-
    ddply(SiliconTopVendor,
                        .(SubCustomer.sum),
                        summarise,
                        TotalObligation=sum(Obligation.2014,na.rm=TRUE),
          Avg.1990.1999=sum(ifelse(year(Fiscal.Year)>=1990 & year(Fiscal.Year)<=1999, Obligation.2014,0),na.rm=TRUE)/10,
          Avg.2000.2009=sum(ifelse(year(Fiscal.Year)>=2000 & year(Fiscal.Year)<=2009, Obligation.2014,0),na.rm=TRUE)/10,
          Avg.2010.2012=sum(ifelse(year(Fiscal.Year)>=2010 & year(Fiscal.Year)<=2012, Obligation.2014,0),na.rm=TRUE)/3,
          Avg.2013.2014=sum(ifelse(year(Fiscal.Year)>=2013 & year(Fiscal.Year)<=2014, Obligation.2014,0),na.rm=TRUE)/2
)
SubCustomerSummary$BCAavgChange<-SubCustomerSummary$Avg.2013.2014/SubCustomerSummary$Avg.2010.2012
SubCustomerSummary$DrawdownAvgChange<-SubCustomerSummary$Avg.2010.2012/SubCustomerSummary$Avg.2000.2009
SubCustomerSummary$CenturyAvgChange<-SubCustomerSummary$Avg.2000.2009/SubCustomerSummary$Avg.1990.1999
require(data.table)
```

```
## Loading required package: data.table
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'data.table'
```

```r
ggplot(data = subset(SiliconTopVendor[order(SiliconTopVendor$SubCustomer.sum),],
                     Customer=="Defense" & year(Fiscal.Year)<=2014),# subset(ContractSurvival,StartFiscalYear>=2007 & StartFiscalYear<=2013),
       aes(x=Fiscal.Year,
           y=Obligation.2014,
           fill=SubCustomer.sum
           )
       )+ 
    geom_bar(stat="identity") + 
    facet_wrap( ~ ParentConsolidated)+
#                 scales="free_y", #The scales actually do stay fixed
#                 , space="free_y"#But only because the space is free)+
    scale_x_date("Fiscal Year",
                 labels=date_format("'%y"),
                 # breaks="2 years",
                 minor_breaks="1 year",
                 breaks=c(as.Date("1990-01-01"),
                          as.Date("1992-01-01"),
                          as.Date("1994-01-01"),
                          as.Date("1996-01-01"),
                          as.Date("1998-01-01"),
                          as.Date("2000-01-01"),
                          as.Date("2002-01-01"),
                          as.Date("2004-01-01"),
                          as.Date("2006-01-01"),
                          as.Date("2008-01-01"),
                          as.Date("2010-01-01"),
                          as.Date("2012-01-01"),
                          as.Date("2014-01-01"))
                 # breaks=date_breaks("year")
                 # minor_breaks = "1 year"
                 # breaks=date_breaks("year"),
                 # breaks=c(as.Date("1990-01-01"),as.Date("2014-12-31"))
                 )+
    theme(axis.text.x=element_text(angle = 90))+
    scale_y_continuous("Obligations (2014 Dollars Billions)",labels=comma)
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (position_stack).
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
```

![](silicon_valley_graphs_files/figure-html/SubCustomer-1.png) 


```r
n <- 100
x <- rnorm(n)
y <- 2*x + rnorm(n)
out <- lm(y ~ x)
library(knitr)
kable(summary(out)$coef, digits=2)
```

               Estimate   Std. Error   t value   Pr(>|t|)
------------  ---------  -----------  --------  ---------
(Intercept)       -0.05         0.11     -0.46       0.65
x                  1.91         0.10     18.66       0.00


```r
n <- 100
x <- rnorm(n)
y <- 2*x + rnorm(n)
out <- lm(y ~ x)
library(pander)
panderOptions("digits", 2)
pander(out)
```


--------------------------------------------------------------
     &nbsp;        Estimate   Std. Error   t value   Pr(>|t|) 
----------------- ---------- ------------ --------- ----------
      **x**           2          0.12        17      8.8e-32  

 **(Intercept)**    -0.061       0.1        -0.59      0.56   
--------------------------------------------------------------

Table: Fitting linear model: y ~ x


```r
n <- 100
x <- rnorm(n)
y <- 2*x + rnorm(n)
out <- lm(y ~ x)
library(xtable)
tab <- xtable(summary(out)$coef, digits=c(0, 2, 2, 1, 2))
print(tab, type="html")
```

<!-- html table generated in R 3.1.3 by xtable 1.8-0 package -->
<!-- Wed Nov 25 13:29:13 2015 -->
<table border=1>
<tr> <th>  </th> <th> Estimate </th> <th> Std. Error </th> <th> t value </th> <th> Pr(&gt;|t|) </th>  </tr>
  <tr> <td align="right"> (Intercept) </td> <td align="right"> -0.02 </td> <td align="right"> 0.11 </td> <td align="right"> -0.2 </td> <td align="right"> 0.84 </td> </tr>
  <tr> <td align="right"> x </td> <td align="right"> 2.11 </td> <td align="right"> 0.11 </td> <td align="right"> 19.0 </td> <td align="right"> 0.00 </td> </tr>
   </table>
