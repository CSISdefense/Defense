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
## Loading required package: knitr
## Loading required package: scales
## Loading required package: reshape2
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


SiliconTopVendor$HPandOther<-"Other Silicon Valley Major Vendors"
SiliconTopVendor$HPandOther[SiliconTopVendor$ParentID =="HEWLETT PACKARD"]<-"HEWLETT PACKARD"
SiliconTopVendor$HPandOther<-ordered(SiliconTopVendor$HPandOther,
                                     c("Other Silicon Valley Major Vendors","HEWLETT PACKARD"))

SummaryKable(SiliconTopVendor,NULL,"Summary")
```



Table: Summary

  Total     Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change 
-------  ------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------
 18.045   2.308      2011          0.308          0.427          1.254          1.721          1.936  37.3%               12.4%        

```r
SummaryKable(SiliconTopVendor,"ParentID","Vendor")
```



Table: Vendor

     ParentID                           Total     Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  -------------------------------  -------  ------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
9    HEWLETT PACKARD                   13.041   2.059      2011          0.171          0.161          1.037          1.489          1.750  43.7%               17.5%          72.3%   
17   ORACLE                             2.475   0.204      2000          0.045          0.148          0.123          0.145          0.079  17.8%               -45.6%         13.7%   
2    AGILENT TECHNOLOGIES               0.899   0.096      2007          0.000          0.073          0.050          0.052          0.031  4.6%                -40.9%         5.0%    
25   VARIAN ASSOCIATES                  0.581   0.173      1990          0.056          0.002          0.002          0.000          0.000  -84.6%              -100.0%        3.2%    
21   STANFORD UNIVERSITY                0.394   0.043      2005          0.017          0.016          0.010          0.016          0.013  61.5%               -17.3%         2.2%    
26   VARIAN MEDICAL SYSTEMS             0.129   0.017      2002          0.000          0.010          0.008          0.005          0.008  -42.3%              82.8%          0.7%    
6    CISCO SYSTEMS                      0.126   0.097      2014          0.002          0.001          0.000          0.001          0.049  5606.7%             3426.1%        0.7%    
10   INTEL                              0.115   0.028      1993          0.011          0.000          0.000          0.000          0.000  1076.5%             -57.3%         0.6%    
16   NETWORK APPLIANCE                  0.091   0.021      2004          0.002          0.009          0.000          0.000          0.000  -71.5%              198.6%         0.5%    
24   SYNNEX                             0.058   0.011      2010          0.000          0.002          0.006          0.008          0.005  28.0%               -42.7%         0.3%    
23   Symantec                           0.048   0.011      2008          0.000          0.002          0.009          0.003          0.000  -59.1%              -98.7%         0.3%    
5    APPLIED MATERIALS                  0.029   0.010      1996          0.002          0.001          0.000          0.000          0.000  Inf%                -100.0%        0.2%    
28   VMWARE                             0.017   0.012      2009          0.000          0.000          0.006          0.001          0.001  -91.2%              64.4%          0.1%    
4    APPLE COMPUTER                     0.015   0.003      2008          0.000          0.001          0.003          0.001          0.000  -76.1%              -66.5%         0.1%    
15   LSI                                0.007   0.006      1991          0.001          0.000          0.000          0.000          0.000  211.5%              -100.0%        0.0%    
13   KLA TENCOR                         0.005   0.003      2005          0.000          0.000          0.000          0.000          0.000  13.2%               -1.7%          0.0%    
7    GILEAD SCIENCES                    0.004   0.002      1992          0.000          0.000          0.000          0.000          0.000  23.4%               -100.0%        0.0%    
22   SUNPOWER                           0.004   0.002      2005          0.000          0.000          0.000          0.000          0.000  Inf%                365.7%         0.0%    
1    ADOBE                              0.002   0.001      2002          0.000          0.000          0.000          0.000          0.000  Inf%                -100.0%        0.0%    
11   INTUIT                             0.001   0.000      2010          0.000          0.000          0.000          0.000          0.000  Inf%                2.3%           0.0%    
19   SANDISK                            0.001   0.001      1996          0.000          0.000          0.000          0.000          0.000  NaN%                NaN%           0.0%    
29   YAHOO!                             0.001   0.001      2004          0.000          0.000          0.000          0.000          0.000  NaN%                NaN%           0.0%    
27   VARIAN SEMICONDUCTOR EQUIPMENT     0.001   0.000      2001          0.000          0.000          0.000          0.000          0.000  NaN%                NaN%           0.0%    
8    GOOGLE                             0.000   0.000      2006          0.000          0.000          0.000          0.000          0.000  -85.2%              -64.3%         0.0%    
18   SALESFORCE COM                     0.000   0.000      2012          0.000          0.000          0.000          0.000          0.000  Inf%                45.7%          0.0%    
20   SANMINA                            0.000   0.000      2005          0.000          0.000          0.000          0.000          0.000  NaN%                NaN%           0.0%    
14   LAM RESEARCH                       0.000   0.000      1994          0.000          0.000          0.000          0.000          0.000  NaN%                NaN%           0.0%    
3    AMD                                0.000   0.000      2005          0.000          0.000          0.000          0.000          0.000  -100.0%             NaN%           0.0%    
12   JUNIPER NETWORKS                   0.000   0.000      2012          0.000          0.000          0.000          0.000          0.000  NaN%                NaN%           0.0%    

```r
SummaryKable(SiliconTopVendor,"ParentConsolidated","Vendor")
```



Table: Vendor

     ParentConsolidated                     Total     Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  -----------------------------------  -------  ------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
1    HEWLETT PACKARD                       13.041   2.059      2011          0.171          0.161          1.037          1.489          1.750  43.7%               17.5%          72.3%   
2    ORACLE                                 2.475   0.204      2000          0.045          0.148          0.123          0.145          0.079  17.8%               -45.6%         13.7%   
3    AGILENT TECHNOLOGIES                   0.899   0.096      2007          0.000          0.073          0.050          0.052          0.031  4.6%                -40.9%         5.0%    
4    Varian Associates & Successors         0.711   0.173      1990          0.056          0.012          0.010          0.005          0.008  -52.3%              68.8%          3.9%    
6    Other Major Silicon Valley Vendors     0.525   0.101      2014          0.019          0.016          0.024          0.015          0.055  -39.2%              272.0%         2.9%    
5    STANFORD UNIVERSITY                    0.394   0.043      2005          0.017          0.016          0.010          0.016          0.013  61.5%               -17.3%         2.2%    

```r
SummaryKable(SiliconTopVendor,"HPandOther","HP and Other")
```



Table: HP and Other

     HPandOther                             Total     Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  -----------------------------------  -------  ------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
2    HEWLETT PACKARD                       13.041   2.059      2011          0.171          0.161          1.037          1.489          1.750  43.7%               17.5%          72.3%   
1    Other Silicon Valley Major Vendors     5.003   0.311      2000          0.137          0.266          0.217          0.232          0.186  7.0%                -19.9%         27.7%   


```r
SiliconTopVendor<-ddply(SiliconTopVendor,
                    .(ParentID,Fiscal.Year),
                    transform,
                    Annual.Obligation.2014=sum(Obligation.2014,na.rm=TRUE)
    )

SiliconTopVendor<-ddply(SiliconTopVendor,
                        .(ParentID),
                        transform,
                        ParentPeak=ifelse(max(Annual.Obligation.2014,na.rm=TRUE)>=0.01,
                                                  as.character(ParentID),"Other Major Silicon Valley Vendors")
)


ParentOrderDF<-ddply(SiliconTopVendor,
                     .(ParentPeak),
                     summarise,
                     Max.Obligation.2014=max(Annual.Obligation.2014,na.rm=TRUE)
)
ParentOrderDF<-ParentOrderDF[order(-ParentOrderDF$Max.Obligation.2014),]

ParentOrderList<-ParentOrderDF$ParentPeak

SiliconTopVendor$ParentPeak<-ordered(SiliconTopVendor$ParentPeak,ParentOrderList)


# SiliconTopVendorAnnual$Obligation.2014<-SiliconTopVendorAnnual$Obligation.2014*1000000000

ggplot(data = subset(arrange(SiliconTopVendor,SubCustomer.sum),
                     ParentID!="HEWLETT PACKARD"),
       aes(x=Fiscal.Year,
           y=Obligation.2014,
           fill=SubCustomer.sum
       )
)+ 
    geom_bar(stat="identity") + 
    facet_grid(  ParentPeak ~.,
                    scales="free_y", #The scales actually do stay fixed
                     space="free_y")+#But only because the space is free)
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
    scale_y_continuous("Obligations (2014 Billions Dollars)",labels=comma)+
    theme(legend.position="bottom")+theme(strip.text.y=element_text(size=axis.text.size,family="times",face="bold",angle=0))+
    geom_hline(y=0.03, color="blue")+geom_hline(y=0.05, color="red")
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (position_stack).
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

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
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

```
## Warning in loop_apply(n, do.ply): Stacking not well defined when ymin != 0
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

```
## Warning in grid.Call(L_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
## font family not found in Windows font database
```

```
## Warning in grid.Call.graphics(L_text, as.graphicsAnnot(x$label), x$x, x$y,
## : font family not found in Windows font database
```

![](silicon_valley_graphs_files/figure-html/VendorPeaks-1.png) 

```r
#      geom_rect(ymin = 0.02, ymax = 0.05, 
#               xmin = -Inf, xmax = Inf, 
#               fill = 'blue',
#               aes(alpha=0.25)



SummaryKable(SiliconTopVendor,"ParentPeak","Vendor",3)
```



Table: Vendor

     ParentPeak                             Total     Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  -----------------------------------  -------  ------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
1    HEWLETT PACKARD                       13.041   2.059      2011          0.171          0.161          1.037          1.489          1.750  43.7%               17.5%          72.3%   
2    ORACLE                                 2.475   0.204      2000          0.045          0.148          0.123          0.145          0.079  17.8%               -45.6%         13.7%   
5    AGILENT TECHNOLOGIES                   0.899   0.096      2007          0.000          0.073          0.050          0.052          0.031  4.6%                -40.9%         5.0%    
3    VARIAN ASSOCIATES                      0.581   0.173      1990          0.056          0.002          0.002          0.000          0.000  -84.6%              -100.0%        3.2%    
6    STANFORD UNIVERSITY                    0.394   0.043      2005          0.017          0.016          0.010          0.016          0.013  61.5%               -17.3%         2.2%    
9    VARIAN MEDICAL SYSTEMS                 0.129   0.017      2002          0.000          0.010          0.008          0.005          0.008  -42.3%              82.8%          0.7%    
4    CISCO SYSTEMS                          0.126   0.097      2014          0.002          0.001          0.000          0.001          0.049  5606.7%             3426.1%        0.7%    
7    INTEL                                  0.115   0.028      1993          0.011          0.000          0.000          0.000          0.000  1076.5%             -57.3%         0.6%    
8    NETWORK APPLIANCE                      0.091   0.021      2004          0.002          0.009          0.000          0.000          0.000  -71.5%              198.6%         0.5%    
13   Other Major Silicon Valley Vendors     0.070   0.011      1996          0.004          0.003          0.003          0.001          0.001  -58.4%              -57.7%         0.4%    
12   SYNNEX                                 0.058   0.011      2010          0.000          0.002          0.006          0.008          0.005  28.0%               -42.7%         0.3%    
11   Symantec                               0.048   0.011      2008          0.000          0.002          0.009          0.003          0.000  -59.1%              -98.7%         0.3%    
10   VMWARE                                 0.017   0.012      2009          0.000          0.000          0.006          0.001          0.001  -91.2%              64.4%          0.1%    


```r
SiliconTopVendor<-
    ddply(SiliconTopVendor,
          .(PlatformPortfolio),
          transform,
          PlatformPortfolioSC=ifelse(sum(Obligation.2014,na.rm=TRUE)>=0.25,
                                     as.character(PlatformPortfolio),"Remaining Platform Categories")
    )


SummaryKable(SiliconTopVendor,"PlatformPortfolio","Platform")
```



Table: Platform

     PlatformPortfolio                  Total     Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  -------------------------------  -------  ------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
2    Electronics and Communications    15.549   2.197      2011          0.201          0.341          1.143          1.605          1.857  40.4%               15.7%          86.2%   
7    Other R&D and Knowledge Based      0.929   0.103      2009          0.029          0.027          0.068          0.069          0.040  1.3%                -42.7%         5.1%    
3    Facilities and Construction        0.859   0.058      1995          0.044          0.030          0.029          0.024          0.023  -18.8%              -4.6%          4.8%    
5    Missile and Space Systems          0.262   0.035      2007          0.012          0.013          0.000          0.013          0.002  4892.3%             -84.1%         1.5%    
6    Other Products                     0.211   0.034      1999          0.012          0.008          0.006          0.003          0.006  -59.2%              140.7%         1.2%    
1    Aircraft and Drones                0.080   0.010      1990          0.005          0.003          0.002          0.001          0.001  -48.9%              36.3%          0.4%    
8    Other Services                     0.060   0.010      1999          0.002          0.004          0.002          0.001          0.001  -39.7%              -9.0%          0.3%    
9    Ships & Submarines                 0.051   0.008      2012          0.001          0.001          0.002          0.005          0.006  201.0%              15.9%          0.3%    
10   Weapons and Ammunition             0.042   0.015      1993          0.004          0.000          0.000          0.000          0.000  -84.1%              -2.3%          0.2%    
4    Land Vehicles                      0.002   0.001      1999          0.000          0.000          0.000          0.000          0.000  -91.6%              -50.8%         0.0%    
11   Unlabeled                          0.000   0.000      2003          0.000          0.000          0.000          0.000          0.000  NaN%                NaN%           0.0%    

```r
ggplot(data = arrange(SiliconTopVendor,PlatformPortfolioSC),
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
    scale_y_continuous("Obligations (2014 Dollars Billions)",labels=comma)+
    theme(legend.position="bottom")
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
SummaryKable(SiliconTopVendor,"SubCustomer.sum","Defense Component")
```



Table: Defense Component

     SubCustomer.sum     Total     Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  ----------------  -------  ------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
3    Navy               10.126   1.686      2011          0.129          0.129          0.736          1.122          1.480  52.6%               31.9%          56.1%   
4    Other DoD           3.110   0.318      2011          0.051          0.102          0.197          0.292          0.253  48.5%               -13.4%         17.2%   
2    Army                2.764   0.223      2010          0.060          0.127          0.178          0.187          0.114  5.1%                -39.1%         15.3%   
1    Air Force           2.045   0.151      2010          0.067          0.069          0.144          0.120          0.088  -16.4%              -26.4%         11.3%   

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
    scale_y_continuous("Obligations (2014 Dollars Billions)",labels=comma)+
    theme(legend.position="bottom")
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
ggplot(data = arrange(SiliconTopVendor,HPandOther),
       aes(x=Fiscal.Year,
           y=Obligation.2014,
           fill=HPandOther
       )
)+ 
    geom_bar(stat="identity") + 
    facet_wrap( ~ SubCustomer.sum)+
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
    )+
    theme(axis.text.x=element_text(angle = 90))+
    scale_y_continuous("Obligations (2014 Dollars Billions)",labels=comma)+
    theme(legend.position="bottom")
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

![](silicon_valley_graphs_files/figure-html/SubCustomer-2.png) 




```r
DefenseContract  <- read.csv(
    paste("data\\Defense_Location_SP_SiliconValleyCompetitionVendorSizeHistoryBucketPlatformSubCustomer.csv", sep = ""),
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
)

#These will probably be moved into apply_lookups at some point
DefenseContract<-apply_lookups(Path,DefenseContract)
```

```
## Joining by: Customer, SubCustomer
## Joining by: ProductOrServiceArea
## Joining by: PlatformPortfolio
## Joining by: Vendor.Size
## Joining by: CompetitionClassification, ClassifyNumberOfOffers
## Joining by: Fiscal.Year
```

```
## Warning in apply_lookups(Path, DefenseContract): NaNs produced
```

```r
SiliconContract<-subset(DefenseContract,Customer=="Defense" & IsSiliconValley==1)

SummaryKable(DefenseContract,"IsSiliconValley","Summary")
```



Table: Summary

      IsSiliconValley      Total       Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  ----------------  ---------  --------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
2                  NA   6544.625   409.945      2009        184.028        270.324        409.263        377.486        295.383  -7.8%               -21.8%         99.7%   
1                   1     18.045     2.308      2011          0.308          0.427          1.254          1.721          1.936  37.3%               12.4%          0.3%    

