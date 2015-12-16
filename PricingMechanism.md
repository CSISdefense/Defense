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
## Loading required package: splines
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
PricingMechanism  <- read.csv(
    paste("data\\Overall_Summary_SP_FundingMechanismFeeHistoryBucketSubCustomerPlatform.csv", sep = ""),
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
)

#These will probably be moved into apply_lookups at some point
PricingMechanism<-apply_lookups(Path,PricingMechanism)
```

```
## Joining by: Customer, SubCustomer
## Joining by: ProductOrServiceArea
## Joining by: PlatformPortfolio
## Joining by: Pricing.Mechanism
## Joining by: Fiscal.Year
```

```
## Warning in apply_lookups(Path, PricingMechanism): NaNs produced
```

```r
# 
# as.numeric(as.duration(
#     ymd(ContractSample$SignedMonth)-ContractSample$StartFiscalYear)
#     /dyears(1)
#     )

PricingMechanism<-subset(PricingMechanism,Customer=="Defense")



SummaryKable(PricingMechanism,"Pricing.Mechanism","Detail")
```



Table: Detail

     Pricing.Mechanism                                Total       Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  -------------------------------------------  ---------  --------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
7    Firm Fixed Price                              3335.260   216.288      2010         87.471        139.176        212.770        205.476        152.588  -3.4%               -25.7%         50.8%   
4    Cost Plus Fixed Fee                            829.539    61.413      2014         23.147         30.352         40.030         52.784         58.422  31.9%               10.7%          12.6%   
3    Cost Plus Award Fee                            604.976    45.849      2006         16.124         34.918         29.607         26.011         13.575  -12.1%              -47.8%         9.2%    
12   Fixed Price with Economic Price Adjustment     380.390    28.191      2012         10.419         15.860         25.913         22.452         15.067  -13.4%              -32.9%         5.8%    
9    Fixed Price Incentive                          347.324    31.797      2013         15.031          9.300          6.076         17.606         28.820  189.8%              63.7%          5.3%    
5    Cost Plus Incentive                            285.410    27.023      2011          6.979          9.604         19.226         23.371         15.115  21.6%               -35.3%         4.3%    
17   Time and Materials                             197.253    16.672      2008          5.156         10.353         15.525          9.283          1.988  -40.2%              -78.6%         3.0%    
18   Unlabeled                                      149.490    48.588      1990         14.949          0.000          0.000          0.000          0.000  NaN%                NaN%           2.3%    
1    Combination (two or more)                      148.693    47.985      2009          0.000          4.061         41.420          8.990          3.197  -78.3%              -64.4%         2.3%    
2    Cost No Fee                                    128.028    11.258      2009          3.338          5.049          9.475          8.028          5.613  -15.3%              -30.1%         2.0%    
14   Not Reported                                    77.711    11.542      2003          0.000          8.398          5.150          0.076         -0.001  -98.5%              -102.0%        1.2%    
13   Labor Hours                                     28.236     2.806      2010          0.697          1.068          2.073          2.261          0.895  9.1%                -60.4%         0.4%    
8    Fixed Price Award Fee                           22.209     2.410      2003          0.000          1.689          1.744          1.479          0.385  -15.2%              -73.9%         0.3%    
11   Fixed Price Redetermination                     15.478     1.393      2001          0.684          0.711          0.835          0.301          0.189  -64.0%              -37.0%         0.2%    
10   Fixed Price Level of Effort                      6.634     1.449      2013          0.000          0.062          0.464          0.908          1.244  95.8%               37.0%          0.1%    
6    Cost Sharing                                     5.964     1.035      1995          0.342          0.142          0.209          0.182          0.220  -12.9%              20.6%          0.1%    
16   Other (none of the above)                        0.073     0.013      2001          0.000          0.008          0.001          0.002          0.001  21.9%               -58.7%         0.0%    
15   Order Dependent (IDV only)                       0.001     0.001      2007          0.000          0.000          0.000          0.000          0.000  -Inf%               -100.0%        0.0%    

```r
SummaryKable(PricingMechanism,"Base","Base")
```



Table: Base

     Base                    Total       Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  ------------------  ---------  --------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
3    Fixed-Price          4100.661   251.338      2010        113.606        166.737        247.338        247.312        197.049  -0.0%               -20.3%         62.5%   
2    Cost-Plus            1853.918   115.369      2011         49.930         80.064         98.546        110.375         92.946  12.0%               -15.8%         28.2%   
4    Other Cost-Based      232.123    18.639      2008          5.853         11.483         18.061         12.452          4.127  -31.1%              -66.9%         3.5%    
5    Unlabeled             227.201    48.588      1990         14.949          8.398          5.150          0.076         -0.001  -98.5%              -102.0%        3.5%    
1    Combination/Other     148.766    47.986      2009          0.000          4.069         41.421          8.992          3.198  -78.3%              -64.4%         2.3%    

```r
SummaryKable(PricingMechanism,"Fee","Fee")
```



Table: Fee

     Fee             Total       Max   MaxYear   Avg. '90-'99   Avg. '00-'07   Avg. '08-'09   Avg. '10-'12   Avg. '13-'14  Drawdown % Change   BCA % Change   Percent 
---  ----------  ---------  --------  --------  -------------  -------------  -------------  -------------  -------------  ------------------  -------------  --------
4    No Fee       3695.412   242.566      2008         96.661        155.708        240.306        225.955        162.328  -6.0%               -28.2%         56.3%   
2    Fixed         829.539    61.413      2014         23.147         30.352         40.030         52.784         58.422  31.9%               10.7%          12.6%   
5    Other         771.835    79.183      2009         26.053         29.038         73.320         31.820         18.453  -56.6%              -42.0%         11.8%   
3    Incentive     638.698    49.495      2013         22.352         19.046         25.511         41.159         44.155  61.3%               7.3%           9.7%    
1    Award         627.185    48.011      2006         16.124         36.607         31.351         27.489         13.961  -12.3%              -49.2%         9.6%    


```r
PricingMechanismBaseFee<-ddply(PricingMechanism,
                    .(Base,Fee,SubCustomer.detail,SubCustomer.sum,SubCustomer.component),
                    summarise,
                    Annual.Obligation.2014=sum(Obligation.2014,na.rm=TRUE)
    )

 
# LatticePlotWrapper<-function("DoD Component"
#                              ,NULL
#                              ,"Fiscal Year"
#                              ,"Contract Obligations (2014 Billions)"
#                              ,Coloration
#                              ,PricingMechanism
#                              ,VAR.ncol=NA
#                              ,Fiscal.Year
#                              ,Obligation.2014
#                              ,SubCustomer.component
#                              ,Base
#                              ,Fee
# #                              ,MovingAverage=1
# #                              ,MovingSides=1
#                              ,DataLabels=FALSE
#                              #                       ,VAR.override.coloration=NA
# )


ggplot(data = arrange(PricingMechanism,SubCustomer.detail),
       aes(x=Fiscal.Year,
           y=Obligation.2014,
           fill=SubCustomer.detail
       )
)+ 
    geom_bar(stat="identity") + 
    facet_grid(  Base ~ Fee,
                    scales="free_y", #The scales actually do stay fixed
                     space="free_y")+#But only because the space is free)
    scale_x_date("Fiscal Year",
                 labels=date_format("'%y"))
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 10 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 36 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 2 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 6 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 9 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 75 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 22 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

```
## Warning: Removed 41 rows containing missing values (position_stack).
```

```
## Warning: Stacking not well defined when ymin != 0
```

![](PricingMechanism_files/figure-html/SubCustomer-1.png) 

```r
#                  # breaks="2 years",
#                  minor_breaks="1 year",
#                  breaks=c(as.Date("1990-01-01"),
#                           as.Date("1992-01-01"),
#                           as.Date("1994-01-01"),
#                           as.Date("1996-01-01"),
#                           as.Date("1998-01-01"),
#                           as.Date("2000-01-01"),
#                           as.Date("2002-01-01"),
#                           as.Date("2004-01-01"),
#                           as.Date("2006-01-01"),
#                           as.Date("2008-01-01"),
#                           as.Date("2010-01-01"),
#                           as.Date("2012-01-01"),
#                           as.Date("2014-01-01"))
#                  # breaks=date_breaks("year")
#                  # minor_breaks = "1 year"
#                  # breaks=date_breaks("year"),
#                  # breaks=c(as.Date("1990-01-01"),as.Date("2014-12-31"))
#     )+
#     theme(axis.text.x=element_text(angle = 90))+
#     scale_y_continuous("Obligations (2014 Billions Dollars)",labels=comma)+
#     theme(legend.position="bottom")+theme(strip.text.y=element_text(size=axis.text.size,family="times",face="bold",angle=0))+
#     geom_hline(y=0.03, color="blue")+geom_hline(y=0.05, color="red")
# #      geom_rect(ymin = 0.02, ymax = 0.05, 
# #               xmin = -Inf, xmax = Inf, 
# #               fill = 'blue',
# #               aes(alpha=0.25)
# 
# 
# 
# SummaryKable(PricingMechanism,"ParentPeak","Vendor",3)
```
