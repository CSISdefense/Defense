#*************************************Required Libraries******************************************
require(plyr)
require(grid)
require(reshape2)
require(stringr)
require(ggplot2)
# require(logging)
# debug(VariableNumericalFormat)
#*************************************Options*****************************************************
options(error=recover)
options(warn=1)
# basicConfig()
# logdebug("not shown, basic is INFO")
# logwarn("shown and timestamped")

# system("defaults write org.R-project.R force.LANG en_US.UTF-8")
# debug("CreateCSV")

# debug(apply_lookups)
# debug(CreateDuration)



#*************************************Lookup Files*****************************************************
setwd("K:\\Development\\Defense")
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
# Path<-"~\\FPDS\\R scripts and data\\"
# Path<-"C:\\Users\\Greg Sanders\\SkyDrive\\Documents\\R Scripts and Data SkyDrive\\"



source(paste(Path,"helper.r",sep=""))
source(paste(Path,"lookups.r",sep=""))
source(paste(Path,"helper.r",sep=""))
source(paste(Path,"statistics_aggregators.r",sep=""))

options(error=recover)
options(warn=1)

#*************************************
#Place the CSIScontractID list in K:\Development\Defense\data\Contract_Detail_Input.csv

contract.list<-read.csv(
    "K:\\Development\\Defense\\data\\Contract_Detail_Input.csv",
    header=TRUE, sep=",", dec=".", strip.white=TRUE, 
    na.strings=c("NULL","NA"),
    stringsAsFactors=FALSE
)

#data\\defense_contract_SP_ContractSampleCriteriaDetailsCustomer.csv
contract.list <- read_and_join(Path
                                           ,"defense_contract_SP_ContractSampleCriteriaDetailsCustomer.csv"
                                           ,contract.list
                                           ,"data\\"
                               ,"CSIScontractID"
)


#data\\defense_contract_contractdiscretization.csv
contract.list<-read_and_join(Path
                                           ,"defense_contract_contractdiscretization.csv"
                                           ,contract.list
                                           ,"data\\"
                             ,"CSIScontractID"
)


#defense_contract_SP_ContractModificationDeltaCustomer.csv
contract.list<-read_and_join(Path
                                           ,"defense_contract_SP_ContractModificationDeltaCustomer.csv"
                                           ,contract.list
                                           ,"data\\"
                             ,"CSIScontractID"
)

#"data\\Defense_contract_SP_ContractUnmodifiedandOutcomeDetailsCustomer.csv"
contract.list<-read_and_join(Path
                                           ,"Defense_contract_SP_ContractUnmodifiedandOutcomeDetailsCustomer.csv"
                                           ,contract.list
                                           ,"data\\"
                             ,"CSIScontractID"
)


#data\\defense_contract_SP_ContractUnmodifiedCompetitionvehicleCustomer.csv
contract.list<-read_and_join(Path
                                           ,"defense_contract_SP_ContractUnmodifiedCompetitionvehicleCustomer.csv"
                                           ,contract.list
                                           ,"data\\"
                             ,"CSIScontractID"
)
# 
#Use this to add just a single file
# contract.list <-read.csv(
#   paste("data\\defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount.csv",sep=""),
#   header=TRUE, sep=",", dec=".", strip.white=TRUE, 
#   na.strings=c("NULL","NA"),
#   stringsAsFactors=FALSE
# )
# 

# 
# contract.list <-subset(contract.list,select=-c(
#     NumberOfOffersReceived,
# IsFullAndOpen,
# IsSomeCompetition,
# ObligatedAmountIsSomeCompetition,
# IsOnlyOneSource,
# IsFollowonToCompetedAction
# MultipleOrSingleAwardIDC,
# AddMultipleOrSingleAwardIDC,
# AwardOrIDVcontractActionType
# )
# )



#defense_Contract_SP_ContractDetailsR&DCustomer.csv
contract.list<-read_and_join(Path
                                           ,"defense_Contract_SP_ContractDetailsR&DCustomer.csv"
                                           ,contract.list
                                           ,"data\\"
                             ,"CSIScontractID"
)


#defense_contract_SP_ContractPricingCustomer.csv
contract.list<-read_and_join(Path
                                           ,"defense_contract_SP_ContractPricingCustomer.csv"
                                           ,contract.list
                                           ,"data\\"
                             ,"CSIScontractID"
)





#defense_contract_SP_ContractCompetitionVehicleCustomer.csv
contract.list<-read_and_join(Path
                                           ,"defense_contract_SP_ContractCompetitionVehicleCustomer.csv"
                                           ,contract.list
                                           ,"data\\"
                             ,"CSIScontractID"
)



write.table(contract.list
            ,file=paste("output\\contract_detail_output"
                        #I May want to add the date in here later
                        ,".csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)
