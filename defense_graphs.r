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
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
# Path<-"~\\FPDS\\R scripts and data\\"
#Path<-"C:\\Users\\Rhys McCormick\\Documents\\Development\\Data\\"

axis.text.size<-8
strip.text.size<-10
legend.text.size<-12

# table.text.size<-5.75
#title.text.size<-10
#geom.text.size<-3

source(paste(Path,"lookups.r",sep=""))
source(paste(Path,"helper.r",sep=""))
source(paste(Path,"create_procedural_graphs.r",sep=""))

setwd("K:\\Development\\Components")

debug(create_procedural_graphs)
create_procedural_graphs("Defense Components","SubCustomer.component",2000)
#create_procedural_graphs("Defense Components","Overall",2000)

