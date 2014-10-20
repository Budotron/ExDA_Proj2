(## For the sake of reproducibility, the code for downloading the data sets are 
## included. Scan to ####################### below for the code that generates the 
## plot

## Load required packages, and read the data into the curent workspace
packages<-c("data.table", "dplyr", "reshape2", "ggplot2", "Hmisc")
sapply(packages, require, character.only = TRUE)
# function getdata() checks for the existence of a directory containing a file to 
# be downloaded, and if it is not present, downloads a linked file and stores it 
# in a directory in the current workspace. 
#
# input: a URL linked to a file to be downloaded, desired name for the 
#        directory, desired name for the downloaded file, extension for the 
#        file. 
# output : the path to the downloaded file
getdata<-function(fileUrl, dir, filename, ext){
        # create directory, if it is not already present
        dirName<-paste(dir, sep = "")
        if(!file.exists(dirName)){
                dir.create(path = dirName)
        }
        # Get the data, unless this step has already been done
        dest<-paste("./", dirName,"/", filename, ext, sep = "")
        if(!file.exists(dest)){
                download.file(url = fileUrl, 
                              destfile = dest, 
                              method = "curl") 
                datedownloaded<-date()
        }
        dest
}
fileURL<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
temp<-getdata(fileUrl = fileURL, 
              dir = "ExDa2", 
              filename = "ExDAData",
              ext = ".zip")
epaData<-unzip(zipfile = temp)
if (!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
}
# as a cleaning step, convert all column headers to lower case letters without
# puncturation
colnames(NEI)<-tolower(colnames(NEI))
colnames(SCC)<-tolower(names(SCC))
colnames(SCC)<-gsub(pattern = "\\.", "", x = names(SCC))
colnames(SCC)<-gsub(pattern = "_", "", x = names(SCC))

###################################################################################

# to find the cases where coal is used for combustion, look for where coal is 
# the fuel (level 3) and the process is combustion (level 1)

matches<-intersect(
        grep("Coal", SCC$scclevelthree), 
        grep("Combustion", SCC$scclevelone)
        )
# obtain the SCC codes for each of these indices
sccVals<-SCC$scc[matches]
# obtain scc matches in NEI data 
coalNEI<-NEI[NEI$scc==sccVals,]
# melt the coalNEI dataframe by year
melted_coalNEI<-melt(data = coalNEI, 
                     id.vars = "year", 
                     measure.vars = "emissions")
# cast to wide format and sum emissons
total_coal_NEI<-dcast(data = melted_coalNEI, 
                      formula = year~variable, 
                      fun.aggregate = sum)

#create plot
png(file = "plot4.png", width = 480, height = 480, units = "px")
gg<-ggplot(data = total_coal_NEI, aes(x = year, y = emissions))
gg<-gg + geom_bar( stat="identity", binwidth=1)
gg +  labs(x= "Year", 
           y = "Emissions", 
           title = expression("Total PM"[25]* " Coal Combustion Emissions (tons) in the US"))
dev.off()