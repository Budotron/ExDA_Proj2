## For the sake of reproducibility, the code for downloading the data sets are 
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
# select all indices of SCC with vehicle in the EI.Sector
matches<-grep("[Vv]ehicle", SCC$eisector)
# obtain the SCC values
sccVals<-SCC$scc[matches]
# obtain scc matches in NEI
allVehicles<-NEI[which(NEI$scc %in% sccVals),]
# subset the data to only include information for Baltimore and LA
twoCityData<-allVehicles[allVehicles$fips=="24510"|allVehicles$fips=="06037",]
# melt the resulting data frame by year and city identifier
melted_twoCityData<-melt(data = twoCityData, 
                         id.vars = c("fips", "year"), 
                         measure.vars = "emissions")
# cast to wide format and sum emissions
cast_twoCityData<-dcast(data = melted_twoCityData, 
                        formula = fips+year~variable, 
                        fun.aggregate = sum)
# add a city column to the data, and identiy the city
cast_twoCityData$city<-ifelse(test = cast_twoCityData$fips=="06037", 
                              yes = "LA", 
                              no = "MD")
# create plot
png(file = "plot6.png", width = 500, height = 480, units = "px")
g <- ggplot(data = cast_twoCityData, aes(x = year, y = emissions, fill=city))
g <- g + geom_bar( stat="identity", binwidth=1)
g <- g + facet_grid(.~city) 
g <- g + labs(x= "Year", y = expression("Total Vehicular PM"[25]* " emissions (tons)"), title = expression("Tons of Vehicular PM"[25]* " emissions by year Los Angeles and in Baltimore City"))
g
dev.off()