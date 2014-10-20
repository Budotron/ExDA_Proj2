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

# subset the data to only include information for Baltimore City
baltimore<-NEI[NEI$fips=="24510",]

# melt the baltimore data frame by year
MeltedBaltimore<-melt(data = baltimore, 
                      id.vars = c("year", "type"), 
                      measure.vars = "emissions")

#cast to wide format and sum emissions by year and type of polutant
baltimoreEmissions<-dcast(data = MeltedBaltimore, 
                          formula = year+type~variable, 
                          fun.aggregate = sum)

#create plot
png(file = "plot3.png", width = 480, height = 480, units = "px")
g <- ggplot(data = baltimoreEmissions, aes(x = year, y = emissions, fill=type))
g <- g + geom_bar( stat="identity", binwidth=1)
g <- g + facet_grid(.~type) 
g <- g + theme(axis.text.x = element_text(angle = -90, hjust = 1))
g <- g + labs(x= "Year", y = expression("Total PM"[25]* " emissions (tons)"), title = expression("Tons of PM"[25]* " by year and type in Baltimore City"))
g
dev.off()