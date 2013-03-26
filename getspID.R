#getspID{rDiscoverLife}

#function to get all records id's from a given species

getspID <- function(sci_name = NA, databases = "AMNH_BEE,UCMS_ENT,CUIC_ENT,RMBL_ENT,BMEC_ENT,RUAC_ENT", country = "Northeast US", lat = NA, long = NA){
require(plyr)
require(XML)
#re-format input
sci_name2 <- gsub(" ", "+",sci_name)
if (country == "Northeast US"){
lat <- 42	
long <- -78	
}
#construct url list
#url <- "http://www.discoverlife.org/mp/20m?kind=Andrena+banksi&all_points=1&m_gz=1&a=AMNH_BEE,UCMS_ENT,CUIC_ENT,RMBL_ENT,BMEC_ENT,RUAC_ENT&r=.05&la=42&lo=-78"
url <- paste("http://www.discoverlife.org/mp/20m?kind=", sci_name2, "&all_points=1&m_gz=1&a=", databases, "&&r=.05&la=", lat, "&lo=", long, sep = "")
###read 1st url and extract daugther url's
#read html lines	
l <- readLines(url)
#create an empty vector to store is's for each line
id <- rep("a",length(l))
#loop throu lines extracting only id's
for (i in 1:length(l)){
if (length(regmatches(l[i], regexpr("AMNH_BEE[0-9]{8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("AMNH_BEE[0-9]{8}", l[i]))}
if (length(regmatches(l[i], regexpr("UCMS_ENT[0-9]{8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("UCMS_ENT[0-9]{8}", l[i]))}
if (length(regmatches(l[i], regexpr("CUIC_ENT[0-9]{8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("CUIC_ENT[0-9]{8}", l[i]))}
if (length(regmatches(l[i], regexpr("RUAC_ENT[0-9]{8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("RUAC_ENT[0-9]{8}", l[i]))}
if (length(regmatches(l[i], regexpr("RMBL_ENT[0-9]{8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("RMBL_ENT[0-9]{8}", l[i]))}
if (length(regmatches(l[i], regexpr("BMEC_ENT[0-9]{8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("BMEC_ENT[0-9]{8}", l[i]))}
if (length(regmatches(l[i], regexpr("UCRC_ENT[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("UCRC_ENT[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("DART_ENT[0-9]{8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("DART_ENT[0-9]{8}", l[i]))}
if (length(regmatches(l[i], regexpr("VTST_ENT[0-9]{8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("VTST_ENT[0-9]{8}", l[i]))}
if (length(regmatches(l[i], regexpr("KSEM[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("KSEM[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("LLL[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("LLL[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("NLA[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("NLA[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("USGS_DRO[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("USGS_DRO[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("CSCA[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("CSCA[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("EMEC[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("EMEC[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("KCM[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("KCM[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("LACM_ENTB[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("LACM_ENTB[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("NWU[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("NWU[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("RMY[0-9]{1,8}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("RMY[0-9]{1,8}", l[i]))}
if (length(regmatches(l[i], regexpr("BOLD_[A-Z,1-9,_]{8,16}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("BOLD_[A-Z,1-9,_]{8,16}", l[i]))}
if (length(regmatches(l[i], regexpr("BBSL[0-9]{6}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("BBSL[0-9]{6}", l[i]))}
if (length(regmatches(l[i], regexpr("BBSL_[A-Z,1-9,_]{8,16}", l[i]))) > 0){
id[i] <- regmatches(l[i], regexpr("BBSL_[A-Z,1-9,_]{8,16}", l[i]))}
}
#remove lines with no id's
id <- id[-which(id == "a")]
#remove duplicates
id <- unique(id)
}

#issues

#do not capture the second specimen with same lat long... 
#data frames returned are very caotic, specially if using different databases, due to the dispariety among variebles names in the webpage. Post-processing needed.
# BOLD database id's are not recognized by GetData... may happen with other databases that don't use a standard naming

#usage

getspID(sci_name = NA, databases = "AMNH_BEE,UCMS_ENT,CUIC_ENT,RMBL_ENT,BMEC_ENT,RUAC_ENT", country = "US", lat = NA, long = NA)

##sci_name = Scientific name. Just accept one at a time. 

##databeses = list of databases separated by comas and no spaces. Currently only suports AMNH_BEE, UCMS_ENT, CUIC_ENT, RMBL_ENT, BMEC_ENT and RUAC_ENT databases.
#Added:UCRC_ENT,DART_ENT,VTST_ENT,KSEM,LLL,NLA,USGS_DRO,CSCA,EMEC,KCM,LACM_ENTB,NWU,RMY,BOLD,BBSL, For this databases I can not guarantee all id numbers will be selected, as id range of numbers that the code recognaize are based in a guestimate. Other databases may be available. 

##country = limit the scope of the query. only Northeast US suported. For other regions use lat/long

##lat/long = latitude and longitude of the central point. If country = "Northeast US", this is depreciated. Actually the region is limited to plus minus 15 latitudinal degrees (30 longitude) around the central point. (that is between 48 and 33 latitude degrees and between -63 and -93 longitude dergrees for the US defoult)


#Author: Ignasi Bartomeus, @ibartomeus

#Example:

id <- getspID(sci_name = "Andrena accepta", databases = "AMNH_BEE", country = "Northeast US", lat = NA, long = NA)
id
id <- getspID(sci_name = "Andrena regularis", databases = "UCRC_ENT,DART_ENT,VTST_ENT,KSEM,LLL,NLA,USGS_DRO,CSCA,EMEC,KCM,LACM_ENTB,NWU,RMY,BOLD,BBSL", country = "Northeast US", lat = NA, long = NA)
id

#Example in combination with GetData

id <- getspID(sci_name = "Andrena accepta", databases = "AMNH_BEE", country = "Northeast US", lat = NA, long = NA)
out <- getData(ids = id, path = "out_temp")



