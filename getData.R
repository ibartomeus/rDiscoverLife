#getData{rDiscoverLife}

#function to get all information from a given id 

getData <- function(databese = "AMNH_BEE", id_range = c(1:100), ids = NULL, path = NULL){
require(plyr)
require(XML)
require(RCurl)
if(is.null(ids)){
	#construct url list
	url_list <- paste("http://www.discoverlife.org/mp/20l?id=", databese, formatC(id_range, width = 8, format = "d", flag = "0"), sep = "")
	} else{
	url_list <- paste("http://www.discoverlife.org/mp/20l?id=", ids, sep = "")
	}
#create data frame to save the input
df <- data.frame(id = NA)
##loop throught url's 
for (i in 1:length(url_list)){
#Parse url and extract table
doc <- getURL(url_list[i])	
doc2 <- htmlTreeParse(doc, useInternalNodes = TRUE)
tables <- getNodeSet(doc2, "//table")
t <- readHTMLTable(tables[[2]])
#stop here if url is not valid
if(length(t[3,])>1) {
#otherwise complete the data extration
tt <- as.matrix(t(t))
colnames(tt) <- tt[1,]
tt <- as.data.frame(tt)
tt$id <- substr(url_list[i], 39, nchar(url_list[i]))
if(length(grep("^V", colnames(tt)))){
	tt <- tt[,-grep("^V", colnames(tt))]}
if(length(which(colnames(tt) == "MAP"))){
	tt <- tt[,-which(colnames(tt) == "MAP")]}
if(length(which(nchar(colnames(tt)) >  70))){
	tt <- tt[,-which(nchar(colnames(tt)) >  70)]}
if(length(which(colnames(tt) == paste("ID_parent", substr(url_list[i], 39, nchar(url_list[i])))))){
	tt <- tt[,-which(colnames(tt) == paste("ID_parent", substr(url_list[i], 39, nchar(url_list[i]))))]}
#add the output to the dataframe, preserving all columns.
df <- rbind.fill(df, tt[2,])
df <- df[,colSums(is.na(df))<nrow(df)]	
print(i)
if (i %in% seq(100,100000,100))write.table(df, paste(path, ".txt", sep = ""))
	}}
df
}

#issues

#Dissmiss warning : In deparse(object[[i]]) :
  #it is not known that wchar_t is Unicode on this platform

#usage

getData(databese = "AMNH_BEE", id_range = c(1:100))

##database : which database do you want to access. For Bees one of "AMNH_BEE", "AMNH_BEES", "UCMS_ENT", "CUIC_ENT", "RMBL_ENT", "BMEC_ENT", "RUAC_ENT". See http://www.discoverlife.org/mp/20l?act=enter_id for other databases

##id_range : a vector with the id's you want to access. At May 2012 rabges are: AMNH 1-1867240, UCMS 2114-378004, CUIC 1-22087, RMBL 3-1643, BMEC 1-6885 and RUAC 1-7755

#ids: Alternatively a vector with all id's. If a vector is provided, database and id_range are depreciated.

#path: It tends to randomly crash evry 500-5000 records. It's a server response error. Autosave is implemented every 100 records. Indicate the path with the form "folder/subfolder/filename"


#Author: Ignasi Bartomeus, @ibartomeus

#example

(d <- getData(databese = "RUAC_ENT", id_range = c(1:5)))
(d <- getData(databese = "AMNH_BEE", id_range = c(1:5)))
(d <- getData(databese = "UCMS_ENT", id_range = c(1:5)))
(d <- getData(databese = "CUIC_ENT", id_range = c(1:5)))
d


#example in convination with getspID
id <- getspID(sci_name = "Andrena regularis", databases = "AMNH_BEE", country = "Northeast US", lat = NA, long = NA)
out <- getData(ids = id, path = "out_temp")





