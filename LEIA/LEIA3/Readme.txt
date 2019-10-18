1. "nigeria_main_code.R" is the main ABM file.

2. shapefiles.zip should be extracted to the folder shapefiles, a subdirectory of the working directory.

3. "2_create_dataset.R":
	line 7: ou<-read.delim("MER_Structured_Dataset_Site_IM_FY17-19_20190621_v2_1_Nigeria.txt",header=T,colClasses=type,stringsAsFactors=F) # import MER data