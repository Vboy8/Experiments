normalizeTable <- function(source){

	# normalizeTable is a function that normalizes data
	# csv file is needed as an argument
	# FOR EXAMPLE: normalizeTable("example.csv")



	#import csv to dataframe
	dataTable = read.csv(source, na.strings=c("","NA"))

	#see if the dataframe has 12 cells per row
	for (i in (1:nrow(dataTable))){
		if (ncol(dataTable[i,]) != 12){
			stop("Execution stopped. Each row needs Patient ID, Event ID, and 10 diagnoses.")
		}
	}

	#see if all patient id's and event id's are in the dataframe
	for (i in (1:nrow(dataTable))){
		if(is.na(dataTable[i,1])){
			stop(paste("Execution stopped. Patient ID missing in row #",i,''))
		}
		if(is.na(dataTable[i,2])){
			stop(paste("Execution stopped. Event ID missing in row #",i,''))
		}
	}

	#make normalized dataframe
	Patient_ID = vector()
	Event_ID = vector()
	Diagnosis = vector()
	for (i in (1:nrow(dataTable))){
		for (j in (3:ncol(dataTable[i,]))){
			if (!(is.na(dataTable[i,j]))){
				Patient_ID <- c(Patient_ID, dataTable[i,1])
				Event_ID <- c(Event_ID, dataTable[i,2])
				Diagnosis <- c(Diagnosis, levels(dataTable[,j])[dataTable[i,j]])
			}
		}
	}
	dataTable2 = data.frame(Patient_ID, Event_ID, Diagnosis, stringsAsFactors=FALSE)

	#export normalized dataframe to csv
	write.csv(x=dataTable2, file="normalized.csv", row.names = FALSE)
}



# This is the test script:

normalizeTable("example.csv")

#
# This is the result when the test script works:
#
# Patient_ID	Event_ID	Diagnosis
# 1	1	a
# 1	1	b
# 1	2	c
# 2	3	d
# 2	3	e
# 2	3	f
# 3	4	g
# 3	5	h
# 3	5	i
# 3	6	c
#
# (This should all be displayed in a CSV
# file labeled "normalized.csv")
#