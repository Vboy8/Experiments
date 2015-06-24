library("ggplot2")

# Given a normalized table with range of
# visits, total patient numbers, and whether
# or not they are referred to the MDU,
# the code makes a "range of visits" (ROV) bar graph.

# Normalized table should have this structure:

#       col1 col2                    col3
# 1   0 - <2   12     Referred to the MDU
# 2   3 - <4   93     Referred to the MDU
# 3   5 - <6   92     Referred to the MDU
# 4   7 - <8   12     Referred to the MDU
# 5  9 - <10   97     Referred to the MDU
# 6   0 - <2    3 Not Referred to the MDU
# 7   3 - <4   95 Not Referred to the MDU
# 8   5 - <6    9 Not Referred to the MDU
# 9   7 - <8   66 Not Referred to the MDU
# 10 9 - <10   88 Not Referred to the MDU
#

makeROVBarGraph <- function(dat, ylabel){
	ggplot(data=dat, aes(x=as.vector(dat[,1]),
	y=as.vector(dat[,2]), fill=as.vector(dat[,3]))) +
    geom_bar(stat="identity", position=position_dodge()) +
    xlab("Range of Visits") +
    ylab(ylabel) +
	scale_fill_manual(name="Range of Visits",
		values=c("#999999", "#E69F00")) +
	ggtitle("Distribution of Range of Visits")
}

#
# This is test script.
#

rov_labels_1 = c("0 - <2", "3 - <4",
	"5 - <6", "7 - <8", "9 - <10")
total_patients_1 = vector()
for(i in 1:length(rov_labels_1)){
	total_patients_1 = c(total_patients_1, sample(1:100,1))
}
mdu_bound_true = rep("Referred to the MDU",length(rov_labels_1))

rov_labels_2 = rov_labels_1
total_patients_2 = vector()
for(i in 1:length(rov_labels_2)){
	total_patients_2 = c(total_patients_2, sample(1:100,1))
}
mdu_bound_false = rep("Not Referred to the MDU",length(rov_labels_2))

col1 = c(rov_labels_1, rov_labels_2)
col2 = c(total_patients_1, total_patients_2)
col3 = c(mdu_bound_true, mdu_bound_false)
dat = data.frame(col1, col2, col3)

makeROVBarGraph(dat, "Total Patients")

#
# Executing the test script should return a bar graph.
#