library("ggplot2")

"""
Given a normalized table with range of
visits, total patient numbers, and whether
or not they are referred to the MDU,
the code makes a simple bar graph.
"""

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
ggplot(data=dat, aes(x=col1, y=col2,
	fill=col3)) +
    geom_bar(stat="identity", position=position_dodge()) +
    xlab("Range of Visits") +
    ylab("Total Patients") +
	scale_fill_manual(name="Range of Visits",
		values=c("#999999", "#E69F00")) +
	ggtitle("Distribution of Range of Visits")