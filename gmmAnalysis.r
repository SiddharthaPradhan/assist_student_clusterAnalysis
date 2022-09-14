require(RSQLite)
require(tidyverse)
require(cluster)
require(factoextra)
require(DBI)
require(dplyr)
require(gridExtra)
library(rstatix)
library(ggpubr)
require(psych)
require(mclust)

set.seed(1234555)

# TODO
# check if class id or teacher id affects clustering
# might need to try gausian mixuture model, since the data points are close and kmeans is not doing well
# latent profiling 
# growth modeling on gain

# For now need problem level data
con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/sidpr/Desktop/Work/current_temp/maple_ies_research_copy_7_9_2022.db")

data <- DBI::dbGetQuery(con, "SELECT a.StuID,
                        a.avg_first_response_time,
                    	a.avg_problem_time,
                    	a.num_problems_parts_used_bottom_out_hint,
                    	a.num_problem_parts_hints_accessed,
                    	a.per_available_hints_accessed,
                    	a.num_hints_accessed,
                    	a.avg_correct_response_any,
                    	a.avg_accuracy_first_attempt_before_hint,
                    	a.avg_accuracy_first_attempt_after_hint,
                       	a.num_problem_parts_started,
                    	a.num_problem_parts_attempted,
                    	a.num_graded_problems_started,
                    	a.num_graded_problems_attempted,
                    	sr.TeaIDPre,
                    	sr.ClaIDPre,
                    	sr.TeaIDEnd,
                    	sr.ClaIDEnd,
                    	b.pre_total_math_score,
                    	b.mid_total_math_score,
                    	b.post_total_math_score
                        FROM assist_student a
                        INNER JOIN student_roster sr ON a.StuID = sr.StuID
                        INNER JOIN assess_student b on a.StuID = b.StuID
                        WHERE sr.rdm_condition = 'ASSISTments' AND sr.condition_assignment = 'Instant';")

avg_bottom_out_hint_data <- DBI::dbGetQuery(con, "SELECT a.StuID,
                        a.bottom_out_hint,
                        a.problem_id
                        FROM assist_student_problem a;")

avg_bottom_out_hint_data <- na.omit(avg_bottom_out_hint_data)
avg_bottom_out_hint_data <- avg_bottom_out_hint_data %>% group_by(StuID) %>% summarise(avg_bottom_out_hint=sum(ifelse(bottom_out_hint > 0, 1, 0))/n())
data <- dplyr::inner_join(data,avg_bottom_out_hint_data,by="StuID")

a <- length(data$StuID)
data <- na.omit(data)
data <- data %>% dplyr::mutate(gainScore = (post_total_math_score - pre_total_math_score))
print(paste0(a - length(data$StuID), " rows removed for containing NA"))
rm(a)

visualize_cluster_boxplot <- function(clusterVec, data){
    center_df <- data.frame(data, clusterVec)
    features_temp <- colnames(data)
    # Reshape the data
    center_reshape <- gather(center_df, features, values, all_of(features_temp))
    ggplot(data = center_reshape, aes(x = as.factor(clusterVec), y = values, group=clusterVec, fill = factor(clusterVec))) +
        geom_boxplot(alpha = 0.3) + facet_grid(~features, scales = 'free_y', margins = TRUE) +                                                               
        theme(strip.text.x = element_text(size = 4)) + scale_color_brewer(palette="BuPu")
}

# removing additional variables for clustering
# might not need these
#	num_problems_parts_used_bottom_out_hint
#   num_problem_parts_hints_accessed

# no mid scores

working.data <- data %>% select(-c("StuID","TeaIDPre","ClaIDPre","ClaIDEnd","TeaIDEnd","post_total_math_score","mid_total_math_score","num_problems_parts_used_bottom_out_hint","num_problem_parts_hints_accessed","gainScore"))
scaled.working.data <- scale(working.data)
mb = Mclust(scaled.working.data)

#mb$modelName
