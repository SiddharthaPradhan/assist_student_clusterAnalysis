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




# TODO:
# box plots please: done
# heirarchical clustering
# try lagging for problems
# pretest: done included mid test scores as well
# anova on gain scores after clustering
# check if class id or teacher id affects clustering
# might need to try gausian mixuture model, since the data points are close and kmeans is not doing well
# latent profiling 
# growth modeling on gain

# For now need problem level data
con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/sidpr/Desktop/Work/current_temp/maple_ies_research_copy_7_9_2022.db")

#print(dbListTables(con))
set.seed(123456789)

# "SELECT StuID,
# avg_first_response_time, how fast a student answers initially
# avg_problem_time, average answer time
# num_problems_parts_used_bottom_out_hint, # number of bottom out hints used
# num_problem_parts_hints_accessed, # number of hints used per problem part
# per_available_hints_accessed,
# num_correct_response_first_attempt_before_hint,
# num_correct_response_first_attempt_after_hint,
# num_problem_parts_started,
# num_problem_parts_attempted,
# num_graded_problems_started,
# num_graded_problems_attempted                 	
# FROM assist_student;"

# Some issues with data:
    # - NEED per_available_bottom_out_hints_accessed -> maybe group by stuID and problemID and 
    # most of the variable should be avg -> some students might have only completed few questions

# student_level

# to get avg_accuracy_first_attempt_after_hint, use num_graded_problem_parts to find


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

# removing additional variables for clustering
# might not need these
#	num_problems_parts_used_bottom_out_hint
#   num_problem_parts_hints_accessed

# no mid scores

working.data <- data %>% select(-c("StuID","TeaIDPre","ClaIDPre","ClaIDEnd","TeaIDEnd","post_total_math_score","num_problems_parts_used_bottom_out_hint","num_problem_parts_hints_accessed","gainScore"))

# scaled data 
scaled.working.data <- scale(working.data)
scaled.working.data.withID <- data.frame(scaled.working.data, data$StuID, data$gainScore) %>% rename(StuID = data.StuID)

# TESTING START
my_hclust <- function(df){
    cluster <- hclust(dist(df))
    return(cluster)
}

# TESTING END




# subset of data
#subset.data <- data %>% select(-c("StuID",
#                                  "num_problem_parts_started",
#                               "num_problem_parts_attempted",
#                               "num_graded_problems_started",
#                               "num_graded_problems_attempted"))
# scaled subset data
#scaled.subset.data <- scale(subset.data)


visualize_cluster_heatmap <- function(cluster) {
    clusterNum <- c(1: length(cluster$size))
    center_df <- data.frame(clusterNum, cluster$center)
    
    # Reshape the data
    
    features_temp <- unlist(attr(cluster$centers,"dimnames")[2])
    center_reshape <- gather(center_df, features, values, )
    library(RColorBrewer)
    # Create the palette
    hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')
    ggplot(data = center_reshape, aes(x = features, y = clusterNum, fill = values)) +
        geom_tile() +
        coord_equal() +
        scale_fill_gradientn(colours = hm.palette(90)) +
        theme_classic() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

visualize_cluster_boxplot_kmeans <- function(cluster, data){
    center_df <- data.frame(data, cluster$cluster)
    features_temp <- colnames(data)
    # Reshape the data
    temp_x <- unique(cluster$cluster)
    center_reshape <- gather(center_df, features, values, all_of(features_temp))
    ggplot(data = center_reshape, aes(x = as.factor(cluster.cluster), y = values, group=cluster.cluster, fill = as.factor(cluster.cluster))) +
        geom_boxplot(alpha = 0.3) + facet_grid(~features, scales = 'free_y') +                                                                # Change font size
        theme(strip.text.x = element_text(size = 6)) + scale_color_brewer(palette="BuPu")
}

visualize_cluster_boxplot_hclust <- function(cluster, data){
    center_df <- data.frame(data, cluster)
    features_temp <- colnames(data)
    # Reshape the data
    temp_x <- unique(cluster)
    center_reshape <- gather(center_df, features, values, all_of(features_temp))
    ggplot(data = center_reshape, aes(x = as.factor(cluster), y = values, group=cluster, fill = as.factor(cluster))) +
        geom_boxplot(alpha = 0.3) + facet_grid(~features, scales = 'free_y') +                                                                # Change font size
        theme(strip.text.x = element_text(size = 6)) + scale_color_brewer(palette="BuPu")
}


my_kmeans <- function(df, n) {
    cluster.data <- kmeans(df, n, nstart = 25)
    # setting the cluster vector to use StuID instead
    attr(cluster.data$cluster,"names") <- as.character(data$StuID)
    return (cluster.data)
}

get_optimal_k <- function(df){
# wss
    fviz_nbclust(df,kmeans, method="wss") #+ 
     #geom_vline(xintercept = 6,linetype = 2)

    #gap statistic
    fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
        labs(subtitle = "Gap statistic method")   
}



cluster.scaled.data <- my_kmeans(scaled.master.data,5) # was 5, new data suggests k = 3
#1 - TYPICAL STUDENTS - avg knowledge, avg hints, avg progress, avg speed
#2- LOW PERFORMING - low knowledge, avg hints, low usage, low accuray
#3 - GAMMERS - low knowledge, high hints, high progress, fast
#4 - DISENGAGED - low knowledge, avg hints, low usage, low accuracy, fast
#5 - HIGH PERFORMERS - high knowledge, low hints, high progress, fast, high performance

# scaled data for scaled mean
# unsclaed data for unscaled mean
get_center_by_data <- function(df, clusterVector){
    center_unscaled <- aggregate(df,by=list(cluster = clusterVector),FUN=mean)
    return(center_unscaled)
}

# performing anova on gain_score
my_anova <- function(clusterVec, data){
    center_df <- data.frame(data, clusterVec) %>% select(clusterVec, gainScore, StuID) %>%
        rename(clusterNum = clusterVec)
    center_df$clusterNum <- as.factor(center_df$clusterNum)
    center_df$StuID <- as.factor(center_df$StuID)
    
    outlier_table <- center_df %>% group_by(clusterNum) %>% identify_outliers(gainScore)
    outlier_check <- outlier_table %>%
        summarise(num_extreme_outlier = sum(ifelse(is.extreme == TRUE, 1, 0)), num_outlier = n())
    print(paste0("OUTLIER CHECK: ",outlier_check$num_outlier, " total outliers found"))
    print(paste0("OUTLIER CHECK: ",outlier_check$num_extreme_outlier, " extreme outliers found"))
    print("Number of outliers per clusterNum")
    print(table(outlier_table$clusterNum))
    plot(ggqqplot(center_df, "gainScore", facet.by = "clusterNum", title="QQ plot per clusterNum"))
    # Dependent variable is gainScore
    # Independent is clusterNumber
    # TODO:
    #aov <- anova_test(data = center_df, dv = gainScore, wid=StuID, within = clusterNum)
    my_aov <- aov(gainScore~clusterNum, data = center_df)
    return(my_aov)
    # then pairwise t-test?
}

# to get centers
#center_unscaled <- aggregate(master.data,by=list(cluster=cluster.unscaled.data$cluster),FUN=mean)
#center_unscaled


# StuID and Corresponding cluster number
#o=order(cluster.subset.data$cluster)
#data.frame(data$StuID[o],cluster.subset.data$cluster[o])


