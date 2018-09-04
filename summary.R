# Plot issues ----
output$summaryPlot <- renderPlot({
  
  gbif_db <- dbConnect(RSQLite::SQLite(), database_file)
  # Get rows with issues ----
  distinct_issues <- dbGetQuery(gbif_db, "SELECT DISTINCT issue FROM issues")
  distinct_issues <- unlist(distinct_issues, use.names = FALSE)
  
  #summary 
  summary_vals <- data.frame(matrix(ncol = 2, nrow = 0, data = NA))
  
  for (i in 1:length(distinct_issues)){
    this_issue <- dbGetQuery(gbif_db, paste0("SELECT count(*) FROM issues WHERE issue = '", distinct_issues[i], "'"))
    summary_vals <- rbind(summary_vals, cbind(distinct_issues[i], as.numeric(this_issue[1])))
  }
  
  # #no issues
  # this_issue <- dbGetQuery(gbif_db, paste0("SELECT count(*) FROM gbif WHERE issue = ''"))
  # summary_vals <- rbind(summary_vals, cbind("None", as.numeric(this_issue[1])))
  
  names(summary_vals) <- c("issue", "no_records")
  summary_vals$no_records <- as.numeric(paste(summary_vals$no_records))
  
  
  #Sort by no of cases
  #summary_vals <- summary_vals[order(-summary_vals$no_records),]
  summary_vals$issue <- factor(summary_vals$issue, levels = summary_vals$issue[order(-summary_vals$no_records)])
  levels(summary_vals$issue) <- gsub("_", "\n", levels(summary_vals$issue))

  # Close db ----
  dbDisconnect(gbif_db)
  
  ggplot(data = summary_vals, aes(x = issue, y = no_records, label = issue, colour = issue, fill = issue)) +
    geom_col() +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    theme(
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 14, face = "bold"), 
          legend.position="none", 
          #axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
          plot.title = element_text(size = 18, face="bold")
    ) + 
    labs(
        title = "Number of Records by Issue", 
        x = "Issue", 
        y = "No. of Records"
        )
})




# Plot records with multiple issues ----
output$summaryPlot2 <- renderPlot({
  issues_by_rec <- dbGetQuery(gbif_db, "select a.no_issues as no_issues, count(a.gbifID) as no_records, round((count(a.gbifID + 0.0)/(b.total_records + 0.0))*100,2) as percent from (select gbifID, count(*) as no_issues from issues group by gbifID) a, (select count(gbifID) as total_records from gbif) b group by a.no_issues")
  
  issues_by_rec_none <- dbGetQuery(gbif_db, "select 0 as no_issues, count(a.gbifID) as no_records, round((count(a.gbifID + 0.0)/(b.total_records + 0.0))*100,2) as percent from (select gbifID from gbif WHERE gbifID NOT IN (select gbifID from issues)) a, (select count(gbifID) as total_records from gbif) b")
  if (issues_by_rec_none$no_records > 0){
    #If there are records without issues, add them
    issues_by_rec <- rbind(issues_by_rec, issues_by_rec_none)
  }
  
  ggplot(data = issues_by_rec, aes(x = no_issues, y = no_records, label = percent, colour = no_issues, fill = no_issues)) +
    geom_col() +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    theme(
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "bold"), 
          legend.position="none", 
          plot.title = element_text(size = 18, face="bold")
          ) + 
    geom_text(aes(label = paste(percent, "%"), size = 10), position=position_dodge(width = 0.9), vjust = -0.5) +
    labs(
        title = "Number of Issues by Record", 
        subtitle = "Percent is from total no. of rows", 
        x = "No. of Issues/Record", 
        y = "No. of Records"
        )
})




# Related issues ----
output$summaryPlot3 <- renderPlot({
  
  issues_summ <- dbGetQuery(gbif_db, "select replace(a.issue, '_', '\n') as issue_a, replace(b.issue, '_', '\n') as issue_b, count(a.gbifID) as no_records from (select gbifID, issue from issues) a LEFT JOIN (select gbifID, issue from issues) b ON (a.gbifID = b.gbifID AND a.issue != b.issue) WHERE b.issue IS NOT NULL GROUP BY a.issue")
  
  ggplot(data = issues_summ, aes(issue_b, issue_a)) +
    geom_tile(aes(fill = no_records), color = "white") +
    theme(
          axis.title = element_blank(), 
          legend.position="right", 
          #axis.text.x = element_text(size=10, angle = 45, hjust = 1),
          #axis.text.y = element_text(size=10, angle = 45),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 18, face="bold")) +
    scale_x_discrete(limits = levels(issues_summ$issue_a)) + 
    scale_y_discrete(limits = levels(issues_summ$issue_b)) + 
    scale_fill_gradient(low = "yellow", high = "red") + 
    labs(
        fill = "No. of Records\nwith both\nIssues", 
        title = "Records Sharing Issues"
        )

})