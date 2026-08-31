# Interaction Effects: GCC and Workload
# Tables

# Power Use
# Pooled Descriptives

power.summary.pooled <- data.all.df %>%
  group_by(category, gcc) %>%
  summarise(
    M   = mean(meanW, na.rm = TRUE),
    Md  = median(meanW, na.rm = TRUE),
    SD  = sd(meanW, na.rm = TRUE),
    IQR = IQR(meanW, na.rm = TRUE),
    .groups = "drop"
  )

make_two_column_power_table <- function(power.summary.pooled,
                                         heading = "Pooled Execution (W)") {
  
  cats.left  <- c("apache", "concurrency", "database")
  cats.right <- c("functional", "scala", "web")
  
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\scriptsize\n")
  cat(sprintf("\\caption{Power Use Distributions (%s): Descriptive statistics summarising power use observations across workload categories and GNU GCC versions under %s.}\n",
              heading, tolower(heading)))
  cat("\\begin{tabular}{llrrrr@{\\hspace{6mm}}llrrrr}\n")
  cat("\\toprule\n")
  
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} &\n", heading))
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} \\\\\n", heading))
  
  cat("\\cmidrule(r){1-6}\\cmidrule(l){7-12}\n")
  
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR}\n")
  cat("&\n")
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR} \\\\\n")
  
  cat("\\midrule\n\n")
  
  for(i in 1:3){
    
    left  <- power.summary.pooled %>% filter(category == cats.left[i])
    right <- power.summary.pooled %>% filter(category == cats.right[i])
    
    for(j in 1:4){
      
      if(j == 1){
        
        cat(sprintf(
          "\\multirow{6}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f ",
          cats.left[i],
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "\\multirow{6}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          cats.right[i],
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
        
      } else {
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f ",
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
      }
    }
    
    if(i < 3){
      cat("\n\\midrule\n\n")
    }
  }
  
  cat("\n\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

make_two_column_power_table(power.summary.pooled)



# Power Use
# Tiered Execution
power.summary.tiered <- data.all.df %>%
  filter(mode == "HS") %>%
  group_by(category, gcc) %>%
  summarise(
    M   = mean(meanW, na.rm = TRUE),
    Md  = median(meanW, na.rm = TRUE),
    SD  = sd(meanW, na.rm = TRUE),
    IQR = IQR(meanW, na.rm = TRUE),
    .groups = "drop"
  )

make_two_column_power_table.tiered <- function(df,
                                         heading = "Tiered Execution (W)") {
  
  cats.left  <- c("apache", "concurrency", "database")
  cats.right <- c("functional", "scala", "web")
  
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\scriptsize\n")
  cat(sprintf("\\caption{Power Use Distributions (%s): Descriptive statistics summarising power use observations across workload categories and GNU GCC versions under %s.}\n",
              heading, tolower(heading)))
  cat("\\begin{tabular}{llrrrr@{\\hspace{6mm}}llrrrr}\n")
  cat("\\toprule\n")
  
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} &\n", heading))
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} \\\\\n", heading))
  
  cat("\\cmidrule(r){1-6}\\cmidrule(l){7-12}\n")
  
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR}\n")
  cat("&\n")
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR} \\\\\n")
  
  cat("\\midrule\n\n")
  
  for(i in 1:3){
    
    left  <- df %>% filter(category == cats.left[i])
    right <- df %>% filter(category == cats.right[i])
    
    for(j in 1:4){
      
      if(j == 1){
        
        cat(sprintf(
          "\\multirow{4}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f ",
          cats.left[i],
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "\\multirow{4}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          cats.right[i],
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
        
      } else {
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f ",
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
      }
    }
    
    if(i < 3){
      cat("\n\\midrule\n\n")
    }
  }
  
  cat("\n\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

make_two_column_power_table.tiered(power.summary.tiered)





# Power Use
# Interpretive Execution
power.summary.interpretive <- data.all.df %>%
  filter(mode == "INT") %>%
  group_by(category, gcc) %>%
  summarise(
    M   = mean(meanW, na.rm = TRUE),
    Md  = median(meanW, na.rm = TRUE),
    SD  = sd(meanW, na.rm = TRUE),
    IQR = IQR(meanW, na.rm = TRUE),
    .groups = "drop"
  )

make_two_column_power_table.interpretive <- function(df,
                                               heading = "Interpretive Execution (W)") {
  
  cats.left  <- c("apache", "concurrency", "database")
  cats.right <- c("functional", "scala", "web")
  
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\scriptsize\n")
  cat(sprintf("\\caption{Power Use Distributions (%s): Descriptive statistics summarising power use observations across workload categories and GNU GCC versions under %s.}\n",
              heading, tolower(heading)))
  cat("\\begin{tabular}{llrrrr@{\\hspace{6mm}}llrrrr}\n")
  cat("\\toprule\n")
  
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} &\n", heading))
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} \\\\\n", heading))
  
  cat("\\cmidrule(r){1-6}\\cmidrule(l){7-12}\n")
  
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR}\n")
  cat("&\n")
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR} \\\\\n")
  
  cat("\\midrule\n\n")
  
  for(i in 1:3){
    
    left  <- df %>% filter(category == cats.left[i])
    right <- df %>% filter(category == cats.right[i])
    
    for(j in 1:4){
      
      if(j == 1){
        
        cat(sprintf(
          "\\multirow{4}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f ",
          cats.left[i],
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "\\multirow{4}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          cats.right[i],
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
        
      } else {
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f ",
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
      }
    }
    
    if(i < 3){
      cat("\n\\midrule\n\n")
    }
  }
  
  cat("\n\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

make_two_column_power_table.interpretive(power.summary.interpretive)






# Energy Consumption
# Pooled Descriptives

energy.summary.pooled <- data.all.df %>%
  group_by(category, gcc) %>%
  summarise(
    M   = mean(energyWh, na.rm = TRUE),
    Md  = median(energyWh, na.rm = TRUE),
    SD  = sd(energyWh, na.rm = TRUE),
    IQR = IQR(energyWh, na.rm = TRUE),
    .groups = "drop"
  )

make_two_column_energy_table <- function(df,
                                        heading = "Pooled Execution (Wh)") {
  
  cats.left  <- c("apache", "concurrency", "database")
  cats.right <- c("functional", "scala", "web")
  
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\scriptsize\n")
  cat(sprintf("\\caption{Energy Consumption Distributions (%s): Descriptive statistics summarising energy consumption observations across workload categories and GNU GCC versions under %s.}\n",
              heading, tolower(heading)))
  cat("\\begin{tabular}{llrrrr@{\\hspace{6mm}}llrrrr}\n")
  cat("\\toprule\n")
  
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} &\n", heading))
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} \\\\\n", heading))
  
  cat("\\cmidrule(r){1-6}\\cmidrule(l){7-12}\n")
  
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR}\n")
  cat("&\n")
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR} \\\\\n")
  
  cat("\\midrule\n\n")
  
  for(i in 1:3){
    
    left  <- df %>% filter(category == cats.left[i])
    right <- df %>% filter(category == cats.right[i])
    
    for(j in 1:4){
      
      if(j == 1){
        
        cat(sprintf(
          "\\multirow{6}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f ",
          cats.left[i],
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "\\multirow{6}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          cats.right[i],
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
        
      } else {
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f ",
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
      }
    }
    
    if(i < 3){
      cat("\n\\midrule\n\n")
    }
  }
  
  cat("\n\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

make_two_column_energy_table(energy.summary.pooled)



# Energy Consumption
# Tiered Execution
energy.summary.tiered <- data.all.df %>%
  filter(mode == "HS") %>%
  group_by(category, gcc) %>%
  summarise(
    M   = mean(energyWh, na.rm = TRUE),
    Md  = median(energyWh, na.rm = TRUE),
    SD  = sd(energyWh, na.rm = TRUE),
    IQR = IQR(energyWh, na.rm = TRUE),
    .groups = "drop"
  )

make_two_column_energy_table.tiered <- function(df,
                                               heading = "Tiered Execution (Wh)") {
  
  cats.left  <- c("apache", "concurrency", "database")
  cats.right <- c("functional", "scala", "web")
  
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\scriptsize\n")
  cat(sprintf("\\caption{Energy Consumption Distributions (%s): Descriptive statistics summarising energy consumption observations across workload categories and GNU GCC versions under %s.}\n",
              heading, tolower(heading)))
  cat("\\begin{tabular}{llrrrr@{\\hspace{6mm}}llrrrr}\n")
  cat("\\toprule\n")
  
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} &\n", heading))
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} \\\\\n", heading))
  
  cat("\\cmidrule(r){1-6}\\cmidrule(l){7-12}\n")
  
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR}\n")
  cat("&\n")
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR} \\\\\n")
  
  cat("\\midrule\n\n")
  
  for(i in 1:3){
    
    left  <- df %>% filter(category == cats.left[i])
    right <- df %>% filter(category == cats.right[i])
    
    for(j in 1:4){
      
      if(j == 1){
        
        cat(sprintf(
          "\\multirow{4}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f ",
          cats.left[i],
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "\\multirow{4}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          cats.right[i],
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
        
      } else {
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f ",
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
      }
    }
    
    if(i < 3){
      cat("\n\\midrule\n\n")
    }
  }
  
  cat("\n\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

make_two_column_energy_table.tiered(energy.summary.tiered)





# Energy Consumption
# Interpretive Execution
energy.summary.interpretive <- data.all.df %>%
  filter(mode == "INT") %>%
  group_by(category, gcc) %>%
  summarise(
    M   = mean(energyWh, na.rm = TRUE),
    Md  = median(energyWh, na.rm = TRUE),
    SD  = sd(energyWh, na.rm = TRUE),
    IQR = IQR(energyWh, na.rm = TRUE),
    .groups = "drop"
  )

make_two_column_energy_table.interpretive <- function(df,
                                                     heading = "Interpretive Execution (W)") {
  
  cats.left  <- c("apache", "concurrency", "database")
  cats.right <- c("functional", "scala", "web")
  
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\scriptsize\n")
  cat(sprintf("\\caption{Energy Consumption Distributions (%s): Descriptive statistics summarising energy consumption observations across workload categories and GNU GCC versions under %s.}\n",
              heading, tolower(heading)))
  cat("\\begin{tabular}{llrrrr@{\\hspace{6mm}}llrrrr}\n")
  cat("\\toprule\n")
  
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} &\n", heading))
  cat(sprintf("\\multicolumn{6}{c}{\\textbf{%s}} \\\\\n", heading))
  
  cat("\\cmidrule(r){1-6}\\cmidrule(l){7-12}\n")
  
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR}\n")
  cat("&\n")
  cat("\\textbf{Category} & \\textbf{GCC} & \\textbf{M} & \\textbf{M$_d$} & \\textbf{SD} & \\textbf{IQR} \\\\\n")
  
  cat("\\midrule\n\n")
  
  for(i in 1:3){
    
    left  <- df %>% filter(category == cats.left[i])
    right <- df %>% filter(category == cats.right[i])
    
    for(j in 1:4){
      
      if(j == 1){
        
        cat(sprintf(
          "\\multirow{4}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f ",
          cats.left[i],
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "\\multirow{4}{*}{%s} & %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          cats.right[i],
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
        
      } else {
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f ",
          left$gcc[j],
          left$M[j],
          left$Md[j],
          left$SD[j],
          left$IQR[j]
        ))
        
        cat("& ")
        
        cat(sprintf(
          "& %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
          right$gcc[j],
          right$M[j],
          right$Md[j],
          right$SD[j],
          right$IQR[j]
        ))
      }
    }
    
    if(i < 3){
      cat("\n\\midrule\n\n")
    }
  }
  
  cat("\n\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
}

make_two_column_energy_table.interpretive(energy.summary.interpretive)
