# Create empty lists to store aggregated flows for Europe
Flow.List.EU <- sapply(Materials, function(x) vector("list"))

Systems <- Systems.all
for (c in Systems) {
  
  # Calculate flows
  source("Code/11.2_TikZPart1.R")
  
  # Sum up country-specific flows
  for(mat in Materials) {
    
    if(length(Flow.List.EU[[mat]]) == 0) {
      Flow.List.EU[[mat]] <- Flow.List[[mat]]
      next
    }

    for(prod in names(Flow.List[[mat]])) {
      if(is.numeric(Flow.List[[mat]][[prod]])) {
        Flow.List.EU[[mat]][[prod]] <- Flow.List.EU[[mat]][[prod]] + Flow.List[[mat]][[prod]]
      }
      
      if(is.list(Flow.List[[mat]][[prod]])) {
        for(dest in names(Flow.List[[mat]][[prod]])) {
          Flow.List.EU[[mat]][[prod]][[dest]] <- 
            Flow.List.EU[[mat]][[prod]][[dest]] + Flow.List[[mat]][[prod]][[dest]]
        }
      }
    }
  }
  
  # Format for TikZ
  source("Code/11.3_TikZPart2.R")
  message(paste0(format(Sys.time(), "%H:%M:%S"), " TikZ flows have been saved for: ", c))
}

# Format for TikZ (Europe)
c <- "EU"
for (mat in Materials) {
  Flow.List[[mat]] <- Flow.List.EU[[mat]]
}

Flow.List <- Flow.List.EU
source("Code/11.3_TikZPart2.R")
message(paste0(format(Sys.time(), "%H:%M:%S"), " TikZ flows have been saved for: ", c))

# Add flows missing in each country compared to EU for TikZ plotting
flow_ref <- read.csv("Charts/flow_EU.txt", stringsAsFactors = FALSE, check.names = FALSE)

# Loop over all systems
for (c in Systems) {
  
  # 1. Read target flow file
  flow_target <- read.csv(
    paste0("Charts/flow_", c, ".txt"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  
  # 2. Find missing flows
  missing_flows <- setdiff(flow_ref[[1]], flow_target[[1]])
  
  # 3. Create rows for missing flows, all values set to 0
  if(length(missing_flows) > 0){
    zero_rows <- data.frame(matrix(0, nrow = length(missing_flows), ncol = ncol(flow_target)))
    colnames(zero_rows) <- colnames(flow_target)         # Ensure column names match exactly
    zero_rows[[1]] <- missing_flows                     # Fill first column (name) with missing flows
    flow_target_filled <- rbind(flow_target, zero_rows)
  } else {
    flow_target_filled <- flow_target
  }
  
  # 4. Reorder rows to match flow_ref
  flow_target_filled <- flow_target_filled[match(flow_ref[[1]], flow_target_filled[[1]]), ]
  
  # 5. Save completed file
  write.csv(flow_target_filled, 
            paste0("Charts/flow_", c, "_full.txt"), 
            row.names = FALSE, quote = FALSE)
  message(paste0(length(missing_flows)," empty flows have been added for: ", c))
}

flow_data <- readLines("Charts/flow_EU.txt")
writeLines(flow_data, "Charts/flow_EU_full.txt")

rm(c,comp,comp.stop,dest,mat,Names.agg,prod,to.save,
   flow_ref,flow_target,flow_target_filled,zero_rows,missing_cols,missing_flows,
   flow_data)
