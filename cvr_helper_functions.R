get_info <- function(xml_file_path, file_names){
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = length(file_names), clear = FALSE, width= 60)
  
  batch_sequence <- c()
  sheet_number <- c()
  precinct_split_name <- c()
  precinct_split_id <- c()
  cvr_guid <- c()
  batch_number <- c()
  
  for (i in 1:length(file_names)){
    pb$tick()
    
    # Read in xml
    xml_ballot <- read_xml(paste0(xml_file_path, file_names[i]))
    children <- xml_children(xml_ballot)
    
    # Get info values
    batch_sequence <- c(batch_sequence, xml_text(children[2]))
    sheet_number <- c(sheet_number, xml_text(children[3]))
    precinct_split <- xml_text(xml_children(children[4]))
    precinct_split_name <- c(precinct_split_name, precinct_split[1])
    precinct_split_id <- c(precinct_split_id, precinct_split[2])
    
    # Deal with various structures
    if (length(children) == 5){
      batch_number <- c(batch_number, NA)
      cvr_guid <- c(cvr_guid, xml_text(children[5]))
    }
    
    else if (length(children) == 6){
      batch_number <- c(batch_number, xml_text(children[5]))
      cvr_guid <- c(cvr_guid, xml_text(children[6]))
    }
    
    else {
      stop(paste0("Unexpected structure of cvr info for file ", i))
    }
    
  }
  
  # Join into one dataframe
  cvr_info <- data.frame(batch_sequence, sheet_number, precinct_split_name, precinct_split_id, cvr_guid, batch_number)
  return(cvr_info)
}

get_positions <- function(xml_file_path, file_names, 
                          precinct_split_name, sheet_num, cvr_info){
  
  # Find rows with a particular precinct_id
  row_ids <- which(cvr_info$precinct_split_name == precinct_split_name & 
                     cvr_info$sheet_number == sheet_num)
  
  # Read in one ballot
  if (file.exists(paste0(xml_file_path, file_names[row_ids[1]]))){
    xml_ballot <- read_xml(paste0(xml_file_path, file_names[row_ids[1]]))
  }
  else (
    stop(paste0("File does not exist: ", xml_file_path, file_names[row_ids[1]]))
  )
  
  # Get the names of the races on the ballot
  positions <- c()
  children <- xml_children(xml_ballot)
  children_2 <- xml_children(children[1])
  for (i in 1:length(children_2)){
    positions <- c(positions, xml_text(xml_children(children_2[i])[1]))
  }
  
  return(positions)
}

get_votes_df <- function(xml_file_path, file_names, precinct_split_name, 
                         sheet_num, cvr_info){
  
  # Get positions
  positions <- get_positions(xml_file_path, file_names, precinct_split_name, 
                             sheet_num, cvr_info)
  col_names <- c(colnames(cvr_info), positions)
  
  # Initialize votes df
  votes_df <- data.frame((matrix(nrow = 0, ncol = length(col_names))), 
                         stringsAsFactors = FALSE)
  colnames(votes_df) <- col_names
  
  # Find rows with a particular precinct_id
  row_ids <- which(cvr_info$precinct_split_name == precinct_split_name & 
                     cvr_info$sheet_number == sheet_num)
  
  index <- 1
  
  # Loop through xmls and create dataframe of votes
  for (i in 1:length(file_names)){
    if (i %in% row_ids){
      
      # Read in one ballot
      if (file.exists(paste0(xml_file_path, file_names[i]))){
        xml_ballot <- read_xml(paste0(xml_file_path, file_names[i]))
      }
      else (
        stop(paste0("File does not exist: ", xml_file_path, file_names[i]))
      )
      
      votes <- cvr_info[i,]
      children <- xml_children(xml_child(xml_ballot, 1))
      for (i in 1:length(children)){
        
        # Deals w/ undervotes
        if (xml_text(xml_child(children[i], 3)) == "" && 
            xml_text(xml_child(children[i], 4)) == "1"){
          text <- "Undervote"
        }
        # Deals w/ write-ins
        else if (xml_name(xml_child(xml_child(xml_child(children[i],3), 1), 3)) == 'WriteInData'){
          text <- "Write-In"
        }
        # Also deals with write-ins in a different format
        else if (length(xml_children(xml_child(xml_child(children[i],3),1))) == 4){
          if (xml_name(xml_child(xml_child(xml_child(children[i],3), 1), 4)) == 'WriteInData'){
            text <- "Write-In"
          }
          else {
            print("Yo what's happening")
            text <- xml_text(xml_child(xml_child(xml_child(children[i], 3), 1), 1))
          }
        }
        # Puts name of candidate
        else {
          text <- xml_text(xml_child(xml_child(xml_child(children[i], 3), 1), 1))
        }
        votes <- c(votes, text)
      }
      votes_df[index,] <- votes
      index <- index + 1
    }
  }
  
  return(votes_df)
}

clean_cvr_sheet <- function(xml_file_path, file_names, sheet_num, cvr_info){
  
  precinct_ids <- cvr_info %>%
    group_by(precinct_split_name, sheet_number) %>%
    summarize(count = n(), .groups = 'drop') %>%
    filter(sheet_number == sheet_num) %>%
    pull(precinct_split_name)
  
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = length(precinct_ids), clear = FALSE, width= 60)
  
  all_votes <- data.frame()
  for (i in 1:length(precinct_ids)){
    pb$tick()
    votes_df <- get_votes_df(xml_file_path, file_names, precinct_ids[i], sheet_num, cvr_info)
    all_votes <- bind_rows(all_votes, votes_df)
  }
  
  return(all_votes)
}

clean_cvr_data <- function(xml_file_path, file_names){
  
  # Get all the metadata about the votes
  cvr_info <- get_info(xml_file_path, file_names)
  
  # Get the maximum number of sheets in a ballot
  num_sheets <- max(cvr_info$sheet_number)
  
  # Get list of dataframes that are each sheet of the ballot
  sheet_list <- lapply(num_sheets, function(x) clean_cvr_sheet(sheet_num = x), 
                       xml_file_path = xml_file_path, file_names = file_names, 
                       cvr_info = cvr_info)
  
  # Join all sheets together
  cvr <- bind_rows(sheet_list) %>%
    arrange(batch_number, batch_sequence)
  
  return(cvr)
}




