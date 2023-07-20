#' Establish correct frames for all snippets in a video
#'
#' This function establishes the correct frames for all snippets in a video. It works only with event IDs that have all snippets processed.
#'
#' @param data A data frame containing the video data with columns: event_ID, snippet, frame.
#' @return A data frame with an additional column "correct_frame" indicating the correct frames for all snippets.
#' @export
corrected_frames <- function(data) {
  # Arrange the data by event_ID, snippet, and frame and count the occurrences
  frames <- data %>% 
    dplyr::arrange(event_ID, snippet, frame) %>%
    dplyr::group_by(event_ID, snippet, frame) %>% dplyr::count() %>% dplyr::select(event_ID, snippet, frame)
  
  # Group the frames by event_ID and assign correct_frame numbers
  frames2 <- frames %>%
    dplyr::group_by(event_ID) %>%
    dplyr::mutate(correct_frame = dplyr::row_number())
  
  # Join the corrected frames with the original data and arrange by event_ID and correct_frame
  data2 <- dplyr::left_join(data, frames2, by = c("event_ID", "snippet", "frame")) %>%
    dplyr::arrange(event_ID, correct_frame)
  
  # Return the corrected data
  return(data2)
}

#' Read and join Parquet files from a specified directory path
#'
#' This function reads and joins Conventionally named Parquet files from a specified directory path.
#'
#' @param directory_path A character string specifying the directory path where the Parquet files are located.
#' @return A data frame containing the merged data from the Parquet files.
#' @export
read_and_join_conv_parquet_files <- function(directory_path) {
  # Check if the directory path exists
  if (!file.exists(directory_path)) {
    stop("Directory path does not exist.")
  }
  
  # Get Parquet files from the directory path
  parquet_files <- list.files(path = directory_path, pattern = "\\.parquet$", full.names = TRUE)
  
  # Filter Parquet files without "non-conv" in the filename
  parquet_files <- parquet_files[!grepl("non-conv", parquet_files)]
  
  # Check if Parquet files without "non-conv" exist in the directory
  if (length(parquet_files) == 0) {
    stop("No Parquet files without 'non-conv' found in the specified directory.")
  }
  
  # Read Parquet files
  data <- lapply(parquet_files, read_parquet)
  
  # Combine the data frames
  merged_data <- dplyr::bind_rows(data)
  
  # Return the merged data
  return(merged_data)
}

#' Read and Join Non-Conv Parquet Files
#'
#' This function reads Parquet files with the "non-conv" string in their names
#' from a specified directory path, modifies specific vectors in the data frames,
#' and combines them into a single merged dataframe.
#'
#' @param directory_path The path to the directory containing the Parquet files.
#'
#' @return A merged dataframe containing the data from the Parquet files with the
#' "non-conv" string in their names, where specific vectors have been modified as follows:
#' - event_ID: converted to character
#' - tank_num: converted to character
#' - night_num: converted to character
#' - category_id: converted to character
#' - frame: converted to integer
#'
#' @export
#'
#' @examples
#' read_and_join_nonconv_parquet_files("data/parquet_files")
read_and_join_nonconv_parquet_files <- function(directory_path) {
  # Check if the directory path exists
  if (!file.exists(directory_path)) {
    stop("Directory path does not exist.")
  }
  
  # Get Parquet files with "non-conv" from the directory path
  parquet_files <- list.files(path = directory_path, pattern = "non-conv", full.names = TRUE)
  
  # Check if Parquet files with "non-conv" exist in the directory
  if (length(parquet_files) == 0) {
    stop("No Parquet files with 'non-conv' found in the specified directory.")
  }
  
  # Read Parquet files and modify vectors
  modified_data <- lapply(parquet_files, function(file) {
    df <- read_parquet(file)
    
    # Modify specific vectors
    if ("event_ID" %in% names(df)) {
      df$event_ID <- as.character(df$event_ID)
    }
    if ("tank_num" %in% names(df)) {
      df$tank_num <- as.character(df$tank_num)
    }
    if ("night_num" %in% names(df)) {
      df$night_num <- as.character(df$night_num)
    }
    if ("category_id" %in% names(df)) {
      df$category_id <- as.character(df$category_id)
    }
    if ("frame" %in% names(df)) {
      df$frame <- as.integer(df$frame)
    }
    
    return(df)
  })
  
  # Combine the modified data frames
  merged_data <- dplyr::bind_rows(modified_data)
  
  # Return the merged data
  return(merged_data)
}

#' Read and Join All Parquet Files
#'
#' This function reads and combines all Parquet files, both conv and non-conv, from
#' a specified directory path. It uses the functions `read_and_join_conv_parquet_files`
#' and `read_and_join_nonconv_parquet_files` to read and modify the Parquet files,
#' respectively. The resulting data frames are then combined into a single merged dataframe.
#'
#' @param directory_path The path to the directory containing the Parquet files.
#' #'
#' @return A merged dataframe containing the data from all Parquet files in the specified
#' directory, where specific vectors in the non-conv files have been modified as follows:
#' - event_ID: converted to character
#' - tank_num: converted to character
#' - night_num: converted to character
#' - category_id: converted to character
#' - frame: converted to integer
#'
#' @export
#'
#' @examples
#' read_and_join_all_parquet_files("data/parquet_files")
read_and_join_all_parquet_files<- function(directory_path){
    d1 <- read_and_join_conv_parquet_files(directory_path)
    d2<- read_and_join_nonconv_parquet_files(directory_path)

    d3 <- dplyr::bind_rows(d1,d2)

    return(d3)
}

#' Calculate Intersection over Union (IoU) between two bounding boxes
#'
#' This function calculates the Intersection over Union (IoU) between two
#' bounding boxes represented by their center coordinates.
#'
#' @param cx1 The x-coordinate of the center of the first bounding box.
#' @param cy1 The y-coordinate of the center of the first bounding box.
#' @param cx2 The x-coordinate of the center of the second bounding box.
#' @param cy2 The y-coordinate of the center of the second bounding box.
#'
#' @return The Intersection over Union (IoU) value between the two bounding boxes.
#'
#' @examples
#' # Calculate IoU between two bounding boxes
#' iou_value <- calculate_iou(1, 2, 3, 4)
#' print(iou_value)
#'
calculate_iou <- function(cx1, cy1, cx2, cy2) {
  # Calculate the width and height of the intersection between the bounding boxes
  intersection_width <- min(cx1 + 0.5, cx2 + 0.5) - max(cx1 - 0.5, cx2 - 0.5)
  intersection_height <- min(cy1 + 0.5, cy2 + 0.5) - max(cy1 - 0.5, cy2 - 0.5)
  
  # Calculate the area of intersection and union between the bounding boxes
  intersection_area <- max(0, intersection_width) * max(0, intersection_height)
  union_area <- ((1 + abs(cx1 - cx2)) * (1 + abs(cy1 - cy2))) - intersection_area
  
  # Calculate and return the IoU value
  iou <- intersection_area / union_area
  return(iou)
}

#' Filter data based on confidence threshold for Centro and Helio species
#'
#' This function filters the input dataframe based on the specified confidence threshold for Centro and Helio species.
#'
#' @param data The input dataframe to be filtered.
#' @param groundtruth_dir The directory path containing the groundtruth CSV file that indicates which urchin species was added for each event_ID
#' @param confidence_threshold The confidence threshold value for both Centro and Helio species.
#'
#' @return The filtered dataframe based on the confidence threshold for Centro and Helio species.
#'
#' @examples
#' # Filter data based on confidence threshold
#' filtered_data <- confidence_filter(data, 'data/groundtruth', 0.75)
#' print(filtered_data)
#'
confidence_filter <- function(data, groundtruth_dir, confidence_threshold) {
  
  # Load groundtruth data
  groundtruth_data <- read.csv(groundtruth_dir)
  
  # Left join data with groundtruth
  merged_data <- left_join(data, groundtruth_data, by = 'event_ID')
  
  # Filter data by species and confidence threshold
  filtered_data <- merged_data %>%
    filter((Species == 'Centro' & conf >= confidence_threshold) |
             (Species == 'Helio' & conf >= confidence_threshold))
  
  return(filtered_data)
}

#groundtruth_dir <- 'data/Match-event-with-urchin-species.csv'

#t1 <- confidence_filter(data, groundtruth_dir , 0.60)

#hist(t1$conf)


#' Calculate count of a specific category_id for each event_ID and correct_frame
#'
#' This function calculates the count of a specific category_id for each event_ID and correct_frame in the input dataframe.
#'
#' @param data The input dataframe with columns: event_ID, correct_frame (from corrected_frames), category_id and Species (from confidence_filter)
#' @param category_id The category ID to count. 0 of urchin 2 for lobster
#'
#' @return A dataframe with event_ID, correct_frame, Species, and count_category columns.
#'
#' @examples
#' # Calculate count of a specific category_id
#' count_data <- calculate_class_count(data, "0")
#' print(count_data)
#'
calculate_class_count <- function(data, category_id) {  
  count_data <- data %>%
    group_by(event_ID, correct_frame, Species) %>%
    summarise(count_category = sum(category_id == category_id), .groups = "drop")
  
  return(count_data)
}

#t2 <- calculate_class_count(t1, "0")

#' Summarize data by frame group
#'
#' This function groups the input dataframe by a specified frame group, calculates the mean count of a specific category ID,
#' and rounds the mean count.
#'
#' @param data The input dataframe.
#' @param category_id The category ID to calculate the mean count.
#' @param frame_group The numerical value to group the data by chunks of frames.
#'
#' @return A dataframe with event_ID, correct_frame_group, Species, and mean_count columns.
#'
#' @examples
#' # Summarize data by frame group
#' summarized_data <- summarize_data_by_frame_group(data, "0", 200)
#' print(summarized_data)
#'
summarize_data_by_frame_group <- function(data, category_id, frame_group) {
  library(dplyr)
  
  summarized_data <- data %>%
    mutate(correct_frame_group = floor(correct_frame / frame_group) * frame_group) %>%
    group_by(event_ID, correct_frame_group, Species) %>%
    summarise(mean_count = mean(count_category), .groups = "drop") %>%
    mutate(mean_count = round(mean_count, 0))
  
  return(summarized_data)
}

#t3 <- summarize_data_by_frame_group(t2, "0", 200)


#' Visualize counts and create plots
#'
#' This function generates plots to visualize the counts and saves them to the specified output directory.
#'
#' @param data The input dataframe.
#' @param true_counts_dir The directory where the dataframe containing the true start and end counts for urchins is
#' 
#' @param output_dir The directory path to save the plots.
#'
#' @return A dataframe containing when the difference between the detected and true start and end counts
#'
#' @examples
#' # Generate and save plots
#' visualize_counts(data, true_counts, "outputs/")
#'
discarded_counts_plots <- function(data, confidence_threshold, frame_group, true_counts_dir, output_dir) {
  # Find the frame_group where the mean_count of urchin is the same as the true_start and true_end
  true_counts <- read.csv(true_counts_dir)
  count_checks <- data %>%
    left_join(true_counts, by = "event_ID") %>%
    group_by(event_ID) %>%
    mutate(
      true_start_frame = correct_frame_group[which(mean_count == True_start)[1]],
      frames_before_start = true_start_frame - min(correct_frame_group),
      true_end_frame = if_else(any(mean_count == True_end), max(correct_frame_group[mean_count == True_end], na.rm = TRUE), NA_integer_),
      frames_left = if_else(is.na(true_end_frame), NA_integer_, max(correct_frame_group) - true_end_frame),
      frames_left_percentage = frames_left / max(correct_frame_group) * 100,
      frames_before_start_percentage = frames_before_start / max(correct_frame_group) * 100
    ) %>%
    ungroup() %>%
    select(event_ID, True_start, true_start_frame, frames_before_start_percentage, True_end, true_end_frame, frames_left_percentage) %>%
    distinct() %>%
    mutate(conf = confidence_threshold, frame_grouping = frame_group)
  
  # Visualize how much of the data may be discarded at the start and the end
  discard_start <- count_checks %>%
    mutate(chunk = cut(frames_before_start_percentage, breaks = seq(0, 100, by = 10), labels = FALSE))
  
  # Create the heatmap for the discarded at the start
  start_plot <- ggplot(discard_start, aes(x = chunk, y = event_ID, fill = frames_before_start_percentage)) +
    geom_tile() +
    scale_fill_gradient(low = "#cf3939", high = "#42bc42") +
    labs(x = "Percentage Chunk", y = "Event ID", fill = "Percentage of Frames Before True Start") +
    theme_minimal()
  
  # Calculate the chunk numbers for each percentage range
  discard_end <- count_checks %>%
    mutate(chunk = cut(frames_left_percentage, breaks = seq(0, 100, by = 10), labels = FALSE))
  
  # Create the heatmap for frames_left_percentage
  end_plot <- ggplot(discard_end, aes(x = chunk, y = event_ID, fill = frames_left_percentage)) +
    geom_tile() +
    scale_fill_gradient(low = "#cf3939", high = "#42bc42") +
    labs(x = "Percentage Chunk", y = "Event ID", fill = "Percentage of Frames After True End") +
    theme_minimal()
  
  # Save the plots
  ggsave(paste0(output_dir, "confidence_", confidence_threshold, "_framegroup_", frame_group, "_percentage_frames_before_true_start_end.jpg"), start_plot + end_plot, width = 15)

  return(count_checks)
}


#' Generate and save plots for counts based on confidence threshold and frame group
#'
#' This function generates and saves plots for counts based on the specified confidence threshold and frame group.
#'
#' @param data The input dataframe.
#' @param groundtruth_dir The directory path containing the groundtruth CSV file containing the species for each event_ID
#' @param confidence_threshold The confidence threshold value for filtering the data.
#' @param category_id The category ID to calculate the count. 0 urchin lobster 2
#' @param frame_group The numerical value to group the data by chunks of frames.
#' @param true_counts The dataframe containing the true start and end counts for urchins.
#' @param output_dir The directory path to save the plots and count checks.
#'
#' @return None
#'
#' @examples
#' # Generate and save plots for counts
#' plots_counts_confidence(data, "data/groundtruth", 0.75, "0", 200, true_counts, "outputs/")
#'
plots_counts_confidence <- function(data, groundtruth_dir, confidence_threshold, category_id, frame_group, true_counts, output_dir) {
  # Filter data based on confidence threshold
  d1 <- confidence_filter(data, groundtruth_dir, confidence_threshold)
  
  # Calculate count of a specific category_id
  d2 <- calculate_class_count(d1, category_id)
  
  # Summarize data by frame group
  d3 <- summarize_data_by_frame_group(d2, category_id, frame_group)
  
  # Generate and save plots for discarded counts
  d4<-discarded_counts_plots(d3, confidence_threshold, frame_group, true_counts, output_dir)
  
  # Save count checks to CSV
  write.csv(d4, paste0(output_dir, "confidence_", confidence_threshold, "_framegroup_", frame_group, "_count_checks.csv"))
}



#' Generate combinations of confidence threshold and frame group for filtering
#'
#' This function generates combinations of confidence threshold and frame group for filtering.
#'
#' @param confidence_threshold A logical value indicating whether to include confidence threshold in the combinations.
#' @param frame_group A logical value indicating whether to include frame group in the combinations.
#'
#' @return A dataframe with combinations of confidence threshold and frame group.
#'
#' @examples
#' # Generate combinations of confidence threshold and frame group
#' combos <- conf_frame_filter_combos(TRUE, TRUE)
#' print(combos)
#'
conf_frame_filter_combos <- function(confidence_threshold = TRUE, frame_group = TRUE) {

  if (confidence_threshold || frame_group) {
    combos <- expand_grid(
      confidence_threshold = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
      frame_group = c(200, 500, 1000, 1500, 2000, 2500, 3000, 5000)
    )
  }
  
  return(combos)
}
