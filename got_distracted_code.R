

analyze_cam_dat <- function(video_folder = NULL, threshold_value = 1000, cores = 6,
                            crop_height = 100, sensitivity = 10,
                            pattern = "DSCF",
                            move_files = FALSE){

packages <- c("av", "magick", "tidyverse", "future",
              "doFuture","fs")

install.packages(setdiff(packages, rownames(installed.packages())))  

invisible(lapply(packages,require,character.only = TRUE))

rm(packages)

cores <- cores

sensitivity = sensitivity

threshold_value <- threshold_value

crop_height <- crop_height

videos <- list.files(video_folder,full.names = T, pattern = pattern)

found_movement <- NULL
season_t_start <- Sys.time()

plan(multisession,workers = cores)

# Initialize an empty dataframe to store the results


found_movement <- foreach(v = videos,.combine = rbind, 
                          .options.future = list(globals = structure(TRUE, 
                                  add = c("threshold_value", 
                                          "crop_height",
                                          "sensitivity")))) %dofuture% {

  found_movement <- data.frame(video_name = character(), movement = logical(), stringsAsFactors = FALSE)
  
video <- v

video_name <- str_split(video, pattern = "/")[[1]][[2]]

unique_frames_dir <- file.path("frames", paste0("frames_", video_name))

dir_create(unique_frames_dir)

# Extract frames from the video every 1 second
suppressWarnings(av_video_images(video = video, destdir = unique_frames_dir, format = "png", fps = 1))

# Define a function to crop the bottom of the image
crop_bottom <- function(image, crop_height) {
  # Get image dimensions
  img_info <- image_info(image)
  width <- img_info$width
  height <- img_info$height
  
  # Define the area to keep (x, y, width, height)
  area_to_keep <- sprintf("%dx%d+0+0", width, height - crop_height)
  
  # Crop the image
  cropped_image <- image_crop(image, area_to_keep)
  return(cropped_image)
}

# Set the height to crop from the bottom (in pixels)
crop_height <- crop_height  # Adjust this value based on your needs

# Read the frames and crop them
frames <- suppressWarnings(list.files(unique_frames_dir, pattern = "*.png", full.names = TRUE))
images <- lapply(frames, function(frame) {
  img <- image_read(frame)      # Read the image
  crop_bottom(img, crop_height) # Crop the image
})

# Compare consecutive frames for movement
movement_detection <- function(img1, img2) {
  diff <- image_compare(img2, img1, metric = "AE", fuzz = 50)  # Absolute error
  
  return(diff)
}

# Loop through consecutive frames to detect movement

differences <- lapply(1:(length(images)-1), function(i) {
  image_name <- frames[[i]]
  
  diff <- image_compare(images[[i+1]], images[[i]], metric = "AE", fuzz = sensitivity)
  value <- attributes(diff)$distortion
  # Return a data frame row with image name and value
  data.frame(video_name = video_name, image_name = image_name, value = value)
  
})

differences <- do.call(rbind,differences)

# Define a threshold for mean pixel difference to indicate movement
threshold_value <- threshold_value  # Adjust based on your use case

# Check if the mean pixel difference exceeds the threshold
movement_detected <- differences %>% mutate(movement = ifelse(value > threshold_value,TRUE,FALSE))

if (any(movement_detected$movement)) {
  # If there is at least one TRUE, do something
  new_row <-  movement_detected %>%
    filter(movement == TRUE) %>%
    select(video_name, movement) %>%
    distinct(video_name, movement)
} else {
  # If there are no TRUE values
  new_row <-  movement_detected %>%
    filter(movement == FALSE) %>%
    select(video_name, movement) %>% 
    distinct(video_name, movement)}


found_movement <- bind_rows(found_movement, new_row)

return(found_movement)

dir_delete(unique_frames_dir)


}

plan(sequential)

#print(movement_detected)
if(move_files == TRUE){
# Define the source directory (where the original videos are located)
source_dir <- "videos/"

# Define the destination directories within the "videos" folder
true_dir <- file.path(source_dir, "movement_detected/")
false_dir <- file.path(source_dir, "no_movement/")

# Create destination directories if they do not exist
if (!dir.exists(true_dir)) dir.create(true_dir)
if (!dir.exists(false_dir)) dir.create(false_dir)

# Loop through the dataframe and move files based on movement values
suppressMessages({
  lapply(1:nrow(found_movement), function(i) {
    video_name <- found_movement$video_name[i]
    movement_value <- found_movement$movement[i]
    
    # Construct the full path for the source file
    source_file <- file.path(source_dir, basename(video_name))
    
    # Move the file to the corresponding directory based on the movement value
    if (movement_value) {
      destination_file <- file.path(true_dir, basename(video_name))
      file_move(source_file, destination_file)
    } else {
      destination_file <- file.path(false_dir, basename(video_name))
      file_move(source_file, destination_file)
    }
  })
})

# Optional: Print a message after moving the files
cat("Files have been moved based on movement detection.\n")} else {
  cat("No files moved.\n")
}

season_t_run <-  Sys.time() - season_t_start
print(season_t_run)

return(found_movement)


}

#TESTING
# diff <- image_compare(images[[7]], images[[6]], metric = "AE", fuzz = 10)
# image_ggplot(diff)
