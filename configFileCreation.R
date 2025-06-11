



#selecting videos for emotional video task 
library(tidyverse)
vidStats = read.csv('G:\\My Drive\\GitHub\\EmotionalMovieTask\\DEVO_vidStats.csv')


vidStats = vidStats %>% filter(!is.na(ValenceAvgRating))
vidStats = vidStats %>% filter(!is.na(ArousalAvgRating))

vidStats %>% ggplot(aes(x = ValenceAvgRating, y = ArousalAvgRating)) + 
  geom_point()


sum(vidStats$ValenceAvgRating>7)

vidStats$highArousal_lowVal = F

idx = vidStats$ValenceAvgRating>4 & 
      vidStats$ValenceAvgRating<6 &
      vidStats$ArousalAvgRating>5
vidStats$highArousal_lowVal[idx] = T

vidStats %>% ggplot(aes(x = ValenceStandardDeviation, 
                        after_stat(density),
                        group = highArousal_lowVal,
                        color = highArousal_lowVal)) + 
  geom_freqpoly()


vidStats$MF_dif = (vidStats$Valenceavgfemalerating - 
                  as.numeric(vidStats$Valenceavgmalerating)) / 
                  (vidStats$Valenceavgfemalerating + 
                  as.numeric(vidStats$Valenceavgmalerating))

vidStats = arrange(vidStats, MF_dif)

#aiming for three groups: 
#1: low valence (bad), high arousal 
#2: neutral valence, low arousal 
#3: high valence (good), high arousal
n = 180 #number of videos 
configFile = data.frame(fileName = rep('a', n), 
                        vidLen = rep(0, n), #seconds
                        valence = rep(0,n), 
                        arousal = rep(0,n), 
                        impact = rep(0,n),
                        group = rep(0,n))

#120 for each condition allows for future new condition file for same subject
#I'll get the 120 highest (most positive) valence videos
#then match the negatives 


#happy videos are less arousing, so try to get at least somewhat arousing vids
tmp = vidStats[vidStats$ArousalAvgRating > 4, ]

tmp = arrange(tmp, ValenceAvgRating)
happyVids = tmp[(dim(tmp)[1]-119):dim(tmp)[1],]

maxHapA = max(happyVids$ArousalAvgRating)
minHapA = min(happyVids$ArousalAvgRating)

#get sad videos:
#going to use the maximum worst videos: 
tmp = vidStats #[vidStats$ArousalAvgRating > minHapA &
               #vidStats$ArousalAvgRating < maxHapA, ]                       

tmp = arrange(tmp, ValenceAvgRating)
sadVids = tmp[1:120,]


minHapV = min(happyVids$ValenceAvgRating)
maxSadV = max(sadVids$ValenceAvgRating)


#neutral videos: 
tmp = vidStats[vidStats$ArousalAvgRating < 4 &
               vidStats$ValenceAvgRating < minHapV  & 
               vidStats$ValenceAvgRating > maxSadV, ]
#neutral could be because it's divisive or actually neutral, 
#get low standard deviation vids for high agreement
tmp = arrange(tmp, ValenceStandardDeviation)

neutVids = tmp[1:120,]


#set seed for replicable randomization
set.seed(12345)

neutVids = neutVids[sample(1:120, 120),]
happyVids = happyVids[sample(1:120, 120),]
sadVids = sadVids[sample(1:120, 120),]

set1 = sample(1:120, 60)
set2 = 1:120
set2[set1] = NA
set2 = set2[!is.na(set2)]


configFile$fileName[1:60] = neutVids$Videos10repeatsinarow[set1]
configFile$vidLen[1:60] = neutVids$Lengthofmoviesec[set1]
configFile$valence[1:60] = neutVids$ValenceAvgRating[set1]
configFile$arousal[1:60] = neutVids$ArousalAvgRating[set1]
configFile$impact[1:60] = neutVids$ImpactAvgRating[set1]
configFile$group[1:60] = 'neut'

configFile$fileName[61:120] = happyVids$Videos10repeatsinarow[set1]
configFile$vidLen[61:120] = happyVids$Lengthofmoviesec[set1]
configFile$valence[61:120] = happyVids$ValenceAvgRating[set1]
configFile$arousal[61:120] = happyVids$ArousalAvgRating[set1]
configFile$impact[61:120] = happyVids$ImpactAvgRating[set1]
configFile$group[61:120] = 'happy'

configFile$fileName[121:180] = sadVids$Videos10repeatsinarow[set1]
configFile$vidLen[121:180] = sadVids$Lengthofmoviesec[set1]
configFile$valence[121:180] = sadVids$ValenceAvgRating[set1]
configFile$arousal[121:180] = sadVids$ArousalAvgRating[set1]
configFile$impact[121:180] = sadVids$ImpactAvgRating[set1]
configFile$group[121:180] = 'sad'

write.csv( configFile, 'G:\\My Drive\\GitHub\\EmotionalMovieTask\\configFile1.csv')

configFile %>% ggplot(aes(x = valence, y = arousal, color = group)) + geom_point()
#copy the target files: 
source_folder <- "E:\\mp4 (without sound)"      # Folder where original files are
target_folder <- "G:\\My Drive\\GitHub\\EmotionalMovieTask\\resources\\vids"      # Folder where files will be copied

# Create the target folder if it doesn't exist
if (!dir.exists(target_folder)) {
  dir.create(target_folder, recursive = TRUE)
}

# Read the CSV file
file_data <- configFile

# Sanity check: does 'fileName' column exist?
if (!"fileName" %in% names(file_data)) {
  stop("The CSV file does not contain a column named 'fileName'.")
}



library(stringr)

# Define your overrides as a named vector
manual_fixes <- c(
  "546.4-MeToo_Is_Changing_How_Sex_Is_Simulated_on_Set-ED.mp4" = "546.4-#MeToo_Is_Changing_How_Sex_Is_Simulated_on_Set-ED.mp4",
  "470.4-Shark_Bite_GoPro_DiveFilm_HD_Podcast-ED.mp4" = "470.4-Shark_Bite_GoPro!_DiveFilm_HD_Podcast-ED.mp4",
  "650-Womans_Hands_Typing-ED.mp4" = "650-Woman's Hands Typing-ED.mp4",
  "327-New_Year_Eve_Party-ED.mp4" = "327-New Year's Eve Party-ED.mp4",
  "668.4-Most_HEARTWARMING_Military_Homecoming_Surprises-ED.mp4" = "668.4-Most_HEARTWARMING_Military_Homecoming_Surprises!-ED.mp4",
  "601.1_-_Woman_Runs_Over_Boyfriend_With_Car_After_Learning_Hes_HIV_Positive_-_ED_1.mp4" = "601.1 - Woman Runs Over Boyfriend With Car After Learning He's HIV Positive - ED 1.mp4",
  "526.3-Monster_EXPLICIT-ED2.mp4" = "526.3-Monster_(EXPLICIT)-ED2.mp4",
  "600.3_-_Injecting_Speedball_-_Crack__Heroin-_ED.mp4" = "600.3_-_Injecting_Speedball_-_Crack_&_Heroin-_ED.mp4",
  "629.3-Las_Vegas_Shooting_Watch_Cops_Body_Cam_of_Dramatic_Chase-ED.mp4" = "629.3-Las_Vegas_Shooting_Watch_Cop's_Body_Cam_of_Dramatic_Chase-ED.mp4",
  "600.4_-_Injecting_Speedball_-_Crack__Heroin-_ED.mp4" = "600.4_-_Injecting_Speedball_-_Crack_&_Heroin-_ED.mp4",
  "696.3-Eggnog_Gallon_Challenge_WARNING_Projectile_Vomiting-ED.mp4" = "696.3-Eggnog_Gallon_Challenge_WARNING!_Projectile_Vomiting-ED.mp4",
  "428.4-Police_Academy_6_featuring_Raw_Moans_-_The_Chills-ED.mp4" = "428.4-Police_Academy_6_(featuring_Raw_Moans)_-_The_Chills-ED.mp4"
)


# Iterate and copy files
ii = 1
for (file in file_data$fileName) {
  
  # #hard codes for files names that didn't work: 
  # if(file == '546.4-MeToo Is Changing How Sex Is Simulated on Set-ED.mp4'){
  #   configFile$fileName[ii] = '546.4-#MeToo_Is_Changing_How_Sex_Is_Simulated_on_Set-ED.mp4'
  #   file = configFile$fileName[ii]
  # }
  # if(file == '470.4-Shark Bite GoPro DiveFilm HD Podcast-ED.mp4'){
  #   configFile$fileName[ii] = '470.4-Shark_Bite_GoPro!_DiveFilm_HD_Podcast-ED.mp4'
  #   file = configFile$fileName[ii]
  # }
  # if(file == '650-Womans Hands Typing-ED.mp4'){
  #   configFile$fileName[ii] = "650-Woman's Hands Typing-ED.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '327-New Year Eve Party-ED.mp4'){
  #   configFile$fileName[ii] = "327-New Year's Eve Party-ED.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '668.4-Most HEARTWARMING Military Homecoming Surprises-ED.mp4'){
  #   configFile$fileName[ii] = "668.4-Most_HEARTWARMING_Military_Homecoming_Surprises!-ED.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '601.1 - Woman Runs Over Boyfriend With Car After Learning Hes HIV Positive - ED 1.mp4'){
  #   configFile$fileName[ii] = "601.1 - Woman Runs Over Boyfriend With Car After Learning He's HIV Positive - ED 1.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '526.3-Monster EXPLICIT-ED2.mp4'){
  #   configFile$fileName[ii] = "526.3-Monster_(EXPLICIT)-ED2.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '600.3 - Injecting Speedball - Crack  Heroin- ED.mp4'){
  #   configFile$fileName[ii] = "600.3_-_Injecting_Speedball_-_Crack_&_Heroin-_ED.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '629.3-Las Vegas Shooting Watch Cops Body Cam of Dramatic Chase-ED.mp4'){
  #   configFile$fileName[ii] = "629.3-Las_Vegas_Shooting_Watch_Cop's_Body_Cam_of_Dramatic_Chase-ED.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '600.4 - Injecting Speedball - Crack  Heroin- ED.mp4'){
  #   configFile$fileName[ii] = "600.4_-_Injecting_Speedball_-_Crack_&_Heroin-_ED.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '696.3-Eggnog Gallon Challenge WARNING Projectile Vomiting-ED.mp4'){
  #   configFile$fileName[ii] = "696.3-Eggnog_Gallon_Challenge_WARNING!_Projectile_Vomiting-ED.mp4"
  #   file = configFile$fileName[ii]
  # }
  # if(file == '428.4-Police Academy 6 featuring Raw Moans - The Chills-ED.mp4'){
  #   configFile$fileName[ii] = "428.4-Police_Academy_6_(featuring_Raw_Moans)_-_The_Chills-ED.mp4"
  #   file = configFile$fileName[ii]
  # }
  file_clean <- str_squish(file)  # removes extra spaces and trims
  file_clean <- iconv(file_clean, to = "ASCII//TRANSLIT")
  if (file_clean %in% names(manual_fixes)) {
    print(paste("Manual fix triggered for:", file_clean))
    corrected <- manual_fixes[[file]]
    configFile$fileName[ii] <- corrected
    file <- corrected
  }
  
  
  source_file <- file.path(source_folder, file)
  target_file <- file.path(target_folder, file)
  
  if (file.exists(source_file)) {
    file.copy(from = source_file, to = target_file, overwrite = TRUE)
  } else {
    file <- str_replace_all(file, "_", " ")
    source_file <- file.path(source_folder, file)
    if (file.exists(source_file)) {
      target_file <- file.path(target_folder, file)
      configFile$fileName[ii] = file
      file.copy(from = source_file, to = target_file, overwrite = TRUE)
    } else { 
      warning(paste("File not found:", source_file))
      print(ii)
    }
    
    # warning(paste("File not found:", source_file))
  }
  ii = ii + 1
}

cat("File copy process complete.\n")

write.csv( configFile, 'G:\\My Drive\\GitHub\\EmotionalMovieTask\\configFile1.csv')





