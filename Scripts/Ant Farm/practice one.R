library(stringr)
> 
  > # Read the 'ImageJ' sheet from your Excel file
  > df <- read_excel("C:/Users/PC1/Downloads/ImageJ_Analysis (1).xlsx", sheet = "ImageJ")
  
  library(stringr)
  > 
    > # Read the 'ImageJ' sheet from your Excel file
    > df <- read_excel("C:/Users/PC1/Downloads/ImageJ_Analysis (1).xlsx", sheet = "ImageJ")
    New names:
      • `` -> `...13`
    • `` -> `...14`
    • `` -> `...20`
    > 
      > # Filter out rows where Image_ID is missing
      > df_cleaned <- df %>%
      +     filter(!is.na(Image_ID)) %>%
      +     mutate(
        +         # Normalize Image_ID (e.g., turn 't_336_2A' into 't336_2A')
          +         Image_ID = ifelse(str_count(Image_ID, "_") == 2,
                                      +                           str_replace(Image_ID, "^([a-zA-Z]+)_([0-9]+)_", "\\1\\2_"),
                                      +                           Image_ID),
        +         
          +         # Split into two new columns
          +         Image_ID_clean = str_extract(Image_ID, "^[^_]+"),       # before _
        +         Tank_Number    = str_extract(Image_ID, "(?<=_)[^_]+")   # after _
        +     )
    > 
      > # View the first few rows
      > head(df_cleaned %>% select(Image_ID, Image_ID_clean, Tank_Number))
    