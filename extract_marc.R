library(xml2)
library(dplyr)
library(tidyverse)



extract_marc21_data <- function(tag,xml_file) {
  # Read the XML file
  doc <- read_xml(xml_file)
  
  # Extract data from all fields with the specified tag
  fields <- xml_find_all(doc, paste0("//datafield[@tag='", tag, "']"))
  
  # Create a list of data frames for each field
  field_data <- lapply(fields, function(field) {
    record_id <- xml_text(xml_find_first(field, ".//../controlfield[@tag='001']"))
    
    # Extract all subfields within the field
    subfield_elements <- xml_children(field)
    subfields <- setNames(
      sapply(subfield_elements, function(subfield) xml_text(subfield)),
      sapply(subfield_elements, function(subfield) xml_attr(subfield, "code"))
    )
    
    # Combine subfields into a single data frame
    subfields <- as.data.frame(t(subfields), stringsAsFactors = FALSE)
    colnames(subfields) <- paste0(tag, "_", colnames(subfields))
    
    # Add the RecordID to the data frame
    subfields$RecordID <- record_id
    
    return(subfields)
  })
  
  # Combine the list of data frames into a single dataframe
  df <- bind_rows(field_data)
  
  return(df)
}



