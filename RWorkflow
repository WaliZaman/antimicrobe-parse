Tests <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_sglisampletests_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)
Samples <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_sglisamples_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)
Antibiotics <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_antibiotics_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)
Pathogens <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_pathogen_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)
Species <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_species_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)
Specimen <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_specimens_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)
SpecimenCat <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_specimen_categories_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)
Sensitivity <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_testsensitivities_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)
TestMethods <- read.csv("D:\\Wali\\amrbd_datbasein_csv\\STATICBAHIS_testmethods_202207251225.csv", header=TRUE, stringsAsFactors=TRUE)

#Cleaning up the tables by getting rid of unneeded columns
TestsCleaned = subset(Tests, select = -c(pathogen_id, created_by, created_at, 
                                                  updated_at))
SamplesCleaned = subset(Samples, select = -c(created_by, created_at, 
                                         updated_at))
AntibioticsCleaned = subset(Antibiotics, select = -c(created_by, created_at, 
                                             updated_at))
PathogensCleaned = subset(Pathogens, select = -c(created_by, created_at, 
                                             updated_at))
SpecimenCleaned = subset(Specimen, select = -c(created_by, created_at, 
                                             updated_at))
SensitivityCleaned = subset(Sensitivity, select = -c(created_by, created_at, 
                                               updated_at))
TestMethodsCleaned = subset(TestMethods, select = -c(created_by, created_at, 
                                               updated_at))


#Merging the tables, so that we have one table containing all the data
TestsUpdated <- merge(x=TestsCleaned, y=SamplesCleaned, by="sample_id", all.x=TRUE)
TestsUpdated_1 <- merge(x=TestsUpdated, y=AntibioticsCleaned, by="antibiotic_id", all.x=TRUE)
TestsUpdated_2 <- merge(x=TestsUpdated_1, y=PathogensCleaned, by="pathogen_id", all.x=TRUE)
TestsUpdated_3 <- merge(x=TestsUpdated_2, y=SpecimenCleaned, by="specimen_id", all.x=TRUE)
TestsUpdated_4 <- merge(x=TestsUpdated_3, y=SpecimenCat, by="specimen_category_id", all.x=TRUE)
TestsUpdated_5 <- merge(x=TestsUpdated_4, y=TestMethodsCleaned, by="test_method_id", all.x=TRUE)
InputTable <- merge(x=TestsUpdated_5, y=SensitivityCleaned, by="test_sensitivity_id", all.x=TRUE)


#Cleaning the InputTable by removing unneeded columns
InputTableCleaned = subset(InputTable, select = -c(test_id, antibiotic_id, pathogen_id, test_method_id,
                                                   specimen_id, specimen_category_id, test_sensitivity_id,
                                                   specimen_alt_names, pathogen_alt_names, antibiotic_alt_names))


#Generating an output table
OutputTable = data.frame(Antibiotic = c(InputTableCleaned$antibiotic_name),
  Pathogen = c(InputTableCleaned$pathogen_name),
  Specimen = c(InputTableCleaned$specimen_name),
  Sensitivity = c(InputTableCleaned$test_sensitivity_type),
  stringsAsFactors = TRUE

)

#Adding empty columns
OutputTable <- OutputTable %>%
  add_column(Count_S = NA)
OutputTable <- OutputTable %>%
  add_column(Count_I = NA)
OutputTable <- OutputTable %>%
  add_column(Count_R = NA)

OutputTableF <- data.frame(
OutputTable1 <- OutputTable %>%
  group_by(Antibiotic, Pathogen, Specimen) %>%
  summarise(Count_S = n_distinct(Sensitivity == 'S')),
  
OutputTable2 <- OutputTable %>%
  group_by(Antibiotic, Pathogen, Specimen) %>%
  summarise(Count_I = n_distinct(Sensitivity == 'I')),

OutputTable3 <- OutputTable %>%
  group_by(Antibiotic, Pathogen, Specimen) %>%
  summarise(Count_R = n_distinct(Sensitivity == 'R'))

)

#Cleaning the output table

FinalOutputTable = subset(OutputTableF, select = -c(Antibiotic.1, Pathogen.1, Specimen.1,
                                                    Antibiotic.2, Pathogen.2, Specimen.2))

View(FinalOutputTable)

#Calculating Total count and adding them to a new column
FinalOutputTable$TotalCount = rowSums(FinalOutputTable[,c("Count_S", "Count_I", "Count_R")])
# Calculating sensitivity as percentage of total count
FinalOutputTable$SensistivityPercent <- FinalOutputTable$Count_S/FinalOutputTable$TotalCount 

#Exporting the Table as an excel file

#Step 1. Separating the Final Output Table into smaller tables according to "Specimen"
for(i in unique(FinalOutputTable$Specimen)) {
  nam <- paste("FinalOutputTable", i, sep = ".")
  assign(nam, FinalOutputTable[FinalOutputTable$Specimen==i,])

}

#Step 2. Exporting as excel file with multiple sheets

SpecimenWiseTable <- list("Ascitic Fluid" = `FinalOutputTable.Ascitic Fluid`,
                          "Aural Swab" = `FinalOutputTable.Aural Swab`,
                          "Aural Swab (Left)" = `FinalOutputTable.Aural Swab (Left)`,
                          "Blood" = FinalOutputTable.Blood,
                          "Bronchial Lavage" = `FinalOutputTable.Bronchial Lavage`,
                          "Conjunctival Swab" = `FinalOutputTable.Conjunctival Swab`,
                          "Discharge" = FinalOutputTable.Discharge,
                          "Ear Swab" = `FinalOutputTable.Ear swab`,
                          "ET-Tube Swab" = `FinalOutputTable.ET-Tube Swab`,
                          "Eye A/C Fluid" = `FinalOutputTable.Eye A/C Fluid`,
                          "Eye Fluid" = `FinalOutputTable.Eye Fluid`,
                          "High Vaginal Swab" = `FinalOutputTable.High Vaginal Swab`,
                          "HVS" = FinalOutputTable.HVS,
                          "Joint Discharge" = `FinalOutputTable.Joint Discharge`,
                          "Nipple Discharge" = `FinalOutputTable.Nipple Discharge`,
                          "Parietal Abcess Fluid" = `FinalOutputTable.Parietal Abscess Fluid`,
                          "Pus" = FinalOutputTable.Pus,
                          "Sputum" = FinalOutputTable.Sputum,
                          "Stool" = FinalOutputTable.Stool,
                          "Synovial Fuid" = `FinalOutputTable.Synovial Fluid`,
                          "Throat Swab" = `FinalOutputTable.Throat Swab`,
                          "Tracheal Aspirate" = `FinalOutputTable.Tracheal Aspirate`
                          
)

write.xlsx(SpecimenWiseTable, file = "D:\\Wali\\amrbd_datbasein_csv\\Specimen Wise Table.xlsx")
