healthIndicatorList <- function(option='core'){
  # Return the list of health indicators for constructing 'selectInput'
  # option:  core is it is just the letter codes, full if it is letter codes and descriptors
  if(option=='full'){
    full_indic <- c("Antenatal care coverage - at least one visit (2/3 years) (%)" = "anc1",
                    "Antenatal care coverage - at least one visit (5 years) (%)" = "anc15",
                    "Antenatal care coverage - at least four visits (2/3 years) (%)" = "anc4",
                    "Antenatal care coverage - at least four visits (5 years) (%)" = "anc45",
#                     "Antibiotic treatment in children with ARI symptoms (%)" = "antip",
#                     "Adolescent fertility rate (per 1000 girls aged 15-19 years)" = "asfr1",
                    "BCG immunization coverage among 1-year-olds (%)" = "bcgv",
                    "Children (<5 years) with ARI symptoms taken to facility (%)" = "carep",
#                     "Co-coverage (less than 3 interventions)" = "cc31",
#                     "Co-coverage (less than 3 interventions)" = "cc34",
#                     "Co-coverage (6 or more interventions)" = "cc61",
#                     "Co-coverage (6 or more interventions)" = "cc64",
#                     "Composite coverage index" = "cci",
#                     "Child mortality rate (per 1000 live births)" = "cmr",
                    "Contraceptive prevalence - modern methods (%)" = "cpmo",
                    "Contraceptive prevalence - modern and traditional methods (%)" = "cpmt",
                    "Births by caesarean section (2/3 years) (%)" = "csection",
                    "Births by caesarean section (5 years) (%)" = "csection5",
                    "DTP3 immunization coverage among 1-year-olds (%)" = "dptv",
                    "Early initiation of breastfeeding (2/3 years) (%)" = "ebreast",
                    "Early initiation of breastfeeding (5 years) (%)" = "ebreast5",
                    "Family planning needs satisfied (%)" = "fps",
                    "Full immunization coverage among 1-year-olds (%)" = "fullv",
#                     "Hib immunization coverage among 1-year-olds (%)" = "hib3v",
#                     "Infant mortality rate (deaths per 1000 live births)" = "imr",
#                     "Intermittent preventive treatment for malaria during pregnancy (%)" = "iptp",
                    "Children (<5 years) sleeping under insecticide-treated nets (%)" = "itnch",
                    "Pregnant women sleeping under insecticide-treated nets (%)" = "itnwm",
                    "Measles immunization coverage among 1-year-olds (%)" = "mslv",
#                     "Neonatal mortality rate (deaths per 1000 live births)" = "nmr",
#                     "Prevalence of obesity in non-pregnant women (15-49 years) (%)" = "obesewm",
#                     "Children (<5 years) with diarrhoea receiving ORS (%)" = "ors",
                    "Children (<5 years) with diarrhoea receiving ORT and continued feeding (%)" = "ort",
#                     "Children (<3 years) overweight (%)" = "overwgt3",
#                     "Children (<5 years) overweight (%)" = "overwgt5",
#                     "Postnatal care for babies (%)" = "pncall",
#                     "Postnatal care for babies born outside a health facility (%)" = "pnchome",
#                     "Postnatal care for women (%)" = "pncwm",
#                     "Postnatal mortality rate (per 1000 live births)" = "pnmr",
                    "Polio immunization coverage among 1-year-olds (%)" = "poliov",
#                     "Population using improved sanitation facilities (%)" = "sanit",
                    "Births attended by skilled health personnel (2/3 years) (%)" = "sba",
                    "Births attended by skilled health personnel (5 years) (%)" = "sba5",
#                     "Children (<3 years) stunted (%)" = "stunt3",
#                     "Children (<5 years) stunted (%)" = "stunt5",
#                     "Total fertility rate (per woman)" = "tfr",
#                     "Under-five mortality rate (deaths per 1000 live births)" = "u5mr",
#                     "Children (<3 years) underweight (%)" = "uweight3",
#                     "Children (<5 years) underweight (%)" = "uweight5",
                    "Children (6-59 months) who received vitamin A supplementation (%)" = "vita")
#                     "Children (<3 years) wasted (%)" = "wast3",
#                     "Children (<5 years) wasted (%)" = "wast5",
#                     "Population using improved drinking water sources (%)" = "water")
    
    return(full_indic)
  }
  else{
    core_indic <- c("anc1", "anc15", "anc4", "anc45", "bcgv", "carep", "cpmo", 
                    "cpmt", "csection", "csection5", "dptv", "ebreast", "ebreast5", "fps", "fullv", 
                    "itnch", "itnwm", "mslv", "ort", 
                     "poliov", "sba", "sba5", "vita")
    return(core_indic)
  }
  
  return(NULL)  # No matching option
}