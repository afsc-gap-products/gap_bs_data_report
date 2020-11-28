#' ---
#' title: Data Report: Select relevant species
#' purpose: List and select all of the relevant species for each survey
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---


SpCodeName.General<-list("Walleye Pollock" = 934083, # Species	Gadus chalcogrammus Pallas, 1814 – Walleye ), 
                     'Pacific cod' = 164711, #Species	Gadus macrocephalus Tilesius, 1810 – morue du Pacifique, bacalao del Pacifico, Pacific cod
                     "Yellowfin Sole" = 172907, # Species	Limanda aspera (Pallas, 1814) – yellowfin sole, limande à nageoires jaunes, Yellowfin Sole 
                     "Northern Rock Sole" = 616392, # Species	Lepidopsetta polyxystra Orr & Matarese, 2000 – northern rock sole, limande du nord, Northern Rock Sole
                     "Southern Rock Sole" = 172917, # Species	Lepidopsetta bilineata (Ayres, 1855) – rock sole, fausse limande du Pacifique, Rock Sole
                     "Flathead Sole" = 172875, # Species	Hippoglossoides elassodon Jordan & Gilbert, 1880 – flathead sole, Flathead Sole, plie à tête plate
                     "Bering Flounder" = 172876, # Species	Hippoglossoides robustus Gill & Townsend, 1897 – Bering flounder, Bering Flounder, plie de Béring
                     "Alaska Plaice" = 172901, # Species	Pleuronectes quadrituberculatus Pallas, 1814 – Alaska plaice, Alaska Plaice
                     "Greenland Turbot" = 172930, #  Species	Reinhardtius hippoglossoides (Walbaum, 1792) – Greenland halibut, platija negra, Greenland turbot, Newfoundland turbot, turbot, greeenland halibut, flétan du Groenland, Greenland Halibut
                     "Arrowtooth Flounder" = 172862, # Species	Atheresthes stomias (Jordan & Gilbert, 1880) – arrowtooth flounder, Arrowtooth Flounder, plie à grande bouche 
                     "Kamchatka Flounder" = 172861, #  Species	Atheresthes evermanni Jordan & Starks, 1904 – Kamchatka flounder, Kamchatka Flounder
                     "Pacific Halibut" = 172932) #Species: Hippoglossus stenolepis Schmidt, 1904 – valid)


SpeciesList<-list("EBS" = list("Walleye Pollock" = SpCodeName.General$`Walleye Pollock`, 
                               'Pacific cod' = SpCodeName.General$`Pacific cod`,
                               "Yellowfin Sole" = SpCodeName.General$`Yellowfin Sole`, 
                               "Northern and Southern Rock Sole (grouped)" = c(SpCodeName.General$`Northern Rock Sole`, 
                                                                               SpCodeName.General$`Southern Rock Sole`), 
                               "Flathead Sole" = SpCodeName.General$`Flathead Sole`, 
                               "Bering Flounder" = SpCodeName.General$`Bering Flounder`, 
                               "Alaska Plaice" = SpCodeName.General$`Alaska Plaice`, 
                               "Greenland Turbot" = SpCodeName.General$`Greenland Turbot`, 
                               "Arrowtooth Flounder" = SpCodeName.General$`Arrowtooth Flounder`, 
                               "Kamchatka Flounder" = SpCodeName.General$`Kamchatka Flounder`, 
                               "Pacific Halibut" = SpCodeName.General$`Pacific Halibut`)
                  )
# spplist<-SpeciesList[survey]

#############Taxize

itis_reclassify<-function(tsn, categories, missing.name){
  
  # Find which codes are in which categories
  tsn0<-as.numeric(tsn)[!(is.na(tsn))]
  tsn.indata<-classification(sci_id = tsn0, db = 'itis')
  tsn.indata<-tsn.indata[!(names(tsn.indata) %in% 0)]
  valid0<- sciname<-category0<-bottomrank<-sppname<- TSN<-c()
  
  TSN<-c()
  bottomrank<-c()
  category0<-c()
  sciname<-c()
  valid0<-c()
  
  
  for (i in 1:length(categories)) {
    
    a<-list.search(lapply(X = tsn.indata, '[', 3), categories[i][[1]] %in% . )
    
    # for (ii in 1:length(categories[i][[1]])) {
    # a<-c(a, list.search(lapply(X = tsn.indata, '[', 3), categories[i][[1]][[ii]] %in% . ))
    # }
    
    if (length(a)!=0) {
      
      sppcode<-names(a)
      sppcode<-gsub(pattern = "[a-zA-Z]+", replacement = "", x = sppcode)
      sppcode<-gsub(pattern = "\\.", replacement = "", x = sppcode)
      
      for (ii in 1:length(sppcode)) {
        TSN<-c(TSN, sppcode[ii])
        
        bottomrank<-c(bottomrank, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$rank[
          nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
        
        category0<-c(category0, names(categories[i]))
        
        sciname<-c(sciname, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$name[
          nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
        
        valid0<-c(valid0,
                  ifelse(nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])>1,
                         "valid", "invalid"))
      }
    }
  }
  
  df.out<-data.frame(TSN = TSN,
                     category = category0,
                     valid = valid0,
                     rank = bottomrank,
                     sciname = sciname )
  
  return(list("df.out" = df.out,
              "tsn.indata" = tsn.indata))
}




# a <- unique(dat$SCIENTIFIC)
# a <- gsub(pattern = " sp.", replacement = "", x = a)
# a <- a[!(a %in% "")]
# b<-get_tsn(sci_com = a, searchtype = "scientific")
# c<-b[1:(length(a))]
# sppinsurvey<-classification(c, db = 'itis')

categories<-list("Plantae" = 202422,
                 "Chromista" = 590735,
                 "Fungi" = 555705,
                 "Bacteria" = 50,
                 "Protozoa" = 43780,
                 "Archaea" = 935939,
                 "Porifera" = 46861,
                 "Cnidaria" = 48738,
                 "Platyhelminthes" = 53963,
                 "Nematoda" = 59490,
                 "Annelida" = 64357,
                 
                 "Arthropoda" = 82696,
                 "Echinodermata" = 156857,
                 "Mollusca" = 69458,
                 # "Chordata"  = "phylum",
                 "Urochordata" = 158853,
                 "Agnatha" = 914178,
                 "Chondrichthyes" = 159785,
                 "Sarcopterygii" = 161048,
                 "Tetrapoda" = 914181,
                 "Actinopterygii" = 161061)

# spp.cat<-itis_reclassify(tsn = c,
#                          categories,
#                          missing.name="Uncategorized")


# 

# Polychaeta Taxonomic Serial No.: 64358
# Hirudinea <- Lumbriculata Taxonomic Serial No.: 1048704
# 631020 Lumpenus fabricii slender eelblenny,lompénie élancée     valid
# 73892                                   Colus                                    NA        valid
# 53856                                Ctenophora comb jellies,sea walnuts,castanha do mar,ctenóforo        valid
# 68243                           Serpula          NA     valid
# 158191                              Cucumaria          NA     valid
# 79472                           Musculus                                                    NA     valid
# 1077949                           Psolus                          NA     valid
# 157891                                 Echinacea                                                      NA        valid
# 167550                             Liparis                                                      snailfishes,sea snails        valid
# 1081156 Psolus squamatus          NA     valid
# 914211 Echiura                                                         NA     valid
# 51669                                 Cyanea                                                 NA        valid



# x  Not Found:  Beringraja binoculata
# x  Not Found:  Bathyraja interrupta egg case
# x  Not Found:  Icelustula
# x  Not Found:  Polychaete tubes
# x  Not Found:  Urticina lofotensis
# x  Not Found:  Naticidae egg
# x  Not Found:  Neptunea middendorffii
# x  Not Found:  Tochuina gigantea
# x  Not Found:  Sebastes melanostictus
# x  Not Found:  Beringius J (McLean and Clark)
# x  Not Found:  Aulosaccus schulzei
# x  Not Found:  Suberites montalbidus
# x  Not Found:  Neptunea D (Clark and McLean)
# x  Not Found:  Bathyraja aleutica egg case
# x  Not Found:  Grandicrepidula grandis
# x  Not Found:  Boltenia ecinata
# x  Not Found:  Bathyraja egg case
# x  Not Found:  Peltodoris nobilis
# x  Not Found:  Lycodes beringi
# x  Not Found:  Sasakiopus salebrosus
# x  Not Found:  Halipteris A (Stone 2015)
# x  Not Found:  Decapodiformes
# x  Not Found:  Buccinum egg
# x  Not Found:  Fusitriton oregonensis egg
# x  Not Found:  Colus oceandromae
# x  Not Found:  Platichthys stellatus X Pleuronectes quadrituberculatus hybrid
# x  Not Found:  Neptunea E (Clark and McLean)
# x  Not Found:  Halocynthia hispidus
# x  Not Found:  Solaster F (Clark)
# x  Not Found:  Latrunculia oparinae
# x  Not Found:  Ophiopholis japonica
# x  Not Found:  Ophiopholis kennerleyi
# x  Not Found:  Polymastia fluegeli
# x  Not Found:  Leptasterias katharinae
# x  Not Found:  Nodulotrophon coronatus
# x  Not Found:  Bathyraja taranetzi egg case
# x  Not Found:  Pteraster F (Clark)



