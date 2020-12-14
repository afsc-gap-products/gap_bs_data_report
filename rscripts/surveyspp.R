#' ---
#' title: Data Report: Select relevant species
#' purpose: List and select all of the relevant species for each survey
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---


# SpCodeName.General<-list("Walleye Pollock" = 934083, # Species	Gadus chalcogrammus Pallas, 1814 – Walleye ), 
#                      'Pacific cod' = 164711, #Species	Gadus macrocephalus Tilesius, 1810 – morue du Pacifique, bacalao del Pacifico, Pacific cod
#                      "Yellowfin Sole" = 172907, # Species	Limanda aspera (Pallas, 1814) – yellowfin sole, limande à nageoires jaunes, Yellowfin Sole 
#                      "Northern Rock Sole" = 616392, # Species	Lepidopsetta polyxystra Orr & Matarese, 2000 – northern rock sole, limande du nord, Northern Rock Sole
#                      "Southern Rock Sole" = 172917, # Species	Lepidopsetta bilineata (Ayres, 1855) – rock sole, fausse limande du Pacifique, Rock Sole
#                      "Flathead Sole" = 172875, # Species	Hippoglossoides elassodon Jordan & Gilbert, 1880 – flathead sole, Flathead Sole, plie à tête plate
#                      "Bering Flounder" = 172876, # Species	Hippoglossoides robustus Gill & Townsend, 1897 – Bering flounder, Bering Flounder, plie de Béring
#                      "Alaska Plaice" = 172901, # Species	Pleuronectes quadrituberculatus Pallas, 1814 – Alaska plaice, Alaska Plaice
#                      "Greenland Turbot" = 172930, #  Species	Reinhardtius hippoglossoides (Walbaum, 1792) – Greenland halibut, platija negra, Greenland turbot, Newfoundland turbot, turbot, greeenland halibut, flétan du Groenland, Greenland Halibut
#                      "Arrowtooth Flounder" = 172862, # Species	Atheresthes stomias (Jordan & Gilbert, 1880) – arrowtooth flounder, Arrowtooth Flounder, plie à grande bouche 
#                      "Kamchatka Flounder" = 172861, #  Species	Atheresthes evermanni Jordan & Starks, 1904 – Kamchatka flounder, Kamchatka Flounder
#                      "Pacific Halibut" = 172932) #Species: Hippoglossus stenolepis Schmidt, 1904 – valid)
# 
# 
# SpeciesList<-list("EBS" = list("Walleye Pollock" = SpCodeName.General$`Walleye Pollock`, 
#                                'Pacific cod' = SpCodeName.General$`Pacific cod`,
#                                "Yellowfin Sole" = SpCodeName.General$`Yellowfin Sole`, 
#                                "Northern and Southern Rock Sole (grouped)" = c(SpCodeName.General$`Northern Rock Sole`, 
#                                                                                SpCodeName.General$`Southern Rock Sole`), 
#                                "Flathead Sole" = SpCodeName.General$`Flathead Sole`, 
#                                "Bering Flounder" = SpCodeName.General$`Bering Flounder`, 
#                                "Alaska Plaice" = SpCodeName.General$`Alaska Plaice`, 
#                                "Greenland Turbot" = SpCodeName.General$`Greenland Turbot`, 
#                                "Arrowtooth Flounder" = SpCodeName.General$`Arrowtooth Flounder`, 
#                                "Kamchatka Flounder" = SpCodeName.General$`Kamchatka Flounder`, 
#                                "Pacific Halibut" = SpCodeName.General$`Pacific Halibut`)
#                   )
# spplist<-SpeciesList[survey]

#############Taxize

itis_reclassify<-function(tsn, categories, missing.name){
  
  # Find which codes are in which categories
  if (is.numeric(tsn)){
    tsn0<-as.numeric(tsn)[!(is.na(tsn))]
  } else {
    tsn0<-as.character(tsn)[!(is.na(tsn))]
  }
  tsn.indata<-classification(sci_id = tsn0, db = 'itis')
  tsn.indata<-tsn.indata[!(names(tsn.indata) %in% 0)]
  
  valid0 <- sciname <- category0 <- bottomrank <- sppname <- TSN <- cat_TSN <- c()
  
  for (i in 1:length(categories)) {
    
    a<-rlist::list.search(.data = lapply(X = tsn.indata, '[', 3), 
                   categories[i][[1]] %in% . )
    
    # a<-rlist::list.search(.data = lapply(X = tsn.indata, '[', 3), 
    #                       . %in% categories[i][[1]])# == . )
    
    names(a)<-gsub(pattern = "\\.id", replacement = "", x = names(a))
    
    sppcode<-names(a)
    
    if (length(sppcode)!=0) {
        
        for (ii in 1:length(sppcode)) {
        #   
        #   if ((is.na(tsn.indata[ii][[1]]$id))) {
        #     
        #     cat_TSN<-c(cat_TSN, a[ii][[1]])
        #     
        #     bottomrank<-c(bottomrank, NA)
        #     
        #     TSN<-c(TSN, NA)
        #     
        #     category0<-c(category0, missing.name)
        #     
        #     sciname<-c(sciname, NA)
        #     
        #     valid0<-c(valid0, NA)
        #     
        #     
        #   } else {
            
            cat_TSN<-c(cat_TSN, names(categories)[i])
            
            bottomrank<-c(bottomrank, 
                          tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$rank[
                            nrow(tsn.indata[names(tsn.indata) %in% 
                                              sppcode[ii]][[1]])])
            
            TSN<-c(TSN, 
                   as.numeric(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$id[
                     nrow(tsn.indata[names(tsn.indata) %in% 
                                sppcode[ii]][[1]])]))
            
            category0<-c(category0, names(categories)[i])
            
            sciname<-c(sciname, 
                       tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$name[
                         nrow(tsn.indata[names(tsn.indata) %in% 
                                           sppcode[ii]][[1]])])
            
            valid0<-c(valid0,
                      ifelse(nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])>1,
                             "valid", "invalid"))
        # }
      }
    }
  }
  
  df.out<-data.frame("cat_TSN" = cat_TSN,
                     "category" = category0,
                     "valid" = valid0,
                     "rank" = bottomrank,
                     "TSN" = TSN,
                     "sciname" = sciname )
  df.out<-unique(df.out)
  
  return(list("df.out" = df.out,
              "tsn.indata" = tsn.indata))
}




# a <- unique(dat$SCIENTIFIC)
# a <- gsub(pattern = " sp.", replacement = "", x = a)
# a <- a[!(a %in% "")]
# b<-get_tsn(sci_com = a, searchtype = "scientific")
# c<-b[1:(length(a))]
# sppinsurvey<-classification(c, db = 'itis')

categories <- list(
  # kingdom
  "Plantae" = 202422,
  "Fungi" = 555705,
  "Protozoa" = 43780,
  "Bacteria" = 50,
  "Archaea" = 935939,  
  
  # Animalia
  # Invert
  "Porifera" = 46861,
  "Cnidaria" = 48738,
  "Platyhelminthes" = 53963,
  "Nematoda" = 59490,
  "Annelida" = 64357,
  "Chromista" = 590735,
  "Echinodermata" = 156857,
  "Arthropoda" = 82696,
  "Mollusca" = 69458,  
  
  # Vert
  "Urochordata" = 158853,
  "Agnatha" = 914178,
  "Chondrichthyes" = 159785,
  "Sarcopterygii" = 161048,
  "Tetrapoda" = 914181,
  "Actinopterygii" = 161061
)


# Make reference table

dat.tsn<-unique(dat[,c("COMMON", "SCIENTIFIC", "SID")])
names(dat.tsn)<-paste0(names(dat.tsn), "_orig")

dat.tsn$SCIENTIFIC<-dat.tsn$SCIENTIFIC_orig
dat.tsn$SCIENTIFIC<-gsub(pattern = " sp.", replacement = "", x = dat.tsn$SCIENTIFIC)
dat.tsn$SCIENTIFIC<-gsub(pattern = " spp.", replacement = "", x = dat.tsn$SCIENTIFIC)
dat.tsn$SCIENTIFIC<-gsub(pattern = " egg case", replacement = "", x = dat.tsn$SCIENTIFIC)
dat.tsn$SCIENTIFIC<-gsub(pattern = " egg", replacement = "", x = dat.tsn$SCIENTIFIC)
dat.tsn$SCIENTIFIC<-gsub(pattern = " hybrid", replacement = "", x = dat.tsn$SCIENTIFIC)
dat.tsn$SCIENTIFIC<-gsub(pattern = " tubes", replacement = "", x = dat.tsn$SCIENTIFIC)
dat.tsn$SCIENTIFIC[dat.tsn$SCIENTIFIC %in% "Hirudinea"]<-"Lumbriculata"
dat.tsn$SCIENTIFIC<-gsub(pattern = "gastropod", replacement = "Gastropoda", 
                         x = dat.tsn$SCIENTIFIC, ignore.case = TRUE)
dat.tsn$SCIENTIFIC<-gsub(pattern = "Gastropodaa", replacement = "Gastropoda", 
                         x = dat.tsn$SCIENTIFIC, ignore.case = TRUE)

dat.tsn$SCIENTIFIC<-gsub( " *\\(.*?\\) *", "", dat.tsn$SCIENTIFIC)

for (i in 1:nrow(dat.tsn)){
  dat.tsn$SCIENTIFIC[i]<-ifelse((nchar(strsplit(x = dat.tsn$SCIENTIFIC[i], split = " ")[[1]][2]) %in% 1), 
                             strsplit(x = dat.tsn$SCIENTIFIC[i], split = " ")[[1]][1], 
                             dat.tsn$SCIENTIFIC[i])
  if (grepl(pattern = " X ", x = dat.tsn$SCIENTIFIC[i], ignore.case = T)){
    dat.tsn$SCIENTIFIC[i]<-NA
  }
}
dat.tsn$SCIENTIFIC<-trimws(dat.tsn$SCIENTIFIC)

spp.cat1<-itis_reclassify(tsn = unique(dat.tsn$SCIENTIFIC),
                          categories,
                          missing.name="Uncategorized")

reftable<-spp.cat1$df.out

reftable<-dplyr::rename(reftable, 
       "SCIENTIFIC" = "sciname")

reftable<-left_join(x = dat.tsn, y = reftable, by ="SCIENTIFIC")

spp.cat2<-list("tsn.list" = spp.cat1$tsn.indata, 
               "reftable" = reftable)

save(spp.cat2, file = "./data/specieslistinTSN.rdata")
write.csv(x = spp.cat2$reftable, file = "./data/speciesTSN.csv")



# x  Not Found:  Icelusniger
# x  Not Found:  Ophiura sarsii
# x  Not Found:  
# x  Not Found:  Labidochirusendescens
# x  Not Found:  Ciliatoclinocardium ciliatum
# x  Not Found:  Serpula columbiana
# x  Not Found:  Liponema brevicorne
# x  Not Found:  Beringraja binoculata
# x  Not Found:  Icelustula
# x  Not Found:  Urticina lofotensis
# x  Not Found:  Neptunea middendorffii
# x  Not Found:  Tochuina gigantea
# x  Not Found:  Sebastes melanostictus
# x  Not Found:  Aulosaccus schulzei
# x  Not Found:  Suberites montalbidus
# x  Not Found:  Grandicrepidula grandis
# x  Not Found:  Boltenia ecinata
# x  Not Found:  Peltodoris nobilis
# x  Not Found:  Lycodes beringi
# x  Not Found:  Sasakiopus salebrosus
# x  Not Found:  Decapodiformes
# x  Not Found:  Peltodoris nobilis
# x  Not Found:  Lycodes beringi
# x  Not Found:  Sasakiopus salebrosus
# x  Not Found:  Decapodiformes
# x  Not Found:  Colus oceandromae
# x  Not Found:  Halocynthia hispidus
# x  Not Found:  Latrunculia oparinae
# x  Not Found:  Ophiopholis japonica
# x  Not Found:  Ophiopholis kennerleyi
# x  Not Found:  Polymastia fluegeli
# x  Not Found:  Leptasterias katharinae
# x  Not Found:  Nodulotrophon coronatus



# x  Not Found:  Gastropodaa



