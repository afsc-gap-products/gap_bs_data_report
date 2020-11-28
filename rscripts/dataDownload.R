#' ---
#' title: Data Report: Download Data
#' purpose: Download relevant data for each survey
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---

# https://www.fisheries.noaa.gov/alaska/commercial-fishing/alaska-groundfish-bottom-trawl-survey-data

if (survey %in% "EBS") {
  # Download EBS
  download.file(url = "https://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ebs2017_2018.zip", 
                destfile=paste0("./data/ebs2017_2018.zip") )
  
  
  if (filetype==".zip") {
    zip::unzip(zipfile = paste0("./data/ebs2017_2018.zip"), 
          overwrite = T,
          exdir = paste0("./data/"))
  }
}

library(taxize)







# itis_reclassify<-function(tsn, categories, missing.name){
#   
#   # Find which codes are in which categories
#   tsn0<-as.numeric(tsn)[!(is.na(tsn))]
#   tsn.indata<-classification(sci_id = tsn0, db = 'itis')
#   tsn.indata<-tsn.indata[!(names(tsn.indata) %in% 0)]
#   valid0<- sciname<-category0<-bottomrank<-sppname<- TSN<-c() 
#   
#   TSN<-c()
#   bottomrank<-c()
#   category0<-c()
#   sciname<-c()
#   valid0<-c()
#   
#   
#   for (i in 1:length(categories)) {
#     
#     a<-list.search(lapply(X = tsn.indata, '[', 3), categories[i][[1]] %in% . )
#     
#     # for (ii in 1:length(categories[i][[1]])) {
#     # a<-c(a, list.search(lapply(X = tsn.indata, '[', 3), categories[i][[1]][[ii]] %in% . ))
#     # }
#     
#     if (length(a)!=0) {
#       
#       sppcode<-names(a)
#       sppcode<-gsub(pattern = "[a-zA-Z]+", replacement = "", x = sppcode)
#       sppcode<-gsub(pattern = "\\.", replacement = "", x = sppcode)
#       
#       for (ii in 1:length(sppcode)) {
#         TSN<-c(TSN, sppcode[ii])
#         
#         bottomrank<-c(bottomrank, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$rank[
#           nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
#         
#         category0<-c(category0, names(categories[i]))  
#         
#         sciname<-c(sciname, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$name[
#           nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
#         
#         valid0<-c(valid0, 
#                   ifelse(nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])>1, 
#                          "valid", "invalid"))
#       }
#     }
#   }
#   
#   df.out<-data.frame(TSN = TSN, 
#                      category = category0, 
#                      valid = valid0, 
#                      rank = bottomrank, 
#                      sciname = sciname )
#   
#   return(list("df.out" = df.out, 
#               "tsn.indata" = tsn.indata))
# }
# 
# 
# spp_reclassify<-function(landings.df, spcat.list, place){
#   
#   ####
#   ####FINFISH AND SHELFISH AND OTHER TOTALS
#   ###
#   
#   temp<-landings.df
#   total.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
#                       by=list(year=temp$year),
#                       FUN=sum,na.rm=TRUE)
#   
#   #Finfish
#   temp<-landings.df[landings.df$OFS %in% "F",]
#   if (nrow(temp)!=0) {
#     finfish.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
#                           by=list(year=temp$year),
#                           FUN=sum, na.rm=TRUE)
#     
#     if (sum(!(min(landings.df$year):max(landings.df$year) %in% finfish.df$year)) >0){
#       
#       finfish.df<-rbind.data.frame(finfish.df, 
#                                    cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
#                                                     "POUNDS" = 0, 
#                                                     "DOLLARS" = 0))
#       landings.df<-landings.df[order(landings.df$year),]
#       
#     }
#   } else {
#     finfish.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
#                            POUNDS = 0,
#                            DOLLARS = 0)
#   }
#   
#   #Shellfish
#   temp<-landings.df[landings.df$OFS %in% "S",]
#   if (nrow(temp)!=0) {
#     shellfish.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
#                             by=list(year=temp$year),
#                             FUN=sum,na.rm=TRUE)
#     
#     if (sum(!(min(landings.df$year):max(landings.df$year) %in% shellfish.df$year)) >0){
#       
#       shellfish.df<-rbind.data.frame(shellfish.df, 
#                                      cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
#                                                       "POUNDS" = 0, 
#                                                       "DOLLARS" = 0))
#       shellfish.df<-shellfish.df[order(shellfish.df$year),]
#     }
#   } else {
#     shellfish.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
#                              POUNDS = 0,
#                              DOLLARS = 0)
#   }
#   
#   
#   #Other
#   temp<-landings.df[landings.df$OFS %in% "O",]
#   if (nrow(temp)!=0) {
#     other.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
#                         by=list(year=temp$year),
#                         FUN=sum,na.rm=TRUE)
#     
#     if (sum(!(min(landings.df$year):max(landings.df$year) %in% other.df$year)) > 0){
#       
#       other.df<-rbind.data.frame(other.df, 
#                                  cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[
#                                    !(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
#                                    "POUNDS" = 0, 
#                                    "DOLLARS" = 0))
#       other.df<-other.df[order(other.df$year),]
#     }
#   } else {
#     other.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
#                          POUNDS = 0,
#                          DOLLARS = 0)
#   }
#   
#   
#   land.df<-land.tot<-cbind.data.frame("Total" = total.df$POUNDS,
#                                       "Finfish" = finfish.df$POUNDS,
#                                       "Shellfish" = shellfish.df$POUNDS,
#                                       "Other" = other.df$POUNDS,
#                                       "Key Species" = NA)
#   rev.df<-rev.tot<-cbind.data.frame("Total" = total.df$DOLLARS,
#                                     "Finfish" = finfish.df$DOLLARS,
#                                     'Shellfish' = shellfish.df$DOLLARS,
#                                     "Other" = other.df$DOLLARS,
#                                     "Key Species" = NA)
#   price.df<-price.tot<-cbind.data.frame("Key Species" = rep_len(x = NA, length.out = nrow(rev.df)))
#   
#   
#   ####
#   ####SPECIES
#   ###
#   uniquespp<-data.frame()
#   
#   
#   if (!(is.na(spcat.list))) {
#     temp<-itis_reclassify(tsn = as.numeric(paste(unique(landings.df$TSN))), 
#                           categories = spcat.list$Areas[place][[1]], 
#                           missing.name="Uncategorized")
#     
#     tsn.id<-temp[1][[1]]
#     # tsn.id<-tsn.id[!(tsn.id$category %in% c("Other", "Uncategorized")), 
#     #                c("TSN", "category")]
#     tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
#     
#     
#     land.df<-data.frame(t(land.df))
#     rev.df<-data.frame(t(rev.df))
#     price.df<-data.frame(t(price.df))
#     
#     
#     #Add species to tables
#     land.df$Footnotes<-rev.df$Footnotes<-price.df$Footnotes<-""
#     
#     for (i in 1:length(names(spcat.list$Areas[place][[1]]))) {
#       
#       spp0<-sort(names(spcat.list$Areas[place][[1]]))[i]
#       
#       temp<-landings.df[landings.df$TSN %in% tsn.id$TSN[tsn.id$category %in% spp0],]
#       # temp<-temp[!(is.na(temp$POUNDS) & is.na(temp$DOLLARS)), ]
#       
#       #FOOTNOTES
#       Footnotes<-""
#       
#       #Species that should be included in this sum and footnote
#       sppno<-as.numeric(unlist(spcat.list$Areas[place][[1]][spp0]))
#       spp<-tsn.indata.foot<-classification(sci_id = sppno[sppno>0], db = 'itis')
#       sppnames<-taxspp(spp = tsn.indata.foot, spp0, landings.df)
#       species<-sppnames$species
#       notspecies<-sppnames$notspecies
#       
#       spp00<-strsplit(x = spp0, split = ",")
#       spp00<-strsplit(x = spp00[[length(spp00)]], split = "and")
#       spp00<-gsub(x = unlist(spp00), pattern = "and ", replacement = "", ignore.case = T)
#       spp00<-gsub(x = unlist(spp00), pattern = "& ", replacement = "", ignore.case = T)
#       spp00<-trimws(spp00)
#       
#       
#       if (!(is.null(species)) && !(tolower(species) %in% tolower(spp00))) {        #If species names the few specific species in the group, then dont footnote
#         Footnotes<-""
#         #If a specific species (and not a gorup) #No Footnote required
#       } else if (length(spp) == 1 & !(spp[[1]]$rank[nrow(spp[[1]])] %in% "species") ) { #If there is only 1 code and its not a specific species
#         Footnotes<-paste0("This species group includes species within the ",
#                           paste0(ifelse(spp[[1]]$rank[nrow(spp[[1]])] %in% "genus",
#                                         paste0("*", (spp[[1]]$name[nrow(spp[[1]])]), "*"),
#                                         tolower(spp[[1]]$name[nrow(spp[[1]])])) ,
#                                  " ", spp[[1]]$rank[nrow(spp[[1]])]), ".")
#       } else if (length(spp) > 1) {
#         
#         
#         if (length(species)>0 & length(notspecies)>0) {
#           Footnotes<-paste0("This species group includes species within the ",
#                             funct_list(notspecies), " and ", funct_list(species) , ".")
#         } else if (length(species)==0 & length(notspecies)>0) {
#           Footnotes<-paste0("This species group includes species within the ",
#                             funct_list(notspecies), ".")
#         } else if (length(species)>0 & length(notspecies)==0) {
#           Footnotes<-paste0("This species group includes ",
#                             funct_list(species) , ".")
#         }
#       }
#       #Species that should be excluded from this sum and footnote
#       if (sum(sppno<0)>0) {
#         spp<-tsn.indata.foot<-classification(sci_id = (sppno[sppno<0])*-1, db = 'itis')
#         sppnames<-taxspp(spp = tsn.indata.foot, spp0, landings.df)
#         species<-sppnames$species
#         notspecies<-sppnames$notspecies
#         
#         if (length(tsn.indata.foot)>1) {
#           if (length(species)>0 & length(notspecies)>0) {
#             Footnotes<-paste0(Footnotes, " This species group excludes species within the ",
#                               funct_list(notspecies), " and, specifically, ", funct_list(species) , ".")
#           } else if (length(species)==0 & length(notspecies)>0) {
#             Footnotes<-paste0(Footnotes, " This species group excludes species within the ",
#                               funct_list(notspecies), ".")
#           } else if (length(species)>0 & length(notspecies)==0) {
#             Footnotes<-paste0(Footnotes, " This species group excludes ",
#                               funct_list(species) , ".")
#           }
#         }
#       }
#       
#       #COMPILE TABLES
#       
#       if (nrow(temp) %in% 0) {
#         land.df<-rbind.data.frame(land.df, 
#                                   c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
#         rev.df<-rbind.data.frame(rev.df, 
#                                  c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
#         price.df<-rbind.data.frame(price.df, 
#                                    c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
#         
#       } else {
#         
#         #Unique Species
#         uniquespp0<-unique(temp[,c("TSN", "CommonName")])
#         uniquespp0$Category<-spp0
#         uniquespp0$CommonName1<-funct_list(spp00)
#         uniquespp<-rbind.data.frame(uniquespp, uniquespp0)  
#         
#         
#         
#         temp.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] , 
#                            by=list(year=temp$year), 
#                            FUN=sum,na.rm=TRUE)
#         
#         #make sure columns match
#         if (sum((!(min(landings.df$year):max(landings.df$year) %in% temp.df$year)) >0)) {
#           yr0<-(min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% temp.df$year)]
#           temp.df<-rbind.data.frame(temp.df, 
#                                     cbind.data.frame('year' = yr0, 
#                                                      'POUNDS' = rep_len(x = NA, length.out = length(yr0)), 
#                                                      'DOLLARS' = rep_len(x = NA, length.out = length(yr0))))
#         }
#         temp.df<-temp.df[order(temp.df$year, decreasing = F),]
#         
#         land.df<-rbind.data.frame(land.df, 
#                                   c(temp.df$POUNDS, Footnotes))
#         rev.df<-rbind.data.frame(rev.df, 
#                                  c(temp.df$DOLLARS, Footnotes))
#         price.df<-rbind.data.frame(price.df, 
#                                    c(temp.df$POUNDS/temp.df$DOLLARS, Footnotes))
#         
#       }
#       
#       rownames(land.df)[length(rownames(land.df))]<-
#         rownames(rev.df)[length(rownames(rev.df))]<-
#         rownames(price.df)[length(rownames(price.df))]<-tolower2(spp0, capitalizefirst = T)  
#     }
#     
#     
#     
#     uniquespp$Area<-place
#     uniquespp$SciName<-""
#     a<-classification(sci_id = uniquespp$TSN, db = 'itis')
#     for (i in 1:nrow(uniquespp)){
#       if (a[i][[1]]$rank[nrow(a[i][[1]])] %in% "species") {
#         uniquespp$SciName[i]<-a[i][[1]]$name[nrow(a[i][[1]])]
#       } else {
#         uniquespp$SciName[i]<-paste0(a[i][[1]]$name[nrow(a[i][[1]])], " ", a[i][[1]]$rank[nrow(a[i][[1]])])
#       }
#     } 
#     
#     
#     land.df<-cbind.data.frame(rownames(land.df), land.df)
#     rev.df<-cbind.data.frame(rownames(rev.df), rev.df)
#     price.df<-cbind.data.frame(rownames(price.df), price.df)
#     
#   } else {
#     
#     land.df<-t(land.df)
#     land.df<-cbind.data.frame("keyspecies" = rownames(land.df)[-nrow(land.df)], 
#                               land.df[-nrow(land.df),], 
#                               Footnotes = NA)
#     rev.df<-t(rev.df)
#     rev.df<-cbind.data.frame("keyspecies" = rownames(rev.df)[-nrow(rev.df)], 
#                              rev.df[-nrow(rev.df),], 
#                              Footnotes = NA)
#     price.df<-t(price.df)
#     price.df<-cbind.data.frame("keyspecies" = NA, 
#                                price.df, 
#                                Footnotes = NA)
#   }
#   
#   colnames(land.df)<-colnames(rev.df)<-colnames(price.df)<-c("keyspecies", 
#                                                              as.character(min(landings.df$year):
#                                                                             max(landings.df$year)), 
#                                                              "Footnotes")
#   
#   #####footnote with species
#   return(list("revenue" = rev.df, 
#               "landings" = land.df, 
#               "price" = price.df, 
#               "uniquespp" = uniquespp))
# }
# 
# 
# categories<-list("Plantae" = 202422, 
#                  "Chromista" = 590735,
#                  "Fungi" = 555705,
#                  "Bacteria" = 50,
#                  "Protozoa" = 43780,
#                  "Archaea" = 935939,
#                  "Porifera" = 46861, 
#                  "Cnidaria" = 48738, 
#                  "Platyhelminthes" = 53963, 
#                  "Nematoda" = 59490, 
#                  "Annelida" = 64357, 
#                  
#                  "Arthropoda" = 82696, 
#                  "Echinodermata" = 156857, 
#                  "Mollusca" = 69458, 
#                  # "Chordata"  = "phylum", 
#                  "Urochordata" = 158853,
#                  "Agnatha" = 914178,
#                  "Chondrichthyes" = 159785,
#                  "Sarcopterygii" = 161048, 
#                  "Tetrapoda" = 914181, 
#                  "Actinopterygii" = 161061)
# 
# spp.cat<-itis_reclassify(tsn = unique(landings.data$Tsn), 
#                          categories, 
#                          missing.name="Uncategorized")
