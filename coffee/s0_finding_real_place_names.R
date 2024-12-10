## data cleaning

rm(list = ls())
call_package = function(package){
  if(!suppressMessages(suppressWarnings(require(package, character.only = T)))){
    install.packages(package);suppressMessages(suppressWarnings(require(package, character.only = T)))}  
}
call_package('tidyverse')
call_package('ggthemes')
call_package('stringdist')


header_cleaning = function(data_names, to_remove = c(" ", "/", "[\\(\\)]","#","-", ",","[.]")){
  strvar = data_names
  for( i in to_remove){
    strvar = unlist(str_replace_all(strvar, i, ""))
  }
  return(tolower(strvar))
}


replace_accents= function(strvar, to_remove = list("á" = "a", "ó" = "o","í" = "i", "é" = "e" , "ú" = "u")){
  strvar = tolower(strvar)
  for( i in 1:length(to_remove)){
    strvar = unlist(str_replace_all(strvar, names(to_remove)[i], to_remove[[i]]))
  }
  return(strvar)
}


compute_string_similarity = function(str_list1, str_list2){
  
  simmatrix = stringdist::stringdistmatrix(str_list1, str_list2,method = 'jw')%>%
    data.frame()%>%
    `rownames<-`(str_list1)%>%
    `colnames<-`(str_list2)
  
  return(simmatrix)
}



get_unique_attributes = function(dataframe, feature_name){
  return(dataframe%>%
           pull(feature_name)%>%
           unique()%>%
           na.omit()%>%
           c())
}



strings_to_change = function(raw_values, real_values, similarity_thresh = 0.085){
  
  sim_matrix = compute_string_similarity(raw_values, real_values)
  condition_matrix = apply(sim_matrix,2,function(x)(x<similarity_thresh) &( x !=0))
  which_valuestochange = apply(condition_matrix, 1, function(j) T%in% j)
  
  valuesto_changeby = condition_matrix%>%
    data.frame()%>%
    filter(which_valuestochange)%>%
    group_modify(~.x%>%
                   apply(1,function(j) real_values[which(j%in%T)[1]]))
  
  
  return(data.frame(raw_values = names(valuesto_changeby), change_to = unname(valuesto_changeby)))
}


## reading from excel file
# some of the column names are wrong this step is for detecting thos eworng names an merge all the years data into one dataframe

filepath = "data/IHCAFE_SOIL_DATABASE.xlsx"
tabnames = readxl::excel_sheets(filepath)

# only get years
yeartabs = which(sapply(tabnames, function(x) stringr::str_sub(x,1,2)=='20'))

coffe_data_list = lapply(1:length(yeartabs), function(i){
  ntab = yeartabs[i]
  year = names(yeartabs)[i]
  
  yeardata = readxl::read_excel("data/IHCAFE_SOIL_DATABASE.xlsx", ntab)  
  return(yeardata%>%
           mutate(year = year))
})


newnames= lapply(1:length(coffe_data_list), function(x) header_cleaning(names(coffe_data_list[[x]])))


compare = unique(unlist(newnames))
dfcount = data.frame(lapply(newnames, function(x){
  (compare %in% x)*1
}))
columnstoremove = apply(dfcount, 1, sum) != max(apply(dfcount, 1, sum))

names_tokeep = compare[!columnstoremove]

coffee_data_df = do.call(rbind, lapply(1:length(coffe_data_list), function(x){
  cat(x)
  raw_names = names(coffe_data_list[[x]])
  colname = header_cleaning(raw_names)
  columns = unlist(sapply(1:length(names_tokeep), function(j) raw_names[which(colname %in% names_tokeep[j])]))
  df = coffe_data_list[[x]]%>%
    dplyr::select(columns)
  names(df) = names_tokeep
  return(df)
}))

## remove rows with a high percentage of na
rowstoremove = which(apply(coffee_data_df, 1,function(x) sum(is.na(x)) > (length(names_tokeep)*.8)))

## remove duplicated rows
coffee_data_df_filtered = coffee_data_df[-rowstoremove,]%>%
  filter(!is.na(lab))%>%
  mutate(id = paste0(lab, finca, year))%>%
  group_by(lab, finca, year) %>% slice(1L)%>%
  ungroup()



### finding the true department, municipio and aldea names.
## the cleaning is done through four steps, 
# first check if the departamento names has a similar name with the original names, which are provided by the geodata file, if any similarity is more than a percentage threshold the value is replaced
# second check if the municipio names has a similar name with the original names, which are provided by the geodata file, if any vsimilarity is more than a percentage threshold the value is replaced
# third check if the aldea names has a similar name with the original names, which are provided by the geodata file, if any vsimilarity is more than a percentage threshold the value is replaced
# fourth for those aldeas that don't match the municipio or departamento with the the original geodata file, those aldeas are compared with the values in the geodata file, if there is one that has the same name and it is unique, the departament and/or municpio is replaced



## reading rgeaodata

geo_data = read_sf("D:/OneDrive - CGIAR/projects/suelos_honduras/spatial_files/country/vector/tb_limitealdeas.shp")%>%
  mutate(plain_depto = header_cleaning(stringi::stri_trans_general(replace_accents(DEPTO),"Latin-ASCII")))%>%
  mutate(plain_muni = header_cleaning(stringi::stri_trans_general(replace_accents(MUNI),"Latin-ASCII")))%>%
  mutate(plain_aldea = header_cleaning(stringi::stri_trans_general(replace_accents(ALDEA),"Latin-ASCII")))%>%
  mutate(id_aldea = paste0(plain_depto, '_' , plain_muni, '_', plain_aldea))


geo_data$plain_depto

# creating names without any character differnt to alphanumeric

database = coffee_data_df_filtered%>%
  dplyr::select(depto, municipio, aldea)%>%
  na.omit()%>%
  mutate(plain_depto = header_cleaning(stringi::stri_trans_general(replace_accents(depto),"Latin-ASCII")))%>%
  mutate(plain_muni = header_cleaning(stringi::stri_trans_general(replace_accents(municipio),"Latin-ASCII")))%>%
  mutate(plain_aldea = header_cleaning(stringi::stri_trans_general(replace_accents(aldea),"Latin-ASCII")))%>%
  mutate(id_aldea = paste0(plain_depto, '_' , plain_muni, '_', plain_aldea))

  
real_dptonames = get_unique_attributes(geo_data, "plain_depto")
real_muninames = get_unique_attributes(geo_data, "plain_muni")
real_aldeasnames = get_unique_attributes(geo_data, "plain_aldea")

raw_dptonames = get_unique_attributes(database, "plain_depto")
raw_muninames = get_unique_attributes(database, "plain_muni")
raw_aldeasnames = get_unique_attributes(database, "plain_aldea")


dpto_tochange = strings_to_change(raw_dptonames, real_dptonames)
muni_tochange = strings_to_change(raw_muninames, real_muninames)
aldeas_tochange = strings_to_change(raw_aldeasnames,real_aldeasnames, similarity_thresh = 0.06)

# change those names similar to the real values
database_c = database%>%
  mutate(plain_depto  = sapply(plain_depto, function(x) ifelse(x %in% dpto_tochange$raw_values, dpto_tochange%>%filter(raw_values == x)%>%
                                                                 pull(change_to)%>%c(), x)))%>%
  mutate(plain_muni  = sapply(plain_muni, function(x) ifelse(x %in% muni_tochange$raw_values, muni_tochange%>%filter(raw_values == x)%>%
                                                               pull(change_to)%>%c(), x)))%>%
  mutate(plain_aldea  = sapply(plain_aldea, function(x) ifelse(x %in% aldeas_tochange$raw_values, aldeas_tochange%>%filter(raw_values == x)%>%
                                                                 pull(change_to)%>%c(), x)))%>%
  mutate(orig_id_aldea = id_aldea)%>%
  mutate(id_aldea = paste0(plain_depto, '_' , plain_muni, '_', plain_aldea))


## remove duplicated rows
database_c_g = database_c%>%
  dplyr::select(id_aldea, plain_depto, plain_muni, plain_aldea)%>%
  group_by(id_aldea, plain_depto, plain_muni, plain_aldea )%>%
  slice(1L)%>%
  ungroup()


## 

raw_dptonames = get_unique_attributes(database_c_g, "plain_depto")


database_c_g_corrected = do.call(rbind,lapply(raw_dptonames, function(dptoname){
  print(dptoname)
  databasemunis = database_c_g%>%
    filter(plain_depto %in% dptoname)%>%
    pull(plain_muni)%>%
    unique()
  
  orig_mun = geo_data%>%
    filter(plain_depto == dptoname)%>%
    arrange(plain_muni)%>%
    pull(plain_muni)%>%
    unique()
  datafixed = do.call(rbind,lapply(databasemunis, function(muniname){
    cat(dptoname,'\n  m: ',muniname)
    
    orig_aldeas = geo_data%>%
      filter(plain_depto == dptoname)%>%
      filter(plain_muni == muniname)%>%
      arrange(plain_aldea)%>%
      pull(plain_aldea)
    
    data_aldeas = database_c_g%>%
      filter(plain_depto == dptoname)%>%
      filter(plain_muni == muniname)%>%
      arrange(plain_muni)
    
    
    isinreal_loc = sapply(as.character(data_aldeas%>%
                                         pull(plain_aldea)), function(x) x %in% orig_aldeas)
    whicharenot = data.frame(plain_aldea = names(isinreal_loc), isin = unname(isinreal_loc))
    
    j = 1
    
    real_aldeasdata = do.call(rbind,lapply(1:nrow(whicharenot), function(j){
      if(whicharenot$isin[j] == F){
        real_loc = geo_data%>%
          filter(plain_aldea %in% whicharenot$plain_aldea[j])%>%
          pull(id_aldea)%>%
          str_split('_',simplify = T)%>%
          data.frame()
        
        
        if(nrow(real_loc)>0){
          real_loc = real_loc%>%
            rename('plain_depto' = X1,
                   'plain_muni' = X2,
                   'plain_aldea' = X3)%>%
            mutate(database_aldea = whicharenot$aldea[j])
          
          
          if(nrow(real_loc)>1)
            real_loc= real_loc%>%
              filter(plain_depto == dptoname)
          
          if(nrow(real_loc)>1)
            real_loc= real_loc%>%
              filter(plain_muni == muniname)
          
          if(nrow(real_loc)>1){print(real_loc)}
        }
        return(real_loc)
      }
    }))
    if(is.null(real_aldeasdata))
      return(fixed_data_aldeas = data_aldeas%>%
               data.frame()%>%
               mutate(raw_id_aldea = id_aldea)%>%
               mutate(fixed = ifelse( whicharenot$isin, 'real', 'not in geodatabase' )))
    if(!(length(real_aldeasdata)>0 & dim(real_aldeasdata)[1]>0))
      return(fixed_data_aldeas = data_aldeas%>%
               data.frame()%>%
               mutate(raw_id_aldea = id_aldea)%>%
               mutate(fixed = ifelse( whicharenot$isin, 'real', 'not in geodatabase' )))
    
    
    
    real_aldeasdata = real_aldeasdata%>%
      left_join(whicharenot, by = 'plain_aldea')%>%
      mutate(id_aldea = paste0(plain_depto, '_' , plain_muni, '_', plain_aldea))
    x = 3
    data_aldeasc = data_aldeas%>%
      data.frame()%>%
      mutate(fixed = 'not in geodatabase')%>%
      mutate(raw_id_aldea = id_aldea)
    x = 2
    fixed_data_aldeas = do.call(rbind,lapply(1:nrow(data_aldeasc), function(x){
      
      if(whicharenot$isin[x] == F){
        realval = real_aldeasdata%>%
          filter(plain_aldea == data_aldeasc$plain_aldea[x])%>%
          pull(id_aldea)
        if(length(realval)>1) {print('check'); return(data_aldeasc[x,])}
        if(length(realval)>0)if(!data_aldeasc$id_aldea[x] == realval){
          data_aldeasc$id_aldea[x] = realval
          data_aldeasc[x,'fixed'] = 'changed'
          return(data_aldeasc[x,])
        }
        return(data_aldeasc[x,])
      }
      else{
        data_aldeasc[x,'fixed'] = 'real'
      }
      return(data_aldeasc[x,]) 
    }))
    
    return(fixed_data_aldeas%>%
             data.frame())
  }))
  
}))


table(database_c_g_corrected$fixed)

ids_with_data = database_c_g_corrected%>%
  filter(fixed %in% c('real','changed'))%>%
  pull(id_aldea)%>%
  unique()

ids_without_data = database_c_g_corrected%>%
  filter(fixed %in% c('not in geodatabase'))%>%
  pull(id_aldea)%>%
  unique()


ids_tochange = strings_to_change(ids_without_data, ids_with_data, similarity_thresh = 0.04)


ids_tochange

database_c_g_corrected_c = database_c_g_corrected%>%
  mutate(fixed  = sapply(1:length(fixed), function(x) ifelse(id_aldea[x] %in% ids_tochange$raw_values, 'changed', fixed[x])))%>%
  mutate(id_aldea  = sapply(id_aldea, function(x) ifelse(x %in% ids_tochange$raw_values, dpto_tochange%>%filter(raw_values == x)%>%
                                                                 pull(change_to)%>%c(), x)))

table(database_c_g_corrected_c$fixed)

## merging
real_names = database_c%>%
  mutate(s1_id_aldea = id_aldea)%>%
  dplyr::select(s1_id_aldea, orig_id_aldea)%>%
  left_join(database_c_g_corrected_c%>%
              mutate(s1_id_aldea = raw_id_aldea)%>%
              dplyr::select(s1_id_aldea, id_aldea, fixed ), by = 's1_id_aldea')%>%na.omit()%>%
  left_join(geo_data%>%
              data.frame()%>%
              dplyr::select(id_aldea, DEPTO, ALDEA,MUNI, GEOCODIGO), by = 'id_aldea')



coffee_data_df_filtered%>%
  mutate(plain_depto = header_cleaning(stringi::stri_trans_general(replace_accents(depto),"Latin-ASCII")))%>%
  mutate(plain_muni = header_cleaning(stringi::stri_trans_general(replace_accents(municipio),"Latin-ASCII")))%>%
  mutate(plain_aldea = header_cleaning(stringi::stri_trans_general(replace_accents(aldea),"Latin-ASCII")))%>%
  mutate(orig_id_aldea = paste0(plain_depto, '_' , plain_muni, '_', plain_aldea))%>%
  left_join(real_names%>%
              na.omit()%>%
              group_by(orig_id_aldea)%>%
              slice(1L), by = 'orig_id_aldea')%>%
  readr::write_excel_csv('results/coffee_database_real_locality.csv')

