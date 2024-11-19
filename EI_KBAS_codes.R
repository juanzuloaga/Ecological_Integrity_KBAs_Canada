
############################################################################
#                                                                          #
#             Implementing the Key Biodiversity Areas'                     #
#             Ecological Integrity criterion in Canada.                    #
#                          Scoping analysis.                               #
#                                                                          #
#                                                                          #
#  by Juan Zuloaga (Research Associate) & Andrew Gonzalez (Professor)      # 
#                     McGill University                                    #
#                                                                          #
#                             2024                                         #
############################################################################ 


# 0. PACKAGES -------
# List of Packages
  Packages <- c("geodata",          # To download datasets "Countries" and "HFP Venter"
                "landscapemetrics", # To calculate number of patches and their area
                "qgisprocess",      # 
                "ggplot2",          # To create plots (graphs and maps)
                "sf",               # To read vector files (polygons and lines)
                "sfheaders",
                "spatialEco",
                "stringr",
                "terra"             # To read raster files and analysis
                )

#install.packages(Packages) If needed

# Loading packages
  lapply(Packages, library, character.only =TRUE) 
  
# 1. SETTINGS -------
  
# Setting main folder as a default
  setwd("D:/EI_paper")

# Creating intermediate folder to save rasters
  
  if(!dir.exists("./Intermediate")){
      dir.create("./Intermediate")
      print("dir created")
    }else{
      print("dir exists")
    }

# Creating temporary folder to remove temp raster at the end of the process
  
  if(!dir.exists("./temp_to_REMOVE")){
      dir.create("./temp_to_REMOVE")
      print("dir created")
    }else{
      print("dir exists")
    }
    
# Set temporary folder
  terra::terraOptions(tempdir= "./temp_to_REMOVE")

# 2. TEMPLATE LAYERS & VARIABLES--------

# 2.1 Templates -------------
  # Canada boundaries
    Canada <- read_sf("./Data/Canada_aeac_diss.shp")
  
  # canada Provinces
 
  Canada_provinces <- read_sf("./Data/GeopoliticalAreas_p_nolakes.shp")
  
  # Raster template Canada 300m
    canada_template <- rast("./Intermediate/template_Can300m.tif")
    
  # Canada map to mask HFP
    countries <- geodata::world(resolution = 5,
                                level = 0,
                                path = "./Data/")
    
    Can <- countries[countries$NAME_0 == "Canada", ]
    
  # Ecoregions (Dinerstein et al. 2017)
    
    ecoregions <- read_sf("./Data/Ecoregion_Canada_clip.shp")
  
 
    
# 2.2 Some variables for analysis -----------
  
  # Minimun continous area to select (10,000Km2, 5,000Km2)
    # 10,000Km 2 is a reference from IUCN guidelines, however we tested the
    # effect of including smaller areas
    patch_size <- c(10000, 5000) # Testing 10,000Km2 and 5,000Km2 continous patches at a particular HFP
    
  # We tested for multiple HFP threshold
    # that is selecting areas of each HFP value
      threshold_hfp <- c(0,1,2,3,4,5)

  # We tested different thresholds for deforested areas, usinf the 
    # Forest Land scape Integrity Index (FLII),
    # e.g., greater than 9600 high forest integrity (well connected)
    flii_threshold <- c(6000, 8000, 9600)
    
  # CRS for to project ALL rasters (preserving area for calculations)
    crs_can <- "ESRI:102001"
    
  # Buffers for vectors (on each side of the lines or surrounding polygons)
    buffers_features <- c(150, 300, 450)
    
    
# 3. CREATING LAYERS FROM DATA SETS -------
  
   # We merged multiple global, national and provincial data sets
      # aimed to measure human pressure in Canada
      # - GLOBAL
        # - Landscape Integrity index (FLII, Graham et al. 2020), raster, res= ~300m
      # - NATIONAL: 
        # Human Foot print Canada: Hirsh et al 2022, raster, res= ~ 300m
      # - PROVINCIAL (Linear and polygons features)
        # Roads: Poley et al 2022
        # Boreal Ecosystem Anthropogenic Disturbances (BEAD): (Downloaded from https://open.canada.ca/data/en/dataset/afd0ce47-17c3-445c-b823-2f86409da2e0) 
        # Seismic lines (Poley ????)

  
    # Let's first rasterize linear data sets and
    # also create three raster layers for FLII, using different thresholds
    
# 3.1 Rasterizing linear features, using multiple buffers ---------

    # Lines and polygons were rasterized (using a Canada template 300m),
    # We used the Processing>Toolbox>rasterize (overwrite with Attribute) in QGIS (faster)
    # three buffers were used to rasterize these features (150, 300, 450m)
    # The QGIS process generates .tif layers with a burned values
    # We then we used the script below to create a map with only 1s,
      # and the rest NAs 
     
        name_folder <- c(#"BEAD",
                         #"Seismic_Poley",
                        # "Roads"
                         "HFI2021_abmi" #for this one use different reclassify values see within the function
                         )
        
    # Boreal Ecosystem Anthropogenic Disturbances
      #files_burned <- list.files("./Intermediate/Seismic_others_2010/raster_burned", pattern = "*.tif$")
    # Seismic Poley
      # files_burned <- list.files("./Intermediate/Seismic_Poley/raster_burned/", pattern = "*.tif$")
    # Roads
      #files_burned <- list.files("./Intermediate/Roads/raster_burned/", pattern = "*.tif$")

      for(i in 1:length(name_folder)){
          
          path_folder <- paste0("./Intermediate/", name_folder[i], "/raster_burned/")
          cat(paste0("Processing folder... ",  name_folder[i]), "\n")
          
          files_burned <- list.files(path_folder, pattern = "*.tif$")
        
        for(j in 1:length(files_burned)){
          
          cat(paste0("Processing raster...", files_burned[j]), "\n")
          
          
          lines_pol <-  rast(paste0(path_folder, files_burned[j]))
       
          # Reclassify values
          
           lines_pol[lines_pol == 0] <- NA
          
          # for Alberta HFI2021 use this
             # lines_pol[lines_pol > 0] <- 1
             # lines_pol[lines_pol == 0] <- NA
          
          if(!exists(paste0("./Intermediate/", name_folder[i], "/raster_binary/"))){
            dir.create(paste0("./Intermediate/", name_folder[i], "/raster_binary/"))
            
          }else{
            next
          }
                writeRaster(lines_pol,
                      paste0("./Intermediate/", name_folder[i], "/raster_binary/", files_burned[j], ".tif"),
                      overwrite=T)
        }
    }


# 3.2.  Creating a deforested areas layer from FLII -----------
      
  ## let's create a mask layer to use in a further step
      ## That is, values above the threshold (high FLII) will be removed, 
      ## so, we will keep only values with low FLII
      ## We tested three thresholds (FLII < 0.96, FLII < 0.80, and FLII < 0.60)
    
    # FLII    
      flii <- rast("./Data/FLII/flii_canada_aeac_recl_p.tif")

    # Remove values above threshold

    for(f in 1:length(flii_threshold)){
    
      cat(paste0("Processing....../Intermediate/FLII/flii_small_", flii_threshold[f], ".tif"), "\n")
        flii_t <- flii
        flii_t[flii_t > flii_threshold[f]]<-NA
      
      # Calculating number of patches
        flii_patches <- get_patches((flii_t*0)+1, directions=8, return_raster = T)
      
      # Calculating patches' area
        flii_patches_area <- lsm_p_area((flii_t*0)+1, directions = 8)
      
      # Reclassifying raster values using patches' area
        m1 <- c(flii_patches_area$id, (flii_patches_area$value*0.01))  # Convert values in hectares to Km2
        rclmat1 <- matrix(m1, ncol=2, byrow=F) # creating a matrix 
        flii_small <- terra::classify(flii_patches$layer_1$class_1, rclmat1)
        names(flii_small) <- paste0("flii_small_", flii_threshold[f])
      
      # Removing patches smaller than 10km2
        flii_small[flii_small <= 10] <- NA
        
        writeRaster(flii_small,
                    paste0("./Intermediate/FLII/flii_small_", flii_threshold[f], ".tif"),
                    overwrite=T)
    }
    

    
# 3.3. Connectivity Status Index (CSI) --------------------
    
    
    csi_list <- list.files("./Intermediate/CSI/",
                           pattern = ".shp$",
                           full.names = T)
    
    csi_list_names <- list.files("./Intermediate/CSI/",
                                 pattern = ".shp$",
                                 full.names = F)
    
    
    hfp_2_all_10000_r <- rast("./Intermediate/HFP_ALL/FLII_9600/hfp_10000_Km2/hfp_2.tif")
    
    
    dir.create("./Intermediate/CSI/csi_mean_vol/")
    dir.create("./Intermediate/CSI/All_reclass_csi/")
    dir.create("./Intermediate/CSI/csi_mean_vol_threshold/")
    
    csi_threshold <- c(90, 95)
    
    for(i in 1:length(csi_list)){
      
      cat(paste0("Rasterizing........", substr(csi_list_names[i], 1, 5)), "\n")
      
      # Ratserizing CSI
      
      csi_5r <- read_sf(csi_list[i])%>%
        st_transform(crs_can)%>%
        terra::rasterize(canada_template,
                         field="CSI_mean_v",
                         filename = paste0("./Intermediate/CSI/csi_mean_vol/",
                                           substr(csi_list_names[i], 1, 5),
                                           ".tif"),
                         background=NA,
                         overwrite=T)
      
      
      
      
      for(j in 1:length(csi_threshold)){
        
        cat(paste0("Selecting areas with threshold greater than.......", csi_threshold[j]), "\n")
        
        csi_5r_threshhold <- csi_5r
        
        csi_5r_threshhold[csi_5r_threshhold > csi_threshold[j]]<-NA
        
        writeRaster(csi_5r_threshhold,
                    filename = paste0("./Intermediate/CSI/csi_mean_vol_threshold/", substr(csi_list_names[i], 1, 5), "_", csi_threshold[j], ".tif"),
                    overwrite=T)
        
      }
    }
    
    
    
    
# 3.4  HydroBASINS Level 3  -------------------
    #(from https://www.hydrosheds.org/products/hydrobasins)
    basins_3_NA <- terra::vect("./Data/hybas_na_lev01-06_v1c/hybas_na_lev03_v1c.shp")
    basins_3_AR <- terra::vect("./Data/hybas_ar_lev01-06_v1c/hybas_ar_lev03_v1c.shp")
    basins_canada <- terra::union(basins_3_NA, basins_3_AR)%>%
      terra::intersect(Can)%>%
      terra::project(crs_can)
    
      
# 4. ANALYSIS --------------

# 4.1 COMPARING NATIONAL AND GLOBAL Human Foot Print data sets ---------
 
      # Firstly, we evaluated the effect of selecting a HFP threshold (0,1,2,3,4,5)
      # on the NUMBER of emerging continuous patches of low human pressure
      # and their AREA, comparing both National and Global HFP data sets)
      # Secondly, we tested the effect of removing each data set (e.g., roads, BEAD, etc) from the National HFP
      # Thirdly, we tested the effect of removing ALL data sets from the National HFP 
      
           
# 4.1.1 National HFP (Using Hirsh et al 2022, res= ~ 300m) ----------
      
# Selecting areas of low NATIONAL HFP (0, 1, 2, 3, 4 and 5) 

  # Here we select pixels with values below HFP threshold (testing: 0,1,2,3,4,5),
      # then we select continous areas (testing minumim size 10,000Km2 and 5,000Km2)
      
  #  Loading Human footprint - Canada (Hirch et al. 2022; 300m)
     hfp <- rast("./Data/hfp_Can_r300m_aeac.tif")
      
     cat(crayon::bold(paste0("Identifying areas of low human pressure", " ...."), '\n'))
    
     for(i in threshold_hfp){
    
        cat(crayon::red(paste0("Testing HFP threshold = ", unique(i), " ...."), '\n'))
    
         # Creating a directory to save temporary files that will be deleted after completing the process ....
        
          if(!dir.exists("./temp_to_REMOVE")){
            dir.create("./temp_to_REMOVE")
            print("Creating a directory to save temporary files...........dir created")
          }else{
            print("Creating a directory to save temporary files........dir exists!")
          }
        
          for(p in 1:length(patch_size)){
            cat(paste0("Selecting areas greater than....", patch_size[p]), '\n')
            
           # "Setting temp directory ....
            terra::terraOptions(tempdir="./temp_to_REMOVE/")
          
           cat(paste0("Removing HFP values greater than threshold_hfp = ",  unique(i)), '\n')
            hfp_t <- hfp
            hfp_t[hfp_t > i] <- NA
            
           cat(paste0("Calculating number of patches and their area", "\n"))
            # Calculating number of patches
            hfp_patches <- get_patches((terra::as.factor(hfp_t)*0)+1, directions=8, return_raster = T)
            
            # Calculating patches' area
            hfp_patches_area <- lsm_p_area(terra::as.factor(hfp_t*0)+1, directions = 8)
            
            cat(paste0("Reclassifying patches using area size", "\n"))
            
            # Reclassifying raster values using patches' area
            m2 <- c(hfp_patches_area$id, (hfp_patches_area$value*0.01))  # Convert values in hectares to Km2
            rclmat2 <- matrix(m2, ncol=2, byrow=F) # creating a matrix 
            hfp_area <- terra::classify(hfp_patches$layer_1$class_1, rclmat2)
            
            cat(paste0("Removing patches below area size threshold.....", "\n"))
           
              hfp_area_patch <- hfp_area
              hfp_area_patch[hfp_area_patch < patch_size[p]] <- NA
            
            cat("Saving raster", '\n')
            
            if(!dir.exists(paste0("./Intermediate/HFP/", "hfp_", patch_size[p], "_Km2/"))){
              dir.create(paste0("./Intermediate/HFP/", "hfp_", patch_size[p], "_Km2/"))
              print("dir created")
            }else{
              print("dir exists")
            }
            
            terra::writeRaster(hfp_area_patch,
                       paste0("./Intermediate/HFP/hfp_", patch_size[p], "_Km2/hfp_",  i, ".tif"),
                       overwrite=T)
          }
          
         unlink("./temp_to_REMOVE")
      }
      
  
# Creating a Master Table (number of patches and their size)
  
  # Source the function
   source("./master_tables.R")
  
  # Folders to read
   input_folder <- c("./Intermediate/HFP/hfp_10000_Km2/",
                    "./Intermediate/HFP/hfp_5000_Km2/")
  
  # Output folder
   output_folder <- "./Intermediate/HFP/"
  
  # Run function
   master_tables(threshold_hfp, input_folder, output_folder)
     
  
# 4.1.2 Global HFP ------------
    
# 4.1.2.1.Venter et al (1993 and 2009), raster, res= ~1000m ----------
    
  # Years to load
    hfp_years <- c(1993, 2009)
    
  # Let's select Venter HFP for Canada (1993 and 2009)
    for(h in 1:length(hfp_years)){
      
      cat(paste0("Loading HFP global data......", hfp_years[h]), "\n")
      hfp_venter <- geodata::footprint(year = hfp_years[h],
                                       path ="./Data/HFP_Venter/")
      
      cat(paste0("Masking HFP using Canada boundaries and projecting it....", hfp_years[h]), "\n")
      
      hfp_Can <- 
        terra::crop(hfp_venter, Can)%>%
        terra::mask(Can)%>%
        terra::project(crs_can,
                       method="bilinear",
                       res=1000,
                       filename= paste0("./Intermediate/HFP_Venter/HFP", hfp_years[h], "_Can.tif"),
                       overwrite=T)
    }
    
    
    #  Let's call the function "select_using_threshold.R"
      # to to select areas from Human footprint using multiple thresholds,
      # then calculate the number of patches, their size and save as a .tif layer
    
    source("./select_using_threshold.R")
    
    # Input folders
    input_folder <- "./Intermediate/HFP_Venter/"
    
    # Output folder
    output_folder <- "./Intermediate/HFP_Venter/"
    
    # Run the function
    mask_calc_patches_size(input_folder,
                           threshold_hfp,
                           patch_size,
                           output_folder)
    
# Creating a Master Table (number of patches and their size)
    
    # Source the function
      source("./master_tables.R")
    
    # Years to load
      hfp_years <- c(1993, 2009)
    
      for(i in 1:length(hfp_years)){
        
        # Folders to read
        input_folder <- c(paste0("./Intermediate/HFP_Venter/HFP", hfp_years[i], "_Can/hfp_10000_Km2/"),
                          paste0("./Intermediate/HFP_Venter/HFP", hfp_years[i], "_Can/hfp_5000_Km2/"))
        
        # Output folder
        output_folder <- paste0("./Intermediate/HFP_Venter/HFP", hfp_years[i], "_Can/")
        
        
        # Run function
        master_tables(threshold_hfp, input_folder, output_folder)
      }
    

# 4.1.2.2. Williams et al 2020, res= ~1000m ----------   

      # List of files
        hfp_williams <- list.files("./Data/HFP_Williams/Human_footprint_maps",
                                   pattern = "tif$",
                                   full.names = T)
        hfp_williams_names <- list.files("./Data/HFP_Williams/Human_footprint_maps",
                                   pattern = "tif$",
                                   full.names = F)
      
      # Canada map to mask HFP
        countries <- geodata::world(resolution = 5,
                                  level = 0,
                                  path = "./Data/")
      
        Can_MOL <- countries[countries$NAME_0 == "Canada", ]%>%
        project("ESRI:54009")
       
      # Let's select Williams HFP for Canada 
      for(h in 1:length(hfp_williams)){

        hfp_williams_r <- rast(hfp_williams[h])
        
        cat(paste0("Masking HFP using Canada boundaries and projecting it....", hfp_williams_names[h]), "\n")
        
        hfp_Can <- 
          terra::crop(hfp_williams_r, Can_MOL)%>%
          terra::mask(Can_MOL)%>%
          terra::project(crs_can,
                         method="bilinear",
                         res=1000,
                         filename= paste0("./Intermediate/HFP_Williams/", hfp_williams_names[h]),
                         overwrite=T)
     
      }
      
   # Let's call the function "select_using_threshold.R"
      # to to select areas from Human footprint using multiple thresholds,
      # then calculate the number of patches, their size and save as a .tif layer
      
      source("./select_using_threshold.R")
      
      # Input folders
      input_folder <- "./Intermediate/HFP_Williams/"
     
      # Output folder
      output_folder <- "./Intermediate/HFP_Williams/"
      
      # Run the function
      mask_calc_patches_size(input_folder,
                             threshold_hfp,
                             patch_size,
                             output_folder)
      
# Creating a Master Table (number of patches and their size)
      
    # Source the function
    source("./master_tables.R")
    
    # Years to load
    hfp_years <- c(2000, 2005, 2010, 2013)
    
    for(i in 1:length(hfp_years)){
      
      # Folders to read
      input_folder <- c(paste0("./Intermediate/HFP_Williams/hfp", hfp_years[i], "_merisINT/hfp_10000_Km2/"),
                        paste0("./Intermediate/HFP_Williams/hfp", hfp_years[i], "_merisINT/hfp_5000_Km2/"))
      
      # Output folder
      output_folder <- paste0("./Intermediate/HFP_Williams/hfp", hfp_years[i], "_merisINT/")
      
      
      # Run function
      master_tables(threshold_hfp, input_folder, output_folder)
    }
  
    
# 4.1.2.3. Theobald et al. 2017 change, res= ~1000m ----------       
    
    
    GHM_Theobald_list <- list.files("./Data/GHM_Theobald/gHMv1_1000m_2017_static_stressors",
                                    pattern = ".tif$",
                                    full.names = T)
    
    
    GHM_Theobald_names <- list.files("./Data/GHM_Theobald/gHMv1_1000m_2017_static_stressors",
                                    pattern = ".tif$",
                                    full.names = F)
    dir.create("./Intermediate/GHM_Theobald/GHM_Theobald_2017_1000/")
    
    # the second raster has a different crs, 
    # so, we have to use Can_prof instead of Can and run it manually (that is select 2 within the list of rasters)
    Can_3857 <- terra::project(Can, "EPSG:3857") 
    
    for(i in 1:length(GHM_Theobald_list)){
      
      cat(paste0("processing........", GHM_Theobald_names[[i]]), "\n")
      
      r1 <- terra::rast(GHM_Theobald_list[[i]])%>%
            terra::crop(Can_3857)%>%
          # terra::crop(Can)
            terra::mask(Can_3857)%>%
            terra::project(crs_can,
                           "bilinear",
                           res=1000,
                           filename= paste0("./Intermediate/GHM_Theobald/GHM_Theobald_2017/",
                                            GHM_Theobald_names[[i]]), overwrite=T)

    }
    
    
    GHM_Theobald_Can <- list.files("./Intermediate/GHM_Theobald/GHM_Theobald_2017/",
                                   pattern = ".tif$",
                                   full.names = T)
    
   hfp_venter_temp <-  rast("./Intermediate/HFP_Venter/HFP1993_Can.tif")%>%
     terra::project(crs_can, res=1000)
 e <- terra::ext(-3558700, 3448300, 181334.5, 4956334)
   
    
    
    GHM_3 <- (rast(GHM_Theobald_Can[[3]])/65536)
    GHM_4 <- (rast(GHM_Theobald_Can[[4]])/65536)
    GHM_5 <- (rast(GHM_Theobald_Can[[5]])/65536)
    GHM_1 <- (rast(GHM_Theobald_Can[[1]])/65536)%>%
      terra::resample(GHM_5)
    GHM_2 <- (rast(GHM_Theobald_Can[[2]])/65536)%>%
      terra::resample(GHM_5)
    
    ghm_stack <- c(GHM_1, GHM_2, GHM_3, GHM_4, GHM_5)

    GHM_all_fuzz <- (1 - ( (1 - GHM_1) * 
                           (1 - GHM_2) *
                           (1 - GHM_3) *
                           (1 - GHM_4) *
                           (1 - GHM_5) ) )
    
    writeRaster(GHM_all_fuzz, "./Intermediate/GHM_Theobald/GHM_Theobald_2017_1000/GHM_Theobald_2017_1000.tif",
                overwrite=T)
    
      
      # Let's call the function "select_using_threshold.R"
      # to to select areas from Human footprint using multiple thresholds,
      # then calculate the number of patches, their size and save as a .tif layer
      
 
    threshold_hfp <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
    
   
    
    source("./select_using_threshold.R")
    
    # Input folders
    input_folder <- "./Intermediate/GHM_Theobald/GHM_Theobald_2017_1000/"
    
    # Output folder
    output_folder <- "./Intermediate/GHM_Theobald/GHM_Theobald_2017_1000/"
    
    # Run the function
    mask_calc_patches_size(input_folder,
                           threshold_hfp,
                           patch_size,
                           output_folder)
    
      
      # Creating a Master Table (number of patches and their size)
      
      # Source the function
      source("./master_tables.R")
      
      # Years to load
      hfp_years <- c(2017)
      
      for(i in 1:length(hfp_years)){
        
        # Folders to read
        input_folder <- paste0("./Intermediate/GHM_Theobald/GHM_Theobald_2017_1000/GHM_Theobald_", hfp_years[i], "_1000/hfp_5000_Km2/")
        
        # Output folder
        output_folder <- paste0("./Intermediate/GHM_Theobald/GHM_Theobald_2017_1000/GHM_Theobald_", hfp_years[i], "_1000/")
        
        
        # Run function
        master_tables(threshold_hfp, input_folder, output_folder)
      }
      
      
      
# 4.1.2.4. Theobald et al. 2017 change, res= ~300m ----------       
      
      Theobald_2017_1 <- rast("./Data/GHM_Theobald/GHM_2017_300/gHMv1_300m_2017_static-0000000000-0000000000.tif")
      Theobald_2017_2 <- rast("./Data/GHM_Theobald/GHM_2017_300/gHMv1_300m_2017_static-0000046592-0000000000.tif")
      
      Theobald_2017_merge <- terra::mosaic(Theobald_2017_1,
                                           Theobald_2017_2)
      
      Can_3857 <- terra::project(Can, "EPSG:3857")
      
      
      Theobald_2017_cAN <- crop(Theobald_2017_merge, Can_3857)%>%
        terra::mask(Can_3857)%>%
        terra::project(canada_template, "bilinear", 300)
      
      dir.create("./Intermediate/GHM_Theobald/GHM_Theobald_2017_300/")
      
      writeRaster((Theobald_2017_cAN/65536), "./Intermediate/GHM_Theobald/GHM_Theobald_2017_300/GHM_Theobald_2017_300.tif",
                  overwrite=T)
      
  
      # Let's call the function "select_using_threshold.R"
      # to to select areas from Human footprint using multiple thresholds,
      # then calculate the number of patches, their size and save as a .tif layer
      
      
      threshold_hfp <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
      
      
      
      source("./select_using_threshold.R")
      
      # Input folders
      input_folder <- "./Intermediate/GHM_Theobald/GHM_Theobald_2017_300/"
      
      # Output folder
      output_folder <- "./Intermediate/GHM_Theobald/GHM_Theobald_2017_300/"
      
      # Run the function
      mask_calc_patches_size(input_folder,
                             threshold_hfp,
                             patch_size,
                             output_folder)
      
      
      # Creating a Master Table (number of patches and their size)
      
      # Source the function
      source("./master_tables.R")
      
      # Years to load
      hfp_years <- c(2017)
      
      for(i in 1:length(hfp_years)){
        
        # Folders to read
        input_folder <- paste0("./Intermediate/GHM_Theobald/GHM_Theobald_2017_300/GHM_Theobald_", hfp_years[i], "_300/hfp_10000_Km2/")
        
        # Output folder
        output_folder <- paste0("./Intermediate/GHM_Theobald/GHM_Theobald_2017_300/GHM_Theobald_", hfp_years[i], "_300/")
        
        
        # Run function
        master_tables(threshold_hfp, input_folder, output_folder)
      }
      
      
      
       
      
# 4.2 MERGING multiple National and Provincial/Territorial ----------
    #Human Pressure data sets

# 4.2.1 Removing FLII from National HFP  -------------

  #  Let's call the function "mask_calc_patches_size "
      # to mask (remove) FLII from Human footprint,
      # then calculate the number of patches, their size and save as a .tif layer

      source("./mask_calc_patches_size.R")
    
    # mask raster FLII (we used three thresholds, 6000, 8000, 9600)
      FLII_small <-  "./Intermediate/FLII/"

    # Input folders
      input_folder <- c("./Intermediate/HFP/hfp_10000_Km2/",
                        "./Intermediate/HFP/hfp_5000_Km2/")
    # Output folder
      output_folder <- "./Intermediate/HFP_FLII/"

    # Run the function

      mask_calc_patches_size(input_folder,
                             FLII_small,
                             patch_size,
                             output_folder)

      
  # Creating a Master Table (number of patches and their size)
      # Change the input and output folders 6000, 8000, 9600
      
    # Source the function
      source("./master_tables.R")
      
    # Folders to read
      input_folder <- c("./Intermediate/HFP_FLII/FLII_9600/hfp_10000_Km2/",
                         "./Intermediate/HFP_FLII/FLII_9600/hfp_5000_Km2/")
    # Output folder
      output_folder <- "./Intermediate/HFP_FLII/FLII_9600/"

    # Run function
      master_tables(threshold_hfp, input_folder, output_folder)
      
# 4.2.2 Removing Seismic lines-Poley from National HFP ----------------
      
      # (testing three buffers 150, 300, 450)
      
      #  Let's call the fucntion "mask_calc_patches_size "
      # to mask (remove) Seismic lines from Human footprint,
      # then calculate the number of patches, their size and save as a .tif layer
      
      source("./mask_calc_patches_size.R")
      
     # Input folders
      input_folder <- c("./Intermediate/HFP/hfp_10000_Km2/",
                        "./Intermediate/HFP/hfp_5000_Km2/")
      
     # Mask raster seismic Poley (we used three thresholds, 150m, 300m, 450)
      seismic_poley <-  "./Intermediate/Seismic_Poley/raster_binary"
     
      # Output folder
      output_folder <- "./Intermediate/HFP_Seismic_Poley/"
      
    
      # Run the function
      mask_calc_patches_size(input_folder,
                             seismic_poley,
                             patch_size,
                             output_folder)
     
      
  # Creating a Master Table (number of patches and their size)
  
      # Source the function
      source("./master_tables.R")
      buffers_features <- c(150, 300, 450)
      
      for(i in 1:length(buf)){
      # Folders to read
      input_folder <- c(paste0("./Intermediate/HFP_Seismic_Poley/seismic_buf", buffers_features[i], "m/hfp_10000_Km2/"),
                        paste0("./Intermediate/HFP_Seismic_Poley/seismic_buf", buffers_features[i], "m/hfp_5000_Km2/"))
      
      # Output folder
      output_folder <- paste0("./Intermediate/HFP_Seismic_Poley/seismic_buf", buffers_features[i], "m/")
      
      
      # Run function
      master_tables(threshold_hfp, input_folder, output_folder) 
      
      }
    
# 4.2.3. Removing  Boreal Ecosystem Anthropogenic Disturbances from National HFP---------
      
    # Merging and saving rasters: lines and polygons buffered
     
      for(i in 1:length(buffers_features)){
      terra::writeRaster(terra::merge(rast(paste0("./Intermediate/BEAD/raster_binary/lines_buf", buffers_features[i], "m", ".tif")),
                                      rast(paste0("./Intermediate/BEAD/raster_binary/polygons_buf", buffers_features[i], "m", ".tif"))),
         paste0("./Intermediate/BEAD/merged_lines_polygons_buffered/bead_", buffers_features[i], "m", ".tif"),
         overwrite=T)
      }
         
         
          #  Let's call the fucntion "mask_calc_patches_size " to mask (remove) new raster layers from Human footprint,
      # then calculate the number of patches, their size and save as a .tif layer
      
      source("./mask_calc_patches_size.R")
      
    # Input folders
      input_folder <- c("./Intermediate/HFP/hfp_10000_Km2/",
                        "./Intermediate/HFP/hfp_5000_Km2/")
      
    # Mask raster seismic Poley (we used three thresholds, 150m, 300m, 450)
      seismic_poley <-  "./Intermediate/BEAD/merged_lines_polygons_buffered"
    
    # Output folder
      output_folder <- "./Intermediate/HFP_BEAD/"
      
    
      # Run the function
      
      mask_calc_patches_size(input_folder,
                             seismic_poley,
                             patch_size,
                             output_folder)
      
 # Creating a Master Table (number of patches and their size)
      
      # Source the function
      source("./master_tables.R")

      for(i in 1:length(buffers_features)){
        # Folders to read
        input_folder <- c(paste0("./Intermediate/HFP_BEAD/bead_", buffers_features[i], "m/hfp_10000_Km2/"),
                          paste0("./Intermediate/HFP_BEAD/bead_", buffers_features[i], "m/hfp_5000_Km2/"))
        
        # Output folder
        output_folder <- paste0("./Intermediate/HFP_BEAD/bead_", buffers_features[i], "m/")
        
        
        # Run function
        master_tables(threshold_hfp, input_folder, output_folder) 
        
      }
      
      
# 4.2.4. Removing Roads_Poley from National HFP--------
      
      # merging Provinces roads
      
      roads_buffer <- c("buf_150", "buf_300", "buf_450")
      for(i in 1:length(roads_buffer)){
      cat(paste0("Merging rasters......", roads_buffer[i]), "\n")
      list_roads <- list.files(paste0("./Intermediate/Roads_Poley/raster_binary/", roads_buffer[i]),  pattern = ".tif$", full.names = T)%>%
        terra::sprc(lapply(rast))%>%
        terra::merge()
      
       cat("Saving raster....", "\n")
      terra::writeRaster(list_roads, paste0("./Intermediate/Roads_Poley/all_roads_buffered/", roads_buffer[i], ".tif"),
                         overwrite=T)
      }
      

      
      #  Let's call the fucntion "mask_calc_patches_size "
      # to mask (remove) ROADS from Human footprint,
      # then calculate the number of patches, their size and save as a .tif layer
      
      source("./mask_calc_patches_size.R")
      
      # Input folders
      input_folder <- c("./Intermediate/HFP/hfp_10000_Km2/",
                        "./Intermediate/HFP/hfp_5000_Km2/")
      
      # Mask raster Roads Poley (we used three thresholds, 150m, 300m, 450)
      roads_poley <-  "./Intermediate/Roads_Poley/all_roads_buffered"
      
      # Output folder
      output_folder <- "./Intermediate/HFP_Roads_Poley/"
      
      
      # Run the function
      
      mask_calc_patches_size(input_folder,
                             roads_poley,
                             patch_size,
                             output_folder)
      
 # Creating a Master Table (number of patches and their size)
      
      # Source the function
      source("./master_tables.R")

      for(i in 1:length(buffers_features)){
        # Folders to read
        input_folder <- c(paste0("./Intermediate/HFP_Roads_Poley/buf_", buffers_features[i], "/hfp_10000_Km2/"),
                          paste0("./Intermediate/HFP_Roads_Poley/buf_", buffers_features[i], "/hfp_5000_Km2/"))
        
        # Output folder
        output_folder <- paste0("./Intermediate/HFP_Roads_Poley/buf_", buffers_features[i], "/")
        
        
        # Run function
        master_tables(threshold_hfp, input_folder, output_folder) 
        
      }
      
      
      
# 4.2.5 Removing CSI from National HFP  -------------
      
      #  Let's call the function "mask_calc_patches_size "
      # to mask (remove) CSI from Human footprint,
      # then calculate the number of patches, their size and save as a .tif layer
      
      source("./mask_calc_patches_size.R")
      
      # mask raster FLII (we used three thresholds, 6000, 8000, 9600)
      CSI_files <-  "./Intermediate/CSI/csi_mean_vol_threshold/"
      
      # Input folders
      input_folder <- c("./Intermediate/HFP/hfp_10000_Km2/",
                        "./Intermediate/HFP/hfp_5000_Km2/")
      
      # Output folder
      output_folder <- "./Intermediate/HFP_CSI/"
      
      # Run the function
      
      mask_calc_patches_size(input_folder,
                             CSI_files,
                             patch_size,
                             output_folder)
      
      
    # Creating a Master Table (number of patches and their size)
      # Change the input and output folders 6000, 8000, 9600
      
      # Source the function
      source("./master_tables.R")
      
      csi_folders <- list.dirs("./Intermediate/HFP_CSI/",
                               recursive=F,
                               full.names = F)
      
      for(f in 1:length(csi_folders)){
        
        
        # Folders to read
        cat(paste0("Processing.....", csi_folders[f]), "\n")
        
        input_folder <- c(paste0("./Intermediate/HFP_CSI/", csi_folders[f], "/hfp_10000_Km2/"),
                          paste0("./Intermediate/HFP_CSI/", csi_folders[f], "/hfp_5000_Km2/"))
        
        # Output folder
        output_folder <- paste0("./Intermediate/HFP_CSI/", csi_folders[f], "/")
        
        # Run function
        master_tables(threshold_hfp, input_folder, output_folder)
        
      }
      
     
      
# 4.2.6 Merging ALL features HFP ---------------   

    # Merging ALL features
      
      # For linear features we use buffer = 300m and for FLII we tested 9600, 8000, 6000
      
     # FLII_folder <- list.files("./Intermediate/FLII",  pattern = ".tif$", full.names = T)%>%
     #   lapply(rast)
     # FLII_folder_names <- list.files("./Intermediate/FLII",  pattern = ".tif$", full.names = F)
      
      FLII_9600 <- rast("./Intermediate/FLII/FLII_9600.tif")
      
      Roads_Poley300 <- rast("./Intermediate/Roads_Poley/all_roads_buffered/buf_300.tif")
      
      BEAD300 <- rast("./Intermediate/BEAD/merged_lines_polygons_buffered/bead_300m.tif")
      
      seismic_Poley300 <- rast("./Intermediate/Seismic_Poley/raster_binary/seismic_buf300m.tif.tif")
      
      cs_7_95 <- rast("./Intermediate/CSI/csi_mean_vol_threshold/csi_7_95.tif")
      
    #  for(i in 1:length(FLII_folder)){
      
     
    #  cat("Merging rasters......", "\n")
      
      all_features300 <- list(Roads_Poley300,
                              BEAD300,
                              seismic_Poley300,
                              FLII_9600,
                              cs_7_95)%>%
      terra::sprc()%>%
        terra::merge(filename = "./Intermediate/All_features/all_features300.tif")
      
      #cat("Saving raster....", "\n")
   #   terra::writeRaster(all_features300,
     #                    "./Intermediate/All_features/all_features300.tif",
      #                                           overwrite=T)
      
   #   }
      
# 4.2.7 Removing ALL FEATURES from National HFP ---------------   
      
  #  Let's call the function "mask_calc_patches_size"
      # to mask (remove) ALL FEATURES from Human footprint,
      # then calculate the number of patches, their size and save as a .tif layer
      
      source("./mask_calc_patches_size.R")
      
      # Input folders
      input_folder <- c(#"./Intermediate/HFP/hfp_10000_Km2/")
      #,
                        "./Intermediate/HFP/hfp_5000_Km2/")
      
      # Mask All features
      all_features <-  "./Intermediate/All_features/"
      
      # Output folder
      output_folder <- "./Intermediate/HFP_ALL/"

      # Run the function
      
      mask_calc_patches_size(input_folder,
                             all_features,
                             patch_size,
                             output_folder)
      
  # Creating a Master Table (number of patches and their size)

      # Source the function
      source("./master_tables.R")
      
    
     
      
      # Folders to read
      input_folder <- c("./Intermediate/HFP_ALL/all_features300/hfp_10000_Km2/",
                        "./Intermediate/HFP_ALL/all_features300/hfp_5000_Km2/")
      
      # Output folder
      output_folder <- "./Intermediate/HFP_ALL/all_features300/"
      
      
      # Run function
      master_tables(threshold_hfp, input_folder, output_folder)
      
      


      
# 4.3. Comparing with Provincial datasets  --------------------     
    
# 4.3.1 using ALBERTA HFI ------------------
      
