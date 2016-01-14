twrex_dict <- list()

## Types: LOCATION-LOCATION
twrex_dict[["dbo-location"]] <- "twrex-locationinlocation"
twrex_dict[["dbo-ispartof"]] <- "twrex-locationinlocation"
twrex_dict[["dbo-country"]] <- "twrex-locationinlocation"
twrex_dict[["dbo-archipelago"]] <- "twrex-locationinlocation"
twrex_dict[["dbo-mountainrange"]] <- "twrex-locationinlocation"

## Types: LOCATION-PERSON
twrex_dict[["dbo-birthplace"]] <- "twrex-birthplace"
twrex_dict[["dbo-nationality"]] <- "twrex-birthplace"
twrex_dict[["dbo-stateoforigin"]] <- "twrex-birthplace"

## Types: LOCATION-THING 
twrex_dict[["dbo-location"]] <- "twrex-thinginlocation"
twrex_dict[["dbo-state"]] <- "twrex-thinginlocation"
twrex_dict[["dbo-locationcity"]] <- "twrex-thinginlocation"
twrex_dict[["dbo-locatedinarea"]] <- "twrex-thinginlocation"
twrex_dict[["dbo-headquarter"]] <- "twrex-thinginlocation"
twrex_dict[["dbo-city"]] <- "twrex-thinginlocation"

twrex_dict[["dbo-territory"]] <- "twrex-warinlocation"
twrex_dict[["dbo-place"]] <- "twrex-warinlocation"

## Types: PERSON-THING
# twrex_dict[["???"]] <- "twrex-officialname"

twrex <- c("twrex-locationinlocation",
           "twrex-birthplace", 
           "twrex-thinginlocation", 
           "twrex-warinlocation")
