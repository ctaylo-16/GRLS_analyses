household_fuel_groups <- function() {
  groups <- list(
    electric = c(
      "electric", "geothermal electric", "electric but geothermal",
      "elec gerothermal", "Electric Heat Pump", "electric fire place",
      "electric/induction (magnetic)", "induction cooktop", "microwave",
      "electric/microwave", "microwave (electric)", "george forman grill",
      "microwave?", "microwave and electric oven", "microwave oven"
    ),
    natural_gas = c(
      "natural gas boiler", "gas hydronic heat system", "gas forced air",
      "gas-heated hot water", "gas heated hot water",
      "hot water heated by natural gas", "in floor hot water heating from gas",
      "in floor hot water heating with natural gas",
      "hot water heated floors  water heated by natural gas",
      "infloor hot water heating  water heated w/gas",
      "inflor heating with hot water heated by a gas furnace",
      "infloor heating with natural gas no forced air furnace",
      "boiler system fueled by propane",
      "hydronic heat fueled by propane h2o heaters",
      "hydronic in floor heating fueled by propane",
      "radiant floor heat powered by propane boiler",
      "in floor heating with natural gas", "radiant water via natural gas",
      "natural gas fireplace little use", "occassionally gas fireplace",
      "propane until 1 year ago-switched to natural gas",
      paste0(
        "natural gas for water heater, dryer and fireplace (turned off),; ",
        "i often 'smell gas,' but gas co. has not found leaks"
      ),
      "water/propane (boiler unit)", "natural gas and electric",
      "outdoor gas grill", "natural gass grill"
    ),
    oil = c(
      "oil", "oil fired hot water", "kerosene", "kerosine", "kerosens",
      "kerosene for emergencies", "small oil radiator",
      "free satnding plug in oil heater", "electric oil units",
      "kerosene heaters/hardly ever used"
    ),
    propane = c(
      "propane", "hydronic heat fueled by propane",
      "radiant floor heat powered by propane boiler", "outdoor grill propane"
    ),
    wood = c(
      "wood", "wood pellets", "wood pellet", "wood pellet stove",
      "hot water from outside wood furnace", "outside wood furnace", "pellets",
      "hot water heated by outside wood furnace", "outside wood stove",
      "outside woodstove", "pellet stove", "occasional wood stove",
      "fireplace wood", "pellets and wood stoves", "charcoal",
      "grill(charcoal)", "smoker", "barbecue", "charcoal / wood grill",
      "smoker grill", "charcol", "bbq - pellets", "bbq - wood pellets",
      "charcoal grill", "wood - outdoor grill", "charcoal barbecue",
      "charcoal and wood chips", "charcoal grill outside", "charcoal/wood",
      "charcoal with propane starter", "wood pellt bbq", "charcoal grille",
      "bbq wood pellets", "grilling,charcoal", "grilol charcol", "wood chips",
      "w00d"
    ),
    renewable = c(
      "solar", "passive solar", "geothermal", "radiant/geo thermal",
      "geo-thermal", "geo thermal", "geothermal heat pumps", "geo heat pump",
      "gio thurmo", "gio thurmal", "giothurmal", "ground source heat pump",
      "inground heat pump", "inground heat pump source", "in ground heat pump",
      "geothermal/electric heat exchanger", "geothermal heat exchange",
      "geothermal heat pump", "geo thermal heat pump",
      "heat exchanger with geothermal", "gero thermal - electric", "sun",
      "geothermal heating", "gerothermal", "solar hot water", "geo thurmo"
    )
  )

  groups
}

household_pipe_groups <- function() {
  list(
    metal = c(
      "mix of copper and pvc", "mixed copper and pvc", "galvinazed iron",
      "salvanized", "some copper, some pvc", "both copper & plastic",
      "metal and pvc", "iron", "iron and copper", "lead probably", "lead",
      "galvanized steel", "copper and plastic", "some copper/some pvc",
      "mixture of all above", "copper except under sinks which is pvc",
      "combination of copper and pvc  pvc appears under sinks etc.  ",
      "i think copper but pvc under sink and other areas where water enters the house",
      "metal/pvc", "galvanized", "cast iron", "copper/metal/pvc", "cast",
      "pex and copper", "pex, cu", "pex, with copper manifolds",
      "galvenized steel", "galvinized", "copper and pvc",
      "copper/metal and pvc/plastic", "both copper and pvc",
      "maybe copper/metal also", "some copper", "copper, cpvc",
      "both metal and plastic",
      "probably a mixture of copper/metal & pvc/plastic", "pvc/copper",
      "both metal & pvc", "copper,metal, pvc", "mix of copper metal and pvc",
      "galvenized", "copper, pvc and metal", "copper/pvc", "pvc & copper",
      "copper plastic", "copper pvs stainles steel", "both pvc and copper",
      "main-copper inside-plasticc", "copper & pvc", "steel"
    ),
    plastic = c(
      "mix of copper and pvc", "mixed copper and pvc", "pex", "pec",
      "some copper, some pvc", "both copper & plastic", "metal and pvc",
      "copper and plastic", "some copper/some pvc", "mixture of all above",
      "k-peck", "copper except under sinks which is pvc",
      "combination of copper and pvc  pvc appears under sinks etc.  ",
      "i think copper but pvc under sink and other areas where water enters the house",
      "metal/pvc", "copper/metal/pvc", "pex and copper", "pe", "pex, cu",
      "pex, with copper manifolds", "pecs", "poly", "copper and pvc", "pvc",
      "cvc", "copper/metal and pvc/plastic", "both copper and pvc", "cpvc",
      "pvex", "pvc and copper", "copper, cpvc", "cpcv",
      "both metal and plastic",
      "probably a mixture of copper/metal & pvc/plastic", "pvc/copper",
      "both metal & pvc", "copper,metal, pvc", "mix of copper metal and pvc",
      "polybutylene", "copper, pvc and metal", "copper/pvc", "pvc & copper",
      "plex", "pic system", "pec system", "plastic", "copper plastic",
      "copper pvs stainles steel", "both pvc and copper",
      "main-copper inside-plasticc", "copper & pvc", "peg", "pec plastic",
      "pex tubing"
    )
  )
}

household_water_groups <- function() {
  list(
    municipal = c(
      "municipal", "municipal at home, well at work with me daily",
      "municipal well", "filtered municipal", "municipal from lake",
      "municipal filtered via reverse osmosis",
      "municiapl wells with home filtration system",
      "municipal with town well water", "highly filtered municipal",
      "private through municipal", "municipal and well",
      "both municipal and well", "municipal and private well",
      "municipal filtered", "municipal with a reverse osmosis filtration unit",
      "municipal but reverse osmosis filtered", "filtered muni water",
      "municipal but reverse osmosis water system"
    ),
    groundwater = c(
      "well", "ground", "underground aquifers", "underground aquifer",
      "community owned wells", "town well water", "town well",
      "community well w/chlorine", "community well chlorine filter",
      "subdivision well",
      paste0(
        "community well for subdivision  has chloroine  we filter all chlorine ",
        "out and have a filtration system"
      ),
      "well water with chlorine filter",
      paste0(
        "comunity well with a clorine station  we have a filter in our home ",
        "to filter out the chlorine"
      ),
      "well and municipal", "both well and municipal", "well 50%, municipal 50%",
      "fox river and well", "1/2 well 1/2 municipal", "reverse osmosis from well",
      "aquifer", "town wells", " community well, treated & filtered",
      "wel and municipal", "well and spring", "well water goes to municipal",
      "community well water", "well/bottle/city", "aquafer"
    ),
    spring_surface = c(
      "berkeley springs spring water", "spring", "hicknley springs",
      "spring water", "lake water via city",
      "neighborhood water tanks from lake tahoe water", "lake tahoe",
      "lake michigan", "natural spring", "spring on property",
      "lake filtered/treated", "lake filtered system"
    ),
    bottled = c(
      "bottled", "bottled spring water", "bottled water",
      "bottled water/municipal", ".bottle", "bottled spring",
      "dogs drink bottled", "bottled and municipal"
    ),
    rainwater_cistern = c(
      "cistern", "rain fed cistern", "rain fed cistern filtered and uv sanitized",
      "uv and filtered rain filled cistern", "rain water", "rain",
      "rain water tanks", "rainwater", "rain water "
    )
  )
}

household_mode_columns <- function() {
  c(
    "area_type", "house_type", "water_source", "pipes_metal_any",
    "pipes_plastic_any", "heating_fuel_primary", "cooking_fuel_primary",
    "heating_fuel_secondary", "cooking_fuel_secondary"
  )
}

location_mode_columns <- function() {
  c("region_name", "state", "zip")
}

normalise_household_input <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ {
          value <- tolower(as.character(.x))
          replace(value, is.na(value) | value == "", "no_records_available")
        }
      )
    )
}

recode_household_fuel <- function(primary, other) {
  groups <- household_fuel_groups()

  dplyr::case_when(
    other %in% groups$electric ~ "electric",
    other %in% groups$natural_gas ~ "natural_gas",
    other %in% groups$oil ~ "oil",
    other %in% groups$propane ~ "propane",
    other %in% groups$wood ~ "wood",
    other %in% groups$renewable ~ "renewable",
    TRUE ~ primary
  )
}

recode_household_water <- function(primary, other) {
  groups <- household_water_groups()

  dplyr::case_when(
    other %in% groups$municipal ~ "municipal",
    other %in% groups$groundwater ~ "groundwater",
    other %in% groups$spring_surface ~ "spring_surface",
    other %in% groups$bottled ~ "bottled",
    other %in% groups$rainwater_cistern ~ "rainwater_cistern",
    TRUE ~ primary
  )
}

prepare_household_records <- function(house) {
  pipes <- household_pipe_groups()

  house |>
    normalise_household_input() |>
    dplyr::mutate(
      heating_fuel_primary = recode_household_fuel(
        .data$heating_fuel_primary,
        .data$heating_fuel_primary_other
      ),
      heating_fuel_secondary = recode_household_fuel(
        .data$heating_fuel_secondary,
        .data$heating_fuel_secondary_other
      ),
      cooking_fuel_primary = recode_household_fuel(
        .data$cooking_fuel_primary,
        .data$cooking_fuel_primary_other
      ),
      pipes_metal_any = dplyr::case_when(
        .data$pipes_copper_metal == "no_records_available" |
          .data$sec_pipes_copper_metal == "no_records_available" ~
          "no_records_available",
        .data$pipes_other_specify %in% pipes$metal |
          .data$sec_pipes_other_specify %in% pipes$metal |
          .data$pipes_copper_metal == "1" |
          .data$sec_pipes_copper_metal == "1" ~ "1",
        TRUE ~ "0"
      ),
      pipes_plastic_any = dplyr::case_when(
        .data$pipes_pvc_plastic == "no_records_available" |
          .data$sec_pipes_pvc_plastic == "no_records_available" ~
          "no_records_available",
        .data$pipes_other_specify %in% pipes$plastic |
          .data$sec_pipes_other_specify %in% pipes$plastic |
          .data$pipes_pvc_plastic == "1" |
          .data$sec_pipes_pvc_plastic == "1" ~ "1",
        TRUE ~ "0"
      ),
      water_source = recode_household_water(
        .data$water_source,
        .data$water_source_other
      )
    )
}

rename_household_mode_windows <- function(data) {
  data |>
    dplyr::rename_with(
      ~ gsub("_0_1_2$", "_early_life", .x),
      dplyr::ends_with("_0_1_2")
    ) |>
    dplyr::rename_with(
      ~ gsub("_0_1_2_3_4_5_6_7_8_9_10$", "_whole_life", .x),
      dplyr::ends_with("_0_1_2_3_4_5_6_7_8_9_10")
    ) |>
    dplyr::rename_with(
      ~ gsub("_3_4_5_6_7_8_9_10$", "_rest_of_life", .x),
      dplyr::ends_with("_3_4_5_6_7_8_9_10")
    )
}

.household_mode_or_multiple <- function(value) {
  value <- value[!is.na(value)]

  if (length(value) == 0L) {
    return(NA_character_)
  }

  counts <- table(value)
  modes <- names(counts)[counts == max(counts)]

  if (length(modes) == 1L) modes[[1L]] else "multiple"
}

add_household_mode_window <- function(data, columns, study_years) {
  suffix <- paste(study_years, collapse = "_")

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns),
        ~ .household_mode_or_multiple(
          .x[as.numeric(.data$year_in_study) %in% study_years]
        ),
        .names = paste0("{.col}_mode_", suffix)
      )
    ) |>
    dplyr::ungroup()
}

add_household_mode_windows <- function(data, columns) {
  data <- add_household_mode_window(data, columns, 0:2)
  data <- add_household_mode_window(data, columns, 3:10)
  data <- add_household_mode_window(data, columns, 0:10)
  rename_household_mode_windows(data)
}

add_household_recent_modes <- function(
    data,
    columns,
    endpoint_column,
    number_of_years) {
  output_suffix <- paste0("_mode_", number_of_years, "yrs_prior")

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns),
        ~ {
          endpoint <- .single_group_value(
            .data[[endpoint_column]],
            endpoint_column,
            dplyr::first(.data$subject_id)
          )
          years <- seq.int(endpoint - number_of_years, endpoint - 1L)
          .household_mode_or_multiple(
            .x[as.numeric(.data$year_in_study) %in% years]
          )
        },
        .names = paste0("{.col}", output_suffix)
      )
    ) |>
    dplyr::ungroup()
}

household_window_output_columns <- function(columns) {
  as.vector(outer(
    columns,
    c("early_life", "rest_of_life", "whole_life"),
    paste,
    sep = "_mode_"
  ))
}

build_household_features <- function(house, location, cohort) {
  house_columns <- household_mode_columns()
  location_columns <- location_mode_columns()
  cohort_subjects <- cohort |>
    dplyr::select("subject_id") |>
    dplyr::distinct()

  house_tidy <- prepare_household_records(house) |>
    dplyr::semi_join(cohort_subjects, by = "subject_id") |>
    add_household_mode_windows(house_columns) |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      avg_house_age = mean(
        as.numeric(dplyr::na_if(.data$house_age, "no_records_available")),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()

  location_tidy <- location |>
    normalise_household_input() |>
    dplyr::semi_join(cohort_subjects, by = "subject_id") |>
    add_household_mode_windows(location_columns)

  house_per_dog <- house_tidy |>
    dplyr::distinct(.data$subject_id, .keep_all = TRUE) |>
    dplyr::select(
      "subject_id",
      dplyr::all_of(household_window_output_columns(house_columns)),
      "avg_house_age"
    )

  location_per_dog <- location_tidy |>
    dplyr::distinct(.data$subject_id, .keep_all = TRUE) |>
    dplyr::select(
      "subject_id",
      "travel_mode",
      "travel_mode_other",
      "country",
      "region_name",
      dplyr::all_of(household_window_output_columns(location_columns))
    )

  endpoint <- cohort |>
    dplyr::select(
      "subject_id",
      "year_in_study_diagnosis_or_final_record_year"
    ) |>
    dplyr::distinct()

  if (anyNA(endpoint$year_in_study_diagnosis_or_final_record_year)) {
    stop(
      "Missing year_in_study_diagnosis_or_final_record_year for household features.",
      call. = FALSE
    )
  }

  house_five_year <- house_tidy |>
    dplyr::left_join(
      endpoint,
      by = "subject_id",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      year_in_study_diagnosis_or_final_record_year = as.numeric(
        .data$year_in_study_diagnosis_or_final_record_year
      ),
      year_in_study = as.numeric(.data$year_in_study)
    ) |>
    add_household_recent_modes(
      columns = house_columns,
      endpoint_column = "year_in_study_diagnosis_or_final_record_year",
      number_of_years = 5
    ) |>
    dplyr::distinct(.data$subject_id, .keep_all = TRUE) |>
    dplyr::select(
      "subject_id",
      dplyr::all_of(paste0(house_columns, "_mode_5yrs_prior"))
    )

  cohort |>
    dplyr::left_join(
      location_per_dog,
      by = "subject_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      house_per_dog,
      by = "subject_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      house_five_year,
      by = "subject_id",
      relationship = "one-to-one"
    )
}
