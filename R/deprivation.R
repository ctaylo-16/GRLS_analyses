normalise_us_zip <- function(zip_code) {
  as.numeric(substr(as.character(zip_code), 1, 5))
}

first_mode <- function(values) {
  unique_values <- unique(values)
  unique_values[which.max(tabulate(match(values, unique_values)))]
}

add_mdi_quintiles <- function(mdi) {
  mdi_breaks <- stats::quantile(
    mdi$MDI.rate,
    probs = seq(0, 1, 0.2),
    na.rm = TRUE
  )

  mdi |>
    dplyr::mutate(
      MDI_quintile = cut(
        MDI.rate,
        breaks = mdi_breaks,
        include.lowest = TRUE,
        labels = 1:5
      )
    )
}

build_deprivation_dataset <- function(
    clinics,
    owner_addresses,
    zip_lookup,
    mdi) {
  zip_lookup <- zip_lookup |>
    dplyr::rename(postal_code = zip)

  mdi <- mdi |>
    dplyr::rename(county_fips = County) |>
    add_mdi_quintiles()

  clinic_mdi <- clinics |>
    dplyr::mutate(postal_code = normalise_us_zip(postal_code)) |>
    dplyr::left_join(zip_lookup, by = "postal_code") |>
    dplyr::left_join(mdi, by = "county_fips") |>
    dplyr::select(subject_id, clinic_name, MDI.rate, MDI_quintile)

  owner_mdi <- owner_addresses |>
    dplyr::group_by(subject_id) |>
    dplyr::summarise(
      mode_zipcode = first_mode(primary_zip_code),
      .groups = "drop"
    ) |>
    dplyr::mutate(postal_code = normalise_us_zip(mode_zipcode)) |>
    dplyr::left_join(zip_lookup, by = "postal_code") |>
    dplyr::left_join(mdi, by = "county_fips") |>
    dplyr::rename(
      owner_MDI.rate = MDI.rate,
      owner_MDI_quintile = MDI_quintile
    ) |>
    dplyr::select(subject_id, owner_MDI.rate, owner_MDI_quintile)

  clinic_mdi |>
    dplyr::left_join(owner_mdi, by = "subject_id")
}
