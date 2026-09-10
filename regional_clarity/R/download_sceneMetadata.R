#' Download Landsat scene-level metadata from EDI
#'
#' @description
#' A function to facilitate downloading of Landsat scene-level metadata that
#' accompanies the siteSR and lakeSR data products on the Environmental Data
#' Initiative (EDI). Scene-level metadata captures per-overpass attributes
#' (e.g. cloud cover, image quality, geometric accuracy) that aren't carried
#' through into siteSR/lakeSR's per-site aggregation.
#'
#' @details
#' Downloads two components of the scene-level metadata (identical content in
#' both the [siteSR](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2254.1)
#' and [lakeSR](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2114.1)
#' EDI packages):
#' * reduced column scene-level metadata for Landsat 4, 5, and 7 (.csv)
#' * reduced column scene-level metadata for Landsat 8 and 9 (.csv)
#'
#' The two files have slightly different schemas: the Landsat 4/5/7 file has
#' a single `IMAGE_QUALITY` column and `GEOMETRIC_RMSE_VERIFY_QUAD_*`
#' columns, while the Landsat 8/9 file splits `IMAGE_QUALITY` into
#' `IMAGE_QUALITY_OLI` and `IMAGE_QUALITY_TIRS` and has no quad columns.
#' Both files share a `sat_id` column (derived from Earth Engine's
#' `system:index`) that can be used to join scene-level metadata back onto
#' siteSR/lakeSR rows, which carry the same `sat_id`.
#'
#'
#' @param save_location A string containing the path to the folder where the
#' dataset should be saved.
#' @param product Either "siteSR" or "lakeSR" - which EDI package to pull
#' from. The scene-level metadata content is identical between the two, so
#' this only matters if you want the file's provenance/citation tied to a
#' particular package's revision lineage.
#' @param version Either "newest" or an integer corresponding to the data
#' package version to use.
#' @param ask Logical. Should the user be asked before downloading and
#' overwriting scene metadata files that already exist locally?
#'
#' @return A named character vector containing the local file paths for the
#' downloaded scene metadata datasets. Returned invisibly.
#' @export
#'
#' @importFrom purrr map_chr
#' @importFrom EDIutils read_data_package_citation read_data_entity_names read_data_entity
#' @importFrom readr read_csv write_csv
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#'
#' @examples
#' \dontrun{
#' download_sceneMetadata(save_location = "~/Downloads/", product = "siteSR")
#' }
download_sceneMetadata <- function(save_location, product = c("siteSR", "lakeSR"),
                                   version = "newest", ask = TRUE){

  # Resolve which EDI package to pull from
  product <- match.arg(product)
  identifier <- switch(product, siteSR = 2254, lakeSR = 2114)

  # Scene metadata EDI ID
  scene_id <- construct_id(identifier = identifier, version = version)

  # Filenames to be used for the two scene metadata files:
  scene_names <- c(
    "sceneMetadata_Landsat457.csv",
    "sceneMetadata_Landsat89.csv"
  )

  if(ask == TRUE){
    # Check if any files with the standard names are already present in the save
    # location:
    if(any(file.exists(file.path(save_location, scene_names)))) {
      user_decision <- ask_user(algal_mask = FALSE,
                                which_sr = "generic",
                                file_message = "scene-level metadata")

      # Act on input
      if (user_decision == "yes") {
        cli::cli_alert_info("Proceeding with download.")
      } else {
        cli::cli_abort("Cancelled by user.")
      }
    }
  }

  # Get EDI entity names
  dl_entities <- EDIutils::read_data_entity_names(packageId = scene_id) %>%
    dplyr::filter(grepl(pattern = "^reduced column scene-level metadata", x = entityName))

  cli::cli_alert_info("This is a large download. It may take a few minutes.")

  # For each entity, read, message, and save
  dl_list <- split(dl_entities, f = dl_entities$entityName) %>%
    purrr::map_chr(.x = .,
                   .f = ~{
                     out_name <- switch(
                       .x$entityName,
                       "reduced column scene-level metadata for Landsat 4, 5, and 7" = "sceneMetadata_Landsat457.csv",
                       "reduced column scene-level metadata for Landsat 8 and 9" = "sceneMetadata_Landsat89.csv"
                     )

                     # Read in data as raw bytes
                     raw_bytes <- EDIutils::read_data_entity(packageId = scene_id,
                                                             entityId = .x$entityId)
                     # Parse
                     suppressMessages({
                       temp_file <- readr::read_csv(raw_bytes, show_col_types = FALSE)
                     })

                     readr::write_csv(
                       x = temp_file,
                       file = file.path(save_location, out_name)
                     )

                     cli::cli_alert_success("Downloaded {.val {(.x$entityName)}} as {.file {out_name}}.")

                     # Return the path to the loop
                     return(file.path(save_location, out_name))
                   })

  # Clean and store filenames
  names(dl_list) <- basename(dl_list)

  # Suggest citation
  cli::cli_alert_info(
    "Scene-level metadata recommended citation: {EDIutils::read_data_package_citation(packageId = scene_id)}"
  )

  return(invisible(dl_list))
}
