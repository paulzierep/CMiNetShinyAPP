# Galaxy-aware input staging for CMiNet.
#
# Sourced by app.R. The shared galaxy_ie.R provides the Send to Galaxy buttons
# and the history picker; this file only handles the part that is specific to
# CMiNet, namely telling the app's file inputs about the datasets the Galaxy
# wrapper staged on disk. Everything degrades to a no-op outside Galaxy, so
# standalone `shiny::runApp()` usage is unchanged.

# Resolve the CSV a fileInput should be read from. A file the user picked in the
# browser always wins; otherwise the dataset staged by the Galaxy wrapper (or
# the copy an import from the history just pointed CMINET_INPUT at) is used.
# Returning NULL keeps the app's `req()` guards working unchanged.
cminet_resolve_input <- function(input_value, staged_path) {
  if (!is.null(input_value) && !is.null(input_value$datapath)) {
    return(input_value$datapath)
  }
  if (nzchar(staged_path) && file.exists(staged_path)) {
    return(staged_path)
  }
  NULL
}

# Best-effort: register a file with the browser-side file input so the UI shows
# it as already uploaded. Data resolution does not depend on this succeeding,
# see cminet_resolve_input().
cminet_send_file_input <- function(session, input_id, path) {
  if (!nzchar(path) || !file.exists(path)) return(FALSE)
  session$sendInputMessage(input_id, list(
    name = basename(path),
    size = file.info(path)$size,
    type = "text/csv",
    datapath = path
  ))
  TRUE
}

# Called once when the session starts, so a wrapper-staged dataset shows up in
# the form instead of the user having to upload it again.
cminet_stage_galaxy_inputs <- function(session) {
  abundance <- Sys.getenv("CMINET_INPUT", unset = "")
  weighted <- Sys.getenv("CMINET_WEIGHTED_NETWORK", unset = "")
  if (!nzchar(abundance) && !nzchar(weighted)) return(invisible(NULL))

  if (nzchar(abundance)) {
    cminet_send_file_input(session, "file", abundance)
  }
  if (nzchar(weighted)) {
    cminet_send_file_input(session, "weightedFile", weighted)
    cminet_send_file_input(session, "finalWeightedFile", weighted)
    shiny::updateSelectInput(
      session, "fileOption",
      selected = "Upload Weighted Network (CSV file)"
    )
    shiny::updateSelectInput(
      session, "finalFileOption",
      selected = "Upload Weighted Network (CSV file)"
    )
  }

  invisible(NULL)
}