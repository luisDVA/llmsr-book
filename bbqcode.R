#' Render a Quarto multilingual project
#'
#' @importFrom rlang `%||%`
#'
#' @details babelquarto expects a book/website folder with
#' each qmd/Rmd present in as many languages as needed,
#' with the same basename but,
#' - once with only `.qmd` as extension for the main language,
#' - once with `.es.qmd` (using the language code) for each other language.
#'
#' You also need to register the language in the configuration file,
#' see [babelquarto::register_main_language()]
#' and [babelquarto::register_further_languages()]:
#'
#' ```yaml
#' babelquarto:
#'   mainlanguage: 'en'
#'   languages: ['es', 'fr']
#' ```
#'
#' @importFrom rlang `%||%`
#'
#' @param project_path Path where the book/website source is located
#' @param site_url Base URL of the book/website.
#' @param profile Quarto profile(s) to use.
#'
#' @export
#'
#' @examples
#' directory <- withr::local_tempdir()
#' quarto_multilingual_book(parent_dir = directory, project_dir = "blop")
#' render_book(file.path(directory, "blop"))
#' \dontrun{
#' if (require("servr") && rlang::is_interactive()) {
#'   servr::httw(file.path(directory, "blop", "_book"))
#' }
#' }
#'
#' @rdname render
render_book <- function(project_path = ".",
                        site_url = NULL, profile = NULL) {
  render(project_path, site_url = site_url, type = "book", profile = profile)
}

#' @export
#' @rdname render
render_website <- function(project_path = ".",
                           site_url = NULL, profile = NULL) {
  render(project_path, site_url = site_url, type = "website", profile = profile)
}

render <- function(path = ".",
                   site_url = NULL,
                   type = c("book", "website"),
                   profile = NULL) {
  # configuration ----
  config <- file.path(path, "_quarto.yml")
  config_contents <- read_yaml(config)

  site_url <- site_url %||% site_url(config_contents, type)
  site_url <- sub("/$", "", site_url)

  output_dir <- config_contents[["project"]][["output-dir"]] %||%
    switch(
      type,
      book = "_book",
      website = "_site"
    )

  language_codes <- config_contents[["babelquarto"]][["languages"]]
  if (is.null(language_codes)) {
    cli::cli_abort("Can't find {.field babelquarto.languages} in {.field _quarto.yml}") # nolint: line_length_linter
  }
  main_language <- config_contents[["babelquarto"]][["mainlanguage"]]
  if (is.null(main_language)) {
    cli::cli_abort("Can't find {.field babelquarto.mainlanguage} in {.field _quarto.yml}") # nolint: line_length_linter
  }

  output_folder <- file.path(path, output_dir)
  if (fs::dir_exists(output_folder)) fs::dir_delete(output_folder)

  # render project ----
  temporary_directory <- withr::local_tempdir()
  profile <- profile %||% Sys.getenv("QUARTO_PROFILE")
  fs::dir_copy(path, temporary_directory)
  withr::with_dir(file.path(temporary_directory, fs::path_file(path)), {
    fs::file_delete(fs::dir_ls(regexp = "\\...\\.qmd", recurse = TRUE))
    metadata <- list("true")
    names(metadata) <- sprintf("lang-%s", main_language)
    quarto::quarto_render(
      as_job = FALSE,
      metadata = metadata,
      profile = c(main_language, profile)
    )
  })
  fs::dir_copy(
    file.path(temporary_directory, fs::path_file(path), output_dir),
    path
  )

  purrr::walk(
    language_codes,
    render_quarto_lang,
    path = path,
    output_dir = output_dir,
    type = type
  )

  # Add the language switching link to the sidebar ----
  ## For the main language ----

  # we need to recurse but not inside the language folders!
  all_docs <- fs::dir_ls(output_folder, glob = "*.html", recurse = TRUE)
  other_language_docs <- unlist(
    purrr::map(
      language_codes,
      ~fs::dir_ls(file.path(output_folder, .x), glob = "*.html", recurse = TRUE)
    )
  )
  main_language_docs <- setdiff(all_docs, other_language_docs)

  purrr::walk(
    language_codes,
    ~ purrr::walk(
      main_language_docs,
      add_links,
      main_language = main_language,
      language_code = .x,
      site_url = site_url,
      type = type,
      config = config_contents,
      output_folder = output_folder,
      path_language = main_language,
      project_dir = path
    )
  )
  purrr::walk(
    main_language_docs,
    add_cross_links,
    main_language = main_language,
    site_url = site_url,
    config = config_contents,
    output_folder = output_folder,
    path_language = main_language
  )
  ## For other languages ----
  for (other_lang in language_codes) {
    other_lang_docs <- fs::dir_ls(
      file.path(output_folder, other_lang),
      glob = "*.html", recurse = TRUE
    )
    languages_to_add <- c(main_language, setdiff(language_codes, other_lang))
    purrr::walk(
      languages_to_add,
      ~ purrr::walk(
        other_lang_docs,
        add_links,
        main_language = main_language,
        language_code = .x,
        site_url = site_url,
        type = type,
        config = config_contents,
        output_folder = output_folder,
        path_language = other_lang,
        project_dir = path
      )
    )
    purrr::walk(
      other_lang_docs,
      add_cross_links,
      main_language = main_language,
      site_url = site_url,
      config = config_contents,
      output_folder = output_folder,
      path_language = other_lang
    )
  }

}

site_url <- function(config_contents, type) {

  if (nzchar(Sys.getenv("BABELQUARTO_CI_URL"))) {
    return(Sys.getenv("BABELQUARTO_CI_URL"))
  }

  config_contents[[type]][["site-url"]] %||% ""

}

render_quarto_lang <- function(language_code, path, output_dir, type) {

  temporary_directory <- withr::local_tempdir()
  fs::dir_copy(path, temporary_directory)
  project_name <- fs::path_file(path)

  config_path <- file.path(temporary_directory, project_name, "_quarto.yml")
  config <- read_yaml(config_path)

  freeze_directory_exists <- fs::dir_exists(
    file.path(temporary_directory, project_name, "_freeze")
  )

  if (freeze_directory_exists) {
    filter_freeze_directory(
      temporary_directory,
      project_name,
      language_code
    )
  }

  config[["lang"]] <- language_code

  config[[type]][["title"]] <- config[[sprintf("title-%s", language_code)]] %||% # nolint: line_length_linter
    config[[type]][["title"]]

  config[[type]][["subtitle"]] <- config[[sprintf("subtitle-%s", language_code)]] %||% # nolint: line_length_linter
    config[[type]][["subtitle"]]

  config[[type]][["description"]] <- config[[sprintf("description-%s", language_code)]] %||% # nolint: line_length_linter
    config[[type]][["description"]]

  if (type == "book") {
    config[[type]][["author"]] <- config[[sprintf("author-%s", language_code)]] %||% # nolint: line_length_linter
      config[[type]][["author"]]

    config[["book"]][["chapters"]] <- purrr::map(
      config[["book"]][["chapters"]],
      use_lang_chapter,
      language_code = language_code,
      book_name = project_name,
      directory = temporary_directory
    )
    config[["book"]][["appendices"]] <- purrr::map(
      config[["book"]][["appendices"]],
      use_lang_chapter,
      language_code = language_code,
      book_name = project_name,
      directory = temporary_directory
    )
    # Replace TRUE and FALSE with 'true' and 'false'
    # to avoid converting to "yes" and "no"
    config <- replace_true_false(config)
    yaml::write_yaml(
      config,
      file = file.path(temporary_directory, project_name, "_quarto.yml")
    )
  }

  if (type == "website") {

    # only keep what's needed
    qmds <- fs::dir_ls(
      file.path(temporary_directory, fs::path_file(path)),
      glob = "*.qmd",
      recurse = TRUE
    )
    language_qmds <- purrr::keep(
      qmds, \(x) endsWith(x, sprintf(".%s.qmd", language_code))
    )
    fs::file_delete(qmds[!(qmds %in% language_qmds)])
    for (qmd_path in language_qmds) {
      fs::file_move(
        qmd_path,
        sub(sprintf("%s.qmd", language_code), "qmd", qmd_path)
      )
    }
    # Replace TRUE and FALSE with 'true' and 'false'
    # to avoid converting to "yes" and "no"
    config <- replace_true_false(config)

    yaml::write_yaml(config, file = config_path)
  }

  config_lines <- brio::read_lines(config_path)
  brio::write_lines(config_lines, path = config_path)

  # Render language book
  metadata <- list("yes")
  names(metadata) <- sprintf("lang-%s", language_code)
  withr::with_dir(file.path(temporary_directory, project_name), {
    quarto::quarto_render(
      as_job = FALSE,
      metadata = metadata,
      profile = language_code
    )
  })

  # Copy it to local not temporary _book/<language-code>
  fs::dir_copy(
    file.path(temporary_directory, project_name, output_dir),
    file.path(path, output_dir, language_code)
  )

}

