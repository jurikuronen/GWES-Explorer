# Runs the integration tests with Node.
run_integration_tests <- function() {
    library(GWESExplorer)

    node_executable <- Sys.which("node")
    if (!nzchar(node_executable)) {
        stop("Node.js is required to run integration tests.")
    }

    # Creates file information for an example data file.
    example_file <- function(example_filename) {
        data.frame(
            datapath = system.file("extdata", example_filename, package = "GWESExplorer", mustWork = TRUE),
            name = example_filename
        )
    }

    # Read the example Maela data.
    example_data <- new.env(parent = emptyenv())
    data_load_result <- GWESExplorer:::.read_data(
        example_data,
        outliers_file = example_file("maela_outliers.outliers"),
        tree_file = example_file("maela_tree.nex"),
        fasta_file = example_file("maela_fasta.fasta"),
        loci_file = example_file("maela_loci.loci"),
        phenotype_file = example_file("maela_phenotypes.csv"),
        gff_file = example_file("maela_gff.gff3")
    )
    if (data_load_result$success != GWESExplorer:::.STATUS_SUCCESS) {
        stop(as.character(data_load_result$status))
    }

    # Since the integration tests run Vega in Node, the Vega JavaScript library must be loaded directly.
    # GWES-Explorer loads Vega in the browser via htmlwidgets. The code below creates the widget and finds
    # the Vega library file among its dependencies.
    circular_plot_widget <- vegawidget::vegawidget(example_data$circular_plot_spec)
    widget_dependencies <- htmlwidgets::getDependency(class(circular_plot_widget)[1], package = "vegawidget")
    vega_dependency <- Filter(
        function(widget_dependency) widget_dependency$name == "vega",
        widget_dependencies
    )[[1]]
    vega_file <- system.file(
        vega_dependency$src$file, vega_dependency$script, package = "vegawidget", mustWork = TRUE
    )

    # Write the circular plot specification to a temporary file for Node to read.
    spec_file <- tempfile(fileext = ".json")
    on.exit(unlink(spec_file))
    writeLines(as.character(vegawidget::vw_as_json(example_data$circular_plot_spec)), spec_file)

    # Check if test filenames to run were passed as command-line arguments; otherwise run all the tests.
    test_dir <- "tests/integration/tests"
    test_filenames <- commandArgs(trailingOnly = TRUE)
    test_file_paths <- if (length(test_filenames) > 0) {
        file.path(test_dir, test_filenames)
    } else {
        list.files(test_dir, pattern = "[.]cjs$", full.names = TRUE)
    }
    if (length(test_file_paths) == 0) {
        stop("No integration tests found.")
    }

    any_test_failed <- FALSE
    for (test_file_path in test_file_paths) {
        node_exit_status <- system2(node_executable, shQuote(c(test_file_path, spec_file, vega_file)))
        if (node_exit_status != 0) {
            message("Integration test failed: ", test_file_path)
            any_test_failed <- TRUE
        }
    }
    if (any_test_failed) {
        stop("Integration tests failed.")
    }
}

run_integration_tests()
