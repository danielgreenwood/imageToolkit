test_that("Keras and TensorFlow are installed and working", {
  skip_if_not_installed("keras")

  expect_silent({
    library(keras)
    library(tensorflow)

    # Check if TensorFlow is available and working
    tf <- tensorflow::tf_config()
    expect_true(!is.null(tf$version), info = "Keras or Tensorflow not installed. Run keras::install_keras()")

  })
})
