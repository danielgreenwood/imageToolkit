
<!-- README.md is generated from README.Rmd. Please edit that file -->

# imageToolkit

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/imageToolkit)](https://CRAN.R-project.org/package=imageToolkit)
[![R-CMD-check](https://github.com/beansprout88/imageToolkit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/beansprout88/imageToolkit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

imageToolkit provides functions for analysing biological image data from high-content microscopy. Of special note, it includes the train_cnn function, a convolutional neural network training function using Keras/Tensorflow but specialised for reading biological imaging data from 2D single-cell crops of TIFFs that may include an arbitrary number of channels, for example those generated by fluorescence microscopy. However the same principal can be applied to any multi-channel imaging format such as imagig mass cytometry. 

Images are read into a custom-sized tensor using th IJTIFF package, which is specialised to handle biological imaging formats that typically fail to load correctly with standard image-loading tools. Options for image augmentation can be customised, but defaults are optimised for cellular phenotyping: i.e. Images may be rotated or flipped, but zoom is limited to 10% (as cell size is generally a strong predictor of phenotype.

Install with `devtools::install_github("danielgreenwood/imageToolkit")`

![image](https://github.com/danielgreenwood/imageToolkit/assets/117200027/15b40dff-8d41-4f7f-906d-711e9f133a9e)
