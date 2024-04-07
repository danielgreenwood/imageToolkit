#' Train a CNN on a directory of images
#' This function trains a keras CNN model on a folder of n-dimensional images and returns the trained model together with validation data
#' @param model A keras CNN model
#' @param image_directory The directory containing folders of images named by class
#' @param image_list Alternatively, a list of pre-loaded crops may be supplied
#' @param validation_split The proportion of images that should be saved for validation
#' @param epochs Training epochs
#' @param weight_classes Should the loss function be weighted to account for unbalanced classes
#' @param sub_cropsize If the images should be further cropped, specify the number of pixels
#' @param channels Specify the names of the channels that should be read into the model
#' @param batch_size Batch size
#' @param scale Manually specify number to divide pixel values by
#' @param log_scale Log scale intensity values
#' @param featurewise_standardisation Argument passed to image generator
#' @param imagewise_normalisation Argument passed to image generator
#' @param subsample_training_data Argument passed to image generator
#' @param early_stopping Stop the training early once the loss function stabilises
#' @param ... Other image normalsiation arguments
#'
#' @return Returns a list containing a trained keras model, training history, confusion matrix (from validation images), validation image predictions and names
#' @export
train_cnn <-
  function(model,
           image_directory = NULL,
           image_list = NULL,
           validation_split = 0.2,
           epochs = 10,
           weight_classes = FALSE,
           sub_cropsize = NULL,
           channels = 1:5,
           batch_size = 128,
           scale = NULL,
           log_scale = FALSE,
           featurewise_standardisation = FALSE,
           imagewise_normalisation = FALSE,
           subsample_training_data = NULL,
           early_stopping = TRUE,
           # Extra image augmentation options
           ...) {

    if (is.null(image_directory))
      image_directory = choose.dir()
    setwd(image_directory)

    # Get image labels
    if(is.null(image_list)){
      cat("Gathering image list\n")
      image_list = list.files(path = image_directory,
                              pattern = '.tif',
                              recursive = TRUE, full.names = TRUE)
      image_list = image_list[stringr::str_detect(image_list, stringr::fixed(".tif"))]

    }
    cat("Found", length(image_list), "images\n")
    image_list = sample(image_list)
    if(!is.null(subsample_training_data)){
      image_list = sample(image_list, subsample_training_data)
    }
    labels_list = strsplit(image_list, '/')
    label_position = length(labels_list[[1]]) - 1
    labels = vector('character', length(labels_list))
    for (i in 1:length(labels)) {
      labels[i] = dplyr::nth(labels_list[[i]], label_position)
    }
    labels_numeric = as.numeric(factor(labels)) - 1
    labels_key = levels(factor(labels))
    cat(length(labels_key), "classes detected:\n")
    print(table(labels))

    # Find image shape and load images
    image_temp = ijtiff::read_tif(image_list[1])
    image_shape = dim(image_temp[, , , ])
    image_shape[3] = length(channels)
    images_array = array(dim = c(length(image_list), image_shape))
    cat("Loading images\n")

    cat("Images array dimensions", dim(images_array), "\n")

    pb = progress::progress_bar$new(total = length(image_list),
                          format = " [:bar] :percent eta: :eta",
                          clear = FALSE)
    pb$tick(0)
    for (i in 1:length(image_list)) {
      pb$tick()
      tryCatch({
        img_temp <- ijtiff::read_tif(image_list[i], msg = F)[,,channels,]
        if(any(is.na(img_temp))) warning("NAs generated in image ", image_list[i])
        images_array[i, , , ] <-  img_temp

      },
      error = function(e) {
        print(e)
        print(image_list[i])
      } )
    }

    # Select channels
    #images_array = images_array[, , , channels]
    if(any(is.na(images_array))) cat("\nWarning: NAs present\n")

    # Apply subcrop (assumes images are square)
    if (is.numeric(sub_cropsize)) {
      image_centre = round(image_shape[1] / 2)
      centre_offset = round(sub_cropsize / 2)
      xymin = image_centre - centre_offset
      xymax = image_centre + centre_offset - 1
      new_image_size = xymax - xymin + 1
      images_array = images_array[, xymin:xymax, xymin:xymax, ]
      cat("Images cropped to size ", new_image_size, "\n")
    }

    # scale
    if(is.numeric(scale)) images_array = images_array * scale

    # Log scale
    if (log_scale) {
      # First make safe by removing zeros
      images_array[images_array == 0] = 1
      images_array = log10(images_array)
    }

    # Rescale and normalise by channel
    if (featurewise_standardisation) {
      for (i in 1:length(channels)) {
        # Center
        images_array[, , , i] = images_array[, , , i] - mean(images_array[, , , i])
        # Normalise SD
        images_array[, , , i] = images_array[, , , i] / stats::sd(images_array[, , , i])
      }
    }

    if (imagewise_normalisation) {
      for (i in 1:length(channels)) {
        for(j in 1:length(image_list)){
          # Center
          images_array[j, , , i] = images_array[j, , , i] - mean(images_array[j, , , i])
          # Normalise SD
          images_array[j, , , i] = images_array[j, , , i] / stats::sd(images_array[j, , , i])
        }
      }
    }

    # Split into training and validation datasets
    validation_elements = round(length(image_list) * validation_split)
    validation_images_array = images_array[1:validation_elements, , , ]
    training_images_array = images_array[-(1:validation_elements), , , ]
    rm(images_array)

    validation_labels = labels_numeric[1:validation_elements]
    training_labels = labels_numeric[-(1:validation_elements)]

    validation_image_names = image_list[1:validation_elements]
    training_image_names = image_list[-(1:validation_elements)]

    # setup data import
    training_datagen = keras::image_data_generator(
      #samplewise_center = TRUE,
      #samplewise_std_normalization = TRUE,
      rotation_range = 180,
      horizontal_flip = TRUE,
      vertical_flip = TRUE,
      ...

    )

    validation_datagen = keras::image_data_generator()

    training_images = flow_images_from_data(
      training_images_array,
      y = training_labels,
      generator = training_datagen,
      batch_size = batch_size
    )

    validation_images = keras::flow_images_from_data(validation_images_array,
                                              y = validation_labels,
                                              generator = validation_datagen)

    # Weight classes according to abundance
    if (weight_classes) {
      class_abundance = table(labels)
      class_weights = sum(class_abundance) / class_abundance
      class_weights = class_weights / mean(class_weights)
      class_weights = as.list(class_weights)
      names(class_weights) = as.character(1:length(class_weights) - 1)
    } else{
      class_weights = NULL
    }

    # Train model
    if(!early_stopping){
      history = model %>%
        fit(
          training_images,
          epochs = epochs,
          validation_data = validation_images,
          class_weight = class_weights,
          workers = 12,
          max_queue_size = 10
        )
    } else if (early_stopping){
      history = model %>%
        keras::fit(
          training_images,
          epochs = epochs,
          validation_data = validation_images,
          class_weight = class_weights,
          workers = 12,
          max_queue_size = 10, callbacks = list(
            keras::callback_early_stopping(patience = 100, restore_best_weights = T),
            keras::callback_reduce_lr_on_plateau(patience = 20)
          )
        )
    }

    # Generate confusion matrix
    predictions = predict(model, validation_images_array)

    if (ncol(predictions) > 1) {
      prediction_classes = max.col(predictions) - 1
    } else{
      prediction_classes = round(predictions)
    }

    confusion_matrix = caret::confusionMatrix(factor(prediction_classes), factor(validation_labels))
    print(confusion_matrix)

    return(
      list(
        model = model,
        history = history,
        confusion_matrix = confusion_matrix,
        validation_predictions = predictions,
        validation_predictions = predictions,
        validation_image_names = validation_image_names,
        training_image_names = training_image_names
      )
    )
  }
