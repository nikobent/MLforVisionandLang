library("keras")
cifar10 <- dataset_cifar10()

x_train <- cifar10$train$x / 255
x_test <- cifar10$test$x / 255

y_train <- to_categorical(cifar10$train$y, 10)
y_test <- to_categorical(cifar10$test$y, 10)

#model with dropout
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), padding = "same",
                activation = 'relu', input_shape = c(32,32,3)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  dropout_layer(rate = 0.25)%>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), padding = "same",
                activation = 'relu') %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  dropout_layer(rate = 0.25)%>%
  layer_flatten() %>%
  layer_dense(units = 512, activation = 'relu') %>%
  dropout_layer(rate = 0.5) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  batch_size = 32,
  epochs = 20,
  verbose = 1,
  validation_data = list(x_test, y_test),
  shuffle = TRUE
)

score <- model %>% evaluate(
  x_test, y_test,
  verbose = 0
)
