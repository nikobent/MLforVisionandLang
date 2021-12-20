library("keras")

mnist <- dataset_mnist()
#Data preperation
x_train <- array_reshape(mnist$train$x, c(nrow(mnist$train$x), 784))
x_test <- array_reshape(mnist$test$x, c(nrow(mnist$test$x), 784))

x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(mnist$train$y, 10)
y_test <- to_categorical(mnist$test$y, 10)

#model questions 2-6
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, input_shape = c(784)) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 12,
  verbose = 1,
  validation_split = 0.2
)

score <- model %>% evaluate(
  x_test, y_test,
  verbose = 0
)

#model2 questions 7-9
model2 <- keras_model_sequential()
model2 %>%
  layer_dense(units = 256, input_shape = c(784), activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model2)

model2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history2 <- model2 %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 12,
  verbose = 1,
  validation_split = 0.2
)

score2 <- model2 %>% evaluate(
  x_test, y_test,
  verbose = 0
)

#DEEP model questions 10-onwards
#Data preperation
x_train <- array_reshape(mnist$train$x, c(nrow(mnist$train$x), 28, 28,1))
x_test <- array_reshape(mnist$test$x, c(nrow(mnist$test$x), 28, 28,1))

x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(mnist$train$y, 10)
y_test <- to_categorical(mnist$test$y, 10)

#model 
model3 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3),
                activation = 'relu', input_shape = c(28,28,1)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model3)

model3 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

history3 <- model3 %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 6,
  verbose = 1,
  validation_split = 0.2
)

score3 <- model3 %>% evaluate(
  x_test, y_test,
  verbose = 0
)

#model with dropout
model4 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3),
                activation = 'relu', input_shape = c(28,28,1)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  dropout_layer(rate = 0.25)
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  dropout_layer(rate = 0.5)
  layer_dense(units = 10, activation = 'softmax')

summary(model4)

model4 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

history4 <- model4 %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 6,
  verbose = 1,
  validation_split = 0.2
)

score4 <- model4 %>% evaluate(
  x_test, y_test,
  verbose = 0
)

 