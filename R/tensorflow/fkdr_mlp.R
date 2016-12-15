# Load data and start Tensorflow session
library(tensorflow)
datasets <- tf$contrib$learn$datasets
mnist <- datasets$mnist$read_data_sets("MNIST-data", one_hot = TRUE)
sess <- tf$InteractiveSession()

# Parameters
learning_rate = 0.001
training_epochs = 15L
batch_size = 100L
display_step = 1L

# Network Parameters
n_input = 9216L # 96x96 pixels
n_hidden_1 = 256L # 1st layer number of features
n_hidden_2 = 256L # 2nd layer number of features
n_classes = 30L # 15 x, 15 y coordinates

# tf Graph input
x = tf$placeholder(tf$float32, shape(NULL, n_input))
y = tf$placeholder(tf$float32, shape(NULL, n_classes))

# Weight Initialization
weight_variable <- function(shape) {
  initial <- tf$truncated_normal(shape, stddev=0.1)
  tf$Variable(initial)
}

bias_variable <- function(shape) {
  initial <- tf$constant(0.1, shape=shape)
  tf$Variable(initial)
}

# Create model
layer1 = tf$add(tf$matmul(x, weight_variable(shape(n_input, n_hidden_1))),
                bias_variable(shape(n_hidden_1)))
layer1 = tf$nn$relu(layer1)

layer2 = tf$add(tf$matmul(layer1, weight_variable(shape(n_hidden_1, n_hidden_2))),
                bias_variable(shape(n_hidden_2)))
layer2 = tf$nn$relu(layer2)

out_layer = tf$matmul(layer2, weight_variable(shape(n_hidden_2, n_classes))) +
  bias_variable(shape(n_classes))

# Define loss and optimizer
cost = tf$reduce_mean(tf$nn$softmax_cross_entropy_with_logits(out_layer, y))
optimizer = tf$train$AdamOptimizer(learning_rate = learning_rate)$minimize(cost)
correct_prediction <- tf$equal(tf$argmax(out_layer, 1L), tf$argmax(y, 1L))
accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))

# Initialize graph
sess$run(tf$initialize_all_variables())

# Train and Evaluate the Model
for (i in seq_len(mnist$train$num_examples)) {
  batch <- mnist$train$next_batch(50L)
  if (i %% 100 == 0) {
    train_accuracy <- accuracy$eval(feed_dict = dict(
      x = batch[[1]],
      y = batch[[2]]
    ))
    cat(sprintf("step %d, training accuracy %g\n", i, train_accuracy))
  }
  optimizer$run(feed_dict = dict(
    x = batch[[1]],
    y = batch[[2]]
  ))
}

train_accuracy <- accuracy$eval(feed_dict = dict(
  x = mnist$test$images,
  y = mnist$test$labels
))
cat(sprintf("test accuracy %g", train_accuracy))
