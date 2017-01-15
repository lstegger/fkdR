# Load data and start Tensorflow session
library(fkdR)
library(tensorflow)

# --- DATA ---
train.x = as.matrix(d.train$Image)
train.y = as.matrix(d.train[,-31])
# Only use data with zero NAs for now
zeroNAindices = which(rowSums(is.na(d.train)) == 0)
train.x = train.x[zeroNAindices, ]
train.y = train.y[zeroNAindices, ]
# # Simple split for now
# trainIndices = sample(1:nrow(train.x), size = round(0.7 * nrow(train.x)), replace=FALSE)
# train.x = train.x[trainIndices, ]
# test.x = train.x[-trainIndices, ]
# train.y = train.y[trainIndices, ]
# test.y = train.y[-trainIndices, ]
# Scale pixel intensities to [0, 1]
train.x = train.x / 255
test.x = test.x / 255
# Scale target coordinates to [-1, 1]
train.y = (train.y - 48) / 48
test.y = (test.y - 48) / 48
# --- /DATA ---

# Parameters
learning_rate = 0.001
training_epochs = 25L
batch_size = 50L
display_step = 1L

# Network Parameters
n_input = 9216L # 96x96 pixels
n_classes = 30L # 15 x, 15 y coordinates

# tf Graph input
x = tf$placeholder(tf$float32, shape(NULL, n_input))
y = tf$placeholder(tf$float32, shape(NULL, n_classes))

# Weight and bias convienience functions
weight_variable <- function(shape) {
  initial <- tf$truncated_normal(shape, stddev=0.1)
  tf$Variable(initial)
}

bias_variable <- function(shape) {
  initial <- tf$constant(0.1, shape=shape)
  tf$Variable(initial)
}

# Convolution and max pooling convenience functions
conv2d <- function(x, W) {
  tf$nn$conv2d(x, W, strides=c(1L, 1L, 1L, 1L), padding='SAME')
}

max_pool_2x2 <- function(x) {
  tf$nn$max_pool(
    x,
    ksize=c(1L, 2L, 2L, 1L),
    strides=c(1L, 2L, 2L, 1L),
    padding='SAME')
}

# Create model
## First layer (convolution)
W_conv1 <- weight_variable(shape(5L, 5L, 1L, 32L))
b_conv1 <- bias_variable(shape(32L))
x_image <- tf$reshape(x, shape(-1L, 96L, 96L, 1L))
h_conv1 <- tf$nn$relu(conv2d(x_image, W_conv1) + b_conv1)
## Second layer (pooling)
h_pool1 <- max_pool_2x2(h_conv1)
## Third layer (convolution)
W_conv2 <- weight_variable(shape = shape(5L, 5L, 32L, 64L))
b_conv2 <- bias_variable(shape = shape(64L))
h_conv2 <- tf$nn$relu(conv2d(h_pool1, W_conv2) + b_conv2)
## Fourth layer (pooling)
h_pool2 <- max_pool_2x2(h_conv2)
## Fifth layer (densely connected)
W_fc1 <- weight_variable(shape(36864L, 1024L))
b_fc1 <- bias_variable(shape(1024L))
h_pool2_flat <- tf$reshape(h_pool2, shape(-1L, 24L * 24L * 64L))
h_fc1 <- tf$nn$relu(tf$matmul(h_pool2_flat, W_fc1) + b_fc1)
## Dropout
# keep_prob <- tf$placeholder(tf$float32)
# h_fc1_drop <- tf$nn$dropout(h_fc1, keep_prob)
## Sixth layer (readout)
W_fc2 <- weight_variable(shape(1024L, 30L))
b_fc2 <- bias_variable(shape(30L))
# y_conv <- tf$matmul(h_fc1_drop, W_fc2) + b_fc2
y_conv <- tf$matmul(h_fc1, W_fc2) + b_fc2

# Define loss and optimizer
cost = tf$reduce_mean(tf$square(y_conv - y))
optimizer = tf$train$AdamOptimizer(learning_rate = learning_rate)$minimize(cost)
accuracy = tf$sqrt(cost) * 48

# Initialize graph
sess <- tf$Session()
sess$run(tf$initialize_all_variables())

# Simple function that returns indices for batching
nextBatchIndices <- function(indices, batchNr, batch_size) {
  position = batchNr * batch_size - batch_size + 1
  if ((position + batch_size) > length(indices)) {
    return(indices[position:length(indices)])
  }
  return(indices[position:(position + batch_size - 1)])
}

# Train and Evaluate the Model
numberOfBatches = ceiling(nrow(train.x) / batch_size)

for(epoch in seq_len(training_epochs)) {
  shuffledIndices = sample(seq_len(nrow(train.x)))

  for(batchNr in seq_len(numberOfBatches)) {
    rowIndices = nextBatchIndices(shuffledIndices, batchNr, batch_size)

    train_accuracy <- sess$run(accuracy, feed_dict = dict(x = train.x[rowIndices, ], y = train.y[rowIndices, ]))
    cat(sprintf("Epoch: %d | Batch: %d/%d | Training RMSE: %g\n", epoch, batchNr, numberOfBatches, train_accuracy))

    sess$run(optimizer, feed_dict = dict(x = train.x[rowIndices, ], y = train.y[rowIndices, ]))
  }
}

test_accuracy <- sess$run(accuracy, feed_dict = dict(x = test.x, y = test.y))
cat(sprintf("Test RMSE: %g", test_accuracy))

# Plot on first test image
data = test.x * 255
pred = sess$run(y_conv, feed_dict = dict(x = test.x)) * 48 + 48
plotFacialKeypoints(data, 1, pred)

# # Save data
# saver <- tf$train$Saver()
# data_file <- saver$save(sess, paste0("/Users/henry/fkdr_submissions/", "fkdr_cnn_25epochs.ckpt"))
#
# # Restore Data
# Watch out to restore the correct checkpoint file!!!
# sess = tf$Session()
# restorer = tf$train$import_meta_graph(paste0("/Users/henry/fkdr_submissions/", "fkdr_cnn????.ckpt.meta"))
# restorer$restore(sess, tf$train$latest_checkpoint("/Users/henry/fkdr_submissions/"))
#
# Make submission file
data = d.test$Image / 255
pred = sess$run(y_conv, feed_dict = dict(x = data)) * 48 + 48
writeSubmissionFile(predictions = pred, "/Users/henry/fkdr_submissions/")
