# Load data and start Tensorflow session
library(tensorflow)

# --- DATA ---
train.x = as.matrix(d.train$Image)
train.y = as.matrix(d.train[,-31])
# Only use data with zero NAs for now
zeroNAindices = which(rowSums(is.na(d.train)) == 0)
train.x = train.x[zeroNAindices, ]
train.y = train.y[zeroNAindices, ]
# Simple split for now
# trainIndices = sample(1:nrow(train.x), size = round(0.7 * nrow(train.x)), replace=FALSE)
train.x = train.x[trainIndices, ]
test.x = train.x[-trainIndices, ]
train.y = train.y[trainIndices, ]
test.y = train.y[-trainIndices, ]
# Scale pixel intensities to [0, 1]
train.x = train.x / 255
test.x = test.x / 255
# Scale target coordinates to [-1, 1]
train.y = (train.y - 48) / 48
test.y = (test.y - 48) / 48
# --- /DATA ---

# Parameters
learning_rate = 0.001
training_epochs = 1000L

batch_size = 50L
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
cost = tf$reduce_mean(tf$square(out_layer - y))
optimizer = tf$train$AdamOptimizer(learning_rate = learning_rate)$minimize(cost)
# y_mean = tf$reduce_mean(y)
# r_squared = tf$reduce_sum(tf$square(out_layer - y_mean)) / tf$reduce_sum(tf$square(y - y_mean))
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
pred = out_layer$eval(feed_dict = dict(x = test.x)) * 48 + 48
plotFacialKeypoints(data, 100, pred)

# # Save data
# saver <- tf$train$Saver()
# data_file <- saver$save(sess, paste0("/Users/henry/fkdr_submissions/", "fkdr_mlp_1000epochs.ckpt"))
#
# # Restore Data
# sess = tf$Session()
# restorer = tf$train$import_meta_graph(paste0("/Users/henry/fkdr_submissions/", "fkdr_mlp_1000epochs.ckpt.meta"))
# restorer$restore(sess, tf$train$latest_checkpoint("/Users/henry/fkdr_submissions/"))
#
# # Make submission file
# data = d.test$Image / 255
# pred = sess$run(out_layer, feed_dict = dict(x = data)) * 48 + 48
# writeSubmissionFile(predictions = pred, "/Users/henry/fkdr_submissions/")
