library(tensorflow)
abc = tf$Session()
w1 = tf$Variable(tf$truncated_normal(shape=shape(10)), name='w1')
w2 = tf$Variable(tf$truncated_normal(shape=shape(20)), name='w2')
saver = tf$train$Saver()

abc$run(tf$initialize_all_variables())
saver$save(abc, 'my-model')
# `save` method will call `export_meta_graph` implicitly$
# you will get saved graph files:my-model$meta

abc = tf$Session()
new_saver = tf$train$import_meta_graph('my-model.meta')
new_saver$restore(abc, tf$train$latest_checkpoint('./'))
all_vars = tf$trainable_variables()

