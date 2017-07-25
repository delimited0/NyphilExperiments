library(PythonInR)
pyImport(import = "os")
pyImport(import = "numpy", as = "np")
pyImport(import = "tensorflow", as = "tf")
pyImport(import = "edward", as = "ed")
pyImport(from = "edward.models", import = "Normal")

# the getting started example ----
# http://edwardlib.org/getting-started
getting_started_example <- 
"
x_train = np.linspace(-3, 3, num=50)
y_train = np.cos(x_train) + np.random.normal(0, 0.1, size=50)
x_train = x_train.astype(np.float32).reshape((50, 1))
y_train = y_train.astype(np.float32).reshape((50, 1))

W_0 = Normal(mu=tf.zeros([1, 2]), sigma=tf.ones([1, 2]))
W_1 = Normal(mu=tf.zeros([2, 1]), sigma=tf.ones([2, 1]))
b_0 = Normal(mu=tf.zeros(2), sigma=tf.ones(2))
b_1 = Normal(mu=tf.zeros(1), sigma=tf.ones(1))

x = x_train
y = Normal(mu=tf.matmul(tf.tanh(tf.matmul(x, W_0) + b_0), W_1) + b_1,
sigma=0.1)

qW_0 = Normal(mu=tf.Variable(tf.zeros([1, 2])),
              sigma=tf.nn.softplus(tf.Variable(tf.zeros([1, 2]))))
qW_1 = Normal(mu=tf.Variable(tf.zeros([2, 1])),
sigma=tf.nn.softplus(tf.Variable(tf.zeros([2, 1]))))
qb_0 = Normal(mu=tf.Variable(tf.zeros(2)),
sigma=tf.nn.softplus(tf.Variable(tf.zeros(2))))
qb_1 = Normal(mu=tf.Variable(tf.zeros(1)),
sigma=tf.nn.softplus(tf.Variable(tf.zeros(1))))

inference = ed.KLqp({W_0: qW_0, b_0: qb_0,
                     W_1: qW_1, b_1: qb_1}, data={y: y_train})
inference.run(n_iter=1000)
"
pyExec(getting_started_example)

pyExec("mu = qW_0.mean().value()")
mu <- pyGet("mu")

x_train <- pyGet("x_train")
y_train <- pyGet("y_train")

inferred <- pyObject("inference")
qW1std <- pyGet("qW_1.std()")
b1std <- pyGet("b_1.std()")

# linear regression example ----
# http://edwardlib.org/tutorials/supervised-regression
lin_reg_test <- 
"
def build_toy_dataset(N, w, noise_std=0.1):
  D = len(w)
  x = np.random.randn(N, D).astype(np.float32)
  y = np.dot(x, w) + np.random.normal(0, noise_std, size=N)
  return x, y

N = 40  # number of data points
D = 10  # number of features

w_true = np.random.randn(D)
X_train, y_train = build_toy_dataset(N, w_true)
X_test, y_test = build_toy_dataset(N, w_true)

X = tf.placeholder(tf.float32, [N, D])
w = Normal(mu=tf.zeros(D), sigma=tf.ones(D))
b = Normal(mu=tf.zeros(1), sigma=tf.ones(1))
y = Normal(mu=ed.dot(X, w) + b, sigma=tf.ones(N))

qw = Normal(mu=tf.Variable(tf.random_normal([D])),
            sigma=tf.nn.softplus(tf.Variable(tf.random_normal([D]))))
qb = Normal(mu=tf.Variable(tf.random_normal([1])),
            sigma=tf.nn.softplus(tf.Variable(tf.random_normal([1]))))

inference = ed.KLqp({w: qw, b: qb}, data={X: X_train, y: y_train})
inference.run()
"
pyExec(lin_reg_test)
pyExecp("foo = qw.mu.eval()")
pyPrint("type(foo)")
pyGet("foo", simplify = FALSE)
pyGet("X_train")
qw_mean <- pyGet("foo")
pyExecp("baz = qw.mean()")
pyPrint("baz")
qw_mean <- pyGet("foo")

# pass data to edward ----
pyExec("test_var = 3")
test_var <- pyGet("test_var")

pySet("nums", runif(100))
pyPrint("nums")
pyPrint("type(nums)")
pyGet("nums")

pySet("df", airquality)
pyPrint("df")

pyExit()