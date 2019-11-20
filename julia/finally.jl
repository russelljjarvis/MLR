using Flux
using Flux.Tracker, Statistics, DelimitedFiles
using Flux.Tracker: Params, gradient, update!
using Flux: gpu
using Flux: onehot, chunk, batchseq, throttle, crossentropy
using Flux: @epochs

# This replicates the housing data example from the Knet.jl readme. Although we
# could have reused more of Flux (see the mnist example), the library's
# abstractions are very lightweight and don't force you into any particular
# strategy.
# https://github.com/FluxML/model-zoo/blob/master/other/housing/housing.jl
cd(@__DIR__)

isfile("housing.data") ||
  download("https://raw.githubusercontent.com/MikeInnes/notebooks/master/housing.data",
           "housing.data")

rawdata = readdlm("housing.data")'


using CSV, DataFrames
file = "BostonHousing.csv"

df = DataFrame(CSV.File(file)) #|> DataFrame!
#df[:medv]
#
lstat = df[:lstat]
medv = df[:medv]

print(names(df))
# The last feature is our target -- the price of the house.
split_ratio = 0.5 # For the train test split

x = rawdata[12:12,:] |> gpu
y = rawdata[14:14,:] |> gpu
Xs = x
Ys = y
# Normalise the data
x = (x .- mean(x, dims = 2)) ./ std(x, dims = 2)

# Split into train and test sets
split_index = floor(Int,size(x,2)*split_ratio)
x_train = x[:,1:split_index]
y_train = y[:,1:split_index]
x_test = x[:,split_index+1:size(x,2)]
y_test = y[:,split_index+1:size(x,2)]

# The model
W = param(randn(1,1)/10) |> gpu
b = param([0.]) |> gpu

predict(x) = W*x .+ b
meansquarederror(ŷ, y) = sum((ŷ .- y).^2)/size(y, 2)
loss(x, y) = meansquarederror(predict(x), y)

η = 0.1
θ = Params([W, b])

for i = 1:12
  g = gradient(() -> loss(x_train, y_train), θ)
  for x in θ
    update!(x, -g[x]*η)
  end
  @show loss(x_train, y_train)
end

# Predict the RMSE on the test set
err = meansquarederror(predict(x_test),y_test)
println(err)

# https://github.com/FluxML/model-zoo/blob/master/text/char-rnn/char-rnn.jl
m = Chain(
  LSTM(1, 505),
  LSTM(505, 505),
  Dense(505, 1),
  softmax)


#modelReg = Chain(Dense(1, 505))
#loss(x, y) = Flux.mse(modelReg(x), y)
#opt = SGD(Flux.params(modelReg), 0.1)
#@epochs 100 Flux.train!(loss, regData, opt)

m = gpu(m)

#function loss(xs, ys)#
#  l = sum(crossentropy.(m.(gpu.(xs)), gpu.(ys)))
#  Flux.truncate!(m)
#  return l
#end
opt = SGD(params(m),0.1)
#
#opt = SGD()
#opt() # updates the weights

tx, ty = (Xs[5], Ys[5])
evalcb = () -> @show loss(tx, ty)
@epochs 100 Flux.train!(loss, zip(Xs, Ys), opt)
opt = ADAM(0.01)
@epochs 100 Flux.train!(loss, params(m), zip(Xs, Ys), opt, cb = throttle(evalcb, 30))
