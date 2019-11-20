#import Pkg; Pkg.add("Flux")
using Flux
#layers = [Dense(10, 5, σ), Dense(5, 2), softmax]
#model(x) = foldl((x, m) -> m(x), layers, init = x)



model = Chain(
  Dense(768, 128, σ),
  LSTM(128, 256),
  LSTM(256, 128),
  Dense(128, 10),
  softmax)

data = rand(768, 128)

#loss(x, y) = crossentropy(model(x), y)
W = rand(768,10)
b = rand(768)

fhat(x) = W*x + b

function loss(x,y)
    yhat = fhat(x) # our prediction for y
    return sum((y-yhat).^2)
end


Flux.train!(loss, zip(data,data), ADAM())

#model(rand(10)) # => 2-element vector
# http://blog.yhat.com/posts/julia-neural-networks.html
#using Pkg
##Pkg.clone("https://github.com/EricChiang/ANN.jl.git")
#=using ANN
ann = ArtificialNeuralNetwork(12)

fit!(ann,X_train,y_train,epochs=30,alpha=0.1,lambda=1e-5)

y_proba = predict(ann,X_test)

y_pred = Array(Int64,length(y_test))

for i in 1:length(y_test)
    # must translate class index to label
    y_pred[i] = ann.classes[indmax(y_proba[i,:])]
end

println("Prediction accuracy: ",mean(y_pred .== y_test))
=#
