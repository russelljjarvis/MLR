#import Pkg; Pkg.add("RDatasets")
using CSV: write
using CSV
using DataFrames: DataFrame
using Knet
using Random
using RDatasets
using MLDataUtils, Knet

#import Pkg; Pkg.add("StatsBase")
using StatsBase: sample
Random.seed!(123);
using Pkg
function check_install(pkg_name)
   if ! in(pkg_name,keys(Pkg.installed()))
      Pkg.add(pkg_name)
      print("$pkg_name.jl")
      eval("using $pkg_name")
      #exec("using $pkg_name")

   end
end

vs = Vector{String}()
push!(vs,"CSV")
push!(vs,"DataFrames")
push!(vs,"ScikitLearn")
push!(vs,"Knet")
push!(vs,"ArgParse")
push!(vs,"MLDataUtils")
push!(vs,"Plots")
push!(vs,"Statistics")
#push!(vs,"Debugger")
#push!(vs,"Mocha")


for element in vs
   check_install(element)
   print(element)

end

#iris = dataset("datasets", "iris");

if ! isfile("BostonHousing.csv")
    run(`wget https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv`)
end
boston = CSV.File("BostonHousing.csv")
bostonf = DataFrame(boston)
subset_all = bostonf[:,[:lstat,:medv]]
trainb, testb = splitobs(bostonf, at = 0.7);
subset_all = bostonf[:,[:lstat,:medv]]

#xtrn,ytrn = trainb[:,[:lstat,:medv]]
#xtst,ytst = testb[:,[:lstat,:medv]]

xtrn,ytrn = (trainb[:lstat],trainb[:medv])
xtst,ytst = (testb[:lstat],testb[:medv])


xdat = Array([bostonf[:,[:lstat]]]);
ydat = Array([bostonf[:,[:medv]]]);

#ydat = map(x -> x == "setosa" ? 1 : x == "versicolor" ? 2 : 3, ydat);

function partition(xdat::Array{<:AbstractFloat, 2}, ydat::Array{<:Int, 1}, ratio::AbstractFloat = 0.3)
    scnt = size(xdat, 1) / length(unique(ydat));
    ntst = Int(ceil((size(xdat, 1) * ratio) / length(unique(ydat))));
    idx  = Int.(sample(1:(length(ydat) / length(unique(ydat))), ntst, replace = false));
    for i in 2:length(unique(ydat))
        idx = vcat(idx, Int.(sample(((scnt * (i - 1)) + 1):(scnt * i), ntst, replace = false)));
    end
    xtrn = xdat[.!in.(1:length(ydat), Ref(Set(idx))), :];
    ytrn = ydat[.!in.(1:length(ydat), Ref(Set(idx)))];
    xtst = xdat[idx, :];
    ytst = ydat[idx];

    return (xtrn, ytrn, xtst, ytst);
end

#xtrn, ytrn, xtst, ytst = partition(xdat, ydat);

#dtrn =
# minibatch(xtrn, ytrn)#, 10, shuffle = true);
#dtst =
#minibatch(xtst, ytst)#, 10);

# Define the dense layer
struct Dense; w; b; f; end
Dense(i::Int, o::Int, f = relu) = Dense(param(o, i), param0(o), f); # constructor
(d::Dense)(x) = d.f.(d.w * x .+ d.b); # define method for dense layer

# Define the chain layer
struct Chain; layers; end
(c::Chain)(x) = (for l in c.layers; x = l(x); end; x); # define method for feed-forward
(c::Chain)(x, y) = nll(c(x), y, dims = 1); # define method for negative-log likelihood loss function

# Specify the model
model = Chain((Dense(size(xtrn, 2), 10), Dense(10, 3, identity)));

# Train the model for 100 epochs
function accuracy(m::Chain, d::Knet.Data)
    _, yidx = findmax(m(d.x), dims = 1);
    yprd = [i[1] for i in yidx];

    return sum(yprd .== d.y) / length(d.y)
end

err = hcat(nll(model(xtrn),  ytrn), nll(model(dtst.x), dtst.y))
#acc = hcat(accuracy(model, dtrn), accuracy(model, dtst))
for x in adam(model, repeat(dtrn, 100))
    global err = vcat(err, hcat(nll(model(dtrn.x), dtrn.y), nll(model(dtst.x), dtst.y)))
    print(err)
    #global acc = vcat(acc, hcat(accuracy(model, dtrn), accuracy(model, dtst)))
end

# Save loss and accuracy to csv for visualization
write("error-knet.csv", DataFrame(err, [:training, :testing]))
#write("accuracy-knet.csv", DataFrame(acc, [:training, :testing]))

# or even faster without saving the loss
# adam!(model, repeat(dtrn, 100));

# Predict species
_, trn_yidx = findmax(model(xtrn), dims = 1); # training set
trn_yprd = [i[1] for i in trn_yidx];

_, tst_yidx = findmax(model(dtst.x), dims = 1); # testing set
tst_yprd = [i[1] for i in tst_yidx];

# Check accuracy of the model
accuracy(model, dtrn)
accuracy(model, dtst)
print(accuracy(model, dtrn))
