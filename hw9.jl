using Pkg
using CSV
using DataFrames
using MLDataUtils, Knet
using IJulia, Knet
using ScikitLearn.CrossValidation: cross_val_score
using StatsPlots

#notebook(dir=Knet.dir("tutorial"))
if ! isfile("BostonHousing.csv")
    run(`wget https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv`)
end
boston = CSV.File("BostonHousing.csv")
bostonf = DataFrame(boston)
function check_install(pkg_name)
   if ! in(pkg_name,keys(Pkg.installed()))
      Pkg.add(pkg_name)
      include("$pkg_name.jl")
   end
end
vs = Vector{String}()
push!(vs,"CSV")
push!(vs,"DataFrames")
push!(vs,"ScikitLearn")
push!(vs,"Knet")
push!(vs,"ArgParse")
push!(vs,"MLDataUtils")
push!(vs,"Plots")#

for element in vs
   check_install(element);
end
if ! in("MLPreprocessing",keys(Pkg.installed()))
    Pkg.clone("https://github.com/JuliaML/Learn.jl")
    Pkg.build("Learn")
using MLPreprocessing


predict(w,x)=(w[1]*x.+w[2])

loss(w,x,y)=(sum(abs2,y-predict(w,x)) / size(x,2))

lossgradient = grad(loss)

function train(w, x, y; lr=.1, epochs=20)
    for epoch=1:epochs
        g = lossgradient(w, x, y)
        update!(w, g; lr=lr)
    end
    return w
end
seed=-1; #help="random number seed: use a nonnegative int for repeatable results")
epochs=20; #help="number of epochs for training")
lr=0.1;
test=0.0;
#atype::Array{Float32}
gcheck=0;
seed::Int;
epochs::Int;
lr::Float64; #help="learning rate")
test::Float64; # help="ratio of data to split for testing")
#atype::KnetArray{Float32};# help="array type: Array for cpu, KnetArray for gpu")
#fast::action=:store_true;# help="skip loss printing for faster run")
gcheck::Int;  #help="check N random gradients")


#x = rand(4)
#Dtrain = DataFrame(A=rand(10), B=collect(1:10), C=[string(x) for x in 1:10])
#Dtest = DataFrame(A=rand(10), B=collect(1:10), C=[string(x) for x in 1:10])



trainb, testb = splitobs(bostonf, at = 0.7);
subset_all = bostonf[:,[:lstat,:medv]]

subset_train = trainb[:,[:lstat,:medv]]
subset_test = testb[:,[:lstat,:medv]]
scaler = fit(StandardScaler, subset_train)
transform(subset_test, scaler)

#features = convert(Matrix, subset_all)
#features = float.(features)


clf = fit(StandardScaler, subset_all)
#PreProcessing.StandardScaler{Float64}([-1.875, 3.75, 1.5, -1.875], [5.55512, 5.06828, 6.61438, 5.92532], 4, 1)
xnew = transform(clf, features)

#scaler = fit(StandardScaler, subset, obsdim=1, operate_on=[1,3])
transform(subset, scaler)



# Scatter plot with some custom settings
@df subset scatter(:lstat, :medv, title = "Boston Julia", xlabel = "lstat", ylabel = "median value",m=(0.5, [:cross :hex :star7], 12), bg=RGB(.2,.2,.2))

# save a png
png("iris")


#=
report(epoch)=println((:epoch,epoch,:trn,loss(w,xtrn,ytrn),:tst,loss(w,xtst,ytst)))
if o[:fast]
    @time (train(w, xtrn, ytrn; lr=o[:lr], epochs=o[:epochs]); gpu()>=0 && Knet.cudaDeviceSynchronize())
else
    report(0)
    @time for epoch=1:o[:epochs]
        train(w, xtrn, ytrn; lr=o[:lr], epochs=1)
        report(epoch)
        if o[:gcheck] > 0
            gradcheck(loss, w, xtst, ytst; gcheck=o[:gcheck], verbose=true)
        end
    end
end


#return w
#end

# This allows both non-interactive (shell command) and interactive calls like:
# $ julia housing.jl --epochs 10
# julia> Housing.main("--epochs 10")
#PROGRAM_FILE=="housing.jl" && main(ARGS)

end # module Housing


# check_install("DecisionTree")
using DecisionTree
if ! in("DataFrames",keys(Pkg.installed()))
   Pkg.add("DataFrames")
end
if ! in("CSV",keys(Pkg.installed()))
   Pkg.add("CSV")
end
using CSV
using DataFrames
if ! isfile("kaggle-del-train.csv")
   run(`wget http://www.rob-mcculloch.org/data/kaggle-del-train.csv`)
end
if ! isfile("kaggle-del-test.csv")
   run(`wget http://www.rob-mcculloch.org/data/kaggle-del-test.csv`)
end

ktr = CSV.File("kaggle-del-train.csv")
kte = CSV.File("kaggle-del-test.csv")
ktef = DataFrame(ktr)
ktrf = DataFrame(kte)
print(varinfo())
labels = names(ktef);
features = convert(Matrix, ktef)
features = features'


labels = string.(labels)
features = float.(features)
# describe(ktef)
model = DecisionTreeClassifier(max_depth=2)
fit!(model, features, labels)
print_tree(model, 5)
println(get_classes(model)) # returns the ordering of the columns in predict_proba's output
# run n-fold cross validation over 3 CV folds
# See ScikitLearn.jl for installation instructions
# accuracy = cross_val_score(model, features, labels, cv=3)




# A Simple TreeLet’s fit a single tree to the data using the R package rpart.First we fit a big tree by using a small
# cp(the .0001 below).
# library(rpart)set.seed(99)
# big.tree =rpart(DelIn2Yr~.,data=ktr, control=rpart.control(cp=.0001))
# nbig =length(unique(big.tree$where))cat("size of big tree: ",nbig,"\n")## size of big tree:  376
# head(big.tree$where)
=#
