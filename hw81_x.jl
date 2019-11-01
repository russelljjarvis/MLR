using Pkg

function check_install(pkg_name)
   if ! in(pkg_name,keys(Pkg.installed()))
      Pkg.add(pkg_name)
   end
end
vs = Vector{String}()
push!(vs,"CSV")
push!(vs,"DataFrames")
push!(vs,"ScikitLearn")
for element in vs
   check_install(element)
   # using element
end
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
using ScikitLearn.CrossValidation: cross_val_score
# accuracy = cross_val_score(model, features, labels, cv=3)




# A Simple TreeLet’s fit a single tree to the data using the R package rpart.First we fit a big tree by using a small
# cp(the .0001 below).
# library(rpart)set.seed(99)
# big.tree =rpart(DelIn2Yr~.,data=ktr, control=rpart.control(cp=.0001))
# nbig =length(unique(big.tree$where))cat("size of big tree: ",nbig,"\n")## size of big tree:  376
# head(big.tree$where)
Pkg.add("RData")
