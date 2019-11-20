using Pkg
using CSV
using DataFrames
using MLDataUtils, Knet
using IJulia, Knet
#using ScikitLearn.CrossValidation: cross_val_score
#using StatsPlots



#notebook(dir=Knet.dir("tutorial"))
if ! isfile("BostonHousing.csv")
    run(`wget https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv`)
end
boston = CSV.File("BostonHousing.csv")
bostonf = DataFrame(boston)
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
push!(vs,"PredictMD")

for element in vs
   check_install(element)
   print(element)

end
