using Pkg
#Pkg.add("Knet")
using IJulia
#Pkg.add("Debug")
#ENV["GRDIR"]=""
#Pkg.build("GR")
#using Debug
#Main.JuliaInterpreter.@bp

#Pkg.rm("StatPlots")
#Pkg.add("StatPlots")

using Knet;
using DataFrames
using Plots;
using StatsPlots;

include(Knet.dir("data","housing.jl"));
x,y = housing();
#Base.@debug()
#Now that we have the data
predict(ω, x) = ω[1] * x .+ ω[2];
loss(ω, x, y) = mean(abs2, predict(ω, x)-y);
lossgradient = grad(loss);

function train(ω, data; lr=0.01)
    for (x,y) in data
        dω = lossgradient(ω, x, y)
        for i in 1:length(ω)
            ω[i] -= dω[i]*lr
        end
    end
    return ω
end;

gr();
scatter(x', y[1,:], layout=(3,5), reg=true, size=(950,500))

ω = Any[ 0.1*randn(1,13), 0.0 ];
errdf = DataFrame(Epoch=1:20, Error=0.0);
let
    cntr=1
    cntr::Int64
    println(cntr)
    for i=1:200
        train(ω, [(x,y)])
        if mod(i, 10) == 0
            println("Epoch $i: $(round(loss(ω,x,y)))")
            errdf[cntr, :Epoch]=i
            errdf[cntr, :Error]=loss(ω,x,y)
            cntr+=1
        end
    end
end

p1 = scatter(errdf[:,:Epoch], errdf[:,:Error], xlabel="Epoch", ylabel="Error")
p2 = scatter(predict(ω, x)', y', reg=true, xlabel="Predicted", ylabel="Observed")
plot(p1, p2, layout=(1,2), size=(950,500))

ω = Any[0.1f0*randn(Float32,64,13), zeros(Float32,64,1),
        0.1f0*randn(Float32,15,64), zeros(Float32,15,1),
        0.1f0*randn(Float32,1,15),  zeros(Float32,1,1)]
function predict(ω, x)
    x = mat(x)
    for i=1:2:length(ω)-2
        x = relu.(ω[i]*x .+ ω[i+1])
    end
    return ω[end-1]*x .+ ω[end]
end

loss(ω, x, y) = mean(abs2, predict(ω, x)-y)
lossgradient = grad(loss)

errdf = DataFrame(Epoch=1:60, Error=0.0)
let
    cntr = 1
    for i=1:600
        train(ω, [(x,y)])
        if mod(i, 10) == 0
            errdf[cntr, :Epoch]=i
            errdf[cntr, :Error]=loss(ω,x,y)
            cntr+=1
        end
    end
end



p3 = scatter(errdf[:,:Epoch], errdf[:,:Error], xlabel="Epoch", ylabel="Error")
p4 = scatter(predict(ω, x)', y', reg=true, xlabel="Predicted", ylabel="Observed")
plot(p3, p4, layout=(1,2), size=(950,500))
plot(p2, p4, layout=(1,2), size=(950,500))


xtrn, xtst = x[:, 1:400], x[:, 401:end]
ytrn, ytst = y[:, 1:400], y[:, 401:end]

ω = Any[0.1f0*randn(Float32,64,13), zeros(Float32,64,1),
        0.1f0*randn(Float32,15,64), zeros(Float32,15,1),
        0.1f0*randn(Float32,1,15),  zeros(Float32,1,1)]
errdf = DataFrame(Epoch=1:60, TrnError=0.0, ValError=0.0)
let
    cntr = 1
    for i=1:600
        train(ω, [(xtrn, ytrn)])
        if mod(i, 10) == 0
            errdf[cntr, :Epoch]=i
            errdf[cntr, :TrnError]=loss(ω,xtrn,ytrn)
            errdf[cntr, :ValError]=loss(ω,xtst,ytst)
            cntr+=1
        end
    end
end

#using StatPlots
@df errdf[5:60,:] plot(:Epoch, [:ValError, :TrnError], xlabel="Epoch", ylabel="Error",
                       label=["Validation" "Training"], lw=3)
gui()
#using PyPlot
# use x = linspace(0,2*pi,1000) in Julia 0.6
#x = range(0; stop=2*pi, length=1000); y = sin.(3 * x + 4 * cos.(2 * x));
#plot(x, y, color="red", linewidth=2.0, linestyle="--")
#title("A sinusoidally modulated sinusoid")
#After doing

#predict
#Epoch 10:
