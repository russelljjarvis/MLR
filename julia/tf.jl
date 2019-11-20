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
push!(vs,"TensorFlow")
push!(vs,"Distributions")
push!(vs,"Printf")

for element in vs
   check_install(element)
   print(element)
end
using TensorFlow
using Distributions
using Printf


# Build the model
sess = Session(Graph())

X = placeholder(Float64, shape=[-1, 50])
Y_obs = placeholder(Float64, shape=[-1, 10])

variable_scope("logisitic_model"; initializer=Normal(0, .001)) do
    global W = get_variable("W", [50, 10], Float64)
    global B = get_variable("B", [10], Float64)
end

Y=nn.softmax(X*W + B)

Loss = -reduce_sum(log(Y).*Y_obs)
optimizer = train.AdamOptimizer()
minimize_op = train.minimize(optimizer, Loss)
saver = train.Saver()

# Run training
run(sess, global_variables_initializer())
checkpoint_path = mktempdir()
@info("Checkpoint files saved in $checkpoint_path")
for epoch in 1:100
    cur_loss, _ = run(sess, [Loss, minimize_op], Dict(X=>x, Y_obs=>y))
    println(@sprintf("Current loss is %.2f.", cur_loss))
    train.save(saver, sess, joinpath(checkpoint_path, "logistic"), global_step=epoch)
end
