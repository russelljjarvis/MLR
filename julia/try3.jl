include("paths.jl")
##### Beginning of file

# This file was generated by PredictMD version 0.20.0
# For help, please visit https://www.predictmd.net

import PredictMD

output_directory = pwd()
print(output_directory)
PredictMD.generate_examples("/home/russell/git/MLR/subdir/output"; notebooks = true)
### Begin project-specific settings

#PredictMD.require_julia_version("v0.7.0")

#PredictMD.require_predictmd_version("0.20.0")

# PredictMD.require_predictmd_version("0.20.0", "0.21.0-")

PROJECT_OUTPUT_DIRECTORY = PredictMD.project_directory(
    pwd(),
    "output",
    "boston_housing_example",
    )

### End project-specific settings

### Begin Knet neural network regression code


knetmlp_losshyperparameters = Dict()
knetmlp_losshyperparameters[:L1] = Cfloat(0.0)
knetmlp_losshyperparameters[:L2] = Cfloat(0.0)
knetmlp_optimizationalgorithm = :Adam
knetmlp_optimizerhyperparameters = Dict()
knetmlp_minibatchsize = 48

knet_mlp_regression = PredictMD.single_labeldataframeknetregression(
    feature_names,
    single_label_name;
    package = :Knet,
    name = "Knet MLP",
    predict_function_source = knet_mlp_predict_function_source,
    loss_function_source = knet_mlp_loss_function_source,
    losshyperparameters = knetmlp_losshyperparameters,
    optimizationalgorithm = knetmlp_optimizationalgorithm,
    optimizerhyperparameters = knetmlp_optimizerhyperparameters,
    minibatchsize = knetmlp_minibatchsize,
    modelweights = knetmlp_modelweights,
    maxepochs = 100,
    printlosseverynepochs = 10,
    feature_contrasts = feature_contrasts,
    )


#display(knet_mlp_regression_plot_testing)

PredictMD.singlelabelregressionmetrics(
    knet_mlp_regression,
    training_features_df,
    training_labels_df,
    single_label_name,
    )

PredictMD.singlelabelregressionmetrics(
    knet_mlp_regression,
    testing_features_df,
    testing_labels_df,
    single_label_name,
    )

knet_mlp_regression_filename = joinpath(
    PROJECT_OUTPUT_DIRECTORY,
    "knet_mlp_regression.jld2",
    )

PredictMD.save_model(knet_mlp_regression_filename, knet_mlp_regression)

### End Knet neural network regression code
