# Bayesian Knowledge Tracing in R

We introduce an **R** implementation of the Bayesian Knowledge Tracing algorithm and its variants, which estimate student cognitive mastery from problem-solving sequences.

The R package `BKT` is publicly available on [GitHub](https://github.com/Feng-Ji-Lab/bkt). It can be installed and loaded using the following R commands:

``` R
devtools::install_github("Feng-Ji-Lab/bkt")
library(BKT)
```

This package is based on the work of [pyBKT](https://github.com/CAHLR/pyBKT).


# Installation and Import

`BKT` can be installed and loaded using the following R commands:

``` R
devtools::install_github("Feng-Ji-Lab/bkt")
library(BKT)
```

# Preparing Data and Running the Model

The following serves as a mini-tutorial for how to get started with `BKT`.

## Input and Output Data

The accepted input formats are data files of type csv (comma separated) or tsv (tab separated). `BKT` will automatically infer which delimiter to use in the case that it is passed a data file. Since column names mapping meaning to each field in the data (i.e. skill name, correct/incorrect) vary per data source, you may need to specify a mapping from your data file's column names to `BKT`'s expected column names. In many cases with Cognitive Tutor and Assistments datasets, `BKT` will be able to automatically infer column name mappings, but in the case that it is unable to, it will raise an error. Note that the correctness is given by -1 (no response), 0 (incorrect), or 1 (correct).

## Creating and Training Models

The process of creating and training models in `BKT` resemble that of SciKit Learn. `BKT` provides easy methods of fetching online datasets and to fit on a combination or all skills available in any particular dataset.

```R
library(BKT)

# Initialize the model with an optional seed
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

# Fetch Assistments and CognitiveTutor data (optional - if you have your own dataset, that's fine too!)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/as.csv", ".")
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

# Train a simple BKT model on all skills in the CT dataset
result_all <- fit(model, data_path = "ct.csv")

# Train a simple BKT model on one skill in the CT dataset
# Note that calling fit deletes any previous trained BKT model!
result_single <- fit(model, data_path = "ct.csv", skills = "Plot imperfect radical")

# Train a simple BKT model on multiple skills in the CT dataset
result_double <- fit(model, data_path = "ct.csv", skills = c("Plot imperfect radical", "Plot pi"))

# View the trained parameters!
print(params(result_all))
```

Note that if we train on a dataset that has unfamiliar columns to `BKT`, you will be required to specify a mapping of column names in that dataset to expected `BKT` columns. This is referred to as the model defaults (i.e. it specifies the default column names to lookup in the dataset). An example usage is provided below for an unknown dataset which has column names "row", "skill\_t", "answer", and "gs\_classes".
```R
# Unfamiliar dataset file.
file = 'mystery.csv'

# For other non-Assistments/CogTutor style datasets, we will need to specify the
# columns corresponding to each required column (i.e. the user ID, correct/incorrect).
# For that, we use a defaults list.
# In this case, the order ID that BKT expects is specified by the column row in the
# dataset, the skill_name is specified by a column skill_t and the correctness is specified
# by the answer column in the dataset.
defaults = {'order_id': 'row', 'skill_name': 'skill_t', 'correct': 'answer'}

# Fit using the defaults (column mappings) specified in the list.
fit(model, data_path = file, defaults = defaults)
```

## Model Prediction and Evaluation

Prediction and evaluation behave similarly to SciKit-Learn. `BKT` offers a variety of features for prediction and evaluation.

```R
library(BKT)

# Initialize the model with an optional seed
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

# Train a simple BKT model on all skills in the CT dataset
fit(model, data_path = 'ct.csv')

# Predict on all skills on the training data.
preds_df = predict(model, data_path = 'ct.csv')

# Evaluate the RMSE of the model on the training data.
# Note that the default evaluate metric is RMSE.
training_rmse = evaluate(model, data_path = 'ct.csv')

# We can define a custom metric as well.
mae <- function(true_vals, pred_vals) {
  """ Calculates the mean absolute error. """
  return(mean(abs(true_vals - pred_vals)))
}
result <- evaluate(result, data_path = "ct.csv", metric = mae)
```

## Crossvalidation

Crossvalidation is offered as a blackbox function similar to a combination of fit and evaluate that accepts a particular number of folds, a seed, and a metric (either one of the provided 'rmse' or a custom R function taking 2 arguments). Similar arguments for the model types, data path/data, and skill names are accepted as with the fit function.

``` R
library(BKT)

# Initialize the model with an optional seed
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

# Crossvalidate with 5 folds on all skills in the CT dataset.
crossvalidated_errors = crossvalidate(model, data_path = 'ct.csv', skills = "Plot non-terminating improper fraction", folds = 5)

# Crossvalidate on a particular set of skills with a given 
# seed, folds and metric.
mae <- function(true_vals, pred_vals) {
  """ Calculates the mean absolute error. """
  return(mean(abs(true_vals - pred_vals)))
}

# Note that the skills argument accepts a REGEX pattern. In this case, this matches and 
# crossvalidates on all skills containing the word fraction.
crossvalidated_mae_errs = crossvalidate(model, data_path = 'ct.csv', skills = ".*fraction.*",
                                              folds = 5, metric = mae)
```
## Parameter Fixing

Another advanced feature supported by BKT is parameter fixing, where we can fix one or more parameters and train the model conditioned on those fixed parameters. This can be useful if you already know the ground truth value of some parameters beforehand, or to avoid degenerate model creation by fixing parameters at reasonable values. To specify which parameters and values we want fixed for any skill, we can pass in a list to set_coef, and then specify fixed=True in the fit call:

```R
library(BKT)

# Fixes the prior rate to 0.1 for the Plot imperfect radical skill, and trains the model given those fixed parameters.
model <- set_coef(model, list("Plot non-terminating improper fraction" = list("prior" = 0.5)))
result <- fit(model,
    forgets = TRUE,
    data_path = "ct.csv", skills = "Plot non-terminating improper fraction",
    fixed = list("Plot non-terminating improper fraction" = list("prior" = TRUE))
)
print(params(result))
```

Within the set_coef list, the 'prior' parameter takes a scalar, while 'learns', 'forgets', 'guesses', and 'slips' takes an np.array, in order to provide support for parameter fixing in model extensions with multiple learn or guess classes. An example of such is shown below. 

```R
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
model <- set_coef(model, list("Plot non-terminating improper fraction" = list("learns" = c(0.25), "forgets" = c(0.25))))
print(params(model))
result <- fit(model,
    data_path = "ct.csv", forgets = TRUE, skills = "Plot non-terminating improper fraction",
    fixed = list("Plot non-terminating improper fraction" = list("learns" = TRUE, "forgets" = TRUE))
)
print(params(result))
```

# Internal Data Format

`BKT` models student mastery of a skills as they progress through series of learning resources and checks for understanding. Mastery is modelled as a latent variable has two states - "knowing" and "not knowing". At each checkpoint, students may be given a learning resource (i.e. watch a video) and/or question(s) to check for understanding. The model finds the probability of learning, forgetting, slipping and guessing that maximizes the likelihood of observed student responses to questions. 

To run the `BKT` model, define the following variables:
* `num_subparts`: The number of unique questions used to check understanding. Each subpart has a unique set of emission probabilities.
* `num_resources`: The number of unique learning resources available to students.
* `num_fit_initialization`: The number of iterations in the EM step.


Next, create an input object `Data`, containing the following attributes: 
* `data`: a matrix containing sequential checkpoints for all students, with their responses. Each row represents a different subpart, and each column a checkpoint for a student. There are three potential values: {0 = no response or no question asked, 1 = wrong response, 2 = correct response}. If at a checkpoint, a resource was given but no question asked, the associated column would have `0` values in all rows. For example, to set up data containing 5 subparts given to two students over 2-3 checkpoints, the matrix would look as follows:

        | 0  0  0  0  2 |
        | 0  1  0  0  0 |
        | 0  0  0  0  0 |
        | 0  0  0  0  0 |
        | 0  0  2  0  0 |   

  In the above example, the first student starts out with just a learning resource, and no checks for understanding. In subsequent checkpoints, this student also responds to subpart 2 and 5, and gets the first wrong and the second correct.   

* `starts`: defines each student's starting column on the `data` matrix. For the above matrix, `starts` would be defined as: 

        | 1  4 |

* `lengths`: defines the number of check point for each student. For the above matrix, `lengths` would be defined as: 

        | 3  2 |

* `resources`: defines the sequential id of the resources at each checkpoint. Each position in the vector corresponds to the column in the `data` matrix. For the above matrix, the learning `resources` at each checkpoint would be structured as: 

        | 1  2  1  1  3 |

* `stateseqs`: this attribute is the true knowledge state for above data and should be left undefined before running the `pyBKT` model. 


The output of the model can will be stored in a `fitmodel` object, containing the following probabilities as attributes: 
* `As`: the transition probability between the "knowing" and "not knowing" state. Includes both the `learns` and `forgets` probabilities, and their inverse. `As` creates a separate transition probability for each resource.
* `learns`: the probability of transitioning to the "knowing" state given "not known".
* `forgets`: the probability of transitioning to the "not knowing" state given "known".
* `prior`: the prior probability of "knowing".

The `fitmodel` also includes the following emission probabilities:
* `guesses`: the probability of guessing correctly, given "not knowing" state.
* `slips`: the probability of picking incorrect answer, given "knowing" state.
