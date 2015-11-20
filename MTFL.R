## OBjective is to develop a 'useful' model that takes in wallnode+wearnode' data and uses to determine feature selection with minimal damage due to imputation
## Logic involves following steps:

### What is the contribution
## (1) Several Integration outputs considered as datasets (max signal, weighted signal, trilateration based). You know what I mean
## (2) MTFL*:
##### (a) Divide the data into subsets (manual for now) based on missing chunks
##### (b) Predict RMSSD for each chunk (few models try with CV)
##### (c) Wrapper based feature selection for each
##### (d) Get the support & final features table with Support and stuff
##### (e) (Optional) Logic for final featue ranking and test(actual) data predictions
##### (f) Try these for interactions and higher order and things!!!
##### (g) Use SGB Lasso, RF and Reg tree (for now)

## Additional Procedure 
## Use reduced modeling (Saar approach), Only imputation approach and MTL approach to predict
### Use the MTFL approach and prove it is better

data_in <- read.csv("Wear_wall_integrated.csv")

data_sens <- data_in
