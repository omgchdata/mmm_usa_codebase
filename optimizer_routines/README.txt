Keep in mind R files will have code specific to their environment (AzureML -- Web Services).

./middle_out -- Middle-Out Optimizer (MOO)
  ./referenced_functions -- functions used in MOO
  ./sample_inputs -- sample inputs for MOO
  ./main_routine -- script to execute MOO

./annalect_optimization -- Optimization Routines from Annalect tweaked for Azure
  ./referenced_functions -- functions used in Annalect Optimization Routines, below
  ./exclusive_reach -- Annalect Exclusive Reach (AER)
    ./sample_inputs -- sample inputs for AER
    ./main_routine -- script to execute AER
  ./paid_reach -- Annalect Paid Reach (APR)
    ./sample_inputs -- sample inputs for APR
    ./main_routine -- script to execute APR