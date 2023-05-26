
# Linear regression

# If MSR is correlated with PCR status, then we can use MSR as appr. for disease....

# linear regression, MSQ vs selected variables
lm(log(MSQ) ~ BES_ID + PARITY, data = animal_data) #FIX: add PCR and parity in data.. Or use PCR data...

