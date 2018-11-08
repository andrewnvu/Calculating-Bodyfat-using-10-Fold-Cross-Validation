# Calculating-Bodyfat-using-10-Fold-Cross-Validation
Given a dataset with 13 measures (Density was unused), find the best combination of measures to predict body fat using a 10-Fold Cross-Validation.

Uses supervised learning or a form of 'bagging' to create a super-model as I apply the model 10 (number of folds) times.

Applies a 10-Fold Cross-Validation among different combination or predictors to obtain the Mean Squared Errors (MSE) to find the best predictor of bodyfat. 

At least 10 - 15 different combinations were required for project requirements.

The best combination or predictors that I have found to be best:
  log(Height), Neck, Chest, log(Abdomen), Hip, Thigh, Knee, log(Forearm), Wrist

This project was made for my Data Science course at CSUF
