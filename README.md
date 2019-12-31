Understanding Logistic Regression from Scratch
================
Jun
Dec 29, 2019

Introduction
------------

In statistics and data science, logistic regression is used to predict the probability of a certain class or event. Usually, the model is binomial, but can also extend to multinomial. It probably is one of the simplest yet extremely useful models for a lot of applications, with its fast implementation and ease of interpretation.

This tutorial will focus on the binomial logistic regression (with possible follow up on a multinomial model). I will discuss the basics of the logistic regression, how it is related to linear regression and how to construct the model in `R` using simply the matrix operation. Using only math and matrix operation (not the built-in model in `R`) will help us understand logistic regression under the hood.

Finally, I will use the constructed model to classify some generated data and show the decision boundary.

You can also see the tutorial [here](https://towardsdatascience.com/logistic-regression-from-scratch-in-r-b5b122fd8e83).
