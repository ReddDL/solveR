# Author: DE LEON, Richard Emmanuel D. 
# Created for CMSC 150 Final Project
# Implements Polynomical Regression

# MATRIX THAT RETURNS THE AUGMENTED COEFFICIENT MATRIX AND THE VARIABLES IN A LIST (Taken from previous exercise)
AugCoeffMatrix = function(XPoints, YPoints, order, verbose){
  # XPoints is the first column of the table
  # YPoints is the second column of the table
  
  if(verbose){
    print(XPoints)
    print(YPoints)
  }
  # Initialize matrix to hold values
  augMat = matrix(data = NA, 
                  nrow = (order+1), 
                  ncol = (order+1), 
                  dimnames = list(1:(order+1),1:(order+1)))
  
  # Initial Matrix
  for(i in 1:(order+1)){
    for(j in 1:(order+1)){
      augMat[i,j] = sum(XPoints^((j-1)+(i-1)))
      
      if (verbose){
        print(paste("i is: ", i))
        print(paste("j is: ", j))
        print(augMat)
      }
    }
  }
  
  # SOLVE FOR RHS
  RHS = c()
  for (i in 0:(order)){
    currentSum = 0
    for (j in 1:(length(XPoints))){
      currentSum = currentSum + (XPoints[j]^i * YPoints[j])
    }
    RHS[i+1] = currentSum
  }
  
  augMat = cbind(augMat,RHS)
  if(verbose){
    print(RHS)
    print(augMat)
  }
  
  randomVariables = c()
  # CREATE VARIABLES 
  for (i in 1:(order+1)){
    randomVariables[i] = paste("x ^",(i-1))
  }
  
  if (verbose){
    print(randomVariables)
  }
  
  augcoeffmatrix = list(augcoeffmatrix = augMat, variables = randomVariables)
  return(augcoeffmatrix)
  
}

# USE GAUSS-JORDAN ELIMINATION METHOD TO SOLVE FOR THE SOLUTION SET OF THE MATRIX (Taken from previous exercise)
GaussJordanMethod = function(AugMatrixList){
  nVar = length(AugMatrixList$variables)
  augMat = AugMatrixList$augcoeffmatrix
  print(nVar)
  
  matIterations = list()
  matIterations[[1]] = augMat
  solutionSet = c()
  for(i in 1: nVar){
    if (i != nVar){
      pivotRowNum = which.max(abs(augMat[(i:nVar), i]))
      pivotRowNum = pivotRowNum + (i-1)
      
      if (augMat[pivotRowNum, i] == 0){
        print("No unique solution")
        return(solutionSet(NA));
      }
      
      tempRow = augMat[pivotRowNum,]
      augMat[pivotRowNum,] = augMat[i,]
      augMat[i,] = tempRow
      augMat = augMat
    }
    
    augMat[i,] = augMat[i,] / augMat[i,i]
    
    for (j in 1: nVar){
      if (i == j){
        next;
      } else {
        normalizedRow = augMat[j,i] * augMat[i,]
        augMat[j,] = augMat[j,] - normalizedRow
      }
      label = paste("Iteration: ", (i-1))
      matIterations[[i+1]] = label
      matIterations[[i+1]] = augMat
    }
  }
  # print(augMat)
  solutionSet = augMat[,(nVar + 1)]
  
  result = list(variables = AugMatrixList$variables, augcoeffmatrix = augMat, solution = solutionSet, matHistory = matIterations)
  return(result)
}

# USES THE GAUSSIAN METHOS OF ELIMINATION TO GIVE SOLUTION SET OF THE MATRIX (Taken from previous exercise)
GaussianMethod = function(AugMatrixList){
  nVar = length(AugMatrixList$variables)
  augMat = AugMatrixList$augcoeffmatrix
  
  matIterations = list()
  matIterations[[1]] = augMat
  solutionSet = c()
  
  # forward elimination
  for(i in 1: (nVar - 1)){
    # print(paste("Iteration is: ", i))
    pivotRowNum = which.max(abs(augMat[(i:nVar), i]))
    
    # idk why like this
    pivotRowNum = pivotRowNum + (i-1)
    # print(paste("Pivot row num is: ", pivotRowNum))
    
    if (augMat[pivotRowNum, i] == 0){
      print("No unique solution")
      return (solutionSet(NA))
    }
    
    tempRow = augMat[pivotRowNum,]
    augMat[pivotRowNum,] = augMat[i,]
    augMat[i,] = tempRow
    augMat = augMat
    
    for (j in (i+1):nVar){
      #FIND PIVOT ELEMENT
      pivotElement = augMat[i,i]
      # FIND MULTIPLIER
      multiplier = (augMat[j,i] / pivotElement)
      #FIND NORMALIZED ROW
      normalizedRow = multiplier * augMat[i,]
      #update
      augMat[j,] = augMat[j,] - normalizedRow
      label = paste("Iteration: ", (i-1))
      matIterations[[i+1]] = label
      matIterations[[i+1]] = augMat
    }
  }
  
  # print(augMat[,nVar+1][1])
  
  # backward substitution
  for(k in (nVar): 1){
    solutionSet[k] = (augMat[k, nVar + 1] - sum(augMat[k, (k+1):nVar] * solutionSet[(k+1):nVar])) / augMat[k,k]
  }
  result = list(variables = AugMatrixList$variables, augcoeffmatrix = augMat, solution = solutionSet,  matHistory = matIterations)
  return (result);
}

# GETS THE NTH ORDER POLYNOMIAL GIVEN DATA AND A DEGREE
PolynomialRegression = function(order, data, estimate, verbose){
  # XPoints is the first column of the dataset
  XPoints = data[[1]]
  # YPoints is the second column of the dataset
  YPoints = data[[2]]

  # NOTE 1: returns NA if order is less than 1
  if (order < 1){
    if (verbose){
      print("The order is " + order)
    }
    return(NA)
  }
  
  # NOTE 2: returns NA if length of dependent and independent variables are different
  if(length(XPoints) != length(YPoints)){
    if (verbose){
      print("XPoints has a length of " + length(Xpoints) + "while YPoints is " + length(YPoints))
    }
    return(NA)
  }
  
  # Returns NA if the order is greater than n - 1
  if(order > (length(XPoints) - 1)){
    if (verbose){
      print("Should have n-1 data points")
    }
    return(NA)
  }
  # The list of the augmented coefficient matrix and the variables
  augMatList = AugCoeffMatrix(XPoints, YPoints, order, verbose)
  eliminatedMat = GaussianMethod(augMatList)
  coefficients = eliminatedMat$solution
  mat_iterations = eliminatedMat$matHistory
  
  # The augmented coefficient matrix
  augcoeffmatrix = augMatList$augcoeffmatrix
  # The random variables
  variables = augMatList$variables
  
  if(verbose){
    print(augcoeffmatrix)
    print(coefficients)
    print(variables)
  }
  
  # Creates a polynomial string
  Polynomial_string = ""
  terms = c()
  # pastes the coefficients and the variables together with "*" and places them in a string vector
  for (i in 0: order + 1){
    terms[i] = paste(coefficients[i], variables[i], sep = " * ")
  }
  
  # Pastes the terms together with "+"
  Polynomial_string = terms[1]
  for (i in 2:(order+1)){
    Polynomial_string = paste(Polynomial_string, terms[i], sep = " + ")
  }
  
  # Adds function(x) to the string
  Polynomial_string = paste("function(x)", Polynomial_string)
  if (verbose == TRUE){
    print(Polynomial_string)
    print(mat_iterations)
  }
  
  # Creates the polynomial function with eval and parse
  Polynomial_function = eval(parse(text = Polynomial_string))
  
  # Gets the estimate given the input
  estimateResult = Polynomial_function(estimate)
  return (list(augcoeffmatrix = augMatList$augcoeffmatrix, estimate = estimateResult, coefficients = coefficients, Polynomial_string = Polynomial_string, Polynomial_function = Polynomial_function, mat_iter = mat_iterations))
}