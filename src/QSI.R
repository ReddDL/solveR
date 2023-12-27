# DE LEON, Richard Emmanuel D. 
# Created for project in CMSC 150 
# Implements Quadratic Spline Interpolation

# ============ Implements Gauss Jordan method of elimination
GaussJordanMethod = function(AugMatrixList){
  nVar = length(AugMatrixList$variables) - 1
  augMat = AugMatrixList$augcoeffmatrix
  
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
  solutionSet = augMat[,(nVar + 1)]
  
  result = list(variables = AugMatrixList$variables, augcoeffmatrix = augMat, solution = solutionSet, matHistory = matIterations)
  return(result)
}

# ============ Implements Quadratic Spline Interpolation
quadraticSplineInterpolation = function(data, estimate, verbose){
  # Sorts the data by the X Points
  data = data[order(data[,1]), ]
  
  # Gets the x and y values
  xValues = data[,1]
  yValues = data[,2]
  
  #CASES
  # 1. Estimate is less than the minimum data point
  # 2. Estimate is greater than the maximum data point
  # 3. x and y are not equal 
  # 4. The data has less than 3 data points
  if (estimate < xValues[1] || estimate > xValues[length(xValues)] || length(xValues) != length(yValues) || length(xValues) < 3){
    return (NULL)
  }
  
  #The number of intervals
  dataLength = length(data[[1]])
  numOfSplines = (dataLength - 1)
  numOfUnknowns = (3 * numOfSplines)

  # Create Matrix 
  twoConsecPointsMatrix = matrix(0, nrow = numOfSplines * 2, ncol = (numOfUnknowns + 1))
  
  # Get the rows and columns of the matrix
  rows = nrow(twoConsecPointsMatrix)
  cols = ncol(twoConsecPointsMatrix)
  
  # Attach first row
  twoConsecPointsMatrix[1,1] = xValues[1]^2
  twoConsecPointsMatrix[1,2] = xValues[1]^1
  twoConsecPointsMatrix[1,3] = xValues[1]^0
  twoConsecPointsMatrix[1,cols] = yValues[1]
  
  currentColumn = 1
  row = 2
  
  # 1. Each quadratic splines passes through two consecutive data points
  # i is the row
  for (i in 2:numOfSplines){
      twoConsecPointsMatrix[row,currentColumn] = xValues[i]^2
      twoConsecPointsMatrix[row,currentColumn + 1] = xValues[i]^1
      twoConsecPointsMatrix[row,currentColumn + 2] = xValues[i]^0
      twoConsecPointsMatrix[row,cols] = yValues[i]
      
      row  = row + 1
      currentColumn = currentColumn + 3
      
      twoConsecPointsMatrix[row,currentColumn] = xValues[i]^2
      twoConsecPointsMatrix[row,currentColumn + 1] = xValues[i]^1
      twoConsecPointsMatrix[row,currentColumn + 2] = xValues[i]^0
      twoConsecPointsMatrix[row,cols] = yValues[i]

      row = row + 1
  }
  
  # Attach the last row
  twoConsecPointsMatrix[numOfSplines * 2,cols - 3] = xValues[dataLength]^2
  twoConsecPointsMatrix[numOfSplines * 2, cols - 2] = xValues[dataLength]^1
  twoConsecPointsMatrix[numOfSplines * 2, cols - 1] = xValues[dataLength]^0
  twoConsecPointsMatrix[numOfSplines * 2, cols] = yValues[dataLength]
  
  # 2. Quadratic Splines have continuous derivatives at the interior data points
  # Set the row and column
  derivColumn = 1
  derivRow = 1
  # Create matrix
  derivInteriorMatrix = matrix(0,nrow = numOfSplines, ncol = (numOfUnknowns+1))
  # Iterate and place in matrix
  for(i in 2:(dataLength-1)){ # i is the xvalues
      derivInteriorMatrix[derivRow,derivColumn] = xValues[i] * 2
      derivInteriorMatrix[derivRow,derivColumn + 1] = xValues[i]^0
      derivInteriorMatrix[derivRow,derivColumn + 2] = 0
      derivInteriorMatrix[derivRow,derivColumn + 3] = (-1) * xValues[i] * 2
      derivInteriorMatrix[derivRow,derivColumn + 4] = (-1) * xValues[i]^0
      derivRow = derivRow + 1
      derivColumn = derivColumn + 3
  }
  
  derivInteriorMatrix[nrow(derivInteriorMatrix), 1] = 1

  augMatToSolve = rbind(twoConsecPointsMatrix, derivInteriorMatrix)

  # Create column names 
  randomVariables = c()
  for (i in 1:((cols-1)/3)){
    randomVariables = c(randomVariables, paste("a",i, sep=""))
    randomVariables = c(randomVariables, paste("b",i, sep=""))
    randomVariables = c(randomVariables, paste("c",i, sep=""))
  }
  randomVariables = c(randomVariables, "RHS")
  colnames(augMatToSolve) = randomVariables

  augMatList = list(augcoeffmatrix = augMatToSolve, variables = randomVariables)
  eliminatedMat = GaussJordanMethod(augMatList)
  solutionSet = eliminatedMat$solution
  mat_iterations = eliminatedMat$matHistory
  
  # Create intervals
  intervalList = c()
  for (i in 1:numOfSplines){
    intervalString = paste(xValues[i], "<= x")
    intervalString = paste(intervalString, "<=", xValues[i+1])
    intervalList[i] = intervalString
  }

  # Create functions and add to list
  functionList = c()
  current = 1
  for (i in 1:numOfSplines){
    functionString = "function(x)"
    functionString = paste(functionString, paste(solutionSet[current], "* x ^ 2 +"))
    functionString = paste(functionString, paste(solutionSet[current + 1], "* x ^ 1 +"))
    functionString = paste(functionString, paste(solutionSet[current + 2], "* x ^ 0"))
    current = current + 3
    functionList[i] = functionString
  }
  
  if(verbose == TRUE){
    print(functionList)
  }
  
  intervalsAndFunctions = data.frame(intervalList,functionList)

  # Check which function to use
  for ( i in 1:numOfSplines){
    if (xValues[i] <= estimate & estimate <= xValues[i+1]){
      functionIndexToUse = i
      intervalToUse = i
    }
  }
  # Which interval to use
  xInterval = intervalList[intervalToUse]
  
  # Create the function
  PolynomialFunction = eval(parse(text = functionList[functionIndexToUse]))
  #Evaluate estimate
  evaluatedFunction = PolynomialFunction(estimate)
  # Create labelled list
  toReturn = list(intervals = intervalsAndFunctions, estimate = evaluatedFunction, fxn = PolynomialFunction, xInterval = xInterval, mat_iter = mat_iterations)
  return(toReturn)
}
