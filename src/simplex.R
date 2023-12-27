# DE LEON, Richard Emmanuel D. 
# Created as part of the final project in CMSC 150 
# Implements cost minimization of foods using simplex mixed constraints

# Limit the digits
options(scipen = 100)

# The data for the foods stored in a matrix
nutriFacts <- matrix(
  data = c(0.16, 73.8, 0, 0.8, 68.2, 13.6, 8.5,	8, 5867.4, 160.2,	159, 2.3,
           0.07, 23.7, 0,	0.1, 19.2, 5.6, 1.6, 0.6, 15471, 5.1, 14.9,	0.3,
           0.04, 6.4,	0, 0.1,	34.8,	1.5, 0.7,	0.3,	53.6, 2.8, 16, 0.2,
           0.18, 72.2, 0,	0.6, 2.5,	17.1, 2, 2.5,	106.6, 5.2, 3.3, 0.3,
           0.02, 2.6, 0, 0, 1.8, 0.4,	0.3, 0.2,	66, 0.8, 3.8, 0.1,
           0.53, 20, 0,	0.1, 1.5,	4.8, 1.3, 0.7, 467.7,	66.1,	6.7, 0.3,
           0.06, 171.5, 0, 0.2, 15.2, 39.9, 3.2, 3.7, 0, 15.6, 22.7, 4.3,
           0.31, 88.2, 0, 5.5, 8.1, 2.2, 1.4, 9.4, 98.6, 0.1, 121.8, 6.2,
           0.84, 277.4, 129.9, 10.8, 125.6, 0, 0, 42.2, 77.4, 0, 21.9, 1.8,
           0.78, 358.2, 0, 12.3, 1237.1, 58.3, 11.6, 8.2, 3055.2, 27.9, 80.2, 2.3,
           0.27, 25.8, 0, 0.4, 11.1, 5.7, 1.4, 1, 766.3, 23.5, 6.2, 0.6,
           0.24, 81.4, 0, 0.5, 0, 21, 3.7, 0.3, 73.1, 7.9, 9.7, 0.2,
           0.15, 104.9, 0, 0.5, 1.1, 26.7, 2.7, 1.2, 92.3, 10.4, 6.8, 0.4,
           0.32, 15.1, 0, 0.1, 0.5, 4.1, 0.2, 0.2, 24, 1, 3.4, 0.1,
           0.49, 46.4, 0, 0.3, 3.8, 11.3, 2.6, 0.8, 133, 74.5, 19.8, 0.3,
           0.15, 61.6, 0, 0.2, 0, 15.4, 3.1, 1.2, 268.6, 69.7, 52.4, 0.1,
           0.16, 78, 0, 0.5, 151.4, 15.1, 0.6, 3, 0, 0, 21, 1,
           0.05, 65, 0, 1, 134.5, 12.4, 1.3, 2.2, 0, 0, 10.8, 0.7,
           0.06, 65, 0, 1, 132.5, 11.8, 1.1, 2.3, 0, 0, 26.2, 0.8,
           0.09, 81, 0, 3.3, 68.9, 12.4, 0.6, 1.1, 2.9, 0.1, 6.7, 0.5,
           0.16, 67.2, 0, 3.1, 75.4, 9.6, 0.5, 0.5, 35.2, 0.9, 3.1, 0.1,
           0.03, 78.1, 5.1, 4.5, 57.8, 9.3, 0, 0.9, 101.8, 0, 6.2, 0.4,
           0.05, 35.8, 10.9, 4.1, 41.3, 0, 0, 0, 152.9, 0, 1.2, 0,
           0.25, 112.7, 29.4, 9.3, 173.7, 0.4, 0, 7, 296.5, 0, 202, 0.2,
           0.16, 149.9, 33.2, 8.1, 119.6, 11.4, 0, 8, 307.4, 2.3, 291.3, 0.1,
           0.23, 121.2, 18.3, 4.7, 121.8, 11.7, 0, 8.1, 500.2, 2.3, 296.7, 0.1,
           0.13, 85.5, 4.4, 0.4, 126.2, 11.9, 0, 8.4, 499.8, 2.4, 302.3, 0.1,
           0.08, 74.5, 211.5, 5, 140, 0.6, 0, 6.2, 316, 0, 24.5, 0.7,
           0.11, 99.6, 211.2, 7.3, 168, 1.3, 0, 6.7, 409.2, 0.1, 42.6, 0.7,
           0.15, 56.4, 28.1, 4.3, 248.9, 0.3, 0, 3.9, 0, 0, 23.8, 0.4,
           0.27, 141.8, 27.4, 12.8, 461.7, 0.8, 0, 5.4, 0, 10.8, 9, 0.6,
           0.33, 37.1, 13.3, 1.4, 405.1, 0.3, 0, 5.5, 0, 7.4, 2, 0.2,
           0.15, 80.6, 17.4, 7.1, 279.8, 0.6, 0, 3.4, 0, 5.5, 11.4, 0.4,
           0.31, 119.6, 0, 2.6, 213.3, 23, 0.5, 1.4, 40.6, 0, 4.8, 7.5,
           0.28, 111, 0, 1.8, 307.6, 19.6, 2, 4.3, 1252.2, 15.1, 48.6, 4.5,
           0.28, 110.5, 0, 0.1, 290.5, 24.5, 0.7, 2.3, 1252.2, 15.1, 0.9, 1.8,
           0.34, 115.1, 0, 0.7, 204.4, 27.9, 4, 4, 1250.2, 0, 12.9, 16.8,
           0.32, 112.2, 0, 0.2, 340.8, 24.8, 0.4, 1.9, 1252.2, 15.1, 4, 1.8,
           0.38, 110.8, 0, 0.1, 265.5, 21.3, 0.7, 5.6, 1252.2, 15.1, 8.2, 4.5,
           0.82, 145.1, 0, 2.3, 2.3, 25.3, 4, 6.1, 37.4, 0, 18.7, 1.6,
           0.52, 607.2, 0, 1.5, 16.5, 128.2, 0, 17.3, 0, 0, 23.1, 47.2,
           0.44, 181,  14.2, 7, 267, 19.9, 0, 10.1, 281.9, 1.6, 64.6, 0.9,
           0.59, 369.4, 56.4, 20.6, 802, 26.7, 0, 20.7, 855, 2.2, 220.6, 2.4,
           0.83, 275, 42.8, 10.2, 563.9, 32.7, 0, 13.6, 126.3, 2.6, 51.4, 2.5,
           0.31, 242.1, 44.1, 14.5, 670.3, 18, 0, 10.4, 0, 0.1, 23.5, 2.3,
           0.39, 100.8, 0, 0.1, 4.5, 20.9, 1.3, 3.4, 0, 0, 7.2, 0.3,
           0.08, 102.7, 0, 0.2, 0.8, 22.3, 0.3, 2.1, 0, 0, 7.9, 0.9,
           0.17, 98.7, 0, 0.5, 0.7, 19.8, 0.9, 3.3, 0, 0, 4.9, 1,
           0.07, 188.5, 0, 16, 155.5, 6.9, 2.1, 7.7, 0, 0, 13.1, 0.6,
           0.81,  710.8, 105.1, 72.2, 38.4, 0, 0, 13.8, 14.7, 0, 59.9, 0.4,
           0.45, 49.9, 34.1, 2.7, 121.2, 0, 0, 5.9, 53.8, 0, 91.7, 0.7,
           0.69,  115.6, 35.7, 2.1, 333.2, 0, 0, 22.7, 68, 0, 3.4, 0.5,
           0.04,  108.3, 0, 1.2, 1.1, 22.1, 4.3, 3.4, 55.6, 0, 2.8, 0.8,
           0.22,  139.2, 0, 9.2, 212.6, 15, 1.2, 2.2, 61.5, 9.6, 14.2, 0.5,
           0.12,  108, 0, 1, 486.2, 22.5, 0.9, 2.6, 0, 0, 10.2, 1.2,
           0.19,  142, 0, 7.4, 149.7, 17.8, 1.8, 2, 55.6, 0, 43.7, 0.4,
           0.39,  150.1, 12.3, 4.6, 1862.2, 18.7, 1.5, 7.9, 1308.7, 0, 27.1, 1.5,
           0.67,  184.8, 7.2, 4, 964.8, 26.8, 4.1, 11.1, 4872, 7, 33.6, 2.1,
           0.71, 158.1, 10, 3.8, 1915.1, 20.4, 4, 11.2, 3785.1, 4.8, 32.6, 2.2,
           0.75,  175.7, 10, 5, 1864.9, 21.8, 1.5, 10.9, 20.1, 4.8, 82.8, 2.8,
           0.39,  170.7, 0, 3.8, 1744.4, 33.2, 1, 4.1, 1393, 133, 27.6, 3.5,
           0.99,  163.7, 22.3, 6.6, 992, 16.6, 1.5, 9.5, 163.7, 3.5, 186, 1.5,
           0.65,  203.4, 19.8, 13.6, 1076.3, 15, 0.5, 6.1, 153.8, 2.2, 178.6, 0.6,
           0.67,  172, 2.5, 5.9, 951.3, 22.8, 8.6, 7.9, 888, 1.5, 81, 2
  ), 
  ncol = 12,
  byrow = TRUE,
  dimnames = list(
    c("Frozen Broccoli", # The rows
      "Carrots, Raw", 
      "Celery, Raw", 
      "Frozen Corn", 
      "Lettuce, Iceberg, Raw", 
      "Peppers, Sweet, Raw", 
      "Potatoes, Baked", 
      "Tofu", 
      "Roasted Chicken", 
      "Spaghetti W/ Sauce", 
      "Tomato, Red, Ripe, Raw", 
      "Apple, Raw, W/ Skin", 
      "Banana", 
      "Grapes", 
      "Kiwifruit, Raw, Fresh", 
      "Oranges", 
      "Bagels", 
      "Wheat Bread", 
      "White Bread", 
      "Oatmeal Cookies", 
      "Apple Pie", 
      "Chocolate Chip Cookies", 
      "Butter, Regular", 
      "Cheddar Cheese", 
      "3.3% Fat, Whole Milk", 
      "2% Lowfat Milk", 
      "Skim Milk", 
      "Poached Eggs", 
      "Scrambled Eggs", 
      "Bologna, Turkey", 
      "Frankfurter, Beef", 
      "Ham, Sliced, Extralean", 
      "Kielbasa, Prk", 
      "Cap'N Crunch", 
      "Cheerios", 
      "Corn Flks, Kellogg'S", 
      "Raisin Brn, Kellg'S", 
      "Rice Krispies", 
      "Special K", 
      "Oatmeal", 
      "Malt-OMeal, Choc", 
      "Pizza W/ Pepperoni", 
      "Taco", 
      "Hamburger W/ Toppings", 
      "Hotdog, Plain", 
      "Couscous", 
      "White Rice", 
      "Macaroni, Ckd", 
      "Peanut Butter", 
      "Pork", 
      "Sardines in Oil", 
      "White Tuna in Water", 
      "Popcorn, Air-Popped", 
      "Potato Chips, Bbqflvr", 
      "Pretzels", 
      "Tortilla Chip", 
      "Chicknoodl Soup", 
      "Splt Pea&Hamsoup", 
      "Vegetbeef Soup", 
      "Neweng Clamchwd", 
      "Tomato Soup", 
      "New E Clamchwd, W/ Mlk", 
      "Crm Mshrm Soup, W/ Mlk", 
      "Beanbacn Soup, W/ Watr"), 
    c("Price/Serving", # The column variables
      "Calories", 
      "Cholesterol mg", 
      "Total_Fat g", 
      "Sodium mg", 
      "Carbohydrates g", 
      "Dietary_Fiber g", 
      "Protein g", 
      "Vit_A IU", 
      "Vit_C IU",
      "Calcium mg", 
      "Iron mg")
  )  # dimnames
)  # matrix

# The min and max nutrients
minNutrients = c(2000, 0, 0, 0, 0, 25, 50, 5000, 50, 800, 10)
maxNutrients = c(2250, 300, 65, 2400, 300, 100, 100, 50000, 20000, 1600, 30)

# Returns the built tableau for minimization
createInitTableu = function(inputDiet){
  
  # Create the solution vector
  SOL = c()
  
  # Matrix with nutrients only (no price/noserving size)
  noPriceNutriFacts = nutriFacts[,-1]
  
  # ================== THIS PART IS FOR THE MAXIMIZED NUTRIENTS
  # ================== This part is multiplied by -1 since this is a maximization equation
  
  # Get the number of input food
  numOfFood = length(inputDiet)
  
  # initialize an empty matrix (11 rows for the 11 nutrients)
  maxTableu = matrix(nrow = 11, ncol = 0)
  
  #iterate and add columns of food to the tableu given the input
  for (i in 1:numOfFood){
    maxTableu = cbind(maxTableu, (-1) * noPriceNutriFacts[inputDiet[i],])
  } # for loop add columns
  
  # assign column names (column names are the food)
  colnames(maxTableu) = inputDiet
  
  # add nutrients to the last column multiplied by -1
  SOL = c(SOL, (-1) * maxNutrients)
  
  # ================== THIS PART IS FOR THE MINIMIZED NUTRIENTS
  # initialize empty matrix
  minTableu = matrix(nrow = 11, ncol = 0)
  
  # iterate through the input and cbind foods
  for (i in 1:numOfFood){
    minTableu = cbind(minTableu, noPriceNutriFacts[inputDiet[i],])
  } # for loop min
  
  # set the column names as the input diet
  colnames(minTableu) = inputDiet

  #cbind the min variables
  SOL = cbind(SOL,minNutrients)

  # combine the max and min matrices
  initialTableu = rbind(maxTableu, minTableu)

  # ======================= THIS PART IS FOR THE FOOD SERVINGS 
  # ======================= This part is multiplied by -1 since this is a maximization equation
  
  # initialize for the max food servings
  maxFoodServingTableu = matrix(0, nrow = numOfFood, ncol = numOfFood)
  # put row names on the matrix
  rownames(maxFoodServingTableu) = paste0("x", 1:numOfFood)
  # put 1 on each corresponding food
  for (i in 1:nrow(maxFoodServingTableu)){
    maxFoodServingTableu[i,i] = -1
  }

  # cbind the max food servings to the end of the matrix
  SOL = c(SOL, rep(-10,numOfFood))
  
  # append the maxFoodServingTableu to the existing tableu
  initialTableu = rbind(initialTableu, maxFoodServingTableu)

  # ==================== THIS IS FOR THE COSTS OF THE FOODS
  # create a vector that holds the cost of each food
  costsForEachFood = c()
  # iterate and add to the vector
  for (i in 1:numOfFood){ 
    costsForEachFood = cbind(costsForEachFood, nutriFacts[inputDiet[i],1])
  }
  
  # cbind the SOL values
  SOL = c(SOL, 0)
  
  # rbind the costs vector to the initial tableu
  initialTableu = rbind(initialTableu, costsForEachFood)
  
  # cbind the RHS to the initial tableau
  initialTableu = cbind(initialTableu, SOL)
  
  # =================== MINIMIZATION PROCESSING
  # transpose the initial tableue
  initialTableu = t(initialTableu)

  # Get column count
  cols = ncol(initialTableu)
  
  # Temporarily store the RHS to attach the slacks
  RHS = initialTableu[, cols]
  
  # Negate the last row of the initial tableau to turn to maximization problem
  initialTableu[nrow(initialTableu),] = (-1) * initialTableu[nrow(initialTableu),]

  # Create slacks based from the number of rows using diagonal matrix
  slacks = diag(nrow = nrow(initialTableu), ncol = length(inputDiet)+1)
  colnames(slacks)[ncol(slacks)] = "Z"
  
  # Set the names of the slacks as the input diet
  colnames(slacks)[1:(ncol(slacks)-1)] = inputDiet
  
  # cbind the slacks to the initial tableu
  initialTableu = cbind(initialTableu[,1:(cols-1)], slacks)
  
  # cbind the stored RHS
  initialTableu = cbind(initialTableu, RHS)

  return(initialTableu)
} # create initial tableau

# The simplex method implementation
simplexMethod = function(inputDiet){
  # Gets the length of the input dier
  numOfFoods = length(inputDiet)
  
  # Creates the initial tableau given the input
  tableuToSolve = createInitTableu(inputDiet)
  
  # Get the rows and columns of the tableau
  rows = nrow(tableuToSolve)
  cols = ncol(tableuToSolve)
  
  # Set the number of iterations as 0
  numIterations = 0
  
  # List to store the iterations
  mat_iter = list()
  
  # Continue iterating while there is a negative in the last row excluding the solution column
  while(any(tableuToSolve[rows,1:(cols-1)] < 0)){
    
    # Select the pivot column (smallest negative in the last row)
    pivotColumn = which.min(tableuToSolve[rows, 1:(cols-1)])

    # Compute for test ratios of the pivot column
    testRatio = c()
    testRatio = tableuToSolve[,cols] / tableuToSolve[,pivotColumn]
    
    # Set all test rations less than 0 to inf
    testRatio[testRatio <= 0] = Inf

    # Get the pivot row (min positive test ratio)
    pivotRow = which.min(testRatio)
    
    # Get the pivot element 
    pivotElement = tableuToSolve[pivotRow, pivotColumn]
    
    # Returns null if the pivot element is 0, or the number of iterations exceeds 500
    if (pivotElement == 0 || numIterations == 500){
      return (NULL)
    }
    
    # Get the normalized row by dividing the pivot row with the pivot element
    normalizedRow = tableuToSolve[pivotRow,] / pivotElement
    
    # Row elimination for all rows
    for(i in 1:rows){
      # If i == pivot row, set as the normalizedRow
      if(i == pivotRow){
        tableuToSolve[i,] = normalizedRow
      # Else, newRow = row - (normalizedRow * the element in the pivot column of that row)
      } else {
        tableuToSolve[i,] = tableuToSolve[i,] - (normalizedRow * tableuToSolve[i,pivotColumn])
      }
    }
    # Increment the number of iterations
    numIterations = numIterations + 1
    
    # Store the current matrix and the basic solution in the list
    mat_iter[[numIterations]] = getBasicSolution(tableuToSolve)
  } # while simplex is valid
  
  # Get the final tableau and solution
  solution = getResult(tableuToSolve, inputDiet)
  
  # Return the results of the simplex method
  return (list("FinalSol" = solution, "Iterations" = mat_iter))

} # simplex method

# Gets the basic solution per iteration
getBasicSolution = function(tableu){
  
  # Get row and columns of the tableu
  rows = nrow(tableu)
  cols = ncol(tableu)

  # Intiialize empty matrix with 1 row given the number of columns
  basicSolutionMat = matrix(nrow = 1, ncol = cols)
  # Set the column names equal to the column names of the tableau
  colnames(basicSolutionMat) = colnames(tableu)
  
  # Iterate through every column
  for (i in 1:cols) {
    # Checks if the number of elements that are 1 in the column is only 1
    if (sum(tableu[, i] == 1) == 1) {
      # Finds the row index where 1 occured in the column 
      rowWithOne = which(tableu[,i] == 1)
      # Get the equivalent value in the RHS 
      basicSolutionMat[,i] = tableu[rowWithOne,cols]
    # 0 if any other number is encountered
    } else {
      basicSolutionMat[,i] = 0
    }
  }
  # Store in a labelled list with the tableau and basic solution
  tableuAndBasicSol = list("tableu" = tableu, "BasicSolution" = basicSolutionMat)
  # Return for the specific iteration
  return(tableuAndBasicSol)
}

# Gets the final result of the simplex method
getResult = function(tableu, inputDiet){
  # Number of input food
  numOfFoods = length(inputDiet)
  
  # Gets the rows and columns of the matrix
  rows = nrow(tableu)
  cols = ncol(tableu)
  
  # Creates vector for: 
  # 1.) Foods that have a value
  # 2.) The servings of that food
  foodWithValue = c()
  servings = c()
  
  # Extracts the total price of the foods from the RHS (rounded to 2 decimal places)
  totalPrice = round(tableu[rows,cols],2)
  
  # Initialize matrix with 1 row and equivalent columns
  solutionColsMat = matrix(nrow = 1, ncol = cols)
  # Assign the last row to the solutionColsMat
  solutionColsMat[1,1:cols] = tableu[rows, 1:cols]
  # Copy the column names
  colnames(solutionColsMat) = colnames(tableu)
  
  # Gets the columns from the food to the end
  solutionCols = tableu[rows,((cols - numOfFoods) - 1):cols]
  
  # Iterate through the input food
  for (i in 1: numOfFoods){
    # If the solution column is not equal to zero, there is a solution
    if (solutionCols[i] != 0){
      # Put the food with value in the vector
      foodWithValue[i] = inputDiet[i]
      # Get the equivalent servings of that food and put in the servings vector (rounded to 2 decimal places)
      servings[i] = round(solutionCols[i],2) 
    }
  }
  
  # Initialize costs of the food
  costsOfFoods = c()
  
  # For each food, set the cost of the food as the cost per serving multiplied by the servings
  for (i in 1:numOfFoods){
    costsOfFoods[i] = round(nutriFacts[inputDiet[i],1] * servings[i],3)
  }
  
  # Check which rows are non zerio in the servings vector (returns row indices)
  nonZeroRows = which(servings != 0)
  
  # Create a data frame with the food which are non zero, the servings of that food, and the total cost of that food from the three vectors
  basicSolutionDf = data.frame(Food = foodWithValue[nonZeroRows], Servings = servings[nonZeroRows], Cost = costsOfFoods[nonZeroRows])
  
  # Create a labelled list with the final tableau, final basic solution, breakdown of the cost, and optimal cost
  result = list("FinalTableu" = tableu, "FinalBasicSol" = solutionColsMat, "Breakdown" = basicSolutionDf, "OptimalCost" = totalPrice[1])
  # Returns the result
  return (result)
}

