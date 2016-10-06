test_that("Wrong data inputs",{
  #Jag är lite tveksam kring dessa, tror vi kmr få samma problem som senast, att den producerar
  #både warnings och error. Leffe sa ngt om det senast, typ ngt på s, subset, eller ngt.
  #Kan fråga honom imorrn.
  expect_warning(knapsack_dynamic(x=data.frame(f=1:3, v=2:4),W = 2000))
  expect_warning(knapsack_dynamic(x=data.frame(w=1:3, v=2:4),W = -1))
  expect_warning(knapsack_dynamic(x=data.frame(w=1:3, v=2:4),W = c(1, 200))
  expect_warning(knapsack_dynamic(x=data.frame(w=1:7, v=2:-4),W = 2111)) 
  expect_warning(knapsack_dynamic(x=data.frame(w=1:3, v=2:4),W = "Tre"))
  expect_warning(knapsack_dynamic(x=matrix(1:4, ncol=2),W = 2000))
  
})

test_that("Results",{ 
  
  expect_equal(knapsack_dynamic(x=data.frame(w=c(1,3,4,5), v=c(1,4,5,7)), W=7)[1], 9)
  expect_equal(knapsack_dynamic(x=data.frame(w=c(1,3,4,5), v=c(1,4,5,7)), W=7)$element[2], 3)
  expect_equal(knapsack_dynamic(x=data.frame(w=c(1,3,4,5), v=c(1,4,5,7)), W=12)$element[1], 16)
  #Gör några fler för kanske andra matriser, typ de i instruktionerna.
  #Kan försöka göra några bara expect error också.
  
})