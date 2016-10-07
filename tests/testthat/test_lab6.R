
test_that("Wrong data inputs",{
  expect_warning(try(knapsack_dynamic(x=data.frame(f=1:3, v=2:4),W = 2000),silent = T))
  expect_warning(try(knapsack_dynamic(x=data.frame(w=1:3, v=2:4),W = -1),silent = T))
  expect_warning(try(knapsack_dynamic(x=data.frame(w=1:3, v=2:4),W = c(1, 200)), silent =T)) 
                 expect_warning(try(knapsack_dynamic(x=data.frame(w=1:7, v=2:-4),W = 2111),silent = T))  
                 expect_warning(try(knapsack_dynamic(x=data.frame(w=1:3, v=2:4),W = "Tre"),silent = T))  
                 expect_warning(try(knapsack_dynamic(x=matrix(1:4, ncol=2),W = 2000),silent = T))
   

                 expect_warning(try(knapsack_greedy(x=data.frame(f=1:3, v=2:4),W = 2000),silent = T))
                 expect_warning(try(knapsack_brute_force(x=data.frame(w=1:3, v=2:4),W = -1),silent = T))
                 expect_warning(try(knapsack_greedy(x=data.frame(w=1:3, v=2:4),W = c(1, 200)), silent =T)) 
                 expect_warning(try(knapsack_brute_force(x=data.frame(w=1:7, v=2:-4),W = 2111),silent = T))  
                 expect_warning(try(knapsack_greedy(x=data.frame(w=1:3, v=2:4),W = "Tre"),silent = T))  
                 expect_warning(try(knapsack_brute_force(x=matrix(1:4, ncol=2),W = 2000),silent = T))             
                 
                               
})


  
  
test_that("Results",{ 
  
  expect_equal(knapsack_dynamic(x=data.frame(w=c(1,3,4,5), v=c(1,4,5,7)), W=7)$value, 9)
  expect_equal(knapsack_dynamic(x=data.frame(w=c(1,3,4,5), v=c(1,4,5,7)), W=7)$element[2], 3)
  expect_equal(knapsack_dynamic(x=data.frame(w=c(1,3,4,5), v=c(1,4,5,7)), W=12)$value, 16)

  
})


