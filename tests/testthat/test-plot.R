test_that("multiplication works", {
  

    nightowl::plot(ChickWeight,
      mapping = list(
       x = "Time",
       y = "weight",
       fill = "Diet"
     ),
     boxplot = list(dodge = 2)
     
    )

})
