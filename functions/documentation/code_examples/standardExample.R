boxConstraints = data.frame("lower" = c(150, 0.8, 1, 0.8, 1),
                            "upper" = c(800, 3, 50, 3, 50))

startAt = c(400, 1, 30, 1.3, 15)

fittedModel = standardModel(inputData = mockData,
                            constraints = boxConstraints,
                            startingValues = startAt,
                            doTrace = TRUE)
