# creating empty variables to store the results
all.res.2 <- data.frame(
  # two cells need to be changed
  n.sim = c(1:100),
  to.change.1 = NA,
  to.change.2 = NA,
  
  #  warnings
  warnings = NA,
  
  # all.full.coeffs
  all.full.coeffs.intercept = NA,
  all.full.coeffs.Condition = NA,
  all.full.coeffs.Trial = NA,
  all.full.coeffs.Condition.Trial = NA,
  
  all.full.sd.intercept = NA,
  all.full.sd.Condition = NA,
  all.full.sd.Trial = NA,
  all.full.sd.Condition.Trial = NA,
  
  # test.full.null
  test.full.null.Chisq = NA,
  test.full.null.Df = NA,
  test.full.null.Pr..Chisq. = NA,
  
  # model comparisons
  # Condition*Trial
  test.2.way.Condition.Trial.Chisq = NA,
  test.2.way.Condition.Trial.Chi.Df = NA,
  test.2.way.Condition.Trial.Pr..Chisq. = NA,
  test.2.way.Condition.Trial.n.opt.warnings = NA,
  test.2.way.Condition.Trial.n.fun.warnings = NA,
  
  # assumptions
  collinearity.test.Condition = NA,
  collinearity.test.Trial = NA,
  
  # model comparisons
  # main effects
  test.main.Condition.Chisq = NA,
  test.main.Condition.Chi.Df = NA,
  test.main.Condition.Pr..Chisq. = NA,
  test.main.Condition.n.opt.warnings = NA,
  test.main.Condition.n.fun.warnings = NA,
  test.main.Trial.Chisq = NA,
  test.main.Trial.Chi.Df = NA,
  test.main.Trial.Pr..Chisq. = NA,
  test.main.Trial.n.opt.warnings = NA,
  test.main.Trial.n.fun.warnings = NA
  
)

boot.full.values <- c()
boot.Cond.values <- c()
boot.full.estimates <- c()
boot.Cond.estimates <- c()


all.res.3 <- data.frame(
  # three cells need to be changed
  n.sim = c(1:100),
  to.change.1 = NA,
  to.change.2 = NA,
  to.change.3 = NA,
  #  warnings
  warnings = NA,

  # all.full.coeffs
  all.full.coeffs.intercept = NA,
  all.full.coeffs.Condition = NA,
  all.full.coeffs.Trial = NA,
  all.full.coeffs.Condition.Trial = NA,

  all.full.sd.intercept = NA,
  all.full.sd.Condition = NA,
  all.full.sd.Trial = NA,
  all.full.sd.Condition.Trial = NA,

  # test.full.null
  test.full.null.Chisq = NA,
  test.full.null.Df = NA,
  test.full.null.Pr..Chisq. = NA,

  # model comparisons
  # Condition*Trial
  test.2.way.Condition.Trial.Chisq = NA,
  test.2.way.Condition.Trial.Chi.Df = NA,
  test.2.way.Condition.Trial.Pr..Chisq. = NA,
  test.2.way.Condition.Trial.n.opt.warnings = NA,
  test.2.way.Condition.Trial.n.fun.warnings = NA,

  # assumptions
  collinearity.test.Condition = NA,
  collinearity.test.Trial = NA,

  # model comparisons
  # main effects
  test.main.Condition.Chisq = NA,
  test.main.Condition.Chi.Df = NA,
  test.main.Condition.Pr..Chisq. = NA,
  test.main.Condition.n.opt.warnings = NA,
  test.main.Condition.n.fun.warnings = NA,
  test.main.Trial.Chisq = NA,
  test.main.Trial.Chi.Df = NA,
  test.main.Trial.Pr..Chisq. = NA,
  test.main.Trial.n.opt.warnings = NA,
  test.main.Trial.n.fun.warnings = NA

)

all.res.4 <- data.frame(
  # 4 cells need to be changed
  n.sim = c(1:100),
  to.change.1 = NA,
  to.change.2 = NA,
  to.change.3 = NA,
  to.change.4 = NA,
  
  #  warnings
  warnings = NA,
  
  # all.full.coeffs
  all.full.coeffs.intercept = NA,
  all.full.coeffs.Condition = NA,
  all.full.coeffs.Trial = NA,
  all.full.coeffs.Condition.Trial = NA,
  
  all.full.sd.intercept = NA,
  all.full.sd.Condition = NA,
  all.full.sd.Trial = NA,
  all.full.sd.Condition.Trial = NA,
  
  # test.full.null
  test.full.null.Chisq = NA,
  test.full.null.Df = NA,
  test.full.null.Pr..Chisq. = NA,
  
  # model comparisons
  # Condition*Trial
  test.2.way.Condition.Trial.Chisq = NA,
  test.2.way.Condition.Trial.Chi.Df = NA,
  test.2.way.Condition.Trial.Pr..Chisq. = NA,
  test.2.way.Condition.Trial.n.opt.warnings = NA,
  test.2.way.Condition.Trial.n.fun.warnings = NA,
  
  # assumptions
  collinearity.test.Condition = NA,
  collinearity.test.Trial = NA,
  
  # model comparisons
  # main effects
  test.main.Condition.Chisq = NA,
  test.main.Condition.Chi.Df = NA,
  test.main.Condition.Pr..Chisq. = NA,
  test.main.Condition.n.opt.warnings = NA,
  test.main.Condition.n.fun.warnings = NA,
  test.main.Trial.Chisq = NA,
  test.main.Trial.Chi.Df = NA,
  test.main.Trial.Pr..Chisq. = NA,
  test.main.Trial.n.opt.warnings = NA,
  test.main.Trial.n.fun.warnings = NA
  
)




