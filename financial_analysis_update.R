
calc_NPV <- function(annual_revenue, i, lifetime_yrs, CAPEX, OPEX) {
  NPV <- -CAPEX  # Initial investment
  for (year in 1:lifetime_yrs) {
    NPV <- NPV + annual_revenue / ((1 + i) ^ year)  # Discounted cash flows
  }
  return(NPV)
}

# Given values
annual_revenue <- 7574.42905
i <- 0.05  # Discount rate
lifetime_yrs <- 25
CAPEX <- 56056.99209
OPEX <- 0  # Not used in this calculation

# Calculating NPV
NPV <- calc_NPV(annual_revenue, i, lifetime_yrs, CAPEX, OPEX)
print(NPV)

calc_NPV <- function(annual_revenue=7574.42905, i=0.05, lifetime_yrs, CAPEX=56056.99209, OPEX=0){
  NPV <- -CAPEX  # 初始年的NPV
  
  for (t in 1:lifetime_yrs) {
    NPV <- NPV + (annual_revenue - OPEX) / (1 + i)^t
    if (NPV >= 0) {
      return(t)  # 返回回报年份
    }
  }
  
  return(NA)  # 如果没有回报，返回NA
}

# 假设项目寿命为25年
lifetime_years <- 25

# 计算回报年份
payback_year <- calc_NPV(annual_revenue = 7574.42905, i = 0.05, lifetime_yrs = lifetime_years, CAPEX = 56056.99209, OPEX = 0)

if (!is.na(payback_year)) {
  cat("项目从第", payback_year, "年开始回报。")
} else {
  cat("项目在给定的时间范围内没有回报。")
}



#3 Levelized cost of electricity (LCOE)=====
#Life_span_generation_kWH is one of the required inputs to estimate the Levelized
#cost of electricity (following function)


Life_span_generation_kWH <- function (yearly_generation_kWH=73538146110, discount = 0.03, lifetime_yrs = 25) {
  t <- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G, 0))
}

# NPV of cost
LCOE <- function(NPV_cost=56056992090, Life_span_generation_KWH) {
  lcoe <- NPV_cost / Life_span_generation_KWH
  return(round(lcoe, 2))
}

# 使用函数
life_span_gen <- Life_span_generation_kWH()
lcoe_result <- LCOE(Life_span_generation_KWH = life_span_gen)

# 输出结果
print(life_span_gen)
print(lcoe_result)



