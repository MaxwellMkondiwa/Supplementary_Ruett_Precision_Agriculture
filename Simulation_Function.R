# Simulation function for simulation of General, Improved, and Sensor monitoring ####

Simulation <- function(){
  
  # Cost_Benefit function for monitoring strategies ####
  
  Cost_Benefit <- function(n_years, # number of years to run the simulation
                           highrisk_year, # Years with a high risk for occurrence 
                           # of fungal infections
                           CV, # coefficient indicating variation into a time series
                           area, # area of the heather production system
                           sample_costs, # costs per lab sample
                           value_of_discarded_plant, # value of a sampled and then 
                           # discarded heather plant
                           plant_value_of_A1_quality, # plant value of marketable heather
                           # plant that would not have achieved high quality without 
                           # the respective monitoring strategy.
                           Initial_investment, # Mandatory investment in the first year
                           # to start then respective monitoring approach
                           additional_investment, # Occurring investments is the following
                           # years 
                           labor_costs, # Labor costs to conduct monitoring
                           post_processing_costs, # Data processing of acquired data
                           sample_number, # Number of lab samples
                           additional_saved_plants, # number of heather plants that would
                           # not have achieved high marketable quality without the
                           # respective monitoring strategy.
                           adjustment_sample_size, # Adjustment to increase sample size 
                           # in high risk years 
                           increased_resource_use_costs, # Costs for increased resource
                           # use to protect heather plants in high risk years
                           resource_savings # Monetary savings for reduced resource
                           # use in normal risk years due to the respective monitoring
                           # strategy
  ){
    
    # labor costs per area
    yearly_labor_costs_per_ha <- vv(labor_costs, CV, n = n_years)
    
    # number of samples per ha
    samples_per_ha <- round(vv(sample_number, CV, n = n_years) *
                              (1+highrisk_year * (vv(adjustment_sample_size, CV, n = n_years))), digits = 0)
    
    # sample cost per ha
    sample_costs_per_ha <- samples_per_ha * sample_costs
    
    # value of discarded plants per ha
    discarded_plant_value_per_ha <- samples_per_ha *
      vv(value_of_discarded_plant, CV, n = n_years)
    
    # total labor and sample costs
    labor_and_sample_cost <- (yearly_labor_costs_per_ha +
                                sample_costs_per_ha +
                                discarded_plant_value_per_ha) * area
    
    # add investment, maintenance and post-processing costs
    total_cost <- labor_and_sample_cost +
      vv(post_processing_costs, CV, n = n_years) + 
      c(Initial_investment, rep(0,n_years-1)) +
      c(0,vv(additional_investment, CV, n = n_years-1))
    
    # Benefits of monitoring strategies consist of resource savings that are achieved
    # in normal risk years and the value of saved plants in highrisk years. 
    
    # In normalrisk years the number of frequent fungicide applications can be reduced
    # to some extent, because more monitoring increases knowledge about plant health 
    # status in the field. Therefore, resource savings can be achieved in these years.
    
    # Although monitoring allows for resource savings in normal risk years, no 
    # resource savings are achieved in high-risk years because a higher number of 
    # pesticide applications is required. However, thanks to more precise knowledge 
    # about spatially distributed plant vitality, producers are able to protect quality
    # of more plants, which are safely cultivated until they are sold.
    resource_savings <- vv(resource_savings, CV, n = n_years) * area * (1-highrisk_year)
    
    value_of_high_quality_plants <- vv(additional_saved_plants, CV, n = n_years) * 
      plant_value_of_A1_quality * area * highrisk_year
    
    total_benefits <- resource_savings + value_of_high_quality_plants
    
    # cashflow calculation in the return command
    return(cashflow = total_benefits - total_cost)}
  
  # High risk years are years where weather conditions lead to higher risks
  # of plant losses. High risk years can increase sample size and resource use.
  highrisk_year <- chance_event(chance_high_risk, n = n_years)
  
  # Here we simulate the cost benefit function for standard monitoring (General)
  cashflow_G <- Cost_Benefit(n_years = n_years,
                             highrisk_year = highrisk_year,
                             CV = var_CV,
                             area = production_area,
                             sample_costs = lab_costs_per_sample,
                             value_of_discarded_plant = plant_value_of_discarded_plant,
                             plant_value_of_A1_quality = plant_value_of_A1_quality,
                             Initial_investment = Initial_investment_G , 
                             additional_investment = additional_investment_G ,
                             labor_costs = labor_costs_G,
                             post_processing_costs = post_processing_costs_G,
                             sample_number = sample_number_G,
                             additional_saved_plants = Number_of_saved_high_quality_plants_G,
                             adjustment_sample_size = adjustment_sample_size_G,
                             resource_savings = resource_savings_G)
  
  # Here we calculate the Net Present Value for 'General' monitoring
  NPV_G <- discount(cashflow_G, discount_rate, calculate_NPV = TRUE)
  
  # Here we simulate the cost benefit function for more intense monitoring (Improved)
  cashflow_I <- Cost_Benefit(n_years = n_years,
                             highrisk_year = highrisk_year,
                             CV = var_CV,
                             area = production_area,
                             sample_costs = lab_costs_per_sample,
                             value_of_discarded_plant = plant_value_of_discarded_plant,
                             plant_value_of_A1_quality = plant_value_of_A1_quality,
                             Initial_investment = Initial_investment_I , 
                             additional_investment = additional_investment_I ,
                             labor_costs = labor_costs_I,
                             post_processing_costs = post_processing_costs_I,
                             sample_number = sample_number_I,
                             additional_saved_plants = Number_of_saved_high_quality_plants_I,
                             adjustment_sample_size = adjustment_sample_size_I,
                             resource_savings = resource_savings_I)
  
  # Here we calculate the Net Present Value for 'Improved' monitoring
  NPV_I <- discount(cashflow_I, discount_rate, calculate_NPV = TRUE)
  
  # Here we calculate the Net Present Value for the decision to apply 'Improved' instead
  # of 'General' monitoring.
  comp_NPV_IG <- NPV_I - NPV_G
  
  # Here we simulate the cost benefit function for monitoring with sensor technology (Sensor)
  cashflow_S  <- Cost_Benefit(n_years = n_years,
                              highrisk_year = highrisk_year,
                              CV = var_CV,
                              area = production_area,
                              sample_costs = lab_costs_per_sample,
                              value_of_discarded_plant = plant_value_of_discarded_plant,
                              plant_value_of_A1_quality = plant_value_of_A1_quality,
                              Initial_investment = Initial_investment_S , 
                              additional_investment = additional_investment_S ,
                              labor_costs = labor_costs_S,
                              post_processing_costs = post_processing_costs_S,
                              sample_number = sample_number_S,
                              additional_saved_plants = Number_of_saved_high_quality_plants_S,
                              adjustment_sample_size = adjustment_sample_size_S,
                              resource_savings = resource_savings_S)
  
  # Here we calculate the Net Present Value for 'Sensor' monitoring
  NPV_S <- discount(cashflow_S, discount_rate, calculate_NPV = TRUE)
  
  # Here we calculate the Net Present Value for the decision to apply 'Sensor' instead
  # of 'General' monitoring.
  comp_NPV_SG <- NPV_S - NPV_G
  
  return(list(cashflow_G = cashflow_G,
              NPV_G = NPV_G,
              cashflow_I = cashflow_I,
              NPV_I = NPV_I, 
              cashflow_S = cashflow_S,
              NPV_S = NPV_S,
              comp_NPV_IG = comp_NPV_IG,
              comp_NPV_SG = comp_NPV_SG))
}
