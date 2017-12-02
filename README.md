# startup_analysis
A data analytics project on startup funding.

# CLEANING 

      1) clean.R (run on startup_funding.csv)
            a) date column : format of date
                            add dd, mm and yyyy columns
            b) location : make multiple locations as different rows
                          format location names
            c) amt in usd : Make all strings numbers (remove commas)
                            Remove na rows
            d) funding type : format funding type names
            e) categorize industry vertical : new column for broader classification
            f) founder education : make multiple universities as different rows
            g) founder designation : new column for broader classification (imputed data)

# VISUALIZATION
                  
      1) basic_vis.R : Year wise startup count (bar chart)
                       Month wise startup count (bar chart)
                       Major Industry verticals split up (pie chart)
                       Average and maximum funding in the three years (line chart)

      2) interactive_plot.R : Major industries and amount invested by top investors (interactive bar plot)

      3) bubble_map.R : Map with showing startup count in cities (bubble map)         

      4)founder_education.R : Number of startups produced by all universities (bar chart)
                              Number of startups produced by IITs (bar chart)
                              Number of founders and engineers produced by major universities (bar chart)
      5)Investorwordcloud :word cloud of investor names						
      6)SkillsWordCloud :word cloud of founder skills and market specialization

# PREDICTIVE MODELS

      1) clusterFunding.R : has all predictive models and their graphs
                        
                        
