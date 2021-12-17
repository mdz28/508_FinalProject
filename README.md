# Siting Hope - Predicting Opioid Overdoses in Mesa, AZ 
 
## What are the use cases for your app and what should the app do?
Mesa’s current use case for its current dashboard for all of Maricopa County called MaricopaRx, is geared towards connecting medical providers, educators, and other stakeholders engaged in the fight to end the opioid epidemic in Maricopa County. However, the dashboard excludes a direct use for opioid users themselves. Thus, to fill this gap, Siting Hope’s primary use case will be for opioid users to locate nearest OPS site in addition to the following:
- Educating bystanders or witnesses of what to do in a situation when someone is experiencing an overdose.
- Customizable hotline call feature that connects users to social services allowing users to input racial, gender, or language preferences to eliminate concern of police and ensure users receive the most comforting and helpful service.
- Maps showing the nearest places to go purchase Narcan/ naloxone including specific pharmacies not requiring health insurance
- Dashboard version for public health officials of Maricopa County to monitor the status of each OPS including construction costs, # of social services, and # of prevented overdoses. The dashboard can also be used to help public officials determine where to invest in curating environments and providing social service resources for users including establishing overdose-specific EMS services in hotspot neighborhoods.

## How could data make a difference in answering this question? Do you have a sense for the business-as-usual decision making? 
The City of Mesa does not currently have major local programs aimed at preventing overdose events. The state of Arizona and Maricopa County house the main overdose prevention programs, with the most specific to Mesa being MaricopaRx, “an online tool to connect treatment providers, educators, community-based organizations, law enforcement, and other stakeholders engaged in the fight to end the opioid epidemic in Maricopa County and beyond. With the information provided by partners and other data, we hope to identify collaborative opportunities to reduce duplication of efforts and accelerate our progress, as well as to address gaps in services, especially among high risk populations.”  In other words, Arizona and Maricopa County are both doing work to ensure high-risk populations have access to the resources that they need.  

As far as we can tell from online research, although MaricopaRx uses overdose and resource data, it is largely to identify gaps in resource allocation or potential collaboration opportunities, and not necessarily to predict what factors may contribute to the likelihood of an overdose event. But what new information might a predictive model be able to give to public health officials outside of gaps in service?  

With the news that NYC has become the first city approved for a supervised drug use site, other cities may begin to explore this as a treatment and overdose prevention effort.  

Our goal with exploring environmental risk factors is not explicitly to remediate those risk factors, because that could just put a Band-Aid on the environment while pushing the issue further underground. Instead, we want this tool to help public health officials understand the environmental factors that are most conducive to drug use, and therefore may put them in closest contact with people who may be using. From there, the tool can help with scenario planning and advocating for supervised use sites through cost-benefit analyses.  

## What datasets have you identified to help you answer this question? 
- CDC Health Data
- Pedestrian street light fixtures
- Code violations
- Drug crimes  

 ## What kind of model would you build and what is the dependent variable? 
We will be building a Poisson regression with 2017 overdoses as the dependent variable 

## How will you validate this model (cross-validation & goodness of fit metrics that relate to the business process)? 
- We will cross-validate using k-fold and LOGO-CV and looking at the mean absolute error distribution 
- We will validate the goodness of fit by comparing our risk prediction map to a kernel density map of the following year’s data 
- We can more closely examine goodness-of-fit by creating a bar chart that shows how well our model predicts risk compared to kernel density at different risk levels so that we can ensure we’re predicting well especially for the highest risk populations  
