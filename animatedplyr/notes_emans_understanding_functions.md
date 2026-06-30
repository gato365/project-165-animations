
# Functionality for animating dplyr verbs
## with_animation -->



## 1. animate_filter -->

### A) Create the configuration, capture the condition, and get the data name
### B) Evaluate the condition and handle missing values
### C) Create the title and detect edge cases
### D) Identify columns used in the condition
### E) Handle edge cases with a callout message
### F) Sample rows and columns to show in the animation
### G) Create the "before" and "after" data frames for the animation
### H) Create the payload for the animation
### I) Render the animation

## 2. animate_select -->
### A) Create the configuration and determine the data name
### B) Create the title and other strings for the animation
### C) Handle edge cases with a callout message
### D)Sample rows and columns to show in the animation 
### E) Create the "before" and "after" data frames for the animation
### F) Create the payload for the animation
### G) Render the animation

## 3. animate_mutate -->

### A) Create the configuration and determine the data name
### B) Capture the expression and check for errors 
### C) Create the data frame and determine the new column
### D) Identify source columns used in the expression 
### E) Sample rows and columns to show in the animation
### F) Create the "before" and "after" data frames for the animation
### G) Create the payload for the animation
### H) Render the animation



# Understanding Tests

R files Approach from AI 
├── test-01-core-behavior.R
├── test-02-edge-cases.R
├── test-03-sampling-reproducibility.R
├── test-04-payload-html.R
├── test-05-print-render.R
└── test-06-save-gif.R


Q1. Why did they choose to separate the tests into layers?
Q2. What could I be missing?
Q3. How do I preform individual tests on each layer?
Q4. I have a issue come up, how do I know which layer the issue is coming from? How can I isolate the issue to a specific layer?
Q5. I intentionally  want to create intention tests that AI did not create across each layer according to the 