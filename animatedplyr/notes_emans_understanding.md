
## animatedplyr

First Layer Explanation

```
├── data              ## [dir] where the data lives 
├── DESCRIPTION       ## [file] descrip info for package
├── inst              ## [dir] examples, templates and shiny
├── LICENSE           ## [file] 
├── NAMESPAC          ## [file] 
├── R                 ## [dir] contains all R package related files 
├── README.md         ## [file] 
├── smoke_test.R      ## [file] 
├── tests             ## [dir] contains tests
└── vignettes         ## [dir] has ways of using package
```



### Notes:
1. In the actual package is shiny will not be in production
2. [Q1] what is a license and what should go into it?
3. [Q2] what is the description and what should go into it? Especially if it's supposed to be well defined and specified.
4. [Q3] what is Name space and why is it needed for a package?
5. [Q4] should I have all the gifts that are made in animation be explored into a Directory outside of the library/package? Yes however, it needs to have a good place to be rather than right outside the package. I think it would be nice to have some formal way of doing this.
6. [task_1] I need to read me to be bomb Diggity and that it is very very well specified and any user could read what the package is doing and how it works and how to implement it for a learning purposes or how to implement it for processing and understanding data.
7. [Q5] what is a smoke_test.R file and why is it important? What is it protecting me from?
8. [task_2] I want to have our implement what ChatGPT said for test testing and then validated with Logan and Rob's perspective in terms of making sure that every case has been explored.
8. [task_3] I want the students to create their own test themselves were. I'll create test on my end using Rob's and Logan's perspective as well as ChatGPT's outline and at the end of the day after our conversation, we will have all the Test under mind.
9. [Q6] what's the general logic of the R files like what's being called?
My attempt of thinking through the logic. If you have a better way of visualizing/mapping the traceback. This is clunky but I would love to see a map that would allow me parse the generated code.
``` 
animate_filter (animate_verbs.R) --> 
  .detect_edge_cases (sampling.R), .df_to_payload (utils.R)
```
Make tasks more clearer stated but answer the quesrtions in 1-2 short sentences except for question 6







Understanding R Dir more

```{markdown}
PUBLIC ENTRY POINTS                INTERNAL PIPELINE (the "assembly line")
─────────────────────              ───────────────────────────────────────

with_animation()  ───routes to───┐
 (with_animation.R)              │
                                 ▼
animate_filter()  ┐
animate_select()  ├─(animate_verbs.R)── each verb runs these 6 stages ──┐
animate_mutate()  ┘                                                     │
                                                                        ▼
   1. CONFIG     .merge_config()                          (config.R)
                    └─ animate_config() ← public, builds the config list
                                                                        
   2. VALIDATE   is.data.frame / is.logical / is.na checks (inline)
                                                                        
   3. SAMPLE     .with_seed( ... )                         (utils.R)  ← reproducibility wrapper
                    └─ .sample_for_filter()                (sampling.R)
                       .sample_for_select()                   │
                       .sample_for_mutate()                   └─ .sample_cols()  (utils.R)
                                                                        
   4. EDGE CASES .detect_edge_cases()                      (sampling.R) ← "All rows kept" etc.
                                                                        
   5. PAYLOAD    .df_to_payload(before) + .df_to_payload(after)  (utils.R)
                    └─ .fmt_cell()                          (utils.R)
                 .default_colors()  +  .disclosure()        (utils.R)
                                                                        
   6. RENDER     .html_template(payload, config)            (render.R) ← injects the JS animation
                    └─ .as_animate_html()                   (render.R) ← attaches class + payload attr
                                                                        ▼
                                                          object of class "animate_html"
                                                                        │
              print.animate_html()  (with_animation.R) ──renders──► htmltools::html_print → Viewer

SIDE BRANCHES (reuse the pipeline output):
  animate_save_gif()  (save_gif.R) ── calls .html_template() again, then webshot2 + magick → .gif
  animate_studio()    (studio.R)   ── launches the Suggests-only Shiny app in inst/shiny/studio/
```



1. [Q1] what's the utility behind payload?
2. [Q2]




lets build the logic around this point:
"Separate tests into layers: 

core behavior:
- does the function work under situations when expected to work
- does it break when it supposed to break (empty df or conditions do not match) 

HTML structure:
- what break this html structure
- iono

 viewer/printing:
 - does it work in inline in r studio and positron
 - iono


GIF saving:
- when it is supposed to work, does it create gif?
- How long to create gif?
- when it does not work doe it provide the right resposne saying that a gif cannot be made.
- is there instructions to provide to find the gif once it is created to find it in the gif dir?



This is my approach of providing further layers into testing


proivde a better partition a layers that use mine and provde a list of why you did what you did.
  
  