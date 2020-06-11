# psychmods

A personal project by me to extract and analyze data from [NUSMods](nusmods.com). The written post corresponding to this repository is found at https://aaron0696.github.io/psychmods/.

This repository contains the following documents.

1. *psychmods.Rmd* is the RMarkdown file that I have used to create the post at https://aaron0696.github.io/psychmods/, which details the codes, explanation and walkthrough of the objective, analysis and results.
2. *myBid.RDS* and *myModInfo.RDS* contains the information extracted from the [NUSMods API](https://nusmods.com/api/), saved in compact R objects. These are the datafiles used in *psychmods.Rmd* and were created with the codes detailed in the post.
3. *honourgif*, *labgif* and *elegif* are the respective .GIF files created to illustrate the change in module popularity ranking across semesters. Made using `gganimate` in R. The codes are available in psychomods.Rmd or the post, under Codes > Phase 5 > GIF Codes.