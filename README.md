![FAFI](aux/fafi-sna-logo.png)

# Family as Faculty as Infrastructure Social Network Analysis

FAFI prepares and engages family and community members—particularly those representing intersectionally marginalized identities—as instructors for pre-admissions teacher education students. Family and community members are provided with voice and input in the teacher preparation process and teacher education students develop the dispositions and practices to hear and learn from families.

This component of the larger project examines the ways in which participants interact across their networks and engage in working together to support and facilitate learning for children. This is the [R code](https://www.r-project.org/) that drives the social network analysis. Utilized packages are listed in the code itself.

This project also seeks to humanize some of the language around social network analysis. For example, the descriptive labels for a key actor analysis have been reframed as:

* **Sages** (typically, "hubs")
* **Stewards** (typically, "gatekeepers")
* **Weavers** (typically, "pulsetakers")

In addition, we look at a variety of centralities as a way to understanding the ways in which the structures of the network can illuminate the *Positionality*, *Reachability*, *Potentiality*, and *Reputation* in working across networks.

## Directory Structure

```
fafi-sna/
┣ .github/                  <- Directory used by Github to serve webpage
┃ ┗ workflows/
┣ aux/                      <- Directory for storing auxiliary files
┃ ┗ fafi-sna-references.bib <- Bibliography for this project in BibTeX format
┃ ┗ theme.css               <- Cascading style sheet for the project
┃ ┗ fafi-sna-logo.png       <- Project logo
┣ docs/                     <- Directory for the rendered literate programming file
┃ ┗ index.html              <- Rendered version of the literate programming file
┣ output/                   <- Target directory for collecting R output files
┃ ┗ plots/                  <- Directory for storing plots in PDF and PNG formats
┃ ┗ csv/                    <- Directory for storing CSV files
┣ R                         <- Directory for storing R scripts
┃ ┗ fafi-sna.R              <- R script distilled from the literate programming file
┣ .gitignore                <- Files and directories to be ignored by Git
┣ .nojekyll                 <- File to tell Github to not use Jekyll
┣ CODE_OF_CONDUCT.md        <- Code of Conduct for project contributors
┣ LICENSE                   <- License (MIT) for project
┣ README.md                 <- This file, a general overview of this project
┗ index.Rmd                 <- Literate programming file for the project analysis
```

## Project Team

* [Jeremy F Price](https://www.jeremyfprice.info/) (Co-Principal Investigator and SNA Lead, IU School of Education-Indianapolis)
* [Cristina Santamaría Graff](https://education.iupui.edu/faculty-research/faculty-directory/santamaria-graff-cristina.html) (Principal Investigator, IU School of Education-Indianapolis)
* Akaash Arora (Graduate Research Assistant, IU School of Education-Indianapolis)
* Román Graff (Ivy Tech Community College, Indianapolis, IN)

## Contributing

To give feedback, report a bug, or request a feature, please open an issue.

To contribute a change, note that the distill project is released with a [Contributor Code of Conduct](code_of_conduct.md). By contributing to this project, you agree to abide by its terms.

## License

All code for this project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details. All outputs, plots, analyses, and figures are licensed under the [CC-BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/) license.

## Acknowledgements

The literate coding framework for this project utilizes the [Distill for R Markdown](https://rstudio.github.io/distill/) package.

This project is funded in part by a [National Association of Family, School, and Community Engagement Minigrant](https://nafsce.org/page/MiniGrant).

[![Open Source](https://img.shields.io/badge/SOURCE-open-informational)](LICENSE) [![Open Data](https://img.shields.io/badge/DATA-open-informational)](https://osf.io/y3xz4/) [![Open Analysis](https://img.shields.io/badge/ANALYSIS-open-informational)](https://osf.io/epsvt/) [![Open Materials](https://img.shields.io/badge/MATERIALS-open-informational)](https://osf.io/5tay8/) [![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
