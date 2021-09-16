
## **Data Reuse Plan**

This is the basic metadata file for our project dataset, also housed in this repository.

## | **What** |

**Description (abstract):** This project is about niche differentiation of lady beetles at Kellogg Biological Station (KBS), South-western Michigan, United States. The major goal of this project is to analyse long term ecological data collected since 1989 on two species of lady beetles. This project is a part of a graduate coursework in Reproducible Quantitative Methods (RQM) at Kent State University.

**Title:** Coccinella septempunctata and Harmonia axyridis caught on sticky traps in the Main Cropping System Experiment, Kellogg Biological Station, 1989-2020

**Permanent Identifier:**

http://doi.org/10.6073/pasta/6b6cc0ad7897d9008e8cf918bbf552d2 
Original data landing page: https://lter.kbs.msu.edu/datatables/67 

**Data Source:** Data extracted from KBS LTER Main Cropping System Experiment
KBS LTER Datatable - Insect Populations via Sticky Traps (msu.edu)
     
**Subject:** Entomology, community ecology, environmental science, agriculture, invasive species, insects

**Related publications:** Have you published an article, thesis or some other publication based on this data? Include a full citation and permanent identifier, if available.
https://doi.org/10.1371/journal.pcbi.1007542
10.1371/journal.pone.0083407
10.1007/s10530-014-0772-4
http://dx.doi.org/10.1890/14-2022.1 

## | **Who** |

KBS LTER, Michigan State University; Christie Bahlai, PhD, Kent State University

**Funder Information** : Support for this research was also provided by the NSF Long-term Ecological Research Program (DEB 1832042) at the Kellogg Biological Station and by Michigan State University AgBioResearch. Additional funding has come from US Department of Energy, US Department of Agriculture, and Electrical Power Research, the BEACON Center for Evolution in Action, and the Mozilla Foundation.


**Collaborators:** Christie Bahlai, Erin VanderJeugdt, Matthew B. Arnold, Omon Obarein, Prashant Ghimire, Nageen Farooq, Michael Back, Kyle Smart, Trixie Taucher, Mike Crowell. Affiliations include: Kent State University, Michigan State University

**Contact person:** 

      Christie Bahlai, Investigator, cbahlai@kent.edu 
Doug Landis, Lead Investigator, landisd@msu.edu 
     

## | **Where** |

**Location:** Kellogg Biological Station’s Main Cropping System Experiment, and adjacent forest plots, South West Michigan.

## | **When** |

**Temporal Coverage:** 1989-05-24 to 2020-09-04. Sampling is conducted weekly during the growing season as described in the sampling protocol. Sampling periods varied from 8-15 weeks in a given year depending on crop management issues and labor availability.

**Publication Date:** When was the data made available in the place of publication (above)? 

        Most recent addition: 2020-09-04 (we have access to newer data, pre-publication)

## | **How** |

**Data collection process:** The standard method used to measure these organisms is a yellow sticky trap. Insects collected on sticky-trap are classified by species,family and order. The insects are counted for each treatment, replicate and station combination. The actual sticky trap location coordinates are given in UTM 16N.
Protocol - Insect Abundance - Sticky Traps (msu.edu)

**Data processing:** How did you clean the data? How are missing or null values handled? Did you write code for processing the data and where can it be found?

      **File index:**

See https://github.com/ReproducibleQM/space_invader/tree/main/data

**File format/s:** Dataset saved as text file in the comma separated values (.csv) format. This file can be viewed in any text editor, or any spreadsheet software.




## Variables that appear in these data:

Year: The year the sample was collected from 1989-2020.
DATE: The date the sample was collected in mm/dd/yyyy format.
TREAT: Treatment ID of plot from 1-7, plus three forest treatments, SF, DF, and CF.
 	Possible values: T1- Field crop rotation, Conventional management 
			T2- Field crop rotation, no-till management
			T3- Reduced input crop rotation with cover crop
T4- Biologically based crop rotation with cover crop
T5- Poplar (plated in 2019)
T6- Switchgrass (Alfalfa from 19
T7- Early successional community 
HABITAT: The predominant plant or plant community present  in the plot, in a given year. Note that the original data file contains spelling errors that are resolved by substitution in the data cleaning script.
Maize, soybean, wheat, early successional, poplar, alfalfa, switchgrass, successional forest, coniferous forest, deciduous forest.
REPLICATE: Number identifiers for replicates of treatment plots. There are six replicates of the agricultural treatments T1-T7, and three replicates each of the surrounding forest plots, SF, CF, and DF
STATION: Sampling station within each plot, from five permanent sampling stations.
SPID: Species Identification: C7 for Coccinella septempunctata and HAXY for Harmonia axyridis. 
SumOfADULTS: number of adults of that species that was in the trap at sampling time

