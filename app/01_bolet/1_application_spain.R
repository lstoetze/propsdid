### Creating a combined & consistent dataset from raw electoral data


# data cleaning and manipulating
library(haven)
library(readxl)
library(tidyverse)
library(fixest)
library(broom)

# Install propsdid package via
# devtools::install_github("lstoetze/propsdid",subdir = "pkg")
# Need permission for private repo
# Create on github key and first set (or put in .Renviron) 
# Sys.setenv(GITHUB_PAT = "your_personal_access_token")

library(propsdid) # need to install from tar.tz

# 1. CLEAN ELECTION DATA ----------------------------------
## 1.1. Clean 2008 elections ------------------------------
data_2008 <- read_excel("data/spanish_electoral_archive/G2008_mesas.xlsx", sheet = "MUNICIPIOS") %>%
  select(1:11, 15:17, "PSOE", "PP", "IU", "UPyD", "PACMA") %>%
  mutate(VOX = NA) %>%
  # Cs and PODEMOS can be replaced by UPyD and IU-EQUO
  rename(PODEMOS = IU, Cs = UPyD, year = ANYO, elig = "CENSO+CERA") %>%
  filter(PROVINCIA %in% c("Asturias", "Teruel", "Leon"))


## 1.2 Clean 2011 elections -------------------------------collapsed
data_2011 <- read_excel("data/spanish_electoral_archive/G2011_mesas.xlsx", sheet = "MUNICIPIOS") %>%
  select(1:11, 15:17, "PSOE", "PP", "EQUO", "IU.LV", "UPyD", "PACMA") %>%
  mutate(VOX = NA, PODEMOS = rowSums(cbind(IU.LV, EQUO), na.rm = TRUE)) %>% # Cs and PODEMOS can be replaced by UPyD and IU-EQUO
  select(-IU.LV, -EQUO) %>%
  rename(Cs = UPyD, year = ANYO, elig = CENSO.CERA) %>%
  filter(PROVINCIA %in% c("Asturias", "Teruel", "Leon"))


## 1.3 Clean 2015 elections -------------------------------
data_2015 <- read_excel("data/spanish_electoral_archive/G2015_mesas.xlsx", sheet = "MUNICIPIOS") %>%
  select(1:11, 15:17, "PSOE", "PP", "PODEMOS", "Cs", "PACMA", "VOX") %>%
  mutate(VOX = coalesce(VOX, 0)) %>%  # Replace NA values with 0
  rename(year = ANYO, elig = "CENSO+CERA") %>%
  filter(PROVINCIA %in% c("Asturias", "Teruel", "Leon"))


## 1.4 Clean 2016 elections -------------------------------
data_2016 <- read_excel("data/spanish_electoral_archive/G2016_mesas.xlsx", sheet = "MUNICIPIOS") %>%
  select(1:11, 15:17, "PSOE", "PP", "PODEMOS", "Cs", "PACMA", "VOX") %>%
  mutate(VOX = coalesce(VOX, 0)) %>%  # Replace NA values with 0
  rename(year = ANYO, elig = CENSO.CERA) %>%
  filter(PROVINCIA %in% c("Asturias", "Teruel", "Leon"))


## 1.5 Clean 2019 April elections -------------------------------
data_2019 <- read_excel("data/spanish_electoral_archive/G2019abril_mesas.xlsx", sheet = "MUNICIPIOS") %>%
  select(1:11, 15:17, "PSOE", "PP", "UNIDAS.PODEMOS", "Cs", "PACMA", "VOX") %>% # together these parties have 99% votes in three provinces
  mutate(VOX = coalesce(VOX, 0)) %>%  # Replace NA values with 0
  rename(PODEMOS = UNIDAS.PODEMOS, year = ANYO, elig = CENSO.CERA) %>%
  filter(PROVINCIA %in% c("Asturias", "Teruel", "Leon"))


## 1.6. Append elections ------------------------------
elec <- bind_rows(data_2008, data_2011, data_2015, data_2016, data_2019) %>%
  rename(munid = cod.mun, province = PROVINCIA, municipality = MUNICIPIO,
         null_votes = NULOS, blank_votes = BLANCOS, list_votes = VOTOS.CANDIDATURAS) %>%
  mutate(votes = list_votes + blank_votes, ballots = votes + null_votes) %>%
  # harmonizing the spelling with Bolet et al. to merge the data, # e.g. cuba (la) -> la cuba
  mutate(municipality = tolower(municipality),
         province = ifelse(province == "Leon", "León", province),
         municipality = gsub("^(.*) \\((.*?)\\)$", "\\2 \\1", municipality),
         municipality = ifelse(municipality == "villadecanes", "toral de los vados", municipality)) %>%
  # drop voting abroad - 3 observations per year (one for each province)
  filter(!grepl("^cera\\d{2}$", municipality)) %>%
  # drop unnecessary variables
  select(-MES, -COD.CCAA, CCAA, -COD.PROV, -COD.MUN, -CENSO)

# The panel is fully balanced:
print (elec %>%
         group_by(munid) %>%
         summarise(years_observed = n_distinct(year)) %>%
         filter(years_observed < 5))

# Calculate the percentage variables
elec <- elec %>%
  mutate(turnout = 100*votes / elig,
         psoe = 100*PSOE / votes,           # this is how % are counted in Spain: with blank votes
         psoelist = 100*PSOE / list_votes,  # another way used a lot elsewhere: without blank votes
         psoeballots = 100*PSOE / ballots,  # this is what Bolet et al. used: with blank and null
         pp = 100*PP / votes,
         podem = 100*PODEMOS / votes,
         cs = 100*Cs / votes,
         vox = 100*VOX / votes,
         pacma = 100*PACMA / votes,
         others = 100 - rowSums(cbind(psoe, pp, podem, cs, vox, pacma), na.rm = TRUE)) %>%
  select(-PSOE, -PP, -PODEMOS, -Cs, -VOX, -PACMA)

summary(elec)

# save as dta
# write_dta(elec, "data/elec.dta")

# 2. MERGE w/ APSR DATA ----------------------------------
## 2.1. Read APSR data ------------------------------
data_2008 <- read_dta("data/apsr_replication_package/2008.dta")
data_2011 <- read_dta("data/apsr_replication_package/2011.dta")
data_2015 <- read_dta("data/apsr_replication_package/2015.dta")
data_2016 <- read_dta("data/apsr_replication_package/2016.dta")
data_2019 <- read_dta("data/apsr_replication_package/2019.dta")
age_pop <- read_dta("data/apsr_replication_package/age_population.dta")
educat <- read_dta("data/apsr_replication_package/education.dta")
unemploy_growth <- read_dta("data/apsr_replication_package/unemploymentgrowth.dta") %>%
  distinct() # remove duplicate rows just to receive less warnings when merging


## 2.2. Clean APSR data ------------------
# Correct a mistake in the 2008 data
data_2008 <- data_2008 %>%
  filter(!(municipality == "toral de los vados")) %>%
  mutate(municipality = ifelse(municipality == "villadecanes", "toral de los vados", municipality),
    coalmine = ifelse(municipality == "toral de los vados", 1, coalmine))

# Combine the APSR data into one frame and create a treatment variable
coal <- bind_rows(data_2008, data_2011, data_2015, data_2016, data_2019) %>%
  # create a treatment variable
  mutate(post = as.numeric(year > 2016),
         coalXpost = coalmine * post) %>%
  # merge with data on covariates
  left_join(unemploy_growth, by = c("municipality", "province")) %>%
  left_join(educat, by = c("municipality", "province")) %>%
  left_join(select(age_pop, -totalpopulation), by = c("municipality", "province")) %>%
  # generate additional covariate variables
  mutate(pop_log = log(totalpopulation),
         men_50_rs = (men_50_plus - min(men_50_plus, na.rm = TRUE)) /
           (max(men_50_plus, na.rm = TRUE) - min(men_50_plus, na.rm = TRUE)) * 100,
         immigration_rs = (immigration - min(immigration, na.rm = TRUE)) /
           (max(immigration, na.rm = TRUE) - min(immigration, na.rm = TRUE)) * 100,
         municipality = tolower(municipality)) %>%
  filter(province %in% c("Asturias", "Teruel", "León")) %>%
  rename (psoeapsr = psoe)

# Remove the intermediary data
rm(age_pop, educat, unemploy_growth, data_2008, data_2011, data_2015, data_2016, data_2019)


## 2.3. Merge APSR w/ Election data -----
spain <- inner_join(coal, elec, by = c("year", "province", "municipality"))

# Remove the intermediary data
rm(coal, elec)

# Check: the APSR article used PSOE share w.r.t. to all ballots cast
all.equal(spain$psoeapsr, spain$psoeballots, tolerance = 0.001)
# We will use the shares w.r.t. valid votes more conventional in political science


# 3. DIFF-IN-DIFF ------------------
## 3.1 Replicate the main APSR result ------------------
apsr1 <- feols(psoeapsr ~ coalmine + post + coalXpost, data = spain, cluster = "munid")
sum_apsr1 <- tidy(apsr1)
apsr2 <- feols(psoeapsr ~ coalmine + coalXpost | year + province, data = spain, cluster = "munid")
sum_apsr2 <- tidy(apsr2)
apsr3 <- feols(psoeapsr ~ coalmine + coalXpost + pop_log + no_education + growthunemployment + men_50_rs + immigration_rs | year + province, data = spain, cluster = "munid")
sum_apsr3 <- tidy(apsr3)
# This fully replicates columns 1-3 in Table 1 from the paper
# The difference in numbers is only due to correcting the data error in 2.1
#rm(apsr1, apsr2, apsr3, sum_apsr1, sum_apsr2, sum_apsr3)


## 3.2 DiD for all parties ------------------
# From now on, we use vote shares relative to all votes for parties
# Rerun regression 3 for all parties
run_did_apsr <- function(dep_var, data) {
  formula <- as.formula(paste(dep_var, " ~ coalmine + post + coalXpost"))
  model <- feols(formula, data = data, cluster = "munid")
  return(model)
}

# List of parties
parties <- c("psoe", "pp", "podem", "cs", "pacma", "vox", "others")

# Create an empty list to store models
models <- list()

# Run the same DID regression for each party and store models in the list
for (party in parties) {
  model <- run_did_apsr(party, spain)
  models[[party]] <- model
}

### 3.1.1 Export DiD results ----
# Show the summary table
etable(models, depvar = FALSE, signif.code = c("***"=0.01, "**"=0.05, "*"=0.10),
       group=list("controls"=c("pop_log", "no_education", "growthunemployment", "men_50_rs", "immigration_rs")))

# Create a combined table for estimation results for each party & turnout
sdid_table <- matrix(nrow = 2, ncol = length(parties))
colnames(sdid_table) <- parties
rownames(sdid_table) <- c("coalXpost", "")


# Fill in the combined table with regression results
for (i in seq_along(models)) {
  sdid_table[1, i] <- coef(models[[i]])["coalXpost"]  # Access the first element of the vector
  sdid_table[2, i] <- sqrt(vcov(models[[i]])["coalXpost","coalXpost"]) # Access the second element of the vector
}

# Round the values in the combined table
sdid_table <- round(sdid_table, digits = 2)

# Add parentheses for standard errors
sdid_table[2, ] <- paste0("(", sdid_table[2, ], ")")


# Generate the LaTeX table
latex_output <- capture.output({
  cat("\\begin{table}[ht]\n")
  cat("\\centering\n")
  cat("\\begin{tabular}{lccccccc}\n")
  cat("\\hline\n")
  cat("\\hline\n")
  cat("& PSOE & PP & PODEMOS & Citizens & PACMA & VOX & Others \\\\\n")
  cat("\\hline\n")

  for (i in 1:nrow(sdid_table)) {
    row_data <- sdid_table[i, ]
    cat(paste0("\\textit{", rownames(sdid_table)[i], "} & ", paste0(row_data, collapse = " & "), " \\\\\n"))

    if (i %% 2 == 0) {
      cat("\\hline\n")
    }
  }

  cat("\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}")
})

# Show and save the LaTeX output
cat(paste(latex_output, collapse = "\n"))
cat(paste(latex_output, collapse = "\n"), file="tables/did_parties_separate.tex")



## 3.2 DiD for all parties (own Package) ------------------

run_did <- function(dep_var, data) {
  sdid_data = as.data.frame(data) %>%  # synthdid does not work with tibbles
    select(munid,year,dep_var,coalXpost) %>%
    filter(complete.cases(.))          # synthdid does not work with NA values
  setup = panel.matrices(sdid_data)
  estimate <- synthdidprop::sc_estimate(Y=setup$Y, N0=setup$N0, T0=setup$T0,method = "did")
  ste = sqrt(vcov(estimate, method='placebo'))
  model <- c(estimate,ste)
  return(model)
}

# List of parties
parties <- c("psoe", "pp", "podem", "cs", "pacma", "vox", "others")

# Create an empty list to store models
models <- list()

# Run the same DID regression for each party and store models in the list
for (party in parties) {
  model <- run_did(party, spain)
  models[[party]] <- model
}




# Create a combined table for estimation results for each party & turnout
  sdid_table <- matrix(nrow = 2, ncol = length(parties))
  colnames(sdid_table) <- parties
  rownames(sdid_table) <- c("coalXpost", "")

  # Fill in the combined table with regression results
  for (i in seq_along(models)) {
    sdid_table[1, i] <- models[[i]][1]  # Access the first element of the vector
    sdid_table[2, i] <- models[[i]][2]  # Access the second element of the vector
  }

  # Round the values in the combined table
  sdid_table <- round(sdid_table, digits = 2)

  # Add parentheses for standard errors
  sdid_table[2, ] <- paste0("(", sdid_table[2, ], ")")


### 3.2.1 Export DiD results ----
# Generate the LaTeX table
latex_output <- capture.output({
    cat("\\begin{table}[ht]\n")
    cat("\\centering\n")
    cat("\\begin{tabular}{lccccccc}\n")
    cat("\\hline\n")
    cat("\\hline\n")
    cat("& PSOE & PP & PODEMOS & Citizens & PACMA & VOX & Others \\\\\n")
    cat("\\hline\n")

      for (i in 1:nrow(sdid_table)) {
        row_data <- sdid_table[i, ]
        cat(paste0("\\textit{", rownames(sdid_table)[i], "} & ", paste0(row_data, collapse = " & "), " \\\\\n"))

        if (i %% 2 == 0) {
          cat("\\hline\n")
        }
      }

      cat("\\hline\n")
      cat("\\end{tabular}\n")
      cat("\\end{table}")
    })

# Show and save the LaTeX output
cat(paste(latex_output, collapse = "\n"))
cat(paste(latex_output, collapse = "\n"), file="tables/did2_parties_separate.tex")



## 3.3 Synthethic DiD for all parties separately ------------------

run_sdid <- function(dep_var, data) {
  sdid_data = as.data.frame(data) %>%  # synthdid does not work with tibbles
    select(munid,year,dep_var,coalXpost) %>%
    filter(complete.cases(.))          # synthdid does not work with NA values
  setup = panel.matrices(sdid_data)
  estimate <- synthdidprop::sc_estimate(Y=setup$Y, N0=setup$N0, T0=setup$T0,method = "sdid")
  ste = sqrt(vcov(estimate, method='jackknife'))
  model <- c(estimate,ste)
  return(model)
}

# List of parties
parties <- c("psoe", "pp", "podem", "cs", "pacma", "vox", "others")

# Create an empty list to store models
models <- list()

# Run the same DID regression for each party and store models in the list
for (party in parties) {
  model <- run_sdid(party, spain)
  models[[party]] <- model
}




# Create a combined table for estimation results for each party & turnout
sdid_table <- matrix(nrow = 2, ncol = length(parties))
colnames(sdid_table) <- parties
rownames(sdid_table) <- c("coalXpost", "")

# Fill in the combined table with regression results
for (i in seq_along(models)) {
  sdid_table[1, i] <- models[[i]][1]  # Access the first element of the vector
  sdid_table[2, i] <- models[[i]][2]  # Access the second element of the vector
}

# Round the values in the combined table
sdid_table <- round(sdid_table, digits = 2)

# Add parentheses for standard errors
sdid_table[2, ] <- paste0("(", sdid_table[2, ], ")")

### 3.3.1 Export SDiD results ----
# Generate the LaTeX table
latex_output <- capture.output({
  cat("\\begin{table}[ht]\n")
  cat("\\centering\n")
  cat("\\begin{tabular}{lccccccc}\n")
  cat("\\hline\n")
  cat("\\hline\n")
  cat("& PSOE & PP & PODEMOS & Citizens & PACMA & VOX & Others \\\\\n")
  cat("\\hline\n")

  for (i in 1:nrow(sdid_table)) {
    row_data <- sdid_table[i, ]
    cat(paste0("\\textit{", rownames(sdid_table)[i], "} & ", paste0(row_data, collapse = " & "), " \\\\\n"))

    if (i %% 2 == 0) {
      cat("\\hline\n")
    }
  }

  cat("\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}")
})

# Show and save the LaTeX output
cat(paste(latex_output, collapse = "\n"))
cat(paste(latex_output, collapse = "\n"), file="tables/sdid_parties_separate.tex")



## 3.3 SDiD for proportions  ------------------

# Prepare Data Long format
spain_long <- as.data.frame(spain) %>%  # Synthdid does not work with tibbles
  select("i"=munid,"t"=year,
         y_psoe="psoe",y_pp= "pp", y_podem="podem",y_cs= "cs",
         y_pacma="pacma", y_vox="vox",y_others="others",
         "coalXpost"=coalXpost) %>%
  mutate(y_vox = ifelse(is.na(y_vox), 0, y_vox)) %>%
  pivot_longer(starts_with("y_")) %>%
  mutate(name = case_when(
    name == "y_psoe" ~ 1,
    name == "y_pp" ~ 2,
    name == "y_podem" ~ 3,
    name == "y_cs" ~ 4,
    name == "y_pacma" ~ 5,
    name == "y_vox" ~ 6,
    name == "y_others" ~ 7,
  )  )

# Prepare Data as an array
df_spain <- panel.array(data.frame(spain_long),unit="i", time="t",
                        category = "name",outcome="value",treatment="coalXpost")

# Estimate model
est_did <- sc_estimate(df_spain$Y,df_spain$N0, df_spain$T0,
                       porp_dat=T,method="did")

est_sc <- sc_estimate(df_spain$Y,df_spain$N0, df_spain$T0,
                       porp_dat=T,method="sc")

est_sdid <- sc_estimate(df_spain$Y,df_spain$N0, df_spain$T0,
                        porp_dat=T,method="sdid")

plot(est_sdid, 
     facet.vertical=F,
     estimates.label = parties[1]) +
  scale_color_grey()


sdid_data = as.data.frame(spain) %>%  # synthdid does not work with tibbles
  select(munid,year,"psoe",coalXpost) %>%
  filter(complete.cases(.))          # synthdid does not work with NA values
setup = panel.matrices(sdid_data)
estimate_psoe <- synthdidprop::sc_estimate(Y=setup$Y, N0=setup$N0, T0=setup$T0,method = "sdid")

sdid_data = as.data.frame(spain) %>%  # synthdid does not work with tibbles
  select(munid,year,"pp",coalXpost) %>%
  filter(complete.cases(.))          # synthdid does not work with NA values
setup = panel.matrices(sdid_data)
estimate_pp <- synthdidprop::sc_estimate(Y=setup$Y, N0=setup$N0, T0=setup$T0,method = "sdid")


plot(est_sdid,
     which_plot = c(1,2),
     estimates.label=parties[1:2],
     facet.vertical = F) +
  scale_color_grey() +
  ylab("Percentage Vote Share") +
  scale_x_continuous(breaks = unique(spain$year), name = "Election") +
  theme(panel.grid.minor = element_blank())

# Create a combined table for estimation results for each party & turnout
sdid_table <- matrix(nrow = 6, ncol = length(parties))
colnames(sdid_table) <- parties

rownames(sdid_table) <- c("DiD","", "SC","","SDiD","")

# Estimates and Se
se_did <- sqrt(vcov(est_did, method =  "jackknife"))
se_sc <-  sqrt(vcov(est_sc, method =  "jackknife"))
se_sdid <- sqrt(vcov(est_sdid, method =  "jackknife"))


# Fill in the combined table with regression results
for (i in 1:ncol(sdid_table)) {
  sdid_table[1, i] <- c(est_did)[i] # Access the first element of the vector
  sdid_table[2, i] <- se_did[i] # Access the first element of the vector
  sdid_table[3, i] <- c(est_sc)[i] # Access the first element of the vector
  sdid_table[4, i] <- se_sc[i] # Access the first element of the vector
  sdid_table[5, i] <- c(est_sdid)[i] # Access the first element of the vector
  sdid_table[6, i] <- se_sdid[i] # Access the first element of the vector
}

# Round the values in the combined table
sdid_table <- round(sdid_table, digits = 2)

# Add parentheses for standard errors
sdid_table[c(2,4,6), ] <- paste0("(", sdid_table[c(2,4,6), ], ")")

### 3.2.1 Export SDiD results ----
# Generate the LaTeX table
latex_output <- capture.output({
  cat("\\begin{table}[ht]\n")
  cat("\\centering\n")
  cat("\\begin{tabular}{lccccccc}\n")
  cat("\\hline\n")
  cat("\\hline\n")
  cat("& PSOE & PP & PODEMOS & Citizens & PACMA & VOX & Others \\\\\n")
  cat("\\hline\n")

  for (i in 1:nrow(sdid_table)) {
    row_data <- sdid_table[i, ]
    cat(paste0("\\textit{", rownames(sdid_table)[i], "} & ", paste0(row_data, collapse = " & "), " \\\\\n"))

    if (i %% 2 == 0) {
      cat("\\hline\n")
    }
  }

  cat("\\hline\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}")
})

# Show and save the LaTeX output
cat(paste(latex_output, collapse = "\n"))
cat(paste(latex_output, collapse = "\n"), file="tables/sdid_parties_prop.tex")
