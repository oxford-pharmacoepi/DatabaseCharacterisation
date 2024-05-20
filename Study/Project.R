cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdmSchema, 
  writeSchema = writeSchema, 
  cdmName = dbName
)

conceptCounts <- read_csv(here("Study/Barts Health_concept_counts.csv"), 
                          show_col_types = FALSE, 
                          col_types = cols(.default = "c")) |>
  newSummarisedResult()

concepts <- cdm[["concept"]] |>
  select("variable_level" = "concept_id", "variable_name" = "concept_name") |>
  distinct() |>
  collect() |>
  mutate(variable_name = iconv(variable_name, to = "UTF-8", sub = ".")) |>
  mutate(variable_name = gsub("M\\+.ni\\+.re's","Meniere's",variable_name),
         variable_name = gsub("Sj\\+.gren","Sjogren's", variable_name),
         variable_name = gsub("Boutonni\\+.re","Boutonnier", variable_name),
         variable_name = gsub("Ch\\+.diak","Chediak", variable_name),
         variable_name = gsub("Cura\\+.ao", "Curasao", variable_name),
         variable_name = gsub("Cr\\+.ole","Creole", variable_name),
         variable_name = gsub("D\\+.j\\+.", "Deja", variable_name),
         variable_name = gsub("Sch+.nlein", "Schonlein", variable_name),
         variable_name = gsub("S\\+.quard", "Sequard", variable_name),
         variable_name = gsub("Waldenstr\\+.m", "Waldenstrom", variable_name),
         variable_name = gsub("Caf\\+.","Cafe", variable_name))  |>
  mutate(variable_level = as.character(variable_level))

conceptCounts |>
  select(-c("variable_name")) |>
  inner_join(
    concepts, by = "variable_level"
  ) |>
  exportSummarisedResult(fileName = "Barts Health_concept_counts_updated.csv")
