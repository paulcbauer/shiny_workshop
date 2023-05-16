library(shiny)
library(jsonlite)

write_json(list(
  Crime_pers = list(
    title = "Crime against persons",
    desc = as.character(p(tags$b("Crime against persons:"), "Population per crime against persons", hr(), helpText("Source: Table A2 in Guerry (1833). Compte général, 1825-1830"))),
    lgd = "Pop. per crime",
    unit = ""
  ),
  Crime_prop = list(
    title = "Crime against property",
    desc = as.character(p(tags$b("Crime against property:"), "Population per crime against property", hr(), helpText("Source: Compte général, 1825-1830"))),
    lgd = "Pop. per crime",
    unit = ""
  ),
  Literacy = list(
    title = "Literacy",
    desc = as.character(p(tags$b("Percent Read & Write:"), "Percent of military conscripts who can read and write", hr(), helpText("Source: Table A2 in Guerry (1833)"))),
    lgd = "Literacy",
    unit = " %"
  ),
  Donations = list(
    title = "Donations to the poor",
    desc = as.character(p(tags$b("Donations to the poor"), hr(), helpText("Source: Table A2 in Guerry (1833). Bulletin des lois"))),
    lgd = "Donations",
    unit = ""
  ),
  Infants = list(
    title = "Illegitimate births",
    desc = as.character(p(tags$b("Population per illegitimate birth"), hr(), helpText("Source: Table A2 in Guerry (1833). Bureau des Longitudes, 1817-1821"))),
    lgd = "Pop. per birth",
    unit = ""
  ),
  Suicides = list(
    title = "Suicides",
    desc = as.character(p(tags$b("Population per suicide"), hr(), helpText("Source: Table A2 in Guerry (1833). Compte général, 1827-1830"))),
    lgd = "Pop. per suicide",
    unit = ""
  ),
  Wealth = list(
    title = "Tax / capita",
    desc = as.character(p(tags$b("Per capita tax on personal property:"), "A ranked index based on taxes on personal and movable property per inhabitant", hr(), helpText("Source: Table A1 in Guerry (1833)"))),
    lgd = "Tax / capita",
    unit = ""
  ),
  Commerce = list(
    title = "Commerce & Industry",
    desc = as.character(p(tags$b("Commerce & Industry:"), "Commerce and Industry, measured by the rank of the number of patents / population", hr(), helpText("Source: Table A1 in Guerry (1833)"))),
    lgd = "Patents / capita",
    unit = ""
  ),
  Clergy = list(
    title = "Clergy",
    desc = as.character(p(tags$b("Distribution of clergy:"), "Distribution of clergy, measured by the rank of the number of Catholic priests in active service / population", hr(), helpText("Source: Table A1 in Guerry (1833). Almanach officiel du clergy, 1829"))),
    lgd = "Priests / capita",
    unit = ""
  ),
  Crime_parents = list(
    title = "Crime against parents",
    desc = as.character(p(tags$b("Crime against parents:"), "Crimes against parents, measured by the rank of the ratio of crimes against parents to all crimes \u2013 Average for the years 1825-1830", hr(), helpText("Source: Table A1 in Guerry (1833). Compte général"))),
    lgd = "Share of crimes",
    unit = " %"
  ),
  Infanticide = list(
    title = "Infanticides",
    desc = as.character(p(tags$b("Infanticides per capita:"), "Ranked ratio of number of infanticides to population \u2013 Average for the years 1825-1830", hr(), helpText("Source: Table A1 in Guerry (1833). Compte général"))),
    lgd = "Infanticides / capita",
    unit = ""
  ),
  Donation_clergy = list(
    title = "Donations to the clergy",
    desc = as.character(p(tags$b("Donations to the clergy:"), "Ranked ratios of the number of bequests and donations inter vivios to population \u2013 Average for the years 1815-1824", hr(), helpText("Source: Table A1 in Guerry (1833). Bull. des lois, ordunn. d’autorisation"))),
    lgd = "Donations / capita",
    unit = ""
  ),
  Lottery = list(
    title = "Wager on Royal Lottery",
    desc = as.character(p(tags$b("Per capita wager on Royal Lottery:"), "Ranked ratio of the proceeds bet on the royal lottery to population \u2013 Average for the years 1822-1826", hr(), helpText("Source: Table A1 in Guerry (1833). Compte rendu par le ministre des finances"))),
    lgd = "Wager / capita",
    unit = ""
  ),
  Desertion = list(
    title = "Military desertion",
    desc = as.character(p(tags$b("Military desertion:"), "Military disertion, ratio of the number of young soldiers accused of desertion to the force of the military contingent, minus the deficit produced by the insufficiency of available billets\u2013 Average of the years 1825-1827", hr(), helpText("Source: Table A1 in Guerry (1833). Compte du ministère du guerre, 1829 état V"))),
    lgd = "No. of desertions",
    unit = ""
  ),
  Insturction = list(
    title = "Instruction",
    desc = as.character(p(tags$b("Instruction:"), "Ranks recorded from Guerry's map of Instruction. Note: this is inversely related to literacy (as defined here)")),
    lgd = "Instruction",
    unit = ""
  ),
  Prostitutes = list(
    title = "Prostitutes",
    desc = as.character(p(tags$b("Prostitutes in Paris:"), "Number of prostitutes registered in Paris from 1816 to 1834, classified by the department of their birth", hr(), helpText("Source: Parent-Duchatelet (1836),De la prostitution en Paris"))),
    lgd = "No. of prostitutes",
    unit = ""
  ),
  Distance = list(
    title = "Distance to paris",
    desc = as.character(p(tags$b("Distance to Paris (km):"), "Distance of each department centroid to the centroid of the Seine (Paris)", hr(), helpText("Source: Calculated from department centroids"))),
    lgd = "Distance",
    unit = " km"
  ),
  Area = list(
    title = "Area",
    desc = as.character(p(tags$b("Area (1000 km^2)"), hr(), helpText("Source: Angeville (1836)"))),
    lgd = "Area",
    unit = " km\u00b2"
  ),
  Pop1831 = list(
    title = "Population",
    desc = as.character(p(tags$b("Population in 1831, in 1000s"), hr(), helpText("Source: Taken from Angeville (1836), Essai sur la Statistique de la Population français"))),
    lgd = "Population (in 1000s)",
    unit = ""
  )
), path = "app_labels.json", pretty = TRUE)