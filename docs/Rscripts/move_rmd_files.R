'match,group,is_participating,start,end,content
1,1,yes,26,57,2021-03-14-128-wicoz-bilanz.Rmd
1,2,yes,61,115,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
2,1,yes,163,200,2020-01-26-123-aktionstage-feb-20.Rmd
2,2,yes,204,268,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
3,1,yes,316,353,2019-11-29-122-aktionstage-dez-19.Rmd
3,2,yes,357,421,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
4,1,yes,469,514,2019-10-16-119-exkursion-klein-ganz-gross.Rmd
4,2,yes,518,572,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
5,1,yes,620,665,2019-10-16-116-workshop-wuehlmaeuse-im-gr.Rmd
5,2,yes,669,723,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
6,1,yes,771,816,2019-03-08-113-pflanzung-wolfbuehl-maerz2.Rmd
6,2,yes,820,884,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
7,1,yes,932,977,2017-11-07-102-exkursion-12-nov-resultate.Rmd
7,2,yes,981,1035,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
8,1,yes,1083,1128,2017-04-03-96-sonderausstellung-wildnispa.Rmd
8,2,yes,1132,1186,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
9,1,yes,1234,1274,2017-04-03-95-aktionstage-april-2017.Rmd
9,2,yes,1278,1342,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
10,1,yes,1390,1431,2017-02-15-111-strukturenbau-14-april.Rmd
10,2,yes,1435,1499,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
11,1,yes,1604,1646,2017-02-15-81-aktionstage-februar-2017.Rmd
11,2,yes,1650,1714,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
12,1,yes,1762,1807,2017-01-30-79-hecken-und-waldrandpflegeku.Rmd
12,2,yes,1811,1865,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
13,1,yes,1914,1959,2017-01-26-29-erste-moeglichkeiten-zum-mi.Rmd
13,2,yes,1963,2027,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
14,1,yes,2075,2120,2016-03-08-71-aststrukturen-bauen-sa-12-m.Rmd
14,2,yes,2124,2188,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
15,1,yes,2237,2277,2016-03-07-74-aktionstage-april-2016.Rmd
15,2,yes,2281,2345,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
16,1,yes,2393,2438,2016-02-26-70-mit-ferienpass-kleinstruktu.Rmd
16,2,yes,2442,2506,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
17,1,yes,2554,2599,2016-01-28-68-einladung-zur-infoveranstal.Rmd
17,2,yes,2603,2657,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
18,1,yes,2705,2750,2014-01-29-16-exkursion-kleinraubtiere-ke.Rmd
18,2,yes,2754,2808,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
19,1,yes,2856,2889,2014-01-29-18-unser-faltblatt.Rmd
19,2,yes,2893,2947,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
20,1,yes,2995,3012,pressespiegel.Rmd
20,2,yes,3016,3070,Bitte nach neuer Unter-Rubrik "Öffentlichkeitsarbeit"
21,1,yes,3118,3155,gesamtschau-der-wirkungskontrolle.Rmd
21,2,yes,3159,3209,Bitte nach neuer Unter-Rubrik "Wirkungskontrolle"
22,1,yes,3257,3279,112-beispiel-mosli.Rmd
22,2,yes,3283,3333,Bitte nach neuer Unter-Rubrik "Wirkungskontrolle"
23,1,yes,3381,3406,124-souvenir-dez-2019.Rmd
23,2,yes,3410,3474,Bitte nach Unter-Rubrik "Praktischer Naturschutz" (ex-Anpacken)
24,1,yes,3522,3566,80-massnahmen-sind-nachweislich-attrakti.Rmd
24,2,yes,3570,3620,Bitte nach neuer Unter-Rubrik "Wirkungskontrolle"
' %>%
  read_csv() -> opcy




folders <- tribble(
  ~col2, ~folder,
  "Öffentlichkeitsarbeit", "_infoveranstaltungen",
  "Praktischer Naturschutz", "_mithelfen-anpacken-mithelfen",
  "Wirkungskontrolle", "_wirkungskontrolle/"

)

move_df <- opcy %>%
  select(match, group, content) %>%
  pivot_wider(names_from = group, values_from = content,names_prefix = "col") %>%
  mutate(
    col2 = str_match(col2, '"(.+)\"')[,2]
  ) %>%
  left_join(folders) %>%
  select(-col2)




oper <- get_rmd_files() %>%
  tibble(filepath = .) %>%
  mutate(
    basename = basename(filepath),
    filepath = dirname(filepath)
         ) %>%
  right_join(move_df, by = c("basename" = "col1"))

oper %>%
  # slice(2) %>%
  select(filepath, folder) %>%
  pmap(function(filepath, folder){
    print(filepath)
    file.copy(filepath, folder, recursive = TRUE)
    unlink(filepath,recursive = TRUE)
    print("sucessfull")
  })


# didnt work:
# ./_news/2014-01-29-18-unser-faltblatt (to _infoveranstaltungen)
# ./_projekt-resultate/80-massnahmen-sind-nachweislich-attrakti (to _wirkungskontrolle)
# ./_projekt-resultate/pressespiegel (to _infoveranstaltungen)
# ./_wirkungskontrolle/2021-08-12-gesamtschau-der-wirkungskontrolle (to _wirkungskontrolle)




