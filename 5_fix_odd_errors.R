# 5_fix_odd_errors.R
# fix errors in DOIs
# May 2025

# tiny number of missing
reference_dois = filter(reference_dois, !is.na(cited_doi))
# replace commas
reference_dois = mutate(reference_dois, 
                        cited_doi = str_replace_all(cited_doi, ',', '.'))

#
index = reference_dois$cited_doi == "10.1001/jamadermatol.2020.1300. doi: 10.1001/jamadermatol.2020.1300"
reference_dois$cited_doi[index] = "10.1001/jamadermatol.2020.1300"
#
index = which(str_detect(reference_dois$cited_doi, "10.1371/journal.pone.0054722 hold stocks"))
reference_dois$cited_doi[index] = "10.1371/journal.pone.0054722"
#
index = reference_dois$cited_doi == "10.3390/ijerph16152735. PMID: 31370266; PMCID: PMC6696211"
reference_dois$cited_doi[index] = "10.3390/ijerph16152735"
#
index = reference_dois$cited_doi == "10.1038/s41598-021-94330-1. PMID: 34290294; PMCID: PMC8295309" 
reference_dois$cited_doi[index] = "10.1038/s41598-021-94330-1"
#
index = reference_dois$cited_doi == "pii: e12648. doi: 10.1371/journal.pone.0012648"
reference_dois$cited_doi[index] = "10.1371/journal.pone.0012648"
#
index = reference_dois$cited_doi == "10.1001/jamanetworkopen.2020.3976 PMC7090843"
reference_dois$cited_doi[index] = "10.1001/jamanetworkopen.2020.3976"
#
index = reference_dois$cited_doi == "10.1371/journal.pone.0061825. 23650505; PMCID: PMC3641072"
reference_dois$cited_doi[index] = "10.1371/journal.pone.0061825"
#
index = reference_dois$cited_doi == "https://doi.org/10.1186/s12889-022-14283-6 2018;4(November):14283-6"
reference_dois$cited_doi[index] = "10.1186/s12889-022-14283-6"
#
index = reference_dois$cited_doi == "10.1186/cc5027. PMID: 16919169; PMCID: PMC1750978"
reference_dois$cited_doi[index] = "10.1186/cc5027"
#
index = reference_dois$cited_doi == "10.1371/journal.pmed.1000100 pmid:19621070"
reference_dois$cited_doi[index] = "10.1371/journal.pmed.1000100"
#
index = reference_dois$cited_doi == "Publisher Full Text"
reference_dois$cited_doi[index] = ''
# 
index = reference_dois$cited_doi == "10.1029/2005GC001083 ISSN: 1525-2027"
reference_dois$cited_doi[index] = '10.1029/2005GC001083'
# 
index = reference_dois$cited_doi == "10.1371/journal.pone.0145956 PMID - 26731103"
reference_dois$cited_doi[index] = '10.1371/journal.pone.0145956'
#
index = reference_dois$cited_doi == "10.1080/10934529.2013.776894. (18) (PDF)"
reference_dois$cited_doi[index] = '10.1080/10934529.2013.776894'
#
index = reference_dois$cited_doi == "10.1109/SC.2010.18"
reference_dois$cited_doi[index] = '10.1109/SC.2010.18'
#
index = reference_dois$cited_doi == "0.1002/hed.26288"
reference_dois$cited_doi[index] = '10.1002/hed.26288'
#
index = reference_dois$cited_doi == "PMC7360503"
reference_dois$cited_doi[index] = '10.1159/000509143'
#
index = reference_dois$cited_doi == "PMC7359746"
reference_dois$cited_doi[index] = '10.12688/f1000research.24187.2'
#
index = reference_dois$cited_doi == "10.4049/jimmunol.2100842. Top choice read"
reference_dois$cited_doi[index] = '10.4049/jimmunol.2100842'
#
index = reference_dois$cited_doi == "10.3389/fchem.2018.00407. PMID: 30255015; PMCID: PMC6141690"
reference_dois$cited_doi[index] = '10.3389/fchem.2018.00407'
#
index = reference_dois$cited_doi == "10.1038/s42003-021-01961-1. PMID: 33785849; PMCID: PMC8010021"
reference_dois$cited_doi[index] = '10.1038/s42003-021-01961-1'
#
index = reference_dois$cited_doi == "10.1186/s13030-019-0142-7 PMC6357406"
reference_dois$cited_doi[index] = '10.1186/s13030-019-0142-7'
#
index = reference_dois$cited_doi == "10.3390/ijerph19159245 (September 15"
reference_dois$cited_doi[index] = '10.3390/ijerph19159245'
#
index = reference_dois$cited_doi == "https://doi.org/10.1080/15572 536.2006.11832797"
reference_dois$cited_doi[index] = '10.1080/15572536.2006.11832797'
#
index = reference_dois$cited_doi == "10.1371/journal.pone.0189576 E"
reference_dois$cited_doi[index] = '10.1371/journal.pone.0189576'
#
index = reference_dois$cited_doi == "10.1016/s1471-4892(01)00081-9. P.M.I.D.: 11764771"
reference_dois$cited_doi[index] = '10.1016/s1471-4892(01)00081-9'
#
index = reference_dois$cited_doi == "10.1007/978-3-642-25950-0_5, 105-123"
reference_dois$cited_doi[index] = '10.1007/978-3-642-25950-0_5'
#
index = reference_dois$cited_doi == "10.4014/jmb,1712.12006"
reference_dois$cited_doi[index] = "10.4014/jmb.1712.12006"
#
index = reference_dois$cited_doi == '10.5281/zenodo.5349280\">https://doi.org/10.5281/zenodo.5349280'
reference_dois$cited_doi[index] = '10.5281/zenodo.5349280'
#
index = reference_dois$cited_doi == "PMC7497983"
reference_dois$cited_doi[index] = '10.1371/journal.pntd.0008700'
#
index = reference_dois$cited_doi == "https://zenodo.org/record/4643152"
reference_dois$cited_doi[index] = '10.5281/zenodo.4542574'
#
index = reference_dois$cited_doi == "10.1371/journal.pone.0054722holdstocksinBionureFarmaSL.PVhasreceivedconsultancyfeesfromNovartis,Roche,MedImmune,HeidelbergEngineering.DignaBiotech.Neurotec.andBionureFarma.Thisdoesnotaltertheauthors'adherencetoallthePLOSONEpoliciesonsharingdataandmaterials.Epub2013/02/23.eng"
reference_dois$cited_doi[index] = '10.1371/journal.pone.0054722'
#
index = reference_dois$cited_doi == "10.5281/zenodo.5016186\">http://doi.org/10.5281/zenodo.5016186"
reference_dois$cited_doi[index] = '10.5281/zenodo.5016186'
#
index = reference_dois$cited_doi == "10.2139/ssrn.3205040\">https://dx.doi.org/10.2139/ssrn.3205040"
reference_dois$cited_doi[index] = '10.2139/ssrn.3205040'
#
index = reference_dois$cited_doi == "0.55575/tektonika2023.1.1.9"
reference_dois$cited_doi[index] = '10.55575/tektonika2023.1.1.9'
#
index = reference_dois$cited_doi == "0.6084/m9.figshare.22629394.v1"
reference_dois$cited_doi[index] = '10.6084/m9.figshare.22629394.v1'
#
index = reference_dois$cited_doi == "101093/nar/gkab301"
reference_dois$cited_doi[index] = '10.1093/nar/gkab301'
#
index = reference_dois$cited_doi == "0.17605/OSF.IO/KQX2D"
reference_dois$cited_doi[index] = "10.17605/OSF.IO/KQX2D"
#
index = reference_dois$cited_doi == ".1128/JB.05819-11"
reference_dois$cited_doi[index] = "10.1128/JB.05819-11"
#
index = reference_dois$cited_doi == "01.2013/JCPSP.6771"
reference_dois$cited_doi[index] = "10.2013/JCPSP.6771"
#
index = reference_dois$cited_doi == "CVJ-21.026"
reference_dois$cited_doi[index] = '10.5830/CVJA-2010-042'
#
index = reference_dois$cited_doi == "101093/occmed/kqx099"
reference_dois$cited_doi[index] = '10.1093/occmed/kqx099'
#
index = str_detect(reference_dois$cited_doi, pattern="S1727-897")
reference_dois$cited_doi[index] = ''
#
index = str_detect(reference_dois$cited_doi, pattern="eprints\\.utar\\.edu")
reference_dois$cited_doi[index] = ''
#
index = reference_dois$cited_doi == "20466419"
reference_dois$cited_doi[index] = '10.1016/S0140-6736(10)60549-1'
#
index = reference_dois$cited_doi == '10083/50265'
reference_dois$cited_doi[index] = ''
#
index = reference_dois$cited_doi == '298.0476//v148'
reference_dois$cited_doi[index] = ''
#
index = reference_dois$cited_doi == '.1590/S1413-35552009005000023'
reference_dois$cited_doi[index] = '10.1590/S1413-35552009005000023'
#
index = reference_dois$cited_doi =='2020.11.23.20237503'
reference_dois$cited_doi[index] = '10.1101/2020.11.23.20237503'

#
#index = reference_dois$cited_doi == ""
#reference_dois$cited_doi[index] = ''

# remove some obvious errors
obvious_errors = c("Free full text", "Reference source", "ReferenceSource", "Publisher full text")
obvious_errors = paste(obvious_errors, collapse='|')
reference_dois = filter(reference_dois,
                        !str_detect(cited_doi, obvious_errors))

# remove spaces
reference_dois = mutate(reference_dois, 
                        cited_doi = str_remove_all(cited_doi, '^(:|-) '),
                        cited_doi = str_remove_all(cited_doi, ' '))

# remove ending full-stop
reference_dois = mutate(reference_dois, 
                        cited_doi = str_remove_all(cited_doi, '\\.$'))
# remove http and doi at start
other_http = c('https://link.springer.com/articles?/','https://www.ahajournals.org/doi/',
               'https://www.frontiersin.org/articles/','https://www.biorxiv.org/content/',
               'https://ascopubs.org/doi/pdf/','Adoi.org/')
other_http = paste(other_http, collapse='|')
reference_dois = mutate(reference_dois, 
                        cited_doi = str_remove_all(cited_doi, '^doi(:|\\.)?'),
                        cited_doi = str_remove_all(cited_doi, other_http),
                        cited_doi = str_remove_all(cited_doi, '^https?://(dx\\.)?doi\\.org/|^doi\\.org/'))

# checks
filter(reference_dois, str_detect(cited_doi, ' ')) %>% pull(cited_doi)
filter(reference_dois, str_detect(cited_doi, ',')) %>% pull(cited_doi)
filter(reference_dois, str_detect(cited_doi, '\\.$')) %>% pull(cited_doi)
filter(reference_dois, str_detect(cited_doi, 'PMC|pmc')) %>% pull(cited_doi)
filter(reference_dois, str_detect(cited_doi, 'PMID|pmid|doi')) %>% pull(cited_doi)
filter(reference_dois, !str_detect(cited_doi, '10\\.')) %>% pull(cited_doi)
filter(reference_dois, str_detect(cited_doi, '^https')) %>% pull(cited_doi)
# remove few that do not start with 10.
reference_dois = filter(reference_dois, str_detect(cited_doi, '10\\.'))

### same fixed for papers cited by reviewers  ###
papers_reviewers_cited = mutate(papers_reviewers_cited, 
                        cited_doi = str_remove_all(cited_doi, '^doi(:|\\.)?'),
                        cited_doi = str_remove_all(cited_doi, other_http),
                        cited_doi = str_remove_all(cited_doi, '^https?://(dx\\.)?doi\\.org/|^doi\\.org/'))
#
index = papers_reviewers_cited$cited_doi == "10.1109/METRIC.1993.263792 pp. 141– 152"
papers_reviewers_cited$cited_doi[index] = '10.1109/METRIC.1993.263792'
#
#index = papers_reviewers_cited$cited_doi == ""
#papers_reviewers_cited$cited_doi[index] = ''

filter(papers_reviewers_cited, str_detect(cited_doi, 'PMID|pmid|doi|pmc|PMC')) %>% pull(cited_doi)
filter(papers_reviewers_cited, str_detect(cited_doi, ' ')) %>% pull(cited_doi)
#
papers_reviewers_cited = mutate(papers_reviewers_cited, 
                                str_replace_all(cited_doi, ' ', ''))


## very last fix, needs to be after previous code
index = reference_dois$cited_doi == "1471-0056.doi:10.1038/nrg3833"
reference_dois$cited_doi[index] = '10.1038/nrg3833'
