# 6_flow_chart_exclusions.R
# called from 6_exclusions.Rmd
# June 2025
library(diagram)
library(dplyr)

# from 0_find_papers_and_get_dois_dates.R; number of articles found by API searches; has been expanded to include all versions
load('data/0_api_search.RData')
n_found = nrow(all_dois)

# from 2_get_xml_details_and_combine.R
load('data/2_xml_data.RData')
n_reviews = nrow(reviewers_names)
n_excluded = nrow(excluded)
n_diff = n_found - n_excluded # article numbers progressing
remove(ref_meta_data, reviewers_names, papers_reviewers_cited, reference_dois, excluded) # no longer needed

# from 4_reviewers_papers_openalex.R
load('data/4_reviewers_papers.RData')
reviewers_excluded = excluded
excluded_tab = group_by(reviewers_excluded, reason) %>%
  tally()
n_openalex = format(filter(excluded_tab, reason == "No Open Alex record found") %>% pull(n), big.mark=',')
n_no_papers1 = filter(excluded_tab, reason == "Reviewer has no papers in their OpenAlex record") %>% pull(n)
n_no_papers = sum(is.na(matched$matches))
n_no_papers_total = n_no_papers1 + n_no_papers
n_reviews_remain = nrow(reviewers_excluded) # number remaining after articles with no reviewers
# from 5_match_papers_reviewers
load('data/5_analysis_data.RData')
n_reviews = sum(is.na(matched$matches))
ref = filter(matched, !is.na(matches)) %>%
  group_by(role) %>%
  tally()
tot_ref = format(sum(ref$n), big.mark=',') 
co_ref = format(filter(ref, role!='Referee') %>% pull(n), big.mark=',') # not referee is all numbered co-referees
reff = format(filter(ref, role=='Referee') %>% pull(n), big.mark=',')

# labels, big N for articles, little n for reviews
l1 = paste('Articles from\nsearches (N = ',  format(n_found, big.mark=','), ')', sep='') # 
l2 = paste('Articles with no\nreviews (N = ', format(n_excluded, big.mark=','), ')', sep='') # 
l3 = paste('Number of articles (N = ', format(n_diff, big.mark=','), ')\nNumber of reviews (n = ', format(n_reviews_remain, big.mark=','), ')', sep='') # 
l4 = paste('Reviewers with no\nOpenAlex record (n = ' , format(n_openalex, big.mark=','), ')', sep='') # 
l5 = paste('Reviewers with\nno articles (n = ', format(n_no_papers_total, big.mark=','), ')', sep='') # 
l6 = paste('Reviewers included (n = ', tot_ref,')\n- Referee (n = ', reff, ')\n- Co-referee (n = ', co_ref, ')', sep='') # 

null = ''
labels = c(l1, l2, l3, l4, l5, l6, null, null, null)
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.27	0.93	white	square	0.33	0.185
2	0.75	0.8	white	square	0.33	0.18
3	0.27	0.64	white	square	0.23	0.26
4	0.75	0.48	white	square	0.27	0.23
5	0.75	0.29	white	square	0.31	0.17
6	0.27	0.11	white	square	0.28	0.26
7	0.27	0.8	transparent	square	0.0001	0.0001
8	0.27	0.48	transparent	square	0.0001	0.0001
9	0.27	0.29	transparent	square	0.0001	0.0001')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = nrow(frame), ncol = nrow(frame), byrow = TRUE, data = 0)
M[3, 1] = "' '"
M[6, 3] = "' '"
M[2, 7] = "' '" # arrow to side
M[4, 8] = "' '" # arrow to side
M[5, 9] = "' '" # arrow to side
# colours
tcol = rep('black', nrow(frame))

## make figure 
jpeg('figures/6_flow_chart_excluded.jpg', width=4.8, height=5.5, units='in', res=400, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0, arr.pos = 0.45,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
invisible(dev.off())
