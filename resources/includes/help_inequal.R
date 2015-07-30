#############################################################################
#
#
############################################################################

helperHTML <- function(term){
  # This function retrurns the definition of a term as an html document
  require('stringr')
  header <- '<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><title>Glossary</title></head><body>'
  tail <- '</body></html>'
  switch(term,
         'paf' = {
           body <- "<h2>Population attributable fraction (PAF)</h2>
                  The contribution of a risk factor to a disease or a death is quantified
                  using the population attributable fraction (PAF). PAF is the proportional
                  reduction in population disease or mortality that would occur if exposure to
                  a risk factor were reduced to an alternative ideal exposure scenario
                  (eg. no tobacco use). Many diseases are caused by multiple risk factors,
                  and individual risk factors may interact in their impact on overall risk of
                  disease. As a result, PAFs for individual risk factors often overlap and add up
                  to more than 100 percent.<br><br>
            <a href=http://www.who.int/healthinfo/global_burden_disease/metrics_paf/en target='_blank'>WHO definition</a>"
           str_replace_all(paste0(header, body, tail), '\n', '')
         }
  )
}

