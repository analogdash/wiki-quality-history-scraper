#CONSTANTS
ORESLIMIT = 50 #maximum revids to request
THRESHOLD = 5 #Levenshtein distance to consider two urls similar

#INPUTS
samplegroup = "phil_pres"
title <- c("marcos")
pageids <- c(142721)

###################

library(ggplot2)
library(rjson)
library(RCurl)
library(httr)
library(tidyr)

samplepages <- list()
wikiextract <- list()
revrecord <- list()

samplepages[[samplegroup]] <- data.frame(title, pageids)

inittime = proc.time()

#groupcnt = 1
for(groupcnt in 1:length(samplepages)){
  
  samplegroup = names(samplepages[groupcnt])

  #pagecnt = 1
  for (pagecnt in 1:length(samplepages[[samplegroup]]$pageids)){
    pageid = toString(samplepages[[samplegroup]]$pageids[pagecnt])
    pagename = samplepages[[samplegroup]]$title[pagecnt]
    apicall = sprintf("https://en.wikipedia.org/w/api.php?action=query&format=json&pageids=%s&prop=revisions&rvlimit=500&rvprop=ids|timestamp|user|userid|parsedcomment|tags|size", pageid)
    jsonresult = getURL(apicall)
    wikidata <- fromJSON(jsonresult,unexpected.escape = "keep")
    wikiextract[[pageid]] <- wikidata$query$pages[[pageid]]$revisions
  
    #for continuing API calls
    conti = NULL
    rvconti = NULL
    while (is.null(wikidata$batchcomplete)){
      conti = wikidata$continue$continue
      rvconti = wikidata$continue$rvcontinue
      apicall = sprintf("https://en.wikipedia.org/w/api.php?action=query&format=json&pageids=%s&prop=revisions&rvlimit=500&rvprop=ids|timestamp|user|userid|parsedcomment|tags|size&continue=%s&rvcontinue=%s",pageid, conti, rvconti)
      jsonresult = getURL(apicall)
      wikidata <- fromJSON(jsonresult,unexpected.escape = "keep")
      wikiextract[[pageid]] <- append(wikiextract[[pageid]], wikidata$query$pages[[pageid]]$revisions)
    }
    
    totalrevs = length(wikiextract[[pageid]])
    batches = as.integer(totalrevs / ORESLIMIT)
    excess = totalrevs %% ORESLIMIT
    
    if(excess == 0){
      batches = batches - 1
      excess = ORESLIMIT
    }
    
    batchdamage = list()
    batchgoodfaith = list()
    batchreverted = list()
    batchwp10 = list()
    
    
    for (batchcnt in 0:batches){
      
      #for handling remainder
      if (batchcnt==batches){
        maxlimit = excess
      } else {
        maxlimit = ORESLIMIT
      }
      
      #formulatin' the API Query URL
      orescall = "https://ores.wmflabs.org/v2/scores/enwiki?revids="
      for (revcollector in 1:maxlimit){
        revisionid <- toString(wikiextract[[pageid]][[revcollector+batchcnt*50]]$revid)
        if (revcollector == 1){
          orescall = paste(orescall,revisionid,sep="")
        } else {
          orescall = paste(orescall,"|",revisionid,sep="")
        }
      }
      
      print(sprintf("DEBUG Making call numba %s of %s", batchcnt, batches))
      
      batchproc = fromJSON(getURL(orescall))
      
      batchdamage = append(batchdamage, batchproc$scores$enwiki$damaging$scores)
      batchgoodfaith = append(batchgoodfaith, batchproc$scores$enwiki$goodfaith$scores)
      batchreverted = append(batchreverted, batchproc$scores$enwiki$reverted$scores)
      batchwp10 = append(batchwp10, batchproc$scores$enwiki$wp10$scores)
      
    }

    for (revcnt in 1:totalrevs){
      revisionid = toString(wikiextract[[pageid]][[revcnt]]$revid)
      parentid = toString(wikiextract[[pageid]][[revcnt]]$parentid)
      times = strptime(wikiextract[[pageid]][[revcnt]]$timestamp, "%Y-%m-%dT%H:%M:%S")
      user = wikiextract[[pageid]][[revcnt]]$user
      userid = wikiextract[[pageid]][[revcnt]]$userid
      size = wikiextract[[pageid]][[revcnt]]$size
      comment = wikiextract[[pageid]][[revcnt]]$comment
      tags = wikiextract[[pageid]][[revcnt]]$tags
      
      apiparsecall = sprintf("https://en.wikipedia.org/w/api.php?action=parse&prop=externallinks&format=json&oldid=%s",revisionid)
      oldrefs = fromJSON(getURL(apiparsecall))
      links = oldrefs$parse$externallinks
      
      if(!is.null(batchgoodfaith[[revisionid]]$prediction) & !is.null(batchdamage[[revisionid]]$prediction) & !is.null(batchreverted[[revisionid]]$prediction) & !is.null(batchwp10[[revisionid]]$prediction)){
        isdamage = batchdamage[[revisionid]]$prediction
        isgoodfaith = batchgoodfaith[[revisionid]]$prediction
        isreverted = batchreverted[[revisionid]]$prediction
      
        chancedamage = batchdamage[[revisionid]]$probability$true
        chancegoodfaith = batchgoodfaith[[revisionid]]$probability$true
        chancereverted = batchreverted[[revisionid]]$probability$true
      
        wp10fa = batchwp10[[revisionid]]$probability$FA
        wp10ga = batchwp10[[revisionid]]$probability$GA
        wp10b = batchwp10[[revisionid]]$probability$B
        wp10c = batchwp10[[revisionid]]$probability$C
        wp10start = batchwp10[[revisionid]]$probability$Start
        wp10stub = batchwp10[[revisionid]]$probability$Stub
        
        revrecord[[samplegroup]][[pagename]][[revisionid]] <- list(times, parentid, user, userid, size, comment, tags, isdamage, isgoodfaith, isreverted, chancedamage, chancegoodfaith, chancereverted, wp10fa, wp10ga, wp10b, wp10c, wp10start, wp10stub, links)
        
        #if (revcnt==1){
         # revrecord[[samplegroup]][[pagename]] <- data.frame(times, revisionid, parentid, user, userid, size, comment, tags, isdamage, isgoodfaith, isreverted, chancedamage, chancegoodfaith, chancereverted, wp10fa, wp10ga, wp10b, wp10c, wp10start, wp10stub, links)
       # } else {
       #   revrecord[[samplegroup]][[pagename]] <- rbind(revrecord[[samplegroup]][[pagename]], data.frame(times, revisionid, parentid, user, userid, size, comment, tags, isdamage, isgoodfaith, isreverted, chancedamage, chancegoodfaith, chancereverted, wp10fa, wp10ga, wp10b, wp10c, wp10start, wp10stub, links))
        #}
        
      } else {
        orescall = "https://ores.wmflabs.org/v2/scores/enwiki?revids="
        orescall = paste(orescall,revisionid,sep="")
        batchproc = fromJSON(getURL(orescall))
        
        limit=0
        if(!is.null(batchproc$scores$enwiki$damaging$scores[[revisionid]]$error$type)){
          while (!is.null(batchproc$scores$enwiki$damaging$scores[[revisionid]]$error$type) & limit < 10){
            batchproc = fromJSON(getURL(orescall))
            limit = limit + 1
          }
        }
        
        
        if (limit == 10){
          print(cat(batchproc$scores$enwiki$damaging$scores[[revisionid]]$error$type, " with revid ", revisionid, " at revcnt ", revcnt))
          
        } else {
        
          isdamage = batchproc$scores$enwiki$damaging$scores[[revisionid]]$prediction
          isgoodfaith = batchproc$scores$enwiki$goodfaith$scores[[revisionid]]$prediction
          isreverted = batchproc$scores$enwiki$reverted$scores[[revisionid]]$prediction
          
          chancedamage = batchproc$scores$enwiki$damaging$scores[[revisionid]]$probability$true
          chancegoodfaith = batchproc$scores$enwiki$goodfaith$scores[[revisionid]]$probability$true
          chancereverted = batchproc$scores$enwiki$reverted$scores[[revisionid]]$probability$true
          
          wp10fa = batchproc$scores$enwiki$wp10$scores[[revisionid]]$probability$FA
          wp10ga = batchproc$scores$enwiki$wp10$scores[[revisionid]]$probability$GA
          wp10b = batchproc$scores$enwiki$wp10$scores[[revisionid]]$probability$B
          wp10c = batchproc$scores$enwiki$wp10$scores[[revisionid]]$probability$C
          wp10start = batchproc$scores$enwiki$wp10$scores[[revisionid]]$probability$Start
          wp10stub = batchproc$scores$enwiki$wp10$scores[[revisionid]]$probability$Stub
          revrecord[[samplegroup]][[pagename]][[revisionid]] <- list(times, parentid, user, userid, size, comment, tags, isdamage, isgoodfaith, isreverted, chancedamage, chancegoodfaith, chancereverted, wp10fa, wp10ga, wp10b, wp10c, wp10start, wp10stub, links)
          #if (revcnt==1){
          #  revrecord[[samplegroup]][[pagename]] <- data.frame(times, revisionid, parentid, user, userid, size, comment, tags, isdamage, isgoodfaith, isreverted, chancedamage, chancegoodfaith, chancereverted, wp10fa, wp10ga, wp10b, wp10c, wp10start, wp10stub, links)
          #} else {
          #  revrecord[[samplegroup]][[pagename]] <- rbind(revrecord[[samplegroup]][[pagename]], data.frame(times, revisionid, parentid, user, userid, size, comment, tags, isdamage, isgoodfaith, isreverted, chancedamage, chancegoodfaith, chancereverted, wp10fa, wp10ga, wp10b, wp10c, wp10start, wp10stub, links))
          #}
        
        }
      }
    }
   print(sprintf("DEBUG Article %s of %s done", pagecnt, length(samplepages[[samplegroup]]$pageids)))
  }
}

masterlist=list()
purgey = data.frame("purgedcitation"=character(), "levscore"=numeric(),"almost"=character(),"revisionid"=character(),"parentid"=character(),"censorer"=character(), "status"=character(), stringsAsFactors=FALSE)



for (revquery in 1:length(revrecord$phil_pres[[1]])){
  workinglist = revrecord$phil_pres[[1]][[revquery]][[20]]
  detruthed = setdiff(workinglist,masterlist)
  
  if (length(detruthed) > 0 && revquery > 1) {
    
    newerlist = revrecord$phil_pres[[1]][[revquery-1]][[20]]
    additions = setdiff(newerlist,workinglist)
    
    for (i in 1:length(detruthed)){
      
      purgedcitation = detruthed[i]
      revisionid = names(revrecord$phil_pres[[1]][revquery-1])
      parentid = revrecord$phil_pres[[1]][[revquery-1]][[2]]
      censorer = revrecord$phil_pres[[1]][[revquery-1]][[3]]
      levscore = 9999
      status = "unchecked"
      
      if(length(additions) > 0){
        for (k in 1:length(additions)){
          if (adist(detruthed[i], additions[k]) < levscore){
            levscore = adist(detruthed[i], additions[k])
            almost = additions[k]
            almost2 = ""
            close = 0
          } else if (adist(detruthed[i], additions[k]) == levscore){
            almost2 = paste(almost2, additions[k])
            close = close + 1
          }
        }
        if(close > 0){
          almost = paste(almost, almost2)
          #print(sprintf("CLOSE %s at %s", close, revquery))
        }
      } else {
        #levscore = 0
        almost = "blank"
      }
      
      if(levscore < THRESHOLD){
        status = "same"
      }
      
      purgey = rbind(purgey, data.frame(purgedcitation, levscore, almost,revisionid,parentid,censorer, status,stringsAsFactors = FALSE))
      
    }
  }
  masterlist = append(masterlist, detruthed)
}

for (checker in 1:length(purgey$purgedcitation)){
  if (purgey$status[checker] == "unchecked"){
    retrieve <- tryCatch(GET(toString(purgey$purgedcitation[checker])),error = function(e) e)
    if (inherits(retrieve,  "error")) {
      purgey$status[[checker]] = "unreachable"
      print("UNREACHABLE DETECTED!")
      next
    } else if(retrieve$status_code == 404){
      
      purgey$status[[checker]] = "missing"
      print("MISSING DETECTED!")
      
    } else if(toString(purgey$almost[checker]) != "blank") {
      retrieve2 = tryCatch(GET(toString(purgey$almost[checker])),error = function(e) e)
      if (!inherits(retrieve2,  "error")){
        if (retrieve$url == retrieve2$url){
          purgey$status[[checker]] = "same"
          print("SAME DETECTED!")
        }
      }
    }
  }
}

write.table(purgey,"marcos.txt",sep="\t")


totaltime = proc.time() - inittime
totalmin = as.integer(totaltime[3] / 60)
totalhr = as.integer(totalmin/60)
totalmin = totalmin %% 60
totalsec = totaltime[3] %% 60

print(sprintf("Scrape complete, total duration %02d:%02d:%2.2f", totalhr,totalmin,totalsec))
