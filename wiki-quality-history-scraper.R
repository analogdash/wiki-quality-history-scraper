library(rjson)

epoch <- c("prehist", "prespan", "span", "america", "marcos", "current")
pageids <- c(21617275, 21432966, 23888361, 5824976, 5824978, 5807137)
samplepages <- data.frame(epoch, pageids)
wikiextract <- list()
revrecord <- list()

for (i in 1:length(samplepages$pageids)){
  currpage <- toString(samplepages$pageids[i])
  apicall <- sprintf("https://en.wikipedia.org/w/api.php?action=query&format=json&pageids=%s&prop=revisions&rvlimit=500&rvprop=ids|timestamp", currpage)
  wikidata <- fromJSON(file=apicall)
  wikiextract[[currpage]] <- wikidata$query$pages[[currpage]]$revisions
  conti <- NULL
  rvconti <- NULL
  while (is.null(wikidata$batchcomplete)){
    conti <- wikidata$continue$continue
    rvconti <- wikidata$continue$rvcontinue
    apicall <- sprintf("https://en.wikipedia.org/w/api.php?action=query&format=json&pageids=%s&prop=revisions&rvlimit=500&rvprop=ids|timestamp&continue=%s&rvcontinue=%s",currpage, conti, rvconti)
    wikidata <- fromJSON(file=apicall)
    wikiextract[[currpage]] <- append(wikiextract[[currpage]], wikidata$query$pages[[currpage]]$revisions)
  }
  totalrevs <- length(wikiextract[[currpage]])
  batches <- as.integer(totalrevs / 50)
  batchdamage <- list()
  batchgoodfaith <- list()
  batchreverted <- list()
  batchwp10 <- list()
  for (j in 0:batches){
    if (j==batches){
      maxlimit = totalrevs %% 50
    } else {
      maxlimit = 50
    }
    orescall = "https://ores.wmflabs.org/v2/scores/enwiki?revids="
    for (k in 1:maxlimit){
      revisionid <- toString(wikiextract[[currpage]][[k+j*50]]$revid)
      if (k == 1){
        orescall <- paste(orescall,revisionid,sep="")
      } else {
        orescall <- paste(orescall,"|",revisionid,sep="")
      }
    }
    print("DEBUG Making call")
    batchproc <- fromJSON(file=orescall)
    batchdamage <- append(batchdamage, batchproc$scores$enwiki$damaging$scores)
    batchgoodfaith <- append(batchgoodfaith, batchproc$scores$enwiki$goodfaith$scores)
    batchreverted <- append(batchreverted, batchproc$scores$enwiki$reverted$scores)
    batchwp10 <- append(batchwp10, batchproc$scores$enwiki$wp10$scores)
  }
  
  for (n in 1:totalrevs){
    revisionid <- toString(wikiextract[[currpage]][[n]]$revid)
    times <- strptime(wikiextract[[currpage]][[n]]$timestamp, "%Y-%m-%dT%H:%M:%S")
    damage <- batchdamage[[revisionid]]$probability$true
    goodfaith <- batchgoodfaith[[revisionid]]$probability$true
    reverted <- batchreverted[[revisionid]]$probability$true
    wp10fa <- batchwp10[[revisionid]]$probability$FA
    wp10ga <- batchwp10[[revisionid]]$probability$GA
    wp10b <- batchwp10[[revisionid]]$probability$B
    wp10c <- batchwp10[[revisionid]]$probability$C
    wp10start <- batchwp10[[revisionid]]$probability$Start
    wp10stub <- batchwp10[[revisionid]]$probability$Stub
    
    if (n==1){
      revrecord[[currpage]] <- data.frame(times, revisionid, damage, goodfaith, reverted, wp10fa, wp10ga, wp10b, wp10c, wp10start, wp10stub)
    } else {
      revrecord[[currpage]] <- rbind(revrecord[[currpage]], data.frame(times, revisionid, damage, goodfaith, reverted, wp10fa, wp10ga, wp10b, wp10c, wp10start, wp10stub))
    }
  }
  print("DEBUG Article done")
}


