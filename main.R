setClass("duration", slots=list(find="numeric", delete="numeric", update="numeric"))
setClass("medDuration", slots=list(find="list", delete="list", update="list"))

initDuration <- new("duration",find=0, delete=0, update=0)
initMedDuration <- new("medDuration",find=list(), delete=list(), update=list())
dataToProcess <- read.csv(file = 'data.csv', header = TRUE)

maxReducer <- function(acc, idx) {
  duration = strtoi(dataToProcess[idx, 3]) - strtoi(dataToProcess[idx, 2])
  if (duration > slot(acc, dataToProcess[idx, 1])) {
    slot(acc, dataToProcess[idx, 1]) = duration
  }
  return(acc)
}

minReducer <- function(acc, idx) {
  duration = strtoi(dataToProcess[idx, 3]) - strtoi(dataToProcess[idx, 2])
  if (duration < slot(acc, dataToProcess[idx, 1]) || slot(acc, dataToProcess[idx, 1]) == 0) {
    slot(acc, dataToProcess[idx, 1]) = duration
  }
  return(acc)
}

meanReducer <- function(acc, idx) {
  duration = strtoi(dataToProcess[idx, 3]) - strtoi(dataToProcess[idx, 2])
  slot(acc, dataToProcess[idx, 1]) = slot(acc, dataToProcess[idx, 1]) + duration
  if (idx == nrow(dataToProcess)) {
    acc@find = acc@find / sum(dataToProcess$operationtype == "find")
    acc@delete = acc@delete / sum(dataToProcess$operationtype == "delete")
    acc@update = acc@update / sum(dataToProcess$operationtype == "update")
  }
  return(acc)
}

medReducer <- function(acc, idx) {
  duration = strtoi(dataToProcess[idx, 3]) - strtoi(dataToProcess[idx, 2])
  slot(acc, dataToProcess[idx, 1]) = append(slot(acc, dataToProcess[idx, 1]), duration)
  if (idx == nrow(dataToProcess)) {
    newAcc = initDuration
    newAcc@find = median(sort(unlist(acc@find)))
    newAcc@delete = median(sort(unlist(acc@delete)))
    newAcc@update = median(sort(unlist(acc@update)))
    return(newAcc)
  }
  return(acc)
}

print("======================= MAX DURATION =======================================")
print(Reduce( maxReducer, 1:nrow(dataToProcess), initDuration))
print("======================= MIN DURATION =======================================")
print(Reduce( minReducer, 1:nrow(dataToProcess), initDuration))
print("======================= MEAN DURATION =======================================")
print(Reduce( meanReducer, 1:nrow(dataToProcess), initDuration))
print("======================= MED DURATION =======================================")
print(Reduce( medReducer, 1:nrow(dataToProcess), initMedDuration))
