setClass("duration", slots=list(find="numeric", delete="numeric", update="numeric"))
setClass("medDuration", slots=list(findMed="numeric", deleteMed="numeric", updateMed="numeric", find="list", delete="list", update="list"))

initDuration <- new("duration",find=0, delete=0, update=0)
initMedDuration <- new("medDuration",findMed=0, deleteMed=0, updateMed=0, find=list(), delete=list(), update=list())
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
    acc@findMed = median(sort(unlist(acc@find)))
    acc@deleteMed = median(sort(unlist(acc@delete)))
    acc@updateMed = median(sort(unlist(acc@update)))
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



