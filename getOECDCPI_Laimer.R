# getUnctadFdiData()
# Downloads OECD FDI Data Flow
#
# Iput:
# startPeriod: First period (year) to request, e.g. 2013.
# endPeriod: Last period (year) to request, e.g. 2023.
# refArea: NULL or "ALL" = all reporting countries; otherwise vector of ISO3 codes, e.g. c("AUS","AUT","DEU").
# measure: NULL or "ALL" = all; T_FA_F5A = equity (excl. reinvested earnings); T_FA_F5B = reinvested earnings; T_FA_FL = intercompany debt; T_FA_F = total FDI financial flows.
# principle: NULL or "ALL" = all; DI = inward FDI; DO = outward FDI.
# accountingEntity: NULL or "ALL" = all entities; otherwise vector, e.g. c("RSP","ROU").
# entityType: NULL or "ALL" = all resident units, "RSP"= resident SPEs only, "ROU"= resident operating units (non-SPEs)
# counterparty: NULL or "ALL" = all partner countries/groups; otherwise vector of partner codes.
# currency: Currency code: "USD_EXC" US dollars.
# flowItem: Flow item, usually "NET_FDI" (net FDI flows).
# freq: Frequency of data, e.g. "A" = annual.
# verbose: TRUE/FALSE; if TRUE, prints the constructed request URL.

# Output:
# - data.table with the selected inputs

# author: Kujtim Avdiu

### there is also an package for OECD data to download in R

getOECDFdiDataFlow <- function(startPeriod=2013,endPeriod=2023,refArea= NULL,measure='T_FA_F',principle=NULL,accountingEntity=NULL,entityType='IMC',counterparty=NULL,currency='USD_EXC',flowItem='NET_FDI',freq='A',verbose=TRUE) {
  
  refArea    <- buildDimOecd(refArea)
  measure     <- buildDimOecd(measure)
  principle   <- buildDimOecd(principle)
  accountingEntity  <- buildDimOecd(accountingEntity)
  entityType <- buildDimOecd(entityType)
  counterparty <- buildDimOecd(counterparty)
  
  # OECD Dataflow: FDI flows by counterpart area, BMD4 :contentReference[oaicite:0]{index=0}
  url <- "https://sdmx.oecd.org/public/rest/data/OECD.DAF.INV,DSD_FDI@DF_FDI_FLOW_CTRY,1.0/"
  key <- paste(refArea,measure,currency,principle,flowItem,accountingEntity,"","",counterparty,entityType,"",freq,"",sep = ".")
  query <- paste(paste0("startPeriod=", startPeriod),paste0("endPeriod=",   endPeriod),"dimensionAtObservation=AllDimensions","format=csvfilewithlabels",sep = "&")
  
  full_url <- paste0(url, key, "?", query)
  
  if (verbose) message("GET ", full_url)
  
  # liest direkt die CSV-Antwort in ein data.table
  data.table::fread(full_url)
}
