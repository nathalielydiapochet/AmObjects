#' AMARETTO initialization data structure
#' @slot MA_matrix_Var describe here
#' @slot RegulatorData describe here
#' @slot RegulatorAlterations describe here
#' @slot ModuleMembership describe here
#' @slot Parameters describe here
#' @slot NrCores describe here -- should it be part of class?
setClass("AmInit", representation(  
    MA_matrix_Var="matrix",
    RegulatorData="matrix",
    RegulatorAlterations="list",
    ModuleMembership="numeric",
    Parameters="list",
    NrCores="numeric"))

#
# representation(regulatorData="data.frame")...
#

#' constructor for AmInit instances, built from list
#' @param initlist a list with obligatory elements
#' @examples
#' ini = AmInit(AmObjects::LIHC_AMARETTOinit)
#' ini
#' @export
AmInit = function(initlist) {
  reqnames = c("MA_matrix_Var", "RegulatorData", 
     "RegulatorAlterations", "ModuleMembership", 
     "Parameters", "NrCores")
  stopifnot(all(names(initlist) %in% reqnames))
  new("AmInit", 
    MA_matrix_Var=initlist$MA_matrix_Var, # NB -- check center/scale?
    RegulatorData = initlist$RegulatorData,
    RegulatorAlterations = initlist$RegulatorAlterations,
    ModuleMembership = initlist$ModuleMembership,
    Parameters = initlist$Parameters,
    NrCores = initlist$NrCores)
}

setMethod("show", "AmInit", function(object) {
  cat("AMARETTO Initialization object:\n")
  md = getSlots(getClass(class(object)))
  cat(" key components are:\n")
  dmam = dim(slot(object, "MA_matrix_Var")) 
  drd = dim(slot(object, "RegulatorData"))
  cat(sprintf("   MA_matrix_Var (%d x %d)\n",
       dmam[1], dmam[2]))
  cat(sprintf("   RegulatorData (%d x %d)\n",
       drd[1], drd[2]))
  cat("...\n") # give more hints!? alterations has substructure!
})

setClass("AmResults", representation(
      NrModules="integer",
      RegulatoryPrograms="matrix",
      AllRegulators="character",
      AllGenes="character",
      ModuleMembership="matrix",
      AutoRegulationReport="matrix",
      ModuleData="matrix",
      RegulatoryProgramData="matrix"))

#' constructor for AMARETTO results object
#' @param reslist list with obligatory components
#' @examples
#' res = AmObjects::LIHC_AMARETTOresults
#' AmResults(res)
#' @export
AmResults = function(reslist) {
  oblig = c("NrModules", "RegulatoryPrograms", 
   "AllRegulators", "AllGenes", "ModuleMembership", 
   "AutoRegulationReport", "ModuleData", "RegulatoryProgramData")
  stopifnot(all(oblig %in% names(reslist)))
  new("AmResults", 
      NrModules=reslist$NrModules,
      RegulatoryPrograms=reslist$RegulatoryPrograms,
      AllRegulators=reslist$AllRegulators,
      AllGenes=reslist$AllGenes,
      ModuleMembership=reslist$ModuleMembership,
      AutoRegulationReport=reslist$AutoRegulationReport,
      ModuleData=reslist$ModuleData,
      RegulatoryProgramData=reslist$RegulatoryProgramData)
}

setMethod("show", "AmResults", function(object) {
  cat(sprintf("AMARETTO Results object with %d modules:\n", object@NrModules))
  md = getSlots(getClass(class(object)))
  cat(" key components are:\n")
  drp = dim(slot(object, "RegulatoryPrograms")) 
  dmm = dim(slot(object, "ModuleMembership"))
  cat(sprintf("   RegulatoryPrograms (%d x %d)\n",
       drp[1], drp[2]))
  cat(sprintf("   ModuleMembership (%d x %d)\n",
       dmm[1], dmm[2]))
  cat("...\n") # give more hints!? could also give info on initialization
# if you wanted to carry that into the results
})
