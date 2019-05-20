#
library(tidyverse)
library(glmnet)
library(corrplot)
library(gridExtra)
getFin <- function(stock){
  if ("rvest" %in% installed.packages()) {
    library(rvest)
  }else{
    install.packages("rvest")
    library(rvest)
  }
  data <- data.frame()
  for (i in 1:length(stock)) {
    tryCatch(
      {
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/financials?p=",stock[i])
        wahis.session <- html_session(url)                                
        p <-    wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        IS <- p[[1]]
        colnames(IS) <- paste(IS[1,])
        IS <- IS[-c(1,5,12,20,25),]
        names_row <- paste(IS[,1])
        IS <- IS[,-1]
        IS <- apply(IS,2,function(x){gsub(",","",x)})
        IS <- as.data.frame(apply(IS,2,as.numeric))
        rownames(IS) <- paste(names_row)
        temp1 <- IS
        RD <- apply(IS['Research Development',],1,mean)
        OR <- apply(IS['Total Revenue',],1,mean)
        NI <- apply(IS['Net Income',],1,mean)
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/balance-sheet?p=",stock[i])
        wahis.session <- html_session(url)
        p <-    wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        BS <- p[[1]]
        colnames(BS) <- BS[1,]
        BS <- BS[-c(1,2,17,28),]
        names_row <- BS[,1]
        BS <- BS[,-1] 
        BS <- apply(BS,2,function(x){gsub(",","",x)})
        BS <- as.data.frame(apply(BS,2,as.numeric))
        rownames(BS) <- paste(names_row)
        temp2 <- BS
        TA <- apply(BS['Total Assets',],1,mean)
        TE <- apply(BS['Total Stockholder Equity',],1,mean)
        Patent <- apply(BS['Intangible Assets',],1,mean)
        url <- "https://finance.yahoo.com/quote/"
        url <- paste0(url,stock[i],"/cash-flow?p=",stock[i])
        wahis.session <- html_session(url)
        p <- wahis.session %>%
          html_nodes(xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table')%>%
          html_table(fill = TRUE)
        CF <- p[[1]]
        colnames(CF) <- CF[1,]
        CF <- CF[-c(1,3,11,16),]
        names_row <- CF[,1]
        CF <- CF[,-1] 
        CF <- apply(CF,2,function(x){gsub(",","",x)})
        CF <- as.data.frame(apply(CF,2,as.numeric))
        rownames(CF) <- paste(names_row)
        temp3 <- CF
        company <- stock[i]
        temp4 <- cbind.data.frame(company,RD,OR,NI,TA,TE,Patent)
        #assign(paste0(stock[i],'.f'),value = list(IS = temp1,BS = temp2,CF = temp3),envir = parent.frame())
        #assign(stock[i],value = temp4,envir = parent.frame())
        data <- rbind(data,temp4)
      },
      error = function(cond){
        message(stock[i], "Give error ",cond)
      }
    )
  }
  return(data)
}
##############
health_LSE <- c('CTH.L', 'FCH.L', 'AKR.L', 'AOR.L', 'COG.L', 'AMS.L', 'AVO.L', 'COS.L', 'CPT.L', 'CSRT.L', 'CTEC.L', 'CREO.L',
             'DEMG.L', 'EKF.L', 'GHG.L', 'IDH.L', 'IHC.L', 'IDHC.L', 'KMK.L', 'LID.L', 'MDC.L', 'NMC.L', 'ODX.L', 'PYC.L',
             'RLM.L', 'SDI.L', 'SN.L', 'SPI.L', 'SUN.L', 'TLY.L', 'TSTL.L', 'VENN.L', 'WDC.L')
health_LSE <- getFin(health_LSE)
health_LSE <- health_LSE %>%
  mutate(Industry = 'High')
##############
parm_LSE <- c('DDDD.L', 'ABC.L', 'AGY.L', 'APH.L', 'AMYT.L', 'AGL.L', 'ANCR.L', 'ANP.L', 'AZN.L', 'AVCT.L', 'BMK.L', 'BXP.L', 'BVXP.L', 
              'BTG.L', 'C4XD.L', 'CTI.L', 'CIR.L', 'CLIN.L', 'DPH.L', 'DNL.L', 'EAH.L', 'EDEN.L', 'ERGO.L', 'ETX.L', 'EVG.L', 'FARN.L', 
              'FUM.L', 'GDR.L', 'GNS.L', 'GSK.L', 'GUN.L', 'HIK.L', 'HZD.L', 'HCM.L', 'HVO.L', 'IMM.L', 'INDV.L', 'IXI.L', 'MXCT.L', 
              'MPH.L', 'MTPH.L', 'MTFB.L', 'ONC.L', 'OPTI.L', 'OBD.L', 'OXB.L', 'PEBI.L', 'PVG.L', 'PRM.L', 'REDX.L', 'RENE.L', 'SALV.L', 
              'SAR.L', 'SCLP.L', 'STX.L', 'SHRS.L', 'SLN.L', 'SUMM.L', 'SNG.L', 'TRX.L', 'TILS.L', 'VAL.L', 'VEC.L', 'VRP.L', 'VERS.L')
parm_LSE <- getFin(parm_LSE)
parm_LSE <- parm_LSE %>%
  mutate(Industry = 'High')
###############
computer_LSE <- c('ACC.L', 'ACSO.L', 'ACT.L', 'AD4.L', 'ARC.L', 'ATQT.L', 'AVV.L', 'BSD.L', 'BGO.L', 'BST.L', 'BLTG.L', 'PRSM.L', 'BRY.L', 
                  'BBSN.L', 'CTP.L', 'CNIC.L', 'CER.L', 'CLSU.L', 'CBUY.L', 'CALL.L', 'CCC.L', 'CNS.L', 'CRW.L', 'TIDE.L', 'D4T4.L', 'DFX.L', 
                  'DSG.L', 'DOTD.L', 'EYE.L', 'EPO.L', 'ECK.L' , 'ECSC.L', 'ELCO.L', 'EMIS.L', 'ESG.L', 'EUSP.L', 'FDM.L', 'FDP.L', 'FBT.L', 
                  'GBG.L', 'GHT.L', 'IDEA.L', 'IDOX.L', 'IMO.L', 'IND.L', 'ING.L', 'INS.L', 'IGP.L', 'IBM.L', 'IOM.L', 'KBT.L', 'KNOS.L', 'MCRO.L', 
                  'MCGN.L', 'MPM.L', 'NASA.L', 'NCC.L', 'NET.L', 'OMG.L', 'OSI.L', 'PTY.L', 'PEN.L', 'PHD.L', 'QTX.L', 'RCN.L', 'RTHM.L', 'RM.L', 
                  'RDT.L', 'SGE.L', 'SND.L', 'SSY.L', 'SDL.L', 'SIM.L', 'SCT.L', 'SPE.L', 'SOPH.L', 'STAR.L', 'SOG.L', 'STL.L', 'SYS.L', 'TAX.L', 
                  'TECH.L', 'TEK.L', 'TERN.L', 'TRCS.L', 'TRD.L', 'TRB.L', 'USY.L', 'WAND.L', 'WTG.L', 'ZOO.L')
computer_LSE <- getFin(computer_LSE)
computer_LSE <- computer_LSE %>%
  mutate(Industry = 'High')
################
tech_LSE <- c('AMO.L', 'BVC.L', 'CML.L', 'CNC.L', 'CYAN.L', 'FTC.L', 'FST.L', 'IQE.L', 'MWE.L', 'NANO.L', 'NAR.L', 'QXT.L', 'SEE.L', 'SPT.L', 
             'SRT.L', 'TCM.L', 'TOOP.L', 'TST.L', 'TRAK.L', 'UKOG.L')
tech_LSE <- getFin(tech_LSE)
tech_LSE <- tech_LSE %>%
  mutate(Industry = 'High')
#################
construction_LSE <- c('INSP.L', 'AXS.L', 'ALU.L', 'ASH.L', 'AUK.L', 'BBY.L', 'BILN.L', 'BOOT.L', 'BREE.L', 'CTO.L', 'COST.L', 'CRH.L', 'EPWN.L', 'ECEL.L', 
                      'FORT.L', 'FOX.L', 'HSM.L', 'IBST.L', 'JHD.L', 'KLR.L', 'KIE.L', 'KGP.L', 'LWB.L', 'MSLH.L', 'MRO.L', 'MBH.L', 'MGNS.L', 'MOGP.L', 'NXR.L', 
                      'PLP.L', 'COD.L', 'SRC.L', 'SMJ.L', 'STCM.L', 'TON.L', 'TYMN.L', 'VANL.L', 'FAN.L')
construction_LSE <- getFin(construction_LSE)
construction_LSE <- construction_LSE %>%
  mutate(Industry = 'Low')
#################
food_LSE <- c('DFIB.L', 'GRG.L', 'JARB.L', 'MCLS.L', 'MRW.L', 'OBT.L', 'OCDO.L', 'SBRY.L', 'SSPG.L', 'TSCO.L', 'TOT.L', 'UDG.L')
food_LSE <- getFin(food_LSE)
food_LSE <- food_LSE %>%
  mutate(Industry = 'Low')
#################
trans_LSE <- c('AA4.L', 'AVAP.L', 'BBA.L', 'BMS.L', ' CKN.L', 'CLG.L', 'DNA3.L', 'FSJ.L', 'GMAA.L', 'GPH.L', 'MPL.L', 'OCN.L', 'STOB.L', 'SUH.L', 'WIN.L')
trans_LSE <- getFin(trans_LSE)
trans_LSE <- trans_LSE %>%
  mutate(Industry = 'Low')
##################
real_LSE <- c('ADA.L', 'ARTL.L', 'BCRE.L', 'BLV.L', 'CNN.L', 'CAPC.L', 'CAL.L', 'CDFF.L', 'CRC.L', 'WSP.L',
              'UTG.L', 'UKCM.L', 'SLI.L', 'SMP.L', 'SRE.L', 'SPDI.L', 'SREI.L', 'SVS.L', 'SAFE.L', 'SAF.L', 'PURP.L', 'PLAZ.L', 'PIRI.L', 'PCTN.L', 'PNS.L', 
              'PCA.L', 'MTVW.L', 'MPO.L', 'WINK.L', 'LXB.L', 'LSL.L', 'LAS.L', 'LOK.L', 'INLZ.L', 'INL.L', 'HUNT.L', 'HMLH.L', 'HLCL.L', 'HWG.L', 'GRI.L', 'FOXT.L', 'FLK.L', 'FCRE.L', 'FCPT.L', 'DUPD.L', 'DCI.L', 'DJAN.L', 'CWD.L', 'CIC.L')
real_LSE <- getFin(real_LSE)
real_LSE <- real_LSE %>%
  mutate(Industry = 'Low')
###################
les_LSE <- c('CCT.L', 'TUNE.L', 'FDEV.L', 'GMD.L', 'GAW.L', 'G4M.L', 'HRN.L', 'KNM.L', 'LWRF.L', 'PHTM.L', 'BC94.L', 'TND.L')
les_LSE <- getFin(les_LSE)
les_LSE <- les_LSE %>%
  mutate(Industry = 'High')
###################
air_LSE <- c('AVON.L', 'BA.L', 'BOE.L', 'CHG.L', 'COB.L', 'CHRT.L', 'DNA3.L', 'DPA.L', 'MGGT.L', 'QQ.L', 'RR.L', 'SNR.L', 'ULE.L')
air_LSE <- getFin(air_LSE)
air_LSE <- air_LSE %>%
  mutate(Industry = 'High')
###################
ee_LSE <- c('LPA.L', 'LUCE.L', 'MAW.L', 'MSYS.L', 'MGAM.L', 'OCT.L', 'OXIG.L', 'PIP.L', 'PPIX.L', 'RSW.L', 'RGP.L', 'SNT.L', 'DIA.L', 'EKT.L', 'FDBK.L', 'GHH.L', 'HLMA.L', 'HDT.L', 'IGE.L', 'JDG.L', 'SOLI.L', 'SXS.L', 'TFW.L', 'TTG.L', 'UVEL.L', 'VLX.L', 'WPHO.L', 'XAR.L', 'ZYT.L')
ee_LSE <- getFin(ee_LSE)
ee_LSE <- ee_LSE %>%
  mutate(Industry = 'High')
###################
elc_LSE <- c('AMPH.L', 'DRX.L', 'GOOD.L', 'GSH.L', 'JEL.L', 'OPG.L', 'SSE.L', 'RUR.L')
elc_LSE <- getFin(elc_LSE)
elc_LSE <- elc_LSE %>%
  mutate(Industry = 'High')
###################
mine_LSE <- c('ACA.L', 'ALBA.L', 'AXM.L', 'ALTN.L', 'XTR.L', 'WSBN.L', 'WRES.L', 'VAST.L', 'TSG.L', 'THR.L', 'THS.L', 'TYM.L', 'SLP.L', 'SRES.L', 'SOLG.L', 'SXX.L', 'SWG.L', 'SHG.L', 'SRB.L', 'SGZ.L', 'SAV.L', 'RIO.L', 'RLD.L', 'RGM.L', 'RRR.L', 'RMM.L', 'RBW.L', 'PXOG.L', 'PREM.L', 'PDZ.L', 'POLY.L', 'POG.L', 'PDL.L', 'PFP.L', 'PGD.L', 'PAF.L', 'OVB.L', 'OMI.L', 'ORM.L', 'NCCL.L', 'MKA.L', 'MTL.L', 'LMI.L', 'LND.L', 'KOD.L', 'KIBO.L', 'KMR.L', 'KEFI.L', 'KAZ.L', 'KDR.L', 'HUM.L', 'HZM.L', 'HOC.L', 'HGM.L', 'HMI.L', 'GFM.L', 'GGP.L', 'GWMO.L', 'GRL.L', 'GDP.L', 'GST.L', 'GLEN.L', 'GEO.L', 'GEMD.L', 'GCM.L', 'GLR.L', 'GAL.L', 'FRES.L', 'FDI.L', 'EUA.L', 'EDL.L', 'ECR.L', 'CGNR.L', 'CON.L', 'CNR.L', 'CNG.L', 'CGH.L', 'CAML.L', 'CEY.L', 'CMCL.L', 'BMN.L', 'BOD.L', 'BRD.L', 'BISI.L', 'BZT.L', 'BKY.L', 'BEM.L', 'AVM.L', 'ASO.L', 'ATYM.L', 'ARS.L', 'AAU.L', 'ANTO.L', 'APF.L', 'AAZ.L', 'AAL.L', 'AYM.L', 'AMC.L')
mine_LSE <- getFin(mine_LSE)
mine_LSE <- mine_LSE %>%
  mutate(Industry = 'Low')
###################
oil_LSE <- c('88E.L', 'AMER.L', 'AEX.L', 'ANGS.L', 'ZOL.L', 'ZEN.L', 'WTE.L', 'WEN.L', 'VGAS.L', 'VOG.L', 'UJO.L', 'TLW.L', 'TRIN.L', 'TRP.L', 'TOM.L', 'TLOU.L', 'SEY.L', 'SRO.L', 'SOU.L', 'SOLO.L', 'SIA.L', '0QHQ.L', 'SQZ.L', 'SEPL.L', 'SDX.L', 'SAVP.L', 'SLE.L', 'RDSB.L', 'ROSE.L', 'RKH.L', 'RPT.L', 'RMP.L', 'QFI.L', 'PVR.L', 'PPC.L', 'PMO.L', 'PTR.L', 'MATD.L', 'PET.L', 'PMG.L', 'PANR.L', 'OPHR.L', 'OEX.L', 'NUGO.L', 'NOG.L', 'NTOG.L', 'MXO.L', 'MSMN.L', 'MYN.L', 'LKOH.L', 'LEK.L', 'LOGP.L', 'JKX.L', 'JOG.L', 'IOF.L', 'INDI.L', 'IOG.L', 'IGAS.L', 'HUR.L', 'HDY.L', 'GKP.L', 'GEEC.L', 'GBP.L', 'FOG.L', 'EXI.L', 'EOG.L', 'ENQ.L', 'EME.L', 'ELA.L', 'EDR.L', 'CLON.L', 'CHAR.L', 'COPL.L', 'CNE.L', 'CAD.L', 'BLVN.L', 'BOR.L', 'BOIL.L', 'BPC.L', 'AST.L', 'ARG.L')
oil_LSE <- getFin(oil_LSE)
oil_LSE <- oil_LSE %>%
  mutate(Industry = 'Low')
###################
noinsure_LSE <- c('ADM.L', 'BEZ.L', 'DLG.L', 'GACB.L', 'HSTG.L', 'HUW.L', 'HSX.L', 'JLT.L', 'LRE.L', 'MHM.L', 'PGH.L', 'RQIH.L', 'RSA.L')
noinsure_LSE <- getFin(noinsure_LSE)
noinsure_LSE <- noinsure_LSE %>%
  mutate(Industry = 'Low')
###################
insure_LSE <- c('AV.L', 'CSN.L', 'HSD.L', 'LGEN.L', 'OMU.L', 'IL0A.L', 'PHNX.L', 'PRU.L', 'SFI.L', 'STJ.L', 'SLA.L')
insure_LSE <- getFin(insure_LSE)
insure_LSE <- insure_LSE %>%
  mutate(Industry = 'Low')
###################
tel_LSE <- c('AVN.L', 'EVRH.L', 'GAMA.L', 'ISAT.L', 'MOS.L', 'MBT.L', 'VOD.L')
tel_LSE <- getFin(tel_LSE)
tel_LSE <- tel_LSE %>%
  mutate(Industry = 'High')
###################
fore_LSE <- c('TREE.L', 'CRPR.L', 'MNDI.L')
fore_LSE <- getFin(fore_LSE)
fore_LSE <- fore_LSE %>%
  mutate(Industry = 'Low')
###################
gas_LSE <- c('ACP.L', 'CAN.L', 'FCRM.L', 'INFA.L', 'MWG.L', 'NG.L', 'PNN.L', 'SVT.L', 'UU.L', 'YU.L')
gas_LSE <- getFin(gas_LSE)
gas_LSE <- gas_LSE %>%
  mutate(Industry = 'Low')
###################
high_LSE <- c('AUTG.L', 'SCE.L', 'TYT.L', 'TRT.L', 
              'AGM.L', 'BIOM.L', 'BYOT.L', 'CAR.L', 'CRDA.L', 'DCTA.L', 'ELM.L', 'HDD.L', 'HAYD.L', 'JMAT.L', 'PHC.L', 'SCPA.L', 'SYNT.L', 'TET.L', 'VRS.L', 'VCT.L', 'ZTF.L',
              'APC.L', 'CPX.L', 'DWHA.L', 'DIA.L', 'EKT.L', 'FDBK.L', 'GHH.L', 'HLMA.L', 'HDT.L', 'IGE.L', 'JDG.L', 'LPA.L', 'LUCE.L', 'MAW.L', 'MSYS.L', 'MGAM.L', 'OCT.L', 'OXIG.L', 'PIP.L', 'PPIX.L', 'RSW.L', 'RGP.L', 'SNT.L', 'SOLI.L', 'SXS.L', 'TFW.L', 'TTG.L', 'UVEL.L', 'VLX.L', 'WPHO.L', 'XAR.L', 'XPP.L', 'ZYT.L', 'BAGR.L', 'BRBY.L', 'CRL.L', 'IDP.L', 'MUL.L', 'PTD.L', 'PZC.L', 'SWL.L', 'TED.L', 'W7L.L')
high_LSE <- getFin(high_LSE)
high_LSE <- high_LSE %>%
  mutate(Industry = 'High')
###################
low_LSE <- c('AEG.L', 'AFC.L', 'ADL.L', 'SAE.L', 'CWR.L', 'CNEL.L', 'HYR.L', 'IKA.L', 'ITM.L', 'PHE.L', 'PPS.L', 'PVCS.L', 'SO4.L', 
             'BAG.L', 'BVIC.L', 'CCR.L', 'CCH.L', 'DGE.L', 'DIS.L', 'FEVR.L', 'GUS.L', 'NICL.L', 'STCK.L', 
             'ADT.L', 'BT-A.L', 'KCOM.L', 'MANX.L', 'TEP.L', 'TDE.L', 'TCN.L',  
             'AGTA.L', 'AEP.L', 'ABF.L', 'CAM.L', 'CARR.L', 'CWK.L', 'DCG.L', 'DKL.L', 'DVO.L', 'PAL.L', 'FIF.L', 'GLB.L', 'GNC.L', 'HFG.L', 'HOTC.L', 'KYGA.L', 'MPE.L', 'OGN.L', 'PFD.L', 'PXS.L', 'PURE.L', 'RE.L', 'RGD.L', 'SIS.L', 'TATE.L', 'UKR.L', 'ULVR.L', 'VLG.L', 'WYN.L', 'ZAM.L',
             'AO.L', 'APGN.L', 'ALY.L', 'ASC.L', 'AUTO.L', 'BME.L', 'BON.L', 'BOO.L', 'BWNG.L', 'CFYN.L', 'CAMB.L', 'CARD.L', 'CPR.L', 'CVSG.L', 'DEB.L', 'DFS.L', 'DTY.L', 'DC.L', 'DNLM.L', 'FDL.L', 'FRAN.L', 'FCCN.L', 'HFD.L', 'INCH.L', 'JD.L', 'JOUL.L', 'JE.L', 'KGF.L', 'KOOV.L', 'LOOK.L', 'WINE.L', 'MMH.L', 'MOSB.L', 'MTC.L', 'MOTR.L', 'MYSL.L', 'NTLG.L', 'PDG.L', 'PETS.L', 'SFE.L', 'SAGA.L', 'SCHO.L', 'SCS.L', 'SHOE.L', 'SGI.L', 'TPT.L', 'UCG.L', 'VTU.L', 'SMWH.L', 
             'MYX.L', 'PFC.L', 'POS.L', 'SCL.L', 'THAL.L', 'VLS.L', 'WG.L', 'LAM.L', 'HTG.L', 'GMS.L', 'GTC.L', 'NTQ.L', 
             'BATS.L', 'IMB.L','FOUR.L', 'AEO.L', 'ASCL.L', 'BOOM.L', 'BHRD.L', 'BMY.L', 'CMX.L', 'CLTV.L', 'CAU.L', 'DMGT.L', 'DCD.L', 'DODS.L', 'EBQ.L', 'ETO.L', 'ERM.L', 'FUTR.L', 'GFIN.L', 'DATA.L', 'GOCO.L', 'HYNS.L', 'HNT.L', 'IME.L', 'INM.L', 'INF.L', 'ITE.L', 'ITV.L', 'JWNG.L', 'SAA.L', 'MTMY.L', 'MDZ.L', 'MMX.L', 'MIRA.L', 'TMMG.L', 'MONY.L', 'NAH.L', 'NFC.L', 'OMIP.L', 'PSON.L', 'PRIM.L', 'QRT.L', 'RBD.L', 'R4E.L', 'REL.L', 'RMV.L', 'SAL.L', 'STVG.L', 'TAP.L', 'TRS.L', 'TMO.L', 'TLA.L', 'ULS.L', 'VELA.L', 'WIL.L', 'WPP.L', 'XLM.L', 'YOU.L', 'ZIN.L')
low_LSE <- getFin(low_LSE)
low_LSE <- low_LSE %>%
  mutate(Industry = 'Low')
###################
LSE <- rbind(health_LSE,parm_LSE,computer_LSE,
             tech_LSE,construction_LSE,real_LSE,
             food_LSE,trans_LSE,les_LSE,air_LSE,
             ee_LSE,elc_LSE,mine_LSE,oil_LSE,
             noinsure_LSE,insure_LSE,tel_LSE,
             fore_LSE,gas_LSE,high_LSE,low_LSE)
LSE <- LSE %>%
  mutate(Frequency=0)
####################
health_FSE <- c('AAQ.F', 'AFX.F', 'DRW8.F', 'DRW3.F', 'EUZ.F', 'FME.F', 'FRE.F', 'GME.F', 'GXI.F', 'MED.F', 'RHK.F', 'SHL.F', 'SBS.F', 'V3V.F', 'AGX.F', 'CNWK.F', 'CUR.F', 'ELN.F', 'MAK.F')
health_FSE <- getFin(health_FSE)
health_FSE <- health_FSE %>%
  mutate(Industry = 'High')
####################
parm_FSE <- c('BIO.F', 'BIO3.F', 'DMP.F', 'MRK.F', 'BBZA.F', 'B8F.F', 'ECX.F', 'EVT.F', 'EXN.F', 'WL6.F', 'MDG1.F', 'MGNK.F', 'MOR.F', 'PA8.F', 'QIA.F', 'SRT.F', 'SRT3.F', 'VSC.F', 'ILM1.F')
parm_FSE <- getFin(parm_FSE)
parm_FSE <- parm_FSE %>%
  mutate(Industry = 'High')
####################
computer_FSE <- c('A1OS.F', 'AOF.F', 'BC8.F', 'COK.F', 'CSH.F', 'COP.F', 'FAA.F', 'FBEN.F', 'GFT.F', 'GKS.F', 'HOC.F', 'ISH2.F', 'IVX.F', 'ISR.F', 'IVU.F', 'KSC.F', 'M3V.F', 'NEM.F', 'NXU.F', 'PSAN.F', 'RIB.F', 'RKET.F', 'SAP.F', 'YSN.F', 'SJJ.F', 'SHF.F', 'SYT.F', 'SOW.F', 'SYZ.F', 'TLI.F', 'OSP2.F', 'UTDI.F', 'WDI.F', 'O1BC.F', 'ADN1.F', 'AEIN.F', 'DTD2.F', 'WIN.F', 'ESY.F', 'LCY.F', 'MYRK.F', 'FCT.F', 'NC5A.F', 'O5H.F', 'RTC.F', 'SZZ.F', 'SVAB.F', 'ARX.F', 'TIS.F')
computer_FSE <- getFin(computer_FSE)
computer_FSE <- computer_FSE %>%
  mutate(Industry = 'High')
################
tech_FSE <- c('ADV.F', 'AIXA.F', 'DAM.F', 'DLG.F', 'DBD.F', 'ELG.F', 'EUCA.F', 'EXC.F', 'SIS.F', 'FEV.F', 'GGS.F', 'IFX.F', 'IXX.F', 'IS7.F', 'OHB.F', 'QSC.F', 'SANT.F', 'WAF.F', 'SMHN.F ', 'TXA.F', 'MUS.F', 'HWSA.F', 'OMR.F', 'ICP.F', 'SCE.F', 'VG0K.F', 'WZM.F')
tech_FSE <- getFin(tech_FSE)
tech_FSE <- tech_FSE %>%
  mutate(Industry = 'High')
#################
construction_FSE <- c('B5A.F', 'HEI.F', 'HOT.F', 'FRO.F', 'FRO3.F', 'KWG.F', 'HOZ.F', 'TUF.F', 'UZU.F', 'WTB.F', 'WTB3.F')
construction_FSE <- getFin(construction_FSE)
construction_FSE <- construction_FSE %>%
  mutate(Industry = 'Low')
#################
food_FSE <- c('SZU.F', 'HAW.F', 'BEZ.F', 'SWA.F', 'MSH.F', 'MFL1.F')
food_FSE <- getFin(food_FSE)
food_FSE <- food_FSE %>%
  mutate(Industry = 'Low')
#################
trans_FSE <- c('AVES.F', 'FRA.F', 'HHFA.F', 'SIX2.F', 'SIX3.F')
trans_FSE <- getFin(trans_FSE)
trans_FSE <- trans_FSE %>%
  mutate(Industry = 'Low')
##################
real_FSE <- c('A4Y.F', 'ADL.F', 'ADJ.F', 'AOX.F', 'AT1.F', 'CCAP.F', 'DMRE.F', 'DEQ.F', 'JB7.F', 'DWNI.F', 'DIC.F', 'DKG.F', 'GWD.F', 'GYC.F', 'HAB.F', 'INS.F', 'LEG.F', 'PAT.F', 'TEG.F', 'TLG.F', 'VNA.F', 'AAA.F', 'O5G.F', 'DRE2.F ', 'ERWE.F', 'BNT1.F', 'FVI.F', 'GIB.F', 'G7B.F', 'TTO.F', 'UPR.F', 'WCMK.F', 'YMO.F')
real_FSE <- getFin(real_FSE)
real_FSE <- real_FSE %>%
  mutate(Industry = 'Low')
###################
high_FSE <- c('ASL.F', 'BMW3.F', 'BMW.F', 'BDT.F', 'CON.F', 'DAI.F', 'ED4.F', 'ZIL2.F', 'GMM.F', 'HLE.F', 'JST.F', 'LEO.F', 'PGN.F', 'PWO.F', 'SHA.F', 'SF3.F', 'VOW.F', 'VOW3.F', 'NSU.F', 'BKS3.F', 'JJO.F', 'PAH3.F', 'SW1.F','SFP1.F', 'BNN.F', 'BAS.F', 'BAYN.F', '1COV.F', 'EVK.F', 'FPE3.F', 'FPE.F', '2HRA.F', 'SDF.F', 'LXS.F', 'LIN.F', 'SGL.F', 'SY1.F', 'WCH.F', 'CHX.F', '333.F', 'SIM.F',       
              'A4Y.F', 'ADL.F', 'ADJ.F', 'AOX.F', 'AT1.F', 'CPX.F', 'COM.F', 'CCAP.F', 'CSQ.F', 'DMRE.F', 'DB1.F', 'DEQ.F', 'JB7.F', 'DWNI.F', 'DIC.F', 'DBAN.F', 'DKG.F', 'DWS.F', 'CAP.F', 'FRU.F', 'GWD.F', 'GYC.F', 'GLJ.F', 'HAB.F', 'HYQ.F', 'INS.F', 'IUR.F', 'LEG.F', 'MLP.F', 'O4B.F', 'PAT.F', 'LNSX.F', 'TEG.F', 'TLG.F', 'VNA.F',
              'AAA.F', 'ALG.F', 'O5G.F', 'DFTK.F', 'FAM1.F', 'DRE2.F', 'ERWE.F', 'BNT1.F', 'FVI.F', 'FRS.F', 'FNG.F', 'GIB.F', 'G7B.F', 'IPOA.F', 'RTML.F', 'MPRK.F', 'KCC.F', 'MSAG.F', '02P.F', 'PUZ.F', 'SBE.F', 'TTO.F', 'UPR.F', 'VMR1.F', 'VDN.F', 'WCMK.F', 'RKB.F', 'YMO.F')
high_FSE <- getFin(high_FSE)
high_FSE <- high_FSE %>%
  mutate(Industry = 'High')
low_FSE <- c('ALV.F', 'DFV.F', 'HNR1.F', 'MUV2.F', 'TLX.F', 'ART.F', 'ACX.F', 'CEC.F', 'CEC1.F', 'DHER.F', 'DEX.F', 'ELB.F', 'FIE.F', 'HAW.F', 'HFG.F', 'H24.F', 'HBH.F', 'HBM.F', 'LO24.F', 'ECK.F', 'B4B.F', 'B4B3.F', 'SAE.F', 'BTBB.F', 'TTK.F', 'ULC.F', 'WEW.F', 'WDLK.F', 'ZAL.F ', 'TIM.F', 'ZO1.F', 'BIJ.F', 'USE.F', 'XMY.F', '8S9.F', 'TVD6.F','DTE.F', 'E4C.F', 
             'DRI.F', 'TGT.F', 'FNTN.F', 'NFN.F', 'O2D.F', 'YOC.F', 'LSX.F', '5AB.F', 'HETA.F', 'YB1A.F', 'SUR.F','EIS.F', 'NDA.F', 'SZG.F','O2C.F', 'SPR.F', 'APM.F', 'BST.F', 'BVB.F', 'EV4.F', 'EVD.F', 'ERMK.F', 'HLG.F', 'PSM.F', 'RRTL.F', 'G24.F', 'SAX.F', 'TC1.F', 'ITN.F', 'KA8.F', 'ODE.F', 'HBD1.F', 'EXJ.F', 'SPM.F', 'WIG1.F', '99SC.F', 'ERO1.F', 'WBAH.F ', 'RTV.F')
low_FSE <- getFin(low_FSE)
low_FSE <- low_FSE %>%
  mutate(Industry = 'Low')
###################
FSE <- rbind(health_FSE,parm_FSE,computer_FSE,
             tech_FSE,construction_FSE,real_FSE,
             food_FSE,trans_FSE,high_FSE,low_FSE)
FSE <- FSE %>%
  mutate(Frequency=1)
####################
####################
health_BSE <- c('APOLLOHOSP.BO', 'SCANDENT.BO', 'KOVAI.BO', 'CMMHOSP.BO', 'FORTISMLR.BO', 'VIMTALABS.BO', 'KMCSHIL.BO', 'SHIVMED.BO', 'GUJMEDI.BO', 'TRABI.BO', 'ASHRAM.BO', 'MEDINOV.BO', 'DOLPHMED.BO', 'CHOKSILA.BO', 'DRAGARWQ.BO', 'BI.BO', 'SHREEPAC.BO', 'NGIND.BO', 'ZDHJERK.BO', 'KRETTOSYS.BO', 'INDRAMEDCO.BO', 'FORTIS.BO', 'LEHIL.BO', 'GKB.BO', 'SASTASUNDR.BO', 'LOOKS.BO', 'SML.BO', 'TEJNAKSH.BO', 'INDOGLOBAL.BO', 'LALPATHLAB.BO', 'NH.BO', 'HCG.BO', 'THYROCARE.BO', 'KMSMEDI.BO', 'ARTEMIS.BO', 'ANG.BO', 'LASA.BO', 'SHALBY.BO', 'KMSL.BO', 'ASTERDM.BO', 'DLCL.BO', 'DECCAN.BO', 'METROPOLIS.BO')
health_BSE <- getFin(health_BSE)
health_BSE <- health_BSE %>%
  mutate(Industry = 'High')
####################
parm_BSE <- c('AMBALALSA.BO', 'CIPLA.BO', 'DRREDDY.BO', 'MERCK.BO', 'LUPIN.BO', 'LYKALABS.BO', 'MOREPENLAB.BO', 'PEL.BO', 'TORNTPHARM.BO', 'TRANSCHEM.BO', 'ABBOTINDIA.BO', 'GLAXO.BO', 'NOVARTIND.BO', 'SANOFI.BO','PFIZER.BO', 'PARNAXLAB.BO', 'BLISSGVS.BO', 'ALEMBICLTD.BO', 'ANUHPHR.BO', 'CHEMOPH.BO', ' DIL.BO', ' UNICHEMLAB.BO', ' ASTRAZEN.BO', ' GUJTHEM.BO', 'MAKERSL.BO', ' JBCHEPHARM.BO', ' TTKHEALTH.BO', ' JAGSNPHARM.BO', ' GUFICBIO.BO', 'VEERHEALTH.BO', 'TRIPR.BO', 'SEQUENT.BO', 'MEDICAPQ.BO', ' PHRMASI.BO', 'ALBERTDA.BO', 'IOLCP.BO', ' WANBURY.BO', ' RESONANCE.BO', ' KOPRAN.BO', ' GUJTERC.BO', 'KABRADG.BO', 'AARTIDRUGS.BO', 'ORCHIDPHAR.BO', 'BIOFILCHEM.BO', 'ISHITADR.BO', 'MARKSANS.BO', 'AREYDRG.BO', 'SYNCOMF.BO', 'IPCALAB.BO', ' KILITCH.BO', 'CORALAB.BO', ' BACPHAR.BO', 'KREBSBIO.BO', 'SHABCHM.BO', 'NEULANDLAB.BO', 'KAMRLAB.BO', 'BERLDRG.BO', 'SHUKRAPHAR.BO', 'INDSWFTLTD.BO', 'NATCAPSUQ.BO', 'WELCURE.BO', 'BIBCL.BO', 'HESTERBIO.BO', 'PARENTLD.BO', 'SANDUPHQ.BO', 'VISTAPH.BO', 'SUNPHARMA.BO', ' SDL.BO', 'JENBURPH.BO', 'HIKAL.BO', 'CAPPL.BO', ' COMBDRG.BO', ' WINTAC.BO', 'NGLFINE.BO', 'EVERESTO.BO', ' VARDHCH.BO', ' AUROPHARMA.BO', 'NATCOPHARM-.BO', ' BALPHARMA.BO', 'BDH.BO', 'VINRKLB.BO', 'MAXIMAA.BO', 'VENUSREM.BO', 'JUBILANT.BO', 'VIVANZA.BO', 
              'SAMRATPH.BO', 'KERALAYUR.BO', 'THEMISMED.BO', ' BRAWN.BO', ' AUROLAB.BO', ' SUVEN.BO', 'KIMIABL.BO', 'GODAVARI.BO', 'VIKRAMTH.BO', 'SHILPAMED.BO', 'ZENITHHE.BO', 'TRANSASIA.BO', 'ROOPAIND.BO', ' RUBRAME.BO', ' MEDICAMEQ.BO', 'SYSCHEM.BO', ' RAYLA.BO', ' COLINZ.BO', 'ZDHJERK.BO', 'KRETTOSYS.BO', 'INDRAMEDCO.BO', 'FORTIS.BO', 'LEHIL.BO', 'GKB.BO', 'SHYAMAINFO.BO', 'SOURCENTRL.BO', 'DESHRAK.BO', ' SANJIVIN.BO', 'FDC.BO', 'LINCOPH.BO', 'SWORDNSH.BO', ' ADVIKLA.BO', ' PANCHSHEEL.BO', ' GENNEX.BO', ' UNJHAFOR.BO', 'MADHUVEER.BO', 'SENBO.BO', 'ZENOTECH.BO', 'GLENMARK.BO', 'WOCKPHARMA.BO', 'INDSWFTLAB.BO', 'CADILAHC.BO', ' AJANTPHARM.BO', 'TYCHE.BO', 'GRANULES.BO', 'DIVISLAB.BO', 'STAR.BO', 'INDOCO.BO', 'MANGALAM.BO', 'NECLIFE.BO', 'VIVIMEDLAB.BO',
              'SMSPHARMA.BO', 'SPARC.BO', ' PIRPHYTO.BO', ' RPGLIFE.BO', 'BAFNAPHARM.BO', ' SYNCOM.BO', 'PARABDRUGS.BO', ' BROOKS.BO', 'APLLTD.BO', 'FERVENTSYN.BO', 'SUNLOC.BO', 'DENISCHEM.BO', 'VRL.BO', ' MERCURYLAB.BO', 'CONCORD.BO', 'SYNGENE.BO', 'ORTINLAABS.BO', 'ALKEM.BO', 'GANGAPHARM.BO', 'FREDUN.BO', ' BAJAJHCARE.BO', ' KPL.BO', ' VALIANTORG.BO', 'LAURUSLABS.BO', ' ARMAX.BO', 'ERIS.BO', ' SMSLIFE.BO', 'SMRUTHIORG.BO', 'DCAL.BO', 'VANTABIO.BO', 'SGRL.BO', 'MEDICO.BO', 'BPLPHARMA.BO', 'ZIMLAB.BO', 'SOLARA.BO', 'RAJNISH.BO', 'AMRUTANJAN.BO')
parm_BSE <- getFin(parm_BSE)
parm_BSE <- parm_BSE %>%
  mutate(Industry = 'High')
####################
computer_BSE <- c('ROLTA.BO', 'UNITEDINT.BO', 'SYLPH.BO', 'CNIRESLTD.BO', '8KMILES.BO', 'JATALIA.BO', 'SCANPGEOM.BO', 'SKUMAR.BO', 'VEDAVAAG.BO', 'JSTL.BO', 'MEGRISOFT.BO', 'INFIBEAM.BO', 'LTTS.BO', 'OCTAWARE.BO', 'MATRIMONY.BO', 'XELPMOC.BO')
computer_BSE <- getFin(computer_BSE)
computer_BSE <- computer_BSE %>%
  mutate(Industry = 'High')
################
tech_BSE <- c('HCL-INSYS.BO', 'COMPUPN.BO', 'PCS.BO', 'VINTRON.BO', 'KDL.BO', 'CRAZYINF.BO', 'CEREBRAINT.BO', 'ACIASIA.BO')
tech_BSE <- getFin(tech_BSE)
tech_BSE <- tech_BSE %>%
  mutate(Industry = 'High')
#################
construction_BSE <- c('LERTHAI.BO', 'TRIVENIGQ.BO', 'NILACHAL.BO', 'SASHWAT.BO', 'NIDHGRN.BO', 'GLITTEKG.BO', 'SOLIDSTON.BO', 'HIMGRANI.BO', 'AROGRANITE.BO', 'CEETAIN.BO', 'SRIVAJRA.BO', 'MADHAV.BO', 'VERTICLIND.BO', 'JAIMATAG.BO', 'PACIFICI.BO', 'GUJBOROS.BO', 'RALEGRA.BO', 'DIVSHKT.BO', 'MIDWEST.BO', 'NEELKAN.BO', 'INANI.BO', 'MAYURFL.BO', 'PGINDST.BO', 'MILESTONE.BO', 'RAMCOIND.BO', 'POKARNA.BO', 'ORIENTALTL.BO', 'JAINMARMO.BO', 'SHIVAEXPO.BO', 'RAWEDGE.BO')
construction_BSE <- getFin(construction_BSE)
construction_BSE <- construction_BSE %>%
  mutate(Industry = 'Low')
#################
food_BSE <- c('APIS.BO', 'KFBL.BO', 'FOODSIN.BO', 'KLRFM.BO', 'AVANTI.BO', 'NHCFOODS.BO', 'CHORDIA.BO', 'WATERBASE.BO', 'LACTOSE.BO', 'TIRUSTA.BO', 'INDXTRA.BO', 'SABOOSOD.BO', 'OVOBELE.BO', 'PROGRESV.BO', 'TRICOMFRU.BO', 'SPECFOOD.BO', 'SKMEGGPROD.BO', 'SITASHREE.BO', 'MEGASTAR.BO', 'OBIL.BO', 'ABHIJIT.BO', 'ACEWIN.BO', 'ZEAL.BO', 'OCEANIC.BO')
food_BSE <- getFin(food_BSE)
food_BSE <- food_BSE %>%
  mutate(Industry = 'Low')
#################
trans_BSE <- c('TIL.BO', 'SANCTRN.BO', 'SANGHVIMOV.BO', 'CONCOR.BO', 'KERNEX.BO', 'ACE.BO', ' NAVKARCORP.BO', ' JITFINFRA.BO')
trans_BSE <- getFin(trans_BSE)
trans_BSE <- trans_BSE %>%
  mutate(Industry = 'Low')
##################
real_BSE <- c('GROVY.BO')
real_BSE <- getFin(real_BSE)
real_BSE <- real_BSE %>%
  mutate(Industry = 'Low')
###################
bio_BSE <- c('VIVOBIOT.BO', 'GENESIS.BO', 'PANACEABIO.BO', 'BIOCON.BO', 'CELESTIAL.BO', 'ALPA.BO', 'CAMSONBIO.BO', 'GVBL.BO', 'SHREEGANES.BO')
bio_BSE <- getFin(bio_BSE)
bio_BSE <- bio_BSE %>%
  mutate(Industry = 'High')
##################
aero_BSE <- c('TANAA.BO', 'HAL.BO')
aero_BSE <- getFin(aero_BSE)
aero_BSE <- aero_BSE %>%
  mutate(Industry = 'High')
##################
tel_BSE <- c('PUNJCOMMU.BO', 'SPICEMOBI.BO', 'SHYAMTEL.BO', 'ADCINDIA.BO', 'ITI.BO', 'VALIANT.BO', 'OPTIEMUS.BO', 'MRO-TEK.BO', 'AVANTEL.BO', 'GTLINFRA.BO', 'XLENERGY.BO', 'AISHWARYA.BO', 'KAVVERITEL.BO')
tel_BSE <- getFin(tel_BSE)
tel_BSE <- tel_BSE %>%
  mutate(Industry = 'High')
##################
aul_BSE <- c('CENTEXT.BO', 'HINDALCO.BO', 'UNIVPRIM.BO', 'BHRKALM.BO', 'SYNTHFO.BO', 'GOLKONDA.BO', 'MAITRI.BO', 'PGFOILQ.BO', 'ALICON.BO', 'GUJFOIL.BO', ' SACHEMT.BO', 'HINDALUMI.BO', 'NATIONALUM.BO', 'BMAL.BO', 'MNKALCOLTD.BO', 'PALCO.BO', 'ARFIN.BO')
aul_BSE <- getFin(aul_BSE)
aul_BSE <- aul_BSE %>%
  mutate(Industry = 'Low')
##################
stl_BSE <- c('UNIABEXAL.BO', 'BOMBWIR.BO', 'TAPARIA.BO', 'LAKPRE.BO', 'ANUPMAL.BO', 'GANDHITUBE.BO', 'REMIEDEL.BO', 'TRANSFRE.BO', 'ACROW.BO', 'PREMPIPES.BO', 'VALLABHSQ.BO', 'KANSHST.BO', 'MAHALXSE.BO', 'SSWRL.BO', 'GUJCONT.BO', 'TNSTLTU.BO', 'MAHASTEEL.BO', 'SRIPIPES.BO', 'USHAMART.BO', 'MUKATPIP.BO', 'ELECTHERM.BO', 'CRIMSON.BO', 'SHBAJRG.BO', 'NILE.BO', 'RAJTUBE.BO', 'GOODLUCK.BO', 'NATFIT.BO', 'SURAJLTD.BO', 'METALCO.BO', 'GOPAIST.BO', 'MSPL.BO', 'TIIL.BO', 'RATHIBAR.BO', 'SPSL.BO', 'BEDMUTHA.BO', 'APLAPOLLO.BO', 'AIML.BO', 'VSSL.BO', 'JTLINFRA.BO', 'BRPL.BO', 'UNISON.BO', 'PACT.BO', 'RAMASTEEL.BO', 'JSLHISAR.BO', 'UMIYA.BO', 'BHARATWIRE.BO', 'RSTL.BO', 'SMLT.BO', 'SHANKARA.BO', 'MIDHANI.BO', 'SUPERSHAKT.BO')
stl_BSE <- getFin(stl_BSE)
stl_BSE <- stl_BSE %>%
  mutate(Industry = 'Low')
##################
life_BSE <- c('MFSL.BO', 'ICICIPRULI.BO', 'SBILIFE.BO', 'HDFCLIFE.BO')
life_BSE <- getFin(life_BSE)
life_BSE <- life_BSE %>%
  mutate(Industry = 'Low')
##################
mining_BSE <- c('ASIIL.BO', 'SANDUMA.BO', 'DECNGOLD.BO', 'SVCRES.BO', 'SOUTHMG.BO', 'NMDC.BO', 'ASHAPURMIN.BO', 'KACHCHH.BO', '20MICRONS.BO', 'MOIL.BO', 'ORISSAMINE.BO')
mining_BSE <- getFin(mining_BSE)
mining_BSE <- mining_BSE %>%
  mutate(Industry = 'Low')
##################
high_BSE <- c('ATUL.BO', 'RALLIS.BO', 'MODIPON.BO', 'BAYERCROP.BO', 'PUNJABCHEM.BO', ' DHANUKA.BO', 'UPL.BO', 'PIIND.BO', 'MONSANTO.BO', 'AIMCOPEST.BO', 'BHASKAGR.BO', 'NACLIND.BO', 'PHYTO.BO', 'ADARSHPL.BO', 'SUCROSA.BO', 'BHAGCHEM.BO', 'KILPEST.BO', 'EXCELCROP.BO', 'INSECTICID.BO', 'ASTEC.BO', 'JUBLINDS.BO', 'SHARDACROP.BO', 'SHIVALIK.BO', ' BHARATRAS.BO', 
              'AMARAJABAT.BO', 'AUTOLITIND.BO', 'BANCOINDIA.BO', 'EXIDEIND.BO', 'LGBBROSLTD.BO', 'SUNDRMFAST.BO', 'SWARAJENG.BO', 'UCALFUEL.BO', 'BOSCHLTD.BO', 'BGWTATO.BO', 'DUNCANENG.BO', 'CHOLAHLDNG.BO', 'AUTOAXLES.BO', 'ACGL.BO', 'SETCO.BO', 'TALBROAUTO.BO', 'ZFSTEERING.BO', 'JAINEX.BO', 'VELJAN.BO', 'BIMETAL.BO', 'BHARATGEAR.BO', 'GAJRA.BO', 'HIMTEK.BO', 'GABRIEL.BO', 'FMGOETZE.BO', 'RANEHOLDIN.BO', 'REILELEC.BO', 'HINDHARD.BO', 'TRITONV.BO', 'PANKAJPIYUS.BO', 'SIMMOND.BO', 'ISTLTD.BO', 'HINDCOMPOS.BO', 'GSAUTO.BO', 'AMFORG.BO', 'JAYUSH.BO', 'SSWL.BO', 'KALYANIFRG.BO', 'PRADPME.BO', 'SPECTRA.BO', 'JMTAUTOLTD.BO', 'ASAHIINDIA.BO', 'SUBROS.BO', 'LUMAXIND.BO', 'MOTHERSUMI.BO', 'IGARASHI.BO', 'RAJGLOWIR.BO', 'RICOAUTO.BO', 'OMAXAUTO.BO', 'MUNJALSHOW.BO', 'JAMNAAUTO.BO', 'SUNCLAYLTD.BO', 'JTEKTINDIA.BO', 'MUNJALAU.BO', 'JAYBARMARU.BO', 'RACLGEAR.BO', 'SAMKRG.BO', 'ASAL.BO', 'SIBARAUT.BO', 'SHANTIGEAR.BO', 'HITECHGEAR.BO', 'YUKEN.BO', 'FRONTSP.BO', 'RASANDIK.BO', 'BHARATSE.BO', 'IPRINGLTD.BO', 'MENONBE.BO', 'PARTIND.BO', 'JAGANLAM.BO', 'ANGIND.BO', 'STERTOOLS.BO', 'REMSONSIND.BO', 'MENNPIS.BO', 'AUTOPINS.BO', 'INDNIPPON.BO', 'CASTEXTECH.BO', 'SUPRAJIT.BO', ' MINDAIND.BO', 'JBMA.BO', 'RML.BO', 'FIEMIND.BO', 'SHIVAMAUTO.BO', 'LUMAXTECH.BO', 'AUTOIND.BO', 'PORWAL.BO', 'PPAP.BO', 'RBL.BO', 'RANEENGINE.BO', 'WABCOINDIA.BO', 'KIRLOSENG.BO', 'ENKEIWHEL.BO', 'SHARDA.BO', 'MINDACORP.BO', 'TALBROSENG.BO', 'SAPL.BO', ' UNIAUTO.BO', 'SWARAJAUTO.BO', 'PRECAM.BO', 'SAIMOH.BO', 'GNA.BO', 'PRICOLLTD.BO', 'TIINDIA.BO', 'SANDHAR.BO', 'VARROC.BO', 'KRANTI.BO', 'KPITTECH.BO', 'HARITASEAT.BO', 'SUNDRMBRAK.BO', 'WHEELS.BO', 
              'HIRECT.BO', 'DELTRON.BO', 'THAKRAL.BO', 'TECHCON.BO', ' SHBCLQ.BO', 'RIR.BO', 'SPELS.BO', 'PERVASIVE.BO', 'SWITCHTE.BO', ' DYNAVSN.BO', ' BCCFUBA.BO', ' PRECISIO.BO', ' FINELINE.BO', 'PANELEC.BO', 'LINAKS.BO', 'APTL.BO', 'KILBURN.BO', 'NETWORK.BO', 'ECORECO.BO', 'ZICOM.BO', 'INTECH.BO', 'MIC.BO', 'SURANASOL.BO', 'METSL.BO', ' HUIL.BO')
high_BSE <- getFin(high_BSE)
high_BSE <- high_BSE %>%
  mutate(Industry = 'High')
##################
low_BSE <- c('GOLDENTOBC.BO', 'GODFRYPHLP.BO', 'ITC.BO', 'SINNAR.BO', 'VSTIND.BO', 'NTCIND.BO', 'RAGHUNAT.BO', 
             'ASSAMCO.BO', 'GOODRICKE.BO', ' TATAGLOBAL.BO', ' BBTC.BO', ' BNALTD.BO', ' LEDOTEA.BO', ' WARRENTEA.BO', ' NEAGI.BO', ' JAYSHREETEA.BO', ' RGRL.BO', ' BANSTEA.BO', ' CCL.BO', ' ANDREWYU.BO', 'LONTE.BO', 'TYROON.BO', 'TERAI.BO', 'DIANATEA.BO', 'TATACOFFEE.BO', 'MCLEODRUSS.BO', 'ROSSELLIND.BO', 'JOONKTOLL.BO', 'JAMESWARREN.BO', 'DHUNTEAIND.BO', ' GPL.BO', ' KANCOTEA.BO')
low_BSE <- getFin(low_BSE)
low_BSE <- low_BSE %>%
  mutate(Industry = 'Low')
##################
BSE <- rbind(health_BSE,parm_BSE,computer_BSE,
             tech_BSE,construction_BSE,real_BSE,
             food_BSE,trans_BSE,bio_BSE,aero_BSE,
             tel_BSE,aul_BSE,stl_BSE,life_BSE,mining_BSE,
             high_BSE,low_BSE)
BSE <- BSE %>%
  mutate(Frequency=0)
######################
areo_HE <- c('FIA1S.HE')
areo_HE <- getFin(areo_HE)
areo_HE <- areo_HE %>%
  mutate(Industry = 'High')
######################
health_HE <- c('ORNAV.HE', 'ORNBV.HE', 'TTALO.HE', 'OKDAV.HE', 'OKDBV.HE', 'PIHLIS.HE', 'REG1V.HE', 'BIOBV.HE', 'SILMA.HE')
health_HE <- getFin(health_HE)
health_HE <- health_HE %>%
  mutate(Industry = 'High')
######################
tel_HE <- c('DNA.HE', 'ELISA.HE', 'TELIA1.HE')
tel_HE <- getFin(tel_HE)
tel_HE <- tel_HE %>%
  mutate(Industry = 'High')
######################
tech_HE <- c('FSC1V.HE', 'DIGIA.HE', 'DIGIGR.HE', 'IFA1V.HE', 'NIXU.HE', 'QPR1V.HE', 'QTCOM.HE', 'SIILI.HE', 'SOLTEQ.HE', 'SOPRA.HE', 'SSH1V.HE', 'TEM1V.HE', 'TLT1V.HE', 'TRH1V.HE', 'NOKIA.HE', 'TIETO.HE', 'BAS1V.HE', 'BITTI.HE', 'ROVIO.HE', 'ASPO.HE', 'SCANFL.HE', 'VAIAS.HE', 'EFO1V.HE', 'NEO1V.HE', 'YEINT.HE')
tech_HE <- getFin(tech_HE)
tech_HE <- tech_HE %>%
  mutate(Industry = 'High')
########################
les_HE <- c('AMEAS.HE')
les_HE <- getFin(les_HE)
les_HE <- les_HE %>%
  mutate(Industry = 'High')
########################
oil_HE <- c('NESTE.HE', 'DOV1V.HE')
oil_HE <- getFin(les_HE)
oil_HE <- les_HE %>%
  mutate(Industry = 'Low')
########################
construnction_HE <- c('AM1.HE', 'KEMIRA.HE', 'METSA.HE', 'METSB.HE', 'OUT1V.HE', 'SSABAH.HE', 'SSABBH.HE', 'STEAV.HE', 'STERV.HE', 'UPM.HE', 'AFAGR. HE', 'ENDOM.HE', 'SOSI1.HE', 'UPONOR.HE', 'YIT.HE', 'ETTE.HE', 'OTE1V.HE', 'PON1V.HE', 'POY1V.HE', 'SRV1V.HE', 'KELAS.HE', 'TULAV.HE')
construnction_HE <- getFin(construnction_HE)
construnction_HE <- construnction_HE %>%
  mutate(Industry = 'Low')
########################
food_HE <- c('ALTIA.HE', 'ATRAV.HE', 'HKSAV.HE', 'OLVAS.HE', 'RAIVV.HE', 'APETIT.HE')
food_HE <- getFin(food_HE)
food_HE <- food_HE %>%
  mutate(Industry = 'Low')
#######################
real_HE <- c('LEHTO.HE')
real_HE <- getFin(real_HE)
real_HE <- real_HE %>%
  mutate(Industry = 'Low')
#######################
HE <- rbind(real_HE,food_HE,construnction_HE,oil_HE,
            les_HE,tech_HE,tel_HE,health_HE,areo_HE)
HE <- HE %>%
  mutate(Frequency=1)
#######################
####### NEW VER. ######
#######################
high_ATH <- c('SPIR.AT','QUEST.AT', 'INTEK.AT', 'PROF.AT', 'MLS.AT', 'LOGISMOS.AT', 'ILYDA.AT', 'ENTER.AT','MEDIC.AT', 'IATR.AT', 'IASO.AT', 'HYGEIA.AT', 'EUROM.AT', 'AXON.AT',
              'LAVI.AT')
high_ATH <- getFin(high_ATH)
high_ATH <- high_ATH %>%
  mutate(Industry = 'High')
#######################
low_ATH <- c('VIO.AT', 'IKTIN.AT', 'ELHA.AT', 'BIOKA.AT', 'ALMYBOR.AT', 'ALMY.AT','TRASTOR.AT', 'PASAL.AT', 'LAMDA.AT', 'KEKR.AT', 'KAMP.AT', 'GRIV.AT', 'ELBIO.AT', 'BRIQ.AT', 'ASTAK.AT',
             'TPEIR.AT', 'TATT.AT', 'EUROB.AT', 'ETE.AT', 'ALPHA.AT','TITP.AT', 'TITK.AT', 'PRD.AT', 'MERKO.AT', 'MATHIO.AT', 'INKAT.AT', 'GEKTERNA.AT', 'ELLAKTOR.AT', 'EKTER.AT', 'DOMIK.AT', 'BIOT.AT', 'AVAX.AT', 'AEGEK.AT','TENERGY.AT', 'TELET.AT', 'PEGAS.AT', 'NAFT.AT', 'LIVAN.AT', 'ATEK.AT','SIDMA.AT', 'PROFK.AT', 'MPITR.AT', 'LEBEP.AT', 'LEBEK.AT', 'ELSTR.AT', 
             'SFA.AT', 'PLAIS.AT', 'NAKAS.AT', 'MSHOP.AT', 'MOTO.AT','SELO.AT', 'PERS.AT', 'NIR.AT', 'NIKAS.AT', 'KYSA.AT', 'KYLO.AT', 'KMOL.AT', 'KEPEN.AT', 'HKRAN.AT', 'EVROF.AT', 'CRETA.AT',
             'REVOIL.AT', 'MOH.AT', 'ELPE.AT', 'ELIN.AT','PPA.AT', 'OLTH.AT','MPP.AT', 'MPK.AT', 'HARB-B.AT', 'KTILA.AT', 'EEE.AT', 
             'KARE.AT',  'KANAK.AT', 'ELGEK.AT','EUPIC.AT', 'EUBRK.AT','AKRIT.AT')
low_ATH <- getFin(low_ATH)
low_ATH <- low_ATH %>%
  mutate(Industry = 'Low')
ATH <- rbind(high_ATH,low_ATH)
ATH <- ATH %>%
  mutate(Frequency=1)
#######################
high_LIS <- c('SCT.LS', 'CTT.LS')
high_LIS <- getFin(high_LIS)
high_LIS <- high_LIS %>%
  mutate(Industry = 'High')
########################
low_LIS <- c('SONI.LS', 'SEM.LS', 'NVG.LS', 'ALTR.LS','TDSA.LS', 'MAR.LS', 'EGL.LS', 'CDU.LS','SON.LS', 'JMT.LS','SNG.LS', 'SNC.LS', 'PHR.LS', 'RAM.LS','ORE.LS', 'EDPR.LS')
low_LIS <- getFin(low_LIS)
low_LIS <- low_LIS %>%
  mutate(Industry = 'Low')
LIS <- rbind(high_LIS,low_LIS)
LIS <- LIS %>%
  mutate(Frequency=1)
#######################
high_CO <- c('ALK-B.CO', 'AMBU.CO', 'CHR.CO', 'COLO-B.CO', 'DEMANT.CO', ' GEN.CO', 'GN.CO', 'LUN.CO', 'NOVO-B.CO', 'NZYM-B.CO', 'OSSR.CO', 'BAVA.CO', 'ORPHA.CO', 'VELO.CO', 'ZEAL.CO', 'BIOPOR.CO', 'CHEMM.CO', 'NEUR.CO', 'ONXEO.CO', 'NETC.CO', 'SIM.CO', 'COLUM.CO', 'NNIT.CO', 'RTX.CO', 'CBRAIN.CO', 'BO.CO', 'DANT.CO', 'BOLIGA.CO', 'NKT.CO', 'SOLAR-B.CO', 'NTR-B.CO',
             'SBS.CO', 'AM-B.CO','SAS-DKK.CO', 'KBHL.CO', 'ROV.CO','FLUG-B.CO')
high_CO <- getFin(high_CO)
high_CO <- high_CO %>%
  mutate(Industry = 'High')
#######################
low_CO <- c('DRLCO.CO', 'VWS.CO', 'ALTA-DKK.CO', 'NORDIC.CO', 'CARL-B.CO', 'RBREW.CO', 'HARB-B.CO', 'STG.CO', 'RBREW.CO', 'ALTA-DKK.CO', 'NORDIC.CO', 
            'MATAS.CO','ALMB.CO', 'TOP.CO', 'TRYG.CO','IMAIL.CO', 'ROCK-A.CO', 'ROCK-B.CO', 'HH.CO', 'PAAL-B.CO', 'ARKIL-B.CO', 'MTHH.CO', 'RIAS-B.CO',
            'DANSKE.CO', 'JYSK.CO', 'NDA-DK.CO', 'RILBA.CO', 'SPNO.CO', 'SYDB.CO', 'BNORDIK-CSE.CO', 'JUTBK.CO', 'LASP.CO', 'SPKSJF.CO', 'VJBA.CO', 'DAB.CO', 'DJS.CO', 'DJUR.CO', 'FYNBK.CO', 'GRLA.CO', 'HVID.CO', 'KRE.CO', 'LOLB.CO', 'MNBA.CO', 'NRDF.CO', 'SALB.CO', 'SKJE.CO', 'TOTA.CO','JDAN.CO', 'ADMCAP-B.CO', 'BLVIS.CO', 'CEMAT.CO', 'CPHCAP-PREF.CO', 'CPHCAP-ST.CO', 'FED.CO', 'GERHSP.CO', 'PRIMOF.CO', 
            'PSNRDCA.CO', 'VIPRO.CO')
low_CO <- getFin(low_CO)
low_CO <- low_CO %>%
  mutate(Industry = 'Low')
CO <- rbind(high_CO,low_CO)
CO <- CO %>%
  mutate(Frequency=1)
#######################
high_SSE <- c('ARJO-B.ST', 'ATT.ST', 'AZN.ST', 'EKTA-B.ST', 'GETI-B.ST', 'SOBI.ST', 'VIRT.ST', 'ALIF.ST', 'AMBEA.ST', 'ATORX.ST', 'BIOA-B.ST', 'BIOG-B.ST', 'BIOT.ST', 'CALTX.ST', 'CAMX.ST', 'CEVI.ST', 'HANDI.ST', ' HNSA.ST', 'HUM.ST', 'IBT-B.ST', 'IMMNOV.ST', 'KARO.ST', 'MCOV-B.ST', 'OASM.ST', 'ONCO.ST', 'ORX.ST', 'PROB.ST', 'QLINEA.ST', 'RAY-B.ST', 'RECI-B.ST', 'SECT-B.ST', 'XVIVO.ST', 'ACE.ST', 'ACTI.ST', 'BACTI-B.ST', 'BINV.ST', 'BONEX.ST', 'BOUL.ST', 'CANTA.ST', 'CARD-B.ST', 'DEDI.ST', 'ELOS-B.ST', 'EPIS-B.ST', 'FEEL.ST', 'GHP.ST', 'IMMU.ST', 'KDEV.ST', 'MCAP.ST', 'MOB.ST', 'MVIR-B.ST', 'NUE.ST', 'NVP.ST', 'ORTI-A.ST', 'ORTI-B.ST', 'SANION.ST', 'LIFCO-B.ST','ERIC-A.ST', 'ERIC-B.ST', 'HEXA-B.ST', 'TIETOS.ST', 'ACAN-B.ST', 'ANOD-B.ST', 'ENEA.ST', 'G5EN.ST', 'GIGSEK.ST', 'HIQ.ST', 'HMS.ST', 'HTRO.ST', 'IAR-B.ST', 'IVSO.ST', 'KNOW.ST', 'NETI-B.ST', 'PACT.ST', 'SINCH.ST', 'STAR-A.ST', 'STAR-B.ST', 'TOBII.ST', 'VIT-B.ST', 'ANOT.ST', 'B3.ST', 'DORO.ST', 'EDGE.ST', 'EMPIR-B.ST', 'FPIP.ST', 'LIME.ST', 'MSAB-B.ST', 'MULQ.ST', 'NCAB.ST', 'NTEK-B.ST', 'PREV-B.ST', 'SOF-B.ST', 'STWK.ST', 'ZETA.ST', 'DOM.ST', 'ELUX-A.ST', 'ELUX-B.ST', 'STRAX.ST', 'NET-B.ST', 'DUST.ST', 'LEO.ST', 'ENRO.ST', 'ENRO-PREF.ST', 'NOLA-B.ST', 'AQ.ST', 'FING-B.ST', 'LAGR-B.ST', 'EWRK.ST', 'ICTA.ST', 'IS.ST', 'MEAB-B.ST', 'NOTE.ST', 'SINT.ST',
              'HPOL-B.ST', 'ALIV-SDB.ST', 'VNE-SDB.ST', 'BULTEN.ST', 'HLDX.ST', 'VBG-B.ST', 'OPUS.ST','BETCO.ST', 'BELE.ST','SAAB-B.ST', 'CTT.ST',
              'MYCR.ST', 'HANZA.ST')
high_SSE <- getFin(high_SSE)
high_SSE <- high_SSE %>%
  mutate(Industry = 'High')
#######################
low_SSE <- c('LUPE.ST', 'AOI.ST', 'ENQ.ST', 'IPCO.ST', 'TETY.ST', 'EOLU-B.ST','TEL2-A.ST', 'TEL2-B.ST', 'TELIA.ST', 'TIGO-SDB.ST', 'ATRE.ST', 
             'AM1S.ST', 'BILL.ST', 'HOLM-A.ST', 'HOLM-B.ST', 'STE-A.ST', 'STE-R.ST', 'ARP-A.ST', 'RROS.ST',  'SSAB-A.ST', 'SSAB-B.ST', 'BEGR.ST',
             'AAK.ST', 'SCST.ST', 'SWMA.ST', 'GRNG.ST', 'MTG-A.ST', 'MTG-B.ST', 'NENT-A.ST', 'NENT-B.ST', 'MOMENT.ST',
             'ARION.ST', 'AZA.ST', 'NDA-SE.ST', 'SEB-A.ST', 'SEB-C.ST', 'SHB-A.ST', 'SHB-B.ST', 'SWED-A.ST', 'TFBANK.ST', 
             'ATRLJ-B.ST', 'BALD-B.ST', 'CAST.ST', 'FABG.ST', 'HEMF.ST', 'HEMF-PREF.ST', 'HUFV-A.ST', 'HUFV-C.ST', 'KLED.ST', 'KLOV-A.ST', 'KLOV-B.ST', 'KLOV-PREF.ST', 'NYF.ST', 'SAGA-A.ST', 'SAGA-B.ST', 'SAGA-D.ST', 'SAGA-PREF.ST', 'WALL-B.ST', 'WIHL.ST', 'BESQ.ST', 'CATE.ST', 'CORE-A.ST', 'CORE-B.ST', 'CORE-PREF.ST','DIOS.ST', 'FPAR.ST', 'FPAR-PREF.ST', 'HEBA-B.ST', 'HEM-B.ST', 'MAG.ST', 'NP3.ST', 'NP3-PREF.ST', 'PLAZ-B.ST', 'STEF-B.ST', 'VICP-A.ST', 'VICP-B.ST', 'VICP-PREF.ST', 'BRIN-B.ST', 
             'BONAV-A.ST', 'BONAV-B.ST', 'JM.ST', 'SSM.ST', 'AF-B.ST', 'BRAV.ST', 'EPI-A.ST', 'EPI-B.ST', 'NCC-A.ST', 'NCC-B.ST', 'NIBE-B.ST', 'PEAB-B.ST', 'SKA-B.ST', 'SWEC-A.ST', 'SWEC-B.ST', 'ELTEL.ST', 'INSTAL.ST', 'INWI.ST', 'LIAB.ST', 'NWG.ST', 'SRNKE-B.ST', 'SYSR.ST', 'BALCO.ST', 'CBTT-B.ST', 'FMM-B.ST', 'PENG-B.ST', 'SEMC.ST', 'SVED-B.ST')
low_SSE <- getFin(low_SSE)
low_SSE <- low_SSE %>%
  mutate(Industry = 'Low')
SSE <- rbind(high_SSE,low_SSE)
SSE <- SSE %>%
  mutate(Frequency=0)
#######################
data <- rbind(LSE,FSE,BSE,HE,ATH,LIS,CO,SSE)
write.csv(data, file = "/Users/chuan/Desktop/MyData.csv")
mydata <- na.omit(data)
write.csv(mydata, file = "/Users/chuan/Desktop/MyData2.csv")
######################
mydata <- mydata %>%
  mutate(SER = TE/TA) %>%
  mutate(RND = RD/OR) %>%
  mutate(ROA = NI/TA) %>%
  mutate(Size = TA)
######################
mydata_new <- mydata %>%
  select(company,SER,RND,ROA,Size,Frequency,Industry,Patent)
write.csv(mydata_new, file = "/Users/chuan/Desktop/MyData3.csv")
######################
data <- mydata_new %>%
  select(SER,RND,ROA,Size,Frequency,Patent) %>%
  filter(RND < 1 & RND >-1)
summary(data)
library("Hmisc")
rcorr(as.matrix(data))
#######################
m1 <- lm(RND ~Frequency,data)
summary(m1)
confint(m1)
m1 <- lm(RND ~.,data)
summary(m1)
confint(m1)
######################
big <- data %>%
  filter(Size > 800000)
small <- data %>%
  filter(Size < 800000)

m1 <- lm(RND ~Frequency,big)
summary(m1)
confint(m1)
m1 <- lm(RND ~.,big)
summary(m1)
confint(m1)
m1 <- lm(RND ~Frequency,small)
summary(m1)
confint(m1)
m1 <- lm(RND ~.,small)
summary(m1)
confint(m1)
######
data <- mydata_new %>%
  select(SER,RND,ROA,Size,Frequency,Patent,Industry) %>%
  filter(RND < 1 & RND >-1)
big <- data %>%
  filter(Industry == 'High') %>%
  select(SER,RND,ROA,Size,Frequency,Patent)
small <- data %>%
  filter(Industry == 'Low') %>%
  select(SER,RND,ROA,Size,Frequency,Patent)

m1 <- lm(RND ~Frequency,big)
summary(m1)
confint(m1)
m1 <- lm(RND ~.,big)
summary(m1)
confint(m1)
m1 <- lm(RND ~Frequency,small)
summary(m1)
confint(m1)
m1 <- lm(RND ~.,small)
summary(m1)
confint(m1)
#####3
library("PerformanceAnalytics")
chart.Correlation(data, histogram = TRUE, pch = 19)
round(apply(data, 2, sd),3)
