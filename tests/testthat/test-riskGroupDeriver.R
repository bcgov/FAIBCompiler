test_that("riskGroupDeriver.R: tree risk group is not correctly assigned.", {
  library(data.table)
  library(testthat)
  ## prepare data set for testing PSP
  inputdata_psp <- expand.grid(SP0 = c("S", "H",  "B",  "C",  "PL", "Y",  "E",
                                       "F", "D",  "PW", "AC", "L",
                                       "MB", "AT", "PY", "PA"),
                               LOSS1_IN = c("BNK", "BTP", "CD", "CNK", "CRO", "D",
                                            "DD", "DDA", "DDB",
                                            "DDD", "DDE", "DDF", "DDH", "DDO",
                                            "DDP", "DDQ", "DDT", "DIR",
                                            "DM", "DR", "DRA", "DRB", "DRC",
                                            "DRL", "DRN", "DRR", "DRS",
                                            "DRT", "DTP", "FRK", "FRS", "LRB",
                                            "MIS", "NGC", "OTH", "SCA",
                                            "SNG")) %>%
    data.table

  inputdata_psp[,':='(SP0 = as.character(SP0),
                      LOSS1_IN = as.character(LOSS1_IN))]
  set(inputdata_psp, , c(paste0("LOSS", 2:8, "_IN")), NA)

  expect_error(inputdata_psp$PATH_IND <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                                compiler = "ff"))

  expect_error(inputdata_psp$PATH_IND <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                                compiler = "VRI"))

  inputdata_psp$PATH_IND1 <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                    compiler = "PSP")
  inputdata_psp[, RISK_GRP1 := riskGroupDeriver(species = SP0,
                                                pathIndex = PATH_IND1,
                                                method = "KBEC")]


  output_rene <- data.table(SP0 = c("L", "D", "S", "PY", "F", "E", "PW", "B",
                                    "PA", "H", "C", "AT", "AC", "PL", "Y", "MB", "PY", "F", "L",
                                    "E", "MB", "AT", "PW", "PA", "Y", "AC", "PL", "C", "B", "D",
                                    "S", "H", "AT", "PA", "H", "L", "C", "PY", "D", "AC", "F", "Y",
                                    "E", "S", "PW", "MB", "PL", "B", "MB", "Y", "L", "D", "AC", "F",
                                    "PY", "AT", "E", "C", "PW", "S", "PL", "B", "H", "PA", "PA",
                                    "AT", "L", "PY", "AC", "D", "F", "MB", "Y", "E", "PL", "C", "H",
                                    "B", "S", "PW", "PA", "AT", "L", "AC", "PY", "PW", "D", "F",
                                    "MB", "PL", "Y", "C", "B", "E", "H", "S", "PA", "AT", "L", "AC",
                                    "PY", "PW", "D", "F", "MB", "PL", "Y", "C", "B", "E", "H", "S",
                                    "AT", "PY", "L", "PW", "D", "F", "AC", "MB", "PL", "Y", "C",
                                    "B", "E", "H", "S", "PA", "PA", "AT", "L", "AC", "PW", "MB",
                                    "D", "PY", "F", "PL", "Y", "C", "B", "H", "E", "S", "PA", "AT",
                                    "MB", "L", "AC", "PW", "D", "PY", "F", "Y", "PL", "C", "B", "H",
                                    "E", "S", "PA", "AT", "MB", "L", "AC", "PW", "D", "PY", "F",
                                    "Y", "PL", "C", "B", "H", "E", "S", "PA", "AT", "MB", "L", "AC",
                                    "PW", "D", "PY", "F", "Y", "PL", "C", "B", "H", "E", "S", "PA",
                                    "AT", "MB", "L", "AC", "PW", "D", "PY", "F", "Y", "PL", "C",
                                    "B", "H", "E", "S", "PA", "AT", "MB", "L", "AC", "PW", "D", "PY",
                                    "F", "Y", "PL", "C", "B", "H", "E", "S", "PA", "AT", "MB", "L",
                                    "AC", "PW", "D", "PY", "F", "Y", "PL", "C", "B", "H", "E", "S",
                                    "AT", "L", "AC", "PY", "PW", "D", "F", "MB", "Y", "PL", "C",
                                    "B", "H", "E", "S", "PA", "PA", "PY", "AT", "L", "MB", "AC",
                                    "PW", "D", "F", "E", "PL", "Y", "C", "B", "H", "S", "PA", "PY",
                                    "AT", "L", "MB", "AC", "PW", "D", "F", "E", "PL", "Y", "C", "B",
                                    "H", "S", "PA", "AT", "MB", "L", "AC", "PW", "D", "F", "PY",
                                    "E", "PL", "Y", "C", "B", "H", "S", "PA", "AT", "MB", "L", "AC",
                                    "PW", "D", "F", "PY", "E", "Y", "PL", "C", "B", "H", "S", "PA",
                                    "AT", "MB", "L", "AC", "PW", "D", "F", "PY", "E", "Y", "PL",
                                    "C", "B", "H", "S", "PA", "AT", "MB", "L", "AC", "PW", "D", "F",
                                    "PY", "E", "Y", "PL", "C", "B", "H", "S", "PA", "AT", "MB", "L",
                                    "AC", "PW", "D", "F", "PY", "E", "Y", "PL", "C", "B", "H", "S",
                                    "PA", "AT", "MB", "L", "AC", "PW", "D", "F", "PY", "E", "Y",
                                    "PL", "C", "B", "H", "S", "PA", "AT", "MB", "L", "AC", "PW",
                                    "D", "F", "PY", "E", "Y", "PL", "C", "B", "H", "S", "PA", "AT",
                                    "MB", "L", "AC", "PW", "D", "F", "PY", "E", "Y", "PL", "C", "B",
                                    "H", "S", "PA", "AT", "MB", "L", "AC", "PW", "D", "F", "PY",
                                    "E", "Y", "PL", "C", "B", "H", "S", "PA", "AT", "MB", "L", "AC",
                                    "PW", "D", "F", "PY", "E", "Y", "PL", "C", "B", "H", "S", "PA",
                                    "AT", "MB", "L", "AC", "PW", "D", "F", "PY", "E", "Y", "PL",
                                    "C", "B", "H", "S", "PA", "AT", "MB", "L", "AC", "PW", "D", "F",
                                    "PY", "E", "Y", "PL", "C", "B", "H", "S", "PA", "AT", "MB", "L",
                                    "AC", "PW", "D", "F", "PY", "E", "Y", "PL", "C", "B", "H", "S",
                                    "AT", "MB", "L", "AC", "PW", "D", "F", "PY", "E", "Y", "PL",
                                    "C", "B", "H", "S", "PA", "PA", "AT", "MB", "L", "PY", "AC",
                                    "PW", "D", "F", "E", "Y", "PL", "C", "B", "H", "S", "PA", "AT",
                                    "MB", "L", "PY", "AC", "PW", "D", "F", "E", "Y", "PL", "C", "B",
                                    "H", "S", "PA", "AT", "MB", "L", "PY", "AC", "PW", "D", "F",
                                    "E", "Y", "PL", "C", "B", "H", "S", "PA", "AT", "MB", "L", "PY",
                                    "AC", "PW", "D", "F", "E", "Y", "PL", "C", "B", "H", "S", "PA",
                                    "AT", "MB", "L", "PY", "AC", "PW", "D", "F", "E", "Y", "PL",
                                    "C", "B", "H", "S"),
                            LOSS1_IN = c("BNK", "BNK", "BNK", "BNK",
                                         "BNK", "BNK", "BNK", "BNK", "BNK", "BNK", "BNK", "BNK", "BNK",
                                         "BNK", "BNK", "BNK", "BTP", "BTP", "BTP", "BTP", "BTP", "BTP",
                                         "BTP", "BTP", "BTP", "BTP", "BTP", "BTP", "BTP", "BTP", "BTP",
                                         "BTP", "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CD",
                                         "CD", "CD", "CD", "CD", "CD", "CD", "CD", "CNK", "CNK", "CNK",
                                         "CNK", "CNK", "CNK", "CNK", "CNK", "CNK", "CNK", "CNK", "CNK",
                                         "CNK", "CNK", "CNK", "CNK", "CRO", "CRO", "CRO", "CRO", "CRO",
                                         "CRO", "CRO", "CRO", "CRO", "CRO", "CRO", "CRO", "CRO", "CRO",
                                         "CRO", "CRO", "D", "D", "D", "D", "D", "D", "D", "D", "D", "D",
                                         "D", "D", "D", "D", "D", "D", "DD", "DD", "DD", "DD", "DD", "DD",
                                         "DD", "DD", "DD", "DD", "DD", "DD", "DD", "DD", "DD", "DD", "DDA",
                                         "DDA", "DDA", "DDA", "DDA", "DDA", "DDA", "DDA", "DDA", "DDA",
                                         "DDA", "DDA", "DDA", "DDA", "DDA", "DDA", "DDB", "DDB", "DDB",
                                         "DDB", "DDB", "DDB", "DDB", "DDB", "DDB", "DDB", "DDB", "DDB",
                                         "DDB", "DDB", "DDB", "DDB", "DDD", "DDD", "DDD", "DDD", "DDD",
                                         "DDD", "DDD", "DDD", "DDD", "DDD", "DDD", "DDD", "DDD", "DDD",
                                         "DDD", "DDD", "DDE", "DDE", "DDE", "DDE", "DDE", "DDE", "DDE",
                                         "DDE", "DDE", "DDE", "DDE", "DDE", "DDE", "DDE", "DDE", "DDE",
                                         "DDF", "DDF", "DDF", "DDF", "DDF", "DDF", "DDF", "DDF", "DDF",
                                         "DDF", "DDF", "DDF", "DDF", "DDF", "DDF", "DDF", "DDH", "DDH",
                                         "DDH", "DDH", "DDH", "DDH", "DDH", "DDH", "DDH", "DDH", "DDH",
                                         "DDH", "DDH", "DDH", "DDH", "DDH", "DDO", "DDO", "DDO", "DDO",
                                         "DDO", "DDO", "DDO", "DDO", "DDO", "DDO", "DDO", "DDO", "DDO",
                                         "DDO", "DDO", "DDO", "DDP", "DDP", "DDP", "DDP", "DDP", "DDP",
                                         "DDP", "DDP", "DDP", "DDP", "DDP", "DDP", "DDP", "DDP", "DDP",
                                         "DDP", "DDQ", "DDQ", "DDQ", "DDQ", "DDQ", "DDQ", "DDQ", "DDQ",
                                         "DDQ", "DDQ", "DDQ", "DDQ", "DDQ", "DDQ", "DDQ", "DDQ", "DDT",
                                         "DDT", "DDT", "DDT", "DDT", "DDT", "DDT", "DDT", "DDT", "DDT",
                                         "DDT", "DDT", "DDT", "DDT", "DDT", "DDT", "DIR", "DIR", "DIR",
                                         "DIR", "DIR", "DIR", "DIR", "DIR", "DIR", "DIR", "DIR", "DIR",
                                         "DIR", "DIR", "DIR", "DIR", "DM", "DM", "DM", "DM", "DM", "DM",
                                         "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DM", "DR",
                                         "DR", "DR", "DR", "DR", "DR", "DR", "DR", "DR", "DR", "DR", "DR",
                                         "DR", "DR", "DR", "DR", "DRA", "DRA", "DRA", "DRA", "DRA", "DRA",
                                         "DRA", "DRA", "DRA", "DRA", "DRA", "DRA", "DRA", "DRA", "DRA",
                                         "DRA", "DRB", "DRB", "DRB", "DRB", "DRB", "DRB", "DRB", "DRB",
                                         "DRB", "DRB", "DRB", "DRB", "DRB", "DRB", "DRB", "DRB", "DRC",
                                         "DRC", "DRC", "DRC", "DRC", "DRC", "DRC", "DRC", "DRC", "DRC",
                                         "DRC", "DRC", "DRC", "DRC", "DRC", "DRC", "DRL", "DRL", "DRL",
                                         "DRL", "DRL", "DRL", "DRL", "DRL", "DRL", "DRL", "DRL", "DRL",
                                         "DRL", "DRL", "DRL", "DRL", "DRN", "DRN", "DRN", "DRN", "DRN",
                                         "DRN", "DRN", "DRN", "DRN", "DRN", "DRN", "DRN", "DRN", "DRN",
                                         "DRN", "DRN", "DRR", "DRR", "DRR", "DRR", "DRR", "DRR", "DRR",
                                         "DRR", "DRR", "DRR", "DRR", "DRR", "DRR", "DRR", "DRR", "DRR",
                                         "DRS", "DRS", "DRS", "DRS", "DRS", "DRS", "DRS", "DRS", "DRS",
                                         "DRS", "DRS", "DRS", "DRS", "DRS", "DRS", "DRS", "DRT", "DRT",
                                         "DRT", "DRT", "DRT", "DRT", "DRT", "DRT", "DRT", "DRT", "DRT",
                                         "DRT", "DRT", "DRT", "DRT", "DRT", "DTP", "DTP", "DTP", "DTP",
                                         "DTP", "DTP", "DTP", "DTP", "DTP", "DTP", "DTP", "DTP", "DTP",
                                         "DTP", "DTP", "DTP", "FRK", "FRK", "FRK", "FRK", "FRK", "FRK",
                                         "FRK", "FRK", "FRK", "FRK", "FRK", "FRK", "FRK", "FRK", "FRK",
                                         "FRK", "FRS", "FRS", "FRS", "FRS", "FRS", "FRS", "FRS", "FRS",
                                         "FRS", "FRS", "FRS", "FRS", "FRS", "FRS", "FRS", "FRS", "LRB",
                                         "LRB", "LRB", "LRB", "LRB", "LRB", "LRB", "LRB", "LRB", "LRB",
                                         "LRB", "LRB", "LRB", "LRB", "LRB", "LRB", "MIS", "MIS", "MIS",
                                         "MIS", "MIS", "MIS", "MIS", "MIS", "MIS", "MIS", "MIS", "MIS",
                                         "MIS", "MIS", "MIS", "MIS", "NGC", "NGC", "NGC", "NGC", "NGC",
                                         "NGC", "NGC", "NGC", "NGC", "NGC", "NGC", "NGC", "NGC", "NGC",
                                         "NGC", "NGC", "OTH", "OTH", "OTH", "OTH", "OTH", "OTH", "OTH",
                                         "OTH", "OTH", "OTH", "OTH", "OTH", "OTH", "OTH", "OTH", "OTH",
                                         "SCA", "SCA", "SCA", "SCA", "SCA", "SCA", "SCA", "SCA", "SCA",
                                         "SCA", "SCA", "SCA", "SCA", "SCA", "SCA", "SCA", "SNG", "SNG",
                                         "SNG", "SNG", "SNG", "SNG", "SNG", "SNG", "SNG", "SNG", "SNG",
                                         "SNG", "SNG", "SNG", "SNG", "SNG"),
                            path_ind_rene = c("01000000", "01000000", "01000000", "01000000", "01000000",
                                              "01000000", "01000000", "01000000", "01000000", "01000000", "01000000",
                                              "01000000", "01000000", "01000000", "01000000", "01000000", "00000001",
                                              "00000001", "00000001", "00000001", "00000001", "00000001", "00000001",
                                              "00000001", "00000001", "00000001", "00000001", "00000001", "00000001",
                                              "00000001", "00000001", "00000001", "00000001", "00000001", "00000001",
                                              "00000001", "00000001", "00000001", "00000001", "00000001", "00000001",
                                              "00000001", "00000001", "00000001", "00000001", "00000001", "00000001",
                                              "00000001", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "00010000",
                                              "00010000", "00010000", "00010000", "00010000", "00010000", "00010000",
                                              "00010000", "00010000", "00010000", "00010000", "00010000", "00010000",
                                              "00010000", "00010000", "00010000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "00000100", "00000100", "00000100", "00000100", "00000100",
                                              "00000100", "00000100", "00000100", "00000100", "00000100", "00000100",
                                              "00000100", "00000100", "00000100", "00000100", "00000100", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "10000000",
                                              "10000000", "10000000", "10000000", "10000000", "10000000", "00000001",
                                              "00000001", "00000001", "00000001", "00000001", "00000001", "00000001",
                                              "00000001", "00000001", "00000001", "00000001", "00000001", "00000001",
                                              "00000001", "00000001", "00000001", "00010000", "00010000", "00010000",
                                              "00010000", "00010000", "00010000", "00010000", "00010000", "00010000",
                                              "00010000", "00010000", "00010000", "00010000", "00010000", "00010000",
                                              "00010000", "00001000", "00001000", "00001000", "00001000", "00001000",
                                              "00001000", "00001000", "00001000", "00001000", "00001000", "00001000",
                                              "00001000", "00001000", "00001000", "00001000", "00001000", "00000010",
                                              "00000010", "00000010", "00000010", "00000010", "00000010", "00000010",
                                              "00000010", "00000010", "00000010", "00000010", "00000010", "00000010",
                                              "00000010", "00000010", "00000010", "00000100", "00000100", "00000100",
                                              "00000100", "00000100", "00000100", "00000100", "00000100", "00000100",
                                              "00000100", "00000100", "00000100", "00000100", "00000100", "00000100",
                                              "00000100", "00001000", "00001000", "00001000", "00001000", "00001000",
                                              "00001000", "00001000", "00001000", "00001000", "00001000", "00001000",
                                              "00001000", "00001000", "00001000", "00001000", "00001000", NA,
                                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "00100000",
                                              "00100000", "00100000", "00100000", "00100000", "00100000", "00100000",
                                              "00100000", "00100000", "00100000", "00100000", "00100000", "00100000",
                                              "00100000", "00100000", "00100000", NA, NA, NA, NA, NA, NA, NA,
                                              NA, NA, NA, NA, NA, NA, NA, NA, NA),
                            risk_grp_rene = c(3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L,
                                              2L, 2L, 2L, 3L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L,
                                              2L, 2L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L,
                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L,
                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                                              3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L,
                                              2L, 2L, 2L, 2L, 2L, 3L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L,
                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L,
                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L,
                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L,
                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L,
                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L))

  inputdata_psp <- merge(inputdata_psp,
                         output_rene,
                         by = c("SP0", "LOSS1_IN"))
  inputdata_psp[is.na(path_ind_rene), path_ind_rene := "00000000"]

  expect_equal(inputdata_psp$PATH_IND1, inputdata_psp$path_ind_rene)
  inputdata_psp[, risk_grp_rene := as.character(risk_grp_rene)]
  expect_equal(inputdata_psp$RISK_GRP1, inputdata_psp$risk_grp_rene)
  inputdata_psp[, LOSS2_IN := LOSS1_IN]
  inputdata_psp[, LOSS1_IN := NA]
  inputdata_psp$PATH_IND2 <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                    compiler = "PSP")
  inputdata_psp[, RISK_GRP2 := riskGroupDeriver(species = SP0,
                                                pathIndex = PATH_IND2,
                                                method = "KBEC")]


  inputdata_psp[, LOSS3_IN := LOSS2_IN]
  inputdata_psp[, LOSS2_IN := NA]
  inputdata_psp$PATH_IND3 <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                    compiler = "PSP")
  inputdata_psp[, RISK_GRP3 := riskGroupDeriver(species = SP0,
                                                pathIndex = PATH_IND3,
                                                method = "KBEC")]

  inputdata_psp[, LOSS4_IN := LOSS3_IN]
  inputdata_psp[, LOSS3_IN := NA]
  inputdata_psp$PATH_IND4 <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                    compiler = "PSP")
  inputdata_psp[, RISK_GRP4 := riskGroupDeriver(species = SP0,
                                                pathIndex = PATH_IND4,
                                                method = "KBEC")]

  inputdata_psp[, LOSS5_IN := LOSS4_IN]
  inputdata_psp[, LOSS4_IN := NA]
  inputdata_psp$PATH_IND5 <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                    compiler = "PSP")
  inputdata_psp[, RISK_GRP5 := riskGroupDeriver(species = SP0,
                                                pathIndex = PATH_IND5,
                                                method = "KBEC")]
  inputdata_psp[, LOSS6_IN := LOSS5_IN]
  inputdata_psp[, LOSS5_IN := NA]
  inputdata_psp$PATH_IND6 <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                    compiler = "PSP")
  inputdata_psp[, RISK_GRP6 := riskGroupDeriver(species = SP0,
                                                pathIndex = PATH_IND6,
                                                method = "KBEC")]
  inputdata_psp[, LOSS7_IN := LOSS6_IN]
  inputdata_psp[, LOSS6_IN := NA]
  inputdata_psp$PATH_IND7 <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                    compiler = "PSP")
  inputdata_psp[, RISK_GRP7 := riskGroupDeriver(species = SP0,
                                                pathIndex = PATH_IND7,
                                                method = "KBEC")]
  inputdata_psp[, LOSS8_IN := LOSS7_IN]
  inputdata_psp[, LOSS7_IN := NA]
  inputdata_psp$PATH_IND8 <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_psp[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                    compiler = "PSP")
  inputdata_psp[, RISK_GRP8 := riskGroupDeriver(species = SP0,
                                                pathIndex = PATH_IND8,
                                                method = "KBEC")]

  expect_equal(inputdata_psp$RISK_GRP1, inputdata_psp$RISK_GRP2)
  expect_equal(inputdata_psp$RISK_GRP1, inputdata_psp$RISK_GRP3)
  expect_equal(inputdata_psp$RISK_GRP1, inputdata_psp$RISK_GRP4)
  expect_equal(inputdata_psp$RISK_GRP1, inputdata_psp$RISK_GRP5)
  expect_equal(inputdata_psp$RISK_GRP1, inputdata_psp$RISK_GRP6)
  expect_equal(inputdata_psp$RISK_GRP1, inputdata_psp$RISK_GRP7)
  expect_equal(inputdata_psp$RISK_GRP1, inputdata_psp$RISK_GRP8)


  ## testing it in VRI compiation situation
  inputdata_vri <- expand.grid(SP0 = c("S", "H",  "B",  "C",  "PL", "Y",  "E",
                                       "F", "D",  "PW", "AC", "L",
                                       "MB", "AT", "PY", "PA"),
                               LOSS1_IN = c("BNK", "BTP", "CD", "CNK", "CRO", "D",
                                            "DD", "DDA", "DDB",
                                            "DDD", "DDE", "DDF", "DDH", "DDO",
                                            "DDP", "DDQ", "DDT", "DIR",
                                            "DM", "DR", "DRA", "DRB", "DRC",
                                            "DRL", "DRN", "DRR", "DRS",
                                            "DRT", "DTP", "FRK", "FRS", "LRB",
                                            "MIS", "NGC", "OTH", "SCA",
                                            "SNG")) %>%
    data.table
  inputdata_vri[,':='(SP0 = as.character(SP0),
                      LOSS1_IN = as.character(LOSS1_IN))]
  # 1) the location is under merchantable height
  inputdata_vri[, ':='(LOC1_FRO = sample(1:9, size = nrow(inputdata_vri), replace = TRUE),
                       H_MERCH = 10)]
  set(inputdata_vri, , c(paste0("LOSS", 2:8, "_IN"), paste0("LOC", 2:8, "_FRO")), NA)
  inputdata_vri$PATH_IND_psp <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_vri[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                       lossIndicatorLocMatrix = inputdata_vri[,paste0("LOC", 1:8, "_FRO"), with = FALSE],
                                                       merchantableHeight = inputdata_vri$H_MERCH,
                                                       compiler = "PSP")
  inputdata_vri[, RISK_GRP_psp := riskGroupDeriver(species = SP0,
                                                   pathIndex = PATH_IND_psp,
                                                   method = "KBEC")]
  inputdata_vri$PATH_IND_vri <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_vri[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                       lossIndicatorLocMatrix = inputdata_vri[,paste0("LOC", 1:8, "_FRO"), with = FALSE],
                                                       merchantableHeight = inputdata_vri$H_MERCH,
                                                       compiler = "VRI")
  inputdata_vri[, RISK_GRP_vri := riskGroupDeriver(species = SP0,
                                                   pathIndex = PATH_IND_vri,
                                                   method = "KBEC")]
  ### psp and vri should give us same output
  expect_equal(inputdata_vri$PATH_IND_psp, inputdata_vri$PATH_IND_vri)
  expect_equal(inputdata_vri$RISK_GRP_psp, inputdata_vri$RISK_GRP_vri)



  #2) the location is above merchantable height
  inputdata_vri[, ':='(LOC1_FRO = sample(10:20, size = nrow(inputdata_vri), replace = TRUE))]
  # randomly force the loc for 30 observations is 10
  inputdata_vri[sample(1:592, 30), LOC1_FRO := 10]
  inputdata_vri$PATH_IND_vri_above <- pathIndicatorGenerator(lossIndicatorMatix = inputdata_vri[,paste0("LOSS", 1:8, "_IN"), with = FALSE],
                                                             lossIndicatorLocMatrix = inputdata_vri[,paste0("LOC", 1:8, "_FRO"), with = FALSE],
                                                             merchantableHeight = inputdata_vri$H_MERCH,
                                                             compiler = "VRI")
  expect_equal(inputdata_vri$PATH_IND_vri_above, rep("00000000", 592))
  inputdata_vri[, RISK_GRP_vri_above := riskGroupDeriver(species = SP0,
                                                         pathIndex = PATH_IND_vri_above,
                                                         method = "KBEC")]
  expect_equal(inputdata_vri$RISK_GRP_vri_above, rep("1", 592))
})

