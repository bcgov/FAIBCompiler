test_that("merge_dupUpdate.R: merge and update function do not work correctly", {
  library(data.table)
  # prepare the tables
  table1 <- data.table(level1 = c(rep("A", 4), rep("B", 4)),
                       level2 = rep(c("1", "2"), 4),
                       dupcol = c(1:8)*10,
                       unicol1 = letters[1:8])
  table2 <- data.table(level1 = LETTERS[1:4],
                       level2 = as.character(1:4),
                       dupcol = c(1:4)*100,
                       unicol2 = seq(0, 0.6, by = 0.2))
  
  ## I. test modified function whether consistent with original function by setting updateDup as FALSE
  ## 1. full join
  mergedtable_org_all <- merge(table1, table2, all = TRUE, by = c("level1", "level2"))
  mergedtable_mod_all <- merge_dupUpdate(table1, table2, all = TRUE, by = c("level1", "level2"), updateDup = FALSE)
  expect_equal(mergedtable_mod_all, mergedtable_org_all)
  
  ## inner join
  mergedtable_org_inner <- merge(table1, table2, by = c("level1", "level2"))
  mergedtable_mod_inner <- merge_dupUpdate(table1, table2, by = c("level1", "level2"), updateDup = FALSE)
  expect_equal(mergedtable_mod_inner, mergedtable_org_inner)
  
  ## left join
  mergedtable_org_left <- merge(table1, table2, all.x = TRUE, by = c("level1", "level2"))
  mergedtable_mod_left <- merge_dupUpdate(table1, table2, all.x = TRUE, by = c("level1", "level2"), updateDup = FALSE)
  expect_equal(mergedtable_mod_left, mergedtable_org_left)
  
  ## right join 
  mergedtable_org_right <- merge(table1, table2, all.y = TRUE, by = c("level1", "level2"))
  mergedtable_mod_right <- merge_dupUpdate(table1, table2, all.y = TRUE, by = c("level1", "level2"), updateDup = FALSE)
  expect_equal(mergedtable_mod_right, mergedtable_org_right)
  
  
  ## II. test whether modified function works correctly in terms of updating, for different values in duplicate columns 
  ## 1. full join
  mergedtable_org_all_notupdate <- merge(table1, table2, all = TRUE, by = c("level1", "level2"))
  mergedtable_mod_all_update2to1 <- suppressWarnings(merge_dupUpdate(table1, table2, all = TRUE, 
                                                by = c("level1", "level2"), 
                                                updateDup = TRUE))
  mergedtable_mod_all_update1to2 <- suppressWarnings(merge_dupUpdate(table2, table1, all = TRUE, 
                                                by = c("level1", "level2")))
  ## 
  expect_warning(merge_dupUpdate(table1, table2, all = TRUE, 
                                 by = c("level1", "level2")))
  ## compare the non-dup column
  expect_equal(mergedtable_org_all_notupdate$unicol1, mergedtable_mod_all_update1to2$unicol1)
  expect_equal(mergedtable_org_all_notupdate$unicol1, mergedtable_mod_all_update2to1$unicol1)
  expect_equal(mergedtable_org_all_notupdate$unicol2, mergedtable_mod_all_update1to2$unicol2)
  expect_equal(mergedtable_org_all_notupdate$unicol2, mergedtable_mod_all_update2to1$unicol2)
  ## compare update column
  expect_equal(mergedtable_mod_all_update2to1$dupcol, 
               c(100, 100,  20,  40,  50,  70, 200, 200, 300, 400))
  expect_equal(mergedtable_mod_all_update1to2$dupcol, 
               c(10, 30, 20, 40, 50, 70, 60, 80, 300, 400))
  
  ## inner join
  mergedtable_org_inner_notupdate <- merge(table1, table2, by = c("level1", "level2"))
  mergedtable_mod_inner_update2to1 <- suppressWarnings(merge_dupUpdate(table1, table2, by = c("level1", "level2")))
  mergedtable_mod_inner_update1to2 <- suppressWarnings(merge_dupUpdate(table2, table1, by = c("level1", "level2")))
  expect_warning(merge_dupUpdate(table2, table1, by = c("level1", "level2")))
  
  ## compare the non-dup column
  expect_equal(mergedtable_org_inner_notupdate$unicol1, mergedtable_mod_inner_update1to2$unicol1)
  expect_equal(mergedtable_org_inner_notupdate$unicol1, mergedtable_mod_inner_update2to1$unicol1)
  expect_equal(mergedtable_org_inner_notupdate$unicol2, mergedtable_mod_inner_update1to2$unicol2)
  expect_equal(mergedtable_org_inner_notupdate$unicol2, mergedtable_mod_inner_update2to1$unicol2)
  ## compare update column
  expect_equal(mergedtable_mod_inner_update2to1$dupcol, 
               c(100, 100,  200, 200))
  expect_equal(mergedtable_mod_inner_update1to2$dupcol, 
               c(10, 30, 60, 80))
  
  ## left join
  mergedtable_org_left_notupdate <- merge(table1, table2, by = c("level1", "level2"), all.x = TRUE)
  mergedtable_mod_left_update2to1 <- suppressWarnings(merge_dupUpdate(table1, table2, by = c("level1", "level2"),
                                                                      all.x = TRUE))
  expect_warning(merge_dupUpdate(table2, table1, by = c("level1", "level2"), all.x = TRUE))
  
  ## compare the non-dup column
  expect_equal(mergedtable_org_left_notupdate$unicol1, mergedtable_mod_left_update2to1$unicol1)
  expect_equal(mergedtable_org_left_notupdate$unicol2, mergedtable_mod_left_update2to1$unicol2)
  
  ## compare update column
  expect_equal(mergedtable_mod_left_update2to1$dupcol, 
               c(100, 100,  20, 40, 50, 70, 200, 200))
  
  ## right join 
  mergedtable_org_right_notupdate <- merge(table1, table2, by = c("level1", "level2"), all.y = TRUE)
  mergedtable_mod_right_update2to1 <- suppressWarnings(merge_dupUpdate(table1, table2, by = c("level1", "level2"),
                                                                      all.y = TRUE))
  expect_warning(merge_dupUpdate(table2, table1, by = c("level1", "level2"), all.y = TRUE))
  
  ## compare the non-dup column
  expect_equal(mergedtable_org_right_notupdate$unicol1, mergedtable_mod_right_update2to1$unicol1)
  expect_equal(mergedtable_org_right_notupdate$unicol2, mergedtable_mod_right_update2to1$unicol2)
  
  ## compare update column
  expect_equal(mergedtable_mod_right_update2to1$dupcol, 
               c(100, 100,  200, 200, 300, 400))
  
  ## III. test whether modified function works correctly in terms of updating, for same values in duplicate column
  table3 <- data.table(level1 = c(rep("A", 4), rep("B", 4)),
                       level2 = rep(c("1", "2", "3", "4"), 2),
                       dupcol = c(1:8)*10,
                       unicol1 = letters[1:8])
  
  table4 <- data.table(level1 = LETTERS[1:4],
                       level2 = as.character(1:4),
                       dupcol = c(10, 60, as.numeric(NA), as.numeric(NA)),
                       unicol2 = seq(0, 0.6, by = 0.2))
  
  ## 1. full join
  mergedtable_org_all_notupdate <- merge(table3, table4, all = TRUE, by = c("level1", "level2"))
  expect_warning(merge_dupUpdate(table3, table4, all = TRUE, 
                                 by = c("level1", "level2")),
                 NA)
  mergedtable_mod_all_update4to3 <- merge_dupUpdate(table3, table4, all = TRUE, 
                                                    by = c("level1", "level2"), 
                                                    updateDup = TRUE)
  mergedtable_mod_all_update3to4 <- merge_dupUpdate(table4, table3, all = TRUE, 
                                                    by = c("level1", "level2"))
  ## 
  ## compare the non-dup column
  expect_equal(mergedtable_org_all_notupdate$unicol1, mergedtable_mod_all_update3to4$unicol1)
  expect_equal(mergedtable_org_all_notupdate$unicol1, mergedtable_mod_all_update4to3$unicol1)
  expect_equal(mergedtable_org_all_notupdate$unicol2, mergedtable_mod_all_update3to4$unicol2)
  expect_equal(mergedtable_org_all_notupdate$unicol2, mergedtable_mod_all_update4to3$unicol2)
  ## compare update column
  expect_equal(mergedtable_mod_all_update4to3$dupcol, 
               c(seq(10, 80, by = 10), NA, NA))
  expect_equal(mergedtable_mod_all_update3to4$dupcol, 
               c(seq(10, 80, by = 10), NA, NA))
               
  
  ## inner join
  mergedtable_org_inner_notupdate <- merge(table3, table4, by = c("level1", "level2"))
  expect_warning(merge_dupUpdate(table4, table3, by = c("level1", "level2")),
                 NA)
  mergedtable_mod_inner_update4to3 <- merge_dupUpdate(table3, table4, by = c("level1", "level2"))
  mergedtable_mod_inner_update3to4 <- merge_dupUpdate(table4, table3, by = c("level1", "level2"))
  
  ## compare the non-dup column
  expect_equal(mergedtable_org_inner_notupdate$unicol1, mergedtable_mod_inner_update3to4$unicol1)
  expect_equal(mergedtable_org_inner_notupdate$unicol1, mergedtable_mod_inner_update4to3$unicol1)
  expect_equal(mergedtable_org_inner_notupdate$unicol2, mergedtable_mod_inner_update3to4$unicol2)
  expect_equal(mergedtable_org_inner_notupdate$unicol2, mergedtable_mod_inner_update4to3$unicol2)
  ## compare update column
  expect_equal(mergedtable_mod_inner_update4to3$dupcol, 
               c(10, 60))
  expect_equal(mergedtable_mod_inner_update3to4$dupcol, 
               c(10, 60))
  
  ## left join
  mergedtable_org_left_notupdate <- merge(table3, table4, by = c("level1", "level2"), all.x = TRUE)
  expect_warning(merge_dupUpdate(table4, table3, by = c("level1", "level2"), all.x = TRUE),
                 NA)
  mergedtable_mod_left_update4to3 <- merge_dupUpdate(table3, table4, by = c("level1", "level2"),
                                                                      all.x = TRUE)
  
  ## compare the non-dup column
  expect_equal(mergedtable_org_left_notupdate$unicol1, mergedtable_mod_left_update4to3$unicol1)
  expect_equal(mergedtable_org_left_notupdate$unicol2, mergedtable_mod_left_update4to3$unicol2)
  
  ## compare update column
  expect_equal(mergedtable_mod_left_update4to3$dupcol, 
               seq(10, 80, by = 10))
  
  ## right join 
  mergedtable_org_right_notupdate <- merge(table3, table4, by = c("level1", "level2"), all.y = TRUE)
  expect_warning(merge_dupUpdate(table4, table3, by = c("level1", "level2"), all.y = TRUE),
                 NA)
  mergedtable_mod_right_update4to3 <- merge_dupUpdate(table3, table4, by = c("level1", "level2"),
                                                                       all.y = TRUE)
  
  ## compare the non-dup column
  expect_equal(mergedtable_org_right_notupdate$unicol1, mergedtable_mod_right_update4to3$unicol1)
  expect_equal(mergedtable_org_right_notupdate$unicol2, mergedtable_mod_right_update4to3$unicol2)
  
  ## compare update column
  expect_equal(mergedtable_mod_right_update4to3$dupcol, 
               c(10, 60, NA, NA))
  
})