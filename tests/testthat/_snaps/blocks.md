# orbi_adjust_blocks() / cli [plain]

    Code
      result2 <- orbi_adjust_blocks(test_data, 1, "test1", shift_start_scan.no = 1)
    Message
      v orbi_adjust_blocks() adjusted 1 block in 1 file
       > block 1 in test1: moved start from scan 1 (6s) to 2 (12s)

---

    Code
      result3 <- orbi_adjust_blocks(test_data, 2, "test1", shift_start_time.min = -1)
    Message
      v orbi_adjust_blocks() adjusted 1 block in 1 file
       > block 2 in test1: moved start from scan 4 (24s) to 1 (6s)
       > block 2 in test1: removed block 1 entirely as a result of the adjustment

---

    Code
      result4 <- orbi_adjust_blocks(test_data, 1, "test1", shift_end_scan.no = 1)
    Message
      v orbi_adjust_blocks() adjusted 1 block in 1 file
       > block 1 in test1: moved end from scan 3 (18s) to 4 (24s)
       > block 1 in test1: moved the start of block 2 to the new end

---

    Code
      result5 <- orbi_adjust_blocks(test_data, 1, "test1", shift_end_time.min = 1)
    Message
      v orbi_adjust_blocks() adjusted 1 block in 1 file
       > block 1 in test1: moved end from scan 3 (18s) to 5 (30s)
       > block 1 in test1: removed block 2 entirely as a result of the adjustment

---

    Code
      result6 <- orbi_adjust_blocks(multi_file_data, block = c(1, 2),
      shift_start_scan.no = c(1, NA), shift_end_scan.no = c(NA, -1))
    Message
      v orbi_adjust_blocks() adjusted 4 blocks in 2 files
       > block 1 in test1: moved start from scan 1 (6s) to 2 (12s)
       > block 1 in test2: moved start from scan 1 (6s) to 2 (12s)
       > block 2 in test1: moved end from scan 6 (36s) to 5 (30s)
       > block 2 in test2: moved end from scan 6 (36s) to 5 (30s)

# orbi_adjust_blocks() / cli [fancy]

    Code
      result2 <- orbi_adjust_blocks(test_data, 1, "test1", shift_start_scan.no = 1)
    Message
      [32m✔[39m [1morbi_adjust_blocks()[22m adjusted 1 block in 1 file
       → block 1 in [34mtest1[39m: moved start from scan 1 (6s) to 2 (12s)

---

    Code
      result3 <- orbi_adjust_blocks(test_data, 2, "test1", shift_start_time.min = -1)
    Message
      [32m✔[39m [1morbi_adjust_blocks()[22m adjusted 1 block in 1 file
       → block 2 in [34mtest1[39m: moved start from scan 4 (24s) to 1 (6s)
       → block 2 in [34mtest1[39m: removed block 1 entirely as a result of the adjustment

---

    Code
      result4 <- orbi_adjust_blocks(test_data, 1, "test1", shift_end_scan.no = 1)
    Message
      [32m✔[39m [1morbi_adjust_blocks()[22m adjusted 1 block in 1 file
       → block 1 in [34mtest1[39m: moved end from scan 3 (18s) to 4 (24s)
       → block 1 in [34mtest1[39m: moved the start of block 2 to the new end

---

    Code
      result5 <- orbi_adjust_blocks(test_data, 1, "test1", shift_end_time.min = 1)
    Message
      [32m✔[39m [1morbi_adjust_blocks()[22m adjusted 1 block in 1 file
       → block 1 in [34mtest1[39m: moved end from scan 3 (18s) to 5 (30s)
       → block 1 in [34mtest1[39m: removed block 2 entirely as a result of the adjustment

---

    Code
      result6 <- orbi_adjust_blocks(multi_file_data, block = c(1, 2),
      shift_start_scan.no = c(1, NA), shift_end_scan.no = c(NA, -1))
    Message
      [32m✔[39m [1morbi_adjust_blocks()[22m adjusted 4 blocks in 2 files
       → block 1 in [34mtest1[39m: moved start from scan 1 (6s) to 2 (12s)
       → block 1 in [34mtest2[39m: moved start from scan 1 (6s) to 2 (12s)
       → block 2 in [34mtest1[39m: moved end from scan 6 (36s) to 5 (30s)
       → block 2 in [34mtest2[39m: moved end from scan 6 (36s) to 5 (30s)

# orbi_segment_block() / cli [plain]

    Code
      res1 <- orbi_segment_blocks(test_data, into_segments = 2)
    Message
      v orbi_segment_blocks() segmented 3 data blocks in 2 files creating 2 segments
      per block (on average) with 1.3 scans per segment (on average)

---

    Code
      res2 <- orbi_segment_blocks(test_data, by_scans = 2)
    Message
      v orbi_segment_blocks() segmented 3 data blocks in 2 files creating 1.3
      segments per block (on average) with 2 scans per segment (on average)

---

    Code
      res3 <- orbi_segment_blocks(test_data, by_time_interval = 1)
    Message
      v orbi_segment_blocks() segmented 3 data blocks in 2 files creating 2.3
      segments per block (on average) with 1.3 scans per segment (on average)

# orbi_segment_block() / cli [fancy]

    Code
      res1 <- orbi_segment_blocks(test_data, into_segments = 2)
    Message
      [32m✔[39m [1morbi_segment_blocks()[22m segmented [32m3 data blocks[39m in 2 files creating 2 segments
      per block (on average) with 1.3 scans per segment (on average)

---

    Code
      res2 <- orbi_segment_blocks(test_data, by_scans = 2)
    Message
      [32m✔[39m [1morbi_segment_blocks()[22m segmented [32m3 data blocks[39m in 2 files creating 1.3
      segments per block (on average) with 2 scans per segment (on average)

---

    Code
      res3 <- orbi_segment_blocks(test_data, by_time_interval = 1)
    Message
      [32m✔[39m [1morbi_segment_blocks()[22m segmented [32m3 data blocks[39m in 2 files creating 2.3
      segments per block (on average) with 1.3 scans per segment (on average)

