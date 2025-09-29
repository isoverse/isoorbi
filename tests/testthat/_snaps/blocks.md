# orbi_adjust_block [plain]

    Code
      result2 <- orbi_adjust_block(test_data, 1, "test1", shift_start_scan.no = 1)
    Message
      v orbi_adjust_block() made the following block adjustments in file test1:
        > moved block 1 start from scan.no 1 (0.10 min) to 2 (0.20 min)

---

    Code
      result3 <- orbi_adjust_block(test_data, 2, "test1", shift_start_time.min = -1)
    Message
      v orbi_adjust_block() made the following block adjustments in file test1:
        > moved block 2 start from scan.no 4 (0.40 min) to 1 (0.10 min)
        > moved block 1 end to the new start of block 2

# orbi_adjust_block [fancy]

    Code
      result2 <- orbi_adjust_block(test_data, 1, "test1", shift_start_scan.no = 1)
    Message
      [32m✔[39m [1morbi_adjust_block()[22m made the following [32mblock[39m adjustments in file [34mtest1[39m:
        → moved [32mblock 1[39m start from [32mscan.no[39m 1 (0.10 min) to 2 (0.20 min)

---

    Code
      result3 <- orbi_adjust_block(test_data, 2, "test1", shift_start_time.min = -1)
    Message
      [32m✔[39m [1morbi_adjust_block()[22m made the following [32mblock[39m adjustments in file [34mtest1[39m:
        → moved [32mblock 2[39m start from [32mscan.no[39m 4 (0.40 min) to 1 (0.10 min)
        → moved [32mblock 1[39m end to the new start of [32mblock 2[39m

# orbi_segment_block() [plain]

    Code
      res1 <- orbi_segment_blocks(test_data, into_segments = 2)
    Message
      v orbi_segment_blocks() segmented 3 data blocks in '2 files' creating 2
      segments per block (on average) with 1.3 scans per segment (on average)

---

    Code
      res2 <- orbi_segment_blocks(test_data, by_scans = 2)
    Message
      v orbi_segment_blocks() segmented 3 data blocks in '2 files' creating 1.3
      segments per block (on average) with 2 scans per segment (on average)

---

    Code
      res3 <- orbi_segment_blocks(test_data, by_time_interval = 1)
    Message
      v orbi_segment_blocks() segmented 3 data blocks in '2 files' creating 2.3
      segments per block (on average) with 1.3 scans per segment (on average)

# orbi_segment_block() [fancy]

    Code
      res1 <- orbi_segment_blocks(test_data, into_segments = 2)
    Message
      [32m✔[39m [1morbi_segment_blocks()[22m segmented [32m3 data blocks[39m in [34m2 files[39m creating 2 segments
      per block (on average) with 1.3 scans per segment (on average)

---

    Code
      res2 <- orbi_segment_blocks(test_data, by_scans = 2)
    Message
      [32m✔[39m [1morbi_segment_blocks()[22m segmented [32m3 data blocks[39m in [34m2 files[39m creating 1.3
      segments per block (on average) with 2 scans per segment (on average)

---

    Code
      res3 <- orbi_segment_blocks(test_data, by_time_interval = 1)
    Message
      [32m✔[39m [1morbi_segment_blocks()[22m segmented [32m3 data blocks[39m in [34m2 files[39m creating 2.3
      segments per block (on average) with 1.3 scans per segment (on average)

