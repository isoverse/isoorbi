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

