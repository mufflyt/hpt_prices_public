testthat::test_that("codebook loads with the expected concepts and no duplicates", {
  codebook <- test_codebook(exclude_concepts = NULL)

  testthat::expect_setequal(
    base::unique(codebook$concept),
    base::c(
      "colonoscopy", "emb", "iud_insertion", "iud_device",
      "vaginal_hysterectomy", "lavh", "drg_uterine_nonmalignant",
      "bariatric_surgery", "drg_bariatric", "surgical_pathology", "dc", "hysteroscopy_sampling", "office_visit_em",
      "drg_cesarean", "drg_vaginal_delivery", "vaginal_delivery_cpt", "cesarean_cpt"
    )
  )
  testthat::expect_true(base::all(base::c("58100", "58300", "45378", "G0121", "742", "743", "43775", "43644", "619", "88305", "788", "807", "59400", "59510") %in% codebook$code))
})

testthat::test_that("normalize_code pads and strips DRGs but leaves CPT alone", {
  testthat::expect_equal(normalize_code(base::c("0742", "742", "742.0", " 743 "), "ms_drg"), base::c("742", "742", "742", "743"))
  testthat::expect_equal(normalize_code(base::c(" 58100 ", "g0121", "58100.0"), "procedure"), base::c("58100", "G0121", "58100"))
})

testthat::test_that("code type gating rejects CDM, revenue codes, and APR-DRG collisions", {
  codebook <- test_codebook()
  tbl <- tibble::tibble(
    row_id = 1:9,
    raw_code = base::c("58100", "58100", "58100", "742", "0742", "742", "742", "45378", "99999"),
    raw_type = base::c("CPT", "CDM", NA, "APR-DRG", "MS-DRG", "DRG", "RC", "hcpcs", "CPT")
  )

  matched <- match_target_codes(tbl, "raw_code", "raw_type", codebook)

  testthat::expect_setequal(matched$row_id, base::c(1L, 3L, 5L, 6L, 8L))
  testthat::expect_equal(matched$type_verified[matched$row_id == 1L], TRUE)
  testthat::expect_equal(matched$type_verified[matched$row_id == 3L], FALSE)
  testthat::expect_equal(matched$code[matched$row_id == 5L], "742")
  testthat::expect_equal(matched$type_verified[matched$row_id == 6L], FALSE)
  testthat::expect_equal(matched$concept[matched$row_id == 8L], "colonoscopy")
})

testthat::test_that("normalize_url_key treats scheme, www, case, and trailing slash as equal but keeps identifying queries", {
  testthat::expect_equal(
    normalize_url_key(base::c(
      "https://www.Example.org/files/123_A_standardcharges.csv?v=2",
      "http://example.org/files/123_A_standardcharges.csv/",
      "https://example.org/files/123%20A_standardcharges.csv",
      "https://bucket.s3.amazonaws.com/f/123_A_standardcharges.csv?X-Amz-Signature=abc&X-Amz-Expires=600",
      "https://store.blob.core.windows.net/f/1.csv?sv=2022-11-02&se=2026-09-13&sig=xyz",
      "https://hospitalpricedisclosure.com/download.aspx?pi=111",
      "https://hospitalpricedisclosure.com/download.aspx?pi=222"
    )),
    base::c(
      "example.org/files/123_A_standardcharges.csv",
      "example.org/files/123_A_standardcharges.csv",
      "example.org/files/123 A_standardcharges.csv",
      "bucket.s3.amazonaws.com/f/123_A_standardcharges.csv",
      "store.blob.core.windows.net/f/1.csv",
      "hospitalpricedisclosure.com/download.aspx?pi=111",
      "hospitalpricedisclosure.com/download.aspx?pi=222"
    )
  )
})

testthat::test_that("interleave_by_host round-robins hosts and is a permutation", {
  urls <- base::c("https://a.org/1", "https://a.org/2", "https://a.org/3", "https://b.org/1", "https://c.org/1", "https://b.org/2")
  order_idx <- interleave_by_host(urls)

  testthat::expect_setequal(order_idx, base::seq_along(urls))
  testthat::expect_equal(url_host(urls[order_idx]), base::c("a.org", "b.org", "c.org", "a.org", "b.org", "a.org"))
  testthat::expect_equal(base::order(order_idx)[order_idx], base::seq_along(urls))
})

testthat::test_that("HPT_DATA_DIR overrides the drive default without checking the drive", {
  dir <- base::file.path(base::tempdir(), "hpt_override_check")
  withr::local_envvar(HPT_DATA_DIR = dir)
  # stand in a drive check that always fails: it must not run when HPT_DATA_DIR is set
  real_check <- base::get("hpt_default_data_dir", envir = base::globalenv())
  base::assign("hpt_default_data_dir", function() base::stop("drive check ran"), envir = base::globalenv())
  withr::defer(base::assign("hpt_default_data_dir", real_check, envir = base::globalenv()))
  testthat::expect_equal(hpt_data_dir(), dir)
  testthat::expect_equal(hpt_path("x"), base::file.path(dir, "x"))
})
