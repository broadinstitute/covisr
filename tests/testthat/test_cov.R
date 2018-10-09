library('covisr')
context('cov')


test_that("Create coverage object", {
  cov_df <- data.frame(
    "chr" =     c('chr1_A', 'chr1_A', 'chr2_A', 'chr2_A', 
                  'chr1_D', 'chr1_D', 'chr2_D', 'chr2_D'),
    "start0" =  c(   0, 1000,    0, 1000,    0, 1000,   0, 1000),
    "end0" =    c(1000, 2000, 1000, 2000, 1000, 2000,1000, 2000),
    "id" =      c('chr1_A_0', 'chr1_A_1', 'chr2_A_0', 'chr2_A_1', 
                  'chr1_D_0', 'chr1_D_1', 'chr2_D', 'chr2_D_1'),
    "sample1" = c(1.46, 0.62, 0.54, 0.56, 0.67, 0.55, 0.53, 0.48) ,
    "sample2" = c(2.35, 1.0,  0.84, 0.85, 1.01, 0.84, 0.97, 0.82),
    "sample3" = c(2.68, 1.45, 1.28, 1.46, 1.45, 1.31, 1.35, 1.2)
  )
  
  reverse_contigs='chr2_D'
  sub_suffix=c('_A', '_D')
  suba=c('chr1_A', 'chr2_A')
  subb=c('chr2_D', 'chr1_D')
  
  cov <- cov.init(cov_df=cov_df, reverse_contigs = reverse_contigs,
                  suba = suba, subb = subb, sub_suffix=sub_suffix)
  expect_error(cov.init())
  expect_error(expect_warning(cov.init(cov_df=df, cov_tsv='test.tsv')))
  expect_equal(cov$df, df)
  expect_is(cov, 'cov')
  expect_equal(cov$prefix, 'output')
  expect_equal(cov$cutoff, 0)
  expect_equal(cov$first_sample_idx, 5)
  expect_equal(cov$n_samples, 3)
})


test_that("Get contigs", cov.get_contigs{
  expect_equal(factor(c("chr1_A", "chr2_A", "chr2_D", "chr1_D")),
               cov.get_contigs(cov_example()))
})


test_that("Add sample index", {
  cov <- cov.add_sample_idx(cov_example())
  expect_equal(cov$sample_idx, c(5, 6, 7))
})