test_that("read professor excel works", {

  adm_students <- readxl::read_xlsx(pkg_file("samples/Data4Test1/Admin/students.xlsx"))
  gpa <- c(3, 3, 2, 2, 1, 1)

  # 1
  profa <- pkg_file("samples/Data4Test1/Faculty/PROFidA.xlsx")
  out <- read_professor_excel(profa, adm_students)
  expect_identical(names(out), paste0("STIDst", 1:6))
  expect_equal(as.vector(out), c(70, 60, 50, 40, 30, 20) + gpa, tolerance = 0.01)

  # 2
  profc <- pkg_file("samples/Data4Test1/Faculty/PROFidC.xlsx")
  out <- read_professor_excel(profc, adm_students)
  expect_identical(names(out), paste0("STIDst", 1:6))
  expect_equal(as.vector(out), c(50, 70, 40, 30, 20, 60), tolerance = 0.01)
})

