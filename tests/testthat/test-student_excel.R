test_that("read_one_file", {

  faculty <- readxl::read_xlsx(pkg_file("samples/Data4Test1/Admin/faculty.xlsx"))

  ## 1
  student1 <- pkg_file("samples/Data4Test1/Students/STIDst1.xlsx")
  out1 <- read_student_excel(student1, faculty)

  expect_identical(names(out1), c("PROFidA", "PROFidB", "PROFidC"))
  expect_equal(as.vector(out1), c(98, 99, 0), tolerance = 0.01)

  ## 2
  student2 <- pkg_file("samples/Data4Test1/Students/STIDst2.xlsx")
  out2 <- read_student_excel(student2, faculty)

  expect_identical(names(out2), c("PROFidA", "PROFidB", "PROFidC"))
  expect_equal(as.vector(out2), c(99, 98, 97), tolerance = 0.01)

  ## 5
  student5 <- pkg_file("samples/Data4Test1/Students/STIDst5.xlsx")
  out5 <- read_student_excel(student5, faculty)

  expect_identical(names(out5), c("PROFidA", "PROFidB", "PROFidC"))
  expect_equal(as.vector(out5), c(98, 99, 97), tolerance = 0.01)
})


test_that("student file error", {
  faculty <- readxl::read_xlsx(pkg_file("samples/Data4Test1/Admin/faculty.xlsx"))

  # 1: 「希望順位」列がない
  student1 <- pkg_file("error/Students/STIDerr1.xlsx")
  expect_error(read_student_excel(student1, faculty))

  # 2: 「教員名」列がない
  student2 <- pkg_file("error/Students/STIDerr2.xlsx")
  expect_error(read_student_excel(student2, faculty))

  # 3: 「教員C」行がない
  student3 <- pkg_file("error/Students/STIDerr3.xlsx")
  expect_error(read_student_excel(student3, faculty))

  # 4: faculty に入っていない「教員D」がある → OK
  student4 <- pkg_file("error/Students/STIDok4.xlsx")
  expect_silent(read_student_excel(student4, faculty))
})



