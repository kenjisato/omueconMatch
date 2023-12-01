read_student_excel <- function(f, faculty) {
  # 学生作成の Excel ファイルを1つ読む

  .required_cols_st <- c("教員名", "希望順位")

  x <- readxl::read_excel(f, sheet = 1)

  # 必須列の確認
  nonexistent_col <- is.na(match(.required_cols_st, names(x)))
  if (any(nonexistent_col)) {
    stop(basename(f), ") 必須列がありません。: ",
         paste(.required_cols_st[nonexistent_col], collapse = ", "))
  }

  # 行（教員名）の確認
  nonexistent_row <- is.na(match(faculty$教員名, x$教員名))
  if (any(nonexistent_row)) {
    stop(basename(f), ") 教員名が不足しています: ",
         paste(faculty$教員名[nonexistent_row], collapse = ", "))
  }

  x$希望順位[is.na(x$希望順位)] <- 100
  y <- jitter(100 - x$希望順位, amount = 0.01)
  names(y) <- faculty$ID[match(x$教員名, faculty$教員名)]

  # 統一された順序に整列する
  y[faculty$ID]
}


read_professor_excel <- function(f, students) {
  # 教員作成の Excel ファイルを1つ読む

  .required_cols_fc <- c("ID", "評価")

  param <- readxl::read_excel(f, sheet = "設定", range = "B2:C3")
  use_gpa <- param$チェック[[1]]

  x <- readxl::read_excel(f, sheet = "評価")

  # 必須列の確認
  nonexistent_col <- is.na(match(.required_cols_fc, names(x)))
  if (any(nonexistent_col)) {
    stop(basename(f), ") 列がありません: ",
         paste(.required_cols_fc[nonexistent_col], sep = ", "))
  }

  # 行（学生）の確認
  nonexistent_row <- is.na(match(students$ID, x$ID))
  if (any(nonexistent_row)) {
    stop(basename(f), ") 行がありません: ",
         paste(students$ID[nonexistent_row], sep = ", "))
  }

  if (use_gpa) {
    x <- merge(x, students[, c("ID", "GPA")], by = "ID")
    x$評価 <- x$評価 + x$GPA
  }

  y <- jitter(x$評価, amount = 0.005)
  names(y) <- x$ID

  # 統一された順序に整列する
  y[students$ID]
}


#' 効用を計算する
#'
#' @param student_list path string. Admin-managed list of students and their GPA.
#' @param faculty_list path string. Admin-managed list of faculty.
#' @param dir_student path string. Directory for student preferences.
#' @param dir_faculty path string. Directory for faculty preferences.
#' @param nc integer. Length of registration ID.
#' @param seed integer. Used as a random seed.
#'
#' @return Returns matching results.
#' @export
#'
matching_utils <- function(
    student_list,
    faculty_list,
    dir_student,
    dir_faculty,
    nc = 8,
    seed = NULL
) {

  set.seed(seed)

  # 学生側ファイルの読み込み
  student_data <- list()
  for (f in list.files(dir_student, pattern = "\\.xlsx$", full.names = TRUE)) {
    # ファイル名「学籍番号.xlsx」から学籍番号を取得
    student_i <- substr(tools::file_path_sans_ext(basename(f)), start = 1, stop = nc)

    # Excel ファイルを読み込んでスコアに変換
    x <- read_student_excel(f, faculty_list)
    student_data[[student_i]] <- x
  }

  Student <- do.call(cbind, student_data)
  Student <- Student[faculty_list$ID, ]


  faculty_data <- list()
  for (f in list.files(dir_faculty, pattern = "\\.xlsx$", full.names = TRUE)) {
    professor_id <- tools::file_path_sans_ext(basename(f))
    x <- read_professor_excel(f, student_list)
    faculty_data[[professor_id]] <- x
  }

  Faculty <- do.call(cbind, faculty_data)
  Faculty <- Faculty[colnames(Student), faculty_list$ID]

  list(Student = Student, Faculty = Faculty)
}



matching_compute <- function(util, slots) {

  result <- matchingR::galeShapley.collegeAdmissions(
    studentUtils = util$Student,
    collegeUtils = util$Faculty,
    slots = slots,
    studentOptimal = TRUE
  )

  result$match_table <- tibble::tibble(
    Student = rownames(util$Faculty),
    Seminar = colnames(util$Faculty)[result$matched.students]
  )

  result
}

