read_student_excel <- function(f, faculty) {
  # 学生作成の Excel ファイルを1つ読む
  message("Processing ", basename(f))

  .required_cols_st <- c(the$student$name, the$student$rank)

  x <- readxl::read_excel(f, sheet = 1)

  # 必須列の確認
  nonexistent_col <- is.na(match(.required_cols_st, names(x)))
  if (any(nonexistent_col)) {
    stop(basename(f), ") 必須列がありません。: ",
         paste(.required_cols_st[nonexistent_col], collapse = ", "))
  }

  # 行（教員名）の確認
  f_prof_name <- faculty |> dplyr::pull(the$admin_fc$name)
  x_prof_name <- x |> dplyr::pull(the$student$name)

  nonexistent_row <- is.na(match(f_prof_name, x_prof_name))
  if (any(nonexistent_row)) {
    stop(basename(f), ") 教員名が不足しています: ",
         paste(faculty[nonexistent_row, the$admin_fc$name], collapse = ", "))
  }

  if ("logical" == x |> pull(the$student$rank) |> typeof()) {
    x <- x |>
      mutate((!!the$student$rank) := NA_integer_)
  }

  replacement <- list()
  replacement[[the$student$rank]] <- 100
  x <- x |> tidyr::replace_na(replacement)
  y <- jitter(100 - x |> dplyr::pull(the$student$rank), amount = 0.01)
  names(y) <- faculty[match(x_prof_name, f_prof_name), the$admin_fc$id, drop = TRUE]

  # 統一された順序に整列する
  y[faculty |> dplyr::pull(the$admin_fc$id)]
}


read_professor_excel <- function(f, students) {
  # 教員作成の Excel ファイルを1つ読む
  message("Processing ", basename(f))

  .required_cols_fc <- c(the$faculty$id, the$faculty$eval)
  param <- readxl::read_excel(f, sheet = the$faculty$sh_opt, range = the$faculty$sh_opt_range)
  use_gpa <- param[1, the$faculty$sh_opt_col, drop = TRUE]

  x <- readxl::read_excel(f, sheet = the$faculty$sh_eval)

  # 必須列の確認
  nonexistent_col <- is.na(match(.required_cols_fc, names(x)))
  if (any(nonexistent_col)) {
    stop(basename(f), ") 列がありません: ",
         paste(.required_cols_fc[nonexistent_col], sep = ", "))
  }

  # 行（学生）の確認
  nonexistent_row <-
    is.na(match(students |> dplyr::pull(the$admin_st$id),
                x |> dplyr::pull(the$faculty$id)))

  if (any(nonexistent_row)) {
    stop(basename(f), ") 行がありません: ",
         paste(students[nonexistent_row, the$admin_st$id], sep = ", "))
  }

  if (use_gpa) {
    x <- merge(x, students[, c(the$admin_st$id, the$admin_st$gpa)],
               by.x = the$faculty$id, by.y = the$admin_st$id)
    x[, the$faculty$eval] <- x[, the$faculty$eval] + x[, the$admin_st$gpa]
  }

  y <- jitter(x |> dplyr::pull(the$faculty$eval), amount = 0.005)
  names(y) <- x |> dplyr::pull(the$faculty$id)

  # 統一された順序に整列する
  y[students |> dplyr::pull(the$admin_st$id)]
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
  Student <- Student[faculty_list |> dplyr::pull(the$admin_fc$id), ]


  faculty_data <- list()
  for (f in list.files(dir_faculty, pattern = "\\.xlsx$", full.names = TRUE)) {
    professor_id <- tools::file_path_sans_ext(basename(f))
    x <- read_professor_excel(f, student_list)
    faculty_data[[professor_id]] <- x
  }

  Faculty <- do.call(cbind, faculty_data)
  Faculty <- Faculty[colnames(Student), faculty_list |> dplyr::pull(the$admin_fc$id)]

  list(Student = Student, Faculty = Faculty)
}



#' マッチングを計算する
#'
#' `matchingR::galeShapley.collegeAdmissions()` を使って学生最適なマッチングを計算する。
#'
#' @param util list. [matching_utils()] の結果
#' @param slots integer vector. 各ゼミの定員
#'
#' @return マッチングの結果。列 (`Student`, `Seminar`) を持つ tibble.
#' @export
#'
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


matching_stat <- function(util, result) {

}
