#' Excelファイルの列名、シート名等を設定する関数
#'
#' @param student 学生側評価の必須列名
#' @param faculty 教員側評価の必須列名、シート名、設定項目の列名等
#' @param admin_fc 事務管理ファイル（ゼミリスト）
#' @param admin_st 事務管理ファイル（学生リスト）
#'
#' @return 副作用のために呼ばれるので何も返さない。`the` という環境に必要な情報が保存される。
#' @export
#'
setup_excel_names <- function(
    student = list(name = "教員名", rank = "希望順位"),
    faculty = list(id = "ID", eval = "評価", sh_opt = "設定", sh_eval = "評価",
                   sh_opt_col = "チェック", sh_opt_range = "B2:C3"),
    admin_fc = list(id = "ID", name = "教員名"),
    admin_st = list(id = "ID", name = "氏名", gpa = "GPA")
) {
  the$student <- student
  the$faculty <- faculty
  the$admin_fc <- admin_fc
  the$admin_st <- admin_st
}