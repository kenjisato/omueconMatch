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
    student = list(name = "\u6559\u54e1\u540d", rank = "\u5e0c\u671b\u9806\u4f4d"),
    faculty = list(id = "ID", eval = "\u8a55\u4fa1", sh_opt = "\u8a2d\u5b9a", sh_eval = "\u8a55\u4fa1",
                   sh_opt_col = "\u30c1\u30a7\u30c3\u30af", sh_opt_range = "B2:C3"),
    admin_fc = list(id = "ID", name = "\u6559\u54e1\u540d"),
    admin_st = list(id = "ID", name = "\u6c0f\u540d", gpa = "GPA")
) {
  the$student <- student
  the$faculty <- faculty
  the$admin_fc <- admin_fc
  the$admin_st <- admin_st
}
