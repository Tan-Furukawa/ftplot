#' plane_plot 関数
#'
#' この関数は、空のプロット領域を作成します。デフォルトでは、x軸とy軸の範囲は[0, 1]に設定され、ラベルや軸は表示されません。
#'
#' @param ... 他のプロット関数に渡す追加の引数
#' @return 空のプロット領域
#' @examples
#' # 例: 空のプロット領域を作成
#' plane_plot()
#'
#' # 他のプロット関数と一緒に使用する例
#' plane_plot(xlim = c(0, 10), ylim = c(0, 5), xlab = "X軸", ylab = "Y軸")
#' @export
plane_plot <- \(...) plot(NA, NA, xlim=c(0,1), ylim=c(0,1), xlab = "", ylab = "", axes = FALSE, ...)

#' normalize_vec
#'
#' この関数は、ベクトルを指定された範囲に正規化します。デフォルトでは、[0.0, 1.0]の範囲に正規化されます。
#'
#' @param x 正規化するベクトル
#' @param range 正規化される範囲の指定。デフォルトは [0.0, 1.0]
#' @return ベクトルの正規化結果と逆変換関数のリスト
#' @examples
#' # 例: ベクトルの正規化
#' norm <- normalize_vec(c(10, 20, 30, 40, 50))
#' norm$nx  # 正規化されたベクトル
#'
#' # 逆変換関数を使用して元のベクトルに戻す例
#' orig <- norm$rev_fn(norm$nx)
#'
#' @export
normalize_vec <- \(range = c(0.0, 1.0)) \(x) {
  min_x = min(x); max_x = max(x)
  return (
    list(
      nx = (x - min_x) / (max_x - min_x) * (range[2] - range[1]) + range[1],
      rev_fn = \(y) (y - range[1]) * (max_x - min_x) / (range[2] - range[1]) + min_x
    )
  )
}

#' make_object 関数
#'
#' この関数は、グラフィカルなオブジェクトを生成するためのユーティリティ関数です。指定した範囲にベクトルを正規化し、x軸やy軸の操作をサポートします。
#'
#' @param y_range y軸の正規化範囲を指定します。
#' @param vec_x x軸に使用するベクトル
#' @param vec_y y軸に使用するベクトル
#' @param x_range x軸の正規化範囲を指定します。デフォルトは [0, 1] です。
#' @return グラフィカルなオブジェクトを含むリスト
#' @examples
#' # 例: グラフィカルオブジェクトを生成
#' obj <- make_object(y_range = c(0, 100), vec_x = 1:10, vec_y = c(20, 30, 40, 50, 60, 70, 80, 90, 100, 110))
#'
#' # グラフィカルオブジェクトの使用例
#' obj$xaxis()
#' obj$lines()
#' obj$points()
#'
#' @export
make_object <- \(y_range, vec_x, vec_y, x_range=c(0,1)) {
    normalize_fn_y <- normalize_vec(range=y_range)
    normalize_fn_x <- normalize_vec(range=x_range)
    nvec_y <- vec_y %>% normalize_fn_y
    return (
        list(
            xaxis = \(npretty = 5, ...) {
                at <- vec_x %>% pretty(n = npretty)
                nat <- at %>% normalize_fn_x %>% (\(x) x$nx)
                axis(side = 1, at=at, labels=nat, ...)
            },
            lines = \(...) lines(vec_x, nvec_y$nx, ...),
            points = \(...) points(vec_x, nvec_y$nx, ...),
            yaxis = \(npretty = 3, ...) {
                at <- vec_y %>% pretty(n = npretty)
                nat <- at %>% normalize_fn_y %>% (\(x) x$nx)
                axis(at=nat, labels=at, ...)
            },
            get_yaxis_space = \(npretty = 3) {
                at <- vec_y %>% pretty(n = npretty)
                nat <- at %>% normalize_fn_y %>% (\(x) x$nx)
                return (nat[2] - nat[1])
            },
            regression = \(...) {
                fit <- lm(y ~ x + 1, data = data.frame(x = vec_x, y = nvec_y$nx))
                abline(fit, ...)
            },
            ytext = \(text, ...) {
                at <- vec_y %>% pretty() %>% normalize_fn_y %>% (\(x) x$nx)
                mtext(text, at = mean(at), line=2.5, cex = 1.2,...)
            }
        )
    )
}
