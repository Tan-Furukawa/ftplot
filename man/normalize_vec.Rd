% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiplot.R
\name{normalize_vec}
\alias{normalize_vec}
\title{normalize_vec}
\usage{
normalize_vec(x, range = c(0, 1))
}
\arguments{
\item{x}{正規化するベクトル}

\item{range}{正規化される範囲の指定。デフォルトは [0.0, 1.0]}
}
\value{
ベクトルの正規化結果と逆変換関数のリスト
}
\description{
この関数は、ベクトルを指定された範囲に正規化します。デフォルトでは、[0.0, 1.0]の範囲に正規化されます。
}
\examples{
# 例: ベクトルの正規化
norm <- normalize_vec(c(10, 20, 30, 40, 50))
norm$nx  # 正規化されたベクトル

# 逆変換関数を使用して元のベクトルに戻す例
orig <- norm$rev_fn(norm$nx)

}
