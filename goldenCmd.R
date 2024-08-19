unloadNamespace("BKT");rm(list = ls());devtools::load_all("./");devtools::test()

现在我实现了一些r里没有的 python 函数，之后要翻译的话就用这里的函数
matrix.reshape(dims) -> reshape_python(matrix, dims)