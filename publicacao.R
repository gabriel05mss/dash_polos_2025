install.packages('rsconnect')
rsconnect::setAccountInfo(name='polos2025', token='639FFE7B8734D746BF8F8F81C9792F34', secret='u26rPwxTjAduA8fucEVSgKvVVjU4bXrCmwT3yxVN')
library(rsconnect)
rsconnect::deployApp('path/to/your/app')
