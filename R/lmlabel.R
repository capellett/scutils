# #' @export
# lm_label <- function(m) {
#   substitute(atop(italic(y)==b~italic(x)+a,
#                   italic(r)^2==r2),
#              list(a = format(coef(m)[[1]], digits = 2),
#                   b = format(coef(m)[[2]], digits = 2),
#                   r2 = format(summary(m)$r.squared, digits = 2))) %>%
#     as.expression() %>%  as.character()}
