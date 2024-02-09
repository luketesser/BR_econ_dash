# url_tp_scr <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/MIHixtSZDcdWqJJNk%2FW2V29ORkjwEk63AL8LmQ2dQZxb472RzOYYwBrivhefrrqsRBazlOVFFn1XjF6Rr0boliC0Dss2HM27xEJFMWfyM4JUWA1c8PqljroIDObg2gdlcv6Gc9a%2Bxcg88enEuS%2BStf6DtdtgV3UA2qiz%2B1%2FmGp2qNhbb1P7H4m71vGtGaz%2BeDbB0p7rI1dMKiK060n%2BdwJaklzjP5a7%2BRdNnLarwm%2BGOoGEq3JD0Eesjvf1IbhWoWPs6cEfXvR%2Bw3dJfd48C1b64JUdVzqWhwcaWQjZyfuRNxASNmjUr0dAP59U20wod8rcWWJsvQ8ZjBIh1H1Fc5g%3D%3D"
#
# tp_isin <- janitor::clean_names(vroom::vroom(file = url_tp_scr, delim = ",", locale = vroom::locale(encoding = "latin1")))
#
# url_tp <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/NmQyC7JsSUHgwk3jYJCTYoagBAvu5O%2BRAaxTPAsD0gjcTxC9rYR5GlsEeg9NwPC4%2BaXxap7t2QzLqAMwfhCHMBnp%2FYxbMp2O7U1yvQDriCTG8SNPjyeVLnrgmsdADFpd6w%2Bc9bIxlmCXWlJasFNE%2F0gTaPkjXetQpBNB3K8ux2vpFKoHxsGy%2F9veUFlgUSM0C1QtCSvihous9kpHBNO0tjoWebQnFuMcdD%2BPf2VRJM6bbTDFuTFbTOudZ8MJrDCPI9EFzkpWmRvBjLYHWb8im7pOgSZ09GgCWEwZcehRHng2UlBiSxsdH6NnwT1AIbpdJqZERgC5x%2BpKZTosjjWTbA%3D%3D"
#
# tp <- janitor::clean_names(vroom::vroom(file = url_tp, delim = ",", locale = vroom::locale(encoding = "latin1")))
#
# tp <- tp |>
#   mutate(isin = substr(ativo, start = 1, stop = 12)) |>
#   left_join(select(tp_isin, nome_2, data_de_vencimento, isin), by = "isin") |>
#   filter(data > "2017-07-01")
#
# real <- tp |>
#   filter(substr(tp$nome_2, start = 1, stop = 5) == "NTN-B") |>
#   arrange(data, data_de_vencimento)
#
# b_pricipal <- real |>
#   dplyr::filter(substr(real$nome_2, start = 1, stop = 15) == "NTN-B Principal") |>
#   mutate(duration = bizdays(from = data, to = data_de_vencimento, cal = "Brazil/ANBIMA")) |>
#   mutate(taxa_ytm = as.double(taxa_ytm))
#
# real_yield <- real |>
#   filter(duration != "-") |>
#   mutate(duration = as.double(duration)) |>
#   mutate(taxa_ytm = as.double(taxa_ytm)) |>
#   left_join(b_pricipal) |>
#   select(data, data_de_vencimento, taxa_ytm, duration) |>
#   filter(data > "2020-01-01")
#
# factors <- real_yield |>
#   split(real_yield$data) |>
#   purrr::map(~factors_sv(lambda = c(.9,.035,.035), yields = real_yield$taxa_ytm/100, maturidades = real_yield$duration/252), .progress = T)
#
# real_curve_factors <- factors |>
#   as.data.frame() |>
#   t() |>
#   as_tibble() |>
#   mutate(dates = unique(real_yield$data))
