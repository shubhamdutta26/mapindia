example_data <- data.frame(
  state = c("AP", "WB", "Tamil Nadu"),
  values = c(5, 8, 7)
)

p <- plot_india("districts", fill = "red")
q <- plot_india(data = statepop, values = "pop_2023", color = "blue")
r <- plot_india(data = example_data, linewidth = 0.8)
s <- plot_india(include = c("UP", "RJ", "NCT"), labels = TRUE, label_color = "blue")
t <- plot_india(regions = "districts", include = "WB", labels = TRUE, fill = "yellow", linewidth = 0.6)
u <- plot_india(include = .east, exclude = "WB", labels = TRUE)
v <- plot_india("state", labels = TRUE, label_size = 2)

test_that("ggplot object is returned", {
  expect_s3_class(p, "ggplot")
  expect_s3_class(q, "ggplot")
  expect_s3_class(r, "ggplot")
  expect_s3_class(s, "ggplot")
  expect_s3_class(t, "ggplot")
  expect_s3_class(u, "ggplot")
  expect_s3_class(v, "ggplot")
})

test_that("no warnings are produced", {
  expect_silent(p)
  expect_silent(q)
  expect_silent(r)
  expect_silent(s)
  expect_silent(t)
  expect_silent(u)
  expect_silent(v)
})

test_that("correct data is used", {
  p_map_data <- map_india(regions = "districts")
  expect_identical(p$data, p_map_data)

  q_map_data <- map_using_data(statepop, values = "pop_2023")
  expect_identical(q$data, q_map_data)

  r_map_data <- map_using_data(example_data)
  r_map_data <- map_using_data(example_data)
  expect_identical(r$data, r_map_data)

  s_map_data <- map_india(regions = "states", include = c("UP", "RJ", "NCT"))
  expect_identical(s$data, s_map_data)

  t_map_data <- map_india(regions = "districts", include = "WB")
  expect_identical(t$data, t_map_data)

  u_map_data <- map_india(include = .east, exclude = "WB")
  expect_identical(u$data, u_map_data)

  v_map_data <- map_india()
  expect_identical(v$data, v_map_data)
})

test_that("plots are stable", {
  vdiffr::expect_doppelganger("State population map in blue", q)
  vdiffr::expect_doppelganger("Example data state map with custom linewidth", r)
})

test_that("layer parameters are correct", {
  expect_s3_class(p$layers[[1]], "ggproto")
  expect_s3_class(p$layers[[1]]$geom, "GeomSf")
  expect_equal(as.character(p$layers[[1]]$aes_params$colour), "black")
  expect_equal(as.character(p$layers[[1]]$aes_params$fill), "red")
  expect_equal(p$layers[[1]]$aes_params$linewidth, 0.3)

  expect_s3_class(q$layers[[1]], "ggproto")
  expect_s3_class(q$layers[[1]]$geom, "GeomSf")
  expect_equal(deparse(q$layers[[1]]$mapping$fill), "~.data[[\"pop_2023\"]]")
  expect_equal(as.character(q$layers[[1]]$aes_params$colour), "blue")
  expect_equal(q$layers[[1]]$aes_params$linewidth, 0.3)

  expect_s3_class(r$layers[[1]], "ggproto")
  expect_s3_class(r$layers[[1]]$geom, "GeomSf")
  expect_equal(deparse(r$layers[[1]]$mapping$fill), "~.data[[\"values\"]]")
  expect_equal(as.character(r$layers[[1]]$aes_params$colour), "black")
  expect_equal(r$layers[[1]]$aes_params$linewidth, 0.8)

  expect_s3_class(s$layers[[1]], "ggproto")
  expect_s3_class(s$layers[[1]]$geom, "GeomSf")
  expect_equal(as.character(s$layers[[1]]$aes_params$fill), "white")
  expect_equal(as.character(s$layers[[1]]$aes_params$colour), "black")
  expect_equal(s$layers[[1]]$aes_params$linewidth, 0.3)
  expect_s3_class(s$layers[[2]], "ggproto")
  expect_s3_class(s$layers[[2]]$geom, "GeomText")
  expect_equal(deparse(s$layers[[2]]$mapping$label), "~.data$abbr")
  expect_equal(as.character(s$layers[[2]]$aes_params$colour), "blue")

  expect_s3_class(t$layers[[1]], "ggproto")
  expect_s3_class(s$layers[[1]]$geom, "GeomSf")
  expect_equal(as.character(t$layers[[1]]$aes_params$fill), "yellow")
  expect_equal(as.character(t$layers[[1]]$aes_params$colour), "black")
  expect_equal(t$layers[[1]]$aes_params$linewidth, 0.6)
  expect_s3_class(t$layers[[2]], "ggproto")
  expect_s3_class(t$layers[[2]]$geom, "GeomText")
  expect_equal(deparse(t$layers[[2]]$mapping$label),
               "~sub(\" District\", \"\", .data$dtname)")

  expect_s3_class(u$layers[[1]], "ggproto")
  expect_s3_class(u$layers[[1]]$geom, "GeomSf")
  expect_equal(as.character(u$layers[[1]]$aes_params$fill), "white")
  expect_equal(as.character(u$layers[[1]]$aes_params$colour), "black")
  expect_equal(u$layers[[1]]$aes_params$linewidth, 0.3)
  expect_s3_class(u$layers[[2]], "ggproto")
  expect_s3_class(u$layers[[2]]$geom, "GeomText")
  expect_equal(deparse(u$layers[[2]]$mapping$label), "~.data$abbr")

  expect_s3_class(v$layers[[1]], "ggproto")
  expect_s3_class(v$layers[[1]]$geom, "GeomSf")
  expect_equal(as.character(v$layers[[1]]$aes_params$fill), "white")
  expect_equal(as.character(v$layers[[1]]$aes_params$colour), "black")
  expect_equal(v$layers[[1]]$aes_params$linewidth, 0.3)
  expect_s3_class(v$layers[[2]], "ggproto")
  expect_s3_class(v$layers[[2]]$geom, "GeomText")
  expect_equal(deparse(v$layers[[2]]$mapping$label), "~.data$abbr")
  expect_equal(v$layers[[2]]$aes_params$size, 2)
})
